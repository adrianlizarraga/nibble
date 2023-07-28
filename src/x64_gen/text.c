#include <string.h>
#include "array.h"
#include "hash_map.h"
#include "stream.h"
#include "x64_gen/text.h"
#include "x64_gen/lir_to_x64.h"
#include "x64_gen/regs.h"

static const u8 x64_reg_val[X64_REG_COUNT] = {
    [X64_RAX] = 0,    [X64_RCX] = 1,    [X64_RDX] = 2,    [X64_RBX] = 3,    [X64_RSP] = 4,  [X64_RBP] = 5,    [X64_RSI] = 6,
    [X64_RDI] = 7,    [X64_R8] = 8,     [X64_R9] = 9,     [X64_R10] = 10,   [X64_R11] = 11, [X64_R12] = 12,   [X64_R13] = 13,
    [X64_R14] = 14,   [X64_R15] = 15,   [X64_XMM0] = 0,   [X64_XMM1] = 1,   [X64_XMM2] = 2, [X64_XMM3] = 3,   [X64_XMM4] = 4,
    [X64_XMM5] = 5,   [X64_XMM6] = 6,   [X64_XMM7] = 7,   [X64_XMM8] = 8,   [X64_XMM9] = 9, [X64_XMM10] = 10, [X64_XMM11] = 11,
    [X64_XMM12] = 12, [X64_XMM13] = 13, [X64_XMM14] = 14, [X64_XMM15] = 15,
};

enum X64_ModMode {
    X64_MOD_INDIRECT = 0,
    X64_MOD_INDIRECT_DISP_U8 = 1,
    X64_MOD_INDIRECT_DISP_U32 = 2,
    X64_MOD_DIRECT = 3
};

// ModRM byte
// mod[7:6], reg[5:3], rm[2:0]
static inline u8 X64_modrm_byte(u8 mod, u8 reg, u8 rm)
{
    return (rm & 0x7) | ((reg & 0x7) << 3) | ((mod & 0x3) << 6);
}

// REX PREFIX: 0100WRXB
// W - 1 is 64-bit operand
// R - Extension of the ModR/M reg field
// X - Extension of the SIB index field
// B - Extension of the ModR/M r/m field, SIB base field, or Opcode reg field
static inline u8 X64_rex_prefix(u8 w, u8 r, u8 x, u8 b)
{
    return 0x40 | ((w & 0x1) << 3) | ((r & 0x1) << 2) | ((x & 0x1) << 1) | (b & 0x1);
}

// REX prefix for memory (or direct reg to reg) addressing without a SIB byte; REX.X not used.
static inline u8 X64_rex_nosib(u8 w, u8 reg, u8 rm)
{
    return X64_rex_prefix(w, reg >> 3, 0, rm >> 3);
}

// REX prefix for memory addressing (mod != 11) with a SIB byte.
static inline u8 X64_rex_sib(u8 w, u8 reg, u8 index, u8 base)
{
    return X64_rex_prefix(w, reg >> 3, index >> 3, base >> 3);
}

// REX prefix for register operand code in last 3 bits of opcode (e.g., push).
// If need extended registers, then REX.B is used as the most-significant bit.
static inline u8 X64_rex_opcode_reg(u8 w, u8 reg)
{
    return X64_rex_prefix(w, 0, 0, reg >> 3);
}

typedef struct X64_AddrBytes {
    u8 mod; // top 2 bits from ModRM byte.
    u8 rm; // bottom 3 bits from ModRM byte.
    u8 sib_byte;
    s32 disp;
    bool rex_b; // w/o SIB byte: extends r/m in ModRM byte. w/ SIB byte: extends base in SIB byte.
    bool rex_x; // Extends index in SIB byte.
    bool has_disp; // Emit disp bytes even if 0 (e.g., for [rbp]).
    bool has_sib_byte;
} X64_AddrBytes;

// Converts a high-level X64 SIBD address to specific ModRM and SIB byte information.
static X64_AddrBytes X64_get_addr_bytes(const X64_SIBD_Addr* addr)
{
    X64_AddrBytes addr_bytes = {0};

    if (addr->kind == X64__SIBD_ADDR_GLOBAL) {
        const Symbol* sym = addr->global;

        if (sym->kind == SYMBOL_PROC) {
            addr_bytes.has_disp = true;
            addr_bytes.disp = 0; // Must be patched by compiler to relative offset of proc
            addr_bytes.mod = X64_MOD_INDIRECT; // indicates rip + disp32
            addr_bytes.rm = 0x5; // b101 indicates rip + disp32
        }
        else {
            assert(sym->kind == SYMBOL_VAR);
            NIBBLE_FATAL_EXIT("Does not yet support global var addresses in x64 ELF text generator."); // TODO: Fix.
        }
    }

    if (addr->kind != X64__SIBD_ADDR_LOCAL) {
        NIBBLE_FATAL_EXIT("Does not yet support non-local addresses in x64 ELF text generator."); // TODO: Fix.
    }

    const bool has_index = addr->local.index_reg != (u8)-1 && addr->local.scale != 0;
    const bool has_base = addr->local.base_reg != (u8)-1;
    const bool disp_is_imm8 = (addr->local.disp <= 127) && (addr->local.disp >= -128);

    addr_bytes.has_disp = addr->local.disp != 0 || (addr->local.base_reg == X64_RBP);
    addr_bytes.disp = addr->local.disp;

    if (has_base && !addr_bytes.has_disp && !has_index) {
        const u8 base_reg = x64_reg_val[addr->local.base_reg];
        addr_bytes.mod = X64_MOD_INDIRECT;
        addr_bytes.rm = base_reg & 0x7;
        addr_bytes.rex_b = base_reg > 7;
    }
    else if (has_base && addr_bytes.has_disp && !has_index) {
        const u8 base_reg = x64_reg_val[addr->local.base_reg];
        addr_bytes.mod = disp_is_imm8 ? X64_MOD_INDIRECT_DISP_U8 : X64_MOD_INDIRECT_DISP_U32;
        addr_bytes.rm = base_reg & 0x7;
        addr_bytes.rex_b = base_reg > 7;
    }
    else if (has_base && has_index) {
        const u8 base_reg = x64_reg_val[addr->local.base_reg];
        const u8 index_reg = x64_reg_val[addr->local.index_reg];
        addr_bytes.mod =
            !addr_bytes.has_disp ? X64_MOD_INDIRECT : (disp_is_imm8 ? X64_MOD_INDIRECT_DISP_U8 : X64_MOD_INDIRECT_DISP_U32);
        addr_bytes.rm = 0x4; // Indicates a SIB byte is necessary.
        addr_bytes.rex_b = base_reg > 7;
        addr_bytes.rex_x = index_reg > 7;
        addr_bytes.has_sib_byte = true;
        addr_bytes.sib_byte =
            X64_modrm_byte(addr->local.scale, index_reg & 0x7, base_reg & 0x7); // SIB byte is computed the same way as ModRM
    }
    else {
        NIBBLE_FATAL_EXIT("Does not support local address with has_base=%d, has_disp=%d, has_index=%d in ELF text generator.",
                          has_base, addr_bytes.has_disp, has_index); // TODO: Fix.
    }

    return addr_bytes;
}

/*
$ xxd -g 1 -s $((0x180)) -l $((0x41)) out.o
00000180: 48 31 ed 8b 3c 24 48 8d 74 24 08 48 8d 54 fc 10  H1..<$H.t$.H.T..
00000190: 31 c0 e8 09 00 00 00 89 c7 b8 3c 00 00 00 0f 05  1.........<.....
000001a0: 55 48 89 e5 48 83 ec 10 c7 45 fc 0a 00 00 00 c7  UH..H....E......
000001b0: 45 f8 01 00 00 00 8b 45 fc 03 45 f8 48 89 ec 5d  E......E..E.H..]
000001c0: c3
*/

/*
objdump -M intel -d out.o

0000000000000000 <_start>:
   0:	48 31 ed             	xor    rbp,rbp
   3:	8b 3c 24             	mov    edi,DWORD PTR [rsp]
   6:	48 8d 74 24 08       	lea    rsi,[rsp+0x8]
   b:	48 8d 54 fc 10       	lea    rdx,[rsp+rdi*8+0x10]
  10:	31 c0                	xor    eax,eax
  12:	e8 09 00 00 00       	call   20 <main>
  17:	89 c7                	mov    edi,eax
  19:	b8 3c 00 00 00       	mov    eax,0x3c
  1e:	0f 05                	syscall

0000000000000020 <main>:
  20:	55                   	push   rbp
  21:	48 89 e5             	mov    rbp,rsp
  24:	48 83 ec 10          	sub    rsp,0x10
  28:	c7 45 fc 0a 00 00 00 	mov    DWORD PTR [rbp-0x4],0xa
  2f:	c7 45 f8 01 00 00 00 	mov    DWORD PTR [rbp-0x8],0x1
  36:	8b 45 fc             	mov    eax,DWORD PTR [rbp-0x4]
  39:	03 45 f8             	add    eax,DWORD PTR [rbp-0x8]
  3c:	48 89 ec             	mov    rsp,rbp
  3f:	5d                   	pop    rbp
  40:	c3                   	ret
 */
// Hard-coded for now.
static const u8 startup_code[] = {0x48, 0x31, 0xed, 0x8b, 0x3c, 0x24, 0x48, 0x8d, 0x74, 0x24, 0x08, 0x48, 0x8d, 0x54, 0xfc, 0x10,
                                  0x31, 0xc0, 0xe8, 0x09, 0x00, 0x00, 0x00, 0x89, 0xc7, 0xb8, 0x3c, 0x00, 0x00, 0x00, 0x0f, 0x05};
static size_t startup_code_main_offset_loc = 0x13; // The location in the startup code to patch the relative offset of the main proc.
static size_t startup_code_next_ip_after_call = 0x17; // The operand to call is relative from the next instruction pointer.

static const u8 main_code[] = {0x55, 0x48, 0x89, 0xe5, 0x48, 0x83, 0xec, 0x10, 0xc7, 0x45, 0xfc, 0x0a, 0x00, 0x00, 0x00, 0xc7, 0x45,
                               0xf8, 0x01, 0x00, 0x00, 0x00, 0x8b, 0x45, 0xfc, 0x03, 0x45, 0xf8, 0x48, 0x89, 0xec, 0x5d, 0xc3};

typedef struct X64_TextGenState {
    Allocator* gen_mem;
    Allocator* tmp_mem;
    Array(u8) buffer;
    HMap proc_offsets; // Symbol* -> starting offset in buffer
} X64_TextGenState;

static inline void X64_set_proc_offset(X64_TextGenState* gen_state, const Symbol* proc_sym)
{
    hmap_put(&gen_state->proc_offsets, PTR_UINT(proc_sym), array_len(gen_state->buffer));
}

static inline void X64_write_imm32_bytes(X64_TextGenState* gen_state, u32 val)
{
    array_push(gen_state->buffer, val & 0xFF); // byte 1
    array_push(gen_state->buffer, (val >> 8) & 0xFF); // byte 2
    array_push(gen_state->buffer, (val >> 16) & 0xFF); // byte 3
    array_push(gen_state->buffer, (val >> 24) & 0xFF); // byte 4
}

static inline void X64_write_imm16_bytes(X64_TextGenState* gen_state, u16 val)
{
    array_push(gen_state->buffer, val & 0xFF); // byte 1
    array_push(gen_state->buffer, (val >> 8) & 0xFF); // byte 2
}

static void X64_elf_gen_proc_text(X64_TextGenState* gen_state, Symbol* proc_sym)
{
    const DeclProc* decl = (const DeclProc*)proc_sym->decl;
    if (decl->is_incomplete) {
        return;
    }

    AllocatorState tmp_mem_state = allocator_get_state(gen_state->tmp_mem);

    // Save this proc's offset in the buffer for use by call instructions.
    X64_set_proc_offset(gen_state, proc_sym);

    Array(X64__Instr) instrs = X64__gen_proc_instrs(gen_state->gen_mem, gen_state->tmp_mem, proc_sym);
    const size_t num_instrs = array_len(instrs);

    // Loop throught X64 instructions and write out machine code to buffer.
    for (size_t i = 0; i < num_instrs; i += 1) {
        X64__Instr* instr = &instrs[i];

        // const bool is_jmp_target = X64__is_instr_jmp_target(instr);
        const X64_Instr_Kind kind = X64__get_instr_kind(instr);

        switch (kind) {
        case X64_Instr_Kind_NOOP: {
            // No-Op. Do nothing.
        } break;
        // PUSH
        case X64_Instr_Kind_PUSH: {
            // We only push r64, which has opcode 0x50 + REG_VAL[2:0]. If reg is >= R8, set REX.B.
            const u8 reg_val = x64_reg_val[instr->push.reg];
            const u8 opcode = 0x50 + (reg_val & 0x7);
            const bool use_ext_regs = reg_val > 7;

            if (use_ext_regs) {
                array_push(gen_state->buffer, X64_rex_opcode_reg(0, reg_val)); // REX.B
            }

            array_push(gen_state->buffer, opcode);
        } break;
        case X64_Instr_Kind_POP: {
            // We only pop r64, which has opcode 0x58 + REG_VAL[2:0]. If reg is >= R8, set REX.B.
            const u8 reg_val = x64_reg_val[instr->pop.reg];
            const u8 opcode = 0x58 + (reg_val & 0x7);
            const bool use_ext_regs = reg_val > 7;

            if (use_ext_regs) {
                array_push(gen_state->buffer, X64_rex_opcode_reg(0, reg_val)); // REX.B
            }

            array_push(gen_state->buffer, opcode);
        } break;
        // RET
        case X64_Instr_Kind_RET: {
            array_push(gen_state->buffer, 0xc3); // opcode is 0xc3 for near return to calling proc.
        } break;
        // ADD
        case X64_Instr_Kind_ADD_RM: {
            // 02 /r => add r8, r/m8
            // 66 03 /r => add r16, r/m16
            // 03 /r => add r32, r/m32
            // REX.W + 03 /r => add r64, r/m64
            const u8 size = instr->add_rm.size;
            const u8 dst_reg = x64_reg_val[instr->add_rm.dst];
            const X64_AddrBytes src_addr = X64_get_addr_bytes(&instr->add_rm.src);
            const bool dst_is_ext = dst_reg > 7;
            const bool use_ext_regs = dst_is_ext || src_addr.rex_b || src_addr.rex_x;
            const bool is_64_bit = size == 8;
            const u8 opcode = size == 1 ? 0x02 : 0x03;

            // 0x66 prefix for 16-bit operands.
            if (size == 2) {
                array_push(gen_state->buffer, 0x66);
            }

            // REX prefix.
            if (use_ext_regs || is_64_bit) {
                array_push(gen_state->buffer, X64_rex_prefix(is_64_bit, dst_reg >> 3, src_addr.rex_x, src_addr.rex_b));
            }

            array_push(gen_state->buffer, opcode); // opcode
            array_push(gen_state->buffer, X64_modrm_byte(src_addr.mod, dst_reg & 0x7, src_addr.rm)); // ModRM byte.

            if (src_addr.has_sib_byte) {
                array_push(gen_state->buffer, src_addr.sib_byte);
            }

            if (src_addr.has_disp) {
                if (src_addr.mod == X64_MOD_INDIRECT_DISP_U8) {
                    array_push(gen_state->buffer, (u8)src_addr.disp);
                }
                else {
                    assert(src_addr.mod == X64_MOD_INDIRECT_DISP_U32);
                    X64_write_imm32_bytes(gen_state, src_addr.disp);
                }
            }
        } break;
        // SUB
        case X64_Instr_Kind_SUB_RI: {
            const u8 size = instr->sub_ri.size;
            const u8 dst_reg = x64_reg_val[instr->sub_ri.dst];
            const u32 imm = instr->sub_ri.imm;
            const bool imm_is_byte = imm <= 255;
            const bool reg_is_ext = dst_reg > 7;

            // 80 /5 ib => sub r/m8, imm8
            if (size == 1) {
                if (reg_is_ext) {
                    array_push(gen_state->buffer, X64_rex_nosib(0, 0, dst_reg)); // REX.B for ext regs
                }
                array_push(gen_state->buffer, 0x80); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, 5, dst_reg)); // 5 is opcode ext in reg, rm for dst_reg
                array_push(gen_state->buffer, instr->sub_ri.imm); // imm8
            }
            // 66 83 /5 ib => sub r/m16, imm8
            // 66 81 /5 iw => sub r/m16, imm16
            else if (size == 2) {
                array_push(gen_state->buffer, 0x66); // 0x66 for 16-bit operands

                if (reg_is_ext) {
                    array_push(gen_state->buffer, X64_rex_nosib(0, 0, dst_reg)); // REX.B for ext regs
                }

                array_push(gen_state->buffer, imm_is_byte ? 0x83 : 0x81); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, 5, dst_reg)); // 5 is opcode ext in reg, rm for dst_reg

                if (imm_is_byte) {
                    array_push(gen_state->buffer, imm); // imm8
                }
                else {
                    array_push(gen_state->buffer, imm & 0xFF); // Lower byte
                    array_push(gen_state->buffer, (imm >> 8) & 0xFF); // Upper byte
                }
            }
            // 83 /5 ib => sub r/m32, imm8
            // 81 /5 id => sub r/m32, imm32
            else if (size == 4) {
                if (reg_is_ext) {
                    array_push(gen_state->buffer, X64_rex_nosib(0, 0, dst_reg)); // REX.B for ext regs
                }

                array_push(gen_state->buffer, imm_is_byte ? 0x83 : 0x81); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, 5, dst_reg)); // 5 is opcode ext in reg, rm for dst_reg

                if (imm_is_byte) {
                    array_push(gen_state->buffer, imm); // imm8
                }
                else {
                    X64_write_imm32_bytes(gen_state, imm);
                }
            }
            // REX.W + 83 /5 ib => sub r/m64, imm8
            // REX.W + 81 /5 id => sub r/m64, imm32
            else {
                assert(size == 8);
                array_push(gen_state->buffer, X64_rex_nosib(1, 0, dst_reg)); // REX.W for 64-bit, REX.B for ext regs
                array_push(gen_state->buffer, imm_is_byte ? 0x83 : 0x81); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, 5, dst_reg)); // 5 is opcode ext in reg, rm for dst_reg

                if (imm_is_byte) {
                    array_push(gen_state->buffer, imm); // imm8
                }
                else {
                    X64_write_imm32_bytes(gen_state, imm);
                }
            }
        } break;
        // MOV
        case X64_Instr_Kind_MOV_RR: {
            const u8 size = instr->mov_rr.size;
            const u8 src_reg = x64_reg_val[instr->mov_rr.src];
            const u8 dst_reg = x64_reg_val[instr->mov_rr.dst];
            const bool src_is_ext = src_reg > 7;
            const bool dst_is_ext = dst_reg > 7;

            // 88 /r => mov r/m8, r8
            if (size == 1) {
                if (src_is_ext || dst_is_ext) {
                    array_push(gen_state->buffer, X64_rex_nosib(0, src_reg, dst_reg)); // REX.RB for ext regs
                }
                array_push(gen_state->buffer, 0x88); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, src_reg, dst_reg)); // ModRM
            }
            // 0x66 + 89 /r => mov r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
            else if (size == 2) {
                array_push(gen_state->buffer, 0x66); // 0x66 for 16-bit operands
                if (src_is_ext || dst_is_ext) {
                    array_push(gen_state->buffer, X64_rex_nosib(0, src_reg, dst_reg)); // REX.RB for ext regs
                }
                array_push(gen_state->buffer, 0x89); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, src_reg, dst_reg)); // ModRM
            }
            // 89 /r => mov r/m32, r32
            else if (size == 4) {
                if (src_is_ext || dst_is_ext) {
                    array_push(gen_state->buffer, X64_rex_nosib(0, src_reg, dst_reg)); // REX.RB for ext regs
                }
                array_push(gen_state->buffer, 0x89); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, src_reg, dst_reg)); // ModRM
            }
            // REX.W + 89 /r => mov r/m64, r64
            else {
                assert(size == 8);
                array_push(gen_state->buffer, X64_rex_nosib(1, src_reg, dst_reg)); // REX.W for 64-bit, REX.RB for ext regs
                array_push(gen_state->buffer, 0x89); // opcode
                array_push(gen_state->buffer, X64_modrm_byte(X64_MOD_DIRECT, src_reg, dst_reg)); // ModRM
            }
        } break;
        case X64_Instr_Kind_MOV_RM: {
            // 8A /r => mov r8, r/m8
            // 66 8B /r => mov r16, r/m16
            // 8B /r => mov r32, r/m32
            // REX.W + 8B /r => mov r64, r/m64
            const u8 size = instr->mov_rm.size;
            const u8 dst_reg = x64_reg_val[instr->mov_rm.dst];
            const X64_AddrBytes src_addr = X64_get_addr_bytes(&instr->mov_rm.src);
            const bool dst_is_ext = dst_reg > 7;
            const bool use_ext_regs = dst_is_ext || src_addr.rex_b || src_addr.rex_x;
            const bool is_64_bit = size == 8;
            const u8 opcode = size == 1 ? 0x8A : 0x8B;

            // 0x66 prefix for 16-bit operands.
            if (size == 2) {
                array_push(gen_state->buffer, 0x66);
            }

            // REX prefix.
            if (use_ext_regs || is_64_bit) {
                array_push(gen_state->buffer, X64_rex_prefix(is_64_bit, dst_reg >> 3, src_addr.rex_x, src_addr.rex_b));
            }

            array_push(gen_state->buffer, opcode); // opcode
            array_push(gen_state->buffer, X64_modrm_byte(src_addr.mod, dst_reg & 0x7, src_addr.rm)); // ModRM byte.

            if (src_addr.has_sib_byte) {
                array_push(gen_state->buffer, src_addr.sib_byte);
            }

            if (src_addr.has_disp) {
                if (src_addr.mod == X64_MOD_INDIRECT_DISP_U8) {
                    array_push(gen_state->buffer, (u8)src_addr.disp);
                }
                else {
                    assert(src_addr.mod == X64_MOD_INDIRECT_DISP_U32);
                    X64_write_imm32_bytes(gen_state, src_addr.disp);
                }
            }
        } break;
        case X64_Instr_Kind_MOV_MI: {
            const u8 size = instr->mov_mi.size;
            const X64_AddrBytes addr_bytes = X64_get_addr_bytes(&instr->mov_mi.dst);
            const bool use_ext_regs = addr_bytes.rex_b || addr_bytes.rex_x;

            // C6 /0 ib => mov r/m8, imm8
            if (size == 1) {
                if (use_ext_regs) {
                    array_push(gen_state->buffer, X64_rex_prefix(0, 0, addr_bytes.rex_x, addr_bytes.rex_b));
                }
                array_push(gen_state->buffer, 0xC6); // opcode
                array_push(gen_state->buffer,
                           X64_modrm_byte(addr_bytes.mod, 0, addr_bytes.rm)); // 0 is opcode extension (i.e., the /0)

                if (addr_bytes.has_sib_byte) {
                    array_push(gen_state->buffer, addr_bytes.sib_byte);
                }

                if (addr_bytes.has_disp) {
                    if (addr_bytes.mod == X64_MOD_INDIRECT_DISP_U8) {
                        array_push(gen_state->buffer, (u8)addr_bytes.disp);
                    }
                    else {
                        assert(addr_bytes.mod == X64_MOD_INDIRECT_DISP_U32);
                        X64_write_imm32_bytes(gen_state, addr_bytes.disp);
                    }
                }

                array_push(gen_state->buffer, (u8)instr->mov_mi.imm);
            }
            // 66 C7 /0 iw => mov r/m16, imm16
            else if (size == 2) {
                array_push(gen_state->buffer, 0x66); // 0x66 for 16-bit operands

                if (use_ext_regs) {
                    array_push(gen_state->buffer, X64_rex_prefix(0, 0, addr_bytes.rex_x, addr_bytes.rex_b)); // REX prefix
                }
                array_push(gen_state->buffer, 0xC7); // opcode
                array_push(gen_state->buffer,
                           X64_modrm_byte(addr_bytes.mod, 0, addr_bytes.rm)); // 0 is opcode extension (i.e., the /0)

                if (addr_bytes.has_sib_byte) {
                    array_push(gen_state->buffer, addr_bytes.sib_byte);
                }

                if (addr_bytes.has_disp) {
                    if (addr_bytes.mod == X64_MOD_INDIRECT_DISP_U8) {
                        array_push(gen_state->buffer, (u8)addr_bytes.disp);
                    }
                    else {
                        assert(addr_bytes.mod == X64_MOD_INDIRECT_DISP_U32);
                        X64_write_imm32_bytes(gen_state, addr_bytes.disp);
                    }
                }

                X64_write_imm16_bytes(gen_state, instr->mov_mi.imm);
            }
            // C7 /0 id => mov r/m32, imm32
            else if (size == 4) {
                if (use_ext_regs) {
                    array_push(gen_state->buffer, X64_rex_prefix(0, 0, addr_bytes.rex_x, addr_bytes.rex_b));
                }
                array_push(gen_state->buffer, 0xC7); // opcode
                array_push(gen_state->buffer,
                           X64_modrm_byte(addr_bytes.mod, 0, addr_bytes.rm)); // 0 is opcode extension (i.e., the /0)

                if (addr_bytes.has_sib_byte) {
                    array_push(gen_state->buffer, addr_bytes.sib_byte);
                }

                if (addr_bytes.has_disp) {
                    if (addr_bytes.mod == X64_MOD_INDIRECT_DISP_U8) {
                        array_push(gen_state->buffer, (u8)addr_bytes.disp);
                    }
                    else {
                        assert(addr_bytes.mod == X64_MOD_INDIRECT_DISP_U32);
                        X64_write_imm32_bytes(gen_state, addr_bytes.disp);
                    }
                }

                X64_write_imm32_bytes(gen_state, instr->mov_mi.imm);
            }
            // REX.W + C7 /0 id => mov r/m64, imm32
            else {
                assert(size == 8);
                array_push(gen_state->buffer,
                           X64_rex_prefix(1, 0, addr_bytes.rex_x, addr_bytes.rex_b)); // REX.W and other reg extensions.
                array_push(gen_state->buffer, 0xC7); // opcode
                array_push(gen_state->buffer,
                           X64_modrm_byte(addr_bytes.mod, 0, addr_bytes.rm)); // 0 is opcode extension (i.e., the /0)

                if (addr_bytes.has_sib_byte) {
                    array_push(gen_state->buffer, addr_bytes.sib_byte);
                }

                if (addr_bytes.has_disp) {
                    if (addr_bytes.mod == X64_MOD_INDIRECT_DISP_U8) {
                        array_push(gen_state->buffer, (u8)addr_bytes.disp);
                    }
                    else {
                        assert(addr_bytes.mod == X64_MOD_INDIRECT_DISP_U32);
                        X64_write_imm32_bytes(gen_state, addr_bytes.disp);
                    }
                }

                X64_write_imm32_bytes(gen_state, instr->mov_mi.imm);
            }
        } break;
        default:
            // NIBBLE_FATAL_EXIT("Unknown X64 instruction kind %d\n", kind);
            break;
        }
    }

    allocator_restore_state(tmp_mem_state);
}

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, BucketList* procs, const Symbol* main_proc)
{
    AllocatorState tmp_mem_state = allocator_get_state(tmp_mem);

    const size_t startup_code_size = sizeof(startup_code);
    const size_t num_procs = procs->num_elems;

    X64_TextGenState gen_state = {.gen_mem = gen_mem,
                                  .tmp_mem = tmp_mem,
                                  .buffer = array_create(gen_mem, u8, startup_code_size << 2),
                                  .proc_offsets = hmap(clp2(num_procs), gen_mem)};

    array_push_elems(gen_state.buffer, startup_code, startup_code_size); // Add _start code.

    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* proc_sym = (Symbol*)(*sym_ptr);

        X64_elf_gen_proc_text(&gen_state, proc_sym);
    }

    // Patch the location of the main proc.
    {
        u64* offset_ptr = hmap_get(&gen_state.proc_offsets, PTR_UINT(main_proc));
        assert(offset_ptr);
        *((u32*)(&gen_state.buffer[startup_code_main_offset_loc])) = (u32)*offset_ptr - (u32)startup_code_next_ip_after_call;
    }
    text_sec->buf = gen_state.buffer;
    text_sec->size = array_len(gen_state.buffer);
    text_sec->align = 0x10;
    text_sec->proc_offs = gen_state.proc_offsets;

    allocator_restore_state(tmp_mem_state);
}

#include <string.h>
#include "array.h"
#include "ast/module.h"
#include "hash_map.h"
#include "stream.h"
#include "x64_gen/text.h"
#include "x64_gen/xir_to_x64.h"
#include "x64_gen/regs.h"

static const u8 x64_reg_val[X64_REG_COUNT] = {
    [X64_RAX] = 0,    [X64_RCX] = 1,    [X64_RDX] = 2,    [X64_RBX] = 3,    [X64_RSP] = 4,  [X64_RBP] = 5,    [X64_RSI] = 6,
    [X64_RDI] = 7,    [X64_R8] = 8,     [X64_R9] = 9,     [X64_R10] = 10,   [X64_R11] = 11, [X64_R12] = 12,   [X64_R13] = 13,
    [X64_R14] = 14,   [X64_R15] = 15,   [X64_XMM0] = 0,   [X64_XMM1] = 1,   [X64_XMM2] = 2, [X64_XMM3] = 3,   [X64_XMM4] = 4,
    [X64_XMM5] = 5,   [X64_XMM6] = 6,   [X64_XMM7] = 7,   [X64_XMM8] = 8,   [X64_XMM9] = 9, [X64_XMM10] = 10, [X64_XMM11] = 11,
    [X64_XMM12] = 12, [X64_XMM13] = 13, [X64_XMM14] = 14, [X64_XMM15] = 15,
};

static u16 x64_setcc_condition_opcodes[] = {
    [COND_U_LT] = 0x920F, [COND_S_LT] = 0x9C0F,   [COND_U_LTEQ] = 0x960F, [COND_S_LTEQ] = 0x9E0F, [COND_U_GT] = 0x970F,
    [COND_S_GT] = 0x9F0F, [COND_U_GTEQ] = 0x930F, [COND_S_GTEQ] = 0x9D0F, [COND_EQ] = 0x940F,     [COND_NEQ] = 0x950F,
};

static u8 x64_jmpcc_short_condition_opcodes[] = {
    [COND_U_LT] = 0x72, [COND_S_LT] = 0x7C,   [COND_U_LTEQ] = 0x76, [COND_S_LTEQ] = 0x7E, [COND_U_GT] = 0x77,
    [COND_S_GT] = 0x7F, [COND_U_GTEQ] = 0x73, [COND_S_GTEQ] = 0x7D, [COND_EQ] = 0x74,     [COND_NEQ] = 0x75,
};

static u16 x64_jmpcc_near_condition_opcodes[] = {
    [COND_U_LT] = 0x0F82, [COND_S_LT] = 0x0F8C,   [COND_U_LTEQ] = 0x0F86, [COND_S_LTEQ] = 0x0F8E, [COND_U_GT] = 0x0F87,
    [COND_S_GT] = 0x0F8F, [COND_U_GTEQ] = 0x0F83, [COND_S_GTEQ] = 0x0F8D, [COND_EQ] = 0x0F84,     [COND_NEQ] = 0x0F85,
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
    ConstAddr patch_entity; // Symbol (or string/float literal) addr that needs to be patched for RIP + disp32
} X64_AddrBytes;

// Converts a high-level X64 SIBD address to specific ModRM and SIB byte information.
static X64_AddrBytes X64_get_addr_bytes(const X64_SIBD_Addr* addr)
{
    if (addr->kind == X64_SIBD_ADDR_GLOBAL) {
        return (X64_AddrBytes){.has_disp = true,
                               .patch_entity = {.kind = CONST_ADDR_SYM, .sym = addr->global},
                               // If this is a procedure, disp will be patched by the compiler to relative offset of proc.
                               // If this is a global variable, the linker will patch it. The compiler will write the 0.
                               .disp = 0,

                               // The following mod/rm values indicate RIP + disp32
                               .mod = X64_MOD_INDIRECT,
                               .rm = 0x5};
    }

    if (addr->kind == X64_SIBD_ADDR_STR_LIT) {
        return (X64_AddrBytes){.has_disp = true,
                               .patch_entity = {.kind = CONST_ADDR_STR_LIT, .str_lit = addr->str_lit},
                               // Compiler will write a displacement of 0, but linker will patch using reloc info.
                               .disp = 0,

                               // The following mod/rm values indicate RIP + disp32
                               .mod = X64_MOD_INDIRECT,
                               .rm = 0x5};
    }

    if (addr->kind == X64_SIBD_ADDR_FLOAT_LIT) {
        return (X64_AddrBytes){.has_disp = true,
                               .patch_entity = {.kind = CONST_ADDR_FLOAT_LIT, .float_lit = addr->float_lit},
                               // Compiler will write a displacement of 0, but linker will patch using reloc info.
                               .disp = 0,

                               // The following mod/rm values indicate RIP + disp32
                               .mod = X64_MOD_INDIRECT,
                               .rm = 0x5};
    }

    assert(addr->kind == X64_SIBD_ADDR_LOCAL);

    X64_AddrBytes addr_bytes = {0};

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

typedef struct X64_TextBBlock {
    u32 index;
    s64 final_offset;
    Array(u8) buffer;

    union {
        u8 bytes[2]; // max 2 bytes
        u16 _u16;
    } jmp_opcode;
    u8 jmp_opcode_len;

    union {
        u8 bytes[4];
        s32 _s32;
    } jmp_offset;
    u8 jmp_offset_len;

    X64_Instr* jmp_instr;
} X64_TextBBlock;

typedef struct X64_UsageLoc {
    u32 bblock_index;
    s32 bblock_offset;
} X64_UsageLoc;

typedef struct X64_InternalReloc {
    X64_UsageLoc usage_loc; // Location into which to write the symbol's relative offset.
    s64 bytes_to_next_ip; // Offset to the next instruction (typically 4)
    ConstAddr ref_addr; // Entity who's relative offset we need to patch.
} X64_InternalReloc;

typedef struct X64_TextGenState {
    X64_TextBBlock* curr_bblock;
    Array(X64_InternalReloc) proc_off_patches; // Usage of non-foreign procs (patched by compiler).
    Array(X64_InternalReloc) relocs; // Relocations for usage of global variables or foreign procedures.
} X64_TextGenState;

static inline void X64_write_imm64_bytes(X64_TextGenState* gen_state, u64 val)
{
    // Increase the length of the byte buffer at once.
    const size_t init_len = array_len(gen_state->curr_bblock->buffer);
    array_set_len(gen_state->curr_bblock->buffer, init_len + sizeof(val));

    // Write the 64-bit value directly in (we know this is little-endian)
    u64* write_loc = (u64*)&gen_state->curr_bblock->buffer[init_len];
    *write_loc = val;
}

static inline void X64_write_imm32_bytes(X64_TextGenState* gen_state, u32 val)
{
    array_push(gen_state->curr_bblock->buffer, val & 0xFF); // byte 1
    array_push(gen_state->curr_bblock->buffer, (val >> 8) & 0xFF); // byte 2
    array_push(gen_state->curr_bblock->buffer, (val >> 16) & 0xFF); // byte 3
    array_push(gen_state->curr_bblock->buffer, (val >> 24) & 0xFF); // byte 4
}

static inline void X64_write_imm16_bytes(X64_TextGenState* gen_state, u16 val)
{
    array_push(gen_state->curr_bblock->buffer, val & 0xFF); // byte 1
    array_push(gen_state->curr_bblock->buffer, (val >> 8) & 0xFF); // byte 2
}

static inline void X64_write_imm_bytes(X64_TextGenState* gen_state, u64 val, u8 num_bytes)
{
    if (num_bytes == 1) {
        array_push(gen_state->curr_bblock->buffer, (u8)val);
    }
    else if (num_bytes == 2) {
        X64_write_imm16_bytes(gen_state, (u16)val);
    }
    else if (num_bytes == 4) {
        X64_write_imm32_bytes(gen_state, (u32)val);
    }
    else {
        assert(num_bytes == 8);
        X64_write_imm64_bytes(gen_state, val);
    }
}

static s32 X64_try_get_global_entity_rel_offset(X64_TextGenState* gen_state, ConstAddr entity)
{
    const X64_UsageLoc curr_loc = {.bblock_index = gen_state->curr_bblock->index,
                                   .bblock_offset = array_len(gen_state->curr_bblock->buffer)};

    switch (entity.kind) {
    case CONST_ADDR_STR_LIT:
    case CONST_ADDR_FLOAT_LIT:
        array_push(gen_state->relocs, (X64_InternalReloc){.usage_loc = curr_loc, .bytes_to_next_ip = 4, .ref_addr = entity});
        return 0;
    case CONST_ADDR_SYM: {
        const Symbol* sym = entity.sym;

        // Record relocation for usage of global variable. Linker will use to patch.
        if (sym->kind == SYMBOL_VAR) {
            array_push(gen_state->relocs, (X64_InternalReloc){.usage_loc = curr_loc, .bytes_to_next_ip = 4, .ref_addr = entity});
            return 0;
        }

        // Record relocation for usage of foreign procedure. Linker will use this info to patch.
        if (sym->kind == SYMBOL_PROC && (sym->decl->flags & DECL_IS_FOREIGN)) {
            array_push(gen_state->relocs, (X64_InternalReloc){.usage_loc = curr_loc, .bytes_to_next_ip = 4, .ref_addr = entity});
            return 0;
        }

        //
        // This is a non-foreign procedure (loc is patched by compiler).
        //
        assert(sym->kind == SYMBOL_PROC);

        // Record patch info.
        array_push(gen_state->proc_off_patches, (X64_InternalReloc){.usage_loc = curr_loc, .bytes_to_next_ip = 4, .ref_addr = entity});
        return 0;
    }
    }

    NIBBLE_FATAL_EXIT("Unexpected global entity kind %d in x64 text relocation creation", entity.kind);
    return 0;
}

static void X64_write_addr_disp(X64_TextGenState* gen_state, const X64_AddrBytes* addr)
{
    if (!addr->has_disp) {
        return;
    }

    if (addr->mod == X64_MOD_INDIRECT && addr->rm == 0x5) { // RIP + disp32 for reference to global entity.
        // Get the relative offset to the symbol/literal (or record patch info).
        s32 rel_off = X64_try_get_global_entity_rel_offset(gen_state, addr->patch_entity);
        X64_write_imm32_bytes(gen_state, (u32)rel_off);
    }
    else if (addr->mod == X64_MOD_INDIRECT_DISP_U8) {
        array_push(gen_state->curr_bblock->buffer, (u8)addr->disp);
    }
    else {
        assert(addr->mod == X64_MOD_INDIRECT_DISP_U32);
        X64_write_imm32_bytes(gen_state, addr->disp);
    }
}

static inline void X64_write_elf_binary_instr_rm(X64_TextGenState* gen_state, u8 opcode_1byte, u8 opcode_large, u8 size, u8 dst,
                                                 const X64_SIBD_Addr* src)
{
    // Example:
    // 02 /r => add r8, r/m8
    // 66 03 /r => add r16, r/m16
    // 03 /r => add r32, r/m32
    // REX.W + 03 /r => add r64, r/m64
    const u8 dst_reg = x64_reg_val[dst];
    const X64_AddrBytes src_addr = X64_get_addr_bytes(src);
    const bool dst_is_ext = dst_reg > 7;
    const bool use_ext_regs = dst_is_ext || src_addr.rex_b || src_addr.rex_x;
    const bool is_64_bit = size == 8;
    const u8 opcode = size == 1 ? opcode_1byte : opcode_large;

    // 0x66 prefix for 16-bit operands.
    if (size == 2) {
        array_push(gen_state->curr_bblock->buffer, 0x66);
    }

    // REX prefix.
    if (use_ext_regs || is_64_bit) {
        array_push(gen_state->curr_bblock->buffer, X64_rex_prefix(is_64_bit, dst_reg >> 3, src_addr.rex_x, src_addr.rex_b));
    }

    array_push(gen_state->curr_bblock->buffer, opcode);
    array_push(gen_state->curr_bblock->buffer, X64_modrm_byte(src_addr.mod, dst_reg, src_addr.rm)); // ModRM byte.

    if (src_addr.has_sib_byte) {
        array_push(gen_state->curr_bblock->buffer, src_addr.sib_byte);
    }

    X64_write_addr_disp(gen_state, &src_addr);
}

static inline void X64_write_elf_binary_instr_mi(X64_TextGenState* gen_state, u8 opcode_instr_1byte, u8 opcode_imm_1byte,
                                                 u8 opcode_imm_larger, u8 opcode_ext, u8 dst_size, const X64_SIBD_Addr* dst, u32 imm)
{
    const X64_AddrBytes dst_addr = X64_get_addr_bytes(dst);
    const bool is_64_bit = dst_size == 8;
    const bool use_ext_regs = dst_addr.rex_b || dst_addr.rex_x;
    const bool imm_is_byte = imm <= 255;
    const u8 opcode = (dst_size == 1) ? opcode_instr_1byte : (imm_is_byte ? opcode_imm_1byte : opcode_imm_larger);

    if (dst_size == 2) {
        array_push(gen_state->curr_bblock->buffer, 0x66); // 0x66 for 16-bit operands
    }

    if (is_64_bit || use_ext_regs) {
        array_push(gen_state->curr_bblock->buffer, X64_rex_prefix(is_64_bit, 0, dst_addr.rex_x, dst_addr.rex_b)); // REX prefix
    }

    array_push(gen_state->curr_bblock->buffer, opcode); // opcode
    array_push(gen_state->curr_bblock->buffer, X64_modrm_byte(dst_addr.mod, opcode_ext, dst_addr.rm));

    if (dst_addr.has_sib_byte) {
        array_push(gen_state->curr_bblock->buffer, dst_addr.sib_byte);
    }

    X64_write_addr_disp(gen_state, &dst_addr);

    if (imm_is_byte) {
        array_push(gen_state->curr_bblock->buffer, imm); // imm8
    }
    else {
        X64_write_imm_bytes(gen_state, imm, is_64_bit ? 4 : dst_size);
    }
}

static inline void X64_write_elf_binary_instr_ri(X64_TextGenState* gen_state, u8 opcode_instr_1byte, u8 opcode_imm_1byte,
                                                 u8 opcode_imm_larger, u8 opcode_ext, u8 dst_size, u8 dst, u32 imm)
{
    const bool is_64_bit = dst_size == 8;
    const u8 dst_reg = x64_reg_val[dst];
    const bool imm_is_byte = imm <= 255;
    const bool reg_is_ext = dst_reg > 7;
    const u8 opcode = (dst_size == 1) ? opcode_instr_1byte : (imm_is_byte ? opcode_imm_1byte : opcode_imm_larger);

    if (dst_size == 2) {
        array_push(gen_state->curr_bblock->buffer, 0x66); // 0x66 for 16-bit operands
    }

    if (is_64_bit || reg_is_ext) {
        array_push(gen_state->curr_bblock->buffer, X64_rex_nosib(is_64_bit, 0, dst_reg)); // REX.B for ext regs
    }

    array_push(gen_state->curr_bblock->buffer, opcode);
    array_push(gen_state->curr_bblock->buffer,
               X64_modrm_byte(X64_MOD_DIRECT, opcode_ext, dst_reg)); // opcode_ext in reg, rm for dst_reg

    if (imm_is_byte) {
        array_push(gen_state->curr_bblock->buffer, imm); // imm8
    }
    else {
        X64_write_imm_bytes(gen_state, imm, is_64_bit ? 4 : dst_size);
    }
}

static inline void X64_write_elf_binary_instr_rr(X64_TextGenState* gen_state, u8 opcode_1byte, u8 opcode_multi_byte, u8 size, u8 dst,
                                                 u8 src)
{
    const u8 src_reg = x64_reg_val[src];
    const u8 dst_reg = x64_reg_val[dst];
    const bool src_is_ext = src_reg > 7;
    const bool dst_is_ext = dst_reg > 7;
    const bool is_64_bit = size == 8;
    const u8 opcode = size == 1 ? opcode_1byte : opcode_multi_byte;

    if (size == 2) {
        array_push(gen_state->curr_bblock->buffer, 0x66); // 0x66 for 16-bit operands
    }

    if (is_64_bit || src_is_ext || dst_is_ext) {
        array_push(gen_state->curr_bblock->buffer, X64_rex_nosib(is_64_bit, src_reg, dst_reg)); // REX.RB for ext regs
    }

    array_push(gen_state->curr_bblock->buffer, opcode);
    array_push(gen_state->curr_bblock->buffer, X64_modrm_byte(X64_MOD_DIRECT, src_reg, dst_reg)); // ModRM
}

static inline void X64_write_elf_binary_instr_mr(X64_TextGenState* gen_state, u8 opcode_1byte, u8 opcode_multi_byte, u8 size,
                                                 X64_SIBD_Addr* dst, u8 src)
{
    const u8 src_reg = x64_reg_val[src];
    const X64_AddrBytes dst_addr = X64_get_addr_bytes(dst);
    const bool src_is_ext = src_reg > 7;
    const bool use_ext_regs = src_is_ext || dst_addr.rex_b || dst_addr.rex_x;
    const bool is_64_bit = size == 8;
    const u8 opcode = size == 1 ? opcode_1byte : opcode_multi_byte;

    // 0x66 prefix for 16-bit operands.
    if (size == 2) {
        array_push(gen_state->curr_bblock->buffer, 0x66);
    }

    // REX prefix.
    if (use_ext_regs || is_64_bit) {
        array_push(gen_state->curr_bblock->buffer, X64_rex_prefix(is_64_bit, src_reg >> 3, dst_addr.rex_x, dst_addr.rex_b));
    }

    array_push(gen_state->curr_bblock->buffer, opcode); // opcode
    array_push(gen_state->curr_bblock->buffer, X64_modrm_byte(dst_addr.mod, src_reg, dst_addr.rm)); // ModRM byte.

    if (dst_addr.has_sib_byte) {
        array_push(gen_state->curr_bblock->buffer, dst_addr.sib_byte);
    }

    X64_write_addr_disp(gen_state, &dst_addr);
}

static s32 X64_get_bblock_best_case_size(X64_TextBBlock* bblock)
{
    assert(bblock);
    s32 base_size = array_len(bblock->buffer);
    s32 jmp_opcode_len = bblock->jmp_opcode_len;
    if (jmp_opcode_len == 0 && bblock->jmp_instr) {
        jmp_opcode_len = 1;
    }
    s32 jmp_offset_len = bblock->jmp_offset_len;
    if (jmp_offset_len == 0 && bblock->jmp_instr) {
        jmp_offset_len = 1;
    }

    return base_size + jmp_opcode_len + jmp_offset_len;
}

static bool X64_update_jmp(X64_TextBBlock* bblock, X64_TextBBlock* bblocks)
{
    if (!bblock->jmp_instr) {
        bblock->jmp_opcode_len = 0;
        bblock->jmp_offset_len = 0;
        return false;
    }

    u32 src_index = bblock->index;
    u32 dst_index = 0;

    const X64_Instr_Kind kind = X64_get_instr_kind(bblock->jmp_instr);
    switch (kind) {
    case X64_Instr_Kind_JMP:
    case X64_Instr_Kind_JMP_TO_RET:
        dst_index = bblock->jmp_instr->jmp.target;
        break;
    case X64_Instr_Kind_JMPCC:
        dst_index = bblock->jmp_instr->jmpcc.target;
        break;
    default:
        NIBBLE_FATAL_EXIT("Unexpected jmp instruction kind: %d\n", kind);
        break;
    }

    s32 offset = 0;

    if (dst_index > src_index) {
        // Add size of bblocks between src and dst.
        // If we don't know the size of the jmp instrs yet, assume best case scenario.
        for (u32 bb = src_index + 1; bb < dst_index; bb += 1) {
            offset += X64_get_bblock_best_case_size(&bblocks[bb]);
        }
    }
    else if (src_index > dst_index) {
        for (u32 bb = dst_index; bb < src_index; bb += 1) {
            offset -= X64_get_bblock_best_case_size(&bblocks[bb]);
        }

        // Add in this block's other instructions
        offset -= array_len(bblock->buffer);

        assert(offset <= 0);

        // Add in opcode length (1 or 2) and distance to next instruction after the jmp (1 or 4).
        if (offset >= (-128 + 2)) {
            offset -= 2; // opcode and offset are 1 byte each.
        }
        else {
            // jmpcc: 2 byte opcode + 4 byte offset
            // jmp: 1 byte opcode + 4 byte offset
            offset -= (kind == X64_Instr_Kind_JMPCC ? 6 : 5);
        }
    }
    else {
        NIBBLE_FATAL_EXIT("Unexpected jmp target == jmp source");
    }

    bool changed_size = false;

    if (bblock->jmp_opcode_len == 0 || bblock->jmp_offset_len == 0 || bblock->jmp_offset._s32 != offset) {
        // If this is the first time we're updating the offset (i.e., jmp_offset_len == 0), assume it fits in a byte.
        const bool old_offset_fits_in_byte = bblock->jmp_offset_len <= 1;
        assert(!old_offset_fits_in_byte || (bblock->jmp_offset._s32 >= -128 && bblock->jmp_offset._s32 <= 127));

        const bool new_offset_fits_in_byte = offset >= -128 && offset <= 127;

        changed_size = old_offset_fits_in_byte != new_offset_fits_in_byte;

        switch (kind) {
        case X64_Instr_Kind_JMP:
        case X64_Instr_Kind_JMP_TO_RET: {
            bblock->jmp_opcode_len = 1;
            bblock->jmp_opcode._u16 = new_offset_fits_in_byte ? 0xEB : 0xE9;
            bblock->jmp_offset_len = new_offset_fits_in_byte ? 1 : 4;
            bblock->jmp_offset._s32 = offset;
        } break;
        case X64_Instr_Kind_JMPCC: {
            const ConditionKind cond = bblock->jmp_instr->jmpcc.cond;
            bblock->jmp_opcode_len = new_offset_fits_in_byte ? 1 : 2;
            bblock->jmp_opcode._u16 =
                new_offset_fits_in_byte ? x64_jmpcc_short_condition_opcodes[cond] : x64_jmpcc_near_condition_opcodes[cond];
            bblock->jmp_offset_len = new_offset_fits_in_byte ? 1 : 4;
            bblock->jmp_offset._s32 = offset;
        } break;
        default:
            NIBBLE_FATAL_EXIT("Unexpected jmp instruction kind: %d\n", kind);
            break;
        }
    }

    return changed_size;
}

//
// TODO(adrian): IMPORTANT(adrian): Change instruciton kinds to include operator size.
// For example, X64_Instr_Kind_MOV_RR_4, X64_Instr_Kind_MOV_RR_8, etc.
// This will remove all the branching on 'size' within each switch case.
//
static void X64_elf_gen_instr(X64_TextGenState* gen_state, X64_Instr* instr)
{
    X64_TextBBlock* bblock = gen_state->curr_bblock;
    const X64_Instr_Kind kind = X64_get_instr_kind(instr);

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
            array_push(bblock->buffer, X64_rex_opcode_reg(0, reg_val)); // REX.B
        }

        array_push(bblock->buffer, opcode);
    } break;
    case X64_Instr_Kind_POP: {
        // We only pop r64, which has opcode 0x58 + REG_VAL[2:0]. If reg is >= R8, set REX.B.
        const u8 reg_val = x64_reg_val[instr->pop.reg];
        const u8 opcode = 0x58 + (reg_val & 0x7);
        const bool use_ext_regs = reg_val > 7;

        if (use_ext_regs) {
            array_push(bblock->buffer, X64_rex_opcode_reg(0, reg_val)); // REX.B
        }

        array_push(bblock->buffer, opcode);
    } break;
    case X64_Instr_Kind_JMP_TO_RET:
    case X64_Instr_Kind_JMPCC:
    case X64_Instr_Kind_JMP: {
        assert(!bblock->jmp_instr);
        bblock->jmp_instr = instr;
    } break;
    // RET
    case X64_Instr_Kind_RET: {
        array_push(bblock->buffer, 0xc3); // opcode is 0xc3 for near return to calling proc.
    } break;
    // LEA
    case X64_Instr_Kind_LEA: {
        // REX.W + 8D /r => lea r64, m
        const u8 dst_reg = x64_reg_val[instr->lea.dst];
        const X64_AddrBytes src_addr = X64_get_addr_bytes(&instr->lea.src);

        array_push(bblock->buffer, X64_rex_prefix(1, dst_reg >> 3, src_addr.rex_x, src_addr.rex_b)); // REX
        array_push(bblock->buffer, 0x8D); // opcode
        array_push(bblock->buffer, X64_modrm_byte(src_addr.mod, dst_reg & 0x7, src_addr.rm)); // ModRM byte.

        if (src_addr.has_sib_byte) {
            array_push(bblock->buffer, src_addr.sib_byte);
        }

        X64_write_addr_disp(gen_state, &src_addr);
    } break;
    // ADD
    case X64_Instr_Kind_ADD_RR: {
        // 00 /r => add r/m8, r8
        // 0x66 + 01 /r => add r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 01 /r => add r/m32, r32
        // REX.W + 01 /r => add r/m64, r64
        X64_write_elf_binary_instr_rr(gen_state, 0x00, 0x01, instr->add_rr.size, instr->add_rr.dst, instr->add_rr.src);
    } break;
    case X64_Instr_Kind_ADD_RM: {
        // 02 /r => add r8, r/m8
        // 66 03 /r => add r16, r/m16
        // 03 /r => add r32, r/m32
        // REX.W + 03 /r => add r64, r/m64
        X64_write_elf_binary_instr_rm(gen_state, 0x02, 0x03, instr->add_rm.size, instr->add_rm.dst, &instr->add_rm.src);
    } break;
    case X64_Instr_Kind_ADD_MR: {
        // 00 /r => add r/m8, r8
        // 0x66 + 01 /r => add r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 01 /r => add r/m32, r32
        // REX.W + 01 /r => add r/m64, r64
        X64_write_elf_binary_instr_mr(gen_state, 0x00, 0x01, instr->add_mr.size, &instr->add_mr.dst, instr->add_mr.src);
    } break;
    case X64_Instr_Kind_ADD_RI: {
        // 80 /0 ib => add r/m8, imm8
        // 66 83 /0 ib => add r/m16, imm8
        // 66 81 /0 iw => add r/m16, imm16
        // 83 /0 ib => add r/m32, imm8
        // 81 /0 id => add r/m32, imm32
        // REX.W + 83 /0 ib => add r/m64, imm8
        // REX.W + 81 /0 id => add r/m64, imm32
        X64_write_elf_binary_instr_ri(gen_state, 0x80, 0x83, 0x81, 0 /*opcode ext*/, instr->add_ri.size, instr->add_ri.dst,
                                      instr->add_ri.imm);
    } break;
    case X64_Instr_Kind_ADD_MI: {
        // 80 /0 ib => add r/m8, imm8
        // 66 83 /0 ib => add r/m16, imm8
        // 66 81 /0 iw => add r/m16, imm16
        // 83 /0 ib => add r/m32, imm8
        // 81 /0 id => add r/m32, imm32
        // REX.W + 83 /0 ib => add r/m64, imm8
        // REX.W + 81 /0 id => add r/m64, imm32
        X64_write_elf_binary_instr_mi(gen_state, 0x80, 0x83, 0x81, 0 /*opcode ext*/, instr->add_mi.size, &instr->add_mi.dst,
                                      instr->add_mi.imm);
    } break;
    // SUB
    case X64_Instr_Kind_SUB_RR: {
        // 28 /r => sub r/m8, r8
        // 0x66 + 29 /r => sub r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 29 /r => sub r/m32, r32
        // REX.W + 29 /r => sub r/m64, r64
        X64_write_elf_binary_instr_rr(gen_state, 0x28, 0x29, instr->sub_rr.size, instr->sub_rr.dst, instr->sub_rr.src);
    } break;
    case X64_Instr_Kind_SUB_RM: {
        // 2A /r => sub r8, r/m8
        // 66 2B /r => sub r16, r/m16
        // 2B /r => sub r32, r/m32
        // REX.W + 2B /r => sub r64, r/m64
        X64_write_elf_binary_instr_rm(gen_state, 0x2A, 0x2B, instr->sub_rm.size, instr->sub_rm.dst, &instr->sub_rm.src);
    } break;
    case X64_Instr_Kind_SUB_MR: {
        // 28 /r => sub r/m8, r8
        // 0x66 + 29 /r => sub r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 29 /r => sub r/m32, r32
        // REX.W + 29 /r => sub r/m64, r64
        X64_write_elf_binary_instr_mr(gen_state, 0x28, 0x29, instr->sub_mr.size, &instr->sub_mr.dst, instr->sub_mr.src);
    } break;
    case X64_Instr_Kind_SUB_RI: {
        // 80 /5 ib => sub r/m8, imm8
        // 66 83 /5 ib => sub r/m16, imm8
        // 66 81 /5 iw => sub r/m16, imm16
        // 83 /5 ib => sub r/m32, imm8
        // 81 /5 id => sub r/m32, imm32
        // REX.W + 83 /5 ib => sub r/m64, imm8
        // REX.W + 81 /5 id => sub r/m64, imm32
        X64_write_elf_binary_instr_ri(gen_state, 0x80, 0x83, 0x81, 5 /*opcode ext*/, instr->sub_ri.size, instr->sub_ri.dst,
                                      instr->sub_ri.imm);
    } break;
    case X64_Instr_Kind_SUB_MI: {
        // 80 /5 ib => sub r/m8, imm8
        // 66 83 /5 ib => sub r/m16, imm8
        // 66 81 /5 iw => sub r/m16, imm16
        // 83 /5 ib => sub r/m32, imm8
        // 81 /5 id => sub r/m32, imm32
        // REX.W + 83 /5 ib => sub r/m64, imm8
        // REX.W + 81 /5 id => sub r/m64, imm32
        X64_write_elf_binary_instr_mi(gen_state, 0x80, 0x83, 0x81, 5 /*opcode ext*/, instr->sub_mi.size, &instr->sub_mi.dst,
                                      instr->sub_mi.imm);
    } break;
    // CMP
    case X64_Instr_Kind_CMP_RR: {
        // 38 /r => cmp r/m8, r8
        // 0x66 + 39 /r => cmp r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 39 /r => cmp r/m32, r32
        // REX.W + 39 /r => cmp r/m64, r64
        X64_write_elf_binary_instr_rr(gen_state, 0x38, 0x39, instr->cmp_rr.size, instr->cmp_rr.dst, instr->cmp_rr.src);
    } break;
    case X64_Instr_Kind_CMP_RM: {
        // 3A /r => cmp r8, r/m8
        // 66 3B /r => cmp r16, r/m16
        // 3B /r => cmp r32, r/m32
        // REX.W + 3B /r => cmp r64, r/m64
        X64_write_elf_binary_instr_rm(gen_state, 0x3A, 0x3B, instr->cmp_rm.size, instr->cmp_rm.dst, &instr->cmp_rm.src);
    } break;
    case X64_Instr_Kind_CMP_MR: {
        // 38 /r => cmp r/m8, r8
        // 0x66 + 39 /r => cmp r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 39 /r => cmp r/m32, r32
        // REX.W + 39 /r => cmp r/m64, r64
        X64_write_elf_binary_instr_mr(gen_state, 0x38, 0x39, instr->cmp_mr.size, &instr->cmp_mr.dst, instr->cmp_mr.src);
    } break;
    case X64_Instr_Kind_CMP_RI: {
        // 80 /7 ib => cmp r/m8, imm8
        // 66 83 /7 ib => cmp r/m16, imm8
        // 66 81 /7 iw => cmp r/m16, imm16
        // 83 /7 ib => cmp r/m32, imm8
        // 81 /7 id => cmp r/m32, imm32
        // REX.W + 83 /7 ib => cmp r/m64, imm8
        // REX.W + 81 /7 id => cmp r/m64, imm32
        X64_write_elf_binary_instr_ri(gen_state, 0x80, 0x83, 0x81, 7 /*opcode ext*/, instr->cmp_ri.size, instr->cmp_ri.dst,
                                      instr->cmp_ri.imm);
    } break;
    case X64_Instr_Kind_CMP_MI: {
        // 80 /7 ib => cmp r/m8, imm8
        // 66 83 /7 ib => cmp r/m16, imm8
        // 66 81 /7 iw => cmp r/m16, imm16
        // 83 /7 ib => cmp r/m32, imm8
        // 81 /7 id => cmp r/m32, imm32
        // REX.W + 83 /7 ib => cmp r/m64, imm8
        // REX.W + 81 /7 id => cmp r/m64, imm32
        X64_write_elf_binary_instr_mi(gen_state, 0x80, 0x83, 0x81, 7 /*opcode ext*/, instr->cmp_mi.size, &instr->cmp_mi.dst,
                                      instr->cmp_mi.imm);
    } break;
    // SETcc
    case X64_Instr_Kind_SETCC_R: {
        const u8 dst_reg = x64_reg_val[instr->setcc_r.dst];
        const bool dst_is_ext = dst_reg > 7;
        const u16 opcode = x64_setcc_condition_opcodes[instr->setcc_r.cond];

        if (dst_is_ext) {
            array_push(bblock->buffer, X64_rex_nosib(0, 0, dst_reg));
        }

        array_push(bblock->buffer, opcode & 0x00FF); // lower opcode byte
        array_push(bblock->buffer, (opcode >> 8) & 0x00FF); // higher opcode byte
        array_push(bblock->buffer, X64_modrm_byte(X64_MOD_DIRECT, 0, dst_reg)); // ModRM
    } break;
    case X64_Instr_Kind_SETCC_M: {
        const X64_AddrBytes dst_addr = X64_get_addr_bytes(&instr->setcc_m.dst);
        const bool use_ext_regs = dst_addr.rex_b || dst_addr.rex_x;
        const u16 opcode = x64_setcc_condition_opcodes[instr->setcc_r.cond];

        if (use_ext_regs) {
            array_push(bblock->buffer, X64_rex_prefix(0, 0, dst_addr.rex_x, dst_addr.rex_b));
        }

        array_push(bblock->buffer, opcode & 0x00FF); // lower opcode byte
        array_push(bblock->buffer, (opcode >> 8) & 0x00FF); // higher opcode byte
        array_push(bblock->buffer, X64_modrm_byte(dst_addr.mod, 0, dst_addr.rm)); // ModRM byte.

        if (dst_addr.has_sib_byte) {
            array_push(bblock->buffer, dst_addr.sib_byte);
        }

        X64_write_addr_disp(gen_state, &dst_addr);
    } break;
    // MOVSX
    case X64_Instr_Kind_MOVSX_RR: {
        // 0F BE /r => movsx r16, r/m8 (need 0x66 prefix for 16-bit dst)
        // 0F BE /r => movsx r32, r/m8
        // REX.W 0F BE /r => movsx r64, r/m8
        // 0F BF /r => movsx r32, r/m16
        // REX.W 0F BF /r => movsx r64, r/m16
        //
        // Operand encoding: operand 1: ModRM:reg (w), operand 2: ModRM:rm (r)
        const u8 dst_size = instr->movsx_rr.dst_size;
        const u8 src_size = instr->movsx_rr.src_size;
        const u16 opcode = src_size == 1 ? 0xBE0F : 0xBF0F;
        const bool need_rex_w = dst_size == 8;
        const u8 dst_reg = x64_reg_val[instr->movsx_rr.dst];
        const u8 src_reg = x64_reg_val[instr->movsx_rr.src];
        const bool src_is_ext = src_reg > 7;
        const bool dst_is_ext = dst_reg > 7;

        if (dst_size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 for 16-bit dst
        }

        if (need_rex_w || src_is_ext || dst_is_ext) {
            array_push(bblock->buffer, X64_rex_nosib(need_rex_w, dst_reg, src_reg)); // REX.RB for ext regs
        }

        array_push(bblock->buffer, opcode & 0x00FF); // lower opcode byte
        array_push(bblock->buffer, (opcode >> 8) & 0x00FF); // higher opcode byte
        array_push(bblock->buffer, X64_modrm_byte(X64_MOD_DIRECT, dst_reg, src_reg)); // ModRM
    } break;
    case X64_Instr_Kind_MOVSX_RM: {
        // 0F BE /r => movsx r16, r/m8 (need 0x66 prefix for 16-bit dst)
        // 0F BE /r => movsx r32, r/m8
        // REX.W 0F BE /r => movsx r64, r/m8
        // 0F BF /r => movsx r32, r/m16
        // REX.W 0F BF /r => movsx r64, r/m16
        //
        // Operand encoding: operand 1: ModRM:reg (w), operand 2: ModRM:rm (r)
        const u8 dst_size = instr->movsx_rm.dst_size;
        const u8 src_size = instr->movsx_rm.src_size;
        const u16 opcode = src_size == 1 ? 0xBE0F : 0xBF0F;
        const bool need_rex_w = dst_size == 8;
        const u8 dst_reg = x64_reg_val[instr->movsx_rm.dst];
        const X64_AddrBytes src_addr = X64_get_addr_bytes(&instr->movsx_rm.src);
        const bool dst_is_ext = dst_reg > 7;
        const bool use_ext_regs = dst_is_ext || src_addr.rex_b || src_addr.rex_x;

        if (dst_size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 for 16-bit dst
        }

        if (need_rex_w || use_ext_regs) {
            array_push(bblock->buffer, X64_rex_prefix(need_rex_w, dst_reg >> 3, src_addr.rex_x, src_addr.rex_b));
        }

        array_push(bblock->buffer, opcode & 0x00FF); // lower opcode byte
        array_push(bblock->buffer, (opcode >> 8) & 0x00FF); // higher opcode byte
        array_push(bblock->buffer, X64_modrm_byte(src_addr.mod, dst_reg, src_addr.rm)); // ModRM

        if (src_addr.has_sib_byte) {
            array_push(bblock->buffer, src_addr.sib_byte);
        }

        X64_write_addr_disp(gen_state, &src_addr);
    } break;
    // MOVZX
    case X64_Instr_Kind_MOVZX_RR: {
        // 0F B6 /r => movzx r16, r/m8 (need 0x66 prefix for 16-bit dst)
        // 0F B6 /r => movzx r32, r/m8
        // REX.W 0F B6 /r => movzx r64, r/m8
        // 0F B7 /r => movzx r32, r/m16
        // REX.W 0F B7 /r => movzx r64, r/m16
        //
        // Operand encoding: operand 1: ModRM:reg (w), operand 2: ModRM:rm (r)
        const u8 dst_size = instr->movzx_rr.dst_size;
        const u8 src_size = instr->movzx_rr.src_size;
        const u16 opcode = src_size == 1 ? 0xB60F : 0xB70F;
        const bool need_rex_w = dst_size == 8;
        const u8 dst_reg = x64_reg_val[instr->movzx_rr.dst];
        const u8 src_reg = x64_reg_val[instr->movzx_rr.src];
        const bool src_is_ext = src_reg > 7;
        const bool dst_is_ext = dst_reg > 7;

        if (dst_size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 for 16-bit dst
        }

        if (need_rex_w || src_is_ext || dst_is_ext) {
            array_push(bblock->buffer, X64_rex_nosib(need_rex_w, dst_reg, src_reg)); // REX.RB for ext regs
        }

        array_push(bblock->buffer, opcode & 0x00FF); // lower opcode byte
        array_push(bblock->buffer, (opcode >> 8) & 0x00FF); // higher opcode byte
        array_push(bblock->buffer, X64_modrm_byte(X64_MOD_DIRECT, dst_reg, src_reg)); // ModRM
    } break;
    case X64_Instr_Kind_MOVZX_RM: {
        // 0F B6 /r => movzx r16, r/m8 (need 0x66 prefix for 16-bit dst)
        // 0F B6 /r => movzx r32, r/m8
        // REX.W 0F B6 /r => movzx r64, r/m8
        // 0F B7 /r => movzx r32, r/m16
        // REX.W 0F B7 /r => movzx r64, r/m16
        //
        // Operand encoding: operand 1: ModRM:reg (w), operand 2: ModRM:rm (r)
        const u8 dst_size = instr->movzx_rm.dst_size;
        const u8 src_size = instr->movzx_rm.src_size;
        const u16 opcode = src_size == 1 ? 0xB60F : 0xB70F;
        const bool need_rex_w = dst_size == 8;
        const u8 dst_reg = x64_reg_val[instr->movzx_rm.dst];
        const X64_AddrBytes src_addr = X64_get_addr_bytes(&instr->movzx_rm.src);
        const bool dst_is_ext = dst_reg > 7;
        const bool use_ext_regs = dst_is_ext || src_addr.rex_b || src_addr.rex_x;

        if (dst_size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 for 16-bit dst
        }

        if (need_rex_w || use_ext_regs) {
            array_push(bblock->buffer, X64_rex_prefix(need_rex_w, dst_reg >> 3, src_addr.rex_x, src_addr.rex_b));
        }

        array_push(bblock->buffer, opcode & 0x00FF); // lower opcode byte
        array_push(bblock->buffer, (opcode >> 8) & 0x00FF); // higher opcode byte
        array_push(bblock->buffer, X64_modrm_byte(src_addr.mod, dst_reg, src_addr.rm)); // ModRM

        if (src_addr.has_sib_byte) {
            array_push(bblock->buffer, src_addr.sib_byte);
        }

        X64_write_addr_disp(gen_state, &src_addr);
    } break;
    // MOV
    case X64_Instr_Kind_MOV_RR: {
        // 88 /r => mov r/m8, r8
        // 0x66 + 89 /r => mov r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 89 /r => mov r/m32, r32
        // REX.W + 89 /r => mov r/m64, r64
        const u8 size = instr->mov_rr.size;
        const u8 src_reg = x64_reg_val[instr->mov_rr.src];
        const u8 dst_reg = x64_reg_val[instr->mov_rr.dst];
        const bool src_is_ext = src_reg > 7;
        const bool dst_is_ext = dst_reg > 7;
        const bool is_64_bit = size == 8;
        const u8 opcode = size == 1 ? 0x88 : 0x89;

        if (size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 for 16-bit operands
        }

        if (is_64_bit || src_is_ext || dst_is_ext) {
            array_push(bblock->buffer, X64_rex_nosib(is_64_bit, src_reg, dst_reg)); // REX.RB for ext regs
        }

        array_push(bblock->buffer, opcode);
        array_push(bblock->buffer, X64_modrm_byte(X64_MOD_DIRECT, src_reg, dst_reg)); // ModRM

        // TODO(adrianlizarraga): Handle source register that use 1-byte high slot (e.g., mov rdi, ah)
        // This is used by mod operations when the arguments are < 2 bytes (see conver_ir.c).
        // const bool is_r2_h = instr->mov_rr.size == 1 && (instr->flags & X64_INSTR_MOV_SRC_RH_MASK); // Use high 1 byte reg.
    } break;
    case X64_Instr_Kind_MOV_MR: {
        // 88 /r => mov r/m8, r8
        // 0x66 + 89 /r => mov r/m16, r16 (NEED 0x66 prefix for 16-bit operands)
        // 89 /r => mov r/m32, r32
        // REX.W + 89 /r => mov r/m64, r64
        const u8 size = instr->mov_mr.size;
        const u8 src_reg = x64_reg_val[instr->mov_mr.src];
        const X64_AddrBytes dst_addr = X64_get_addr_bytes(&instr->mov_mr.dst);
        const bool src_is_ext = src_reg > 7;
        const bool use_ext_regs = src_is_ext || dst_addr.rex_b || dst_addr.rex_x;
        const bool is_64_bit = size == 8;
        const u8 opcode = size == 1 ? 0x88 : 0x89;

        // 0x66 prefix for 16-bit operands.
        if (size == 2) {
            array_push(bblock->buffer, 0x66);
        }

        // REX prefix.
        if (use_ext_regs || is_64_bit) {
            array_push(bblock->buffer, X64_rex_prefix(is_64_bit, src_reg >> 3, dst_addr.rex_x, dst_addr.rex_b));
        }

        array_push(bblock->buffer, opcode); // opcode
        array_push(bblock->buffer, X64_modrm_byte(dst_addr.mod, src_reg, dst_addr.rm)); // ModRM byte.

        if (dst_addr.has_sib_byte) {
            array_push(bblock->buffer, dst_addr.sib_byte);
        }

        X64_write_addr_disp(gen_state, &dst_addr);

        // TODO(adrianlizarraga): Handle source register that use 1-byte high slot (e.g., mov rdi, ah)
        // This is used by mod operations when the arguments are < 2 bytes (see conver_ir.c).
        // const bool is_r2_h = instr->mov_mr.size == 1 && (instr->flags & X64_INSTR_MOV_SRC_RH_MASK); // Use high 1 byte reg.
    } break;
    case X64_Instr_Kind_MOV_RM: {
        // 8A /r => mov r8, r/m8
        // 66 8B /r => mov r16, r/m16
        // 8B /r => mov r32, r/m32
        // REX.W + 8B /r => mov r64, r/m64
        X64_write_elf_binary_instr_rm(gen_state, 0x8A, 0x8B, instr->mov_rm.size, instr->mov_rm.dst, &instr->mov_rm.src);
    } break;
    case X64_Instr_Kind_MOV_MI: {
        // C6 /0 ib => mov r/m8, imm8
        // 66 C7 /0 iw => mov r/m16, imm16
        // C7 /0 id => mov r/m32, imm32
        // REX.W + C7 /0 id => mov r/m64, imm32
        const u8 size = instr->mov_mi.size;
        const X64_AddrBytes addr_bytes = X64_get_addr_bytes(&instr->mov_mi.dst);
        const bool use_ext_regs = addr_bytes.rex_b || addr_bytes.rex_x;
        const bool is_64_bit = size == 8;
        const u8 opcode = size == 1 ? 0xC6 : 0xC7;

        if (size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 for 16-bit operands
        }

        if (is_64_bit || use_ext_regs) {
            array_push(bblock->buffer, X64_rex_prefix(is_64_bit, 0, addr_bytes.rex_x, addr_bytes.rex_b)); // REX prefix
        }

        array_push(bblock->buffer, opcode); // opcode
        array_push(bblock->buffer, X64_modrm_byte(addr_bytes.mod, 0, addr_bytes.rm)); // 0 is opcode extension (i.e., the /0)

        if (addr_bytes.has_sib_byte) {
            array_push(bblock->buffer, addr_bytes.sib_byte);
        }

        X64_write_addr_disp(gen_state, &addr_bytes);
        X64_write_imm_bytes(gen_state, instr->mov_mi.imm, is_64_bit ? 4 : size);
    } break;
    case X64_Instr_Kind_MOV_RI: {
        // NOTE: This is the only instruction that can load a 64-bit immediate.
        // Does not use a modR/M byte. Instead, REX.B and the lower 3 bits of the opcode represent the
        // destination register.
        //
        // B0+rb ib => mov r8, imm8
        // 66 + B8+rw iw => mov r16, imm16
        // B8+rd id => mov r32, imm32
        // REX.W + B8+rd io => mov r64, imm64
        const u8 size = instr->mov_ri.size;
        const u8 dst_reg = x64_reg_val[instr->mov_ri.dst];
        const bool use_ext_regs = dst_reg > 7; // Need REX.B?
        const bool is_64_bit = size == 8;
        const u8 base_opcode = (size == 1) ? 0xB0 : 0xB8;

        if (size == 2) {
            array_push(bblock->buffer, 0x66); // 0x66 prefix for 16-bit operands
        }

        if (is_64_bit || use_ext_regs) {
            array_push(bblock->buffer, X64_rex_opcode_reg(is_64_bit, dst_reg));
        }

        array_push(bblock->buffer, base_opcode + (dst_reg & 0x7)); // opcode + lower 3 bits of dst register

        X64_write_imm_bytes(gen_state, instr->mov_ri.imm, size);
    } break;
    default:
        NIBBLE_FATAL_EXIT("Unknown X64 instruction kind %d\n", kind);
        break;
    }
}

static void X64_elf_gen_proc_text(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym, Array(u8) * buffer,
                                  Array(X64_TextReloc) * relocs, Array(X64_TextReloc) * proc_off_patches)
{
    const DeclProc* decl = (const DeclProc*)proc_sym->decl;
    if (decl->is_incomplete) {
        return;
    }

    AllocatorState tmp_mem_state = allocator_get_state(tmp_mem);

    X64_Instrs instrs = X64_gen_proc_instrs(gen_mem, tmp_mem, proc_sym); // TODO: X64_gen_*(tmp_mem, new_tmp_arena, proc_sym)
    const u32 num_bblocks = array_len(instrs.bblocks);
    X64_TextBBlock* text_bblocks = alloc_array(tmp_mem, X64_TextBBlock, num_bblocks, true);
    X64_TextGenState gen_state = {.relocs = array_create(tmp_mem, X64_InternalReloc, 8),
                                  .proc_off_patches = array_create(tmp_mem, X64_InternalReloc, 8)};

    // Loop throught X64 instructions and write out machine code to buffer.
    for (u32 bb = 0; bb < num_bblocks; bb += 1) {
        X64_TextBBlock* bblock = &text_bblocks[bb];
        bblock->index = bb;
        bblock->buffer = array_create(tmp_mem, u8, 32);

        gen_state.curr_bblock = bblock;

        for (X64_Instr* instr = instrs.bblocks[bb].head; instr; instr = instr->next) {
            X64_elf_gen_instr(&gen_state, instr);
        }
    }

    // Process jumps while they keep changing size (until reach steady-state)
    //
    // NOTE: Jump instructions in x64 can be encoded with a varying number of bytes, which depends on the
    // byte-size of the target offset. If the offset fits in a byte, we can use the short version of the instruction.
    // Otherwise, we must use the long version of the instruction. Importantly, changing the total byte-size of one jump
    // instruction may require changing the size of another jump instruction, and so on...
    //
    // This iterative algorithm initially encodes jump instructions assuming that the offset will
    // fit in a single byte. However, if this assumption is violated, the algorithm must iterate again until we reach
    // a steady-state.
    {
        bool update_jmps = true;

        while (update_jmps) {
            update_jmps = false;

            for (u32 bb = 0; bb < num_bblocks; bb += 1) {
                if (X64_update_jmp(&text_bblocks[bb], text_bblocks)) {
                    update_jmps = true;
                }
            }
        }
    }

    // Serialize bblocks into destination buffer and set bblock final offsets.
    for (u32 bb = 0; bb < num_bblocks; bb += 1) {
        X64_TextBBlock* bblock = &text_bblocks[bb];

        bblock->final_offset = (s64)array_len(*buffer);

        // Copy basic block's instruction bytes and final jump instruction into destination buffer.
        array_push_elems(*buffer, bblock->buffer, array_len(bblock->buffer));

        if (bblock->jmp_instr) {
            assert(bblock->jmp_opcode_len > 0 && bblock->jmp_offset_len > 0);
            array_push_elems(*buffer, bblock->jmp_opcode.bytes, bblock->jmp_opcode_len);
            array_push_elems(*buffer, bblock->jmp_offset.bytes, bblock->jmp_offset_len);
        }
    }

    // Add this proc's relocations (global vars/data, foreign procs) into destination array.
    const size_t num_proc_relocs = array_len(gen_state.relocs);
    for (size_t i = 0; i < num_proc_relocs; i++) {
        X64_InternalReloc* internal_reloc = &gen_state.relocs[i];
        X64_UsageLoc* usage_loc = &internal_reloc->usage_loc;
        X64_TextBBlock* bblock = &text_bblocks[internal_reloc->usage_loc.bblock_index];

        s64 usage_off = bblock->final_offset + usage_loc->bblock_offset;
        array_push(*relocs, (X64_TextReloc){.usage_off = usage_off,
                                            .bytes_to_next_ip = internal_reloc->bytes_to_next_ip,
                                            .ref_addr = internal_reloc->ref_addr});
    }

    // Add this proc's patches (for usages of non-foreign procs) into destination array.
    const size_t num_proc_off_patches = array_len(gen_state.proc_off_patches);
    for (size_t i = 0; i < num_proc_off_patches; i++) {
        X64_InternalReloc* internal_reloc = &gen_state.proc_off_patches[i];
        X64_UsageLoc* usage_loc = &internal_reloc->usage_loc;
        X64_TextBBlock* bblock = &text_bblocks[internal_reloc->usage_loc.bblock_index];

        s64 usage_off = bblock->final_offset + usage_loc->bblock_offset;
        array_push(*proc_off_patches, (X64_TextReloc){.usage_off = usage_off,
                                                      .bytes_to_next_ip = internal_reloc->bytes_to_next_ip,
                                                      .ref_addr = internal_reloc->ref_addr});
    }

    allocator_restore_state(tmp_mem_state);
}

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, BucketList* procs,
                           const Symbol* main_proc)
{
    AllocatorState tmp_mem_state = allocator_get_state(tmp_mem);

    const size_t startup_code_size = sizeof(startup_code);
    const size_t num_procs = procs->num_elems;

    Array(u8) buffer = array_create(gen_mem, u8, startup_code_size << 2);
    u64* proc_offsets = alloc_array(gen_mem, u64, num_procs, true); // Symbol::index -> starting offset in buffer

    Array(X64_TextReloc) relocs = array_create(gen_mem, X64_TextReloc, 32);
    Array(X64_TextReloc) proc_off_patches = array_create(gen_mem, X64_TextReloc, num_procs);

    array_push_elems(buffer, startup_code, startup_code_size); // Add _start code.

    for (Bucket* bucket = procs->first; bucket; bucket = bucket->next) {
        for (size_t i = 0; i < bucket->count; i += 1) {
            Symbol* proc_sym = bucket->elems[i];
            proc_offsets[proc_sym->as_proc.index] = array_len(buffer);
            X64_elf_gen_proc_text(gen_mem, tmp_mem, proc_sym, &buffer, &relocs, &proc_off_patches);
        }
    }

    // Patch the location of the main proc.
    {
        const u64 main_offset = proc_offsets[main_proc->as_proc.index];
        *((u32*)(&buffer[startup_code_main_offset_loc])) = (u32)main_offset - (u32)startup_code_next_ip_after_call;
    }

    // Patch relative offsets to other procs.
    const u32 num_proc_patches = array_len(proc_off_patches);
    for (size_t i = 0; i < num_proc_patches; i++) {
        const X64_TextReloc* patch_info = &proc_off_patches[i];
        assert(patch_info->ref_addr.kind == CONST_ADDR_SYM); // Should be a proc symbol

        const Symbol* proc_sym = patch_info->ref_addr.sym;
        const s64 proc_offset = (s64)proc_offsets[proc_sym->as_proc.index];
        *((s32*)(&buffer[patch_info->usage_off])) = (s32)(proc_offset - (patch_info->usage_off + patch_info->bytes_to_next_ip));
    }

    text_sec->buf = buffer;
    text_sec->size = array_len(buffer);
    text_sec->align = 0x10;
    text_sec->proc_offs = proc_offsets;
    text_sec->relocs = relocs;

    allocator_restore_state(tmp_mem_state);
}

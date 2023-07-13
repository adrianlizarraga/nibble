#include <string.h>
#include "stream.h"
#include "x64_gen/text.h"
#include "x64_gen/regs.h"
#include "array.h"

static const u8 x64_reg_val[X64_REG_COUNT] = {
    [X64_RAX] = 0,
    [X64_RCX] = 1,
    [X64_RDX] = 2,
    [X64_RBX] = 3,
    [X64_RSP] = 4,
    [X64_RBP] = 5,
    [X64_RSI] = 6,
    [X64_RDI] = 7,
    [X64_R8] = 8,
    [X64_R9] = 9,
    [X64_R10] = 10,
    [X64_R11] = 11,
    [X64_R12] = 12,
    [X64_R13] = 13,
    [X64_R14] = 14,
    [X64_R15] = 15,
    [X64_XMM0] = 0,
    [X64_XMM1] = 1,
    [X64_XMM2] = 2,
    [X64_XMM3] = 3,
    [X64_XMM4] = 4,
    [X64_XMM5] = 5,
    [X64_XMM6] = 6,
    [X64_XMM7] = 7,
    [X64_XMM8] = 8,
    [X64_XMM9] = 9,
    [X64_XMM10] = 10,
    [X64_XMM11] = 11,
    [X64_XMM12] = 12,
    [X64_XMM13] = 13,
    [X64_XMM14] = 14,
    [X64_XMM15] = 15,
};

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
static const u8 startup_code[] = {
    0x48, 0x31, 0xed, 0x8b, 0x3c, 0x24, 0x48, 0x8d, 0x74, 0x24, 0x08, 0x48, 0x8d, 0x54, 0xfc, 0x10, 0x31,
    0xc0, 0xe8, 0x09, 0x00, 0x00, 0x00, 0x89, 0xc7, 0xb8, 0x3c, 0x00, 0x00, 0x00, 0x0f, 0x05};

static const u8 main_code[] = {0x55, 0x48, 0x89, 0xe5, 0x48, 0x83, 0xec, 0x10, 0xc7, 0x45, 0xfc, 0x0a, 0x00, 0x00, 0x00, 0xc7,
	                      0x45, 0xf8, 0x01, 0x00, 0x00, 0x00, 0x8b, 0x45, 0xfc, 0x03, 0x45, 0xf8, 0x48, 0x89, 0xec, 0x5d, 0xc3};

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem, Allocator* tmp_mem, const BucketList* procs)
{
#if 0
    const size_t startup_code_size = sizeof(startup_code);
    const size_t main_code_size = sizeof(main_code);
    const size_t total_code_size = startup_code_size + main_code_size;

    Array(u8) buffer = array_create(gen_mem, u8, total_code_size);

    array_push_elems(buffer, startup_code, startup_code_size);
    array_push_elems(buffer, main_code, main_code_size);
    assert(array_len(buffer) == total_code_size);

    text_sec->buf = buffer;
    text_sec->size = array_len(buffer);
    text_sec->align = 0x10;
#else
    const size_t startup_code_size = sizeof(startup_code);

    Array(u8) buffer = array_create(gen_mem, u8, startup_code_size);
    array_push_elems(buffer, startup_code, startup_code_size); // Add _start code.

    const size_t num_procs = procs->num_elems;
    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        const Symbol* sym = (const Symbol*)(*sym_ptr);
    }
#endif
}


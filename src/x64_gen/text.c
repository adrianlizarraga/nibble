#include <string.h>
#include "x64_gen/text.h"
#include "array.h"

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

void X64_init_text_section(X64_TextSection* text_sec, Allocator* gen_mem)
{
    const size_t startup_code_size = sizeof(startup_code);
    const size_t main_code_size = sizeof(main_code);
    const size_t total_code_size = startup_code_size + main_code_size;

    u8* buffer = array_create(gen_mem, u8, total_code_size);

    array_push_elems(buffer, startup_code, startup_code_size);
    array_push_elems(buffer, main_code, main_code_size);
    assert(array_len(buffer) == total_code_size);

    text_sec->buf = buffer;
    text_sec->size = array_len(buffer);
    text_sec->align = 0x10;
}


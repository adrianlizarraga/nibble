" Vim syntax file for the nibble programming language
"
" Put this file in <vim_config_dir>/after/syntax/
" Add the following to vim init file: 
"    autocmd BufRead,BufNewFile *.nib set filetype=nibble
if exists("b:current_syntax")
    finish
endif

syntax keyword nibbleTodos TODO NOTE IMPORTANT FIXME

" Language keywords
syntax keyword nibbleKeywords var const proc typedef sizeof typeof export import from as label goto break continue return if else while do for switch case

syntax region nibbleIncluded start=/"/ skip=/\\\\\|\\"/ end=/"/  display contained
syntax match nibbleInclude /^\s*\zs#include\>\s*"/ display contains=nibbleIncluded
syntax match nibbleStatisAssert '\s*\zs#static_assert\>\s*'

syntax keyword nibbleStructure struct union enum
syntax keyword nibbleType void u8 s8 u16 s16 u32 s32 u64 s64 f32 f64 bool char schar uchar short ushort int uint long ulong llong ullong ssize usize
syntax keyword nibbleBoolean true false
syntax keyword nibbleConstant NULL

" Single line comments
syntax region nibbleLineComment start="//" end="$" contains=nibbleTodos
syntax region nibbleComment start=+/\*+ end=+\*/+ extend fold contains=nibbleTodos

" String and char literals
syntax region nibbleString start=/\v"/ skip=/\v\\./ end=/\v"/
syntax region nibbleString start=/\v'/ skip=/\v\\./ end=/\v'/

" Set highlights
highlight default link nibbleTodos Todo
highlight default link nibbleKeywords Include
highlight default link nibbleStatisAssert Keyword
highlight default link nibbleInclude Keyword
highlight default link nibbleStructure Structure
highlight default link nibbleType Type
highlight default link nibbleBoolean Boolean
highlight default link nibbleConstant Constant
highlight default link nibbleLineComment Comment
highlight default link nibbleComment Comment
highlight default link nibbleString String

let b:current_syntax = "nibble"


#include <stdio.h>
#include <stdbool.h>


static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n') || (c == '\v');
}

static bool is_dec_digit(char c)
{
    return (c >= '0') && (c <= '9');
}

static bool is_oct_digit(char c)
{
    return (c >= '0') && (c <= '7');
}

static bool is_letter(char c)
{
    return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'));
}

static bool is_alphanum(char c)
{
    return is_dec_digit(c) || is_letter(c) || (c == '_');
}

static bool is_cntrl(char c)
{
    return (c >= 0 && c <= 0x1f) || (c == 0x7f);
}

enum CharPropFlags {
    CHAR_IS_WHITESPACE = 1 << 0,
    CHAR_IS_DEC_DIGIT = 1 << 1,
    CHAR_IS_ALPHANUM = 1 << 2,
    CHAR_IS_CNTRL = 1 << 3,
};


static void print_char_props_array(void)
{
    printf("const unsigned char char_props[256] = {");
    for (int i = 0; i < 256; ++i) {
        if ((i % 20) == 0) {
            printf("\n    ");
        }
        unsigned int elem = 0;

        if (is_whitespace(i)) {
            elem |= CHAR_IS_WHITESPACE;
        }
        if (is_dec_digit(i)) {
            elem |= CHAR_IS_DEC_DIGIT;
        }
        if (is_alphanum(i)) {
            elem |= CHAR_IS_ALPHANUM;
        }
        if (is_cntrl(i)) {
            elem |= CHAR_IS_CNTRL;
        }

        printf("0x%x, ", elem);

    }
    printf("\n};\n");
}

int main(void)
{
    print_char_props_array();
}

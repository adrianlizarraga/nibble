#include <stdio.h>
#include <stdbool.h>


static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n') || (c == '\v');
}

static bool is_digit(char c)
{
    return (c >= '0') && (c <= '9');
}

static bool is_letter(char c)
{
    return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z'));
}

static bool is_alphanum(char c)
{
    return is_digit(c) || is_letter(c) || (c == '_');
}

enum CharPropFlags {
    CHAR_IS_WHITESPACE = 1 << 0,
    CHAR_IS_DEC_DIGIT = 1 << 1,
    CHAR_IS_ALPHANUM = 1 << 2,
};

static void print_char_props_array(void)
{
    printf("static const unsigned char char_props[] = {");
    for (int i = 0; i < 256; ++i) {
        if ((i % 20) == 0) {
            printf("\n    ");
        }
        unsigned int elem = 0;

        if (is_whitespace(i)) {
            elem |= CHAR_IS_WHITESPACE;
        }
        if (is_digit(i)) {
            elem |= CHAR_IS_DEC_DIGIT;
        }
        if (is_alphanum(i)) {
            elem |= CHAR_IS_ALPHANUM;
        }

        printf("0x%x, ", elem);

    }
    printf("\n};\n");
}

int main(void)
{
    print_char_props_array();
}

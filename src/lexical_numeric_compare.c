/*
 * lexicographical-numeric compare
 */

#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <caml/mlvalues.h>

static int normalize(int v) {
    return v == 0 ? 0 : (v / abs(v));
}

CAMLprim value caml_lex_numeric_compare(value str1, value str2) {
    const char * s1, * s2;

    s1 = String_val(str1);
    s2 = String_val(str2);

    while (*s1 && *s2) {
        if (isdigit((unsigned char)*s1) && isdigit((unsigned char)*s2)) {
            const char *ps1 = s1, *ps2 = s2;

            while (isdigit((unsigned char)*s1)) s1++;
            while (isdigit((unsigned char)*s2)) s2++;
            size_t ls1 = s1 - ps1, ls2 = s2 - ps2;

            if (ls1 != ls2) return ls1 < ls2 ? Val_int(-1) : Val_int(1);

            int c = memcmp(ps1, ps2, ls1);
            if (c) return Val_int(normalize(c));

        } else {
            if (*s1 != *s2) {
                int res = (int)((unsigned char)*s1 - (unsigned char)*s2);
                return Val_int(normalize(res));
            }
            s1++; s2++;
        }
    }
    int res = (int)((unsigned char)*s1 - (unsigned char)*s2);
    return Val_int(normalize(res));
}

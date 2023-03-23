/*
 * lexicographical-numeric compare
 */
#include <string.h>
#include <ctype.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

CAMLprim value caml_lex_numeric_compare(value str1, value str2) {
    const char* inconsistent = "internal indexing error";
    mlsize_t len, len1, len2;
    int pos, pos1, pos2;
    const char * s1, * s2;
    const char * p1, * p2;
    int n1, n2;
    int res;

    if (str1 == str2) return Val_int(0);
    len1 = caml_string_length(str1);
    len2 = caml_string_length(str2);
    len = len1 <= len2 ? len1 : len2;
    s1 = String_val(str1);
    s2 = String_val(str2);
    p1 = s1;
    p2 = s2;
    pos = 0;

    do {
        while ((pos < len) && (!isdigit(*s1) || !isdigit(*s2))) {
            s1++;
            s2++;
            pos++;
        }
        if (pos > 0) {
            res = memcmp(p1, p2, pos);
            if (res < 0) return Val_int(-1);
            if (res > 0) return Val_int(1);
            if (pos == len) {
                if (len1 < len2) return Val_int(-1);
                if (len2 < len1) return Val_int(1);
                return Val_int(0);
            }
        }
        p1 = s1;
        p2 = s2;
        len = len - pos;
        len1 = len1 - pos;
        len2 = len2 - pos;
        pos1 = pos2 = 0;
        n1 = n2 = 0;
        while ((pos1 < len1) && isdigit(*s1)) {
            n1 = n1 * 10 + *s1 - '0';
            s1++;
            pos1++;
        }
        while ((pos2 < len2) && isdigit(*s2)) {
            n2 = n2 * 10 + *s2 - '0';
            s2++;
            pos2++;
        }
        if (n1 < n2) return Val_int(-1);
        if (n2 < n1) return Val_int(1);
        if ((pos1 == len1) || (pos2 == len2)) {
            if (len1 < len2) return Val_int(-1);
            if (len2 < len1) return Val_int(1);
            return Val_int(0);

        }
        // if here, pos1 == pos2, or something is horribly wrong
        if (pos1 != pos2) caml_failwith(inconsistent);
        pos = pos1;
        p1 = s1;
        p2 = s2;
        len = len - pos;
        len1 = len1 - pos;
        len2 = len2 - pos;
        pos = 0;
    } while (*s1 && *s2);
}

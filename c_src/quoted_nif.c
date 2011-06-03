#include "erl_nif.h"
#include "stdbool.h"
/* The corresponding erlang functions are implemented
   in the src/quoted.erl file. */

static bool is_hex(unsigned char c);
static bool is_safe(unsigned char c);
static unsigned char unhex(unsigned char c);
static unsigned char tohexlower(unsigned char c);
static ERL_NIF_TERM unquote_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM quote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM unquote_loaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_atom(env, "true");
}

ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM temp;
    bool output_bin;
    

    /* Determine type of input.
     * The input format also determines the output format. The caller
     * expects the output to be of the same type as the input format.
     */
    if(enif_is_list(env, argv[0])) {
        output_bin = false;
        if(!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else if(enif_is_binary(env, argv[0])) {
        output_bin = true;
        if(!enif_inspect_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else {
        return enif_make_badarg(env);
    }

    // Allocate a buffer of the same size as the input. This ensures
    // that we only need to realloc once to shrink the size of the buffer
    // if the buffer if the input contains quoted characters.
    if(!enif_alloc_binary(input.size, &output)) {
        return enif_make_badarg(env);
    }

    unsigned int i = 0; // Position in input
    unsigned int j = 0; // Position in output
    unsigned char c0 = 0; // Current character
    unsigned char c1 = 0; // Current character
    unsigned char c2 = 0; // Current character
    while(i < input.size) {
        c0 = input.data[i];
        if('%' == c0) {
            if(input.size < i + 2) {
                return enif_make_badarg(env);
            }
            c1 = input.data[i + 1];
            c2 = input.data[i + 2];
            if(!is_hex(c1) && !is_hex(c2)) {
                return enif_make_badarg(env);
            }
            c0 = (unhex(c1) << 4) | unhex(c2);
            i += 3;
        }
        else {
            // Spaces may be encoded as "%20" or "+". The first is standard,
            // but the second very popular. This library does " "<->"%20", 
            // but also " "<--"+" for compatibility with things like jQuery.
            if (c0=='+') {c0 = ' ';};
            i += 1;
        }
        
        output.data[j++] = c0;
    }

    if(output_bin) {
        if(!enif_realloc_binary(&output, j)) {
            return enif_make_badarg(env);
        }
        return enif_make_binary(env, &output);
    }
    else {
        temp = enif_make_string_len(env, output.data, j, ERL_NIF_LATIN1);
        enif_release_binary(&output);
        return temp;
    }
}


ERL_NIF_TERM quote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM temp;
    bool output_bin;
    

    /* Determine type of input.
     * See comment on output format in unquote_iolist(...)
     */
    if(enif_is_list(env, argv[0])) {
        output_bin = false;
        if(!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else if(enif_is_binary(env, argv[0])) {
        output_bin = true;
        if(!enif_inspect_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else {
        return enif_make_badarg(env);
    }


    // Allocate a buffer that is three times larger than the input
    // that we only need to realloc once to shrink the size of the buffer
    // if the input contains no charactes that needs to be quoted.
    if(!enif_alloc_binary(input.size * 3, &output)) {
        return enif_make_badarg(env);
    }

    unsigned int i = 0; // Position in input
    unsigned int j = 0; // Position in output
    unsigned char c = 0; // Current character
    while(i < input.size) {
        c = input.data[i];
        if(is_safe(c)) {
            output.data[j++] = c;
            i++;
        }
        else {
            output.data[j++] = '%';
            output.data[j++] = tohexlower(c >> 4);
            output.data[j++] = tohexlower(c & 15);
            i++;
        }
    }

    if(output_bin) {
        if(!enif_realloc_binary(&output, j)) {
            return enif_make_badarg(env);
        }
        return enif_make_binary(env, &output);
    }
    else {
        temp = enif_make_string_len(env, output.data, j, ERL_NIF_LATIN1);
        enif_release_binary(&output);
        return temp;
    }
}

inline bool is_hex(unsigned char c) {
    return (c >= '0' && c <= '9')
        || (c >= 'A' && c <= 'F')
        || (c >= 'a' && c <= 'f');
}

inline bool is_safe(unsigned char c) {
    return (c >= '0' && c <= 'z' && (c >= 'a' || 
                                     c <= '9' || 
                                     (c >= 'A' && c <= 'Z')))
        || (c == '.') || (c == '-')
        || (c == '~') || (c == '_'); 
}

inline unsigned char unhex(unsigned char c) {
    if(c >= '0' && c <= '9') { return c - '0'; }
    if(c >= 'A' && c <= 'F') { return c - 'A' + 10; }
    if(c >= 'a' && c <= 'f') { return c - 'a' + 10; }
}

unsigned char tohexlower(unsigned char c) {
    if(c < 10) { return '0' + c; }
    if(c < 16) { return 'a' + (c - 10); }
}

static ErlNifFunc nif_funcs[] = {
    {"is_native", 0, unquote_loaded},
    {"from_url", 1, unquote_iolist},
    {"to_url", 1, quote_iolist}
};

ERL_NIF_INIT(quoted, nif_funcs, NULL, NULL, NULL, NULL)

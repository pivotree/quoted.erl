#include "erl_nif.h"
#include "stdbool.h"

static bool is_hex(unsigned int c);
static unsigned int unhex(unsigned int c);
static ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static unsigned int next_size(unsigned int size);


ERL_NIF_TERM unquote_iolist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary input;
    ErlNifBinary output;
    ERL_NIF_TERM temp;
    

    // Determine type of input
    if(enif_is_list(env, argv[0])) {
        if(!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else if(enif_is_binary(env, argv[0])) {
        if(!enif_inspect_binary(env, argv[0], &input)) {
            return enif_make_badarg(env);
        }
    }
    else {
        return enif_make_badarg(env);
    }

    // Determine type of output. 
    bool output_list;
    if(!enif_compare(enif_make_atom(env, "binary"), argv[1])) {
        output_list = false;
    }
    else if(!enif_compare(enif_make_atom(env, "list"), argv[1])) {
        output_list = true;
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
            i += 1;
        }
        
        output.data[j++] = c0;
    }

    if(!output_list) {
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

inline bool is_hex(unsigned int c) {
    return (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F')
        || (c >= '0' && c <= '9');
}

inline unsigned int unhex(unsigned int c) {
    if(c >= 'a' && c <= 'f') { return c - 'a' + 10; }
    if(c >= 'A' && c <= 'F') { return c - 'A' + 10; }
    if(c >= '0' && c <= '9') { return c - '0'; }
}

inline unsigned int next_size(unsigned int size) {
    if(size == 0) { return 32; }
    else { return size * 2; }
}

static ErlNifFunc nif_funcs[] = {
    {"_nif_unquote", 2, unquote_iolist}
};

ERL_NIF_INIT(quoted, nif_funcs, NULL, NULL, NULL, NULL)

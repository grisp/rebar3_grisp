#ifdef __rtems__
/* Match the init symbol OTP derives from the source filename. */
#define STATIC_ERLANG_NIF_LIBNAME {{{name}}}_c
#endif

#include <erl_nif.h>

static ERL_NIF_TERM hello(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;
    return enif_make_string(env, "Hello from a C NIF", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] = {
    {"hello", 0, hello, 0}
};

ERL_NIF_INIT({{{name}}}_c_nif, nif_funcs, NULL, NULL, NULL, NULL)

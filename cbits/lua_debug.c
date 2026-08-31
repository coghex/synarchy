#include <lua.h>
#include <lauxlib.h>
#include "lua_debug.h"
#include <string.h>

// Caller supplies the output buffer (of size buf_size) so this function
// has no shared state — safe to call from multiple Lua states/threads
// without a reentrancy/lifetime hazard on the returned source pointer.
//
// *kind reports what KIND of chunk the frame belongs to, which short_src
// alone cannot answer: short_src is a display string, so a file-backed
// chunk and a generated label can produce byte-identical text (#1960).
// Lua encodes the distinction in the first byte of ar.source, and that
// byte is the only unambiguous signal available here:
//
//   '@' -> LUA_SOURCE_FILE   loaded from a file; the rest names a path
//   '=' -> LUA_SOURCE_NAMED  a caller-supplied literal label, verbatim
//   else -> LUA_SOURCE_STRING a string chunk; short_src is [string "..."]
//
// LUA_SOURCE_UNKNOWN covers the case where Lua reports no source at all.
int get_lua_caller_info(lua_State *L, int level,
                        char *source_buf, int buf_size, int *line,
                        int *kind) {
    lua_Debug ar;
    if (!lua_getstack(L, level, &ar)) {
        *line = 0;
        *kind = LUA_SOURCE_UNKNOWN;
        return 0;
    }
    if (!lua_getinfo(L, "Sl", &ar)) {
        *line = 0;
        *kind = LUA_SOURCE_UNKNOWN;
        return 0;
    }
    // Copy short_src into the caller's buffer (it's a char array in the struct)
    strncpy(source_buf, ar.short_src, buf_size - 1);
    source_buf[buf_size - 1] = '\0';

    if (ar.source == NULL || ar.source[0] == '\0') {
        *kind = LUA_SOURCE_UNKNOWN;
    } else if (ar.source[0] == '@') {
        *kind = LUA_SOURCE_FILE;
    } else if (ar.source[0] == '=') {
        *kind = LUA_SOURCE_NAMED;
    } else {
        *kind = LUA_SOURCE_STRING;
    }

    *line = ar.currentline;
    return 1;
}

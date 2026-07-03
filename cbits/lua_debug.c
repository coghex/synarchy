#include <lua.h>
#include <lauxlib.h>
#include <string.h>

// Caller supplies the output buffer (of size buf_size) so this function
// has no shared state — safe to call from multiple Lua states/threads
// without a reentrancy/lifetime hazard on the returned source pointer.
int get_lua_caller_info(lua_State *L, int level,
                        char *source_buf, int buf_size, int *line) {
    lua_Debug ar;
    if (!lua_getstack(L, level, &ar)) {
        *line = 0;
        return 0;
    }
    if (!lua_getinfo(L, "Sl", &ar)) {
        *line = 0;
        return 0;
    }
    // Copy short_src into the caller's buffer (it's a char array in the struct)
    strncpy(source_buf, ar.short_src, buf_size - 1);
    source_buf[buf_size - 1] = '\0';

    *line = ar.currentline;
    return 1;
}

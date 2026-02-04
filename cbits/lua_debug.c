#include <lua.h>
#include <lauxlib.h>
#include <string.h>

// Buffer to hold the short_src (lua_Debug.short_src is char[LUA_IDSIZE])
// LUA_IDSIZE is typically 60
static char source_buffer[256];

int get_lua_caller_info(lua_State *L, int level, 
                        const char **source, int *line) {
    lua_Debug ar;
    if (!lua_getstack(L, level, &ar)) {
        *source = "<unknown>";
        *line = 0;
        return 0;
    }
    if (!lua_getinfo(L, "Sl", &ar)) {
        *source = "<unknown>";
        *line = 0;
        return 0;
    }
    // Copy short_src to our buffer (it's a char array in the struct)
    strncpy(source_buffer, ar.short_src, sizeof(source_buffer) - 1);
    source_buffer[sizeof(source_buffer) - 1] = '\0';
    
    *source = source_buffer;
    *line = ar.currentline;
    return 1;
}

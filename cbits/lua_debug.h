#ifndef SYNARCHY_LUA_DEBUG_H
#define SYNARCHY_LUA_DEBUG_H

#include <lua.h>

// Chunk-source kinds reported by get_lua_caller_info's *kind out-param.
//
// These four values are mirrored verbatim by 'ChunkKind' in
// Engine.Scripting.Lua.Debug, which cannot include this header; the two
// lists are kept in step by hand and each names the other (#1960).
#define LUA_SOURCE_UNKNOWN 0
#define LUA_SOURCE_FILE    1
#define LUA_SOURCE_NAMED   2
#define LUA_SOURCE_STRING  3

// Fill *source_buf (of size buf_size), *line and *kind from the Lua stack
// frame at 'level'. Returns 1 on success, 0 when no such frame exists or
// its info is unavailable, in which case *line is 0 and *kind is
// LUA_SOURCE_UNKNOWN.
int get_lua_caller_info(lua_State *L, int level,
                        char *source_buf, int buf_size, int *line,
                        int *kind);

#endif // SYNARCHY_LUA_DEBUG_H

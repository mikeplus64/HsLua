module Lua.Types where
import Foreign.C.Types
import Foreign

-- | Wrapper for @lua_State *@. See @lua_State@ in Lua Reference Manual.
newtype LuaState = LuaState (Ptr ())

-- | Wrapper for @lua_Alloc@. See @lua_Alloc@ in Lua Reference Manual.
type LuaAlloc = Ptr () -> Ptr () -> CSize -> CSize -> IO (Ptr ())

-- | Wrapper for @lua_Reader@. See @lua_Reader@ in Lua Reference Manual.
type LuaReader = Ptr () -> Ptr () -> Ptr CSize -> IO (Ptr CChar)

-- | Wrapper for @lua_Writer@. See @lua_Writer@ in Lua Reference Manual.
type LuaWriter = LuaState -> Ptr CChar -> CSize -> Ptr () -> IO CInt

-- | Wrapper for @lua_CFunction@. See @lua_CFunction@ in Lua Reference Manual.
type LuaCFunction = LuaState -> IO CInt

-- | Wrapper for @lua_Integer@. See @lua_Integer@ in Lua Reference Manual.
-- HsLua uses C @ptrdiff_t@ as @lua_Integer@.
type LuaInteger = CPtrdiff

-- | Wrapper for @lua_Number@. See @lua_Number@ in Lua Reference Manual.
-- HsLua uses C @double@ as @lua_Integer@.
type LuaNumber = CDouble

-- | Enumeration used as type tag. See @lua_type@ in Lua Reference Manual.
data LTYPE = TNONE
           | TNIL
           | TBOOLEAN
           | TLIGHTUSERDATA
           | TNUMBER
           | TSTRING
           | TTABLE
           | TFUNCTION
           | TUSERDATA
           | TTHREAD
           deriving (Eq,Show,Ord)

instance Enum LTYPE where
    fromEnum TNONE          = -1
    fromEnum TNIL           = 0
    fromEnum TBOOLEAN       = 1
    fromEnum TLIGHTUSERDATA = 2
    fromEnum TNUMBER        = 3
    fromEnum TSTRING        = 4
    fromEnum TTABLE         = 5
    fromEnum TFUNCTION      = 6
    fromEnum TUSERDATA      = 7
    fromEnum TTHREAD        = 8
    toEnum (-1)             = TNONE
    toEnum 0                = TNIL
    toEnum 1                = TBOOLEAN
    toEnum 2                = TLIGHTUSERDATA
    toEnum 3                = TNUMBER
    toEnum 4                = TSTRING
    toEnum 5                = TTABLE
    toEnum 6                = TFUNCTION
    toEnum 7                = TUSERDATA
    toEnum 8                = TTHREAD
    toEnum n                = error $ "Cannot convert (" ++ show n ++ ") to LTYPE"

-- | Enumeration used by @gc@ function.
data GCCONTROL  = GCSTOP
                | GCRESTART
                | GCCOLLECT
                | GCCOUNT
                | GCCOUNTB
                | GCSTEP
                | GCSETPAUSE
                | GCSETSTEPMUL
                deriving (Eq,Ord,Show,Enum)


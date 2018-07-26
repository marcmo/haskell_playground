import qualified Scripting.Lua as Lua

import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr

main = executeLuaScript "passingStrings.lua"

executeLuaScript ::  String -> IO ()
executeLuaScript script = do
    s <- Lua.newstate
    Lua.openlibs s
    z <- callReturningString s script 4
    print z
    Lua.close s

getIp :: Lua.LuaState -> String -> IO (Maybe String)
getIp s script = do
    Lua.loadfile s script >> Lua.pcall s 0 0 0
    Lua.getglobal s "ip_address"
    ipPresent <- Lua.isstring s (-1)
    if ipPresent
      then Lua.tostring s (-1) >>= return . Just
      else return Nothing

callReturningString :: Lua.LuaState -> String -> Lua.LuaNumber -> IO String
callReturningString s script x = do
    Lua.loadfile s script >> Lua.pcall s 0 0 0
    Lua.getglobal s "foo"  -- /* function to be called */
    Lua.pushnumber s x   -- /* push 1st argument */
    res <- Lua.pcall s 1 1 0
    if res /= 0
    	then do
    	  e <- Lua.tostring s (-1)
    	  error $ "error running function `f': " ++ e
    	else do
    		isString <- Lua.isstring s (-1)
    		if not isString 
    			then error "function `f' must return a string"
    			else do
                z <- Lua.tostring s (-1)
                Lua.pop s 1 -- /* pop returned value */
                return z

callReturningNumber :: Lua.LuaState -> String -> Lua.LuaNumber -> IO CDouble
callReturningNumber s script x = do
    Lua.loadfile s script >> Lua.pcall s 0 0 0
    -- /* push functions and arguments */
    Lua.getglobal s "foo"  -- /* function to be called */
    Lua.pushnumber s x   -- /* push 1st argument */
    -- /* do the call (1 arguments, 1 result) */
    res <- Lua.pcall s 1 1 0
    if res /= 0
    	then do
    	  e <- Lua.tostring s (-1)
    	  error $ "error running function `f': " ++ e
    	else do
    		isNum <- Lua.isnumber s (-1)
    		if not isNum 
    			then error "function `f' must return a number"
    			else do
                z <- Lua.tonumber s (-1)
                Lua.pop s 1 -- /* pop returned value */
                return z
    		



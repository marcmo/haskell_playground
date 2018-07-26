import Control.Monad.State
import Prelude hiding(read)

increment = modify (+1)
load = put
read = get

normal :: StateT Int IO ()
normal = do
    load 5
    x <- read
    lift (print x)
    increment
    x <- read
    lift (print x)

main = evalStateT normal 0



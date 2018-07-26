    {-# LANGUAGE TemplateHaskell #-}
      import Control.Monad.State(StateT,evalStateT,lift)
      import Prelude hiding(read)
      import Data.Lens.Lazy((~=),access,(%=))
      import Data.Lens.Template(makeLenses)

    data Number = Number {
        _value :: Int
      } deriving (Show)
      $( makeLenses [''Number] )

    increment = value %= succ
      load x = value ~= x
      read = access value

    withLens :: StateT Number IO ()
      withLens = do
          load 5
          x <- read
          lift $ print x
          increment
          x <- read
          lift $ print x

    main = evalStateT withLens (Number 0)

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



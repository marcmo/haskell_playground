    import Text.Printf
    -- import Process

    main :: IO ()
    main = do
        s <- run "sysctl hw.setperf"
        let old = clean s
            new = if old == 100 then 0 else 100 :: Integer
        run $ "sudo sysctl -w hw.setperf=" ++ show new
        printf "cpu: %d -> %d\n" old new

        s <- run "sysctl hw.cpuspeed"
        let clock = fromIntegral (clean s) / 1000
        printf "clock: %f Ghz\n" (clock :: Double)

      where
        clean :: String -> Integer
        clean = read . init . tail . dropWhile (/='=')


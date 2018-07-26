data Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | White
    | Black
    | Custom Int Int Int -- R G B components
      deriving (Eq, Show, Ord)

colorToRGB Orange = (255,128,0)
colorToRGB Yellow = (255,255,0)
colorToRGB Green = (0,255,0)
colorToRGB Blue = (0,0,255)
colorToRGB Purple = (255,0,255)
colorToRGB White = (255,255,255)
colorToRGB Black = (0,0,0)
colorToRGB (Custom r g b) = (r,g,b)

-- instance Show Color where
--     show Red = "Red!!"
--     show Orange = "Orange"
--     show Yellow = "Yellow"
--     show Green = "Green"
--     show Blue = "Blue"
--     show Purple = "Purple"
--     show White = "White"
--     show Black = "Black"
--     show (Custom r g b) =
--         "Custom (R:" ++ show r ++ " B:" ++
--                   show g ++ " G:" ++ show b ++ ")"

func1 x l = map (\y -> y*x) l
func1_ x = map (*x)
func1_2 = map (*2)

func2 f g l = filter f (map g l)
func2_ f g =  filter f . map g

func4 list = map (\y -> y+2)(filter (\z -> z `elem`[1..10])(5:list))
func4_ = map (+2) . filter (`elem` [1..10]) . (5:)

func5 f l = foldr (\x y -> f (y,x)) 0 l

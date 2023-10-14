import Data.List.Split


data Expr = Val String | Max Expr Expr | Sum Expr Expr
    deriving Show

type LineFunc = (Expr, Expr, Expr, Expr, Expr, Expr)

data Func = Line LineFunc | Comp Func Func
    deriving Show    
    

makeLineFunc :: Char -> LineFunc
makeLineFunc c = 
    ( Val $ 'a':c:"1"
    , Val $ 'a':c:"2"
    , Val $ 'a':c:"3" 
    , Val $ 'a':c:"4"
    , Val $ 'b':c:"1"
    , Val $ 'b':c:"2"
    )


makeComp :: Func -> Func
makeComp (Line x) = Line x
makeComp (Comp (Line (a1, a2, a3, a4, b1, b2)) (Line (c1, c2, c3, c4, d1, d2))) = Line
    ( Max (Sum a1 c1) (Sum a2 c3)
    , Max (Sum a1 c2) (Sum a2 c4)
    , Max (Sum a3 c1) (Sum a4 c3)
    , Max (Sum a3 c2) (Sum a4 c4)
    , Max (Max (Sum b1 c1) (Sum b2 c2)) d1
    , Max (Max (Sum b1 c3) (Sum b2 c4)) d2
    )
makeComp (Comp (Line x1) x2) = makeComp (Comp (Line x1) (makeComp x2))
makeComp (Comp x1 x2) = makeComp (Comp (makeComp x1) (makeComp x2))


makeFunc :: String -> Func
makeFunc "" =  Line $ makeLineFunc '!'
makeFunc (x:"") = Line $ makeLineFunc x
makeFunc (x:xs) = Comp (Line $ makeLineFunc x) (makeFunc xs)


getUniqueSimbols :: String -> String
getUniqueSimbols (x1:xs) = 
    if x1 == '>' || x1 == '-' || x1 == '\n' || x1 == ' ' || elem x1 xs 
        then getUniqueSimbols xs
        else x1: getUniqueSimbols xs
getUniqueSimbols [] = []


splitByArrow :: String -> (String, String)
splitByArrow str =
    makeCortege $ splitOn "->" str


makeCortege :: [String] -> (String, String)
makeCortege (x1:x2:_) = (x1, x2)
makeCortege _ = ("error", "error")


makeFuncs :: (String, String) -> (Func, Func)
makeFuncs(x1, x2) = (makeFunc x1, makeFunc x2)


main :: IO ()
main = do
    str <- readFile "static/haiku.txt"
    print $ getUniqueSimbols str
    let parsFunc = map (makeFuncs . splitByArrow) $ lines $ filter (\c -> c /= ' ') str
    print parsFunc
    print $ makeComp parsFunc

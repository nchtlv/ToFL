module Lib
    ( someFunc
    ) where

import System.Random

data Val = Var Char | Empty | Eps deriving (Show, Read)

data Reg = OneVal Val| Variable Reg Reg | Mul Reg Reg | Shuffle Reg Reg | Klini Reg deriving (Show, Read)

alf :: String
alf = "abcd"


genVal :: StdGen -> (Val, StdGen)
genVal stdGen = do
    let newRandom = random stdGen :: (Int, StdGen)
    case  (fst newRandom) `mod` 6 of
        5 -> (Eps, snd newRandom)
        4 -> (Empty, snd newRandom)
        ind -> (Var (alf !! ind), snd newRandom)

genReg :: StdGen -> Int -> (Reg, StdGen)
genReg stdGen depth = do
    let 
        newRandom = random stdGen :: (Int, StdGen)
        newRandomVal = genVal (snd newRandom)
        newRandomReg1 = genReg (snd newRandom) (depth - 1)
        newRandomReg2 = genReg (snd newRandomReg1) (depth - 1)

    if depth  < 1
    then (OneVal (fst newRandomVal), snd newRandomVal)
    else 
        (case (fst newRandom) `mod` 5 of 
            0 -> OneVal (fst newRandomVal)
            1 -> Variable (fst newRandomReg1) (fst newRandomReg2)
            2 -> Mul (fst newRandomReg1) (fst newRandomReg2)
            3 -> Shuffle (fst newRandomReg1) (fst newRandomReg2)
            _ -> Klini (fst newRandomReg1)
        , snd newRandomReg2)

tryToDecrease :: Reg -> Reg
tryToDecrease (OneVal x) = OneVal x
tryToDecrease (Variable reg1 reg2) = 
    if (isEqReg reg1 reg2) 
    then tryToDecrease reg1
    else Variable (tryToDecrease reg1) (tryToDecrease reg2)
tryToDecrease (Mul (Klini reg1) (Klini reg2)) =
    if isEqReg reg1 reg2
    then Klini (tryToDecrease reg1)
    else Mul (tryToDecrease reg1) (tryToDecrease reg2)
tryToDecrease (Mul (OneVal Empty) reg2) = OneVal Empty
tryToDecrease (Mul reg1 (OneVal Empty)) = OneVal Empty
tryToDecrease (Mul (OneVal Eps) reg2) = tryToDecrease reg2
tryToDecrease (Mul reg1 (OneVal Eps)) = tryToDecrease reg1
tryToDecrease (Mul reg1 reg2) = Mul (tryToDecrease reg1) (tryToDecrease reg2)
tryToDecrease (Shuffle reg1 reg2) = Shuffle reg1 reg2
tryToDecrease (Klini(Klini reg)) = tryToDecrease (Klini reg)
tryToDecrease (Klini reg) = tryToDecrease reg



derByVar :: Reg -> Char -> Reg 
derByVar (OneVal Empty) _ = OneVal Empty
derByVar (OneVal Eps) _ = OneVal Empty
derByVar (OneVal (Var var)) base = 
    if var == base
    then OneVal Eps
    else OneVal Empty
derByVar (Variable reg1 reg2) base = do
    let
        left = (derByVar reg1 base)
        right = (derByVar reg2 base)
    incriseVariable left right
derByVar (Klini reg) base = incriseMul (derByVar reg base) (Klini reg)
derByVar (Shuffle reg1 reg2) base = 
    incriseVariable (incriseShuffle (derByVar reg1 base) reg2) (incriseMul reg1 (derByVar reg2 base)) 
derByVar (Mul reg1 reg2)  base = 
    if (isEpsInAlf reg1) 
    then incriseVariable (incriseMul (derByVar reg1 base) reg2) (derByVar reg2 base)
    else incriseMul (derByVar reg1 base) reg2


isEqReg :: Reg -> Reg -> Bool
isEqReg (OneVal Empty) (OneVal Empty) = True
isEqReg (OneVal Eps) (OneVal Eps) = True
isEqReg (OneVal (Var var1)) (OneVal (Var var2)) = var1 == var2
isEqReg (Variable reg1 reg2) (Variable reg3 reg4) = 
    ((isEqReg reg1 reg3) && (isEqReg reg2 reg4)) || ((isEqReg reg1 reg4) && (isEqReg reg2 reg3))
isEqReg (Mul reg1 reg2) (Mul reg3 reg4) = (isEqReg reg1 reg3) && (isEqReg reg2 reg4)
isEqReg (Shuffle reg1 reg2) (Shuffle reg3 reg4) = (isEqReg reg1 reg3) && (isEqReg reg2 reg4)
isEqReg (Klini reg1) (Klini reg2) = isEqReg reg1 reg2
isEqReg _ _ = False


isEpsInAlf :: Reg -> Bool
isEpsInAlf (OneVal Eps) = True
isEpsInAlf (OneVal _) = False
isEpsInAlf (Variable reg1 reg2) = (isEpsInAlf reg1) || (isEpsInAlf reg2)
isEpsInAlf (Mul reg1 reg2) = (isEpsInAlf reg1) && (isEpsInAlf reg2)
isEpsInAlf (Shuffle reg1 reg2) = (isEpsInAlf reg1) && (isEpsInAlf reg2)
isEpsInAlf (Klini _) = True

incriseVariable :: Reg -> Reg -> Reg
incriseVariable (OneVal Eps) (Klini reg) = Klini reg
incriseVariable (Klini reg) (OneVal Eps) = Klini reg
incriseVariable (OneVal Empty) reg = reg
incriseVariable reg (OneVal Empty) = reg
incriseVariable reg1 (Mul (Klini reg2) reg3) =
    if isEqReg reg1 reg3
    then Mul (Klini reg2) reg3
    else Variable reg1 (Mul (Klini reg2) reg3)
incriseVariable reg1 (Mul reg2 (Klini reg3)) =
    if isEqReg reg1 reg2
    then Mul reg2 (Klini reg3)
    else Variable reg1 (Mul reg2 (Klini reg3))

incriseVariable (Mul (Klini reg2) reg3) reg1 =
    if isEqReg reg1 reg3
    then Mul (Klini reg2) reg3
    else Variable (Mul (Klini reg2) reg3) reg1
incriseVariable (Mul reg2 (Klini reg3)) reg1 =
    if isEqReg reg1 reg2
    then Mul reg2 (Klini reg3)
    else Variable(Mul reg2 (Klini reg3)) reg1 
incriseVariable reg1 reg2 = 
    if isEqReg reg1 reg2
    then reg1
    else Variable reg1 reg2


incriseMul:: Reg -> Reg -> Reg
incriseMul (OneVal Eps) reg = reg
incriseMul reg (OneVal Eps) = reg
incriseMul (OneVal Empty) reg = OneVal Empty
incriseMul reg (OneVal Empty) = OneVal Empty
incriseMul (Klini reg1) (Klini reg2) = 
    if (isEqReg reg1 reg2)
    then Klini reg1
    else Mul (Klini reg1) (Klini reg2)
incriseMul reg1 (Klini reg2) = 
    if (isEqReg reg1 reg2)
    then Klini reg1
    else Mul (Klini reg1) (Klini reg2)
incriseMul reg1 reg2 = Mul reg1 reg2


incriseShuffle :: Reg -> Reg -> Reg
incriseShuffle reg (OneVal Eps) = reg
incriseShuffle (OneVal Eps) reg = reg
incriseShuffle reg (OneVal Empty) = OneVal Empty
incriseShuffle (OneVal Empty) reg = OneVal Empty
incriseShuffle reg1 reg2 = 
    if isEqReg reg1 reg2
    then reg1
    else Shuffle reg1 reg2

postfixToInfix :: Reg -> String
postfixToInfix (OneVal (Var c)) = [c]
postfixToInfix (OneVal Empty) = "Empty"
postfixToInfix (OneVal Eps) = "Eps"
postfixToInfix (Variable reg1 reg2) = "(" ++ postfixToInfix reg1 ++ ")" ++ "|"++"(" ++ postfixToInfix reg2 ++ ")"
postfixToInfix (Mul reg1 reg2) = "(" ++ postfixToInfix reg1 ++ ")(" ++ postfixToInfix reg2 ++ ")"
postfixToInfix (Shuffle reg1 reg2) = "(" ++ postfixToInfix reg1 ++ ")#(" ++ postfixToInfix reg2  ++ ")"
postfixToInfix (Klini reg) = "(" ++ postfixToInfix reg ++ ")*"

convertListToString :: [(Reg, Char, Reg, Bool)] -> String
convertListToString [] = ""
convertListToString ((reg1, c, reg2, _):xs) = postfixToInfix reg1 ++ "->" ++[c] ++ "->" ++ postfixToInfix reg2 ++ "\n" ++ convertListToString xs

convertString :: String -> Reg
convertString [] = OneVal Empty
convertString "Empty" = OneVal Empty
convertString "Eps" = OneVal Eps
convertString [x] = OneVal (Var x)
convertString ('(' : str) = do
    let
        (left, right) = parseBr str 1
    if right == ""
    then convertString left
    else do
        case head right of
            '*' -> 
                if length right == 1
                then Klini (convertString left)
                else 
                    case (head (tail right)) of
                        '#' -> Shuffle (Klini (convertString left)) (convertString(tail (tail right)))
                        '|' -> Variable (Klini (convertString left)) (convertString(tail (tail right)))
                        x -> Mul (Klini(convertString left)) (convertString(tail right))
            '|' -> Variable (convertString left) (convertString (tail right))
            '#' -> Shuffle (convertString left) (convertString (tail right))
            x -> Mul (convertString left) (convertString right)
convertString ('E' : 'm' : 'p': 't' : 'y': xs) = Mul (OneVal Empty) (convertString xs)
convertString ('E' : 'p': 's': xs) = Mul (OneVal Eps) (convertString xs)
convertString (x : xs)  = Mul (OneVal (Var x)) (convertString xs)

--необходимо ставить скобки везде, кроме умножения
parseBr :: String -> Int -> (String, String)
parseBr str 0 = ("", str)
parseBr (')' : str) 1 = ("", str)
parseBr (')' : str) n = do
    let
        (left, right) = parseBr str (n-1)
    (')': left, right)
parseBr ('(' : str) n = do
    let
        (left, right) = parseBr str (n+1)
    ('(': left, right)
parseBr (s : str) n = do
    let
        (left, right) = parseBr str n
    (s : left, right)


someFunc :: IO ()
someFunc = do
    stdGen <- newStdGen
    inputReg <- getLine
    print $ show $ convertString $ filter (\x -> x /= ' ') inputReg
    let
        --inputReg' = read $ convertString inputReg
        --defReg = read "Shuffle (Mul(Klini (OneVal (Var 'b'))) (OneVal (Var 'a'))) (Klini (OneVal (Var 'a')))"
        inputReg' = convertString $ filter (\x -> x /= ' ') inputReg
        --genRegV = tryToDecrease $ fst $ genReg stdGen 3
        -- x = (derByVar defReg 'b')
        -- y = (derByVar(derByVar defReg 'b') 'a')
    -- putStrLn $ postfixToInfix genRegV
    --print (show (tryToDecrease inputReg'))
    -- print word
    -- print (show(infixToPrefix word))
    --print (show (derByVar(derByVar defReg 'b') 'a'))
    --print (show (derByVar(derByVar defReg 'b') 'b'))
    --print (postfixToInfix genRegV)
    putStrLn (convertListToString(makeAutomat(makeInitAutomat inputReg' []) (makeInitAutomat inputReg' []) 100))


makeInitAutomat :: Reg -> [(Reg, Char, Reg, Bool )] -> [(Reg, Char, Reg, Bool )]
makeInitAutomat reg currentAutomat = 
    let
        newAutomatA = addOneRegAutomat reg 'a' currentAutomat
        newAutomatB = addOneRegAutomat reg 'b' newAutomatA
        newAutomatC = addOneRegAutomat reg 'c' newAutomatB
        newAutomatD = addOneRegAutomat reg 'd' newAutomatC
    in newAutomatD

makeAutomat :: [(Reg, Char, Reg, Bool)] -> [(Reg, Char, Reg, Bool)] -> Int -> [(Reg, Char, Reg, Bool)]
makeAutomat _ currentAutomat 0 = currentAutomat
makeAutomat [] currentAutomat n = currentAutomat
makeAutomat ((reg1, v, reg2, True):xs) currentAutomat n = makeAutomat xs currentAutomat(n-1)
makeAutomat ((reg1, v, reg2, False):xs) currentAutomat n = 
    let
        currentAutomat1 =  changeFlag (reg1, v, reg2) currentAutomat
        newAutomatA = addOneRegAutomat reg2 'a' currentAutomat1
        newAutomatB = addOneRegAutomat reg2 'b' newAutomatA
        newAutomatC = addOneRegAutomat reg2 'c' newAutomatB
        newAutomatD = addOneRegAutomat reg2 'd' newAutomatC
    in makeAutomat newAutomatD newAutomatD (n-1)

changeFlag :: (Reg, Char, Reg) -> [(Reg, Char, Reg, Bool)] -> [(Reg, Char, Reg, Bool)]
changeFlag _ [] = []
changeFlag (reg1, v1, reg2) ((reg3, v2, reg4, flag) : xs) =
    if isEqReg reg1 reg3 && isEqReg reg2 reg4 && v1 == v2
    then (reg1, v1, reg2, True) : changeFlag (reg1, v1, reg2) xs
    else (reg3, v2, reg4, flag) : changeFlag (reg1, v1, reg2) xs

isInAutomat :: (Reg, Char, Reg, Bool) -> [(Reg, Char, Reg, Bool)] -> Bool
isInAutomat _ [] = False
isInAutomat (reg1, v, reg2, b1) ((x1, x2, x3, b2):xs) = 
    if (isEqReg reg1 x1) && (isEqReg reg2 x3) && (v == x2)
    then True
    else isInAutomat (reg1, v, reg2, b1) xs

addOneRegAutomat :: Reg -> Char -> [(Reg, Char, Reg, Bool )] -> [(Reg, Char, Reg, Bool)]
addOneRegAutomat reg v currentAutomat = do
    let
        newReg = derByVar reg v
    
    if (isEqReg newReg (OneVal Empty)) || isInAutomat (reg, v, newReg, False) currentAutomat
    then currentAutomat
    else (reg, v, newReg, False) : currentAutomat

module Lib
    ( someFunc
    ) where

import System.Random

data Val = Var Char | Empty | Eps deriving Show

data Reg = OneVal Val| Variable Reg Reg | Mul Reg Reg | Shuffle Reg Reg | Klini Reg deriving Show

alf :: String
alf = "abcd"

genVal :: StdGen -> (Val, StdGen)
genVal stdGen = do
    let newRandom = random stdGen :: (Int, StdGen)
    case  (fst newRandom) `mod` 7 of
        6 -> (Eps, snd newRandom)
        5 -> (Empty, snd newRandom)
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


someFunc :: IO ()
someFunc = do
    stdGen <- newStdGen
    let
        defReg = Shuffle (Variable (Klini (OneVal (Var 'a')))  (Klini (OneVal (Var 'b')))) (Klini (OneVal (Var 'a')))
        genRegV = fst $ genReg stdGen 5
    --print $ show genRegV
    let
        x = (derByVar defReg 'b')
        y = (derByVar(derByVar defReg 'b') 'a')
    --print (isEqReg x y)
    --print (show (derByVar defReg 'b'))
    --print (show (derByVar(derByVar defReg 'b') 'a'))
    --print (show (derByVar(derByVar defReg 'b') 'b'))
    print ( show(makeAutomat(makeInitAutomat genRegV [])))


makeInitAutomat :: Reg -> [(Reg, Char, Reg, Bool )] -> [(Reg, Char, Reg, Bool )] --список троек
makeInitAutomat reg currentAutomat = 
    let
        newAutomatA = addOneRegAutomat reg 'a' currentAutomat
        newAutomatB = addOneRegAutomat reg 'b' newAutomatA
        newAutomatC = addOneRegAutomat reg 'c' newAutomatB
        newAutomatD = addOneRegAutomat reg 'd' newAutomatC
    in newAutomatD

makeAutomat :: [(Reg, Char, Reg, Bool)] -> [(Reg, Char, Reg, Bool)]
makeAutomat [] = []
makeAutomat ((reg1, v, reg2, True):xs) = (reg1, v, reg2, True) : makeAutomat xs
makeAutomat ((reg1, v, reg2, False):xs) = 
    let
        currentAutomat = ((reg1, v, reg2, True):xs)
        newAutomatA = addOneRegAutomat reg2 'a' currentAutomat
        newAutomatB = addOneRegAutomat reg2 'b' newAutomatA
        newAutomatC = addOneRegAutomat reg2 'c' newAutomatB
        newAutomatD = addOneRegAutomat reg2 'd' newAutomatC
    in makeAutomat newAutomatD

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










--(((A*)|(b*)) # (a*))

-- Shuffle (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a')))"

--"Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a')))"
--"Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a')))"

-- Variable 
--     (Shuffle 
--         (Variable 
--             (Mul (OneVal Eps) (Klini (OneVal (Var 'a')))) 
--             (Mul (OneVal Empty) (Klini (OneVal (Var 'b'))))) 
--         (Klini (OneVal (Var 'a')))) 
--     (Mul 
--         (Variable 
--             (Klini (OneVal (Var 'a'))) 
--             (Klini (OneVal (Var 'b')))) 
--         (Mul (OneVal Eps) (Klini (OneVal (Var 'a')))))


--      ((a* | b*) a*)





-- Variable 
--     (Shuffle 
--         (Variable 
--             (Mul (OneVal Eps) (Klini (OneVal (Var 'a')))) 
--             (Mul (OneVal Empty) (Klini (OneVal (Var 'b'))))) 
--         (Klini (OneVal (Var 'a')))) 
--     (Mul 
--         (Variable (Klini (OneVal (Var 'a')))
--         (Klini (OneVal (Var 'b')))) 
--         (Mul (OneVal Eps) (Klini (OneVal (Var 'a')))))


-- Variable    
--     (Klini (OneVal (Var 'a'))) 
--     (Mul 
--         (Variable 
--             (Klini (OneVal (Var 'a'))) 
--             (Klini (OneVal (Var 'b')))) 
--         (Klini (OneVal (Var 'a'))))


--        --((a* | b*) a*) | a*


-- (Shuffle (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),'b',Shuffle (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),True),
-- (Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),'b',Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),True),
-- (Klini (OneVal (Var 'a')),'a',Klini (OneVal (Var 'a')),True),
-- (Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),'a',Klini (OneVal (Var 'a')),True),
-- (Shuffle (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),'a',Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),True),
-- (Shuffle (Variable (Klini (OneVal (Var 'a'))) (Klini (OneVal (Var 'b')))) (Klini (OneVal (Var 'a'))),'b',Shuffle (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),True),
-- (Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),'b',Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),True),(Klini (OneVal (Var 'a')),'a',Klini (OneVal (Var 'a')),True),
-- (Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),'a',Klini (OneVal (Var 'a')),True),(Variable (Klini (OneVal (Var 'a'))) (Mul (Variable (Klini (OneVal (Var 'a'))) (Klini (OneVal (Var 'b')))) (Klini (OneVal (Var 'a')))),'b',Mul (Klini (OneVal (Var 'b'))) (Klini (OneVal (Var 'a'))),True),
-- (Klini (OneVal (Var 'a')),'a',Klini (OneVal (Var 'a')),True),(Variable (Klini (OneVal (Var 'a'))) (Mul (Variable (Klini (OneVal (Var 'a'))) (Klini (OneVal (Var 'b')))) (Klini (OneVal (Var 'a')))),'a',Klini (OneVal (Var 'a')),True),
-- (Shuffle (Variable (Klini (OneVal (Var 'a'))) (Klini (OneVal (Var 'b')))) (Klini (OneVal (Var 'a'))),'a',Variable (Klini (OneVal (Var 'a'))) (Mul (Variable (Klini (OneVal (Var 'a'))) (Klini (OneVal (Var 'b')))) (Klini (OneVal (Var 'a')))),True)]"
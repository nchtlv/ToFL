module Lib
    ( 
      splitByArrow,
      prepareAutomate,
      makeAutomate,
      Automate(..)
      --findSG
    ) where

import Data.List.Split

type State = [(String, String)]


data Automate = Automate {
    freeNumber :: Int,
    states :: [(String, State, Bool)],
    grammer :: State,
    transactions :: [(String, Char, String)]
} deriving Show

makeCortege :: [String] -> (String, String)
makeCortege (x1:x2:_) = (x1, x2)
makeCortege _ = ("error", "error")

splitByArrow :: String -> (String, String)
splitByArrow str =
    makeCortege (splitOn "::=" str)

prepareAutomate :: [(String, String)] -> [(String, String)]
prepareAutomate xs = ("S'", ".S") : findSG xs

findSG:: [(String, String)] -> [(String, String)]
findSG [] = []
findSG ((left,right):xs) = 
    if left == "S" 
        then (left, '.' : right) : findSG xs
        else findSG xs

getTransSymbol :: State -> [Char]
getTransSymbol [] = []
getTransSymbol ((left, right) : xs) = 
    case findSymbolAfterDot right of 
        Nothing -> getTransSymbol xs
        Just s -> s : getTransSymbol xs


findSymbolAfterDot :: String -> Maybe Char
findSymbolAfterDot [] = Nothing
findSymbolAfterDot [x] = Nothing
findSymbolAfterDot (x1 : x2 : xs) = 
    if x1 == '.' then Just x2 else findSymbolAfterDot (x2 : xs)

makeAutomate:: Automate -> Automate
makeAutomate (Automate n states grammer transactions) =
    if isNeedNewStep states 
        then makeAutomate (oneMoreStep states (Automate n states grammer transactions))
        else Automate n states grammer transactions 

isNeedNewStep :: [(String, State, Bool)] -> Bool
isNeedNewStep [] = False
isNeedNewStep ((_, _, flag):xs) = (not flag) || isNeedNewStep xs 


oneMoreStep :: [(String, State, Bool)] -> Automate-> Automate
oneMoreStep [] a = a
oneMoreStep ((name, rules, flag) : xs) a = 
    if flag 
        then oneMoreStep xs a
        else oneMoreStep xs (addNewState name rules (makeSet' (getTransSymbol rules)) (markState name a))
 
markState :: String -> Automate -> Automate
markState name (Automate n states grammer transactions) =
    Automate n (markState' name states) grammer transactions

markState':: String -> [(String, State, Bool)] -> [(String, State, Bool)]
markState' _ [] = []
markState' name ((name1, state1, flag) : xs) = 
    if name == name1 then ((name1, state1, True) : xs) else ((name1, state1, flag) : markState' name xs)

addNewState :: String -> State -> [Char] -> Automate -> Automate
addNewState name rules [] a = a
addNewState name rules (x : xs) a = do
    let 
        newState = addNewStateBySymbol rules x
        newA = addNewStateToA name x newState a
    addNewState name rules xs newA

addNewStateToA :: String -> Char -> State -> Automate -> Automate
addNewStateToA name x rules (Automate n states grammer transactions)  = 
    let 
        newRules = rules ++ (makeSet (applyGrammer x rules grammer))
    in
        Automate (n+1) (( x : show n, newRules, False) : states) grammer ((name, x, x : show n) : transactions)


makeSet :: State -> State
makeSet [] = []
makeSet (x : xs) =
    if member x xs 
        then makeSet xs
        else x : makeSet xs


member :: (String, String) -> State -> Bool
member (_, _) [] = False
member (left, right) ((left1, right1) : xs) = 
    if left == left1 && right == right1 then True else member (left, right) xs


makeSet' :: [Char] -> [Char] 
makeSet' [] = []
makeSet' (x : xs) =
    if member' x xs 
        then makeSet' xs
        else x : makeSet' xs


member' :: Char -> [Char] -> Bool
member' _ [] = False
member' x (x1:xs) = 
    if x == x1 then True else member' x xs


applyGrammer :: Char -> State -> State -> State
applyGrammer x [] _ = []
applyGrammer x ((left, right) : xs) grammer = do
    case findSymbolAfterDot right of 
        Nothing -> applyGrammer x xs grammer
        Just r -> 
            if r /= x 
                then applyGrammer' r grammer ++ applyGrammer x xs grammer
                else applyGrammer x xs grammer 

applyGrammer' :: Char -> State -> State
applyGrammer' x [] = []
applyGrammer' x (([left], right) : xs) = 
    if x == left 
        then ([left], '.' : right) : (applyGrammer' x xs)
        else applyGrammer' x xs
applyGrammer' x ((_, _) : xs) = applyGrammer' x xs


addNewStateBySymbol :: State -> Char -> State
addNewStateBySymbol [] symb = []
addNewStateBySymbol ((left, right) : xs) symb = 
    case findSymbolAfterDot right of
        Nothing -> addNewStateBySymbol xs symb
        Just r -> 
            if r == symb 
                then (left, moveDot right) : addNewStateBySymbol xs symb 
                else addNewStateBySymbol xs symb

moveDot :: String -> String
moveDot [] = []
moveDot [x] = [x]
moveDot (x1: x2 : xs) =
    if x1 == '.' then x2 : x1 : xs else x1 : moveDot (x2: xs)


-- Automate {freeNumber = 2, 
-- states = [("a1",[("S","a.Dc"),("D",".Db"),("D",".b")],True),
-- ("S0",[("S'","S.")],True),
-- ("Init",[("S'",".S"),("S",".aDc")],True)], 
-- grammer = [("S","aDc"),("D","Db"),("D","b")], 
-- transactions = [("Init",'a',"a1"),("Init",'S',"S0")]}


-- Automate {freeNumber = 2, 
-- states = [("a1",[("S","a.Dc")],True),
-- ("S0",[("S'","S."),("S",".aDc")],True),
-- ("Init",[("S'",".S"),("S",".aDc")],True)], 
-- grammer = [("S","aDc"),("D","Db"),("D","b")], 
-- transactions = [("Init",'a',"a1"),("Init",'S',"S0")]}

-- Automate {freeNumber = 9, 
-- states = [("b8",[("D","Db.")],True),
-- ("c7",[("S","aDc.")],True),
-- ("b6",[("D","Db.")],True),
-- ("c5",[("S","aDc.")],True),
-- ("b4",[("D","b.")],True),
-- ("D3",[("S","aD.c"),("D","D.b")],True),
-- ("D2",[("S","aD.c"),("D","D.b")],True),
-- ("a1",[("S","a.Dc"),("D",".Db"),("D",".b")],True),
-- ("S0",[("S'","S.")],True),
-- ("Init",[("S'",".S"),("S",".aDc")],True)],
-- grammer = [("S","aDc"),("D","Db"),("D","b")], 
-- transactions = [("D2",'b',"b8"),
-- ("D2",'c',"c7"),
-- ("D3",'b',"b6"),
-- ("D3",'c',"c5"),
-- ("a1",'b',"b4"),
-- ("a1",'D',"D3"),
-- ("a1",'D',"D2"),
-- ("Init",'a',"a1"),("Init",'S',"S0")]}

Automate {freeNumber = 6, 
states = [("b5",[("D","Db.")],True),
("c4",[("S","aDc.")],True),
("b3",[("D","b.")],True),
("D2",[("S","aD.c"),("D","D.b")],True),
("a1",[("S","a.Dc"),("D",".Db"),("D",".b")],True),
("S0",[("S'","S.")],True),
("Init",[("S'",".S"),("S",".aDc")],True)], 
grammer = [("S","aDc"),("D","Db"),("D","b")], 
transactions = [("D2",'b',"b5"),
("D2",'c',"c4"),
("a1",'b',"b3"),
("a1",'D',"D2"),
("Init",'a',"a1"),
("Init",'S',"S0")]}
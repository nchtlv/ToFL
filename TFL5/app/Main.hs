module Main (main) where

import Lib

main :: IO ()
main = do
    print("Enter the grammer into 'test1.txt'. First not-termil must be 'S'")
    str <- readFile "static/test1.txt"
    let parsFunc = map (splitByArrow) $ lines $ filter (\c -> c /= ' ') str
    print parsFunc
    print $ prepareAutomate parsFunc
    print $ makeAutomate (Automate 0 [("Init", prepareAutomate parsFunc, False)] (("S'", "S"): parsFunc) [])
    print $ addLastStates $ makeAutomate (Automate 0 [("Init", prepareAutomate parsFunc, False)] (("S'", "S"): parsFunc) [])


-- Automate {freeNumber = 8, 
-- states = [(")7",[("T","(E).")],True),("E6",[("T","(E.)")],True),("(5",[("T","(.E)")],True),("n4",[("T","n.")],True),("T3",[("S","S+T.")],True),
-- ("+2",[("S","S+.T"),("T",".n"),("T",".(E)")],True),("T1",[("S","T.")],True),("S0",[("S'","S."),("S","S.+T")],True),
-- ("Init",[("S'",".S"),("S",".S+T"),("S",".T")],True)], 
-- grammer = [("S'","S"),("S","S+T"),("S","T"),("T","n"),("T","(E)")], 

-- transactions = [(")7",'|',"tick"),
-- ("n4",'|',"*3"),("n4",'n',"*3"),
-- ("T3",'|',"tick"),
-- ("T1",'|',"tick"),
-- ("E6",')',")7"),
-- ("(5",'E',"E6"),
-- ("+2",'(',"(5"),("+2",'n',"n4"),("+2",'T',"T3"),
-- ("S0",'+',"+2"),("S0",'|',"tick"),
-- ("Init",'T',"T1"),("Init",'S',"S0")]}

module Lib
    ( makeLineFunc,
      makeComp, getUniqueSimbols,
      splitByArrow,
      makeCortege,
      makeFuncs,
      printComp,
      outputFile,
      printChecks,
      printVars,
      printRegEx,
      showExpr
    ) where

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
    , Max (Max (Sum a1 d1) (Sum a2 d2)) b1
    , Max (Max (Sum a3 d1) (Sum a4 d2)) b2
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

printComp :: [(Func, Func)] -> IO ()
printComp [] = return ()
printComp((x1, x2):xs) = do
    print $ makeComp x1
    print $ makeComp x2
    printComp xs


outputFile :: [(Func, Func)] -> String -> IO ()
outputFile fs vars =
    writeFile "static/output1.txt" $
        "(set-logic QF_NIA)\n\
        \(define-fun arcMax ((x1 Int) (x2 Int)) Int (ite (> x1 x2) x1 x2))\n\
        \(define-fun arcSum ((x1 Int) (x2 Int)) Int (ite (= x1 -1) x1 (ite (= x2 -1) x2 (+ x1 x2))))\n\
        \(define-fun >> ((a Int) (b Int)) Bool (or (> a b) (and (= a -1) (= b -1))))\n"
        ++ printVars vars
        ++ printRegEx fs
        ++ printChecks vars
        ++ "(check-sat)\n\
           \(get-model)\n"

printChecks :: String -> String
printChecks "" = ""
printChecks (x:xs) =
    "(assert (>= a" ++ [x] ++ "1 0))\n\
    \(assert (or (>= a"++ [x] ++ "2 0) (= a" ++ [x] ++ "2 -1)))\n\
    \(assert (or (>= a"++ [x] ++ "3 0) (= a" ++ [x] ++ "3 -1)))\n\
    \(assert (or (>= a" ++ [x] ++ "4 0) (= a" ++ [x] ++ "4 -1)))\n\
    \(assert (>= b" ++ [x] ++ "1 0))\n\
    \(assert (or (>= b" ++ [x] ++ "2 0) (= b" ++ [x] ++ "2 -1)))\n"
    ++ printChecks xs


printVars :: String -> String
printVars "" = ""
printVars (v:vs) =
    "(declare-fun a" ++ [v] ++ "1 () Int)\n\
    \(declare-fun a" ++ [v] ++ "2 () Int)\n\
    \(declare-fun a" ++ [v] ++ "3 () Int)\n\
    \(declare-fun a" ++ [v] ++ "4 () Int)\n\
    \(declare-fun b" ++ [v] ++ "1 () Int)\n\
    \(declare-fun b" ++ [v] ++ "2 () Int)\n"
    ++ printVars vs


printRegEx :: [(Func,Func)] -> String
printRegEx [] = ""
printRegEx ((f1, f2):regs) = do
    let
        res1 = makeComp f1
        res2 = makeComp f2
    case (res1, res2) of
        (Line (a1, a2, a3, a4, b1, b2), Line (c1, c2, c3, c4, d1, d2)) ->
            "(assert (>> " ++ showExpr a1 ++ " " ++ showExpr c1 ++ "))\n\
            \(assert (>> " ++ showExpr a2 ++ " " ++ showExpr c2 ++ "))\n\
            \(assert (>> " ++ showExpr a3 ++ " " ++ showExpr c3 ++ "))\n\
            \(assert (>> " ++ showExpr a4 ++ " " ++ showExpr c4 ++ "))\n\
            \(assert (>> " ++ showExpr b1 ++ " " ++ showExpr d1 ++ "))\n\
            \(assert (>> " ++ showExpr b2 ++ " " ++ showExpr d2 ++ "))\n"
            ++ printRegEx regs
        _ -> "ERROR"

showExpr :: Expr -> String
showExpr (Val str) = str
showExpr (Sum e1 e2) = "(arcSum " ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
showExpr (Max e1 e2) = "(arcMax " ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"

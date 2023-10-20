module Main (main) where

import Lib

main :: IO ()
main = do
    str <- readFile "static/test1.txt"
    let parsFunc = map (makeFuncs . splitByArrow) $ lines $ filter (\c -> c /= ' ') str
    print parsFunc
    printComp parsFunc
    outputFile parsFunc (getUniqueSimbols str)

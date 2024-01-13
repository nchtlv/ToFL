module Main (main) where

import Lib

main :: IO ()
main = do
    print("Enter the grammer into 'test1.txt'. First not-termil must be 'S'")
    str <- readFile "static/test1.txt"
    let parsFunc = map (splitByArrow) $ lines $ filter (\c -> c /= ' ') str
    print parsFunc
    print $ prepareAutomate parsFunc
    print $ makeAutomate (Automate 0 [("Init", prepareAutomate parsFunc, False)] parsFunc [])

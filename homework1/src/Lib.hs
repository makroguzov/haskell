module Lib
    ( someFunc, saveInputs
    ) where


someFunc :: IO ()
someFunc = do 
        s <- getLine 
        if s == "privet" then print "poka"
                         else someFunc


saveInputs :: [String] -> IO ()
saveInputs list = do
                s <- getLine
                if s == "mark" then print list
                               else saveInputs (s: list)


getNumber :: IO() 
getNumber = do
        s <- getLine
        print $ splitNumber s 0

mapToNumber :: Char -> Integer
mapToNumber c = toEnum $ fromEnum c - 48 
                               
mapToChar :: Integer -> Char
mapToChar i = toEnum (fromInteger i + 48)
                               
madSumm :: Char -> Integer -> Char
madSumm c ind = mapToChar $ (mapToNumber c + ind) `mod` 10
                               
splitNumber :: String -> Integer -> [Char] 
splitNumber s ind = if s == "" then [] 
                    else (madSumm (head s) ind): (splitNumber  (tail s) (ind + 1))  
                               

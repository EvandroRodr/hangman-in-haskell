module DictPaths where

dictWords :: [Char] -> FilePath
dictWords selection
    | selection == "1" = "dictionaries/dictionary1.txt"
    | selection == "2" = "dictionaries/dictionary2.txt"
    | selection == "3" = "dictionaries/dictionary3.txt"
    | otherwise = "dictionary1.txt"
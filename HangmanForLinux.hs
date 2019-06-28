import System.IO
import System.Process
import System.Random (randomIO)
import Data.Char
import Data.List
import Data.List (transpose)

import DictPaths

hangmanMatrix :: [[String]]
hangmanMatrix =
    transpose
    [["   ", " O ", " O ", " O ", " O " , " O " , " O " ]
    ,["   ", "   ", " | ", " | ", " | " , "/| " , "/|\\" ]
    ,["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\"]]


render :: Int -> [String]
render index =
    "-----------" :
    "|    |" :  
    map ("|   " ++) image
    where image = hangmanMatrix !! index

maxErrors :: Int
maxErrors = length hangmanMatrix - 1

showWord :: String -> String
showWord word = intersperse ' ' [if a `elem` ['a'..'z'] then '_' 
                                else a | a <- word]

tryLetter :: String -> Char -> Int -> IO()
tryLetter word letter tries
    | letter `elem` word = game [if letter == a then toUpper letter else a | a <- word] tries
    | otherwise = game word (tries - 1)

sortWord :: [Char] -> IO[Char]
sortWord selection = do
    dictionary <- readFile (dictWords selection)
    let words = filter validWord $ lines dictionary
    let numWords = length words
    randomNumber <- randomIO
    let randomWord = words !! (randomNumber `mod` numWords)
    return $ randomWord
    where
            validWord word =
                '\'' `notElem` word &&
                map toLower word == word

game :: String -> Int -> IO()
game word tries
    | word == map toUpper word = do
        system("clear")
        putStrLn $ showWord word
        putStrLn "\nVOCE GANHOOOUUU! :DDD"
    | tries == 0 = do
        system("clear")
        putStrLn $ showWord word
        putStrLn $ unlines $ render(maxErrors - tries)
        putStrLn "\nvocê perdeu :( ..."
    | otherwise = do
        system("clear")
        putStrLn $ unlines $ render(maxErrors - tries)
        putStrLn $ "Você tem " ++ show tries ++ " tentativa(s) restantes."
        putStrLn $ showWord word
        putStrLn "Digite uma letra: "
        letter <- getLine
        tryLetter word (head letter) tries

main :: IO()
main = do
    system("clear")
    putStrLn "--------------------------------"
    putStrLn "|  Bem-Vindo ao Hangman Game!  |"
    putStrLn "--------------------------------"
    putStrLn "| Escolha um dicionário:       |"
    putStrLn "| 1. UFC                       |" 
    putStrLn "| 2. Frutas                    |"
    putStrLn "| 3. Países                    |"
    putStrLn "--------------------------------"
    putStr ">>> "
    selection <- getLine
    word <- sortWord selection
    system("clear")
    game (map toLower word) maxErrors
    putStrLn "\nObrigado por jogar! Espero que tenha se divertido! ;D"
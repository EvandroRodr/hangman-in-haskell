-- OBS: Para execução no Windows, torna-se necessária a instalação da biblioteca "Random".

-- Importando bibliotecas do Haskell para auxiliar na execução do código.
import System.IO
import System.Process
import System.Random (randomIO)
import Data.Char
import Data.List
import Data.List (transpose)

-- Importando pacote dos dicionários.
import DictPaths

-- Matriz transposta com o desenho do personagem.
hangmanMatrix :: [[String]]
hangmanMatrix =
    transpose
    [["   ", " O ", " O ", " O ", " O " , " O " , " O " ]
    ,["   ", "   ", " | ", " | ", " | " , "/| " , "/|\\" ]
    ,["   ", "   ", "   ", "/  ", "/ \\", "/ \\", "/ \\"]]

-- Função que recebe o numero de erros como parametro e rederiza o personagem com base nessa entrada.
render :: Int -> [String]
render index =
    "-----------" :
    "|    |" :  
    map ("|   " ++) image
    where image = hangmanMatrix !! index

-- Função para fixar o número máximo de erros com base na matriz transposta do personagem.
maxErrors :: Int
maxErrors = length hangmanMatrix - 1

-- FUnção que recebe uma string e substitui cada caractere por um '_"
showWord :: String -> String
showWord word = intersperse ' ' [if a `elem` ['a'..'z'] then '_' 
                                else a | a <- word]

-- Função que checa se determinada letra dada como entrada, pertence a palavra selecionada.
tryLetter :: String -> Char -> Int -> IO()
tryLetter word letter tries
    | letter `elem` word = game [if letter == a then toUpper letter else a | a <- word] tries
    | otherwise = game word (tries - 1)

-- Uma das principais funções do código. Recebe uma seleção como entrada, entre 1 e 3 e com base
-- nesta entrada seleciona um dicionário correspondente e em seguida escolhe uma palavra aleatória
-- deste dicionário, dando assim o começo do game.
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

-- Função que mantem o loop de jogo, com base no numero de tentativas.
game :: String -> Int -> IO()
game word tries
    | word == map toUpper word = do
        system("cls")
        putStrLn $ showWord word
        putStrLn "\nVOCE GANHOOOUUU! :DDD"
    | tries == 0 = do
        system("cls")
        putStrLn $ showWord word
        putStrLn $ unlines $ render(maxErrors - tries)
        putStrLn "\nvocê perdeu :( ..."
    | otherwise = do
        system("cls")
        putStrLn $ unlines $ render(maxErrors - tries)
        putStrLn $ "Você tem " ++ show tries ++ " tentativa(s) restantes."
        putStrLn $ showWord word
        putStrLn "Digite uma letra: "
        letter <- getLine
        tryLetter word (head letter) tries

-- Função principal, responsável por "rodar" o jogo.
main :: IO()
main = do
    system("cls")
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
    system("cls")
    game (map toLower word) maxErrors
    putStrLn "\nObrigado por jogar! Espero que tenha se divertido! ;D"
module Main where

import System.IO (hFlush, stdout)
import Data.Char (toLower)

-- função principal que inicia o game
main :: IO ()
main = do 
    putStrLn "Bem-vindo ao Jogo!"
    putStrLn "Digite a palavra:"
    -- chama a função para obter a palavra secreta
    secretWord <- getSecretWord
    putStrLn "Tente adivinhar a palavra!"
    -- inicia o loop principal do jogo com a palavra secreta e uma lista vazia de tentativas
    hangmanGame secretWord []

-- função para obter a palavra secreta
getSecretWord :: IO String
getSecretWord = do 
    -- le a palavra
    word <- getLine
    -- limpa a tela para esconder a palavra
    putStrLn "\ESC[2J"
    -- Retorna a palavra convertida para minúscula
    return $ map toLower word

-- função principal do jogo, controlando o loop de tentativas
hangmanGame :: String -> [Char] -> IO ()
hangmanGame secretWord attempts = do 
    -- exibe o estado atual da palavra, mostrando as letras adivinhadas e ocultando as não adivinhadas
    putStrLn $ currentState secretWord attempts
    -- mostra as vidas restantes
    putStrLn $ "Vidas restantes: " ++ show (6 - wrongAttempts secretWord attempts)
    -- verifica se o jogador ganhou 
    if won secretWord attempts then do
        putStrLn "Parabéns! Você ganhou!"
        playAgain <- askToPlayAgain
        if playAgain then do
            main  -- reinicia o jogo
        else
            putStrLn "Obrigado por jogar!"
    -- verifica se o jogador perdeu
    else if lost secretWord attempts then do
        putStrLn ("Você perdeu! A palavra era: " ++ secretWord)
        playAgain <- askToPlayAgain
        if playAgain then do
            main  -- reinicia o jogo
        else
            putStrLn "Obrigado por jogar!"
    else do
        putStrLn "Digite uma letra: "
        -- obtém a letra digitada
        letter <- getLetter
        -- verifica se a letra já foi tentada
        if letter `elem` attempts then do 
            putStrLn "Você já tentou essa letra. Tente outra"
            hangmanGame secretWord attempts
        else 
            hangmanGame secretWord (letter : attempts)

-- função para obter a letra
getLetter :: IO Char
getLetter = do 
    -- le a entrada
    letter <- getLine
    -- garante que foi digitado apenas uma letra
    if length letter /= 1 then do 
        putStrLn "Por favor digite apenas uma letra"
        getLetter
    else 
        -- retorna a letra convertida para minúscula
        return $ toLower $ head letter

-- função que mostra o estado atual da palavra, com letras adivinhadas e ocultas
currentState :: String -> [Char] -> String 
currentState secretWord attempts = map (revealLyrics attempts) secretWord

-- função que revela uma letra se já foi adivinhada, ou mostra '__' se não foi
revealLyrics :: [Char] -> Char -> Char
revealLyrics attempts letter = if letter `elem` attempts then letter else '_'

-- função que verifica se o jogador ganhou 
won :: String -> [Char] -> Bool
won secretWord attempts = all (`elem` attempts) secretWord

-- função que verifica se o jogador perdeu (limitando a 6 tentativas erradas)
lost :: String -> [Char] -> Bool
lost secretWord attempts = wrongAttempts secretWord attempts >= 6

-- função auxiliar para contar tentativas erradas
wrongAttempts :: String -> [Char] -> Int
wrongAttempts secretWord attempts = length $ filter (`notElem` secretWord) attempts

-- função para perguntar se o jogador quer jogar novamente
askToPlayAgain :: IO Bool
askToPlayAgain = do
    putStrLn "Deseja jogar novamente? (s/n)"
    answer <- getLine
    case answer of
        "s" -> return True
        "n" -> return False
        _ -> do
            putStrLn "Resposta inválida. Por favor, responda com 's' ou 'n'."
            askToPlayAgain

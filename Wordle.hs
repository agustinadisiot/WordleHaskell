module Wordle where
import Control.Monad.State
import Data.Char
import System.IO
import System.IO.Unsafe
import Data.Time
import System.Exit (exitSuccess)

-- Define the game State
data Stats = Stats { 
  gamesPlayed :: Int,
  winsGroupedByAttempt :: [(Int, Int)]
} 
instance Show Stats where
  show (Stats { gamesPlayed= games ,winsGroupedByAttempt = wins }) = "Games played: "++ show games ++ "\n" ++ unlines (map showOneStat wins)

showOneStat :: (Int, Int) -> String
showOneStat (attemptCount, wins) = show attemptCount ++ " " ++ replicate wins '|'  

data GameState = GameState {
  mode :: String,
  wordList :: [String],
  strategy :: Strategy,
  currentWord :: String,
  attemptsLeft :: Int,
  stats :: Stats
}

type Strategy = [String] -> String

data ResultStep =  X | O | I
    deriving (Show)
-- X means the letter is not in the word to guess
-- I means the letter is in the word to guess, but in a different position
-- O means the letter is in the word to guess, and in the same position

type Step = [ResultStep]

-- STRATEGIES

-- Random strategy, chooses a random word from the list (Single player)
randomInt :: Integer -> IO Integer
randomInt n =
  do
  time <- getCurrentTime
  return ( (`mod` n) $ read $ take 6 $ formatTime defaultTimeLocale "%q" time )

randomStrat :: Strategy
randomStrat wrs = unsafePerformIO (randomStr' wrs)

randomStr' :: [String] -> IO String
randomStr' wrs = do
  let len = length wrs
  idx <- randomInt (toInteger len-1)
  return (wrs !! fromInteger idx)

-- Human strategy, asks the user for a word (Multiplayer)
humanStrat :: Strategy
humanStrat _ = unsafePerformIO humanStrat'

humanStrat' :: IO String
humanStrat' = do
    putStrLn "Player 1, enter your word: "
    wrd <- getLine >>= checkExit
    putStr "\ESC[A"
    putStr "\ESC[2K" -- Clear the line, so player 2 can't see the word chosen
    if length wrd /= 5 then do
        putStrLn "The word must have 5 letters."
        humanStrat'
    else do
      putStrLn "Player 2, Guess the word!"
      return wrd

------------------------------------------------------------------------------------------------------------------------

-- GAME

-- play starts the game, type "play" to start
play :: IO()
play = do
    putStrLn "\ESC[37m .----------------.  .----------------.  .----------------.  .----------------.  .----------------.  .----------------. "
    putStrLn "\ESC[37m| .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. |"
    putStrLn "\ESC[37m| |\ESC[91m _____  _____ \ESC[37m| || |\ESC[35m     ____     \ESC[37m| || |\ESC[34m  _______     \ESC[37m| || |\ESC[36m  ________    \ESC[37m| || |\ESC[92m   _____      \ESC[37m| || |\ESC[93m  _________   \ESC[37m| |"
    putStrLn "\ESC[37m| |\ESC[91m|_   _||_   _|\ESC[37m| || | \ESC[35m  .'    `.   \ESC[37m| || |\ESC[34m |_   __ \\    \ESC[37m| || |\ESC[36m |_   ___ `.  \ESC[37m| || |\ESC[92m  |_   _|     \ESC[37m| || |\ESC[93m |_   ___  |  \ESC[37m| |"
    putStrLn "\ESC[37m| |\ESC[91m  | | /\\ | | \ESC[37m | || |\ESC[35m  /  .--.  \\\ESC[37m  | || |\ESC[34m   | |__) |   \ESC[37m| || |\ESC[36m   | |   `. \\ \ESC[37m| || |\ESC[92m    | |       \ESC[37m| || |\ESC[93m   | |_  \\_|  \ESC[37m| |"
    putStrLn "\ESC[37m| |\ESC[91m  | |/  \\| | \ESC[37m | || |\ESC[35m  | |    | |  \ESC[37m| || |\ESC[34m   |  __ /    \ESC[37m| || |\ESC[36m   | |    | | \ESC[37m| || |\ESC[92m    | |   _   \ESC[37m| || |\ESC[93m   |  _|  _   \ESC[37m| |"
    putStrLn "\ESC[37m| |\ESC[91m  |   /\\   |  \ESC[37m| || | \ESC[35m \\  `--'  /  \ESC[37m| || |\ESC[34m  _| |  \\ \\_  \ESC[37m| || |\ESC[36m  _| |___.' / \ESC[37m| || |\ESC[92m   _| |__/ |\ESC[37m  | || |\ESC[93m  _| |___/ |  \ESC[37m| |"
    putStrLn "\ESC[37m| |\ESC[91m  |__/  \\__| \ESC[37m | || |\ESC[35m   `.____.'   \ESC[37m| || |\ESC[34m |____| |___| \ESC[37m| || |\ESC[36m |________.'  \ESC[37m| || |\ESC[92m  |________|  \ESC[37m| || |\ESC[93m |_________|  \ESC[37m| |"
    putStrLn "\ESC[37m| |              | || |              | || |              | || |              | || |              | || |              | |"
    putStrLn "\ESC[37m| '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' |"
    putStrLn "\ESC[37m '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------' \ESC[0m"
    putStrLn "The computer will choose a word from the list, and you have to guess it in 6 tries."
    putStrLn "You can choose either a \ESC[35mSingle Player \ESC[0mgame, where the computer chooses a word randomly, or a \ESC[92mMultiplayer \ESC[0mgame, where someone else chooses a word for you to guess."
    putStrLn "Type \ESC[35m's' \ESC[0mfor Single Player, \ESC[92m'm' \ESC[0mfor Multiplayer, or any to quit."
    strat <- getLine >>= checkExit
    if strat == "m" then do
      let initialState = GameState { mode = "e", wordList = [], strategy = humanStrat, currentWord = "", attemptsLeft = 6, stats = Stats {  gamesPlayed = 0, winsGroupedByAttempt = [(1,0), (2,0) ,(3,0) ,(4,0) ,(5,0) ,(6,0)] }}
      evalStateT startPlay initialState
    else if strat == "s" then do
      putStrLn "Now chose if you want to play with english or spanish words. Type \ESC[34m'e' \ESC[0mfor english, \ESC[93m's' \ESC[0mfor spanish, or any to quit."
      lang <- getLine >>= checkExit
      putStrLn "Now chose if you want to play easy or hard mode. In hard mode you can only make guesses from valid words on the list. Type \ESC[93m'h' \ESC[0mfor hard, or any for easy."
      mode <- getLine >>= checkExit
      if lang == "e" then do
        let initialState = GameState { mode = mode , wordList = [], strategy = randomStrat, currentWord = "", attemptsLeft = 6 , stats = Stats {  gamesPlayed = 0, winsGroupedByAttempt = [(1,0), (2,0) ,(3,0) ,(4,0) ,(5,0) ,(6,0)] }}
        finalState <- execStateT (readWordsFromFile "words.txt") initialState
        evalStateT startPlay finalState
      else if lang == "s" then do
        let initialState = GameState { mode = mode , wordList = [], strategy = randomStrat, currentWord = "", attemptsLeft = 6 , stats = Stats {  gamesPlayed = 0, winsGroupedByAttempt = [(1,0), (2,0) ,(3,0) ,(4,0) ,(5,0) ,(6,0)] }}
        finalState <- execStateT (readWordsFromFile "palabras.txt") initialState
        evalStateT startPlay finalState
      else putStrLn "Goodbye!"
    else putStrLn "Goodbye!"

-- The main game function to start playing
startPlay :: StateT GameState IO ()
startPlay = do
  gameState <- get
  let !wrd = strategy gameState (wordList gameState)
  modify (\st -> st { currentWord = wrd, attemptsLeft = 6 })
  play'
  updatedGameState <- get 
  liftIO $ do
    putStrLn "Do you want to play again with the same settings? Type 'y' for yes, anything else for no."
    cont <- getLine >>= checkExit
    if cont == "y" 
      then evalStateT startPlay updatedGameState 
      else play

-- The main loop of the game in the StateT monad
play' :: StateT GameState IO ()
play' = do
  gameState <- get
  let ac = attemptsLeft gameState
  if ac == 0 then do
    modify (\st -> st { stats = Stats { gamesPlayed = gamesPlayed (stats st) + 1, winsGroupedByAttempt = winsGroupedByAttempt (stats st) } })
    updatedGameState <- get
    liftIO $ do
      putStrLn "\ESC[91mYou Lose!\ESC[0m"
      putStrLn ("The word was: \ESC[91m" ++ currentWord gameState ++ "\ESC[0m")
      print (stats updatedGameState)
  else do
    liftIO $ do
      putStrLn ("Guesses left: " ++ show ac)
      putStrLn "Enter your guess: "
    guess <- liftIO $ getLine >>= checkExit
    if length guess /= 5 then do
      liftIO $ putStrLn "\ESC[93mThe word must have 5 letters.\ESC[0m"
      play'
    else if mode gameState == "h" && lowerCase guess `notElem` wordList gameState then do
      liftIO $ putStrLn "\ESC[93mThe word is not on word list.\ESC[0m"
      play'
    else if lowerCase guess == lowerCase (currentWord gameState) then do
      modify (\st -> st { stats = Stats { gamesPlayed = gamesPlayed (stats st) + 1, winsGroupedByAttempt = updateWinsGroupedByAttempt (winsGroupedByAttempt (stats st)) (7-ac) }})
      updatedGameState <- get
      liftIO $ do
        putStrLn "\ESC[92mYou Win!\ESC[0m"
        putStrLn ("The word was: \ESC[92m" ++ currentWord gameState ++ "\ESC[0m")
        print (stats updatedGameState)
    else do
      let res = paintWord guess (step guess (currentWord gameState) (currentWord gameState))
      liftIO $ putStrLn res
      modify (\st -> st { attemptsLeft = ac - 1 })
      play'

------------------------------------------------------------------------------------------------------------------------

--HELPERS

-- lowerCase converts a word to lowercase, making the guesses case insensitive
lowerCase :: String -> String
lowerCase = map toLower

-- step compares the guess with the word to guess and returns a list of ResultStep
-- ResultStep is a list with the same lenght as the word to guess, with each element being either X, I or O
step :: String-> String -> String -> Step
step [w] [g] gssWrd
  |toLower w == toLower g = [O]
  |toLower w `elem` lowerCase gssWrd = [I]
  |otherwise = [X]
step (w : ws) (g : gs) gssWrd
  | toLower w == toLower g = O : step ws gs gssWrd
  | toLower w `elem` lowerCase gssWrd = I : step ws gs gssWrd
  | otherwise = X : step ws gs gssWrd

-- paintWord paints the result of the step in colors, pretty printing the result
paintWord :: String -> Step -> String  
paintWord [] _ = "\ESC[0m"
paintWord (w:ws) (X:xs) = "\ESC[91m" ++ [w] ++ paintWord ws xs
paintWord (w:ws) (I:xs) = "\ESC[93m" ++ [w] ++ paintWord ws xs
paintWord (w:ws) (O:xs) = "\ESC[92m" ++ [w] ++ paintWord ws xs

-- [Stats helper] updateWinsGroupedByAttempt updates the stats of the game
updateWinsGroupedByAttempt :: [(Int, Int)] -> Int -> [(Int, Int)]
updateWinsGroupedByAttempt xs n = map (\(a,b) -> if a == n then (a,b+1) else (a,b)) xs

-- [Words helper] Function to read a file and store the words in the game state
readWordsFromFile :: FilePath -> StateT GameState IO ()
readWordsFromFile filePath = do
    content <- liftIO $ readFile filePath
    let wordsList = lines content
    modify (\st -> st { wordList = wordsList })

-- Function to check if the user wants to exit the game, by typing "exit" at any time
checkExit :: String -> IO String
checkExit input
  | lowerCase input == "exit" = do
      putStrLn "Exiting the game. Goodbye!"
      exitSuccess
  | otherwise = return input

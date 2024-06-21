module Main where
import Wordle ( play )

{- 
Instructions:
Try guess a 5 letter word in 6 tries.
You can choose either a Single Player game, where the computer chooses a word randomly, or a Multiplayer game, where someone else chooses a word for you to guess.
The computer will give you feedback on your guesses, showing you how many letters are correct and how many are in the correct position with corresponding colors:
- Green: correct letter in correct position
- Yellow: correct letter in wrong position
- Red: incorrect letter
You can choose english words or spanish words.
You can choose hard mode where the word you type must be a valid word from the dictionary, in easy mode you can guess whatever 5 characters, even if they don't form a valid word.
You can stop the game at any time by typing 'exit'.
Enjoy!
-} 

main :: IO ()
main = do
    play

{- 
Compile module:
`ghc main.hs`
Run module:
`./main` or run .exe file
-} 

-- Important!!: IO backspace only works in compiled code, not in ghci

-- Information about implamentation with State Monad:
-- StateT Monad Transformer vs State:
-- The StateT monad transformer is a more general tool that allows you to add stateful computations to other monads. It's useful for integrating state management with other effects like I/O, Maybe, etc.
-- State is a simple monad specifically for state management vs. StateT is a monad transformer that allows you to add state management to any other monad.
-- State s a provides a pure stateful computation vs. StateT s m a provides a stateful computation within another monad m.

-- tech debt:
-- if word to guess is hello and guess in hhhhh, it should return OXXXX, not OIIII. So no repeated hints for the same letter, to complicated
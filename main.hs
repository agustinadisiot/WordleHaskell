module Main where
import Wordle ( play , GameState(..), Stats(..) )
import Control.Monad.State

main :: IO ()
main = do
    let initialState = GameState { mode = "" , wordList = [], strategy = undefined, currentWord = "", attemptsLeft = 6 , stats = Stats {  gamesPlayed = 0, winsGroupedByAttempt = [(1,0), (2,0) ,(3,0) ,(4,0) ,(5,0) ,(6,0)] }}
    evalStateT play initialState

{-
Agustina Disiot - 221025
Ivan Monjardin - 239850
-}
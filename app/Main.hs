{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Hashable (Hashable)
import qualified System.Random.PCG as Rand

import AlphaBeta
import qualified AlphaBeta.Ordering as Ordering
import Game
import Game.Display
import Misere
import Prog
import Reversed (Reversed)
import qualified Reversed
import Strategy
import TicTacToe

getGame :: IO (Misere TicTacToe)
getGame = return $ Misere TicTacToe

getAgent :: Hashable (Position g) => IO (AlphaBeta g)
getAgent = do
  seed <- Rand.withSystemRandom Rand.uniformW64
  return $ AlphaBeta $ Ordering.random seed

main :: IO ()
main = do
  game <- getGame
  agent <- getAgent
  runProg (GameContext game agent Reversed.empty)

data GameContext g s = GameContext
  { cgame   :: g
  , cstrat  :: s
  , history :: Reversed (Action g)
  }

instance (Display g, Strategy s g) => Prog (GameContext g s) where
  display GameContext{..} =
    let pos = playGame cgame history in
    case next cgame pos of
      End v -> unlines
          [ displayPosition cgame pos
          , ""
          , "Game over."
          , displayGameValue cgame v
          ]
      Options{} -> displayPosition cgame pos
  generateOptions GameContext{..} =
    let pos = playGame cgame history
        undoOption = case Reversed.tryUnsnoc history of
          Nothing -> []
          Just (rest, _) ->
            [ (option{name="undo", orFirstLetter=True}
            , Modify GameContext{history = rest, ..})
            ]
        quitOption = [(option{name="quit", orFirstLetter=True}, Quit)]
    in undoOption ++ quitOption ++ case next cgame pos of
      End _     -> []
      Options{} ->
        let
          am = actionMap cgame pos
          analyzeOption =
            ( option{name = "analyze", orFirstLetter = True}
            , Info $ displayGameValue cgame $ evaluate cgame cstrat pos
            )
          analyzeMove (name, act) =
            name ++ ": " ++
            displayGameValue
              cgame
              (evaluate cgame cstrat (makeMove cgame pos act))
          analyzeMovesOption =
            ( option{name = "move-analysis", orFirstLetter = True}
            , Info $ unlines $ map analyzeMove am
            )
          actOption (name, act) =
            ( option{name}
            , Modify GameContext{history = history `Reversed.snoc` act, ..}
            )
        in analyzeOption : analyzeMovesOption : map actOption am

playGame :: Game g => g -> Reversed (Action g) -> Position g
playGame g = foldl (makeMove g) (start g) . Reversed.toList

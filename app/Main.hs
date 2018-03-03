{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Hashable (Hashable)
import qualified System.Random.PCG as Rand

import AlphaBeta
import qualified AlphaBeta.Ordering as Ordering
import DotsAndBoxes
import Game
import Game.Display
import Game.Value (GameValue)
import Prog
import Reversed (Reversed)
import qualified Reversed
import qualified Strategy

getGame :: IO DotsAndBoxes
getGame = return DotsAndBoxes{nrows = 2, ncols = 2}

getAgent :: Hashable (Position g) => IO (AlphaBeta g)
getAgent = do
  seed <- Rand.withSystemRandom Rand.uniformW64
  return $ AlphaBeta $ Ordering.random seed

main :: IO ()
main = do
  game <- getGame
  agent <- getAgent
  runProg (GameContext game Reversed.empty (Strategy.evaluate game agent))

data GameContext g = GameContext
  { cgame    :: g
  , history  :: Reversed (Action g)
  , evaluate :: Position g -> GameValue
  }

instance Display g => Prog (GameContext g) where
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
            , Info $ displayGameValue cgame $ evaluate pos
            )
          analyzeMove (name, act) =
            name ++ ": " ++
            displayGameValue
              cgame
              (evaluate (makeMove cgame pos act))
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

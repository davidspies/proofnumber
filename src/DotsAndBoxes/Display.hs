{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DotsAndBoxes.Display () where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Reflection (Reifies, reflect)

import qualified DotsAndBoxes.EdgeVector as EdgeVector
import DotsAndBoxes.Game
import Game (getOptions)
import Game.Display

instance Reifies gs GridSize => Display (DotsAndBoxes gs) where
  displayPosition g Position{..} = unlines (hrow 0 : [
      vrow row ++ "\n" ++ hrow row
      | row <- [1..nrows]
    ]) ++ gameInfo
    where
      GridSize{..} = reflect g
      strFor :: Direction -> String
      strFor = \case {H -> "---"; V -> " | "}
      printEdge :: Edge gs -> String
      printEdge e@Edge{dir}
        | EdgeVector.member e edges = strFor dir
        | otherwise = showPadded (edgeNum e)
      hrow :: Int -> String
      hrow r =
        " +" ++
        intercalate
          "+"
          [printEdge (Edge r c H) | c <- [1..ncols]] ++
        "+"
      vrow :: Int -> String
      vrow r = unwords [printEdge (Edge r c V) | c <- [0..ncols]]
      gameInfo = unlines
        [ "Left: " ++ show leftScore
        , "Right: " ++ show rightScore
        , "Turn: " ++ show turn
        ]
  actionMap g pos =
    map (\a@(Action e) -> (show $ edgeNum e, a)) (getOptions g pos)

edgeNum :: Reifies gs GridSize => Edge gs -> Int
edgeNum = (m Map.!)
  where
    m = Map.fromList $ zip allPositions [1..]

showPadded :: Int -> String
showPadded n =
  let res = show n
  in case length res of
    1 -> " " ++ res ++ " "
    2 -> res ++ " "
    _ -> error "Number too large"

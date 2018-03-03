{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DotsAndBoxes.Display () where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import DotsAndBoxes.Internal
import Game (getOptions)
import Game.Display

instance Display DotsAndBoxes where
  displayPosition g@DotsAndBoxes{..} Position{..} = unlines (hrow 0 : [
      vrow row ++ "\n" ++ hrow row
      | row <- [1..nrows]
    ]) ++ gameInfo
    where
      strFor :: Direction -> String
      strFor = \case {H -> "---"; V -> " | "}
      printEdge :: Edge -> String
      printEdge e@Edge{dir}
        | e `Set.member` edges = strFor dir
        | otherwise = showPadded (edgeNum g e)
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
    map (\a@(Action e) -> (show $ edgeNum g e, a)) (getOptions g pos)

edgeNum :: DotsAndBoxes -> Edge -> Int
edgeNum g = (m Map.!)
  where
    m = Map.fromList $ zip (allPositions g) [1..]

showPadded :: Int -> String
showPadded n =
  let res = show n
  in case length res of
    1 -> " " ++ res ++ " "
    2 -> res ++ " "
    _ -> error "Number too large"

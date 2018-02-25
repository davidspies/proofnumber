{-# LANGUAGE RecordWildCards #-}

module Prog
    ( Prog(..)
    , Operate(..)
    , Option(..)
    , option
    , runProg
    ) where

import Control.Monad (when)
import Control.Monad.Loops (untilJust)
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (isNothing)
import GHC.Stack (HasCallStack)

data Option = Option
  { name          :: String
  , orFirstLetter :: Bool
  }

option :: Option
option = Option{name=error "name unset", orFirstLetter=False}

data Operate p = Modify p | Info String | Quit

class Prog p where
  display :: p -> String
  generateOptions :: p -> [(Option, Operate p)]

matches :: String -> Option -> Bool
matches (map toLower -> s) Option{..} =
  s == map toLower name || orFirstLetter && s == [toLower (head name)]

iterateUntilQuit :: (a -> IO (Operate a)) -> a -> IO ()
iterateUntilQuit f = go
  where
    go x = f x >>= \case
      Modify y -> go y
      Info inf -> putStrLn inf >> go x
      Quit     -> return ()

runProg :: (HasCallStack, Prog p) => p -> IO ()
runProg = iterateUntilQuit go
  where
    go state = do
      putStrLn $ display state
      putStrLn "Options:"
      let opts = generateOptions state
      print $ map (Unquoted . showOpt . fst) opts
      fmap snd $ untilJust $ do
        selection <- getLine
        let res = find (matches selection . fst) opts
        when (isNothing res) $ putStrLn $ "Unknown option: " ++ selection
        return res

newtype Unquoted = Unquoted String

instance Show Unquoted where
  show (Unquoted s) = s

showOpt :: HasCallStack => Option -> String
showOpt Option{..} = if orFirstLetter
  then '(' : head name : ')' : tail name
  else name

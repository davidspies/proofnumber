{-# LANGUAGE RecordWildCards #-}

module LazyMax.Internal
    ( LazyMax
    , get
    , lazyMax
    , pure
    , (?<), (?>), (?<=), (?>=)
    , (<?), (>?), (<=?), (>=?)
    ) where

import Data.Function.Pointless ((.:))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Prelude hiding (pure)
import System.IO.Unsafe (unsafePerformIO)

data LazyMaxI a = Finding{best :: a, remaining :: [a]} | Found a
newtype LazyMax a = LazyMax (IORef (LazyMaxI a))

lazyMax :: Ord a => [a] -> LazyMax a
lazyMax = \case
  [] -> error "lazyMax of empty list"
  (best : remaining) -> LazyMax $ unsafePerformIO $ newIORef Finding{..}

pure :: a -> LazyMax a
pure = LazyMax . unsafePerformIO . newIORef . Found

get :: Ord a => LazyMax a -> a
get (LazyMax r) = unsafePerformIO $ readIORef r >>= \case
  Finding{best, remaining} -> do
    let newbest = maximum (best : remaining)
    writeIORef r (Found newbest)
    return newbest
  Found x -> return x

takeStep :: Ord a => LazyMax a -> IO ()
takeStep (LazyMax xref) = modifyIORef xref $ \case
  x@Found{} -> x
  Finding{best, remaining} -> case dropWhile (<= best) remaining of
    []              -> Found best
    res : remainder -> Finding{best=res, remaining=remainder}

cmpHelper :: Ord a => (a -> a -> b) -> (b -> Bool) -> LazyMax a -> a -> IO b
cmpHelper f stopCond l@(LazyMax lr) r = go
  where
    go = readIORef lr >>= \case
      Found x -> return $ f x r
      Finding{best} ->
        let res = f best r in
        if stopCond res then return res else takeStep l >> go

(?<*), (?>*), (?<=*), (?>=*) :: Ord a => LazyMax a -> a -> IO Bool
(?<*) = cmpHelper (<) not
(?>*) = cmpHelper (>) id
(?<=*) l r = not <$> l ?>* r
(?>=*) l r = not <$> l ?<* r

(?<), (?>), (?<=), (?>=) :: Ord a => LazyMax a -> a -> Bool
(?<) = unsafePerformIO .: (?<*)
(?>) = unsafePerformIO .: (?>*)
(?<=) = unsafePerformIO .: (?<=*)
(?>=) = unsafePerformIO .: (?>=*)

(*<?), (*>?), (*<=?), (*>=?) :: Ord a => a -> LazyMax a -> IO Bool
(*<?) = flip (?>*)
(*>?) = flip (?<*)
(*<=?) = flip (?>=*)
(*>=?) = flip (?<=*)

(<?), (>?), (<=?), (>=?) :: Ord a => a -> LazyMax a -> Bool
(<?) = unsafePerformIO .: (*<?)
(>?) = unsafePerformIO .: (*>?)
(<=?) = unsafePerformIO .: (*<=?)
(>=?) = unsafePerformIO .: (*>=?)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

compareL :: Ord a => LazyMax a -> a -> IO Ordering
compareL l r =
  l ?<* r >>= \case
    True -> return LT
    False -> (l ?>* r) <&> \case
        True -> GT
        False -> EQ

negateOrdering :: Ordering -> Ordering
negateOrdering = \case
  LT -> GT
  EQ -> EQ
  GT -> LT

compareR :: Ord a => a -> LazyMax a -> IO Ordering
compareR = fmap negateOrdering .: flip compareL

ordHelper :: Ord a
  => (a -> a -> b) -> (LazyMax a -> a -> IO b) -> (a -> LazyMax a -> IO b)
  -> LazyMax a -> LazyMax a -> b
ordHelper f g h l@(LazyMax lr) r@(LazyMax rr) = unsafePerformIO go
  where
    go = (,) <$> readIORef lr <*> readIORef rr >>= \case
      (Found x, Found y) -> return $ f x y
      (Finding{}, Found y) -> g l y
      (Found x, Finding{}) -> h x r
      (Finding{best=lbest}, Finding{best=rbest}) ->
        takeStep (if rbest < lbest then r else l) >> go

instance Ord a => Eq (LazyMax a) where
  (==) x y = get x == get y
instance Ord a => Ord (LazyMax a) where
  compare = ordHelper compare compareL compareR
  (<) = ordHelper (<) (?<*) (*<?)
  (>) = ordHelper (>) (?>*) (*>?)
  (<=) = ordHelper (<=) (?<=*) (*<=?)
  (>=) = ordHelper (>=) (?>=*) (*>=?)

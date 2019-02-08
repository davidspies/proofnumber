module LazyMax.Internal
    ( LazyMax
    , get
    , maximum
    , pure
    ) where

import DSpies.Prelude hiding (maximum, pure)

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

data LazyMaxI a =
  Finding{best :: LazyMax a, remaining :: [LazyMax a]} | Found a
newtype LazyMax a = LazyMax (IORef (LazyMaxI a))

maximum :: (HasCallStack, Ord a) => [LazyMax a] -> LazyMax a
maximum = \case
  [] -> error "*** Exception: maximum: empty list"
  (best : remaining) -> LazyMax $ unsafePerformIO $ newIORef Finding{..}

pure :: a -> LazyMax a
pure = LazyMax . unsafePerformIO . newIORef . Found

get :: Ord a => LazyMax a -> a
get (LazyMax r) = unsafePerformIO $ readIORef r >>= \case
  Finding{best, remaining} -> do
    -- Avoids infinite recursion
    let newbest =
          get $ foldl1 (\x y -> if y > x then y else x) (best : remaining)
    writeIORef r (Found newbest)
    return newbest
  Found x -> return x

isSimple :: LazyMaxI a -> Bool
isSimple = \case
  Found{} -> True
  Finding{remaining} -> null remaining

takeStep :: Ord a => LazyMax a -> IO ()
takeStep (LazyMax xref) = do
  x <- readIORef xref
  case x of
    Found{} -> return ()
    Finding{best=best@(LazyMax bref), remaining} -> do
      let nextRemaining = dropWhile (<= best) remaining
      case nextRemaining of
        [] -> do
          takeStep best
          down <- readIORef bref
          -- Path compression
          when (isSimple down) $ writeIORef xref down
        y : ys -> writeIORef xref Finding{best=y, remaining=ys}

cmpHelper :: Ord a => (a -> a -> b) -> (b -> Bool) -> LazyMax a -> a -> IO b
cmpHelper f stopCond l0 r = go l0
  where
    go l@(LazyMax lr) = go'
      where
        go' = readIORef lr >>= \case
          Found x -> return $ f x r
          Finding{best} -> do
            res <- go best
            if stopCond res then return res else takeStep l >> go'

(?<), (?>), (?<=), (?>=) :: Ord a => LazyMax a -> a -> IO Bool
(?<) = cmpHelper (<) not
(?>) = cmpHelper (>) id
(?<=) l r = not <$> l ?> r
(?>=) l r = not <$> l ?< r

(<?), (>?), (<=?), (>=?) :: Ord a => a -> LazyMax a -> IO Bool
(<?) = flip (?>)
(>?) = flip (?<)
(<=?) = flip (?>=)
(>=?) = flip (?<=)

compareL :: Ord a => LazyMax a -> a -> IO Ordering
compareL l r =
  l ?< r >>= \case
    True -> return LT
    False -> (l ?> r) <&> \case
        True -> GT
        False -> EQ

negateOrdering :: Ordering -> Ordering
negateOrdering = \case
  LT -> GT
  EQ -> EQ
  GT -> LT

compareR :: Ord a => a -> LazyMax a -> IO Ordering
compareR = fmap negateOrdering .: flip compareL

data DefaultStep = StepLeft | StepRight | StepBoth

ordHelper :: Ord a
  => (a -> a -> b)
  -> (LazyMax a -> a -> IO b)
  -> (a -> LazyMax a -> IO b)
  -> DefaultStep
  -> LazyMax a -> LazyMax a -> b
ordHelper f g h defaultStep l@(LazyMax lr) r@(LazyMax rr) = unsafePerformIO go
  where
    go = (,) <$> readIORef lr <*> readIORef rr >>= \case
      (Found x, Found y) -> return $ f x y
      (Finding{}, Found y) -> g l y
      (Found x, Finding{}) -> h x r
      (Finding{best=lbest}, Finding{best=rbest}) -> do
        case defaultStep of
          StepLeft -> takeStep (if rbest < lbest then r else l)
          StepRight -> takeStep (if lbest < rbest then l else r)
          StepBoth -> case compare lbest rbest of
            LT -> takeStep l
            EQ -> takeStep l >> takeStep r
            GT -> takeStep r
        go

instance Ord a => Eq (LazyMax a) where
  (==) x y = get x == get y
instance Ord a => Ord (LazyMax a) where
  compare = ordHelper compare compareL compareR StepBoth
  (<) = ordHelper (<) (?<) (<?) StepRight
  (>) = ordHelper (>) (?>) (>?) StepLeft
  (<=) = ordHelper (<=) (?<=) (<=?) StepLeft
  (>=) = ordHelper (>=) (?>=) (>=?) StepRight
  max x y = maximum [x, y]

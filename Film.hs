{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Film (

  -- * Classes

  Film(..),
  Editable(..),
  Sampler(..),
  TimeFoldable(..),
  Shift(..),

  -- * Time Data

  TF(..),
  Q(..),
  Punctuated(..),
  punctuations,

  -- * Misc

  Time
) where

import Control.Applicative


type Time = Double

-- | Shifting time data. Used to implement @cut@.
class Shift f where
  -- | Move all data by some amount of time
  wshift  :: Time -> f a -> f a

-- | Finite-duration, generative data
--
-- prop> lifetime (next l x) = l
-- prop> l1 < l2 => next l1 x <: next l2 x
class Film a where
  -- | generate next window of the requested length
  next     :: Time -> a -> a

  -- | the length or lifetime of @a@
  lifetime :: a -> Time

  -- | Cut at interior time point, I.e. when @0 < t < lifetime x@, to get two pieces.
  cut    :: Time -> a -> Maybe (a,a)

-- | Ability to arbitrarily sequence or resequence a piece of film
--
-- prop> cut (lifetime x) (glue x y) = Just (x,y)
-- prop> glue l r = x where Just (l,r) = cut t x
-- prop> lifetime (glue x y) = lifetime x + lifetime y
-- prop> lifetime x = lifetime l + lifetime r where Just (l,r) = cut t x
class Film a => Editable a where
  -- | @glue x y@ is @x@ followed by @y@. Undoes a 'cut'.
  glue   :: a -> a -> a

-- | Simply a function of 'Time'.
newtype TF a = TF (Time -> a) deriving Functor

instance Show a => Show (TF a) where
  show (TF f) = "0 ↦ " ++ show (f 0)
  --show (TF f) = "0→" ++ show (f 0)
  --show (TF f) = "0🠾" ++ show (f 0)

instance Applicative TF where
  pure x = TF (const x)
  TF ff <*> TF xx = TF (ff <*> xx)

instance Semigroup a => Semigroup (TF a) where
  TF f <> TF x = TF (f <> x)

instance Monoid a => Monoid (TF a) where
  mempty = TF (const mempty)

instance Shift TF where
  wshift delta (TF f) = TF (\t -> f (t - delta))
--  wsplice p (TF f) (TF g) = TF h where
--    h t = if t < p then f t else g t
  
-- | A discrete sequence of values at increasing times.
newtype Q a = Q [(Time,a)] deriving (Eq,Ord,Show,Read,Functor,Foldable)

instance Semigroup a => Semigroup (Q a) where
  Q as <> Q bs = Q (merge as bs) where
    merge a@((t1,x):xs) b@((t2,y):ys) = case compare t1 t2 of
      LT -> (t1,x) : merge xs b
      EQ -> (t1, x <> y) : merge xs ys
      GT -> (t2,y) : merge a ys
    merge xs [] = xs
    merge [] ys = ys

instance Semigroup a => Monoid (Q a) where
  mempty = Q []

instance Shift Q where
  wshift delta (Q as) = Q (map (\(t, a) -> (t + delta, a)) as)
--  wsplice t (Q xs) (Q ys) = ys' `seq` Q (xs' ++ ys') where
--    xs' = takeWhile ((< t) . fst) xs
--    ys' = dropWhile ((< t) . fst) ys

-- | A value can be extracted from any time point in range.
class Sampler f where
  at :: f a -> Time -> a

instance Sampler TF where
  TF f `at` t = f t

instance Sampler (Punctuated e) where
  A x `at` t = x
  AB x p _ more `at` t = if t < p then x else more `at` (t - p)

class TimeFoldable f where
  timeFoldl :: (Time -> b -> a -> b) -> b -> f a -> b
  timeFoldr :: (Time -> a -> b -> b) -> b -> f a -> b

  timeList :: f a -> [(Time, a)]
  timeList = timeFoldr (\t x rest -> (t,x) : rest) []

  timeCount :: f a -> Int
  timeCount = timeFoldl (\_ n _ -> n + 1) 0

  timeNull :: f a -> Bool
  timeNull = timeFoldr (\_ _ _ -> False) True

  timeHead :: f a -> (Time, a)
  timeHead = timeFoldr (\t x _ -> (t,x)) (error "timeHead on headless timedata")

  timeHeadMaybe :: f a -> Maybe (Time, a)
  timeHeadMaybe = timeFoldr (\t x _ -> Just (t,x)) Nothing

  -- | Might be plus infinity
  timeMinimum :: f a -> Time
  timeMinimum = timeFoldl (\t least _ -> min t least) (1/0)

  timeMinimumMaybe :: f a -> Maybe Time
  timeMinimumMaybe chunk =
    let t = timeMinimum chunk in
    if isInfinite t && t > 0 then Nothing else Just t

instance TimeFoldable Q where
  timeFoldl visit b (Q []) = b
  timeFoldl visit b (Q ((t,x):more)) = let b' = visit t b x in b' `seq` timeFoldl visit b' (Q more)

  timeFoldr visit base (Q []) = base
  timeFoldr visit base (Q ((t,x):more)) = visit t x (timeFoldr visit base (Q more))

  timeList (Q ps) = ps

  timeNull (Q []) = True
  timeNull (Q (_:_)) = False

  timeCount (Q ps) = length ps

  timeMinimum (Q [])        = 1/0
  timeMinimum (Q ((t,_):_)) = t

instance TimeFoldable (Punctuated e) where
  timeFoldl visit base ps = go 0 base ps where
    go t b (A x) = visit t b x
    go t b (AB x p _ more) = let b' = visit t b x in b' `seq` go p b' more

  timeFoldr visit base ps = go 0 ps where
    go t (A x) = visit t x base
    go t (AB x p _ more) = visit t x (go p more)
  

-- | A stream of steady values punctuated by discrete jumps.
-- A well formed stream has a finite time between the punctuations.
data Punctuated e a
  = A a                            -- ^ The value persists forever.
  | AB a Time e (Punctuated e a) -- ^ The value persists for the given time, at which point...
      deriving (Eq,Ord,Show,Read,Functor)

punctuations :: Punctuated e a -> Q e
punctuations ps = Q (go ps) where
  go (A _) = []
  go (AB x t e more) = (t,e) : go more

instance Shift (Punctuated e) where
  wshift delta (A x) = A x
  wshift delta (AB x t y more) = AB x (t + delta) y (wshift delta more)
{-
  wsplice p before after = go before where
    go (A x) = AB x p mempty after
    go (AB x t b more) = case compare t p of
      LT -> AB x t b (go more)
      EQ -> case after of
        A y -> AB x p b (A y)
        AB _ _ b' more' -> AB x p (b <> b') more'
      GT -> AB x p mempty after
-}

instance (Semigroup e, Semigroup a) => Semigroup (Punctuated e a) where
  (<>) = liftA2 (<>)
  
instance (Semigroup e, Monoid a) => Monoid (Punctuated e a) where
  mempty = A mempty

instance Semigroup e => Applicative (Punctuated e) where
  pure x = A x
  A f <*> A x = A (f x)
  A f <*> AB x t2 b more2 = AB (f x) t2 b (A f <*> more2)
  AB f t1 b more1 <*> A x = AB (f x) t1 b (more1 <*> A x)
  AB f t1 b1 more1 <*> AB x t2 b2 more2 = case compare t1 t2 of
    LT -> AB (f x) t1 b1 (more1 <*> AB x t2 b2 more2)
    EQ -> AB (f x) t1 (b1 <> b2) (more1 <*> more2)
    GT -> AB (f x) t2 b2 (AB f t1 b1 more1 <*> more2)


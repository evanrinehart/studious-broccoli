{-# LANGUAGE DeriveFunctor #-}
module Film where


type Time = Double

-- | Containers for values distributed in time that can be shifted back or ahead.
class Splice f where
  -- | Move all data by some amount of time
  wshift  :: Time -> f a -> f a
  -- | change from one data set to another at time
  wsplice :: Time -> f a -> f a -> f a

-- | Finite-duration, generative data
--
-- prop> lifetime (next l x) = l
-- prop> l1 < l2 => next l1 x `isPrefixOf` next l2 x
class Film a where
  -- | generate next window of the requested length
  next     :: Time -> a -> a
  -- | the length or lifetime of @a@
  lifetime :: a -> Time

-- | Ability to arbitrarily sequence or resequence a piece of film
--
-- prop> cut (lifetime x) (glue x y) = Just (x,y)
-- prop> glue l r = x where Just (l,r) = cut t x
-- prop> lifetime x = lifetime l + lifetime r where Just (l,r) = cut t x
class Film a => Editable a where
  -- | @glue x y@ is x followed by y. Undoes a cut.
  glue   :: a -> a -> a
  -- | Only cuts at an interior time succeed, i.e. @0 < t < lifetime x@
  cut    :: Time -> a -> Maybe (a,a)

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

instance Splice TF where
  wshift delta (TF f) = TF (\t -> f (t - delta))
  wsplice p (TF f) (TF g) = TF h where
    h t = if t < p then f t else g t
  
newtype Q a = Q [(Time,a)] deriving (Eq,Ord,Show,Read,Functor)

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

instance Splice Q where
  wshift delta (Q as) = Q (map (\(t, a) -> (t + delta, a)) as)
  wsplice t (Q xs) (Q ys) = ys' `seq` Q (xs' ++ ys') where
    xs' = takeWhile ((< t) . fst) xs
    ys' = dropWhile ((< t) . fst) ys

class Sampler f where
  at :: f a -> Time -> a

instance Sampler TF where
  TF f `at` t = f t

instance Sampler (Punctuated b) where
  A x `at` t = x
  AB x p _ more `at` t = if t < p then x else more `at` (t - p)

data Punctuated b a = A a | AB a Time [b] (Punctuated b a) deriving (Eq,Ord,Show,Read,Functor)

instance Splice (Punctuated b) where
  wshift delta (A x) = A x
  wshift delta (AB x t y more) = AB x (t + delta) y (wshift delta more)
  wsplice p before after = go before where
    go (A x) = AB x p [] after
    go (AB x t b more) = case compare t p of
      LT -> AB x t b (go more)
      EQ -> case after of
        A y -> AB x p b (A y)
        AB _ _ b' more' -> AB x p (b++b') more'
      GT -> AB x p [] after

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Film (

  -- * Time Data

  -- | There are many possible containers for time data. These are examples.

  TF(..),
  Q(..),
  Punctuated(..),
  punctuations,
  unpunctuated,
  populate,
  remember,
  timeElems,

  Rail(..),
  Bead(..),
  pureRail,
  railNext,
  railEvents,

  -- * Classes

  Burn(..),
  Glue(..),
  Sampler(..),
  TimeFoldable(..),
  Life(..),

  -- * Scanning, sampling, filtering

  scanQ,
  scanQMaybe,
  sample,
  onlyJust,
  

  -- * Misc


  Time
) where

import Control.Applicative
import Data.Foldable
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (join)

import TimeData (Time)


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

instance Monad TF where
  TF f >>= h = TF (\t -> let TF g = h (f t) in g t)


{-
instance Shift TF where
  wshift delta (TF f) = TF (\t -> f (t - delta))
--  wsplice p (TF f) (TF g) = TF h where
--    h t = if t < p then f t else g t
-}
  
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

-- | A time varying value possibly punctuated with events.
data Rail b a = Rail (Time -> a) Time (Bead b a) deriving Functor
data Bead b a = Bead b (Rail b a) deriving Functor

-- | This behavior goes on forever and has no continuation.
pureRail :: (Time -> a) -> Rail b a
pureRail tf = Rail tf (1/0) railError

railError = error "past the end of infinity"

-- | Attempt to view the next event and continuation.
railNext :: Rail b a -> Maybe (Time, b, Rail b a)
railNext (Rail _ t k)
  | isInfinite t = Nothing
  | otherwise    = let Bead x r = k in Just (t,x,r)

-- | The time until event. May be plus infinity.
railLife :: Rail b a -> Time
railLife (Rail _ t _) = t


-- | Drop everything between events.
railEvents :: Rail b a -> Q b
railEvents = Q . go where
  go r = case railNext r of
    Just (t,x,r') -> (t,x) : go r'
    Nothing -> []

instance Sampler (Rail b) where
  Rail f l k `at` t = if t <= l
    then f t
    else let Bead _ r = k in r `at` (t - l)

instance Semigroup b => Applicative (Rail b) where
  pure x = pureRail (const x)
  Rail ff t1 k1 <*> Rail fx t2 k2 = case compare t1 t2 of
    LT -> let Bead y r = k1 in Rail (ff <*> fx) t1 (Bead y (r <*> Rail fx t2 k2))
    GT -> let Bead y r = k2 in Rail (ff <*> fx) t2 (Bead y (Rail ff t1 k1 <*> r))
    EQ -> Rail (ff <*> fx) t1 bead where
      bead | isInfinite t1 = railError
           | otherwise = let Bead y1 r1 = k1 in
                         let Bead y2 r2 = k2 in
                         Bead (y1 <> y2) (r1 <*> r2)

instance (Semigroup b, Semigroup a) => Semigroup (Rail b a) where
  (<>) = liftA2 (<>)

instance (Semigroup b, Monoid a) => Monoid (Rail b a) where
  mempty = pure mempty

instance Burn (Rail b a) where
  burn t (Rail x l k)
    | isInfinite l = Rail x l k
    | otherwise = let (Bead y r) = k in Rail x (l + t) (Bead y (burn t r))

instance Monoid b => Glue (Rail b a) where
  glue p r1 r2 = seek r1 where
    seek (Rail f t k)
      | isInfinite t = Rail f p (Bead mempty (burn (-p) r2))
      | t < p  = let Bead y r = k in Rail f t (Bead y (seek r))
--      | t == p = 
--      | t > p  = --

-- | A value can be extracted from any time point in range.
class Sampler f where
  at :: f a -> Time -> a

instance Sampler TF where
  TF f `at` t = f t

instance Sampler (Punctuated e) where
  A x `at` t = x
  AB x p _ more `at` t = if t <= p then x else more `at` (t - p)

class TimeFoldable f where
  timeFoldl :: (Time -> b -> a -> b) -> b -> f a -> b
  timeFoldr :: (Time -> a -> b -> b) -> b -> f a -> b

  timeList :: f a -> [(Time, a)]
  timeList = timeFoldr (\t x rest -> (t,x) : rest) []

  --timeScanl :: (Time -> b -> a -> b) -> b -> f a -> [b]
  --timeScanl = _

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

unpunctuated :: Punctuated e a -> TF a
unpunctuated ps = TF (\t -> ps `at` t)

-- | Fill in the blanks with a visiting generator function. It is passed the length of time
-- between event occurrences.
populate :: (Time -> b -> a -> b) -> b -> Q a -> Punctuated a b
populate gen base (Q qs) = go 0 base qs where
  go now b [] = A b
  go now b ((t,a):more) = let b' = gen (t - now) b a in AB b t a (go t b' more)

-- | Just use the latest value of the event or initial value before that.
remember :: a -> Q a -> Punctuated a a
remember = populate (\_ _ x -> x)

timeElems :: Punctuated e a -> NonEmpty (Time,a)
timeElems segs = goStart segs where
  goStart (A x) = (0,x) :| []
  goStart (AB x p _ more) = (0,x) :| go p more
  go t (A x) = [(t,x)]
  go t (AB x p _ more) = (t,x) : go p more


  

-- | Shift data backward in time and discard if it makes sense to. A.k.a. trimming.
class Burn a where
  burn :: Time -> a -> a

-- | Assuming first dataset extends out to some time, shift a second data set that far and merge.
-- 'burn' undoes 'glue'. But you can't undo a 'burn'.
class Glue a where
  glue :: Time -> a -> a -> a

instance Burn (Punctuated e a) where
  burn l start = seek start where
    seek (A x) = A x
    seek (AB x t y more)
      | l <= t    = AB x (t - l) y (shift more)
      | otherwise = seek more
    shift (A x) = A x
    shift (AB x t y more) = AB x (t - l) y (shift more)

instance Burn (TF a) where
  burn l (TF f) = TF (\t -> f (t + l))

instance Burn (Q a) where
  burn l (Q start) = Q (seek start) where
    seek [] = []
    seek ((t,x):more)
      | l <= t    = (t - l, x) : shift more
      | otherwise = seek more
    shift [] = []
    shift ((t,x):more) = (t - l, x) : shift more


instance (Burn a, Burn b) => Burn (a,b) where
  burn t (x,y) = (burn t x, burn t y)

instance Glue (TF a) where
  glue l (TF f) (TF g) = TF (\t -> if t < l then f t else g t)

instance Glue (Q a) where
  glue l (Q xs) (Q ys) = Q (go xs) where
    go [] = shift ys
    go ((t,x) : more) = if t < l then (t,x) : go more else shift ys
    shift [] = []
    shift ((t,y) : more) = (t - l, y) : shift more

instance Monoid e => Glue (Punctuated e a) where
  glue l as bs = go as where
    go (A x) = AB x l mempty (shift bs)
    go (AB x t e more) = if t < l
      then AB x t e (go more)
      else AB x l e' more' where -- specifically discard "old" e if there is a tie
        e' = spy more
        more' = shift more
    spy (A x) = mempty
    spy (AB _ t e _) = if t == l then e else mempty
    shift (A x) = A x
    shift (AB x t e more) = AB x (t - l) e (shift more)



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


-- | A finite-sized piece of a life form. It can grow or be cut into pieces.
-- next and cut may interact to always result in the same [a] sequences in some sense.
-- Since Time is a float not a real or integer, this might be tricky to pull off.
class Life a where
  lifetime :: a -> Time
  next :: Time -> a -> a
  cut :: Time -> a -> Maybe (a,a)

timeCells :: Life a => NonEmpty a -> NonEmpty (Time,a)
timeCells (x0 :| more) = (0,x0) :| go (lifetime x0) more where
  go t (x:more) = (t,x) : go (t + lifetime x) more
  go t [] = []




-- GUI toolkit
-- * widgets, or "bidirectional" input.
-- * layout, graphical "reactive" elements take up space

-- a gui input element is not just an input, but an output
-- the user looks at the element to get a view of some state
-- so obviously, if that state changes, the element changes.
-- Also, when the user engages with the element, they are changing
-- some state in a way that is consistent with behavior 1.

-- a ui element is a lens bundled with a state


-- this slider reflects the value of some Float as a picture.
-- "unrelated", it outputs Float update events. If hooked up right,
-- the users changes are reflected.
--sliderUI :: F2 () (Q UserInput) (TF Float) (TF Picture, Q Float)






-- | Transform a stream of events using a state transfer function.
scanQ :: (a -> s -> (b, s)) -> s -> Q a -> (Q b, s)
scanQ visit start (Q qs) = (Q qs', s') where
  (qs',s') = go qs start
  go [] s = ([], s)
  go ((t,x):more) s =
    let (y,s') = visit x s
        (more',s'') = go more s'
    in ((t,y):more', s'')

-- | Same as 'scanQ' but some events may be ignored.
scanQMaybe :: (a -> s -> (Maybe b, s)) -> s -> Q a -> (Q b, s)
scanQMaybe visit start (Q qs) = (Q qs', s') where
  (qs',s') = go qs start
  go [] s = ([], s)
  go ((t,x):more) s =
    let (my,s') = visit x s in
    let (more',s'') = go more s' in
    case my of
      Just y  -> ((t,y):more', s'')
      Nothing -> (more', s'')

-- | View the signal at the time of the events.
sample :: Sampler f => (a -> b -> c) -> Q a -> f b -> Q c
sample f (Q qs) sig = Q (map (\(t,x) -> (t, f x (sig `at` t))) qs)

-- | Filter out Nothing events.
onlyJust :: Q (Maybe a) -> Q a
onlyJust (Q qs) = Q (go qs) where
  go [] = []
  go ((t,Just x):more) = (t,x) : go more
  go (_:more) = go more

  
  

-- | Transform and filter a stream of events with a mealy machine.

{-
mealyQ :: (s -> a -> (s, Maybe b)) -> s -> Q a -> (Q b, s)
mealyQ visit start (Q qs) = (Q out, final) where
  out = unfoldingList unf
  final = unfoldingEnd unf
  unf = unfoldingScanMaybe g start qs
  g (t,x) s = let (s', y) = visit s x in ((t,y), s')
-}

-- | Transform and filter a stream of events with a State action.

--stateQ :: (a -> State s (Maybe b))  -> s -> Q a -> (Q b, s)
--stateQ act s (Q qs) = _

-- this thing responds to mouse button down up by emitting an event

{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
module Driver4 where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import GHC.Generics
import Data.List.NonEmpty

type Time = Double

type TF a = Time -> a



data Frag a = Frag Time a
data Broken a = Broken Time (Stream (Frag (V a))) (NonEmpty (Frag (V a)))
data Stream a = a :> Stream a

-- By fragmenting time in some way, we can represent a function of time as stream of chunks
-- one going forward in time, another going back. The chunks contain a payload of type V a.
-- The V a are offset in such a way that the original function can be restored.
shatter :: Stream Time -> [Time] -> (Time -> a) -> Broken a
shatter (base :> pastps) futureps f = Broken base (goback base pastps) (goahead base futureps) where
  goahead t [] = Frag (1/0) (V (t - base) f) :| []
  goahead t (t':more) = Frag (t' - t) (V (t - base) f) `cons` goahead t' more
  goback t' (t :> less) = Frag (t' - t) (V (t - base) f) :> goback t less

-- | Any broken-up time function can be re-assembled by sampling the V chunks.
-- i.e. Time heals Broken varying values. 
heal :: Broken a -> Time -> a
heal (Broken base past future) t = if t >= base then goahead base future else goback base past where
  goahead b frags = case uncons frags of
    (Frag size (V off f), Nothing)   -> f (t + off)
    (Frag size (V off f), Just more)
      | t < b + size -> f (t - b + off)
      | otherwise    -> goahead (b + size) more
  goback b' (Frag size (V off f) :> less)
    | t >= b' - size = let b = b' - size in f ((t - b) + off)
    | otherwise      = let b = b' - size in goback b less

-- heal . shatter ts1 ts2 = id




data TS a = TS a Time (TS a)

-- absolute time stream has a first chunk followed by list of new chunks with start times
data Abs a = Abs a [(Time,a)] deriving Show

-- time stream TS3 has sized chunks, the first chunk is infinite so comes with an end time
data Rel a = Rel a Time [(a,Time)] deriving Show

absToRel :: Abs a -> Rel a
absToRel (Abs x []) = Rel x (1/0) []
absToRel (Abs x ((start,y):ys)) = Rel x start (go y ys) where
  go x [] = [(x, 1/0)]
  go x ((t,y):ys) = (x, t) : go y ys

relToAbs :: Rel a -> Abs a
relToAbs (Rel x start xs) = Abs x (go start xs) where
  go t [] = []
  go t ((x,size):more) = (t, x) : go (t + size) more

-- A Time -> a can trivially be promoted to Abs (V a)
embedB :: (Time -> a) -> Abs (V a)
embedB f = Abs (V 0 f) []

sampleV :: V a -> Time -> a
sampleV (V off f) t = f (t + off)



-- I want to show that you can convert any Time -> a into a time stream of chunks.
-- there are many ways to do it. But for any fragmentation, sampling (in some sense) the
-- chunk stream returns the original Time -> a.

-- A causal functions from Time -> a to Time -> b, means that the value of g :: Time -> b
-- at time t can only depend on f :: Time -> a at times less than t.

-- A stream transducer from Abs (V a) to Abs (V b) corresponds to a causal function from
-- Time -> a to Time -> b if... an output chunk only depends on input chunks that came
-- before it in time, OR if it depends on a contemporary chunk, the out chunk contents
-- only depends on the contemporary chunks contents in a causal way.



-- type for a form of time stream transducer. Given the payload of the next input chunk
-- but not the size of that chunk, produce the rest of the out stream assuming there will
-- be no new information.

newtype Nom i o = Nom (i -> Yexen i o)
  deriving Functor

--data Song i o = Song -- simpler, seeking/cancelling a long way potentially expensive
--  { chunksOut :: NonEmpty (o, Time)
--  , cancelOut :: Time -> Juke i o } deriving Functor

data Yex i o = Yex -- every out chunk contains a resuming callback
  { yexOut  :: o
  , yexSize :: Time -- when I want to resume, size < inf = active component
  , yexStop :: Time -> Nom i o {- when someone else wants me to resume early -} } deriving Functor

-- | Parallel composition. Though this instance doesn't actually run components in parallel.
instance Chunk i => Applicative (Nom i) where
  pure x = let go = Nom (\_ -> Yex x (1/0) (const go) :| []) in go
  Nom ff <*> Nom xx = Nom (\i -> ff i <*> xx i)

newtype Yexen i o = Yexen (NonEmpty (Yex i o)) deriving Functor

(k1 <*><*> k2) dt = k1 dt <*> k2 dt

-- truncate the longer one in a brute force attempt to merge two Yexes
-- we're looking for a better way to zip two yex streams
minYex :: Yex i a -> Yex i b -> Yex i (a,b)
minYex (Yex x l1 k1) (Yex y l2 k2) = Yex (x,y) (min l1 l2) (\dt -> k1 dt <*> k2 dt)

-- move a yex toward its end (don't try to go further)
burnYex :: Chunk o => Time -> Yex i o -> Yex i o
burnYex delta (Yex o l k) = Yex (burn delta o) (l - delta) (k . (`subtract` delta))

-- this time, the longer one is 'burned' to begin the next minyex
-- and the cancellations k must be shifted since they no longer "start" at the same time
zipYex :: (Chunk a, Chunk b) => (a -> b -> c) -> NonEmpty i a -> NonEmpty i b -> NonEmpty i c

zipYex p (Yex x l1 k1 :| []) (Yex y l2 k2 :| []) = Yex (p x y) (1/0) (k1 <*><*> k2) :| []

zipYex p (Yex x l1 k1 :| (m1:mm1)) (yy@(Yex y _ k2) :| []) =
  Yex (p x y) l1 (k1 <*><*> k2) `cons` zipYex (m1 :| mm1) (burnYex l1 yy)

zipYex p (yx@(Yex x _ k1 :| []) (Yex y l2 k2 :| (m2:mm2)) =
  Yex (p x y) l2 (k1 <*><*> k2) `cons` zipYex (burnYex l2 yx) (m2 :| mm1)

zipYex p (yx@(Yex x l1 k1) :| more1@(m1:mm1)) (yy@(Yex y l2 k2) :| more2@(m2:mm2)) =
  case compare l1 l2 of
    LT -> Yex (p x y) l1 (k1 <*><*> k2) `cons` zipYex p (m1 :| mm1) (burnYex l1 yy :| more2)
    GT -> Yex (p x y) l2 (k1 <*><*> k2) `cons` zipYex p (burnYex l2 yx :| more1) (m2 :| mm2) 
    EQ -> Yex (p x y) (min l1 l2) (k1 <*><*> k2) `cons` zipYex p (m1 :| mm1) (m2 :| mm2)

zipNom :: (Chunk a, Chunk b) => (a -> b -> c) -> Nom i a -> Nom i b -> Nom i c
zipNom p (Nom f) (Nom g) = Nom h where
  h i = Yexen (zipYex p ys zs) where
    Yexen ys = f i
    Yexen zs = g i

possible :: Nom i (V (a -> b)) -> Nom i (V a) -> Nom i (V b)
possible :: Nom i (K (a -> b), V a) -> Nom i (V b)
possible :: Nom i (K (a -> b), K a) -> Nom i (K b)
possible :: Nom i (V (a -> b), E a) -> Nom i (E b)
possible :: Nom i (E (a -> b), V a) -> Nom i (E b)
possible :: Nom i (K (a -> b), E a) -> Nom i (E b)
possible :: Nom i (E (a -> b), K a) -> Nom i (E b)

--apply :: (Burn a, Burn b) => Nom i (Nom a b) -> Nom i a -> Nom i b

-- instance Burn (E a)
-- instance Burn (V a)
-- instance Burn (K a)
-- instance (Burn i, Burn o) => Burn (Nom i o)...? is Nom an exponential object



-- to do Nom i (a -> b) -> Nom i a -> Nom i b



{-
-- the Applicative instance can't weave yexen as efficiently because it can't
-- assume the output chunk is an instance of Chunk
instance Applicative (Yexen i) where
  --pure x = let go = Yex x (1/0) (const (const go)) in go

  -- first the easy case
  Yexen (yf :| []) <*> Yexen (yx :| []) = Yexen (z :| []) where
    Yex f _ k1 = yf
    Yex x _ k2 = yx
    z = Yex (f x) (1/0) (\dt -> k1 dt <*> k2 dt)

  -- the two intermediate cases
  Yexen (yf :| []) <*> Yexen (yx :| yyx) =
  Yexen (yf :| yyf) <*> Yexen (yx :| []) =

  -- the general case
  Yexen (yf :| (yyf:moref)) <*> Yexen (yx :| (yyx:morex)) = Yexen (z `cons` zs) where
    Yex f size1 yfk = yf
    Yex x size2 yxk = yx
    z = Yex (f x) (min size1 size2) (\dt -> k1 dt <*> d2 dt)
    zs = case compare size1 size2 of
      LT -> yyf <*> yxk size1 
      GT -> yfk size2 <*> yyx
      EQ -> yyf <*> yyx

(<%>) :: (Chunk a, Chunk b) => Nom i (a -> b) -> Nom i a -> Nom i b
-}
      
  






transduce :: Chunk i => Nom i o -> TS i -> TS o
transduce start input = go start input where
  go (Nom eat) (TS i isize rest) =
    let Yex o usize k = eat i in
    case compare usize isize of
      -- program acts before next input
      LT -> let i' = burn usize i
                d = usize
                smaller = TS i' (isize - d) rest
            in TS o d (go (k d) smaller)
      -- GT and EQ seem the same
      GT -> let d = isize
            in TS o d (go (k d) rest)
      EQ -> let d = min isize usize -- either since they're equal
            in TS o d (go (k d) rest)

-- codata TS a = TS a Time (TS a) -- considered as a fragmentation of time into chunks
-- each chunk containing some value a. The first chunk extends from minus infinity to
-- the given time, which may be infinite, in which case there is only 1 chunk.
-- any TS (V a) can be viewed as a function of time
{-
fromStream :: TS (V a) -> Time -> a
fromStream ts t = fromStream2 (shiftStream ts) t

fromStream2 :: TS2 (V a) -> Time -> a
fromStream2 (TS2 f0 rest) t = go f0 rest where
  go (V off f) [] = f (t + off)
  go (V off f) ((limit,g):more) = if t < limit then f (t + off) else go g more

fromStream3 :: TS3 (V a) -> Time -> a
fromStream3 chunks t = fromStream2 (layoutStream3 chunks) t
-}


-- | Lift a function on chunks to a stateless transducer.
instance Arrow Nom where
  arr f = let go = Nom (\i -> Yex (f i) (1/0) (const go)) in go
  Nom f *** Nom g = Nom $ \(i1,i2) ->
    let Yex a1 b1 c1 = f i1
        Yex a2 b2 c2 = g i2
    in Yex (a1,a2) (min b1 b2) (\dt -> c1 dt *** c2 dt)

-- | Serial composition.
instance Category Nom where
  id = let go = Nom (\i -> Yex i (1/0) (const go) :| []) in go
  -- strategy: we only generate 1 yex, the minyex from running f on (g on i)
  -- this will force the host/driver to more input to the entire system at that time.
  -- potentially asking components to process "non-events" for no reason
  Nom f . Nom g = Nom h where
    h i = Yexen $ Yex a2 (min b1 b2) (\dt -> c2 dt . c1 dt) :| [] where
      Yexen (Yex a1 b1 c1 :| more1) = g i
      Yexen (Yex a2 b2 c2 :| more2) = f a1

-- better serial composition. Process yexen stream from g i using f.
-- When f yex wins, burn output of g to continue. When g output wins, truncate
-- current f yex and start over.
(<.<) :: Burn b => Nom b c -> Nom a b -> Nom a c
Nom f <.< Nom g = Nom h where
  h i = let Yexen (cc@(Yex c _ _ :| )) = g i
            Yexen oo = f c
        in Yexen (pipeYexen cc oo)

pipeYexen :: Burn b => NonEmpty (Yex b c) -> NonEmpty (Yex a b) -> NonEmpty (Yex a c)
pipeYexen (cc@(Yex c size1 k1) :| more1) (Yex o size2 k2 :| more2) =
  -- we know what the output is, most of this function is determining the proper `more3'
  let output s = Yex o s (\dt -> k2 dt <.< k1 dt) in
  case size1 size2 of
  LT ->
    -- z yex ends first (not infinite). more2 is invalid and cancelled at time size1
    -- use continuation k2 to get a new Nom, using it on the next z to get next yexen pair
    -- if there is no next z... we should stop.
    case more1 of
      [] -> output size1 :| []
      cc' ->
        let Nom f' = k2 size1
            Yexen oo' = f' (yexOut cc')
        in output size1 `cons` pipeYexen cc' oo'
  EQ ->
    -- initial yex end at the same time, more2 is invalid and cancelled
    -- use continuation k2 to get a new Nom, using it on next z to get next yexen pair
    -- if there is no next z... we should stop. E.g. size = infinity.
    let size = min size1 size2 in
    case more1 of
      []  -> output size :| []
      cc' ->
        let Nom f' = k2 size
            Yexen oo' = f' (yexOut cc')
        in output size `cons` pipeYexen cc' oo'
  GT ->
    -- o yex ends first (not infinite). idea here is that chunk was already considered.
    -- continue this loop with z yex burned so it's caught up (less size, shifted k, ?)
    -- do we need to shift the chunk payload if we don't look at it?
    case more2 of
      []  -> output size2 :| []
      oo' ->
        let shortC = burnYex size2 cc
        in output size2 `cons` pipeYexen (shortC :| more1) oo'
      

-- | Feedback composition. The current output chunk is fed back as part of the input.
feedback :: Nom (i, o) o -> Nom i o
feedback (Nom f) = Nom $ \i -> let Yex o d k = f (i,o) in Yex o d (feedback . k)

-- | Dynamic subtransducer composition. 
host :: Semigroup o => Nom i o -> Nom (E (Nom i o), i) o
host (Nom oldGuest) = Nom $ \(E guestMay, i) -> case guestMay of
  Nothing -> let Yex o size k = oldGuest i in Yex o size (host . k)
  Just (Nom newGuest) ->
    let Yex a _ _    = oldGuest i -- old guest gets a last hurrah?
        Yex b size k = newGuest i
    in Yex (a <> b) size (host . k)
  
-- once a chunk is output, input has no effect until output chunk runs out
doNotDisturb :: Chunk o => Nom i o -> Nom i o
doNotDisturb (Nom guest) = Nom openMode where
  openMode i = let Yex o l k = guest i in Yex o l (closedMode 0 o l k)
  closedMode log o l k dt =
    if dt < l
      then let o' = burn dt o
               l' = l - dt
               log' = log + dt
           in Nom (\_ -> Yex o' l' (closedMode log' o' l' k))
      else doNotDisturb $ k (log + dt)
              


  

class Chunk a where
  subdiv :: Time -> a -> (a,a)
  burn :: Time -> a -> a
  varying :: a -> Bool
  default subdiv :: (Generic a, Chunk' (Rep a)) => Time -> a -> (a,a)
  subdiv t x = let (l,r) = subdiv' t (from x) in (to l, to r)
  burn t x = let (_,r) = subdiv t x in r

class Chunk' f where
  subdiv' :: Time -> f p -> (f p, f p)
  varying' :: f p -> Bool

instance (Chunk' f) => Chunk' (M1 i t f) where
  subdiv' t (M1 x) = let (a,b) = subdiv' t x in (M1 a, M1 b)
  varying' (M1 x) = varying' x

instance (Chunk c) => Chunk' (K1 i c) where
  subdiv' t (K1 x) = let (a,b) = subdiv t x in (K1 a, K1 b)
  varying' (K1 x) = varying x

instance (Chunk' f, Chunk' g) => Chunk' (f :*: g) where
  varying' (x :*: y) = varying' x || varying' y
  subdiv' t xy@(x :*: y) = if varying' xy
    then 
      let (x1,x2) = subdiv' t x
          (y1,y2) = subdiv' t y
      in (x1 :*: y1, x2 :*: y2)
    else
      (xy, xy)

newtype E a = E (Maybe a) deriving (Show)
data V a = V Time (Time -> a)
newtype K a = K a deriving (Show)

sampleAtBase :: V a -> a
sampleAtBase (V base f) = f base

instance Show (V a) where
  showsPrec d (V base _) = showParen (d > 10)
    ( showString "V "
    . shows base
    . showString " 📈")

instance Chunk (E a) where
  subdiv _ (E Nothing)  = (E Nothing, E Nothing)
  subdiv _ (E (Just x)) = (E (Just x), E Nothing)
  varying Nothing  = False
  varying (Just _) = False

instance Chunk (V a) where
  subdiv d (V base f) = (V base f, V (base + d) f)
  varying _ = True

instance Chunk (K a) where
  subdiv _ x = (x,x)
  varying _ = False

--instance Chunk a => Chunk (a -> b) where
-- f subdiv t f = (f, \x -> let (_,ar) = subdiv t x = f ar)

instance Chunk Nest
instance (Chunk a, Chunk b) => Chunk (a,b)

data Nest = Nest
  { n1 :: E Char
  , n2 :: V Time
  , n3 :: K Int }
      deriving (Generic,Show)

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Device (
  -- * Device
  Device,
  look,
  write,
  WriteMe,

  -- * Plan
  --
  -- | A 'Plan' defines a 'Device'\'s behavior and initial state.

  Plan,
  newCache,
  newMailbox,
  runPlan,
  Spy,

  -- * Device Sandwich

  Sandwich(..),
  readSandwich,
  writeSandwich,

  -- * Debugging

{-
  Varying(..),
  Occurring(..),
  Q(..),
  TF(..),
  semanticValue1,
  semanticValue2,
-}

) where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.IntMap as IM
import Data.Proxy
import Prelude hiding (splitAt)
import Text.Printf
import Data.Foldable
import Numeric

import Data.Some

import qualified Mailboxes as Mail
import Mailboxes (noMail, MailPile, Mailbox(..), Mailboxes, MailKeyGen(..))

import Film

import Debug.Trace

-- public API

-- | A time varying, possibly reactive, purely functional object.
--
-- Each 'Device' value has a fixed span of time over which it may be viewed or edited. See 'lifetime'.
--
-- Views (and edits) work with whole chunks of time
-- at once, by way of container types e.g. @f@ and @g@ below.
--
-- The @z@ parameter protects devices with incompatible internal structure from being mixed.
-- 
-- Use 'look' to view the device. It takes a 'Spy' which defines how you want the data. The
-- value returned from 'look' represents a value throughout the 'Device' time span. You can then
-- sample it or take an average over the window, for example, to get concrete data.
--
-- Use 'write' to influence the behavior of the device. It requires a 'WriteMe', which is
-- a reference to a mailbox (see 'newMailbox'). Mailbox values are also extended in time, and
-- will be merged with the existing mail through the 'Monoid' instance for your chosen container.
-- See the 'Q' type for an example.
--
-- Use 'next' to get a future time window of any device.
--
-- Using 'cut' and 'glue' we can experiment with alternative histories in the same span of time.
data Device z = DGuts (Guts z) | DGlue (Device z) (Device z)

data Guts z = Guts
  { gutsOld    :: MeatBoneCache
  , gutsNew    :: MeatBoneCache
  , gutsMail   :: MailPile
  , gutsSize   :: Time
  , gutsDNA    :: DNA
  , gutsBoxes  :: Mailboxes
  , gutsDebug  :: CacheView
  }

type Plan z a = State (PlanState z) a

-- | A mailbox reference. See 'newMailbox'.
newtype WriteMe z f m = WriteMe (Device z -> f m -> Device z)

-- | A method of computing views of a device.
--
-- see 'look', 'newCache', 'newMailbox'
newtype Spy z f a = Spy (MeatBoneCache -> MailPile -> f a)

-- | Continue to the next time window.
nextDevice :: Time -- ^ How long the next time window will be.
     -> Device z
     -> Device z
nextDevice newDelta d
  | newDelta < 0    = error ("next: next window size negative (" ++ show newDelta ++ ")")
  | otherwise = DGuts (shiftNoMail newDelta (finalGuts d))

finalGuts :: Device z -> Guts z
finalGuts (DGuts d)     = d
finalGuts (DGlue _ b2) = finalGuts b2

-- | Observe a 'Device' some way.
-- Get some interesting 'Spy's from 'newMailbox' or 'newCache'. Combine them with 'Functor'
-- 'Applicative' and 'Monoid'.
look :: Splice f => Spy z f a -> Device z -> f a
look (Spy f) (DGuts d) = f (gutsNew d) (gutsMail d)
look spy (DGlue b1 b2) =
  let a = look spy b1
      b = look spy b2
      t = lifetime b1
  in wsplice t a (wshift t b)

-- | Merge a window of data into this mailbox.
write :: (Splice f, Monoid (f m)) => WriteMe z f m -> Device z -> f m -> Device z
write (WriteMe merge) d msg = merge d msg

-- | Every device has a finite lifetime. This is that.
deviceSize :: Device z -> Time
deviceSize (DGuts d) = gutsSize d
deviceSize (DGlue b1 b2) = deviceSize b1 + deviceSize b2

-- | The inverse of 'glue'. Succeeds only if cut time is greater than zero and less than 'size'.
cutDevice :: Time -> Device z -> Maybe (Device z, Device z)
cutDevice t dev = case innerCut t dev of
  Cutted l r -> Just (l,r)
  _          -> Nothing

innerCut :: Time -> Device z -> Cutting z
innerCut t (DGlue b1 b2) = case innerCut t b1 of
  OffLeft -> OffLeft
  Cutted l r -> Cutted (DGlue l r) b2
  OffRight -> case let delta = lifetime b1 in innerCut (t - delta) b2 of
    OffLeft -> Cutted b1 b2
    Cutted l r -> Cutted b1 (DGlue l r)
    OffRight -> OffRight
innerCut t (DGuts d)
  | t > 0 && t < gutsSize d = let (l,r) = cutGuts t d in Cutted (DGuts l) (DGuts r)
  | t <= 0 = OffLeft
  | t >= gutsSize d = OffRight

-- t should be in interior of guts lifetime
cutGuts :: Time -> Guts z -> (Guts z, Guts z)
cutGuts t (Guts old _ mail delta dna boxes debug) =
  let part1 = guts old mail t dna boxes debug
      part2 = shiftKeepMail (delta - t) part1
  in (part1, part2)

data Cutting z = OffLeft | OffRight | Cutted (Device z) (Device z)

-- | Sequence two devices in time to get a longer-lasting device. Can be undone with 'cut'.
glueDevice :: Device z -> Device z -> Device z
glueDevice = DGlue

-- implementation notes... *HERE*
-- operations to be preserved on a device
--   look  :: Spy z a -> Device z -> a                   (function of mail and meat)
--   write :: WriteMe z m -> Device z -> m -> Device z   (mappend to mailbox, but if device is split?)
--   cut   :: Time -> Device z -> (Device z, Device z)   (forget and shift some mail)
--   cat   :: Device z -> Device z -> Device z           (group devices, defer operations?)
--   next  :: Time -> Device z -> Device z               (new bones now old, regenerate meat)
--   size  :: Device z -> Time                           (sum of component sizes)

-- we can do all this if every 'windowable' type supports
-- |

-- if the window boundaries are "understood" then wcut is really a shift
-- wglue is a shift and a replacement prior to some time
-- either way, the data can be shifted


-- |Add a cached state to this device. It can be used as memory or an integrator.
--
-- The generator function takes the state value as of the beginning of the 'Device' lifetime
-- /(old bone)/. It should return the final state at the end of that time /(new bone)/.
-- The observable output @g b@, known as the /meat/ represents anything that happens in between.
-- Note that bones pertain to points in time, unlike meat which extends over finite time.
--
-- Note the generator gets size and input from the current time window, not previous.
--
-- New bones are carried into the future via 'next' then evaluated to WHNF, becoming old in the process.
-- They can be used to speed up behaviors that depend on accumulated history.
newCache :: (Time -> s -> f a -> (g b,s)) -- ^How to compute output window from input window, state, and new window size
         -> s -- ^Initial seed state.
         -> Spy z f a -- ^Input window
         -> Plan z (Spy z g b)
newCache f base (Spy input) = do
  k <- newBase Proxy base
  addRegen (\dt old new mail -> wrap2 (f dt (snd (old ! k)) (input new mail)))
  addCacheDebug (Some $ CacheDebug "unnamed" (const "?") (const "?"))
  return $ Spy (\cache boxes -> fst (cache ! k))

-- | Same as 'newCache' but takes a label and 'Show' instances for a better 'debug' report.
newCacheDebug :: forall z s f a g b . (Show (g b), Show s)
   => String
   -> (Time -> s -> f a -> (g b,s)) -- ^How to compute output window from input window, state, and new window size
   -> s -- ^Initial seed state.
   -> Spy z f a -- ^Input window
   -> Plan z (Spy z g b)
newCacheDebug l f base (Spy input) = do
  k <- newBase Proxy base
  addRegen (\dt old new mail -> wrap2 (f dt (snd (old ! k)) (input new mail)))
  addCacheDebug (Some $ CacheDebug l (show :: g b -> String) (show :: s -> String))
  return $ Spy (\cache boxes -> fst (cache ! k))

-- |Add a new mailbox to this device. A mailbox's value spans the same time window
-- as the device. The 'Monoid' instance should merge two equally sized windows of mail.
--
-- The returned 'Spy' can be used to observe the mailbox.
--
-- Once the device is constructed, 'write' can be used to update the mailbox.
--
-- Mail data that lies outside the device's time window has no effect.
newMailbox ::
  forall z f m . (Splice f, Monoid (f m)) => Plan z (Spy z f m, WriteMe z f m)
newMailbox = do
  g <- gets psGen2
  let (k, g') = Mail.genKey g
  let box = Mail.box "unnamed" (const "?") wshift mempty mappend k
  let someBox = Mail.some box
  modify (\ps -> ps { psGen2 = g', psMailboxes = psMailboxes ps ++ [someBox] })
  let sig = Spy (\cache pile -> Mail.getMail k mempty pile)
  return (sig, WriteMe (deliver box))


-- | Same as 'newMailbox' but takes a label and a Show instance. This is used by the 'debug' report.
newMailboxDebug ::
  forall z f m . (Splice f, Monoid (f m), Show (f m)) => String -> Plan z (Spy z f m, WriteMe z f m)
newMailboxDebug l = do
  g <- gets psGen2
  let (k, g') = Mail.genKey g
  let box = Mail.box l show wshift mempty mappend k
  let someBox = Mail.some box
  modify (\ps -> ps { psGen2 = g', psMailboxes = psMailboxes ps ++ [someBox] })
  let sig = Spy (\cache pile -> Mail.getMail k mempty pile)
  return (sig, WriteMe (deliver box))

-- |Create a 'Device' according to the plan. Uses rank 2 polymorphism to isolate
-- references from the wrong devices.
--
-- You can either use your 'Device' entirely within the callback, or return it as
-- part of the @b@ in a way that hides the @z@. E.g. Wrap it in a record of accessor functions.
--
-- This formulation stops mixing devices with incompatible structure. But it also
-- stops composition of compatible devices that come from two places. Would encoding
-- the entire structure in the type parameter work out nicely?
runPlan :: Time -- ^ The size of the first window
        -> (forall z . Plan z (Device z -> b)) -- ^ Plan should result in a callback to receive and use the new Device
        -> b
runPlan initialDelta plan =
  let g1 = KeyGen 0
      g2 = MailKeyGen 0
      (finalize, goods) = runState plan (PlanState g1 g2 [] [] [] [])
      PlanState _ _ bases regens boxes debug = goods
      bones = compileBases bases
      initialCache = skeleton bones
      template = V.fromList regens
      debugV = V.fromList debug
  in finalize (DGuts $ guts initialCache noMail initialDelta template boxes debugV)

data PlanState z = PlanState
  { psGen1   :: KeyGen BaseKey
  , psGen2   :: MailKeyGen
  , psBases  :: [Wrap] -- original bones
  , psRegens :: [Regen]
  , psMailboxes :: Mailboxes
  , psCacheDebugs :: [Some CacheDebug]
  }

data CacheDebug :: (*,*) -> * where
  CacheDebug :: String -> (gb -> String) -> (s -> String) -> CacheDebug '(gb, s)

type Regen = Time -> MeatBoneCache -> MeatBoneCache -> MailPile -> Wrap2
type DNA = V.Vector Regen


ex :: IO ()
ex = runPlan 1 $ do
  (spy1 :: Spy z Q (), box1) <- newMailboxDebug "box1"
  (spy2 :: Spy z Q (), box2) <- newMailbox
  out1 <- newCache (\l c y -> (y <> y, succ c)) 'c' (pure [5]) :: Plan z (Spy z TF [Double])
  out2 <- newCacheDebug "var1" (\l c y -> (y <> y <> y, succ c)) 'c' (pure []) :: Plan z (Spy z TF [Double])
  return $ \d -> do
    let (DGuts gs) = d
    putStrLn (debugGuts gs)

debugFormatStr = "%10s %10s %20s %15s %15s"
debugFormat :: String -> String -> String -> String -> String -> String
debugFormat a b c d e = printf debugFormatStr as bs cs ds es where
  as = take 10 a
  bs = take 10 b
  cs = take 20 c
  ds = take 15 d
  es = take 15 e

debugGuts :: Guts z -> String
debugGuts d =
  let pile = gutsMail d
      boxes = gutsBoxes d
      delta = gutsSize d
      debug = gutsDebug d
      Cache2 old = gutsOld d
      Cache2 new = gutsNew d
      heading = debugFormat (showFFloat (Just 6) delta "")  "type" "meat" "oldBone" "newBone"
      mailReports = map (\box -> Mail.report box (\a b -> debugFormat a "(mailbox)" b "" "") pile) boxes
      cacheReports = toList (V.zipWith debugCache debug (V.zip old new))
  in unlines (heading : mailReports ++ cacheReports)

debugCache :: Some CacheDebug -> (Wrap2, Wrap2) -> String
debugCache (Some (CacheDebug l shMeat shBones)) (Wrap2 _ oldBone, Wrap2 meat newBone) =
  let z = z
      c = shMeat (unsafeCoerce meat)
      d = shBones (unsafeCoerce oldBone)
      e = shBones (unsafeCoerce newBone)
  in debugFormat l "(cache)" c d e
  

-- move forward in time, preserving mail by splitting and forgetting
shiftKeepMail :: Time -> Guts z -> Guts z
shiftKeepMail newDelta (Guts _ nowOldBones oldMail oldDelta dna boxes debug) =
  let pile' = foldl (\pile box -> Mail.forget oldDelta box pile) oldMail boxes
  in guts nowOldBones pile' newDelta dna boxes debug

-- move forward in time, disregarding any potential mail outside original window
shiftNoMail :: Time -> Guts z -> Guts z
shiftNoMail newDelta (Guts _ nowOldBones _ _ dna boxes debug) =
  guts nowOldBones noMail newDelta dna boxes debug

deliverGuts :: Mailbox (f m) -> f m -> Guts z -> Guts z
deliverGuts box msg d = d { gutsMail = Mail.insert box msg (gutsMail d) }

{-
-- Put m in device mailbox then regenerate meatbone cache from old bones.
-- Current meat, new bones are discarded.
editGutsMail :: Monoid (f m) => Key MailKey (f m) -> Guts z -> f m -> Guts z
editGutsMail k (Guts old _ mail delta dna forgetter debugger) x =
  let mail' = stuffMailbox k x mail
  in guts old mail' delta dna forgetter debugger
-}

deliver :: (Splice f, Monoid (f m)) => Mailbox (f m) -> Device z -> f m -> Device z
deliver box (DGuts someGuts) x = DGuts (deliverGuts box x someGuts)
deliver box (DGlue b1 b2) x = 
  let (l,r) = wcut (lifetime b1) x
      c1 = deliver box b1 l
      c2 = deliver box b2 r
  in DGlue c1 c2

wcut :: Splice f => Time -> f a -> (f a, f a)
wcut t x = (x, wshift (-t) x)

data KeyGen ph = KeyGen Int
genKey :: KeyGen ph -> a -> (Key ph a, Wrap, KeyGen ph)
genKey (KeyGen n) x = (Key n, Wrap x, KeyGen (n+1))

genBiKey :: KeyGen ph -> Proxy b -> s -> (BiKey b s, Wrap, KeyGen ph)
genBiKey (KeyGen n) _ x = (BiKey n, Wrap x, KeyGen (n+1))

newBase :: Proxy b -> s -> Plan z (BiKey b s)
newBase prox x = state $ \ps ->
  let g = psGen1 ps
      bs = psBases ps
      (k,w,g') = genBiKey g prox x
  in (k, ps { psGen1 = g', psBases = bs ++ [w]})
    
addRegen :: Regen -> Plan z ()
addRegen regen = modify (\ps -> ps { psRegens = psRegens ps ++ [regen] })

addCacheDebug :: Some CacheDebug -> Plan z ()
addCacheDebug x = modify (\ps -> ps { psCacheDebugs = psCacheDebugs ps ++ [x] })


instance Functor f => Functor (Spy z f) where
  fmap f (Spy g) = Spy (\cache boxes -> fmap f (g cache boxes))

instance Applicative f => Applicative (Spy z f) where
  pure x = Spy (\_ _ -> pure x)
  (Spy ff) <*> (Spy xx) = Spy (liftA2 (liftA2 (<*>)) ff xx)

instance (Applicative f, Semigroup m) => Semigroup (Spy z f m) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid m) => Monoid (Spy z f m) where
  mempty = pure mempty



-- unsafe, construct a device from regen rules (dna), old bones, and mail
-- the result contains new lazy meatbone cache.
-- When device is inspected, all old bones will be evaluated to avoid leaking space.
guts :: MeatBoneCache -> MailPile -> Time -> DNA -> Mailboxes -> CacheView -> Guts z
guts old mail delta dna boxes debug = 
  let new = Cache2 (V.map (\f -> f delta old new mail) dna)
  in evaluateBones old `seq` Guts old new mail delta dna boxes debug

--regenerate :: Bones -> DNA -> (Meat, Bones)

wrap2 :: (a,b) -> Wrap2
wrap2 (x,y) = Wrap2 x y

-- gross

type CacheView = Vector (Some CacheDebug)
type MeatBoneCache = Cache2 MeatBone
data Wrap = forall a . Wrap a
data Wrap2 = forall a b . Wrap2 a b
newtype Cache ph = Cache (V.Vector Wrap)
newtype Cache2 ph = Cache2 (V.Vector Wrap2)
newtype Key ph a = Key Int deriving Show
newtype BiKey a b = BiKey Int deriving Show
data BaseKey
data MeatBone
data Debug


(!) :: MeatBoneCache -> BiKey meat bone -> (meat, bone)
Cache2 v ! BiKey i = unsafeUnwrap (v V.! i) where
  unsafeUnwrap (Wrap2 x y) = (unsafeCoerce x, unsafeCoerce y)

compileBases :: [Wrap] -> Cache BaseKey
compileBases xs = Cache (V.fromList xs)

skeleton :: Cache BaseKey -> MeatBoneCache
skeleton (Cache bases) = Cache2 (V.map (bombPromote "no initial meat, don't look") bases)

bombPromote :: String -> Wrap -> Wrap2
bombPromote msg (Wrap x) = Wrap2 (error msg) x

evaluateBones :: MeatBoneCache -> ()
evaluateBones (Cache2 v) = foldl f () v where
  f unit (Wrap2 _ y) = y `seq` unit

forgetMeat :: MeatBoneCache -> MeatBoneCache
forgetMeat (Cache2 v) = Cache2 (V.map f v) where
  f (Wrap2 _ y) = Wrap2 (error "(bug) this meat was forgotten to save memory") y




instance Film (Device z) where
  next     = nextDevice
  lifetime = deviceSize

instance Editable (Device z) where
  glue = glueDevice
  cut  = cutDevice

data Tree a = Leaf a | Fork a (Tree a) (Tree a)

data Sandwich :: Tree * -> * -> * -> * where
  Item :: (Splice f, Splice g, Monoid (f i)) => WriteMe z f i -> Spy z g o -> Device z -> Sandwich (Leaf z) (f i) (g o)
  Pile :: Splice h => Sandwich xs (h x) c -> Sandwich ys a (h x) -> Sandwich (Fork (h x) xs ys) a c

o :: Splice h => Sandwich xs (h x) c -> Sandwich ys a (h x) -> Sandwich (Fork (h x) xs ys) a c
o = Pile

instance Film (Sandwich zs a b) where
  lifetime (Item _ _ d) = lifetime d
  lifetime (Pile x y) = max (lifetime x) (lifetime y) -- but they should be equal
  next t (Item box spy d) = Item box spy (next t d)
  next t (Pile x y) = Pile (next t x) (next t y)

instance Editable (Sandwich zs a b) where
  cut t (Item box spy d) = fmap (\(d1,d2) -> (Item box spy d1, Item box spy d2)) (cut t d)
  cut t (Pile x y) = case cut t x of
    Just (x1,x2) -> case cut t y of
      Just (y1,y2) -> Just (Pile x1 y1, Pile x2 y2)
      Nothing -> error "composed objects have different lengths"
    Nothing -> Nothing
  glue (Item box spy d1) (Item _ _ d2) = Item box spy (glue d1 d2)
  glue (Pile x1 y1) (Pile x2 y2) = Pile (glue x1 x2) (glue y1 y2)

readSandwich :: Sandwich zs a (g o) -> g o
readSandwich (Item _ spy d) = look spy d
readSandwich (Pile s1 s2) = readSandwich s1

writeSandwich :: f i -> Sandwich zs (f i) b -> Sandwich zs (f i) b
writeSandwich msg (Item box spy d) = Item box spy (write box d msg)
writeSandwich msg (Pile s1 s2) =
  let s2' = writeSandwich msg s2
      x = readSandwich s2'
      s1' = writeSandwich x s1
  in Pile s1' s2'

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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Device (
  -- * Device
  Device,
  View,
  Editor,

  -- * Plan
  --
  -- | A 'Plan' defines a 'Device'\'s behavior and initial state.

  Plan,
  newCache,
  newInputbox,
  runPlan,

  -- * Profunctor Stuff
  F(..),
  Snip(..),
  Guest(..),
  host,
  guestDevice,

  -- * Replacement for Plan

  M(..),
  M'(..),
  wrap,
  feedback,
  fToM,
  mToF,

  -- * Debugging

  debug,
  --newCacheDebug,
--  newMailboxDebug,

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
import Control.Comonad
import Control.Category
import Control.Arrow
import Data.Profunctor
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Biapplicative
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.IntMap as IM
import Data.Proxy
import Prelude hiding (splitAt, id, (.))
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
-- Use 'look' to view the device. It takes a 'View' which defines how you want the data. The
-- value returned from 'look' represents a value throughout the 'Device' time span. You can then
-- sample it or take an average over the window, for example, to get concrete data.
--
-- Use 'edit' to influence the behavior of the device. It requires an 'Editor', which is
-- a reference to a mailbox (see 'newMailbox'). Mailbox values are also extended in time.
-- See the 'Q' type for an example.
--
-- Use 'next' to get a future time window of any device. Any mailboxes begin the new window empty.
--
-- Using 'cut' and 'glue' we can experiment with alternative histories in the same span of time.

type Device z = Guts z

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

-- | BAD DOCS, BAD A mailbox reference. See 'newMailbox'.
type Editor z f a = (f a -> f a) -> Device z -> Device z
--newtype EditMe z f m = EditMe (Device z -> (f m -> f m) -> Device z)

-- | A method of computing views of a device.
--
-- see 'look', 'newCache', 'newMailbox'
type View z f a = Device z -> f a

newtype Viewer z f a = Viewer (MeatBoneCache -> MailPile -> f a)



-- | Retrospectively, we can repackage a Device as a Guest ?
guestDevice :: Device z -> Editor z f a -> View z g b -> Guest (f a) (g b)
guestDevice d0 edit view = Guest d0 (F gen) where
  gen l fa dev =
    let dev' = edit (const fa) dev
        dna = gutsDNA dev
        boxes = gutsBoxes dev
        debug = gutsDebug dev
        dev'' = guts (gutsOld dev) (gutsMail dev') l dna boxes debug
    in (view dev'', dev'')

{-
guts :: MeatBoneCache -> MailPile -> Time -> DNA -> Mailboxes -> CacheView -> Guts z
guts old mail delta dna boxes debug = 
  let new = Cache2 (V.map (\f -> f delta old new mail) dna)
  in evaluateBones old `seq` Guts old new mail delta dna boxes debug
-}


-- | Continue to the next time window.
nextDevice :: Time -- ^ How long the next time window will be.
     -> Device z
     -> Device z
nextDevice newDelta d
  | newDelta < 0    = error ("next: next window length negative (" ++ show newDelta ++ ")")
  | otherwise = shiftNoMail newDelta d

-- | Observe a 'Device' some way.
-- Get some interesting 'View's from 'newMailbox' or 'newCache'. Combine them with 'Functor'
-- 'Applicative' and 'Monoid'.
look :: Viewer z f a -> Device z -> f a
look (Viewer f) d = f (gutsNew d) (gutsMail d)

-- | Merge a window of data into this mailbox.
edit :: Editor z f m -> Device z -> (f m -> f m) -> Device z
edit inner d f = inner f d

-- | Every device has a finite lifetime. This is that.
deviceSize :: Device z -> Time
deviceSize d = gutsSize d

-- | The inverse of 'glue'. Succeeds only if cut time is greater than zero and less than 'lifetime'.
cutDevice :: Time -> Device z -> Maybe (Device z, Device z)
cutDevice t dev = case innerCut t dev of
  Cutted l r -> Just (l,r)
  _          -> Nothing

innerCut :: Time -> Device z -> Cutting z
innerCut t d
  | t > 0 && t < gutsSize d = let (l,r) = cutGuts t d in Cutted l r
  | t <= 0 = OffLeft
  | t >= gutsSize d = OffRight

-- t should be in interior of guts lifetime
cutGuts :: Time -> Guts z -> (Guts z, Guts z)
cutGuts t (Guts old _ mail delta dna boxes debug) =
  let part1 = guts old mail t dna boxes debug
      part2 = shiftKeepMail (delta - t) part1
  in (part1, part2)

data Cutting z = OffLeft | OffRight | Cutted (Device z) (Device z)

-- |Add a cached state to this device. It can be used as memory or an integrator.
--
-- The generator function takes the state value as of the beginning of the 'Device' lifetime
-- /(old bone)/. It should return the final state at the end of that time /(new bone)/.
-- The observable output @g b@, known as the /meat/ represents anything that happens in between.
-- Note that bones pertain to points in time, unlike meat which extends over finite time.
--
-- Note the generator gets length and input from the current time window, not previous.
--
-- New bones are carried into the future via 'next' then evaluated to WHNF, becoming old in the process.
-- They can be used to speed up behaviors that depend on accumulated history.
newCache :: (Time -> s -> f a -> (g b,s)) -- ^How to compute output window from input window, state, and new window length
         -> s -- ^Initial seed state.
         -> View z f a -- ^Input window
         -> Plan z (View z g b)
newCache f base inputView = do
  k <- newBase Proxy base
  addRegen (\dt old new mail -> wrap2 (f dt (snd (old ! k)) (inputView (dummyDevice new mail))))
  addCacheDebug (Some $ CacheDebug "unnamed" (const "?") (const "?"))
  let viewer = Viewer (\cache boxes -> fst (cache ! k))
  return (look viewer)

dummyDevice :: MeatBoneCache -> MailPile -> Device z
dummyDevice cache pile = Guts bomb cache pile bomb bomb bomb bomb where
  bomb = error "(bug) some of the dummy device fields were used after all?"

{-

-- | Same as 'newCache' but takes a label and 'Show' instances for a better 'debug' report.
newCacheDebug :: forall z s f a g b . (Show (g b), Show s)
   => String -- ^ Label for this cache. Displays in the debug output.
   -> (Time -> s -> f a -> (g b,s)) -- ^How to compute output window from input window, state, and new window length
   -> s -- ^Initial seed state.
   -> View z f a -- ^Input window
   -> Plan z (View z g b)
newCacheDebug l f base (View input) = do
  k <- newBase Proxy base
  addRegen (\dt old new mail -> wrap2 (f dt (snd (old ! k)) (input new mail)))
  addCacheDebug (Some $ CacheDebug l (show :: g b -> String) (show :: s -> String))
  return $ View (\cache boxes -> fst (cache ! k))
-}

-- | Add a new input box to this device. Input boxes can be viewed or edited.
--
-- The observed value of an input box represents data spread over the device lifetime.
--
-- By default, on device creation or 'next', the box is empty. After this the box
-- value may be arbitrarily edited using the 'Editor'.
newInputbox :: Burn (f a)
            => f a -- ^ Used as the initial, empty, default, no signal value.
            -> Plan z (View z f a, Editor z f a)
newInputbox iv = do
  g <- gets psGen2
  let (k, g') = Mail.genKey g
  let box = Mail.box "unnamed" (const "?") burn iv k
  let someBox = Mail.some box
  modify (\ps -> ps { psGen2 = g', psMailboxes = psMailboxes ps ++ [someBox] })
  let sig = Viewer (\cache pile -> Mail.getMail k iv pile)
  return (look sig, deliver box)


{-
-- | Same as 'newMailbox' but takes a label and a Show instance. This is used by the 'debug' report.
newMailboxDebug :: forall z f m . (Splice f, Monoid (f m), Show (f m)) 
                => String -- ^ Label for this mailbox. Displays in the debug output.
                -> Plan z (View z f m, WriteMe z f m)
newMailboxDebug l = do
  g <- gets psGen2
  let (k, g') = Mail.genKey g
  let box = Mail.box l show wshift mempty mappend k
  let someBox = Mail.some box
  modify (\ps -> ps { psGen2 = g', psMailboxes = psMailboxes ps ++ [someBox] })
  let sig = View (\cache pile -> Mail.getMail k mempty pile)
  return (sig, WriteMe (deliver box))
-}

-- |Create a 'Device' according to the plan. Uses rank 2 polymorphism to isolate
-- references from the wrong devices.
--
-- You can either use your 'Device' entirely within the callback, or return it as
-- part of the @b@ in a way that hides the @z@. E.g. Wrap it in a record of accessor functions.
--
-- This formulation stops mixing devices with incompatible structure. But it also
-- stops composition of compatible devices that come from two places. Would encoding
-- the entire structure in the type parameter work out nicely?
runPlan :: Time -- ^ The length of the first window
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
  in finalize (guts initialCache noMail initialDelta template boxes debugV)

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
--  (spy1 :: View z Q (), box1) <- newMailboxDebug "box1"
  (spy2 :: View z Q (), box2) <- newInputbox (Q [])
  out1 <- newCache (\l c y -> (y <> y, succ c)) 'c' (const (pure [5])) :: Plan z (View z TF [Double])
--  out2 <- newCacheDebug "var1" (\l c y -> (y <> y <> y, succ c)) 'c' (pure []) :: Plan z (View z TF [Double])
  return $ \d -> do
    putStrLn (debug d ++ debug d)

debugFormatStr = "%10s %10s %20s %15s %15s"
debugFormat :: String -> String -> String -> String -> String -> String
debugFormat a b c d e = printf debugFormatStr as bs cs ds es where
  as = take 10 a
  bs = take 10 b
  cs = take 20 c
  ds = take 15 d
  es = take 15 e

debugGuts :: Guts z -> [String]
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
  in heading : mailReports ++ cacheReports

debugCache :: Some CacheDebug -> (Wrap2, Wrap2) -> String
debugCache (Some (CacheDebug l shMeat shBones)) (Wrap2 _ oldBone, Wrap2 meat newBone) =
  let z = z
      c = shMeat (unsafeCoerce meat)
      d = shBones (unsafeCoerce oldBone)
      e = shBones (unsafeCoerce newBone)
  in debugFormat l "(cache)" c d e

-- | Show internal data on a device. By default most values are unreadable.
-- But see 'newCacheDebug' and 'newMailboxDebug'.
debug :: Device z -> String
debug dev = unlines (debugGuts dev)

-- move forward in time, preserving mail by splitting and forgetting
shiftKeepMail :: Time -> Guts z -> Guts z
shiftKeepMail newDelta (Guts _ nowOldBones oldMail oldDelta dna boxes debug) =
  let pile' = foldl (\pile box -> Mail.forget oldDelta box pile) oldMail boxes
  in guts nowOldBones pile' newDelta dna boxes debug

-- move forward in time, disregarding any potential mail outside original window
shiftNoMail :: Time -> Guts z -> Guts z
shiftNoMail newDelta (Guts _ nowOldBones _ _ dna boxes debug) =
  guts nowOldBones noMail newDelta dna boxes debug

deliverGuts :: Mailbox (f m) -> (f m -> f m) -> Guts z -> Guts z
deliverGuts box f d = d { gutsMail = Mail.edit box f (gutsMail d) }

{-
-- Put m in device mailbox then regenerate meatbone cache from old bones.
-- Current meat, new bones are discarded.
editGutsMail :: Monoid (f m) => Key MailKey (f m) -> Guts z -> f m -> Guts z
editGutsMail k (Guts old _ mail delta dna forgetter debugger) x =
  let mail' = stuffMailbox k x mail
  in guts old mail' delta dna forgetter debugger
-}

deliver :: Mailbox (f m) -> (f m -> f m) -> Device z -> Device z
deliver box f someGuts = deliverGuts box f someGuts

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

{-
instance Functor f => Functor (Viewer z f) where
  fmap f (View g) = View (\cache boxes -> fmap f (g cache boxes))

instance Applicative f => Applicative (View z f) where
  pure x = View (\_ _ -> pure x)
  (View ff) <*> (View xx) = View (liftA2 (liftA2 (<*>)) ff xx)

instance (Applicative f, Semigroup m) => Semigroup (View z f m) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid m) => Monoid (View z f m) where
  mempty = pure mempty
-}



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




instance Life (Guts z) where
  next     = nextDevice
  lifetime = deviceSize
  cut      = cutDevice



{- the glue operation that requires so much extra type level fu can be eliminated by having
-- time-concat happen at the next higher level. I.e. glue :: [Device] -> [Device] -> [Device]
-- is simply concat. If you can cut a device into two, you can cut a [Device] into two [Device]'s.
-- This is more beautiful.
--
-- Now with less type variables a possibly glued [Device] can still be cut, and extended in time.
--
-- If you single out a device's inputs and outputs, hide the z, make pipes... then the pipes can also
-- be glued and cut.

-- remove gluing from the guts
-}



-- complete the rectangle of length l > 0
-- l = l2 + l1 => ext f l = ext f l2 . ext f l1
newtype F s a b = F { unF :: Time -> a -> s -> (b,s) } -- Category, Profunctor, ...

deriving instance Functor (F s a)

-- in f . g . h . i = f . (g . (h . i)), i "happens first"
-- but due to laziness we see the output of f first, which contains thunks.
-- to observe any effect of the arg, we must chew through all the layers.
-- effect such as "mouse outside box, do nothing"
instance Category (F s) where
  id = F (\_ a s -> (a,s))
  F gen2 . F gen1 = F (\l a s -> let (b,s') = gen1 l a s in gen2 l b s')

instance Semigroup s => Arrow (F s) where
  arr f = F (\l a s -> (f a,s))
  F gen1 *** F gen2 = F gen3 where
    gen3 l (a1,a2) s = ((b1,b2), s1' <> s2') where
      (b1, s1') = gen1 l a1 s
      (b2, s2') = gen2 l a2 s

instance Profunctor (F s) where
  rmap = fmap
  lmap g (F gen) = F (\l z s -> gen l (g z) s)

instance (Semigroup s, Semigroup b) => Semigroup (F s a b) where
  F gen1 <> F gen2 = F gen3 where
    gen3 l a s = (b1 <> b2, s1 <> s2) where
      (b1,s1) = gen1 l a s
      (b2,s2) = gen2 l a s

data Lens s u = Lens (s -> u) ((u -> u) -> s -> s)

instance Category Lens where
  id = Lens id id
  Lens g ong . Lens f onf = Lens (g . f) (onf . ong)

-- | Use a component of a larger state. Embed an F u a b in a larger state.
component :: Lens parent child -> F child a b -> F parent a b
component (Lens bor rpl) (F gen) = F gen' where
  gen' l a s = (b, rpl (const u') s) where (b, u') = gen l a (bor s)

-- | Bundle 'F' with a state and hide the type.
data Guest a b = forall s . Guest s (F s a b)
deriving instance Functor (Guest a)

instance Profunctor Guest where
  rmap = fmap
  lmap g (Guest s bigF) = Guest s (lmap g bigF)

feedGuest :: Time -> a -> Guest a b -> (b, Guest a b)
feedGuest l a (Guest s (F gen)) = let (b,s') = gen l a s in (b, Guest s' (F gen))

-- | There is a free 'F' for any 'Guest'
wrappedGuest :: F (Guest a b) a b
wrappedGuest = F (\l a (Guest s (F gen)) -> let (b,s') = gen l a s in (b, Guest s' (F gen)))



-- This section is like a replacement for Plan

-- | Similar to 'F' but the state type can change.
newtype M a p b q = M { unM :: Time -> a -> p -> (b, q) }

-- | By temporarily rearranging the letters you can 'lmap' and 'rmap'.
newtype M' p q a b = M' { unM' :: M a p b q }

instance Bifunctor (M a p) where
  bimap f g (M gen) = M (\l a s -> let (b,s') = gen l a s in (f b, g s'))

instance Biapplicative (M a p) where
  bipure b u = M (\_ _ _ -> (b,u))
  M f <<*>> M g = M h where
    h l a s = (bf bx, uf ux) where
      (bf, uf) = f l a s
      (bx, ux) = g l a s

instance Profunctor (M' p q) where
  lmap f (M' (M gen)) = M' (M (\l a s -> gen l (f a) s ))
  rmap f (M' (M gen)) = M' (M (\l a s -> let (b,s') = gen l a s in (f b, s')))

instance Functor (M' p q a) where
  fmap = rmap

fToM :: F s a b -> M a s b s
fToM (F gen) = M gen
mToF :: M a s b s -> F s a b
mToF (M gen) = F gen

-- | Set up a component with a slice of state in preparation for '<<*>>'.
wrap :: (s -> u) -> F u a b -> M a s b u
wrap field (F gen) = M (\l a -> gen l a . field)

-- | When all components are gathered and the full state is reassembled,
-- loop output back to input. This lets components have a more or less limited
-- view of each other.
feedback :: M (a,b) s b s -> F s a b
feedback (M gen) = F (\l a s -> let (b,s') = gen l (a,b) s in (b,s'))


-- slowSlider :: F () (Q Mouse, Punctuated Float Float) (Q Float, Punctuated Picture Picture)
-- fastSlider :: F () (Q Mouse, TF Float) (Q Float, TF Picture)



-- | Acts like the contained 'Guest' until events arrive carrying a replacement.
-- Note the replacements can potentially use different state types.
host :: (Burn a, Glue b) => F (Guest a b) (a, Q (Guest a b)) b
host = F gen where
  gen l (input, Q reloads) (Guest s (F guest)) = case reloads of
    [] ->
      let (b,s') = guest l input s
      in (b, Guest s' (F guest))
    ((t, Guest newS (F newGuest)):more) ->
      let (b,_) = guest t input s
          (b',finalGuest) = gen (l - t) (burn t input, Q more) (Guest newS (F newGuest))
          finalB = glue t b b'
      in (finalB, finalGuest)


-- an F s a b packaged with a length, an s, and an a. s hidden
data Snip a b = forall s . Snip Time a s (F s a b)

deriving instance Functor (Snip r)

instance Comonad (Snip r) where
  extract (Snip l a s (F f)) = let (y,_) = f l a s in y
  extend f (Snip l a s (F g)) = Snip l a s (F g') where
    g' l' a' s' = (c, s') where
      c = f (Snip l' a' s' (F g))

cutSnip :: Burn r => Time -> Snip r a -> Maybe (Snip r a, Snip r a)
cutSnip t (Snip len a s (F gen)) = Just (l, r) where
  l = Snip t a s (F gen)
  r = Snip (len - t) (burn t a) s' (F gen) where (_,s') = gen t a s

growSnip :: Monoid r => Time -> Snip r a -> Snip r a
growSnip l sn@(Snip _ a s bigF) = Snip l mempty s' bigF where (_,s') = unF bigF l a s

lenSnip :: Snip r a -> Time
lenSnip (Snip l _ _ _) = l

instance (Monoid r, Burn r) => Life (Snip r a) where
  lifetime = lenSnip
  cut = cutSnip
  next = growSnip



type KB = ()
type ModKey = ()
data Ctrl = Ctrl
  { ctrlKeydown  :: [(KB,[ModKey])]
  , ctrlKeyup    :: [(KB,[ModKey])]
  , ctrlKeyagain :: [(KB,[ModKey])]
  , ctrlTyping   :: [Char]
  , ctrlMouse    :: [(Float,Float)]
  , ctrlClick    :: [Int]
  , ctrlUnclick  :: [Int]
  , ctrlScroll   :: [(Float,Float)]
  }

data MasterIn = MasterIn (TF Time) (Q Ctrl)

instance Burn MasterIn where
  burn l (MasterIn t ctrl) = MasterIn (burn l t) (burn l ctrl)



type Picture = Bool

--ex :: F Database MasterIn (TF Picture)

-- Arrow should be Cartesian Category
-- ArrowChoice should be Cocartesian Category
-- ArrowApply should be Closed Cartesian Category



{-
instance (Monoid (f a), Shift f) => Film (Snip (f a) b) where
  lifetime (Snip l _ _) = l
  next l' sn@(Snip _ _ gen) = Snip l' (mempty, s') gen where
    (_, s') = extract sn
  cut t (Snip l (a,s) (F gen)) = Just (l, r) where
    l = Snip t (a,s) (F gen)
    r = Snip (l - t) (wshift (-t) a, s') where (_,s') = gen t (a,s)
-}

swap (a,b) = (b,a)

button :: F Int (Q Int) (Q ())
button = F gen where
  gen _ mouse s = scanQMaybe ((swap .) . flip buttonRule) s mouse

buttonRule :: Int -> Int -> (Int, Maybe ())
buttonRule 0 0 = (0, Nothing)
buttonRule 0 1 = (1, Nothing)
buttonRule 1 0 = (0, Just ())
buttonRule 1 1 = (1, Nothing)
buttonRule a _ = (a, Nothing)

apply :: F s a b -> Time -> a -> s -> (b, s)
apply (F gen) = gen

-- can we prevent any activity unless the mouse is inside a rectangle
--hoverGuard :: Monoid e => (a -> Mouse) -> Float4 -> F s (Rail e a) b -> F s (Rail e a) b
--hoverGuard area (F gen) = F (\l (mouse,qs) s -> 
-- no because if b has continuous output, we can't invent any without running F

-- can we stop all events but pass other signals to the enclosed component if the mouse
-- is outside the rectangle
--hoverGuard :: Monoid e => (a -> Mouse) -> Float4 -> F s (Rail e a) b -> F s (Rail e a) b


-- can we transparently switch between a sleeping version of a component and one that
-- reacts to events if the mouse is inside a rectangle
--hg :: (a -> Bool) -> F s a b -> F s a b -> F s a b

-- the answer was... No and Yes
-- No - the current driver algorithm reacts to external events by "interrupting" and
-- recomputing (the same) history with a different end time. This essentially gives you
-- an intermediate state of the entire world. No combinator can skip over this.
-- Yes - the current driver can use a pretty big frame size, during which filters will
-- cause components down the line to compute larger spans with no events. In the limit
-- of infinite 'chunk' by the driver, an unreactive component is just a function of time.
-- If external input is delayed until the end of the frame, adding latency, no recomputation.

-- central issue with interrupting, we have to recompute from start of frame (which might
-- not be that much time) then build an intermediate state of the entire world. We need the
-- intermediate state because that is how resuming works, it starts from a state.
-- Recomputation: we have to recompute because the current schedule doesn't contain..
-- Intermediate state construction: since the input has changed, we have to resume from new...

-- The algorithm works by taking an input and starting state, and computing a lazy future.
-- the lazy future unfolds through observations and a spontaneous event dispatcher. This
-- part is "unreactive". I.e. the user has no ability to affect the future. The unreactive
-- algorithm.

-- (Conal refers to this period of unreactivity, which adds latency, as the "sample rate")

-- The 'object in stasis' algorithm is the opposite. The world is a single value and sits
-- there until the user does Put or Loot requests on it. Now it's reactive but inanimate.
-- The object in stasis algorithm.

-- (Note this works well for a typical audio thread, in which case a 1 chunk latency is unavoidable.)

-- To allow the user to affect a simulation in progress, the simplest method is to cancel
-- the rest of the future, re-run the simulation from the beginning (note there is no new
-- external input to consider before this point) but to the current moment. This gives you
-- a new concrete starting state, then you can "resume" from this state using the new input.
-- It has reacted to 1 external action, but at the cost of recomputing everything and
-- constructing an entire concrete intermediate state.

-- Recomputing: this seems a bit ridiculous because we basically already did all this work?
-- to get to this point? Is it true? Or did we compute something else.

-- Fully constructed intermediate state: also ridiculous because it's likely most of this
-- intermediate state hasn't changed and this at best forms a new hierarchy of records, at
-- worst components have rebuilt and provided new data with the same value.

-- Implications: If you give up and only use input at "the sample rate" (end of frame),
-- then to provide reactivity, you have to turn the frame rate up to some value, like 60
-- per second. So you still have the intermediate states constructed and deepseq'd 60/s.
-- On the other hand, if you solve the problem, and provide reactivity within the frame
-- in an efficient way, you can turn the "frame rate" lower, like 1/s, which would greatly
-- improve performance.

-- ***

-- The interrupting algorithm relies on inspecting the 'action stream' to compute the
-- next time an event would occur, i.e. in the world, a timer may goes off, or a ball
-- hits a barrier. This is a hypothetical time, since it's in the future and the user
-- may interrupt, triggering any number of things which happen sooner. So the dispatcher
-- can be woken up early and that future is cancelled.

-- When the future is cancelled, any work done to get that time is invalid, and something
-- somehow needs to start a recomputation from some point.


-- In the current interrupting algorithm, we attempt to save time by cutting the world
-- at the event time, a starting from there from then on.

-- But if multiple interruptions occur within the frame, we rebuild the entire world.
-- Each time. One way to avoid this is to remember the external events in this frame
-- so far, then recompute starting from the original world, then seek to the current
-- time. Now hypothetically, more work was done, but no new full world state was constructed.
-- Pieces of world are constructed for communication between components, but I don't
-- need to deep seq anything now. Also, for a fast frame rate, the difference between
-- recomputing from start and from middle of the frame may not be very much. For this
-- to work, the input events need to be saved and replayed before re-seeking. Not sure
-- if this difference is much of a difference. If the current algorithm is the
-- eager interrupting algorithm, this could be called the rewinding algorithm.
-- Well, this theoretically recomputes more than the eager algorithm, but produces
-- less intermediate states? Yes because if most components would not react to the
-- input, their intermediate state has not changed anyway.

-- Even time varying components which didn't change behavior would not have their
-- generator updated...

-- On constructing states. The cut operation or just the world arrow applied to input
-- results in stream or streams of data, and a final state. The final state is likely
-- a big record, but is only constructed at the end. But the data stream is likely a
-- record of streams of values. These streams may depend recursively on themselves over
-- the course of a frame. Within them are intermediate states, but much fewer of them
-- because many components don't changes at the time others do.

-- example 'stream record'
-- World {
--   worldPicture  :: V Picture -- V f t1 _
--   worldMissiles :: E (IO ()) -- (t2,io):_
-- }
--
-- EV b a = (E b, V a)

{-
data MouseEV = MouseEV {
  click  :: E Int,
  motion :: E Float2,
  mouse  :: V Float2
}
-}

-- example of component (stream function)
-- slider :: F () (MouseEV, V Float) (E Float, V Picture)
-- slider :: F () (MouseEV, V Float) (E Float, V Picture)

-- the slider has two behaviors that don't interact.
-- 1. on mouse events, potentially output update events
-- 2. the picture is a function of some other value, hopefully reflecting desired changes



-- so now we have new terminology for driver algorithms.
-- a *commit* is where an updated current state replaces the existing 'start' state
--   and the input sequence up to that point is discarded. The time pointer is set to zero.
-- A commit happens naturally at the end of a frame, before a new frame begins.
-- This is because the state at the end of frame is returned, so might as well use it.

-- A commit can be performed in the middle of a frame. But it requires recomputing the
-- frame, because the whole thing is based on Time -> a -> s -> (b,s). The frame length
-- is passed to the function, and everything uses that to know when to stop processing
-- and return the current state or form an "intermediate state". If the time changes, 
-- many states will not change, but intermediate states of physics engine will vary
-- critically on the commit time.

-- The cost of recomputing a frame is low if you haven't had many events happen and have
-- not looked at the final state yet. This is regardless of frame length. So I'm calling
-- this the complexity of a frame, which increases with continuous time. Each time an
-- event occurs somewhere, be it from external source or internally generated, complexity
-- increases. If a frame runs on for a long time or if many things occur quickly, you
-- get more complexity, and the cost of doing an early commit increases. Each time you
-- perform a commit, the new frame begins with minimal complexity. The commit itself has
-- a cost, and rewind seeking has a cost depending on complexity. At some point they
-- balance.

-- (side note, the idea that "we shouldn't restart a frame (for whatever reason) because
-- we have already done work that will be done *again* just to get back to the point of
-- divergence" is not always correct. Work is performed each time we cross through a
-- punctuation or event, but no work is performed as time passes through a time varying
-- value. If no events have happened yet, for instance, then no work has been done yet.
-- This means the most extreme rate to do commits that makes sense is to commit after
-- any event or punctuation. We suspect that this more costly than restarting sometimes.)

-- Jargon for the act of restarting the frame with updated or frame length? Reseeking?

-- imagine commit on every event (eager commit strategy) as smooth flow followed by
-- a big deepseq construction of the entire app state. If several events occur in short
-- time, especially if they were all predicted, this is expensive. Eager commit is
-- costly.

-- alternatively, imagine an infinite frame. Then there is smooth flow until an event
-- occurs, at which time we handle the event, and continue traversing the output stream.
-- The app state is never reconstructed, and we do minimal work. However the input
-- can't be changed at any point. Call this an unreactive behavior.
-- * integrators would need to be written to do their own periodic internal commits

-- So eager commit allows reactivity but is the most costly. An unreactive behavior is
-- the cheapest but there is not responsive to newly discovered input.

-- A third strategy is to commit at regular time intervals, at which time input is also
-- applied. This is called the "sample rate" by conal. This doesn't commit as much as
-- eager, and is reactive, but there is a delay. To reduce the max delay, increase the
-- "sample rate", which causes commits more often. Note in this case commits happen
-- regardless of if there's any input or changes of any sort! In case there are seldom
-- changes eager is actually better than this third strategy!

-- So we need a 4th strategy!

-- 1. Eager commit (commit on every event, and end of frame)
-- 2. Unreactive behavior (never commit, use infinite frame)
-- 3. Periodic commit (commit periodically and introduce new input late)
-- 4. ? (commit sometimes, reseeking sometimes)

-- eager commit with a long frame length may actually good for some apps, like desktop apps
-- periodic commit is standard for a video game, where people indeed complain about latency
-- unreactive behavior can work for a demo, movie, or a recording

-- I believe there is a method that is cheaper and more responsive than those 3 strategies.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Device (
  -- * Device
  Device,
  size,
  look,
  next,
  WriteMe(..),

  -- * Plan
  --
  -- | A 'Plan' defines a 'Device'\'s behavior and initial state.

  Plan,
  newCache,
  newMailbox,
  runPlan,
  Spy,

  -- * Time varying values
  --
  -- |Examples of types that can occupy windows of time.

  Varying(..),
  Occurring(..),
  Q(..),
  TF(..),
  semanticValue1,
  semanticValue2,

  -- * Time
  Time

) where

import qualified Data.Vector as V
import Control.Applicative
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.IntMap as IM
import Data.Proxy

-- public API

-- | A 'Device' is a reactive time varying database. Once created it can only be accessed
-- if associated (matching z) views were created in the 'Plan'. In which case 'look' and
-- 'next' allow observation of changing values.
--
-- Every 'Device' is semantically focused on a finite window of time. 'size' returns the
-- length of this window. The initial window size is determined at construction time, and
-- is defined by the 'next' operation from then on.
--
-- If mailboxes ('newMailbox') were created in the 'Plan' then you can influence the behavior
-- of the 'Device' with 'write'.
--
-- The interpretation of observed (or written) values relative to the current window
-- is application dependent. But see 'Varying' and 'Occurring' classes for two
-- families of possible interpretations.
data Device z = Device
  { deviceOld    :: MeatBoneCache
  , deviceNew    :: MeatBoneCache
  , deviceMail   :: Mailboxes
  , deviceWindow :: Time
  , deviceDNA    :: DNA }

type Plan z a = State (PlanState z) a

-- |A place to put values for the device to react to. See 'newMailbox'.
newtype WriteMe z m =
  WriteMe { write :: Device z -> m -> Device z {- ^'mappend' a value into this mailbox -} }

-- |A method of computing views of a device.
--
-- see 'look', 'newCache', 'newMailbox'
newtype Spy z a = Spy (MeatBoneCache -> Mailboxes -> a) deriving Functor

-- |Move to the next time window. Any mailboxes defined on the device are cleared.
next :: Time -- ^ How long the next time window will be.
     -> Device z
     -> Device z
next dt (Device _ new _ _ regens)
  | dt < 0    = error ("next: next window size negative (" ++ show dt ++ ")")
  | otherwise = shift regens new dt

-- |View some aspect of the 'Device' over the current time window.
look :: Spy z a -> Device z -> a
look (Spy f) = (\(Device _ new mail _ _) -> f new mail)

-- |Every device is currently \'focused\' on a span of time. This returns how long it is.
size :: Device z -> Time
size = deviceWindow

-- |Add a cached state to this device. It can be used as memory or an integrator. Returns a 'Spy'
-- which observes the window computed by the function. The output value is cached once inspected.
--
-- When a device is created via 'runPlan', 'next', or 'write', a cache of new windows is generated
-- lazily, but the base states are evaluated immediately to avoid leaking space.
--
-- See the "meat and bones" implementation notes of this feature.
newCache :: (Time -> s -> a -> (b,s)) -- ^How to compute output window from input window, state, and new window size
         -> s -- ^Initial seed state.
         -> Spy z a -- ^Input window
         -> Plan z (Spy z b)
newCache f base (Spy input) = do
  k <- newBase Proxy base
  addRegen (\dt old new mail -> wrap2 (f dt (snd (old ! k)) (input new mail)))
  return $ Spy (\cache _ -> fst (cache ! k))

-- |Add a new mailbox to this device. A mailbox's value describes a span of time
-- consistent with the device. By default a mailbox is 'mempty', and is reset to
-- 'mempty' after using 'next' on a device.
--
-- The returned 'Spy' can be used to observe the mailbox.
--
-- Once the device is constructed, 'write' can be used to update the mailbox.
newMailbox ::
  forall m z . Monoid m => Plan z (Spy z m, WriteMe z m)
newMailbox = do
  g <- gets psGen2
  let (k, _, g') = genKey g (mempty :: m)
  modify (\ps -> ps { psGen2 = g' })
  let sig = Spy (\_ mail -> mailboxLookup k mail)
  return (sig, WriteMe (deliver k))

-- |Create a 'Device' according to the plan. Uses rank 2 polymorphism to isolate
-- references from the wrong devices.
--
-- You can either use your 'Device' entirely within the callback, or return it as
-- part of the @b@ in a way that hides the @z@. E.g. Wrap it in a record of accessor functions.
runPlan :: Time -- ^ The size of the first window
        -> (forall z . Plan z (Device z -> b)) -- ^ New device is passed to the returned function
        -> b
runPlan initialDelta plan =
  let g1 = KeyGen 0
      g2 = KeyGen 0
      (finalize, goods) = runState plan (PlanState g1 g2 [] [])
      PlanState _ _ bases regens = goods
      bones = compileBases bases
      initialCache = skeleton bones
      template = V.fromList regens
  in finalize (device template initialDelta initialCache noMail)

data PlanState z = PlanState
  { psGen1   :: KeyGen BaseKey
  , psGen2   :: KeyGen MailKey
  , psBases  :: [Wrap]
  , psRegens :: [Regen]
  }


type Regen = Time -> MeatBoneCache -> MeatBoneCache -> Mailboxes -> Wrap2
type DNA = V.Vector Regen

shift :: DNA -> MeatBoneCache -> Time -> Device z
shift dna nowOld dt = device dna dt nowOld noMail

-- Put m in device mailbox then regenerate meatbone cache from old bones.
-- Current meat, new bones are discarded.
deliver :: Monoid m => Key MailKey m -> Device z -> m -> Device z
deliver k (Device old _ mail delta dna) x =
  let mail' = stuffMailbox k x mail
  in device dna delta old mail'

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



instance Applicative (Spy z) where
  pure x = Spy (\_ _ -> x)
  (Spy ff) <*> (Spy xx) = Spy (liftA2 (<*>) ff xx)

instance Semigroup m => Semigroup (Spy z m) where
  (<>) = liftA2 (<>)

instance Monoid m => Monoid (Spy z m) where
  mempty = pure mempty



-- unsafe, construct a device from regen rules (dna), old bones, and mail
-- the result contains new lazy meatbone cache.
-- When device is inspected, all old bones will be evaluated to avoid leaking space.
device :: DNA -> Time -> MeatBoneCache -> Mailboxes -> Device z
device dna delta old mail = 
  let new = Cache2 (V.map (\f -> f delta old new mail) dna)
      oldSkel = forgetMeat old
  in evaluateBones oldSkel `seq` Device oldSkel new mail delta dna

wrap2 :: (a,b) -> Wrap2
wrap2 (x,y) = Wrap2 x y

-- gross

type MeatBoneCache = Cache2 MeatBone
newtype Mailboxes = Mailboxes (IM.IntMap Wrap)
data Wrap = forall a . Wrap a
data Wrap2 = forall a b . Wrap2 a b
newtype Cache ph = Cache (V.Vector Wrap)
newtype Cache2 ph = Cache2 (V.Vector Wrap2)
newtype Key ph a = Key Int deriving Show
newtype BiKey a b = BiKey Int deriving Show
data BaseKey
data MailKey
data MeatBone

noMail :: Mailboxes
noMail = Mailboxes IM.empty

stuffMailbox :: Monoid m => Key MailKey m -> m -> Mailboxes -> Mailboxes
stuffMailbox (Key k) more (Mailboxes im) = Mailboxes (IM.insertWith f k (Wrap more) im) where
  f (Wrap _) (Wrap old) = Wrap (unsafeCoerce old <> more)

mailboxLookup :: Monoid m => Key MailKey m -> Mailboxes -> m
mailboxLookup (Key i) (Mailboxes im) = case IM.lookup i im of
  Just (Wrap x) -> unsafeCoerce x
  Nothing -> mempty

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



type Time = Double

class Varying f where
  at :: f a -> Time -> a

class Occurring f where
  occs :: f a -> [(Time,a)]

newtype TF a = TF (Time -> a) deriving Functor

instance Varying TF where
  (TF f) `at` t = f t

newtype Q a = Q [(Time,a)] deriving Functor

instance Occurring Q where
  occs (Q xs) = xs

-- |Witness the interpretation of a device as an infinite time varying value
-- relative to some view of it.
semanticValue1 :: Varying f => Spy z (f a) -> Device z -> Time -> a
semanticValue1 spy dev t
  | t < size dev = look spy dev `at` t
  | otherwise    = let w = size dev in semanticValue1 spy (next w dev) (t - w)

-- |Witness the interpretation of a device as an infinite stream of occurrences
-- relative to some view of it.
semanticValue2 :: Occurring f => Spy z (f a) -> Device z -> [(Time,a)]
semanticValue2 spy start = go 0 start where
  go base dev = let w = size dev in occs (spy `look` dev) ++ go (base + w) (next w dev)




{-

data Calc = Calc
  { calcDigit   :: Int -> Calc
  , calcPlus    :: Calc
  , calcEquals  :: Calc
  , calcDisplay :: Int }

-- try to implement a basic calculator

behold :: Calc
behold = runPlan 1 calculator

calculator :: Plan z (Device z -> Calc)
calculator = do
  (numpad, box1) <- newMailbox   -- [Int]
  (plusButt, box2) <- newMailbox -- [()]
  (eqButt, box3) <- newMailbox   -- [()]
  display  <- newCache alu (0,0) (liftA3 (,,) numpad plusButt eqButt)
  return (calcLoop (look display) box1 box2 box3)

alu :: Time -> (Int,Int) -> ([Int],[()],[()]) -> (Int, (Int,Int))
alu _ (a,b) ([] ,[]  ,[]  ) = (a, (a,b))
alu _ (a,b) ([n],[]  ,[]  ) = let c = a*10 + n in (c, (c,b))
alu _ (a,b) ([] ,[()],[]  ) = (0, (0, b + a))
alu _ (a,b) ([] ,[]  ,[()]) = (a+b, (a+b, 0))
alu _ (a,b) (x,y,z)         = error (show ((a,b),(x,y,z)))

calcLoop :: (Device z -> Int) -> WriteMe z [Int] -> WriteMe z [()] -> WriteMe z [()] -> Device z -> Calc
calcLoop display box1 box2 box3 start = go start where
  go old = let dev = next 1 old in Calc {
    calcDigit   = \n -> go (write box1 dev [n]),
    calcPlus    = go (write box2 dev [()]),
    calcEquals  = go (write box3 dev [()]),
    calcDisplay = display dev
  }

-- without spiffy Device thing
basic :: (Int,Int) -> Calc
basic start = go start where
  go (a,b) = Calc {
    calcDigit   = \n -> go (a*10 + n, b),
    calcPlus    = go (0, b + a),
    calcEquals  = go (a + b, 0),
    calcDisplay = a
  }
  
-- in this hypothetical, a cache holds a foreign device that can make pictures
-- be poked with numbers, and animate.
--
-- When the input Bool is False (disable), the inner device is dormant and does
-- not animate or process. Good for saving cycles on an inactive component!
--
-- otherwise it animates and gets poked, and shows a picture!
innerDeviceExample :: Time -> Inner -> (Bool, [Int]) -> (Maybe Picture, Inner)
innerDeviceExample delta inner (False,_) = (Nothing, inner)
innerDeviceExample delta inner (True,evs) =
  let inner' = applyEvs evs (doTime delta inner)
  in (Just (view inner'), inner')

-}

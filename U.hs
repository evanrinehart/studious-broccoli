{-# LANGUAGE ScopedTypeVariables #-}      
{-# LANGUAGE InstanceSigs #-}      
module U where

import Control.Comonad
import Control.Applicative
import System.IO.Unsafe

import Control.Exception
-- we have multiple microview types

newtype V a = V [(a,a)]   -- may have discontinuous jump
newtype E a = E [Maybe a] -- not nothing only at isolated times
unV (V x) = x
unE (E x) = x

instance Functor V where
  fmap f (V vs) = V (map (g f) vs) where
    g f (x1,x2) = (f x1, f x2)

-- point-wise ($)
instance Applicative V where
  pure x = V (repeat (x,x))
  V ff <*> V xx = V (zipWith g ff xx) where
    g (f1,f2) (x1,x2) = (f1 x1, f2 x2)

instance Functor E where
  fmap f (E es) = E (map (fmap f) es)

-- the no event
instance Monoid a => Monoid (E a) where
  mempty = never

-- the confluence of events
instance Semigroup a => Semigroup (E a) where
  E es1 <> E es2 = E (go es1 es2) where
    go (e:ee) (d:dd) = e <> d : go ee dd
    go e1 [] = e1
    go [] e2 = e2

instance Comonad V where
  duplicate (V vs) = V (go vs) where
    go vs@((x1,x2):rest) = (V vs, V ((x2,x2):rest)) : go rest
  extract = view

instance Foldable V where
  foldr f base (V ((x1,_):rest)) = f x1 (foldr f base (V rest))

{-
instance Traversable V where
  traverse :: forall f a b . Applicative f => (a -> f b) -> V a -> f (V b)
  traverse g (V vs) = fmap V (go vs) where
    go :: [(a,a)] -> f [(b,b)]
    go ((x1,x2):rest) = liftA3 h (g x1) (g x2) (go rest) where
      h y1 y2 hah = (y1,y2) : hah
-}
      

never = E (repeat Nothing)
once x = E (Just x : repeat Nothing)
delay (E es) = E (replicate 5 Nothing ++ es)

-- put an initial occurrence or merge it in if already occurring
inject :: a -> (a -> a -> a) -> E a -> E a
inject x f (E (Nothing:ee)) = E (Just x : ee)
inject x f (E (Just y:ee))  = E (Just (f x y) : ee)

filterMap :: (a -> Maybe b) -> E a -> E b
filterMap f (E es) = E (fmap g es) where
  g (Just x) = f x
  g Nothing  = Nothing

-- sample v at time of e occurrence
sample :: E (a -> b) -> V a -> E b
sample (E es) (V vs) = E (zipWith g es vs) where
  g Nothing  (x,_) = x `seq` Nothing
  g (Just f) (x,_)  = Just (f x)

snap :: (a -> b -> c) -> E a -> V b -> E c
snap f e v = sample (fmap f e) v

-- accumulate occurrences into running summary
-- carefully makes sure current value doesn't transitively depend on next value (loop)
scan :: (b -> a -> b) -> b -> E a -> V b
scan f b (E es) = V (go b es) where
  go b ~(mx : pp) = (b,mystery) : go mystery pp where
    mystery = case mx of
      Nothing -> b
      Just x  -> f b x

view :: V a -> a
view (V ((x,_):_)) = x

hold :: a -> E a -> V a
hold x0 = scan (\_ x -> x) x0


-- make sure that the IO actions used in dark magic don't have any side effects
-- begin dark magic
sysV :: IO a -> V a
sysV getX = V (go ()) where
  go _ = unsafePerformIO (getX >>= \x -> return (x,x)) : (go ())

sysE :: IO (Maybe a) -> E a
sysE getE = E (go ()) where
  go nil = unsafePerformIO (evaluate nil >> getE) : (go nil)
-- end magic

{-
A BBB
AACCB
AEECC
DDEF
DDEFF
GGGGF

0 111
00221
03322
4435
44355
66665
-}




-- example
classic :: a -> E (a -> a) -> V a
classic x src = let z = hold x (sample src z) in z


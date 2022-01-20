{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module TimeData where

import Data.List.NonEmpty
import Control.Exception

type Time = Double

newtype E a = E [(Time,a)]
  deriving (Show,Functor,Foldable)

data V a = V (Time -> a) Time (V a)
  deriving (Functor)

at :: V a -> Time -> a
at (V f p more) t = if t <= p then f t else at more t

{-
mergeEv :: Semigroup a => E a -> E a -> E a
mergeEv (E xs) (E ys) = E (go xs ys) where
  go xs [] = xs
  go [] ys = ys
  go ((t1,x):xmore) ((t2,y):ymore) = 
-}

instance Show (V a) where
  showsPrec d (V _ l more) = if isInfinite l
    then assert (l > 0) $ showParen (d > 10) (showString "V 📈 ∞")
    else showParen (d > 10) (showString "V 📈 " . shows l . showString " " . showsPrec 11 more)

instance Applicative V where
  pure x = eternal (const x)
  V ff t1 k1 <*> V fx t2 k2 = case compare t1 t2 of
    LT -> V (ff <*> fx) t1 (k1 <*> V fx t2 k2)
    GT -> V (ff <*> fx) t2 (V ff t1 k1 <*> k2)
    EQ -> V (ff <*> fx) t1 more where
      more | isInfinite t1 = endOfTimeError
           | otherwise = k1 <*> k2


instance Semigroup a => Semigroup (V a) where va <> vb = pure (<>) <*> va <*> vb
instance Monoid a => Monoid (V a) where mempty = pure mempty

-- | Use an alternative Show instance for 'V' that are piecewise constant.
newtype PWK a = PWK { unPWK :: V a }

instance Show a => Show (PWK a) where
  showsPrec dd v = go dd (uncons (samples v)) where
    go d ((x,t),Nothing) = assert (isInfinite t && t > 0) $ showParen (d > 10)
      ( showString "V "
      . showsPrec 11 x 
      . showString " ∞")
    go d ((x,t),Just more) = showParen (d > 10)
      ( showString "V "
      . showsPrec 11 x
      . showString " "
      . shows t
      . showString " "
      . go 11 (uncons more) )

samples :: PWK a -> NonEmpty (a,Time)
samples (PWK v) = go 0 v where
  go t (V f l more) = if isInfinite l then (f (t + 1),l):|[] else (f (avg t l),l) <| go l more
  avg a b = (a + b)/2
  

-- execute all actions <= ubound, return the rest
dispatchActions :: Time -> [(Time,IO ())] -> IO [(Time,IO ())]
dispatchActions ubound es = go es where
  go [] = return []
  go ((t,io):more) = if t <= ubound then io >> go more else return more

-- we only will be looked at stuff >= lbound, discard anything prior
discardValues :: Time -> V a -> V a
discardValues lbound v = go v where
  go (V f t more) = if t < lbound then go more else V f t more

-- we only want to keep events at or after lbound
discardEvents :: Time -> E a -> E a
discardEvents lbound (E es) = E (go es) where
  go [] = []
  go bueno@((t,_):more) = if t < lbound then go more else bueno

eternal :: (Time -> a) -> V a
eternal f = V f (1/0) endOfTimeError

endOfTimeError :: a
endOfTimeError = error "end of time"

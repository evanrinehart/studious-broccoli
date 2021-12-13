{-# LANGUAGE DeriveFunctor #-}
module CardGame where

import Common
import Event
import Data.IORef


-- a card game...

data Card = Card
  { cardFace :: Int
  , cardArea :: Float4
  , cardHide :: Bool }
      deriving Show

data CardAction a = CardAction { cgaClick :: Float2 -> a } deriving Functor

data MouseView = MouseView [Card] Card [Card]
  deriving Show

-- a basic lens
onThisCard f (MouseView ls c rs) = MouseView ls (f c) rs

-- the game's action functor
basicGame :: CardAction ([Card] -> [Card])
basicGame = CardAction { cgaClick = click (onThisCard toggleCard) }

-- another lens-like thing
click :: (MouseView -> MouseView) -> Float2 -> [Card] -> [Card]
click edit xy cards = case mouseSearch xy cards of
  Nothing   -> cards
  Just view -> fromMouseView (edit view)

-- find the card the mouse is over, in the form of a zipper
mouseSearch :: Float2 -> [Card] -> Maybe MouseView
mouseSearch xy cards = go [] cards where
  go prev [] = Nothing
  go prev (c:cs) = if xy `withinTile` cardArea c
    then Just (MouseView prev c cs)
    else go (c:prev) cs

-- put the cards back together from the view
fromMouseView :: MouseView -> [Card]
fromMouseView (MouseView prev c cs) = reverse prev ++ c:cs

-- basic geometry
within :: Float -> Float2 -> Bool
within x (F2 a b) = a < x && x < b

withinTile :: Float2 -> Float4 -> Bool
withinTile (F2 x y) (F4 a b w h) = within x (F2 a (a+w)) && within y (F2 b (b+h))

-- finally editting a card's field
toggleCard c = c {cardHide = not (cardHide c)}


-- remap controls, and require mouse position be passed in
foo :: CardAction (IO ()) -> UserActions (Float2 -> IO ())
foo ca = (pure (const $ pure ()))
  {
    uaClick = (\n xy -> cgaClick ca xy)
  }

ex = [Card 1 (F4 1 1 4 4) False, Card 2 (F4 5 0 4 4) False, Card 3 (F4 2 4 4 4) False]

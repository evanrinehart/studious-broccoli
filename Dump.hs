{-# LANGUAGE DeriveFunctor #-}
module Dump (
  Dy(..),
  dump,
  undump
) where

import Data.Char
import Data.List

-- | A tree of floats and named nodes
data Dy =
  D String [Dy] |
  F Float
    deriving Show

-- the unparser
dump :: Dy -> String
dump d = go d "" where
  go (F x) = (show x ++)
  go (D tag []) = (tag ++)
  go (D tag xs) = (tag ++) . ("[" ++) . foldr (.) id (h (map go xs)) . ("]" ++)
  h = intersperse (" " ++)

-- the parser
undump :: String -> Dy
undump = fst . parseDy . tokens

tokens :: String -> [Token]
tokens (']':more) = CloseTok : tokens (trim more)
tokens str = case reads str of -- try float
  ((x,more):_) -> FloatTok x : tokens (trim more)
  [] -> case takeWord str of -- try word
    Nothing -> case str of "" -> []; s:_ -> [UnknownTok s]
    Just (tag,'[':more) -> TagTokOpen tag : tokens (trim more)
    Just (tag,more) -> TagTok tag : tokens (trim more)

data Token
  = TagTok String
  | TagTokOpen String
  | CloseTok
  | FloatTok Float
  | UnknownTok Char
  deriving Show

parseDy :: [Token] -> (Dy,[Token])
parseDy (FloatTok x : more) = (F x, more)
parseDy (TagTokOpen tag : more) = let (xs,rest) = go more in (D tag xs, rest) where
  go (CloseTok : rest) = ([], rest)
  go input = case parseDy input of
    (_, []) -> error "early end of tokens (1)"
    (x, more') -> let (xs, rest) = go more' in (x:xs, rest)
parseDy (TagTok tag : more) = (D tag [], more)
parseDy (CloseTok : _) = error "unexpected token ]"
parseDy (UnknownTok c : _) = error ("unexpected token " ++ [c])
parseDy [] = error "early end of tokens (2)"

takeWord :: String -> Maybe (String, String)
takeWord input = case span isAlphaNum input of
  ("",_) -> Nothing
  (word,rest) -> Just (word,rest)

trim :: String -> String
trim = dropWhile (== ' ')


{-# LANGUAGE RankNTypes #-}
module Mailboxes where

-- the mailboxes became too complex to manage without proper types
-- we need:
--   the actual mailboxes, where mail is stored (at mailkey) or is mempty (not there)
--   debugger access that can read each mailbox and give a report string
--   "forgetter", which takes all the mail and shifts it left, to support 'cut'
--   obviously, looking up mail if you have the key.
--   when building the device, we don't have any mail but we have to accumulate
--     support for all this stuff safely.
--
--   so implement it with as few unsafeCoerces as possible.
--
--   * debugger
--   * forgetter

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Some
import Film

import Unsafe.Coerce (unsafeCoerce)

data Mailbox a = Mailbox
  { mbLabel :: String
  , mbDebug :: a -> String
  , mbShiftLeft :: Time -> a -> a
  , mbEmptyVal :: a
  , mbKey :: Key a }

newtype Key a = Key Int
newtype Mail a = Mail a
type MailPile = IntMap (Some Mail)
type Mailboxes = [Some Mailbox]

newtype MailKeyGen = MailKeyGen Int

genKey :: MailKeyGen -> (Key a, MailKeyGen)
genKey (MailKeyGen n) = (Key n, MailKeyGen (n+1))

noMail :: MailPile
noMail = IM.empty

getMail :: Key a -> a -> MailPile -> a
getMail (Key i) e pile = case IM.lookup i pile of
  Just (Some (Mail x)) -> unsafeCoerce x
  Nothing              -> e

adjustMail :: (a -> a) -> Key a -> MailPile -> MailPile
adjustMail f (Key i) pile = IM.adjust g i pile where
  g (Some (Mail x)) = Some (Mail (f (unsafeCoerce x)))

insertMailWith :: (a -> a -> a) -> Key a -> a -> MailPile -> MailPile
insertMailWith f (Key i) x pile = IM.insertWith g i (Some (Mail x)) pile where
  g (Some (Mail a)) (Some (Mail b)) = Some (Mail (f (unsafeCoerce a) (unsafeCoerce b)))

{-
insert :: Mailbox a -> a -> MailPile -> MailPile
insert mb x pile = insertMailWith f k x pile where
  f = mbAppend mb
  k = mbKey mb
-}

edit :: Mailbox a -> (a -> a) -> MailPile -> MailPile
edit mb f pile = insertMailWith g k e pile where
  k = mbKey mb
  e = mbEmptyVal mb
  g _ x = f x
  

forget :: Time -> Some Mailbox -> MailPile -> MailPile
forget dt sm pile = withSome sm $ \tools ->
  let shl = mbShiftLeft tools
      k = mbKey tools
  in adjustMail (shl dt) k pile

report :: Some Mailbox -> (String -> String -> String) -> MailPile -> String
report sm fmt pile = withSome sm $ \tools ->
  let l = mbLabel tools
      f = mbDebug tools
      e = mbEmptyVal tools
      k = mbKey tools
      x = getMail k e pile
  in fmt l (f x)

some box = Some box

box l sho shift e k =
  Mailbox {
    mbLabel=l,
    mbDebug=sho,
    mbShiftLeft=shift,
    mbEmptyVal=e,
    mbKey=k
  }

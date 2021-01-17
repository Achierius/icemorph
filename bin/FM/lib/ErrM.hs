-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrM where

import           Control.Applicative
import           Control.Monad

-- the Error monad: like Maybe type with error msgs
data Err a = Ok a | Bad String
  deriving (Read, Show, Eq)

instance Functor Err where
  fmap f (Bad s) = Bad s
  fmap f (Ok a)  = Ok (f a)

instance Applicative Err where
  pure  = Ok
  (<*>) = liftA2 id

instance Monad Err where
  return      = pure
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

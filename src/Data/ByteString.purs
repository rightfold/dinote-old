module Data.ByteString
( ByteString
, unsafeFreeze
, unsafeThaw

, fromString
) where

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import NN.Prelude
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding)

newtype ByteString = ByteString Buffer

unsafeFreeze :: Buffer -> ByteString
unsafeFreeze = ByteString

unsafeThaw :: ByteString -> Buffer
unsafeThaw (ByteString s) = s

fromString :: String -> Encoding -> ByteString
fromString s e = unsafeFreeze $ unsafePerformEff $ Buffer.fromString s e

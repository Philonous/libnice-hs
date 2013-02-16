{-# OPTIONS_HADDOCK hide #-}
module Network.Ice.Signals (
  module System.Glib.Signals,

  connect_WORD__NONE,
  connect_WORD_WORD__NONE,
  connect_WORD_WORD_WORD__NONE,
  connect_WORD_WORD_STRING__NONE,
  connect_WORD_WORD_STRING_STRING__NONE,

  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString   (peekUTFString,maybePeekUTFString)
import System.Glib.GError      (failOnGError)
import System.Glib.Signals
import System.Glib.GObject


{#context lib="nice" prefix="nice" #}

-- Here are the generators that turn a Haskell function into
-- a C function pointer. The fist Argument is always the widget,
-- the last one is the user g_pointer. Both are ignored.


connect_WORD__NONE ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> IO ()) ->
  IO (ConnectId obj)
connect_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> IO ()
        action _ int1 =
          failOnGError $
          user int1

connect_WORD_WORD__NONE ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_WORD_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> Word -> IO ()
        action _ int1 int2 =
          failOnGError $
          user int1 int2

connect_WORD_WORD_WORD__NONE ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> Word -> Word -> IO ()) ->
  IO (ConnectId obj)
connect_WORD_WORD_WORD__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> Word -> Word -> IO ()
        action _ int1 int2 int3 =
          failOnGError $
          user int1 int2 int3

connect_WORD_WORD_STRING__NONE ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> Word -> String -> IO ()) ->
  IO (ConnectId obj)
connect_WORD_WORD_STRING__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> Word -> CString -> IO ()
        action _ int1 int2 str3 =
          failOnGError $
          peekUTFString str3 >>= \str3' ->
          user int1 int2 str3'

connect_WORD_WORD_STRING_STRING__NONE ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (Word -> Word -> String -> String -> IO ()) ->
  IO (ConnectId obj)
connect_WORD_WORD_STRING_STRING__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Word -> Word -> CString -> CString -> IO ()
        action _ int1 int2 str3 str4 =
          failOnGError $
          peekUTFString str4 >>= \str4' ->
          peekUTFString str3 >>= \str3' ->
          user int1 int2 str3' str4'

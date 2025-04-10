{-# LANGUAGE OverloadedStrings #-}

module Fundamentals.File (
  File(..),
  IO_Method(..),
  (<<-),
  open_file,
  ) where

import System.IO (hGetContents, hPutStr, withFile, IOMode(..))

data IO_Method a = IO_M {
  read  :: String -> IO String,
  write :: String -> String -> IO a
}
disk_io_method :: IO_Method ()
disk_io_method = IO_M
  (\path -> withFile path ReadMode $ \h -> hGetContents h)
  (\path content -> withFile path WriteMode $ \h -> hPutStr h content)

data File a = File {
  path :: String,
  io_method :: IO_Method a,
  buffer :: a}
open_file :: String -> File ()
open_file path = File path disk_io_method ()

(<<-) :: IO (File a) -> String -> IO (File a)
(<<-) f s =
  f >>= \f' -> write (io_method f') (path f') s
    >>= \content -> return $ File (path f') (io_method f') content

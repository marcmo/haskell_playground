{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad
import Text.ProtocolBuffers(messageGet,messagePut,Utf8(..),defaultValue)
import Text.ProtocolBuffers.Header(Utf8(..))
import Text.ProtocolBuffers.Basic(utf8)
import qualified Data.ByteString.Lazy as L(readFile,writeFile,null)
import System
import Data.Char
import qualified Data.Foldable as F
import Data.Maybe(fromMaybe)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString)
import qualified System.IO.UTF8 as U(getLine,putStr)
import Data.Sequence((|>),empty)

import AddressBookProtos.AddressBook
import AddressBookProtos.Person as Person
import AddressBookProtos.Person.PhoneNumber
import AddressBookProtos.Person.PhoneType


mayRead ::  (Read a) => String -> Maybe a
mayRead s = case reads s of ((x,_):_) -> Just x ; _ -> Nothing

main = do
  args <- getArgs
  file <- case args of
            [file] -> return file
            _ -> getProgName >>= \self -> error $ "Usage "++self++" ADDRESS_BOOK_FILE"
  f <- L.readFile file
  newBook <- case messageGet f of
               Left msg -> error ("Failed to parse address book.\n"++msg)
               Right (address_book,_) -> do
                putStrLn "got it..." 
                F.mapM_ (\p -> putStrLn $ show p) (person address_book)
                promptForAddress address_book
  seq newBook $ L.writeFile file (messagePut newBook)

inStr prompt = U.putStr prompt >> getLine
inUtf8 prompt = inStr prompt >>= return . Utf8 . U.fromString

promptForAddress :: AddressBook -> IO AddressBook
promptForAddress address_book = do
  putStrLn "Enter person ID number: "
  personId <- fmap read getLine
  putStrLn "Enter name: "
  n <- fmap (Utf8 . U.fromString) getLine
  putStrLn "Enter email address (blank for none): "
  e <- fmap (Utf8 . U.fromString) getLine
  let me = if L.null (utf8 e) then Nothing else Just e
  let p = defaultValue {Person.id = personId, name = n, email = me}
  p' <- getPhone p
  return (address_book { person = person address_book |> p' })

getPhone :: Person -> IO Person
getPhone p' = do
  putStrLn "Enter a phone number (or leave blank to finish): "
  n <- fmap (Utf8 . U.fromString) getLine
  putStrLn $ "number was:" ++ show n
  if L.null(utf8 n)
    then return p'
    else do
      putStrLn "Is this a mobile, home, or work phone? "
      t <- fmap (mayRead . map toUpper) getLine
      getPhone (p' { phone = phone p' |> defaultValue { number = n, type' = t }})

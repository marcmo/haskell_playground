{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad(when)
import qualified Data.ByteString.Lazy as L(readFile)
import qualified Data.Foldable as F(forM_)
import System.Environment(getArgs,getProgName)
import qualified Data.ByteString.Lazy.UTF8 as U(toString,fromString)
import qualified System.IO.UTF8 as U(putStrLn)

import Text.ProtocolBuffers(messageGet,utf8,Utf8(..),isSet,getVal,defaultValue)
import Data.Sequence((|>),empty)

import AddressBookProtos.AddressBook(persons,AddressBook)
import AddressBookProtos.Person as Person(id,name,email,phone)
import AddressBookProtos.Person.PhoneNumber(number,type')
import AddressBookProtos.Person.PhoneType(PhoneType(MOBILE,HOME,WORK))

outLn = U.putStrLn . U.toString . utf8

listPeople :: AddressBook -> IO ()
listPeople address_book =
  F.forM_ (persons address_book) $ \p -> do
    putStr "Person ID: " >> print (Person.id p)
    putStr "  Name: " >> outLn (name p)
    when (isSet p email) $ do
      putStr "  E-mail address: " >> outLn (getVal p email)
    F.forM_ (phone p) $ \phone_number -> do
      putStr $ case getVal phone_number type' of
                 MOBILE -> "  Mobile phone #: "
                 HOME   -> "  Home phone #: "
                 WORK   -> "  Work phone #: "
      outLn (number phone_number)

main = do
  args <- getArgs
  f <- case args of
         [file] -> L.readFile file
         _ -> getProgName >>= \self -> error $ "Usage "++self++" ADDRESS_BOOK_FILE"
  case messageGet f of
    Left msg -> error ("Failed to parse address book.\n"++msg)
    Right (address_book,_) -> listPeople address_book

conv :: String -> Utf8
conv = Utf8 . U.fromString
addPerson :: AddressBook -> AddressBook
addPerson book =
    let p = defaultValue {Person.id = 222, name = conv "testperson", email = Just $ conv "this@that"} in
      book { persons = persons book |> p }
      
  


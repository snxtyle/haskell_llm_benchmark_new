module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)

newtype BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount account) = atomically $ writeTVar account Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount account) = atomically $ readTVar account

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount account) amount = atomically $ do
    balance <- readTVar account
    case balance of
        Nothing -> return Nothing
        Just current -> do
            let newBalance = current + amount
            writeTVar account (Just newBalance)
            return (Just newBalance)

openAccount :: IO BankAccount
openAccount = BankAccount <$> newTVarIO (Just 0)

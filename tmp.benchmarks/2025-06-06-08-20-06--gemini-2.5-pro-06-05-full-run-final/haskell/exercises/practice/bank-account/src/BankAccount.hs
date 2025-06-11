module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

newtype BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount account) = atomically $ writeTVar account Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount account) = atomically $ readTVar account

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount account) amount = atomically $ do
    mBalance <- readTVar account
    case mBalance of
        Nothing -> return Nothing
        Just balance -> do
            let newBalance = balance + amount
            writeTVar account (Just newBalance)
            return (Just newBalance)

openAccount :: IO BankAccount
openAccount = BankAccount <$> newTVarIO (Just 0)

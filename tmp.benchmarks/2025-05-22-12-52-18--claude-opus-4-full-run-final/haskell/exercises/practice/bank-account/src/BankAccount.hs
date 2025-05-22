module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

data BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount accountVar) = atomically $ writeTVar accountVar Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount accountVar) = atomically $ readTVar accountVar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount accountVar) amount = atomically $ do
    balance <- readTVar accountVar
    case balance of
        Nothing -> return Nothing
        Just currentBalance -> do
            let newBalance = currentBalance + amount
            writeTVar accountVar (Just newBalance)
            return (Just newBalance)

openAccount :: IO BankAccount
openAccount = do
    accountVar <- atomically $ newTVar (Just 0)
    return (BankAccount accountVar)

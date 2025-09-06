module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , updateBalance
    , openAccount
    ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)

data AccountState = Open Integer | Closed

newtype BankAccount = BankAccount (TVar AccountState)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount accountTVar) = atomically $ writeTVar accountTVar Closed

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount accountTVar) = atomically $ do
    state <- readTVar accountTVar
    case state of
        Open balance -> return $ Just balance
        Closed -> return Nothing

updateBalance :: BankAccount -> Integer -> IO (Maybe Integer)
updateBalance (BankAccount accountTVar) amount = atomically $ do
    state <- readTVar accountTVar
    case state of
        Open balance -> do
            let newBalance = balance + amount
            writeTVar accountTVar (Open newBalance)
            return $ Just newBalance
        Closed -> return Nothing

openAccount :: IO BankAccount
openAccount = do
    accountTVar <- atomically $ newTVar (Open 0)
    return $ BankAccount accountTVar

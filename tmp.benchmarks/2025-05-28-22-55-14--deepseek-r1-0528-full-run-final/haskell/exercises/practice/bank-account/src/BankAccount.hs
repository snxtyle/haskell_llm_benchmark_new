module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, writeTVar)

data AccountState = Open Integer | Closed
newtype BankAccount = BankAccount (TVar AccountState)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tvar) = atomically $ do
    state <- readTVar tvar
    case state of
        Open _ -> writeTVar tvar Closed
        Closed -> return ()

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tvar) = atomically $ do
    state <- readTVar tvar
    case state of
        Open balance -> return (Just balance)
        Closed -> return Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tvar) amount = atomically $ do
    state <- readTVar tvar
    case state of
        Open balance -> do
            let newBalance = balance + amount
            writeTVar tvar (Open newBalance)
            return (Just newBalance)
        Closed -> return Nothing

openAccount :: IO BankAccount
openAccount = do
    tvar <- newTVarIO (Open 0)
    return (BankAccount tvar)

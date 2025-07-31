module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

data BankAccount = BankAccount 
    { accountBalance :: TVar (Maybe Integer)
    }

closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ do
    writeTVar (accountBalance account) Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
    readTVar (accountBalance account)

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
    balance <- readTVar (accountBalance account)
    case balance of
        Nothing -> return Nothing
        Just b -> do
            let newBalance = b + amount
            writeTVar (accountBalance account) (Just newBalance)
            return (Just newBalance)

openAccount :: IO BankAccount
openAccount = atomically $ do
    balance <- newTVar (Just 0)
    return $ BankAccount balance

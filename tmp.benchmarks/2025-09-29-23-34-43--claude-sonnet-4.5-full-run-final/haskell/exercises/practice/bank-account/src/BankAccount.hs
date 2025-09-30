module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

data AccountStatus = Open | Closed deriving (Eq)

data BankAccount = BankAccount 
    { balance :: TVar Integer
    , status :: TVar AccountStatus
    }

closeAccount :: BankAccount -> IO ()
closeAccount account = atomically $ writeTVar (status account) Closed

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = atomically $ do
    accountStatus <- readTVar (status account)
    if accountStatus == Closed
        then return Nothing
        else Just <$> readTVar (balance account)

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = atomically $ do
    accountStatus <- readTVar (status account)
    if accountStatus == Closed
        then return Nothing
        else do
            currentBalance <- readTVar (balance account)
            let newBalance = currentBalance + amount
            writeTVar (balance account) newBalance
            return (Just newBalance)

openAccount :: IO BankAccount
openAccount = atomically $ do
    balanceVar <- newTVar 0
    statusVar <- newTVar Open
    return $ BankAccount balanceVar statusVar

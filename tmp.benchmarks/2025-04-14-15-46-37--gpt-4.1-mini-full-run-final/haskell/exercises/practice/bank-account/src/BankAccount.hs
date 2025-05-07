module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM
import Control.Monad (when)

data BankAccount = BankAccount
    { balanceVar :: TVar Integer
    , closedVar  :: TVar Bool
    }

openAccount :: IO BankAccount
openAccount = atomically $ do
    bal <- newTVar 0
    closed <- newTVar False
    return $ BankAccount bal closed

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount _ closed) = atomically $ writeTVar closed True

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount bal closed) = atomically $ do
    isClosed <- readTVar closed
    if isClosed
        then return Nothing
        else Just <$> readTVar bal

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount bal closed) amount = atomically $ do
    isClosed <- readTVar closed
    if isClosed
        then return Nothing
        else do
            current <- readTVar bal
            let newBalance = current + amount
            writeTVar bal newBalance
            return (Just newBalance)

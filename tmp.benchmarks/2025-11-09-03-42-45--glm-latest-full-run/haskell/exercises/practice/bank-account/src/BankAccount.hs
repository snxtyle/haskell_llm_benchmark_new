module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

newtype BankAccount = BankAccount (TVar (Maybe Integer))

openAccount :: IO BankAccount
openAccount = BankAccount <$> newTVarIO (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount accountVar) = atomically $ writeTVar accountVar Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount accountVar) = readTVarIO accountVar

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount accountVar) amount = atomically $ do
    maybeBalance <- readTVar accountVar
    case maybeBalance of
        Nothing -> return Nothing
        Just currentBalance ->
            let newBalance = currentBalance + amount
            in do
                writeTVar accountVar (Just newBalance)
                return (Just newBalance)

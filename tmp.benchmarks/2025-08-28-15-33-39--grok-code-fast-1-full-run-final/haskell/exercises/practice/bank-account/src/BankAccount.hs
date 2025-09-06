module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

data AccountState = Open Integer | Closed

newtype BankAccount = BankAccount (TVar AccountState)

openAccount :: IO BankAccount
openAccount = atomically $ do
    tv <- newTVar (Open 0)
    return (BankAccount tv)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount tv) = atomically $ modifyTVar tv (\_ -> Closed)

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount tv) = atomically $ do
    state <- readTVar tv
    case state of
        Open bal -> return (Just bal)
        Closed -> return Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount tv) amount = atomically $ do
    state <- readTVar tv
    case state of
        Closed -> return Nothing
        Open bal -> do
            let newBal = bal + amount
            writeTVar tv (Open newBal)
            return (Just newBal)

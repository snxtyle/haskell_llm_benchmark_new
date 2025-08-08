module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.MVar

-- BankAccount holds an MVar with Maybe Integer:
--   Just balance -> account is open
--   Nothing      -> account is closed
data BankAccount = BankAccount (MVar (Maybe Integer))

openAccount :: IO BankAccount
openAccount = do
    mv <- newMVar (Just 0)
    pure (BankAccount mv)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount mv) =
    modifyMVar_ mv (const (pure Nothing))

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount mv) =
    withMVar mv pure

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount mv) amount =
    modifyMVar mv $ \state ->
        case state of
            Nothing -> pure (Nothing, Nothing)
            Just bal ->
                let newBal = bal + amount
                in newBal `seq` pure (Just newBal, Just newBal)


{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module CrowdFunding.CrowdFundingEmulator where

--Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Data.Default               (Default (..))
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.TimeSlot as TimeSlot

-- Our offchain code
import qualified CrowdFunding.CrowdFundingOffChain as OffChain


test :: IO ()
test = do 
         -- 2 UTXO each under target amount but both together meet target. Expect success
         -- Emulator.runEmulatorTraceIO trace1

         -- 2 UTXOs but both total less than Target; So expected Fail
         -- Emulator.runEmulatorTraceIO trace2

         -- Non beneficiary tried to withdraw - Expected Fail.
         Emulator.runEmulatorTraceIO trace3


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    -- h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints
    -- h4 <- Emulator.activateContractWallet (Wallet.knownWallet 4) OffChain.endpoints
    -- h5 <- Emulator.activateContractWallet (Wallet.knownWallet 5) OffChain.endpoints

-- this will create 2 UTXOs of 4 and 8 ADA. 
-- Target amount is 10 ADA. So it meets amoung 2 UTXOs
-- 1st redeem action - tries before deadline even though target is met - and will fail
-- Later to change above since if Target is met we can release the amount to CrowdSource beneficiary.
-- 2nd redeem action will not meet the Target and deadline both.


--  So now this UTXO has only 4 ADA
    Emulator.callEndpoint @"Start" h1 $ OffChain.StartParams {
        OffChain.spcrowdSourcer = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.sptargetAmount = 10000000,     -- 10 Ada targetAmount
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spDdata = 1,
        OffChain.spcrowdRefunder = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
        OffChain.spAmount = 4000000              -- Wallet 1 contribution of 4 Ada
    }
    void $ waitNSlots 3

    -- 2nd UTXO  - with 8 Ada - so now total is 4 + 8 = 12 ADA - > 10 Ada target
    Emulator.callEndpoint @"Start" h1 $ OffChain.StartParams {
        OffChain.spcrowdSourcer = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.sptargetAmount = 10000000,     -- 100 Ada -- now reduced target to 10 Ada so it can pass
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spDdata = 1,
        OffChain.spcrowdRefunder = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
        OffChain.spAmount = 8000000              -- Wallet 1 contribution of 8 Ada.
    }


--  this should fail because Deadline did not pass - should be error from OffChain code itself.
    void $ waitNSlots 3
    Emulator.callEndpoint @"Grab" h2 $ OffChain.GrabParams {
          OffChain.gpcrowdSourcer  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpTargetAmount = 10000000
        , OffChain.gpAction    = 1
        , OffChain.gpInt    = 0
        , OffChain.gpRunner = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2
    }


-- Run 2 - should success 
    void $ waitNSlots 80
    Emulator.callEndpoint @"Grab" h2 $ OffChain.GrabParams {
          OffChain.gpcrowdSourcer  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpTargetAmount = 10000000
        , OffChain.gpAction    = 1
        , OffChain.gpInt    = 0
        , OffChain.gpRunner = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2
    }



    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s




trace2 :: Emulator.EmulatorTrace ()
trace2 = do
    -- h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    -- h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints
    h4 <- Emulator.activateContractWallet (Wallet.knownWallet 4) OffChain.endpoints
    -- h5 <- Emulator.activateContractWallet (Wallet.knownWallet 5) OffChain.endpoints

-- this will create 2 UTXOs of 4 and 4 ADA. 
-- Target amount is 10 ADA. So does not meet the Target amount.
-- 1st redeem action - tries before deadline even though target is met - and will fail
-- Later to change above since if Target is met we can release the amount to CrowdSource beneficiary.
-- 2nd redeem action will not meet the Target and deadline both.


--  So now this UTXO has only 4 ADA
    Emulator.callEndpoint @"Start" h3 $ OffChain.StartParams {
        OffChain.spcrowdSourcer = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4,
        OffChain.sptargetAmount = 10000000,     -- 10 Ada targetAmount
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spDdata = 1,
        OffChain.spcrowdRefunder = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spAmount = 4000000              -- Wallet 1 contribution of 4 Ada
    }
    void $ waitNSlots 3

    -- 2nd UTXO  - with 4 Ada - so now total is 4 + 4 = 8 ADA - < 10 Ada target
    Emulator.callEndpoint @"Start" h3 $ OffChain.StartParams {
        OffChain.spcrowdSourcer = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4,
        OffChain.sptargetAmount = 10000000,     -- 100 Ada -- now reduced target to 10 Ada so it can pass
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spDdata = 1,
        OffChain.spcrowdRefunder = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spAmount = 4000000              -- Wallet 1 contribution of 8 Ada.
    }


--  this should fail because Deadline did not pass - should be error from OffChain code itself.
    void $ waitNSlots 3
    Emulator.callEndpoint @"Grab" h4 $ OffChain.GrabParams {
          OffChain.gpcrowdSourcer  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpTargetAmount = 10000000
        , OffChain.gpAction    = 1
        , OffChain.gpInt    = 0
        , OffChain.gpRunner = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4
    }


-- Run 2 - should fail too as we dont have enough 
    void $ waitNSlots 80
    Emulator.callEndpoint @"Grab" h4 $ OffChain.GrabParams {
          OffChain.gpcrowdSourcer  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpTargetAmount = 10000000
        , OffChain.gpAction    = 1
        , OffChain.gpInt    = 0
        , OffChain.gpRunner = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 4
    }



    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s


trace3 :: Emulator.EmulatorTrace ()
trace3 = do
    -- h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    -- h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    h5 <- Emulator.activateContractWallet (Wallet.knownWallet 5) OffChain.endpoints
    h6 <- Emulator.activateContractWallet (Wallet.knownWallet 6) OffChain.endpoints
    h7 <- Emulator.activateContractWallet (Wallet.knownWallet 7) OffChain.endpoints
    h8 <- Emulator.activateContractWallet (Wallet.knownWallet 8) OffChain.endpoints

-- 1 UTXO created by wallet 5 for wallet 6 as the Beneficiary of CrowdFunding
-- but wallet 7 tried to withdraw


--  So now th
    Emulator.callEndpoint @"Start" h5 $ OffChain.StartParams {
        OffChain.spcrowdSourcer = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6,
        OffChain.sptargetAmount = 10000000,     -- 10 Ada targetAmount
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spDdata = 1,
        OffChain.spcrowdRefunder = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 5,
        OffChain.spAmount = 12000000              -- Wallet 
    }

    void $ waitNSlots 3

    Emulator.callEndpoint @"Start" h5 $ OffChain.StartParams {
        OffChain.spcrowdSourcer = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6,
        OffChain.sptargetAmount = 10000000,     -- 10 Ada targetAmount
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spDdata = 1,
        OffChain.spcrowdRefunder = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 5,
        OffChain.spAmount = 1000000              -- Wallet 
    }


-- should fail because beneficiary is not trying to redeem 
    void $ waitNSlots 80
    Emulator.callEndpoint @"Grab" h7 $ OffChain.GrabParams {
          OffChain.gpcrowdSourcer  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpTargetAmount = 10000000
        , OffChain.gpAction    = 1
        , OffChain.gpInt    = 1
        , OffChain.gpRunner = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 6
    }



    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s
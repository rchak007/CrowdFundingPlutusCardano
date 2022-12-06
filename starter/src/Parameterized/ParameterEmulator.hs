
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Beneficiary.BeneficiaryEmulator where

--Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Data.Default               (Default (..))
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import qualified Wallet.Emulator.Wallet as Wallet
import qualified Ledger.TimeSlot as TimeSlot

-- Our offchain code
import qualified Parameterized.ParameterOffChain as OffChain


test :: IO ()
test = do 
         -- Emulator.runEmulatorTraceIO trace1
         Emulator.runEmulatorTraceIO trace2


trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints
    h4 <- Emulator.activateContractWallet (Wallet.knownWallet 4) OffChain.endpoints

    Emulator.callEndpoint @"Start" h1 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spGuess = 20,
        OffChain.spAmount = 40000000
    }
    -- I added below h4 another address creates UTXO but everything else is same - 
    --      the parameters, and Datum are same. 
    -- void $ waitNSlots 2
    -- Emulator.callEndpoint @"Start" h4 $ OffChain.StartParams {
    --     OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
    --     OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
    --     OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
    --     OffChain.spGuess = 20,
    --     OffChain.spAmount = 40000000
    -- }

    -- here we create new UTXO - Parameters are same but Datum is differnt . 
    --     But still we will get the same Plutus Script address. 
    --     Since rememeber Datum is only part of compile and that Hash is part of Address. 
    --        and not value of Datum. 
    --     Whereas Value of Params make address different. 
    void $ waitNSlots 2
    Emulator.callEndpoint @"Start" h4 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spGuess = 10,
        OffChain.spAmount = 40000000
    }


    void $ waitNSlots 2
    Emulator.callEndpoint @"Start" h3 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 1,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 15,
        OffChain.spGuess = 1,
        OffChain.spAmount = 40000000
    }
    void $ waitNSlots 40
    Emulator.callEndpoint @"Grab" h1 $ OffChain.GrabParams {
          OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 15
        , OffChain.gpGuess    = 1
    }
    void $ waitNSlots 15
    Emulator.callEndpoint @"Grab" h2 $ OffChain.GrabParams {
          OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpGuess    = 20
    }
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s


trace2 :: Emulator.EmulatorTrace ()
trace2 = do
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints
    h4 <- Emulator.activateContractWallet (Wallet.knownWallet 4) OffChain.endpoints

    Emulator.callEndpoint @"Start" h1 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3,
        OffChain.spBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 2,
        OffChain.spDeadline = TimeSlot.slotToBeginPOSIXTime def 30,
        OffChain.spGuess = 20,
        OffChain.spAmount = 40000000
    }


    void $ waitNSlots 40
    Emulator.callEndpoint @"Grab" h2 $ OffChain.GrabParams {
          OffChain.gpCreator  = Wallet.mockWalletPaymentPubKeyHash $ Wallet.knownWallet 3
        , OffChain.gpDeadline = TimeSlot.slotToBeginPOSIXTime def 30
        , OffChain.gpGuess    = 20
    }

    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s



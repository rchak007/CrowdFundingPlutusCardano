--1 Extensions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

--This is to work not only with Strings
{-# LANGUAGE OverloadedStrings   #-}

-- required to use custom data types
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}


module Beneficiary.BeneficiaryOnChain where
-- Sections of a Plutus contract


--2 Imports
import PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.V2.Ledger.Api                            as V2LedgerApi
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Prelude                                         as P
import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada

--3 Onchain code


newtype Redeem = Redeem
    {
        redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class

data Dat = Dat 
    {
        creator :: Ledger.PaymentPubKeyHash,
        beneficiary :: Ledger.PaymentPubKeyHash,
        deadline :: V2LedgerApi.POSIXTime,
        ddata :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType Simple = Dat

{-# INLINABLE simpleType #-}
--Actual validator logic
simpleType :: Dat -> Redeem -> Contexts.ScriptContext -> Bool
simpleType d r context = 
    traceIfFalse "Sorry the guess is not correct" (ddata d == redeem r) &&
    traceIfFalse "Wrong pubkeyhash" signedBeneficiary &&
    traceIfFalse "Deadline not yet reached"  deadlinepassed &&
    traceIfFalse "Not paid royalties"  calculateRoyalties
    where
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        signedBeneficiary :: Bool
        signedBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

        deadlinepassed :: Bool
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline d)) (Contexts.txInfoValidRange txinfo)

        calculateRoyalties :: Bool
        calculateRoyalties = validateRoyalties d txinfo



--Boilerplate
simpleTypeV :: V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV = V2UtilsTypeScripts.mkTypedValidator @Simple 
    $$(compile [|| simpleType ||])
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

validator :: V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript simpleTypeV

validatorHash :: V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash simpleTypeV

address :: V1LAddress.Address
address = V1LAddress.scriptHashAddress validatorHash

{-# INLINABLE validateRoyalties #-}
validateRoyalties :: Dat -> Contexts.TxInfo -> Bool
validateRoyalties d txinfo = compareValues (qCreator d txinfo) (totalValue txinfo)

-- Get total amount ADA from the transaction
{-# INLINABLE totalValue #-}
totalValue :: Contexts.TxInfo -> Ada.Ada
totalValue txinfo = Ada.fromValue $ Contexts.valueSpent txinfo

--Get Value paid to the creator of the contract (10%)
{-# INLINABLE qCreator #-}
qCreator :: Dat -> Contexts.TxInfo -> Ada.Ada
qCreator d txinfo = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator d))
{-# INLINABLE compareValues #-}
compareValues :: Ada.Ada -> Ada.Ada -> Bool
-- compareValues Nothing _ = False
-- compareValues a a' = a' >= a `Ada.divide` 10
compareValues a a' = a >= a' `Ada.divide` 10
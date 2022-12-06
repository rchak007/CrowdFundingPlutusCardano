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
{-# LANGUAGE MultiParamTypeClasses      #-} 


module CrowdFunding.CrowdFundingOnChain where
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
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1
import qualified Ledger.Ada                                      as Ada
import qualified Ledger.Tx                                       as LedgerTx
import qualified Data.Map                                        as Map
import qualified Plutus.Contract                                 as PlutusContract
import qualified Data.Text                                       as DataText (Text)
-- import qualified Ledger.Contexts                                 as Validation

--3 Onchain code



-- targetAmount :: Ada.Ada
-- targetAmount = 1000000000     -- 1000 ADA. We hard code constant Target Amount

-- We need to keep track of which addresses are Contributing because only then they can ask Refund
-- actually we dont need to because the UTXOs will have this right? 

-- data CrowdParam = CrowdParam
--     {
 
--     crowdSourcer :: Ledger.PaymentPubKeyHash,  -- once Target reaches this address can redeem
--     -- beneficiary :: Ledger.PaymentPubKeyHash,
--     -- contributors :: [Ledger.PaymentPubKeyHash],
--     deadline :: V2LedgerApi.POSIXTime
    
--     } deriving P.Show

data CrowdParam = CrowdParam
    {
    crowdSourcer :: Ledger.PaymentPubKeyHash,
    targetAmount :: Ada.Ada,
    -- targetAmount :: Integer,
    deadline :: V2LedgerApi.POSIXTime
    -- crowdSourceId :: Integer    -- unique identifier for crowdFund - for now not including.
    } deriving P.Show


PlutusTx.unstableMakeIsData ''CrowdParam
PlutusTx.makeLift ''CrowdParam

newtype Redeem = Redeem
    {
        redeemAction :: Integer    -- 1 is Refund, 2 means crowdSourcer wants to take it back 
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem -- This is to instantiate the IsData class

data Dat = Dat 
    {
        ddata :: Integer,   --- For now later maybe PaymentPubKey for refund.
        crowdRefunder :: Ledger.PaymentPubKeyHash
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Dat

{-# INLINABLE simpleType #-}
--Actual validator logic
simpleType :: CrowdParam -> Dat -> Redeem -> Contexts.ScriptContext -> Bool
--simpleType :: CrowdParam -> BuiltinData -> Redeem -> Contexts.ScriptContext -> Bool
simpleType crowdp d r context = 
    -- traceIfFalse "Sorry the guess is not correct" (ddata d == redeemAction r) && 
    ( traceIfFalse "Wrong CrowdSourcerer pubkeyhash" signedByCrowdSourcer &&
    traceIfFalse "Deadline not yet reached" deadlinepassed &&
    traceIfFalse "Target amount not reached" crdBool &&
    (redeemAction r == 1) ) ||
    ( traceIfFalse "Wrong Refunder pubkeyhash" signedByRefunder && 
      traceIfFalse "Deadline already Passed" deadlineNotpassed &&
      (redeemAction r == 2))
    where
        txinfo :: Contexts.TxInfo
        txinfo = Contexts.scriptContextTxInfo context

        txInputs = Contexts.txInfoInputs txinfo -- txinfo.txInfoInputs   -- Txn Inputs

--      Get All UTXOs at the script address and then get the total Ada Value there
        -- adaScriptJust :: Maybe Ada.Ada
        adaScriptTotal :: Ada.Ada
        adaScriptTotal = sumAdaList validatedValue where-- do
            -- validatedValue <- Contexts.txOutValue . Contexts.txInInfoResolved <$> Contexts.findOwnInput context
                          validatedValue = Contexts.txOutValue . Contexts.txInInfoResolved <$> txInputs
            -- Just $ Ada.fromValue validatedValue
            
            --Ada.fromValue validatedValue

        sumAdaList :: [V2LedgerApi.Value] -> Ada.Ada
        sumAdaList [] = 0
        sumAdaList [x] = Ada.fromValue x
        sumAdaList (x:xs) = (Ada.fromValue x) + sumAdaList xs
    

-- findTxInByTxOutRef :: TxOutRef -> TxInfo -> Maybe TxInInfo


--      Convert Maybe Ada to Ada
        adaScript :: Maybe Ada.Ada -> Ada.Ada
        adaScript (Just x) =  x
        adaScript Nothing = 0

        -- Get the Target Ada for CrowdSourcing
        targetAda :: Ada.Ada
        targetAda = targetAmount crowdp

        -- Validate target amount
        crdBool :: Bool
        -- crdBool = crdCmp crdAda targetAda
        --crdBool = crdCmp targetAda (adaTotal myscriptOutputsAt) 
        -- crdBool = crdCmp (adaScript adaScriptJust) targetAda
        crdBool = crdCmp (adaScriptTotal) targetAda

        -- Compare whether tager is reached
        crdCmp :: Ada.Ada -> Ada.Ada -> Bool
        crdCmp a a' = if a > a'
                        then True
                        else False



--      scriptOutputsAt :: ValidatorHash -> TxInfo -> [(DatumHash, Value)]
        --myscriptOutputsAt :: [(DatumHash, Value)]
        -- myscriptOutputsAt = Contexts.scriptOutputsAt (Contexts.ownHash context) txinfo

        -- myvaluePaidTo = Contexts.valuePaidTo txinfo (address crowdp)

        -- -- toPubKeyHash :: Address -> Maybe PubKeyHash
        -- myPukKeyHash = address crowdp

        -- adaTotal :: [(V2LedgerApi.OutputDatum, V2LedgerApi.Value)] -> Ada.Ada
        -- adaTotal [] = 0
        -- adaTotal ((_,v):[]) = Ada.fromValue v
        -- adaTotal ( (_,v): xs) = (Ada.fromValue v) + adaTotal xs


        -- -- crdVal :: V2LedgerApi.Value
        -- -- crdVal = valueLocked txinfo crowdp
        -- --crdVal =  Contexts.valueLockedBy txinfo $ validatorHash crowdp
        -- crdVal =  Contexts.valueLockedBy txinfo (Contexts.ownHash context ) -- validatorHash crowdp
        -- --crdAda :: Ada.Ada
        -- crdAda = Ada.fromValue crdVal   


        




        -- b2 :: P.String
        -- b2 = b1 a1 a3


--              We dont have beneficiary so its Signed by CrowdSourcer
--              signedByBeneficiary :: Bool
--              signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary benp)
        signedByCrowdSourcer :: Bool
        signedByCrowdSourcer = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (crowdSourcer crowdp)

        signedByRefunder :: Bool
        signedByRefunder = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (crowdRefunder d)


--              Deadline for CrowdSourcer deadline need to pass to redeem
        deadlinepassed :: Bool
        deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline crowdp)) (Contexts.txInfoValidRange txinfo)

        deadlineNotpassed :: Bool
        deadlineNotpassed = not $ LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline crowdp)) (Contexts.txInfoValidRange txinfo)


        -- valueLocked :: Contexts.TxInfo -> CrowdParam -> V2LedgerApi.Value
        -- valueLocked t c = Contexts.valueLockedBy t $ validatorHash c


-- valueLockedBy :: TxInfo -> ValidatorHash -> Value
-- Get the total value locked by the given validator in this transaction.
-- adaLockedBy :: TxInfo -> ValidatorHash -> Ada
-- Get the total amount of Ada locked by the given validator in this transaction.

        -- targetReached :: Bool
        -- -- targetReached = (  Ada.fromValue $ Contexts.valueLockedBy txinfo $ validatorHash crowdp)  == (targetAmount)
        -- --targetReached = (  adaLocked)  >= (targetAmount)
        -- -- targetReached = validateTargets crowdp txinfo
        -- targetReached = crdCmp 5000 (10000)

        -- adaTarget = Ada.fromValue targetAmount



        --getUTXOs ::  CrowdParam -> Ada.Ada
        --getUTXOs ::  CrowdParam -> PlutusContract.Contract w s DataText.Text (Maybe [(V2LedgerApi.TxOutRef, LedgerTx.ChainIndexTxOut)])
--        getUTXOs ::  CrowdParam -> [(V2LedgerApi.TxOutRef, LedgerTx.ChainIndexTxOut)]
        -- getUTXOs crowdp = do       
        --     utxos <- PlutusContract.utxosAt $ address  crowdp            
        --     let 
        --         --utxos <- PlutusContract.utxosAt $ address  crowdp
        --         xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        --         -- outAda = findUTXOAmt (xs, 0)   -- List and amout tuple. 
        --     return (Just xs)

        -- getTotalUTXOValues ::  CrowdParam -> Ada.Ada
        -- getTotalUTXOValues crowdp = 10
        --     where 
        --         Just xs = getUTXOs crowdp
                -- outAda = findUTXOAmt (xs, 0)   -- List and amout tuple. 
    

        -- findUTXOAmt :: ([(V2LedgerApi.TxOutRef, LedgerTx.ChainIndexTxOut)], Ada.Ada) -> Ada.Ada
        -- findUTXOAmt ([],v)  = 0
        -- findUTXOAmt ((oref, o), v) = v + (Ada.fromValue $ LedgerTx._ciTxOutValue o)
        -- findUTXOAmt (((oref,o): xs), v) = findUTXOAmt (xs, v + (Ada.fromValue $ LedgerTx._ciTxOutValue o) )
        
 --       findUTXOAmt :: 
        -- findUTXOAmt (((oref,o):xs) , amt)
        -- -- (Ada.fromValue $ LedgerTx._ciTxOutValue o) 
        --    | checkUTXO ((oref ,o), amt)  = return (amt + (Ada.fromValue $ LedgerTx._ciTxOutValue o))
        --    | otherwise = findUTXOAmt (xs, amt)


            --     else if (redeemAction r == 1) 
--         then 
--             traceIfFalse "Wrong pubkeyhash" signedByContributor &&
--             traceIfFalse "Deadline not yet reached"  deadlinepassed
--             where
--                 txinfo :: Contexts.TxInfo
--                 txinfo = Contexts.scriptContextTxInfo context

--                 signedByContributor :: Bool
--                 signedByContributor = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (crowdSourcer crowdp)

-- --              Deadline - we want the deadline not to pass for Contributor Refund
--                 deadlineNotpassed :: Bool
--                 deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.to (deadline benp)) (Contexts.txInfoValidRange txinfo)

--     else traceIfFalse "Redeem Action can only be 1 or 2 " (1 == 2)   -- Always fails


data Simple
instance V2UtilsTypeScripts.ValidatorTypes Simple where
    type instance RedeemerType Simple = Redeem
    type instance DatumType Simple = Dat

--Boilerplate

simpleTypeV :: CrowdParam -> V2UtilsTypeScripts.TypedValidator Simple
simpleTypeV crowdp = V2UtilsTypeScripts.mkTypedValidator @Simple 
    ($$(compile [|| simpleType ||]) `PlutusTx.applyCode` PlutusTx.liftCode crowdp)
    $$(compile [|| wrap ||]) where
        wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem


validator :: CrowdParam -> V2LedgerApi.Validator
validator = V2UtilsTypeScripts.validatorScript . simpleTypeV

validatorHash :: CrowdParam -> V2LedgerApi.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . simpleTypeV

address :: CrowdParam -> V1LAddress.Address
address = V1LAddress.scriptHashAddress . validatorHash

-- {-# INLINABLE validateRoyalties #-}
-- validateRoyalties :: BenParam -> Contexts.TxInfo -> Bool
-- validateRoyalties benp txinfo = compareValues (qCreator benp txinfo) (totalValue txinfo)

-- -- Get total amount ADA from the transaction
-- {-# INLINABLE totalValue #-}
-- totalValue :: Contexts.TxInfo -> Ada.Ada
-- totalValue txinfo = Ada.fromValue $ Contexts.valueSpent txinfo

-- --Get Value paid to the creator of the contract (10%)
-- {-# INLINABLE qCreator #-}
-- qCreator :: BenParam -> Contexts.TxInfo -> Ada.Ada
-- qCreator benp txinfo = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator benp))

-- {-# INLINABLE compareValues #-}
-- compareValues :: Ada.Ada -> Ada.Ada -> Bool
-- compareValues a a' = a <= a'

-- {-# INLINABLE adaLocked #-}
-- adaLocked :: Ada.Ada
-- adaLocked = Ada.fromValue $ valueLocked
        --adaLocked = ContextsV1.adaLockedBy txinfo $ validatorHash crowdp

-- {-# INLINABLE valueLocked #-}
-- valueLocked :: Contexts.TxInfo -> CrowdParam -> V2LedgerApi.Value
-- valueLocked txinfo crowdp = Contexts.valueLockedBy txinfo $ validatorHash crowdp

-- {-# INLINABLE validateTargets #-}
-- validateTargets :: CrowdParam -> Contexts.TxInfo -> Bool
-- --validateTargets crowdp txinfo = compareValues (targetAmount crowdp) ( Ada.fromValue $ valueLocked txinfo crowdp)
-- validateTargets crowdp txinfo = compareValues (targetAmount crowdp) ( 1000000)



-- a1 :: Ada.Ada
-- a1 = 10000

-- a22 :: V2LedgerApi.Value
-- a22 = Ada.toValue a1

-- a3 :: Ada.Ada
-- a3 = Ada.fromValue a22

-- b1 :: Ada.Ada -> Ada.Ada -> P.String
-- b1 a a' = if a > a'
--               then "greater"
--           else "not greater"
-- b2 :: P.String
-- b2 = b1 a1 a3
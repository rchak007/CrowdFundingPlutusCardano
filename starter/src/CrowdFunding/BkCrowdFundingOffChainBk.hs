
{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-} -- To allow notation like Getparams {..}

module CrowdFunding.CrowdFundingOffChain where

-- Haskell imports
import qualified Control.Monad            as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)

-- Plutus imports
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Ledger                                          (PaymentPubKeyHash, Value)
import qualified Text.Printf              as TextPrintf (printf)
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger

--Own modules
import qualified CrowdFunding.CrowdFundingOnChain as OnChain

data StartParams = StartParams
    { 
      spcrowdSourcer :: Ledger.PaymentPubKeyHash     -- part of parameter
    , sptargetAmount :: Ada.Ada                      -- part of parameter
    , spDeadline    :: !LedgerApiV2.POSIXTime        -- part of parameter
    , spDdata :: !Integer                              -- Datum for now not meaningful   
    , spAmount      :: !Integer            -- Ada at script - This will be the Contribution amount 
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

data GrabParams = GrabParams 
    {
          gpcrowdSourcer :: Ledger.PaymentPubKeyHash      -- part of parameter
        , gpDeadline :: !LedgerApiV2.POSIXTime            -- part of parameter
        , gpTargetAmount :: Ada.Ada                       -- part of parameter
        , gpGuess    :: !Integer                          -- Redeemer - for now meaningless
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type CrowdSchema =
    PlutusContract.Endpoint "Start" StartParams
    PlutusContract..\/ PlutusContract.Endpoint "Grab" GrabParams

start :: PlutusContract.AsContractError e => StartParams -> PlutusContract.Contract w s e ()
start sp = do
    let p = OnChain.CrowdParam
            {
                OnChain.crowdSourcer = spcrowdSourcer sp,
                OnChain.targetAmount = sptargetAmount sp,
                OnChain.deadline = spDeadline sp
            }
        d = OnChain.Dat { OnChain.ddata = spDdata sp}   -- dummy Datum for now
        v = Ada.lovelaceValueOf $ spAmount sp
        txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) (LedgerApiV2.Datum $ PlutusTx.toBuiltinData d) v
        -- txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) () v        
        lookups = Constraints.plutusV2OtherScript $ OnChain.validator p
        scriptAddress = OnChain.address p
        scriptHash = OnChain.validatorHash p

-- the final goal is to build and submit the transaction
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups txConstraints
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Start Endpoint - Submited Contribution amount - Value: %s ---------------------------" (P.show v)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Script with Address: %s and Hash: %s ---------------------------" (P.show scriptAddress) (P.show scriptHash)

grab :: forall w s. GrabParams -> PlutusContract.Contract w s DataText.Text ()
grab GrabParams{..} = do              -- the {..} lets us access the inside variables directly
    crowdSourcer <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    if now < gpDeadline    -- now deadline we need to Pass . Only after deadline we can do below for now.
        -- later we need to change when we want to collect refund or even if target is reached etc.
        then PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached - Deadline: %s - Now: %s" (P.show gpDeadline) (P.show now)
        else do
            let param = OnChain.CrowdParam {
                        OnChain.crowdSourcer = gpcrowdSourcer
                      , OnChain.targetAmount = gpTargetAmount
                      , OnChain.deadline = gpDeadline
                }
                r = OnChain.Redeem { OnChain.redeemAction = gpGuess }   -- not using yet just placeholder
            maybeutxo <- findUtxoInValidator param gpGuess --finds the utxos associated to the CrowdSourcer that have passed deadline and can withdraw
            amtUtxo <- amountUtxoInValidator param gpGuess
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Amount in 1st utxo -: %s" (P.show amtUtxo)
            case maybeutxo of
                Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %s or not deadline reached %s and pubkey %s" (P.show r) (P.show $ now) (P.show $ OnChain.crowdSourcer param)
                Just (oref, o) -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                                  Constraints.plutusV2OtherScript (OnChain.validator param)
                        tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            Constraints.mustValidateIn (LedgerApiV2.from now) -- P.<>
                            -- Constraints.mustPayToPubKey gpCreator (getTotalValuePay o) -- we dont have this royalty
                    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.Simple lookups tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                    PlutusContract.logInfo @P.String $ "collected gifts"

--getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getValue :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Ledger.Value
getValue (_, o) = LedgerTx._ciTxOutValue  o  


getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.Dat
getDatum (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    LedgerApiV2.Datum e <- snd datHashOrDatum
    
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.Dat) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Integer -> Bool
checkUTXO (oref,o) n = do
    case getDatum (oref,o) of
        Nothing -> False
        Just OnChain.Dat{..}
            | ddata == n -> True
            | otherwise  -> False


ada0 :: Ledger.Value
ada0 = Ada.toValue 0

ada25 :: Ledger.Value
ada25 = Ada.toValue 25

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref,o)] n = do
    if checkUTXO (oref, o) n then 
        return (oref, o)
    else 
        Nothing
findUTXO ((oref,o):xs) n
    | checkUTXO (oref ,o)  n = return (oref, o)
    | otherwise = findUTXO xs n



getAmtUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> Integer -> (Ledger.Value)
getAmtUTXO [] _ = ada0
getAmtUTXO [(oref,o)] n = getValue (oref, o)
getAmtUTXO ((oref,o):xs) n = getValue (oref, o)



amountUtxoInValidator :: OnChain.CrowdParam -> Integer -> PlutusContract.Contract w s DataText.Text (Ledger.Value)
amountUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = getAmtUTXO xs n
        -- Chuck 
        --v = P.printXs (xs, 0)
    --PlutusContract.logInfo @P.String $ TextPrintf.printf "Total utxos amount %s" (P.show v)
    return out


findUtxoInValidator :: OnChain.CrowdParam -> Integer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator gparam n = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs n
        -- Chuck 
        --v = P.printXs (xs, 0)
    --PlutusContract.logInfo @P.String $ TextPrintf.printf "Total utxos amount %s" (P.show v)
    return out


getTotalValuePay :: LedgerTx.ChainIndexTxOut -> Ledger.Value
getTotalValuePay o = do
    Ada.toValue $ (Ada.fromValue $ LedgerTx._ciTxOutValue o) `Ada.divide` 10
    -- return tValue

-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () CrowdSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` grab') >> endpoints
    where 
        start' = PlutusContract.endpoint @"Start" start
        grab' = PlutusContract.endpoint @"Grab" $ grab
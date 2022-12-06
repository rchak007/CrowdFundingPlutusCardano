{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Starting.MyCompiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Parameterized.ParameterOnChain as MFS
import qualified Ledger


writeValidator :: FilePath -> (MFS.BenParam -> LedgerApiV2.Validator) -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeMyFirstValidatorScript :: IO (Either (FileError ()) ())
-- writeMyFirstValidatorScript = writeValidator "src/Parameterized/ParameterOnChain.Plutus" MFS.validator
writeMyFirstValidatorScript = writeValidator "src/Parameterized/Deploy/Parameterized.plutus" $ MFS.validator $ MFS.BenParam 
    {
        MFS.creator = Ledger.PaymentPubKeyHash "80b34df2162e9c4a38ce63322a8f903c9455a0bebd64c02cf1f3222a",
        MFS.beneficiary = Ledger.PaymentPubKeyHash "75eacb8808f937e42cde4312d2d4bb42bd1cbfca379bbe90a3ec0383",
        MFS.deadline = 1665105051000
    }
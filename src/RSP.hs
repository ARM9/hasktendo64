module RSP where

import qualified Data.ByteString as B
import qualified Data.Vector     as V
import           Data.Word

import           Memory
import qualified RSP.R4000       as R4000

data RSP = RSP
            { cpu  :: R4000.R4000
            , dram :: Memory
            , iram :: Memory
            }

new :: RSP
new = RSP
        { cpu = R4000.new
        , dram = V.empty
        , iram = V.empty
        }

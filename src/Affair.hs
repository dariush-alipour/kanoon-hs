module Affair where

import qualified Race as Race 
import qualified Verdict as Verdict 

type Affair = (Race.Race, Race.Race, Verdict.Verdict)

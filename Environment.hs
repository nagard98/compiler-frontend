import qualified Data.Map as Map
import AbsGrammar ( Type, Position )

type LocalEnv = Map.Map String EnvData

type GlobalEnv = [LocalEnv]

-- TODO: expand this definition as needed
data EnvData = Generic Position Type 
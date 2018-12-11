module Runtime.Types where

-- Artefact
data Artefact = OCIArtefact | NixArtefact

-- Overlay: An unpacked artefact with sufficient configuration provided ready for deployment
-- For container based procedure, this would be a rootfs plus config.json
data Overlay = OCIOverlay String | NixOverlay

-- Automaton: A deployed overlay 
data Automaton = Automaton

data Container = Container { uid :: String
                           , path :: String
                           } deriving (Show)
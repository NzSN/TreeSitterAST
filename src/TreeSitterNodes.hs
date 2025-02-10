module TreeSitterNodes
(BasicInfo(..),

) where

data BasicInfo =
  BasicInfo { node_type :: String,
              named     :: Bool }

data Fields =

data Node =
  Leaf { info :: BasicInfo } |
  Internal { info :: BasicInfo,
             fields :: }

module BackendDescription.NodeDescriptionHelper where

import Utility

node_type_ident :: String -> String
node_type_ident t = upper_the_first_char t ++ "_T"

node_name_ident :: String -> String
node_name_ident t = t ++ "_i"

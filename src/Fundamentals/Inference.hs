-- | Logics that used to do transform a GrammarNode into
-- a form that acceptable in performance aspect. And inference
-- out a pattern of expressions that used to evaluate a GrammarNode
-- into correspond source code that used to generate sentence of
-- target language.
module Fundamentals.Inference where

import qualified TreeSitterGrammarNodes as TSGN

import Control.Monad.State (State, runState)
import Data.Text.Lazy (Text)

type TextGN = TSGN.GrammarNode Text

data TransitionPoint a =
  SEQ { seqNode :: a } |
  CHOICE { choiceNode :: a }
  deriving (Eq,Ord,Show)

data InferenceMeta a = IM {
  node :: a,
  transitionPoints :: [TransitionPoint a] }
  deriving (Eq,Ord,Show)

type InferGN = State (InferenceMeta TextGN)
trans :: (Text -> InferGN Text) -> TextGN -> TextGN
trans f n = fst $ runState (mapM f n) $ IM TSGN.Empty []

-- | Inference Rules

choiceTrans :: Text -> InferGN Text
choiceTrans = undefined

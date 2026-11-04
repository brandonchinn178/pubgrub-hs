module PubGrub.Utils.Text (
  showT,
) where

import Data.Text (Text)
import Data.Text qualified as Text

-- Remove when we can require text >= 2.1.2
showT :: (Show a) => a -> Text
showT = Text.pack . show

module Util where

import Data.Text (Text)
import qualified Data.Text as T

-- partition "/" "a/b/c" = Just ("a", "b/c")
partition :: Text -> Text -> Maybe (Text, Text)
partition sep s = case T.breakOnAll sep s of
    (left, right):_ -> case T.stripPrefix sep right of
        Just right -> Just (left, right)
        Nothing -> undefined
    [] -> Nothing

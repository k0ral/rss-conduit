{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Text.RSS.Lens (module Text.RSS.Lens) where

-- {{{ Imports
import           Text.RSS.Lens.Rules
import           Text.RSS.Types

import           Lens.Micro.TH
import           URI.ByteString
-- }}}

makeLensesWith rules ''RssCategory
makeLensesWith rules ''RssEnclosure
makeLensesWith rules ''RssSource
makeLensesWith rules ''RssItem
makeLensesWith rules ''RssTextInput
makeLensesWith rules ''RssCloud
makeLensesWith rules ''RssImage
makeLensesWith rules ''RssDocument

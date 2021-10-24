{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module HTMX.Types where

import Css3.Selector (ToCssSelector(..), SelectorGroup(..))
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON(..), Value(..), (.=))
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Links (Link)

-- | A URL that points to a hypermedia resource
newtype Hyperlink = Hyperlink { unHyperlink :: Text } deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-boost/>
-- | hx-boost can only be set to "true", so it takes no arguments
data HXBoost = HXBoost deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-confirm/>
data HXConfirm = HXConfirm
    { hxConfirmMessage :: Text
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-confirm/> 
data HXDelete = HXDelete
    { hxDeleteLink :: Hyperlink -- Text
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-disable/>
data HXDisable = HXDisable deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-encoding/>
data HXEncoding = HXEncoding deriving (Eq, Show)

-- | A type that represents the set of all HTMX extensions.
data HTMXExtension =
    JSONEnc
    | MethodOverride
    | MorphdomSwap
    | ClientSideTemplates
    | Debug
    | PathDeps
    | ClassTools
    | RemoveMe
    | IncludeVals
    | AJAXHeader
    | EventHeader
    | Preload
    | OtherHTMXExtension Text -- ^ Used for custom HTMX extensions
    deriving (Eq, Show, Generic)

instance Hashable HTMXExtension where

-- | <https://htmx.org/attributes/hx-ext/>
data HXExt = HXExt
    { hxExtIgnore :: Bool -- ^ Ignores extensions when set to @True@
    , hxExtExtensions :: HashSet HTMXExtension -- ^ The set of HTMX extensions to use or ignore
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-get/>
data HXGet = HXGet
    { hxGetLink :: Hyperlink
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-headers/>
data HXHeaders = HXHeaders { hxHeadersJSON :: Value } deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-history-elt/>
-- | Takes no arguments at all
data HXHistoryElt = HXHistoryElt

-- | <https://htmx.org/attributes/hx-include/>
data HXInclude = HXInclude
    { hxIncludeSelector :: SelectorGroup
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-indicator/>
data HXIndicator = HXIndicator
    { hxIndicatorClosest :: Bool
    , hxIndicatorSelector :: SelectorGroup
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-params/>
data HXParams where
    HXParams :: [Text] -> HXParams -- ^ Include the list of parameter names
    HXParamsNot :: [Text] -> HXParams -- ^ Include all EXCEPT the list of parameter names
    HXParamsAll :: HXParams -- ^ Include all parameters
    HXParamsNone :: HXParams -- ^ Include no parameters
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-patch/>
data HXPatch = HXPatch
    { hxPatchLink :: Hyperlink
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-post/>
data HXPost = HXPost
    { hxPostLink :: Hyperlink
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-preserve/>
data HXPreserve = HXPreserve
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-prompt/>
data HXPrompt = HXPrompt
    { hxPromptMessage :: Text
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-push-url/>
data HXPushURL where
    HXPushURL :: Bool -> HXPushURL -- ^ Specify whether or not the URL used within the
                                   -- current element will be added to browser history
                                   -- using @True@ or @False@ respectively
    HXPushURLLink :: Hyperlink -> HXPushURL -- ^ Use when you want to push a custom URL
                                            -- to the browser's history
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-put/>
data HXPut = HXPut
    { hxPutLink :: Hyperlink
    }
    deriving (Eq, Show)

-- TODO: HXRequest type not determined. Could just be JSON maybe?
{-
data HXRequest = HXRequest

data MaybeJavaScript a = JustValue a | JavaScript Text
    deriving (Eq, Show)

instance ToJSON a => ToJSON (MaybeJavaScript a) where
    toJSON :: MaybeJavaScript a -> Value
    toJSON mbJS = case mbJS of
        JustValue val -> toJSON val
        JavaScript expr -> String expr

data HXRequestVal = HXRequestVal
    { hxRequestValTimeout :: MaybeJavaScript Int
    , hxRequestValCredentials :: MaybeJavaScript Bool
    , hxRequestValNoHeaders :: MaybeJavaScript Bool
    }
    deriving (Eq, Show)

instance ToJSON HXRequestVal where
    toJSON :: HXRequestVal -> Value
    toJSON HXRequestVal{..} = Aeson.object
        [ "timeout" .= hxRequestValTimeout
        , "credentials" .= hxRequestValCredentials
        , "noHeaders" .= hxRequestValNoHeaders
        ]
-}

-- | <https://htmx.org/attributes/hx-select/>
data HXSelect = HXSelect
    { hxSelectSelector :: SelectorGroup
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-sse/>
data HXSSE where
    HXSSE :: { hxSSESource :: Text, hxSSEName :: Text} -> HXSSE -- ^ For specifying a SSE source and a SSE to respond to
    HXSSELink :: Text -> HXSSE -- ^ For declaring a SSE that can be inherited by children elements
    HXSSEName :: Text -> HXSSE -- ^ For declaring which SSE from a parent element to repsond to
    deriving (Eq, Show)

-- | For specifying how the response will be swapped in
-- relative to the target of an AJAX request
data SwapPos =
    SwapPosInner -- ^ Replace the inner HTML of the target element
    | SwapPosOuter -- ^ Replace the entire target element with the response
    | SwapPosBeforeBegin -- ^ Insert the response before the target element
    | SwapPosAfterBegin -- ^ Insert the response before the first child of the target element
    | SwapPosBeforeEnd -- ^ Insert the response after the last child of the target element
    | SwapPosAfterEnd -- ^ Insert the response after the target element
    | SwapPosNone -- ^ Does not append content from response (out of band items will still be processed)
    deriving (Eq, Show)

-- | The amount of time (in seconds) that HTMX will wait
-- after receiving a response to swap the content
data SwapDelay where
    SwapDelay :: Int -> SwapDelay
    deriving (Eq, Show)

-- | The time (in seconds) between the swap and the settle logic
data SwapSettle where
    SwapSettle :: Int -> SwapSettle
    deriving (Eq, Show)

-- | For specifying how you want the viewport to reach the specified location
data SwapViewType =
    SwapViewTypeScroll -- ^ Scroll the viewport smoothly to the specified location
    | SwapViewTypeShow -- ^ Instantly jump the viewport to the the specified location
    deriving (Eq, Show)

-- | For specifying where you want the viewport to be after content is appended to the document
data SwapViewSelector where
    SwapViewSelector :: SelectorGroup -> SwapViewSelector -- ^ Move the viewport to the top/bottom of a specific element
    SwapViewSelectorWindow :: SwapViewSelector -- ^ Move the viewport to the top/bottom of the browser window
    SwapViewSelectorNone :: SwapViewSelector -- ^ Move to either the top/bottom of the element this modifier is applied to
    deriving (Eq, Show)

-- | For specifying whether you want the viewport to go to
-- the top or bottom of the specified location
data SwapViewMove =
    SwapViewMoveTop -- ^ Move viewport to the top of the specified location
    | SwapViewMoveBottom -- ^ Move viewport to the bottom of the specified location
    deriving (Eq, Show)

-- | For changing the scrolling behavior of the target element
data SwapView = SwapView
    { swapViewType :: SwapViewType
    , swapViewSelector :: SwapViewSelector
    , swapViewMove :: SwapViewMove
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-swap/>
data HXSwap = HXSwap
    { hxSwapPos :: SwapPos
    , hxSwapDelay :: Maybe SwapDelay
    , hxSwapSettle :: Maybe SwapSettle
    , hxSwapView :: Maybe SwapView
    }
    deriving (Eq, Show)

-- | <https://htmx.org/attributes/hx-swap-oob/>
data HXSwapOOB where
    HXSwapOOB :: HXSwapOOB
    HXSwapOOBSwap :: SwapPos -> HXSwapOOB
    HXSwapOOBSwapSelector :: SwapPos -> SelectorGroup -> HXSwapOOB

-- | <https://htmx.org/attributes/hx-target/>
data HXTarget where
    HXTarget :: HXTarget --TODO: Keep like so or add "This" suffix?
    HXTargetSelector :: SelectorGroup -> HXTarget
    HXTargetSelectorClosest :: SelectorGroup -> HXTarget
    HXTargetSelectorFind :: SelectorGroup -> HXTarget

{-
-- TODO: Study more. Basically all possible events plus event modifiers.
-- type HXTriggerVal = Text

type HXTrigger = Text

-- BELOW EXPERIMENTAL!!

type HXWS = Text

-- TODO: Add QuasiQuoters for parsing and generating values that are checked at compile time for the various arguments to the HTMX attributes.
-- TODO: Write tests to check that the Val types are generating the correct Text for the HTMX attributes. Tests for HTMX tag functionality maybe?
-}
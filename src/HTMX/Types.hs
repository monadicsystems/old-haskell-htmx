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


data HXBoost = HXBoost deriving (Eq, Show)

data HXConfirm = HXConfirm
    { hxConfirmMessage :: Text
    }
    deriving (Eq, Show)

data HXDelete = HXDelete
    { hxDeleteLink :: Text
    }
    deriving (Eq, Show)

data HXDisable = HXDisable deriving (Eq, Show)

data HXEncoding = HXEncoding deriving (Eq, Show)

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
    | OtherHTMXExtension Text
    deriving (Eq, Show, Generic)

instance Hashable HTMXExtension where

data HXExt = HXExt
    { hxExtIgnore :: Bool
    , hxExtExtensions :: HashSet HTMXExtension
    }
    deriving (Eq, Show)

data HXGet = HXGet
    { hxGetLink :: Text
    }
    deriving (Eq, Show)

data HXHeaders a where
    HXHeaders :: ToJSON a => { hxHeadersJSON :: a } -> HXHeaders a

deriving instance (Eq a) => Eq (HXHeaders a)
deriving instance (Show a) => Show (HXHeaders a)

data HXHistoryElt = HXHistoryElt

data HXInclude = HXInclude
    { hxIncludeSelector :: SelectorGroup
    }
    deriving (Eq, Show)

data HXIndicator = HXIndicator
    { hxIndicatorClosest :: Bool
    , hxIndicatorSelector :: SelectorGroup
    }
    deriving (Eq, Show)

data HXParams where
    HXParams :: [Text] -> HXParams
    HXParamsNot :: [Text] -> HXParams
    HXParamsAll :: HXParams
    HXParamsNone :: HXParams
    deriving (Eq, Show)

data HXPatch = HXPatch
    { hxPatchLink :: Text
    }
    deriving (Eq, Show)

data HXPost = HXPost
    { hxPostLink :: Text
    }
    deriving (Eq, Show)

data HXPreserve = HXPreserve
    deriving (Eq, Show)

data HXPrompt = HXPrompt
    { hxPromptMessage :: Text
    }
    deriving (Eq, Show)

data HXPushURL = HXPushURL
    { hxPushURLLink :: Text
    }
    deriving (Eq, Show)

data HXPut = HXPut
    { hxPutLink :: Text
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

data HXSelect = HXSelect
    { hxSelectSelector :: SelectorGroup
    }
    deriving (Eq, Show)

data HXSSE where
    HXSSE :: { hxSSELink :: Text, hxSSEName :: Text} -> HXSSE
    HXSSELink :: Text -> HXSSE
    HXSSEName :: Text -> HXSSE
    deriving (Eq, Show)

data SwapPos =
    SwapPosInner
    | SwapPosOuter
    | SwapPosBeforeBegin
    | SwapPosAfterBegin
    | SwapPosBeforeEnd
    | SwapPosAfterEnd
    | SwapPosNone
    deriving (Eq, Show)

data SwapDelay where
    SwapDelay :: Int -> SwapDelay
    deriving (Eq, Show)

data SwapSettle where
    SwapSettle :: Int -> SwapSettle
    deriving (Eq, Show)

data SwapScrollSelector where
    SwapScrollSelector :: SelectorGroup -> SwapScrollSelector
    SwapScrollSelectorWindow :: SwapScrollSelector
    deriving (Eq, Show)

data SwapScrollMove = SwapScrollMoveTop | SwapScrollMoveBottom
    deriving (Eq, Show)

data SwapViewType = SwapViewTypeScroll | SwapViewTypeShow
    deriving (Eq, Show)

data SwapView where
    SwapView :: SwapViewType -> SwapScrollMove -> Maybe SwapScrollSelector -> SwapView
    deriving (Eq, Show)

data HXSwap = HXSwap
    { hxSwapValPos :: SwapPos
    , hxSwapValSwap :: Maybe SwapDelay
    , hxSwapValSettle :: Maybe SwapSettle
    , hxSwapValView :: Maybe SwapView
    }
    deriving (Eq, Show)

data HXSwapOOB where
    HXSwapOOB :: HXSwapOOB
    HXSwapOOBSwap :: SwapPos -> HXSwapOOB
    HXSwapOOBSwapSelector :: SwapPos -> SelectorGroup -> HXSwapOOB

data HXTarget where
    HXTarget :: HXTarget --TODO: Keep like normal or add "This" suffix?
    HXTargetSelector :: SelectorGroup -> HXTarget
    HXTargetSelectorClosest :: SelectorGroup -> HXTarget
    HXTargetSelectorFind :: SelectorGroup -> HXTarget

-- TODO: Study more. Basically all possible events plus event modifiers.
-- type HXTriggerVal = Text

type HXTrigger = Text

-- BELOW EXPERIMENTAL!!

type HXWS = Text

-- TODO: Add QuasiQuoters for parsing and generating values that are checked at compile time for the various arguments to the HTMX attributes.
-- TODO: Write tests to check that the Val types are generating the correct Text for the HTMX attributes. Tests for HTMX tag functionality maybe?

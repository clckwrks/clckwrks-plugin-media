{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Clckwrks.Media.Types where

import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), ixSet, ixFun)
import Data.SafeCopy (SafeCopy(..), base, contain, deriveSafeCopy)
import qualified Data.Serialize as S
import Data.Text     (Text)
import Web.Routes    (PathInfo(..))

newtype MediumId = MediumId { unMediumId :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable, PathInfo)

instance SafeCopy MediumId where
    getCopy = contain $ fmap MediumId S.get
    putCopy = contain . S.put . unMediumId
    errorTypeName _ = "MediumId"

data MediumKind
    = JPEG
    | PNG
    | GIF
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''MediumKind)

mediumContentType :: MediumKind -> String
mediumContentType JPEG = "image/jpeg; charset=binary"
mediumContentType PNG  = "image/png; charset=binary"
mediumContentType GIF  = "image/gif; charset=binary"

data Medium
    = Medium { mediumId     :: MediumId
             , uploadName   :: FilePath
             , mediumPath   :: FilePath
             , mediumKind   :: MediumKind
             -- , fileSize    :: Integer
             }
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Medium)

data PreviewSize
    = Tall
    | Grande
    | Venti
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''PreviewSize)

instance Indexable Medium where
    empty = ixSet [ ixFun ((:[]) . mediumId)
                  ]

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Clckwrks.Media.URL where

import Clckwrks.Media.Types (MediumId(..))
import Data.Data            (Data, Typeable)
import Web.Routes.TH        (derivePathInfo)

data MediaAdminURL
    = Upload
    | AllMedia
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''MediaAdminURL)

data MediaURL
    = GetMedium MediumId
    | Preview MediumId
    | MediaAdmin MediaAdminURL
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''MediaURL)

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module ApecsHintDemo where

import Apecs
import Apecs.Gloss
import Apecs.TH
import Linear (V2 (..))

newtype Position = Position (V2 Float) deriving (Read, Show)
newtype Velocity = Velocity (V2 Float) deriving (Read, Show)
data Flying = Flying deriving (Read, Show)

makeWorld "World" [''Position, ''Velocity, ''Flying, ''Camera]
makeMapComponents [''Position, ''Velocity, ''Flying]

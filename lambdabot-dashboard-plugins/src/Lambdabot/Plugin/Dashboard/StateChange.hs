{-# LANGUAGE ScopedTypeVariables #-}

module Lambdabot.Plugin.Dashboard.StateChange (StateChange (Original, Modified), fromStateChange, whenModified) where

data StateChange st = Original st | Modified st

fromStateChange :: StateChange st -> st
fromStateChange (Original st) = st
fromStateChange (Modified st) = st

whenModified :: Monad m => forall st. m () -> (st -> m ()) -> StateChange st -> m ()
whenModified otherwiseDo _ (Original _) = otherwiseDo
whenModified _ doIt (Modified dashboardState) = doIt dashboardState
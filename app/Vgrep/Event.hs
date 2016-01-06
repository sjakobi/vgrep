{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module Vgrep.Event where

import Control.Applicative
import Graphics.Vty as Vty

newtype EventHandler s = EventHandler { handle :: s -> Event -> IO (Next s) }

instance Monoid (EventHandler s) where
    mempty = EventHandler $ \_ _ -> return Unchanged
    h1 `mappend` h2 = EventHandler $ \state event ->
        liftA2 mappend (handle h1 state event)
                       (handle h2 state event)

data Next s = Continue s
            | Halt s
            | Unchanged
            deriving (Functor)

instance Monoid (Next s) where
    mempty = Unchanged
    Unchanged `mappend` next = next
    next      `mappend` _    = next

handleKey :: Key -> [Modifier] -> (s -> s) -> EventHandler s
handleKey key modifiers action = EventHandler $ \state -> \case
    EvKey k ms | k == key && ms == modifiers -> (return . Continue . action) state
    _                                        -> return Unchanged

handleResize :: (DisplayRegion -> s -> s) -> EventHandler s
handleResize action = EventHandler $ \state -> \case
    EvResize w h -> (return . Continue . action (w, h)) state
    _            -> return Unchanged

exitOn :: Key -> [Modifier] -> EventHandler s
exitOn key modifiers = EventHandler $ \state -> \case
    EvKey k ms | k == key && ms == modifiers -> (return . Halt) state
    _                                        -> return Unchanged

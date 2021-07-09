module Events
    (Event(..)
    , HasRaiseEvent(..)
    , RaiseEvent
    , raiseEvent
    , newRaiseEvent
    ) where

import           RIO

import           TMPacket


data Event =
  EventConnected
  | EventDisconnected
  | EventTMPacket !TMPacket
  deriving (Show)



class HasRaiseEvent env where
  raiseEventL :: Lens' env RaiseEvent


newtype RaiseEvent = RaiseEvent (Event -> IO ())

instance HasRaiseEvent RaiseEvent where
    raiseEventL = id

newRaiseEvent :: (Monad m) => (Event -> IO ()) -> m RaiseEvent 
newRaiseEvent f = pure (RaiseEvent f)


raiseEvent
    :: (MonadIO m, MonadReader env m, HasRaiseEvent env) => Event -> m ()
raiseEvent event = do
  RaiseEvent f <- view raiseEventL 
  liftIO $ f event

module Draw.Main where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.STM   ( STM )
import qualified Control.Concurrent.STM   as STM
import           Control.Exception        ( bracket, finally )
import           Control.Monad            ( forM_, forever )
import           Data.Aeson               ( (.:), (.=) )
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.KeyMap        as KeyMap
import qualified Data.Aeson.Types         as Aeson
import           Data.Bifunctor           ( first, second )
import qualified Data.ByteString          as BS
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Network.WebSockets       as WS

type RoomName = T.Text

type DrawingInstructions = T.Text

data Message
  = Clear
  | Draw DrawingInstructions
  | Joined [DrawingInstructions]

instance Aeson.ToJSON Message where
  toJSON Clear = Aeson.object [
       "tag" .= Aeson.String "Clear"
     ]
  toJSON (Draw instructions) = Aeson.object [
       "tag" .= Aeson.String "DrawingInstructions"
     , "instructions" .= instructions
     ]
  toJSON (Joined instructionsList) = Aeson.object [
       "tag" .= Aeson.String "Joined"
     , "instructionsList" .= instructionsList
     ]

data Command
  = JoinRoom RoomName
  | Drawed RoomName DrawingInstructions
  | Cleared RoomName

instance Aeson.FromJSON Command where
  parseJSON x@(Aeson.Object o) = case KeyMap.lookup "tag" o of
    Just (Aeson.String "JoinRoom") -> JoinRoom <$> o .: "room"
    Just (Aeson.String "Drawed")   -> Drawed <$> o .: "room" <*> o .: "instructions"
    Just (Aeson.String "Cleared")  -> Cleared <$> o .: "room"
    _                              -> Aeson.typeMismatch "Invalid json" x
  parseJSON x = Aeson.typeMismatch "Invalid json" x


data Client = Client { name   :: Int
                     , socket :: WS.Connection
                     }

instance Eq Client where
  c1 == c2 = name c1 == name c2

instance Show Client where
  show c = show $ name c

newClient :: Server -> WS.Connection -> IO Client
newClient server socket = do
  name <- STM.atomically $ do
    newName <- STM.readTVar (nextName server)
    STM.modifyTVar' (nextName server) (+1)
    pure newName
  pure Client {..}

data Server = Server { rooms    :: STM.TVar (Map.Map RoomName ([Client], [DrawingInstructions]))
                     , nextName :: STM.TVar Int
                     }

newServer :: IO Server
newServer = do
  rooms <- STM.newTVarIO Map.empty
  nextName <- STM.newTVarIO 0
  return Server {..}

main :: IO ()
main = do
  server <- newServer
  WS.runServer "0.0.0.0" 8080 $ \pending -> do
    conn <- WS.acceptRequest pending

    bracket (newClient server conn) (clientQuit server) (handleCommands server)

clientQuit :: Server -> Client -> IO ()
clientQuit server client = do
  STM.atomically $ do
    roomsMap <- STM.readTVar $ rooms server
    STM.writeTVar (rooms server) (fmap (first (filter (client /=))) roomsMap)

handleCommands :: Server -> Client -> IO ()
handleCommands server client = do
    forever $ do
      command <- Aeson.decode <$> WS.receiveData (socket client)

      case command of
        Just (JoinRoom room) -> do
          ins <- STM.atomically $ do
            roomsMap <- STM.readTVar $ rooms server
            case Map.lookup room roomsMap of
              Just x -> do
                STM.writeTVar (rooms server) (Map.adjust (first (client :)) room roomsMap)
                pure (snd x)

              Nothing -> do
                STM.writeTVar (rooms server) (Map.insert room ([client], []) roomsMap)
                pure []

          WS.sendTextData (socket client) (Aeson.encode (Joined ins))

        Just (Drawed room instructions) -> do
          m <- STM.atomically $ do
            roomsMap <- STM.readTVar $ rooms server
            STM.writeTVar (rooms server) (Map.adjust (second (instructions :)) room roomsMap)
            pure $ Map.lookup room roomsMap
          case m of
            Nothing           -> pure ()
            Just (clients, _) -> forM_ clients $ \c -> WS.sendTextData (socket c) (Aeson.encode (Draw instructions))

        Just (Cleared room) -> do
          m <- STM.atomically $ do
            roomsMap <- STM.readTVar $ rooms server
            STM.writeTVar (rooms server) (Map.adjust (second (const [])) room roomsMap)
            pure $ Map.lookup room roomsMap
          case m of
            Nothing           -> pure ()
            Just (clients, _) -> forM_ clients $ \c -> WS.sendTextData (socket c) (Aeson.encode Clear)

        Nothing -> pure ()

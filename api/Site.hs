{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Monad.Reader (asks)
import           Control.Monad.State (modify)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Text.Read (readMaybe)
import           Control.Lens (makeLenses, view, over)
import           Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Typeable (Typeable)
import           Data.Time.Clock (UTCTime)
import           Data.Time.ISO8601 (formatISO8601Millis, parseISO8601)
import qualified Text.JSON as JSON
import qualified Data.Map as Map
import           Snap.Util.FileServe (serveDirectory)
import           Snap (SnapletInit, Snaplet, Handler,
                 addRoutes, nestSnaplet, serveSnaplet,
                 defaultConfig, makeSnaplet, getParam,
                 snapletValue, writeText, method, Method (POST, GET))
import           Snap.Snaplet.AcidState (Update, Query, Acid,
                 HasAcid (getAcidStore), makeAcidic, update, query, acidInit)



data Entry = TransferEntry | SpendingEntry

data PersistentEntry = PersistentEntry { _active :: Bool
                                       , _createdAt :: UTCTime
                                       , _to :: T.Text
                                       , _from :: T.Text
                                       , _amount :: Float
                                       }
    deriving (Show,Ord,Eq,Typeable)

data PersistentEntries = PersistentEntries { _psEntries :: [PersistentEntry] }
    deriving (Show,Ord,Eq,Typeable)


readSpecificName :: (JSON.JSON a) => String -> Map.Map String JSON.JSValue -> JSON.Result a
readSpecificName name mapping = case Map.lookup name mapping of
    Just value -> JSON.readJSON value
    Nothing -> JSON.Error $ "Missing `"++name++"`"

instance JSON.JSON UTCTime where
    -- Example JSON representation 2012-04-23T18:25:43.511Z
    showJSON = JSON.showJSON . formatISO8601Millis
    readJSON o = do
        case JSON.readJSON o of
            JSON.Ok text -> case parseISO8601 text of
                Just time -> return time
                Nothing -> JSON.Error "parseISO8601 failed"
            JSON.Error m -> JSON.Error m
    -- FIXME how to pattern match on JSString?
    --readJSON _ = JSON.Error "Not JSString as UTCTime"

instance JSON.JSON PersistentEntry where
    showJSON PersistentEntry { _active=active, _to=to, _from=from, _amount=amount } = JSON.makeObj [
        ("entry", JSON.makeObj [ ("active", JSON.showJSON active)
                               , ("to", JSON.showJSON to)
                               , ("from", JSON.showJSON from)
                               , ("amount", JSON.showJSON amount)
                               ] ) ]

    readJSON (JSON.JSObject o) = maybePersistentEntry active to from amount
        where entryMapping = Map.fromList $ JSON.fromJSObject o
              active = readSpecificName "active" entryMapping :: JSON.Result Bool
              to = readSpecificName "to" entryMapping :: JSON.Result T.Text
              from = readSpecificName "from" entryMapping :: JSON.Result T.Text
              amount = readSpecificName "amount" entryMapping :: JSON.Result Float
              maybePersistentEntry err@(JSON.Error m) _ _ _ = JSON.Error m
              maybePersistentEntry _ err@(JSON.Error m) _ _ = JSON.Error m
              maybePersistentEntry _ _ err@(JSON.Error m) _ = JSON.Error m
              maybePersistentEntry _ _ _ err@(JSON.Error m) = JSON.Error m
              maybePersistentEntry (JSON.Ok active) (JSON.Ok to) (JSON.Ok from) (JSON.Ok amount) = return $ PersistentEntry { _active=active
                                                                                                                            , _to=to
                                                                                                                            , _from=from
                                                                                                                            , _amount=amount
                                                                                                                            }

makeLenses ''PersistentEntry
deriveSafeCopy 0 'base ''PersistentEntry
makeLenses ''PersistentEntries
deriveSafeCopy 0 'base ''PersistentEntries

addEntry :: PersistentEntry -> Update PersistentEntries ()
addEntry e = modify (over psEntries (e:))

getEntries :: Int -> Query PersistentEntries [PersistentEntry]
getEntries n = asks ((take n)._psEntries)

makeAcidic ''PersistentEntries ['addEntry, 'getEntries]

data App = App { _acid :: Snaplet (Acid PersistentEntries) }

type AppHandler = Handler App App

makeLenses ''App

instance HasAcid App PersistentEntries where
    getAcidStore = view (acid.snapletValue)


routeEntries = do
    writeText $ T.pack $ JSON.encode $ PersistentEntry { _active=True, _to="jim", _from="sofia", _amount=3.14}

routePostEntry = do
    writeText $ T.pack "POST entries"


routes :: [(ByteString, Handler App App ())]
routes = [ ("", serveDirectory "resources/static")
         , ("/entries", method GET routeEntries)
         , ("/entries", method POST routePostEntry)
         ]

--        writeText . T.pack . show =<< query GetEntries
--routes = [ ("", serveDirectory "resources/static")
--         , ("/list", writeText . T.pack . show =<< query GetList)
--         , ("/add/:elem", do
--            Just rawElem <- getParam "elem" -- TODO handle the Nothing-case
--            let Just elem = readMaybe $ T.unpack $ decodeUtf8 rawElem :: Maybe Int
--            update $ AddElement elem
--            writeText $ T.pack $ show elem
--           )
--         ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit (PersistentEntries [])
    addRoutes routes
    return $ App a

main = serveSnaplet defaultConfig app

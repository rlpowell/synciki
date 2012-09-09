module DropBox where

import Prelude
import Yesod
import Data.Maybe
import Data.List
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs

import Network.HTTP.Conduit -- the main module

import qualified Data.ByteString.Lazy.Char8 as LC

import Text.Regex.PCRE.Rex

newtype Title = Title String
  deriving (Show, Read, Eq)

instance PathPiece Natural where
    toPathPiece (Natural i) = T.pack $ show i
    fromPathPiece s =
        case reads $ T.unpack s of
            (i, ""):_
                | i < 1 -> Nothing
                | otherwise -> Just $ Natural i
            [] -> Nothing

-- Turn a list of url/file basename pairs into a widget
pageListPairsToWidget :: [(String, String)] -> GWidget sub master ()
pageListPairsToWidget pairs = toWidget [hamlet|
<ul>
  $forall (a, b) <- pairs
    <li>a: #{a}
    <li>b: #{b}
|]

-- Accessory function stolen from
-- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html
css :: ArrowXml a => String -> a XmlTree XmlTree
css tag = multi (hasName tag)

-- Accessory function stolen from
-- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html ,
-- with modifications to use http-conduit
webPageGet :: String -> IO (IOSArrow XmlTree (NTree XNode))
webPageGet url = do
  contents <- simpleHttp url
  return $ readString [withParseHTML yes, withWarnings no] (LC.unpack contents)

-- Takes a URL and returns a Just pair of the original and just the part between the last / and the extension, IFF the extension is .txt (otherwise Nothing)
fullFilePair :: String -> Maybe (String, String)
fullFilePair = [rex|^(?{ }.*/(?{ }[^/.]+)\.txt)$|]

-- Given a dropbox public URL, pull all the hrefs, turn them into
-- url/file basename pairs, and turn those into a widget
dropBoxPageListWidget :: String -> IO (GWidget sub master ())
dropBoxPageListWidget url = do
  page <- webPageGet url
  -- Most of this next line is from
  -- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html
  hrefs <- runX $ page >>> css "a" >>> getAttrValue "href"
  let pairs = nub $ catMaybes $ map fullFilePair hrefs
  return $ pageListPairsToWidget pairs


-- Subsites have foundations just like master sites.
data DropBox = DropBox

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSub "DropBox" [] [parseRoutes|
/ SubRootR GET
|]

-- And we'll spell out the handler type signature.
getSubRootR :: Yesod master => GHandler DropBox master RepHtml
getSubRootR = defaultLayout [whamlet|Welcome to the dropbox subsite!|]


-- -- *************** BEGIN dropbox oauth; FIXME: move else where ***********************************************
-- bsToText :: ByteString -> Text
-- bsToText = decodeUtf8With lenientDecode
-- 
-- authDropbox :: YesodAuth m
--             => ByteString -- ^ Consumer Key
--             -> ByteString -- ^ Consumer Secret
--             -> AuthPlugin m
-- authDropbox key secret = authOAuth
--                 (newOAuth { oauthServerName      = "dropbox"
--                           , oauthRequestUri      = "https://api.dropbox.com/1/oauth/request_token"
--                           , oauthAccessTokenUri  = "https://api.dropbox.com/1/oauth/access_token"
--                           , oauthAuthorizeUri    = "https://www.dropbox.com/1/oauth/authorize"
--                           , oauthSignatureMethod = HMACSHA1
--                           , oauthConsumerKey     = key
--                           , oauthConsumerSecret  = secret
--                           , oauthVersion         = OAuth10
--                           })
--                 extractCreds
--   where
--     extractCreds (Credential dic) = do
--         let crId = decodeUtf8With lenientDecode $ fromJust $ lookup "uid" dic
--         return $ Creds "dropbox" crId $ map (bsToText *** bsToText ) dic
-- 
-- dropboxUrl :: AuthRoute
-- dropboxUrl = oauthUrl "dropbox"
-- 
-- -- *************** END dropbox oauth; FIXME: move else where ***********************************************
-- 
-- --    getAuthId = return . Just . read . T.unpack . credsIdent
-- --
-- --    -- You can add other plugins like BrowserID, email or OAuth here
-- --    authPlugins _ = [ authDropbox "hudm8hxiyw0cf7z" "lfyu75cd04og056" ]

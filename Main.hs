{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies, TypeSynonymInstances, OverloadedStrings, QuasiQuotes, ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Happstack.Foundation
import qualified Data.IxSet as IxSet
import Data.IxSet (IxSet, Indexable, Proxy(..), getEQ, getOne, ixSet, ixFun)
import Data.Text  (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Maybe
import HSP

-- If we start doing complicated URL manipulation, this would be
-- valuable, but I think String (or re-branded String) is fine for now.
--
-- import Network.URL

-- Added for auth
import Happstack.Auth
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile
import Control.Exception           (bracket)
import Data.Acid.Local             (createCheckpointAndClose)
import System.FilePath             ((</>))

-- Added for Blaze under Auth
import qualified Happstack.Server.HSP.HTML as HTML
import Text.Blaze.Html                        (Html)
import Text.Blaze.Html.Renderer.String         (renderHtml)
import qualified HSX.XMLGenerator as HSX

-- Pulling remote web pages
import qualified Network.HTTP.Conduit as NHC
import qualified Text.XML.HXT.Core as HXT
import Data.Tree.NTree.TypeDefs
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.Regex.PCRE.Rex -- also needs TemplateHaskell, QuasiQuotes, ViewPatterns
import Data.List


------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

-- | an id which uniquely identifies a paste
--
-- NOTE: 'PasteId 0' indicates that a 'Paste' has not been assigned an
-- id yet. Though.. I am not thrilled about 0 having special meaning
-- that is not enforced by the type system.
newtype ComponentId = ComponentId { unComponentId :: Integer }
    deriving (Eq, Ord, Read, Show, Enum, Data, Typeable, SafeCopy)
$(derivePathInfo ''ComponentId)

newtype ComponentPath = ComponentPath { unComponentPath :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
$(derivePathInfo ''ComponentPath)

-- | The format of the paste. Currently we only support plain-text,
-- but later we might add support for Haskell syntax hightlighting,
-- etc.
data Format
    = PlainText
      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)
$(deriveSafeCopy 0 'base ''Format)

newtype MyURL = MyURL { unMyURL :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

----------------------
-- Component Files
----------------------
-- Each component is trolled for a list of files it contains, and
-- then various data about them is cached.

-- | The type used to search on/for/index component files; a
-- component path and a name uniquely identify a file, but only in
-- combination together.
data ComponentFileIndex = ComponentFileIndex
  { cfi_componentPath :: ComponentPath
  , cfi_name          :: Text
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ComponentFileIndex)

-- | Maps synciki's view of a component file (ComponentFileIndex) to
-- the outside world's view (a URL).  Also has the cache time and
-- the associated component.
data ComponentFile = ComponentFile
  { cf_componentFileIndex :: ComponentFileIndex
  , cf_url                :: MyURL
  , cf_componentId        :: ComponentId 
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ComponentFile)

instance Indexable ComponentFile where
    empty =
        ixSet [ ixFun $ (:[]) . cf_componentId
              , ixFun $ (:[]) . cf_componentFileIndex
              ]

-- | the component configuration
data Component = Component
    { componentId   :: ComponentId
    , title         :: Text
    , componentPath :: ComponentPath
    , added         :: UTCTime
    , userId        :: UserId
    , url           :: MyURL
    , refreshed     :: UTCTime
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Component)

-- | The 'Indexable Paste' instance will allow us to create an 'IxSet Paste'
--
-- We index on the 'PasteId' and the time it was pasted.
instance Indexable Component where
    empty =
        ixSet [ ixFun $ (:[]) . componentId
              , ixFun $ (:[]) . added
              , ixFun $ (:[]) . componentPath
              , ixFun $ (:[]) . Main.userId
              ]

-- | record to store in acid-state
data CtrlVState = CtrlVState
    { components      :: IxSet Component
    , nextComponentId :: ComponentId
    , componentFiles  :: IxSet ComponentFile
    }
    deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''CtrlVState)

-- | initial value to use with acid-state when no prior state is found
initialCtrlVState :: CtrlVState
initialCtrlVState =
    CtrlVState { components      = IxSet.empty
               , nextComponentId = ComponentId 1
               , componentFiles  = IxSet.empty
               }

------------------------------------------------------------------------------
-- Acid-State events
------------------------------------------------------------------------------

getCFsByCId :: ComponentId -> Query CtrlVState [ComponentFile]
getCFsByCId cId = IxSet.toList . getEQ cId . componentFiles <$> ask

getCFByCFI :: ComponentFileIndex -> Query CtrlVState (Maybe ComponentFile)
getCFByCFI cfi = getOne . getEQ cfi . componentFiles <$> ask

updateCF :: ComponentFile -> Update CtrlVState ComponentFileIndex
updateCF cf@ComponentFile{cf_componentFileIndex = cfi, ..} =
  do cvs@CtrlVState{..} <- get
     put $ cvs { componentFiles = IxSet.insert cf componentFiles }
     return cfi

insertCF :: ComponentFile -> Update CtrlVState ComponentFileIndex
insertCF cf@ComponentFile{cf_componentFileIndex = cfi, ..} =
 do cvs@CtrlVState{..} <- get
    put $ cvs { componentFiles = IxSet.updateIx cfi cf componentFiles }
    return cfi

deleteComponent :: ComponentId
            -> Update CtrlVState Bool
deleteComponent componentId
    | componentId <= ComponentId 0 =
        do return False
    | otherwise =
        do cvs@CtrlVState{..} <- get
           put $ cvs { components = IxSet.deleteIx componentId components }
           return True

-- | add or update a paste
--
-- If the PasteId is '0', then update the paste to use the next unused PasteId and insert it into the IxSet.
--
-- Otherwise, we update the existing paste.
insertComponent :: Component
            -> Update CtrlVState ComponentId
insertComponent p@Component{..}
    | componentId == ComponentId 0 =
        do cvs@CtrlVState{..} <- get
           put $ cvs { components = IxSet.insert (p { componentId = nextComponentId }) components
                     , nextComponentId = succ nextComponentId
                     }
           return nextComponentId
    | otherwise =
        do cvs@CtrlVState{..} <- get
           put $ cvs { components = IxSet.updateIx componentId p components }
           return componentId

-- | get a component by its id
getComponentById :: ComponentId -> Query CtrlVState (Maybe Component)
getComponentById cId = getOne . getEQ cId . components <$> ask

-- | get a component by its path
getComponentByPath :: ComponentPath -> Query CtrlVState (Maybe Component)
getComponentByPath cPath = getOne . getEQ cPath . components <$> ask

type Limit  = Int
type Offset = Int

-- | get recent pastes
getRecentComponents :: Limit  -- ^ maximum number of recent pastes to return
                -> Offset -- ^ number of pastes skip (useful for pagination)
                -> Query CtrlVState [Component]
getRecentComponents limit offset =
    do CtrlVState{..} <- ask
       return $ take limit $ drop offset $ IxSet.toDescList (Proxy :: Proxy UTCTime) components

-- | now we need to tell acid-state which functions should be turn into
-- acid-state events.
$(makeAcidic ''CtrlVState
   [ 'getComponentById
   , 'getRecentComponents
   , 'getComponentByPath
   , 'insertComponent
   , 'deleteComponent
   , 'getCFsByCId
   , 'getCFByCFI
   , 'insertCF
   , 'updateCF
   ])

------------------------------------------------------------------------------
-- Route
------------------------------------------------------------------------------

-- | All the routes for our web application
data Route
    = ViewRecent
    | ViewComponentById ComponentId
    | ViewComponentByPath ComponentPath
    | NewComponent
    | DeleteComponentPage ComponentId
    | CSS
    | U_AuthProfile AuthProfileURL
    | ViewPath ComponentPath
    | ViewPathPage ComponentId String String
      deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | we will just use template haskell to derive the route mapping
$(derivePathInfo ''Route)

------------------------------------------------------------------------------
-- CtrlV type-aliases
------------------------------------------------------------------------------

-- | The foundation types are heavily parameterized -- but for our app
-- we can pin all the type parameters down.
type CtrlV'    = FoundationT' Route CtrlVState () IO
type CtrlV     = XMLGenT CtrlV'
type CtrlVForm = FoundationForm Route CtrlVState () IO

------------------------------------------------------------------------------
-- From demo-hsp Acid.hs
------------------------------------------------------------------------------

-- | 'Acid' holds all the 'AcidState' handles for this site.
data Acid = Acid
    { acidAuth        :: AcidState AuthState
    , acidProfile     :: AcidState ProfileState
    }

-- | run an action which takes 'Acid'.
--
-- Uses 'bracket' to open / initialize / close all the 'AcidState' handles.
--
-- WARNING: The database files should only be opened by one thread in
-- one application at a time. If you want to access the database from
-- multiple threads (which you almost certainly do), then simply pass
-- the 'Acid' handle to each thread.
withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
        f (Acid auth profile)

------------------------------------------------------------------------------
-- Composite Data Functions
------------------------------------------------------------------------------
-- Stuff that operates on acid data by running its own queries.

-- | "Do the thing".
updateOrInsertCF :: ComponentFile -> CtrlV ComponentFileIndex
updateOrInsertCF cf@ComponentFile{cf_componentFileIndex = cfi, ..} =
  do
    mCF <- query (GetCFByCFI cfi)
    case mCF of
      Nothing -> do update (InsertCF cf)
      (Just dbcf) -> do update (UpdateCF cf)

------------------------------------------------------------------------------
-- appTemplate
------------------------------------------------------------------------------

-- | page template function
--
-- There are two forms here because we need to make it work for both
-- our usual happstack-foundation/HSP stuff, and for happstack-auth,
-- which is Blaze rather than HSP based.  So we make the base
-- template, and then make two versions that give it each of the two
-- contexts it needs.
baseAppTemplate :: ( XMLGenerator m
               , Happstack m
               , EmbedAsAttr m (Attr String Route)
               , EmbedAsChild m headers
               , EmbedAsChild m body
               ) =>
               Acid
            -> String   -- ^ page title
            -> headers  -- ^ extra headers to add to \<head\> tag
            -> body     -- ^ contents of \<body\> tag
            -> m (HSX.XMLType m)
baseAppTemplate acid@Acid{..} ttl moreHdrs bdy = HTML.defaultTemplate ttl <%><link rel="stylesheet" href=CSS type="text/css" media="screen" /><% moreHdrs %></%> $
                <%>
                 <div id="logo">^V</div>
                 <ul class="menu">
                  <li><a href=NewComponent>new component</a></li>
                  <li><a href=ViewRecent>recent components</a></li>
                 </ul>
                   <% do mUserId <- getUserId acidAuth acidProfile

                         -- Debugging
                         -- authState <- query' acidAuth AskAuthState
                         -- let authDump = traceMsg "authState: " $ ppDoc authState

                         case mUserId of
                            Nothing -> do
                              <ul class="auth">
                                <li>You are not logged in</li>
                                <li><a href=(U_AuthProfile $ AuthURL A_Login)>login</a></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_CreateAccount)>create account</a></li>
                              </ul>
                            (Just uid) -> do
                              <ul class="auth">
                                <li>You are logged in with profile <% show $ unUserId uid %></li>
                                -- Debugging
                                -- <li><% show authDump %></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_Logout)>logout</a></li>
                                <li><a href=(U_AuthProfile $ AuthURL A_AddAuth)>add another auth mode to your profile</a></li>
                              </ul>
                   %>
                 <% bdy %>
                </%>

-- This is the baseAppTemplate wrapped so that it can be used in the
-- normal happstack-foundation stuff.
appTemplate :: ( EmbedAsChild CtrlV' headers
               , EmbedAsChild CtrlV' body
               ) =>
               Acid
            -> String   -- ^ page title
            -> headers  -- ^ extra headers to add to \<head\> tag
            -> body     -- ^ contents of \<body\> tag
            -> CtrlV Response
appTemplate acid ttl moreHdrs bdy = liftM toResponse (XMLGenT $ baseAppTemplate acid ttl moreHdrs bdy)

-- | This makes it easy to embed a PasteId in an HSP template
instance EmbedAsChild CtrlV' ComponentId where
    asChild (ComponentId cId) = asChild ('#' : show cId)

instance EmbedAsChild CtrlV' ComponentPath where
    asChild (ComponentPath cPath) = asChild cPath

-- | This makes it easy to embed a timestamp into an HSP
-- template. 'show' provides way too much precision, so something
-- using formatTime would be better.
instance EmbedAsChild CtrlV' UTCTime where
    asChild time = asChild (show time)

------------------------------------------------------------------------------
-- Pages
------------------------------------------------------------------------------

-- | page handler for 'ViewRecent'
viewRecentPage :: Acid -> CtrlV Response
viewRecentPage acid =
    do method GET
       recent <- query (GetRecentComponents 20 0)
       case recent of
         [] -> appTemplate acid "Recent Components" () <p>There are no components yet.</p>
         _  -> appTemplate acid "Recent Components" () $
                <%>
                 <h1>Recent Components</h1>
                 <table>
                  <thead>
                   <tr>
                    <th>id</th>
                    <th>title</th>
                    <th>path</th>
                    <th>date</th>
                    <th>url</th>
                    <th>userid</th>
                    <th>view</th>
                    <th>delete?</th>
                   </tr>
                  </thead>
                  <tbody>
                   <% mapM mkTableRow recent %>
                  </tbody>
                 </table>
                </%>
    where
      mkTableRow Component{..} =
          <tr>
           <td><a href=(ViewComponentById componentId)><% componentId %></a></td>
           <td><a href=(ViewComponentById componentId)><% title       %></a></td>
           <td><a href=(ViewComponentByPath componentPath)><% componentPath %></a></td>
           <td><% added       %></td>
           <td><% unMyURL url         %></td>
           <td><% show $ unUserId userId %></td>
           <td><a href=(ViewPath componentPath)>View Path</a></td>
           <td><a href=(DeleteComponentPage componentId)>Delete</a></td>
          </tr>

-- | tiny helper function
cIdToText :: ComponentId -> Text
cIdToText cId = Text.pack $ show $ unComponentId cId

-- BEGIN dropbox stuff

-- Accessory function stolen from
-- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html
css :: HXT.ArrowXml a => String -> a HXT.XmlTree HXT.XmlTree
css tag = HXT.multi (HXT.hasName tag)

-- Accessory function stolen from
-- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html ,
-- with modifications to use http-conduit
-- 
-- FIXME: There's probably all *kinds* of error handling we're not
-- doing here; what does NHC.simpleHttp return on error?
webPageGet :: String -> IO (HXT.IOSArrow HXT.XmlTree (NTree HXT.XNode))
webPageGet pageUrl = do
  contents <- NHC.simpleHttp pageUrl
  return $ HXT.readString [HXT.withParseHTML HXT.yes, HXT.withWarnings HXT.no] (LC.unpack contents)

-- Takes a URL and returns a Just pair of the original and just the part between the last / and the extension, IFF the extension is .txt (otherwise Nothing)
fullFilePair :: String -> Maybe (String, String)
fullFilePair = [rex|^(?{ }.*/(?{ }[^/.]+)\.txt)$|]

-- Given a dropbox public URL, pull all the hrefs, turn them into
-- pairs of the URL and the file's basename
--
-- FIXME: note that the second bit here, which becomes the cfi_name,
-- is completely wrong; it should be extracted by parsing the file.
-- 
-- FIXME: There's probably all *kinds* of error handling we're not
-- doing here; what does webPageGet return on error?
dropBoxPathList :: MyURL -> IO [(String, String)]
dropBoxPathList pageUrl = do
  page <- liftIO $ webPageGet $ unMyURL pageUrl
  -- Most of this next line is from
  -- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html
  hrefs <- HXT.runX $ page
                  HXT.>>> css "a" 
                  HXT.>>> HXT.getAttrValue "href"
  return $ nub $ catMaybes $ map fullFilePair hrefs

-- | Given a component, pull the list page and cache the file
-- information therefrom.
--
-- FIXME: There's probably all *kinds* of error handling we're not
-- doing here; fix this when we fix dropBoxPathList and webPageGet 
dropBoxListPageToCFs :: Component -> CtrlV [(ComponentFileIndex, MyURL)]
dropBoxListPageToCFs component@Component{..} = do
  entries <- liftIO $ dropBoxPathList url
  now <- liftIO getCurrentTime

  -- If the cache has expired
  --
  -- FIXME: cache time should be in a config file
  if (diffUTCTime now refreshed) > 60 then
      do
        cId <- update (InsertComponent component { refreshed  = now })
        mapM makeStuff entries
    else
      do
        cfs <- query (GetCFsByCId componentId)
        return $ map findStuff cfs

  where
    findStuff :: ComponentFile -> (ComponentFileIndex, MyURL)
    findStuff cf@ComponentFile{..} = (cf_componentFileIndex, cf_url)

    makeStuff :: (String, String) -> CtrlV (ComponentFileIndex, MyURL)
    makeStuff (fileURL, fileName) = do
      let mycfi = ComponentFileIndex { cfi_componentPath = componentPath
                                     , cfi_name          = Text.pack fileName
                                     }
      cfi <- updateOrInsertCF $
        ComponentFile { cf_componentFileIndex = mycfi
          , cf_url            = MyURL fileURL
          , cf_componentId    = componentId
          }
      return (cfi, MyURL fileURL)

viewPath :: Acid -> ComponentPath -> CtrlV Response
viewPath acid cPath =
    do mComponent <- query (GetComponentByPath cPath)
       case mComponent of
         Nothing ->
             do notFound ()
                appTemplate acid "Path not found." () $
                    <p>Path <% cPath %> could not be found.</p>
         (Just component@Component{..}) ->
             do entries <- liftIO $ dropBoxPathList url
                entries2 <- dropBoxListPageToCFs component
                ok ()
                appTemplate acid ("Path " ++ (Text.unpack $ unComponentPath cPath)) () $
                  <div class="pathContents">
                    <p>Path <% cPath %> has url <% unMyURL url %></p>
                    <ul>
                    <% mapM (showEntry componentId) entries %>
                    </ul>
                  </div>
    where 
      showEntry cId entry = 
        <li>
         <a href=(ViewPathPage cId ((fst entry) ++ "?dl=1") (snd entry))><% snd entry %></a>
        </li>

renderPage :: EmbedAsChild m String => String -> XMLGenT m (XMLType m)
renderPage pageUrl =
  <p>some stuff</p>

viewPathPage :: Acid -> ComponentId -> String -> String -> CtrlV Response
viewPathPage acid cId pageUrl name =
  do mComponent <- query (GetComponentById cId)
     case mComponent of
        Nothing ->
          do notFound ()
             appTemplate acid "Id not found." () $
                <p>Component ID <% cId %> could not be found.</p>
        (Just component@Component{..}) ->
          do pageHtml <- renderPage pageUrl
             appTemplate acid ("Page " ++ name ++ " in section " ++ (Text.unpack $ unComponentPath componentPath)) () pageHtml



-- END dropbox stuff

viewComponentPageByPath :: Acid -> ComponentPath -> CtrlV Response
viewComponentPageByPath acid cPath =
    do mComponent <- query (GetComponentByPath cPath)
       viewComponentPage acid mComponent $ "Component path " <> (unComponentPath cPath) <> " could not be found."

viewComponentPageById :: Acid -> ComponentId -> CtrlV Response
viewComponentPageById acid cid =
    do mComponent <- query (GetComponentById cid)
       viewComponentPage acid mComponent $ "Component id " <> (cIdToText cid) <> " could not be found."

-- | page handler for 'ViewComponent'
viewComponentPage :: Acid -> Maybe Component -> Text -> CtrlV Response
viewComponentPage acid mComponent failed =
    do method GET
       case mComponent of
         Nothing ->
             do notFound ()
                appTemplate acid "Component not found." () $
                    <p><% failed %></p>
         (Just component@Component{..}) ->
             do ok ()
                appTemplate acid ("Component " ++ (Text.unpack $ cIdToText componentId)) () $
                    <div class="component">
                     <dl class="component-header">
                      <dt>Component:</dt><dd><a href=(ViewComponentById componentId)><% componentId %></a></dd>
                      <dt>Title:</dt><dd><% title %></dd>
                      <dt>Path:</dt><dd><% componentPath %></dd>
                      <dt>UserId:</dt><dd><% show $ unUserId userId %></dd>
                      <dt>URL:</dt><dd><% unMyURL url %></dd>
                     </dl>
                    </div>

-- | convert the paste to HTML. We currently only support 'PlainText',
-- but eventually it might do syntax hightlighting, markdown, etc.
--
-- Note that we do not have to worry about escaping the txt
-- value.. that is done automatically by HSP.
formatComponent :: Format -> Text -> CtrlV XML
formatComponent PlainText txt =
    <pre><% txt %></pre>

deleteComponentPage :: Acid -> ComponentId -> CtrlV Response
deleteComponentPage acid@Acid{..} cId =
    do mUserId <- getUserId acidAuth acidProfile
       case mUserId of
          Nothing ->
            appTemplate acid "Delete a Component" () $
              <%>
                <h1>You Are Not Logged In</h1>
              </%>
          (Just uid) ->
            -- FIXME: Make sure the uid matches!
            if unComponentId cId > 0 then
              deleteC cId
            else
              appTemplate acid "Delete a Component" () $
                <%>
                  <h1>Invalid Component ID <% cId %></h1>
                </%>
    where
      deleteC :: ComponentId -> CtrlV Response
      deleteC toDeleteId =
          -- FIXME: Do something if the retval is false, I guess?
          -- Shouldn't ever happen, though.
          do retval <- update (DeleteComponent toDeleteId)
             seeOtherURL ViewRecent

-- | page handler for 'NewComponent'
newComponentPage :: Acid -> CtrlV Response
newComponentPage acid@Acid{..} =
    do here <- whereami
       mUserId <- getUserId acidAuth acidProfile
       case mUserId of
          Nothing ->
            appTemplate acid "Add a Component" () $
              <%>
                <h1>You Are Not Logged In</h1>
              </%>
          (Just uid) ->
            appTemplate acid "Add a Component" () $
              <%>
                <h1>Add a component</h1>
                <% reform (form here) "add" success Nothing (componentForm uid) %>
              </%>
    where
      success :: Component -> CtrlV Response
      success component =
          do cId <- update (InsertComponent component)
             seeOtherURL (ViewComponentById cId)

-- | the 'Form' used for entering a new paste
componentForm :: UserId -> CtrlVForm Component
componentForm userId =
    (fieldset $
       ul $
          (,,,) <$> (li $ label <span>title</span>  ++> (inputText "" `transformEither` required) <++ errorList)
                <*> (li $ label <span>path</span>   ++> (inputText "" `transformEither` required) <++ errorList)
                <*> (li $ label <span>format</span> ++> formatForm)
                <*> (li $ label <span>url</span>    ++> errorList ++> (inputText "http://" `transformEither` required))
                <* inputSubmit "add component"
    )  `transformEitherM` toComponent
    where
      formatForm =
          select [(a, show a) | a <- [minBound .. maxBound]] (== PlainText)
      toComponent (ttl, cPath, fmt, url) =
          do now <- liftIO getCurrentTime
             return $ Right $
                        (Component { componentId     = ComponentId 0
                                   , title           = ttl
                                   , componentPath   = ComponentPath cPath
                                   , added           = now
                                   , userId          = userId
                                   , url             = MyURL $ Text.unpack url
                                   , refreshed       = now
                                   })
      required txt
          | Text.null txt = Left "Required"
          | otherwise     = Right txt

------------------------------------------------------------------------------
-- Auth Support Functions
------------------------------------------------------------------------------

-- the key to using happstack-authenticate with HSP is simple. First
-- you need to be able to embed Html in your HSP monad like this:
--
-- I (Robin Lee Powell) don't know how to fix this warning:
--
--    Warning: orphan instance:
--      instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Html
--
instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Html where
    asChild html = asChild (CDATA False (renderHtml html))

-- If you want to use your usual app template, then the auth stuff,
-- which is in RouteT AuthProfileURL, needs to have a way to render
-- your routes inside that context.
-- 
instance (Functor m, Monad m) => EmbedAsAttr (RouteT AuthProfileURL m) (Attr String Route) where
    asAttr (n := u) = do
           asAttr $ MkAttr (toName n, pAttrVal (concatMap (((++) "/") . Text.unpack) (toPathSegments u)))

-- Stick our usual app template into a state where the auth
-- functions, which are Blaze based, are OK with it.
appTemplate' :: (Happstack m, EmbedAsAttr m (Attr String Route), XMLGenerator m, EmbedAsChild m Html, HSX.XMLType m ~ XML) => Acid -> String -> Html -> Html -> m Response
appTemplate' a t h b = liftM toResponse (baseAppTemplate a t h b)

------------------------------------------------------------------------------
-- route
------------------------------------------------------------------------------


-- | the route mapping function
route :: Acid -> Text -> Route -> CtrlV Response
route acid@Acid{..} baseURL url =
    case url of
      -- FIXME: replace the ViewRecent thing here with "go back to
      -- the last page we were on". - rlpowell
      (U_AuthProfile authProfileURL) ->
          do vr <- showURL ViewRecent
             XMLGenT $ nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile (appTemplate' acid) Nothing (Just baseURL) vr authProfileURL
      ViewRecent      -> viewRecentPage acid
      (ViewComponentById cId) -> viewComponentPageById acid cId
      (ViewComponentByPath cPath) -> viewComponentPageByPath acid cPath
      (ViewPath cPath) -> viewPath acid cPath
      (ViewPathPage cId pageUrl name) -> viewPathPage acid cId pageUrl name
      NewComponent        -> newComponentPage acid
      CSS             -> serveFile (asContentType "text/css") "style.css"
      (DeleteComponentPage cid) -> deleteComponentPage acid cid

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

-- | start the app. listens on port 8000.
--
-- FIXME: the port and base URL could/should be in a config file or
-- command line arguments or something.
--
-- NB: This requires a tweak to simpleApp of version 0.1.0 to take
-- the base URI
main :: IO ()
main =  withAcid Nothing $ \acid ->
        do
           simpleApp id
              Conf { port      = 8080
                   , validator  = Nothing
                   , logAccess = Just logMAccess
                   , timeout = 30
                   }
              (AcidLocal Nothing initialCtrlVState)
              ()
              ViewRecent
              "http://vrici.lojban.org:8080"
              (route acid "http://vrici.lojban.org:8080")

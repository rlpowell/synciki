{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies, TypeSynonymInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -cpp -pgmPcpphs -optP--cpp -optP--hashes #-}
module Main where

import Happstack.Foundation
import qualified Data.IxSet as IxSet
import Data.IxSet (IxSet, Indexable, Proxy(..), getEQ, getOne, ixSet, ixFun, ixGen)
import Data.Text  (Text)
import Data.Text.Encoding as DTE
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe
import HSP
import qualified Data.Map as Map
import Data.Char

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
import Data.Acid.Advanced          (query')
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
import qualified Data.ByteString as BS
import Text.RegexPR
import Data.List

-- Debugging
import Text.Show.Pretty
import Debug.Trace

-- | Whether the package was built with debugging settings.
debugMode :: Bool
#ifdef SYNCIKI_DEBUG
debugMode = True
#else
debugMode = False
#endif

debugTrace :: String -> a -> a
debugTrace str stuff =
  if debugMode then
    trace str stuff
  else
    stuff

debugTraceSelfShow :: (Show a) => String -> a -> a
debugTraceSelfShow str stuff =
  if debugMode then
    trace (str ++ (show stuff)) stuff
  else
    stuff

------------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------------

-- The basic idea is to pull pages from the web that are in some
-- sort of markup, and present them to the client in a coherent
-- manner in a common location.  So, a client might see
-- [host]/bob/widgets and [host]/bob/nasa , and those pages might
-- link to each other and so on, even though /bob/widgets is from
-- Bob's google drive and /bob/nasa is from bob's old blog.
--
-- The data model outline:
--
-- (synciki) users have (synciki) paths and (page) sources.
--
-- Paths have (page) sources that generate a list of pages.
--
-- (page) sources have one or more pages, depending on type;
-- public/private DropBox directory, public/private google
-- directory, single URL to a page, apache dir listings, etc?
--
-- Pages have a title (which is turned into a name slug), tags,
-- other metadata?, and content.
--
-- Outside users (which we'll call "clients") see particular content
-- as at [host]/path/name
--
-- Terminology: someone using the system as a normal user, who may
-- or may not have an account with synciki or anything but is
-- accessing the pages at [host]/[path]/[name], is a client.  A
-- person with a synciki account that manages their paths and
-- sources so that those URLs exist is a user.

newtype SourceId = SourceId { unSourceId :: Integer }
    deriving (Eq, Ord, Read, Show, Enum, Data, Typeable, SafeCopy)
$(derivePathInfo ''SourceId)

newtype PathId = PathId { unPathId :: Integer }
    deriving (Eq, Ord, Read, Show, Enum, Data, Typeable, SafeCopy)
$(derivePathInfo ''PathId)

newtype PageId = PageId { unPageId :: Integer }
    deriving (Eq, Ord, Read, Show, Enum, Data, Typeable, SafeCopy)
$(derivePathInfo ''PageId)

-- The type of source, and hence how pages should be found via that
-- source.
data SourceType
    = DropBoxIndex
    | GoogleDriveIndex
    -- FIXME: Others
      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)
$(deriveSafeCopy 0 'base ''SourceType)

-- The markup used in the pages under this source
data Format
    = PlainText
    | Pandoc
      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable)
$(deriveSafeCopy 0 'base ''Format)

-- Just a String type for URLs.  Might be replaced later if we need
-- more processing complexity in the type itself (see Network.URL
-- for example).
newtype MyURL = MyURL { unMyURL :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

-- String type needed for indexing
newtype PathSlug = PathSlug { unPathSlug :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
$(derivePathInfo ''PathSlug)
newtype PathHost = PathHost { unPathHost :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
$(derivePathInfo ''PathHost)

-- String type needed for indexing
newtype PageSlug = PageSlug { unPageSlug :: String }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
$(derivePathInfo ''PageSlug)


data Path = Path
  { pathId       :: PathId
  , pathSlug     :: PathSlug   -- UNIQUE in combination with host. FIXME: unenforced.
                               -- FIXME: Needs validation.
  , pathHost     :: PathHost   -- UNIQUE in combination with slug. FIXME: unenforced.
                               -- Currently unused, but would allow virtualhost sort of stuff
  , pathUserId   :: UserId
  , pathSources  :: [SourceId] -- A small list; 5 elements would be large
  , pathAdded    :: UTCTime
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Path)


instance Indexable Path where
    empty =
        ixSet [ ixGen (Proxy :: Proxy PathId)
              , ixFun $ (:[]) . pathSlug
              , ixFun $ (:[]) . pathUserId
              , ixFun $ (:[]) . pathHost
              ]

data Source = Source
  { sourceId        :: SourceId
  , sourceURL       :: MyURL       -- FIXME: Needs validation
  , sourceUserId    :: UserId
  , sourceType      :: SourceType
  , sourceFormat    :: Format
  , sourceHome      :: PageSlug    -- FIXME: make a form that allows selection of this from the cached pages
  , sourceRefreshed :: UTCTime
  , sourceAdded     :: UTCTime
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Source)

instance Indexable Source where
    empty =
        ixSet [ ixGen (Proxy :: Proxy SourceId)
              , ixFun $ (:[]) . sourceUserId
              ]

-- Each Source is trolled for a list of files it contains, and then
-- various data about them is cached in the Page structure.
data Page = Page
  { pageId        :: PageId
  , pageSourceId  :: SourceId   -- UNIQUE IN COMBINATION WITH slug. FIXME: unenforced.
  , pageUserId    :: UserId
  , pageURL       :: MyURL
  , pageTitle     :: String
  , pageFormat    :: Format     -- Duplicated from the source
  , pageSlug      :: PageSlug   -- UNIQUE IN COMBINATION WITH sourceid. FIXME: unenforced.
                                -- FIXME: Needs creation
  , pageTags      :: [String]   -- FIXME: Needs validation; perhaps on source reload or admin view?
  }
  deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Page)

-- FIXME: One more uniqueness constraint needed: All of the page
-- slugs in all of the sources referenced by a given path should be
-- unique. This should be checked when a source is attached to a
-- path.

instance Indexable Page where
    empty =
        ixSet [ ixGen (Proxy :: Proxy PageId)
              , ixFun $ (:[]) . pageSlug
              , ixFun $ (:[]) . pageSourceId
              , ixFun $ (:[]) . pageUserId
              , ixFun $ (:[]) . pageURL
              ]


-- | record to store in acid-state
data CtrlVState = CtrlVState
    { paths        :: IxSet Path
    , nextPathId   :: PathId
    , sources      :: IxSet Source
    , nextSourceId :: SourceId
    , pages        :: IxSet Page
    , nextPageId   :: PageId
    }
    deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''CtrlVState)

-- | initial value to use with acid-state when no prior state is found
initialCtrlVState :: CtrlVState
initialCtrlVState =
    CtrlVState { paths        = IxSet.empty
               , nextPathId   = PathId 1
               , sources      = IxSet.empty
               , nextSourceId = SourceId 1
               , pages        = IxSet.empty
               , nextPageId   = PageId 1
               }

------------------------------------------------------------------------------
-- Acid-State events
------------------------------------------------------------------------------

-- Note that IxSet's updateIx also inserts, given a primary key; it
-- works by deleting and then inserting.

-- Path Events
getPath :: PathId -> Query CtrlVState (Maybe Path)
getPath pid = getOne . getEQ pid . paths <$> ask

getPathsByUserId :: UserId -> Query CtrlVState [Path]
getPathsByUserId userid = IxSet.toList . getEQ userid . paths <$> ask

getPathByHostAndSlug :: PathHost -> PathSlug -> Query CtrlVState (Maybe Path)
getPathByHostAndSlug phost pslug = getOne . getEQ phost . getEQ pslug . paths <$> ask

-- Returns the *old* nextPathId
incrementPathId :: Update CtrlVState PathId
incrementPathId = do
    cvs@CtrlVState{..} <- get
    let pid = nextPathId
    put $ cvs { nextPathId = succ nextPathId }
    return pid

updatePath :: Path -> Update CtrlVState ()
updatePath upath@Path{..} = do
    cvs@CtrlVState{..} <- get
    put $ cvs { paths = IxSet.updateIx pathId upath paths }
    return ()

deletePath :: PathId -> Update CtrlVState ()
deletePath pid = do
    cvs@CtrlVState{..} <- get
    put $ cvs { paths = IxSet.deleteIx pid paths }
    return ()

-- Source Events
getSource :: SourceId -> Query CtrlVState (Maybe Source)
getSource sid = getOne . getEQ sid . sources <$> ask

getSources :: [SourceId] -> Query CtrlVState [Source]
getSources sids = do
  cstate <- ask
  return $ IxSet.toList $ (sources cstate) IxSet.@+ sids

getSourceByURL :: MyURL -> Query CtrlVState (Maybe Source)
getSourceByURL url = getOne . getEQ url . sources <$> ask

getSourcesByUserId :: UserId -> Query CtrlVState [Source]
getSourcesByUserId userid = IxSet.toList . getEQ userid . sources <$> ask

-- Returns the *old* nextSourceId
incrementSourceId :: Update CtrlVState SourceId
incrementSourceId = do
    cvs@CtrlVState{..} <- get
    let sid = nextSourceId
    put $ cvs { nextSourceId = succ nextSourceId }
    return sid

updateSource :: Source -> Update CtrlVState ()
updateSource source@Source{..} = do
    cvs@CtrlVState{..} <- get
    put $ cvs { sources = IxSet.updateIx sourceId source sources }
    return ()

deleteSource :: SourceId -> Update CtrlVState ()
deleteSource sid = do
    cvs@CtrlVState{..} <- get
    put $ cvs { sources = IxSet.deleteIx sid sources }
    return ()

-- Page Events
getPage :: PageId -> Query CtrlVState (Maybe Page)
getPage pid = getOne . getEQ pid . pages <$> ask

-- NTS: Other ways to say it the above, for general Haskell
-- learning:
-- 
-- getPage pid = fmap (getOne . getEQ pid . pages) ask
--
-- getPage pid = fmap (\x -> getOne ( getEQ pid ( pages x))) ask
--
-- getPage pid = do
--   state <- ask
--   return $ getOne . getEQ pid . pages $ state
--
-- getPage pid = do
--   state <- ask
--   return (getOne (getEQ pid (pages state)))

getPagesBySourceId :: SourceId -> Query CtrlVState [Page]
getPagesBySourceId sid = IxSet.toList . getEQ sid . pages <$> ask

getPagesBySourceIds :: [SourceId] -> Query CtrlVState [Page]
getPagesBySourceIds sids = do
  cstate <- ask
  return $ IxSet.toList $ (pages cstate) IxSet.@+ sids

getPageBySourceIdsAndSlug :: [SourceId] -> PageSlug -> Query CtrlVState (Maybe Page)
getPageBySourceIdsAndSlug sids slug = do
  cstate <- ask
  return $ getOne $ (pages cstate) IxSet.@+ sids IxSet.@= slug

getPageBySourceIdAndSlugAndURL :: SourceId -> PageSlug -> MyURL -> Query CtrlVState (Maybe Page)
getPageBySourceIdAndSlugAndURL sid slug url = getOne . getEQ url . getEQ slug . getEQ sid . pages <$> ask

getPagesByUserId :: UserId -> Query CtrlVState [Page]
getPagesByUserId userid = IxSet.toList . getEQ userid . pages <$> ask

getPagesBySlug :: PageSlug -> Query CtrlVState [Page]
getPagesBySlug pageSlug = IxSet.toList . getEQ pageSlug . pages <$> ask

-- Returns the *old* nextPageId
incrementPageId :: Update CtrlVState PageId
incrementPageId = do
    cvs@CtrlVState{..} <- get
    let pid = nextPageId
    put $ cvs { nextPageId = succ nextPageId }
    return pid

updatePage :: Page -> Update CtrlVState ()
updatePage page@Page{..} = do
    cvs@CtrlVState{..} <- get
    put $ cvs { pages = IxSet.updateIx pageId page pages }
    return ()

deletePage :: PageId -> Update CtrlVState ()
deletePage pid = do
    cvs@CtrlVState{..} <- get
    put $ cvs { pages = IxSet.deleteIx pid pages }
    return ()

-- | now we need to tell acid-state which functions should be turn into
-- acid-state events.
$(makeAcidic ''CtrlVState
   [ 'getPath
   , 'getPathsByUserId
   , 'getPathByHostAndSlug
   , 'incrementPathId
   , 'updatePath
   , 'deletePath
   , 'getSource
   , 'getSources
   , 'getSourceByURL
   , 'getSourcesByUserId
   , 'incrementSourceId
   , 'updateSource
   , 'deleteSource
   , 'getPage
   , 'getPagesBySourceId
   , 'getPagesBySourceIds
   , 'getPagesByUserId
   , 'getPagesBySlug
   , 'getPageBySourceIdsAndSlug 
   , 'getPageBySourceIdAndSlugAndURL
   , 'incrementPageId
   , 'updatePage
   , 'deletePage
   ])

------------------------------------------------------------------------------
-- Route Type
------------------------------------------------------------------------------

-- | All the routes for our web application
data Route
    = AdminViewAll
    | AdminViewPath PathId
    | AdminViewSource SourceId
    | AdminViewPage PageId
    | AdminNewPath
    | AdminNewSource
    | AdminEditPath PathId
    | AdminEditSource SourceId
    | AdminDeletePath PathId
    | AdminDeleteSource SourceId
    | ViewPage PathHost PathSlug PageSlug
    | ViewPath PathHost PathSlug
    | CSS
    | U_AuthProfile AuthProfileURL
      deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | we will just use template haskell to derive the route mapping
$(derivePathInfo ''Route)

------------------------------------------------------------------------------
-- route Dispatch Function
------------------------------------------------------------------------------

-- | the route mapping function
route :: Acid -> Text -> Route -> CtrlV Response
route acid@Acid{..} baseURL url =
    case url of
      AdminViewAll                       -> adminViewAll acid
      (AdminViewPath pid)                -> adminViewPath acid pid
      (AdminViewSource sid)              -> adminViewSource acid sid
      (AdminViewPage pid)                -> adminViewPage acid pid
      AdminNewPath                       -> adminNewPath acid
      AdminNewSource                     -> adminNewSource acid
      (AdminEditPath pid)                -> adminEditPath acid pid
      (AdminEditSource sid)              -> adminEditSource acid sid
      (AdminDeletePath pid)              -> adminDeletePath acid pid
      (AdminDeleteSource sid)            -> adminDeleteSource acid sid
      (ViewPage phost pathSlug pageSlug) -> viewPage acid phost pathSlug pageSlug
      (ViewPath phost pathSlug)          -> viewPath acid phost pathSlug
      CSS                                -> serveFile (asContentType "text/css") "style.css"
      -- FIXME: replace the AdminViewAll thing here with "go back to
      -- the last page we were on". - rlpowell
      (U_AuthProfile authProfileURL)     -> do
          vr <- showURL AdminViewAll
          XMLGenT $ nestURL U_AuthProfile $ handleAuthProfile acidAuth acidProfile (appTemplate' acid) Nothing (Just baseURL) vr authProfileURL

------------------------------------------------------------------------------
-- CtrlV type-aliases
------------------------------------------------------------------------------

-- | The foundation types are heavily parameterized -- but for our app
-- we can pin all the type parameters down.
type CtrlV'    = FoundationT' Route CtrlVState () IO
type CtrlV     = XMLGenT CtrlV'
type CtrlVForm = FoundationForm Route CtrlVState () IO

------------------------------------------------------------------------------
-- Composite Data Functions
------------------------------------------------------------------------------
-- Stuff that operates on acid data by running its own queries.

-- updateIx already does this
--
-- -- | "Do the thing".
-- updateOrInsertCF :: ComponentFile -> CtrlV ComponentFileIndex
-- updateOrInsertCF cf@ComponentFile{cfIndex = cfi, ..} =
--   do
--     mCF <- query (GetCFByCFI cfi)
--     case mCF of
--       Nothing -> do update (InsertCF cf)
--       (Just dbcf) -> do update (UpdateCF cf)

findPage :: PathHost -> PathSlug -> PageSlug -> CtrlV (Maybe Page)
findPage pathHostIn pathSlugIn pageSlugIn = do
  mPath <- query (GetPathByHostAndSlug pathHostIn pathSlugIn)
  case mPath of
    Nothing     -> return Nothing
    (Just Path{..}) -> do
      mPage <- query (GetPageBySourceIdsAndSlug pathSources pageSlugIn)
      return mPage

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
baseAppTemplate Acid{..} ttl moreHdrs bdy =
  HTML.defaultTemplate ttl <%><link rel="stylesheet" href=CSS type="text/css" media="screen" /><% moreHdrs %></%> $
    <%>
      <div id="logo">^V</div>
      <ul class="menu">
        <li><a href=AdminViewAll>Admin View</a></li>
        <li><a href=AdminNewPath>Add Path</a></li>
        <li><a href=AdminNewSource>Add Source</a></li>
      </ul>
      <% do
        mUserId <- getUserId acidAuth acidProfile
        authState <- query' acidAuth AskAuthState
        let authDump = debugTraceSelfShow "authState: " $ ppDoc authState
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

-- | Make for easy embedding of custom types into HSP templates.
instance EmbedAsChild CtrlV' PathId where
    asChild (PathId pId) = asChild ('#' : show pId)

instance EmbedAsChild CtrlV' SourceId where
    asChild (SourceId sId) = asChild ('#' : show sId)

instance EmbedAsChild CtrlV' PageId where
    asChild (PageId pId) = asChild ('#' : show pId)

instance EmbedAsChild CtrlV' UserId where
    asChild (UserId uId) = asChild ('#' : show uId)

instance EmbedAsChild CtrlV' PageSlug where
    asChild (PageSlug slug) = asChild slug

instance EmbedAsChild CtrlV' PathSlug where
    asChild (PathSlug slug) = asChild slug

instance EmbedAsChild CtrlV' PathHost where
    asChild (PathHost phost) = asChild phost

instance EmbedAsChild CtrlV' MyURL where
    asChild (MyURL url) = asChild url

instance EmbedAsChild CtrlV' SourceType where
    asChild DropBoxIndex = asChild ("DropBox Index/Directory Page" :: String)
    asChild GoogleDriveIndex = asChild ("Google Drive Index/Directory Page" :: String)

-- FIXME: should show something more user friendly here
instance EmbedAsChild CtrlV' Format where
    asChild PlainText = asChild ("Plain/Unformatted Text" :: String)
    asChild Pandoc = asChild ("Pandoc (Enhanced Markdown)" :: String)

-- | This makes it easy to embed a timestamp into an HSP
-- template. 'show' provides way too much precision, so something
-- using formatTime would be better.
instance EmbedAsChild CtrlV' UTCTime where
    asChild time = asChild (show time)

------------------------------------------------------------------------------
-- Pages
------------------------------------------------------------------------------

ifLoggedIn :: (Happstack m) => Acid -> m a -> (UserId -> m a) -> m a
ifLoggedIn Acid{..} no yes = do
  mUserId <- getUserId acidAuth acidProfile
  case mUserId of
    Nothing -> do
      no
    (Just uid) -> do
      (yes uid)

ifLoggedInResponse :: Acid -> String -> GenXML CtrlV' -> (UserId -> GenXML CtrlV') -> CtrlV Response
ifLoggedInResponse acid@Acid{..} title no yes = do
  ifLoggedIn acid
    (appTemplate acid title () $ do 
      no)
    $ \uid -> appTemplate acid title () $ do
        (yes uid)

pathHeader :: [GenXML CtrlV']
pathHeader =
  [ <div class="path-header-slug">Slug</div>
  , <div class="path-header-host">Host</div>
  , <div class="path-header-added">Date Added</div>
  , <div class="path-header-admin-view">Admin View</div>
  , <div class="path-header-client-view">Client View</div>
  , <div class="path-header-edit">Edit</div>
  , <div class="path-header-delete">Delete</div>
  ]

pathBody :: Path -> [GenXML CtrlV']
pathBody Path{..} =
  [ <div class="path-body-slug"><% pathSlug       %></div>
  , <div class="path-body-host"><% pathHost       %></div>
  , <div class="path-body-added"><% pathAdded %></div>
  , <div class="path-body-admin-view"><a href=(AdminViewPath pathId)>Admin View</a></div>
  , <div class="path-body-client-view"><a href=(ViewPath pathHost pathSlug)>Client View</a></div>
  , <div class="path-body-edit"><a href=(AdminEditPath pathId)>Edit</a></div>
  , <div class="path-body-delete"><a href=(AdminDeletePath pathId)>Delete</a></div>
  ]

sourceHeader :: [GenXML CtrlV']
sourceHeader =
  [ <div class="source-header-url">URL</div>
  , <div class="source-header-type">Type</div>
  , <div class="source-header-format">Format</div>
  , <div class="source-header-refreshed">Date Last Refreshed</div>
  , <div class="source-header-added">Date Added</div>
  , <div class="source-header-view">Admin View</div>
  , <div class="source-header-refresh">Refresh</div>
  , <div class="source-header-edit">Edit</div>
  , <div class="source-header-delete">Delete</div>
  ]

sourceBody :: Source -> [GenXML CtrlV']
sourceBody Source{..} =
  [ <div class="source-body-url"><% sourceURL       %></div>
  , <div class="source-body-type"><% sourceType       %></div>
  , <div class="source-body-format"><% sourceFormat       %></div>
  , <div class="source-body-refreshed"><% sourceRefreshed       %></div>
  , <div class="source-body-added"><% sourceAdded %></div>
  , <div class="source-body-admin view"><a href=(AdminViewSource sourceId)>Admin View</a></div>
  , <div class="source-body-refresh"><a href=(AdminViewSource sourceId)>Refresh</a></div>
  , <div class="source-body-edit"><a href=(AdminEditSource sourceId)>Edit</a></div>
  , <div class="source-body-delete"><a href=(AdminDeleteSource sourceId)>Delete</a></div>
  ]

pageHeader :: [GenXML CtrlV']
pageHeader =
  [ <div class="page-header-url">URL</div>
  , <div class="page-header-source">Source</div>
  , <div class="page-header-slug">Slug</div>
  , <div class="page-header-title">Title</div>
  , <div class="page-header-format">Format</div>
  , <div class="page-header-tags">Tags</div>
  , <div class="page-header-admin-view">Admin View</div>
  , <div class="page-header-refresh">Refresh</div>
  ]

pageBody :: Page -> [GenXML CtrlV']
pageBody Page{..} =
  [ <div class="page-body-url"><% pageURL       %></div>
  , <div class="page-body-source"><a href=(AdminViewSource pageSourceId)>View Source</a></div>
  , <div class="page-body-slug"><% pageSlug       %></div>
  , <div class="page-body-title"><% pageTitle       %></div>
  , <div class="page-body-format"><% pageFormat       %></div>
  , <div class="page-body-tags"><% intercalate ", " pageTags       %></div>
  , <div class="page-body-admin-view"><a href=(AdminViewPage pageId)>Admin View</a></div>
  , <div class="page-body-refresh"><a href=(AdminViewSource pageSourceId)>Refresh</a></div>
  ]

pathPageHeader :: [GenXML CtrlV']
pathPageHeader =
  [ <div class="page-header-url">URL</div>
  , <div class="page-header-source">Source</div>
  , <div class="page-header-slug">Slug</div>
  , <div class="page-header-title">Title</div>
  , <div class="page-header-format">Format</div>
  , <div class="page-header-tags">Tags</div>
  , <div class="page-header-admin-view">Admin View</div>
  , <div class="page-header-client-view">Client View</div>
  , <div class="page-header-refresh">Refresh</div>
  ]

pathPageBody :: Path -> Page -> [GenXML CtrlV']
pathPageBody Path{..} Page{..} =
  [ <div class="page-body-url"><% pageURL       %></div>
  , <div class="page-body-source"><a href=(AdminViewSource pageSourceId)>View Source</a></div>
  , <div class="page-body-slug"><% pageSlug       %></div>
  , <div class="page-body-title"><% pageTitle       %></div>
  , <div class="page-body-format"><% pageFormat       %></div>
  , <div class="page-body-tags"><% intercalate ", " pageTags       %></div>
  , <div class="page-body-admin-view"><a href=(AdminViewPage pageId)>Admin View</a></div>
  , <div class="page-body-client-view"><a href=(ViewPage pathHost pathSlug pageSlug)>Client View</a></div>
  , <div class="page-body-refresh"><a href=(AdminViewSource pageSourceId)>Refresh</a></div>
  ]

makeTable :: GenXML CtrlV' -> [GenXML CtrlV'] -> [a] -> (a -> [GenXML CtrlV']) -> GenXML CtrlV'
makeTable ifNotFound header thingies thingyConverter =
  case thingies of
     [] -> ifNotFound
     _  -> <table>
            <thead>
              <tr>
                <% mapM mkHeader header %>
              </tr>
            </thead>
            <tbody>
              <% mapM mkTableRow thingies %>
            </tbody>
           </table>
  where
    mkHeader item = <th><% item %></th>
    mkTableEntry item = <td><% item %></td>
    mkTableRow tpath =
        <tr>
          <% mapM mkTableEntry $ thingyConverter tpath %>
        </tr>

pathTable :: Acid -> GenXML CtrlV'
pathTable acid = do
  ifLoggedIn acid (<p>You Are Not Logged In</p>) $ \uid -> do
    paths <- query (GetPathsByUserId uid)
    makeTable (<p>There are no paths yet.</p>) pathHeader paths pathBody

sourceTable :: Acid -> GenXML CtrlV'
sourceTable acid = do
  ifLoggedIn acid (<p>You Are Not Logged In</p>) $ \uid -> do
    sources <- query (GetSourcesByUserId uid)
    makeTable (<p>There are no sources yet.</p>) sourceHeader sources sourceBody

pageTable :: Acid -> [Page] -> GenXML CtrlV'
pageTable acid pages = do
  ifLoggedIn acid (<p>You Are Not Logged In</p>) $ \uid -> do
    makeTable (<p>There are no pages yet.</p>) pageHeader pages pageBody

pathPageTable :: Acid -> Path -> GenXML CtrlV'
pathPageTable acid mypath@Path{..} = do
  ifLoggedIn acid (<p>You Are Not Logged In</p>) $ \_ -> do
    pages <- query (GetPagesBySourceIds pathSources)
    makeTable (<p>There are no pages yet.</p>) pathPageHeader pages (pathPageBody mypath)

adminViewAll :: Acid -> CtrlV Response
adminViewAll acid@Acid{..} = do
  ifLoggedInResponse acid "View All" <h1>You Are Not Logged In</h1> $ \uid -> do
    pages <- query (GetPagesByUserId uid)
    <div class="view-all-content">
      <h1>Your Paths</h1>
      <% pathTable acid %>
      <h1>Your Sources</h1>
      <% sourceTable acid %>
      <h1>Your Pages</h1>
      <% pageTable acid pages %>
    </div>

makeDL :: [GenXML CtrlV'] -> a -> (a -> [GenXML CtrlV']) -> GenXML CtrlV'
makeDL header thingy thingyConverter =
  <div class="dl-body">
      <dl>
        <% mapM mkBoth $ zip header $ thingyConverter thingy %>
      </dl>
  </div>
  where
    mkBoth (headbit, item) = <div class="dl-row"><dt><% headbit %></dt><dd><% item %></dd></div>

ifItemOK :: (Happstack m) => Maybe a -> (a -> UserId) -> UserId -> m b -> (a -> m b) -> (a -> m b) -> m b
ifItemOK mItem itemToUserId uid ifNotFound ifNotOwned ifOK =
    case mItem of
      Nothing -> do
        notFound ()
        ifNotFound
      (Just item) -> do 
        if (itemToUserId item) == uid then do
            ok ()
            (ifOK item)
          else
            (ifNotOwned item)

adminViewPath :: Acid -> PathId -> CtrlV Response
adminViewPath acid pid = do
  ifLoggedInResponse acid "View Path" <h1>You Are Not Logged In</h1> $ \uid -> do
    mPath <- query (GetPath pid)
    ifItemOK mPath pathUserId uid
      (<p>Path id <% pid %> could not be found.</p>)
      (\ipath -> <p>Path <% pathSlug ipath %>/<% pid %> is not owned by you.</p>)
      (\ipath -> do
        sources <- query (GetSources $ pathSources ipath)
        pages <- query (GetPagesBySourceIds (map sourceId sources))
        let collisions = getPageCollisions pages
        <div>
          <h1>Path</h1>
          <% makeDL pathHeader ipath pathBody %>
          <h1>Path's Sources</h1>
          <% makeTable (<p>There are no sources associated with this path.</p>) sourceHeader sources sourceBody %>
          <% if collisions == [] then
                <% () %>
              else <%
                <div>
                  <h1>Page Collisions</h1>
                  <p>FIXME: explain</p>
                  <dl>
                  <% map presentPageCollisions collisions %>
                  </dl>
                </div>
              %>
             %>
          <h1>Path's Pages</h1>
          <% pathPageTable acid ipath %>
        </div>
        )

presentPageCollisions :: (PageSlug, [MyURL]) -> GenXML CtrlV'
presentPageCollisions (slug, urls) =
  <div class="collision">
    <dt><% slug %></dt>
    <dd>
      <ul>
        <% mapM urlStuff urls %>
      </ul>
    </dd>
  </div>

  where
    urlStuff url = <li><% url %></li>


getPageCollisions :: [Page] -> [(PageSlug, [MyURL])]
getPageCollisions pages =
  -- NOTE: Very inefficient.  Unlikely to matter, though, in
  -- practice.
  --
  -- Walk the list and for each page slug, walk the list *again*
  -- finding all identical ones' URLs, then throw away the
  -- singletons.
  nub $ filter interesting $ map findMatching pages
  where
    -- List has at least 2 elements (plus [])
    interesting :: (PageSlug, [MyURL]) -> Bool
    interesting (a, (_:_:_)) = True
    interesting _ = False

    findMatching :: Page -> (PageSlug, [MyURL])
    findMatching page = ((pageSlug page), map pageURL $ filter (sameSlug (pageSlug page)) pages)

    sameSlug :: PageSlug -> Page -> Bool
    sameSlug slug page = slug == (pageSlug page)


-- FIXME: Needs to show pages
adminViewSource :: Acid -> SourceId -> CtrlV Response
adminViewSource acid sid = do
  ifLoggedInResponse acid "View Source" <h1>You Are Not Logged In</h1> $ \uid -> do
    mSource <- query (GetSource sid)
    ifItemOK mSource sourceUserId uid
      (<p>Source id <% sid %> could not be found.</p>)
      (\isource -> <p>Source <% sourceURL isource %>/<% sid %> is not owned by you.</p>)
      (\isource -> do
        _ <- refreshSource isource
        pages <- query (GetPagesBySourceId sid)
        let collisions = getPageCollisions pages
        <div>
          <h1>Source</h1>
          <% makeDL sourceHeader isource sourceBody %>
          <% if collisions == [] then
                <% () %>
              else <%
                <div>
                  <h1>Page Collisions</h1>
                  <p>FIXME: explain</p>
                  <dl>
                  <% map presentPageCollisions collisions %>
                  </dl>
                </div>
              %>
          %>
          <h1>Source's Pages</h1>
          <% pageTable acid pages %>
        </div>
        )

getURLContent :: MyURL -> CtrlV Text
getURLContent url = do
  urlContentRaw <- NHC.simpleHttp $ unMyURL $ url
  return $ DTE.decodeUtf8 $ BS.concat $ LC.toChunks urlContentRaw 

-- Note that we do not have to worry about escaping the txt
-- value, that is done automatically by HSP.
formatPage :: Format -> [Text] -> GenXML CtrlV'
formatPage PlainText txt =
    <pre>plain: <% Text.unlines txt %></pre>
formatPage Pandoc txt =
    <pre>pandoc: <% Text.unlines txt %></pre>


adminViewPage :: Acid -> PageId -> CtrlV Response
adminViewPage acid pid = do
  ifLoggedInResponse acid "View Page" <h1>You Are Not Logged In</h1> $ \uid -> do
    mPage <- query (GetPage pid)
    ifItemOK mPage pageUserId uid
      (<p>Page id <% pid %> could not be found.</p>)
      (\ipage -> <p>Page <% pageSlug ipage %>/<% pid %> is not owned by you.</p>)
      (\ipage -> do
        content <- getURLContent (pageURL ipage)
        let (metadata, body) = pageContentsToMetadataAndBody content
        <div>
          <h1>Path</h1>
          <% makeDL pageHeader ipage pageBody %>
          <h1>Content</h1>
          <% formatPage (pageFormat ipage) body %>
        </div>)


-- | the 'Form' used for entering a new paste
pathForm :: UserId -> [Source] -> CtrlVForm Path
pathForm userId sources = do
    (fieldset $
       ul $
            (,,) <$> (li $ label <span>slug</span>  ++> (inputText "" `transformEither` required) <++ errorList)
                 <*> (li $ label <span>host</span>   ++> (inputText "" `transformEither` required) <++ errorList)
                 <*> (li $ label <span>sources</span> ++> sourcesForm <++ errorList)
                 <* inputSubmit "add path"
      )  `transformEitherM` toPath
    where
      sourcesForm = selectMultiple [(sourceId source, show $ unMyURL $ sourceURL source) | source <- sources] (\_ -> False)
      toPath (slug, phost, selSources) =
          do now <- liftIO getCurrentTime
             return $ Right $
                        (Path { pathId       = PathId 0
                              , pathSlug     = PathSlug $ Text.unpack slug
                              , pathHost     = PathHost $ Text.unpack phost
                              , pathUserId  = userId
                              , pathSources  = selSources
                              , pathAdded   = now
                              })
      required txt
          | Text.null txt = Left "Required"
          | otherwise     = Right txt

adminNewPath :: Acid -> CtrlV Response
adminNewPath acid@Acid{..} = do
    here <- whereami
    ifLoggedInResponse acid "Add A Path" <h1>You Are Not Logged In</h1> $ \uid -> do
      sources <- query (GetSourcesByUserId uid)
      <div class="add-path-content">
        <h1>Add A Path</h1>
        <% reform (form here) "add" success Nothing (pathForm uid sources) %>
      </div>
    where
      success :: Path -> CtrlV Response
      success spath = do
        oldpid <- update (IncrementPathId)
        _ <- update (UpdatePath (spath { pathId = oldpid }))
        seeOtherURL (AdminViewPath oldpid)

-- | the 'Form' used for entering a new paste
sourceForm :: UserId -> CtrlVForm Source
sourceForm userId =
    (fieldset $
       ul $
            (,,) <$> (li $ label <span>url</span>    ++> errorList ++> (inputText "http://" `transformEither` required))
                 <*> (li $ label <span>type</span> ++> typeForm)
                 <*> (li $ label <span>format</span> ++> formatForm)
                 <* inputSubmit "add source"
    )  `transformEitherM` toSource
    where
      formatForm =
          select [(a, show a) | a <- [minBound .. maxBound]] (== Pandoc)
      typeForm =
          select [(a, show a) | a <- [minBound .. maxBound]] (== DropBoxIndex)
      toSource (url, typ, fmt) =
          do now <- liftIO getCurrentTime
             return $ Right $
                        (Source { sourceId        = SourceId 0
                                , sourceURL       = MyURL $ Text.unpack url
                                , sourceUserId    = userId
                                , sourceType      = typ
                                , sourceFormat    = fmt
                                , sourceHome      = PageSlug ""
                                , sourceRefreshed = now
                                , sourceAdded     = now
                                })
      required txt
          | Text.null txt = Left "Required"
          | otherwise     = Right txt

adminNewSource :: Acid -> CtrlV Response
adminNewSource acid@Acid{..} = do
    here <- whereami
    ifLoggedInResponse acid "Add A Source" <h1>You Are Not Logged In</h1> $ \uid -> do
      <div class="add-source-content">
        <h1>Add A Source</h1>
        <% reform (form here) "add" success Nothing (sourceForm uid) %>
      </div>
    where
      success :: Source -> CtrlV Response
      success ssource = do
        oldsid <- update (IncrementSourceId)
        _ <- update (UpdateSource (ssource { sourceId = oldsid }))
        seeOtherURL (AdminViewSource oldsid)

adminEditPath :: Acid -> PathId -> CtrlV Response
adminEditPath acid@Acid{..} pid = do
                appTemplate acid "unfinished" () $ <p>unfinished</p>

adminEditSource :: Acid -> SourceId -> CtrlV Response
adminEditSource acid@Acid{..} sid = do
                appTemplate acid "unfinished" () $ <p>unfinished</p>

adminDeletePath :: Acid -> PathId -> CtrlV Response
adminDeletePath acid@Acid{..} pid = do
    ifLoggedIn acid (appTemplate acid "Delete A Path" () $ <h1>You Are Not Logged In</h1>) $ \uid -> do
      mPath <- query (GetPath pid)
      ifItemOK mPath pathUserId uid
        (appTemplate acid "Delete A Path" () $ <p>Path id <% pid %> could not be found.</p>)
        (\ipath -> appTemplate acid "Delete A Path" () $ <p>Path <% pathSlug ipath %>/<% pid %> is not owned by you.</p>)
        (\_ -> do
          _ <- update (DeletePath pid)
          seeOtherURL AdminViewAll)

adminDeleteSource :: Acid -> SourceId -> CtrlV Response
adminDeleteSource acid@Acid{..} sid = do
    ifLoggedIn acid (appTemplate acid "Delete A Source" () $ <h1>You Are Not Logged In</h1>) $ \uid -> do
      mSource <- query (GetSource sid)
      ifItemOK mSource sourceUserId uid
        (appTemplate acid "Delete A Source" () $ <p>Source id <% sid %> could not be found.</p>)
        (\isource -> appTemplate acid "Delete A Source" () $ <p>Source <% sourceURL isource %>/<% sid %> is not owned by you.</p>)
        (\_ -> do
          _ <- update (DeleteSource sid)
          seeOtherURL AdminViewAll)

-- FIXME: make sure everything is coerced to lowercase
pageContentsToMetadataAndBody :: Text -> (Map.Map String String, [Text])
pageContentsToMetadataAndBody contents =
  let
      -- Break into lines, get the ones at the beginning that start
      -- with %
      (header, body) = span (\txt -> ((Text.head txt) == '%')) 
                        $ map (Text.filter ((/=) '\r')) 
                        $ Text.lines contents
      -- Join lines that start with % and spaces and don't have an
      -- obvious header tag to the previous line
      merger rest str = case (matchRegexPR "^%\\s\\s+([^:]+(\\s.*)?)$" $ Text.unpack str) of
        Nothing -> rest ++ [Text.unpack str]
        Just _ -> (init rest) ++ [(last rest) ++ " " ++ (subRegexPR "^%\\s\\s+([^:]+(\\s.*)?)$" "\\1" $ Text.unpack str)]
      merged = foldl merger [] $ debugTraceSelfShow "header: " header
      -- Get only those lines that look like metadata
      metadata = takeWhile (\x -> isJust $ matchRegexPR "^%\\s*[-a-zA-Z0-9]+:\\s+" x) $ trace "merged: " merged 
      -- Split them into name/value pairs
      splitter str = (makeSlug $ subRegexPR "^%\\s*([-a-zA-Z0-9]+):\\s.*" "\\1" str,
                        subRegexPR "^%\\s*[-a-zA-Z0-9]+:\\s+" "" str)
      splitted = map splitter $ trace "metadata: " metadata 
      -- Turn the pairs into Maps.
      final = Map.unions $ map (\x -> Map.insert (fst x) (snd x) Map.empty) $ trace "splitted: " splitted 
  in
    (final, body)

makeSlug :: String -> String
makeSlug input = gsubRegexPR "^-|-$" "" $ gsubRegexPR "[^-a-z0-9]" "-" $ map Data.Char.toLower input

findTitle :: MyURL -> Map.Map String String -> String
findTitle url metadata =
  fromMaybe urlTitle $ Map.lookup "title" metadata
  where
    urlTitle = subRegexPR "^.*/([^./]+).*$" "\\1" $ unMyURL url

makeTags :: Format -> String -> [String]
makeTags PlainText input = [""]
makeTags Pandoc input = splitRegexPR "\\s*,\\s*" input

sourceAndURLsToPages :: Source -> [MyURL] -> CtrlV [PageId]
sourceAndURLsToPages Source{..} urls =
  mapM doPage urls
  where
    doPage :: MyURL -> CtrlV PageId
    doPage url = do
      urlContent <- getURLContent url

      let (metadata, body) = pageContentsToMetadataAndBody urlContent

      let title = findTitle url metadata

      let slug = PageSlug $ makeSlug title

      let newpage = Page { pageId       = PageId 0
                         , pageSourceId = sourceId
                         , pageUserId   = sourceUserId
                         , pageURL      = url
                         , pageTitle    = title
                         , pageFormat   = sourceFormat
                         , pageSlug     = slug
                         , pageTags     = makeTags sourceFormat $ fromMaybe "" $ Map.lookup "tags" metadata
                         }

      -- Distinguish on URL now so we can alarm on duplicates later
      mPage <- query (GetPageBySourceIdAndSlugAndURL sourceId slug url)
      case mPage of
        Nothing -> do
          oldpid <- update (IncrementPageId)
          _ <- update (UpdatePage (newpage { pageId = oldpid }))
          return oldpid
        (Just Page{..}) -> do
          _ <- update (UpdatePage (newpage { pageId = pageId }))
          return pageId


dropBoxIndexSourceToPages :: Source -> CtrlV [PageId]
dropBoxIndexSourceToPages source = do
  urls <- dropBoxIndexSourceToURLs source
  sourceAndURLsToPages source urls

dropBoxIndexSourceToURLs :: Source -> CtrlV [MyURL]
dropBoxIndexSourceToURLs Source{..} = do
  page <- liftIO $ urlToHXT $ unMyURL sourceURL
  -- We want the href attributes of things like this:
  --
  -- <li class="browse-file list-view-cols">
  --   <div class="filename-col">
  --     <a href="https://www.dropbox.com/sh/wgig8uhogr1w5h4/xHyjN0pJtb/synciki/publictest1.txt" [snip]
  --
  -- So, all the li's with a browse-file class, their a children,
  -- the hrefs (then we have to de-dupe later with nub)
  hrefs <- liftIO $ HXT.runX $ page
                  HXT.>>> css "li"
                  HXT.>>> HXT.hasAttrValue "class" (\classes -> elem "browse-file" $ words classes)
                  HXT.>>> css "a" 
                  HXT.>>> HXT.getAttrValue "href"
  -- Debugging:
  debugTrace ("hrefs: " ++ (show $ ppDoc $ nub hrefs)) $ return $ map (\a -> MyURL (a ++ "?dl=1")) $ nub hrefs

  -- "?dl=1" gives us the URL to download the raw text of the file
  -- return $ map (\a -> MyURL (a ++ "?dl=1")) $ nub hrefs

refreshSource :: Source -> CtrlV [PageId]
refreshSource isource@Source{..} = do
  now <- liftIO getCurrentTime
  -- Delete the current list of pages so we don't end up with lies
  -- about pages that are now removed
  oldpages <- query (GetPagesBySourceId sourceId)
  _ <- mapM (\p -> update (DeletePage (pageId p))) oldpages
  pages <- rgetPages sourceType
  _ <- update (UpdateSource (isource { sourceRefreshed = now }))
  return pages

  where
    rgetPages :: SourceType -> CtrlV [PageId]
    rgetPages DropBoxIndex = dropBoxIndexSourceToPages isource
    rgetPages _ = error "Can't refresh that type!"

-- | convert a content page to HTML. We currently only support
-- 'PlainText', but eventually it might do syntax hightlighting,
-- markdown, etc.

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
urlToHXT :: String -> IO (HXT.IOSArrow HXT.XmlTree (NTree HXT.XNode))
urlToHXT pageUrl = do
  contents <- NHC.simpleHttp pageUrl
  return $ HXT.readString [HXT.withParseHTML HXT.yes, HXT.withWarnings HXT.no] (LC.unpack contents)

-- -- Takes a URL and returns a Just pair of the original and just the part between the last / and the extension, IFF the extension is .txt (otherwise Nothing)
-- fullFilePair :: String -> Maybe (String, String)
-- fullFilePair = [rex|^(?{ }.*/(?{ }[^/.]+)\.txt)$|]
-- 
-- 
-- -- Given a dropbox public URL, pull all the hrefs, turn them into
-- -- pairs of the URL and the file's basename
-- --
-- -- FIXME: note that the second bit here, which becomes the cfi_name,
-- -- is completely wrong; it should be extracted by parsing the file.
-- -- 
-- -- FIXME: There's probably all *kinds* of error handling we're not
-- -- doing here; what does urlToHXT return on error?
-- dropBoxPathList :: MyURL -> IO [(String, String)]
-- dropBoxPathList pageUrl = do
--   page <- liftIO $ urlToHXT $ unMyURL pageUrl
--   -- Most of this next line is from
--   -- http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html
--   hrefs <- HXT.runX $ page
--                   HXT.>>> css "a" 
--                   HXT.>>> HXT.getAttrValue "href"
--   return $ nub $ catMaybes $ map fullFilePair hrefs
-- 
--  -- FIXME: Put back
 --
 -- -- | Given a component, pull the list page and cache the file
 -- -- information therefrom.
 -- --
 -- -- FIXME: There's probably all *kinds* of error handling we're not
 -- -- doing here; fix this when we fix dropBoxPathList and urlToHXT 
 -- dropBoxListPageToCFs :: Component -> CtrlV [(ComponentFileIndex, MyURL)]
 -- dropBoxListPageToCFs component@Component{..} = do
 --   entries <- liftIO $ dropBoxPathList url
 --   now <- liftIO getCurrentTime
 -- 
 --   -- If the cache has expired
 --   --
 --   -- FIXME: cache time should be in a config file
 --   if (diffUTCTime now refreshed) > 60 then
 --       do
 --         cId <- update (InsertComponent component { refreshed  = now })
 --         mapM makeStuff entries
 --     else
 --       do
 --         cfs <- query (GetCFsByCId componentId)
 --         return $ map findStuff cfs
 -- 
 --   where
 --     findStuff :: ComponentFile -> (ComponentFileIndex, MyURL)
 --     findStuff cf@ComponentFile{..} = (cfIndex, contentURL)
 -- 
 --     makeStuff :: (String, String) -> CtrlV (ComponentFileIndex, MyURL)
 --     makeStuff (fileURL, fileName) = do
 --       let mycfi = ComponentFileIndex { cfi_componentPath = componentPath
 --                                      , cfi_name          = Text.pack fileName
 --                                      }
 --       cfi <- updateOrInsertCF $
 --         ComponentFile { cfIndex = mycfi
 --           , contentURL        = MyURL fileURL
 --           , cf_componentId    = componentId
 --           }
 --       return (cfi, MyURL fileURL)
-- 
-- viewPath :: Acid -> ComponentPath -> CtrlV Response
-- viewPath acid cPath =
--     do mComponent <- query (GetComponentByPath cPath)
--        case mComponent of
--          Nothing ->
--              do notFound ()
--                 appTemplate acid "Path not found." () $
--                     <p>Path <% cPath %> could not be found.</p>
--          (Just component@Component{..}) ->
--              do entries <- dropBoxListPageToCFs component
--                 ok ()
--                 appTemplate acid ("Path " ++ (Text.unpack $ unComponentPath cPath)) () $
--                   <div class="pathContents">
--                     <p>Path <% cPath %> has url <% unMyURL url %>; last refresh at <% refreshed %></p>
--                     <ul>
--                     <% mapM (showEntry componentId) entries %>
--                     </ul>
--                   </div>
--     where 
--       showEntry :: ComponentId -> (ComponentFileIndex, MyURL) -> XMLGenT CtrlV' (XMLType CtrlV')
--       showEntry cId (cfi, myurl) = 
--         let url = unMyURL myurl
--             name = cfi_name cfi
--             cfpath = cfi_componentPath cfi
--           in
--             <li>
--               <a href=(ViewPathPage cId cfpath name)><% name %></a>
--             </li>
-- 
-- renderPage :: MyURL -> XMLGenT CtrlV' (XMLType CtrlV')
-- renderPage pageUrl =
--   <p>some stuff: <% unMyURL pageUrl %></p>
-- 
-- viewPathPage :: Acid -> ComponentId -> ComponentPath -> Text -> CtrlV Response
-- viewPathPage acid cId cPath name =
--   do mComponent <- query (GetComponentById cId)
--      case mComponent of
--         Nothing ->
--           do
--             notFound ()
--             appTemplate acid "Id not found." () $
--               <p>Component ID <% cId %> could not be found.</p>
--         (Just component@Component{..}) ->
--           do 
--             mCF <- query (GetCFByCFIParts cPath name)
--             case mCF of
--               Nothing -> do
--                 notFound ()
--                 appTemplate acid "Path/Name not found." () $
--                   <p>Component File <% cPath %>/<% name %> could not be found.</p>
--               (Just cf@ComponentFile{..}) -> do
--                 pageHtml <- renderPage contentURL
--                 appTemplate acid ("Page " ++ (Text.unpack name) ++ " in section " ++ (Text.unpack $ unComponentPath componentPath)) () pageHtml
-- 
-- 
-- 

viewPage :: Acid -> PathHost -> PathSlug -> PageSlug -> CtrlV Response
viewPage acid@Acid{..} myPathHost myPathSlug myPageSlug = do
  -- FIXME: since we're retrieving the content *anyway*, let's
  -- refresh whatever of the particular page's info that we can
  mPath <- query (GetPathByHostAndSlug myPathHost myPathSlug)
  case mPath of
    Nothing -> do
      appTemplate acid "Page Not Found" () $ <div>
        <p>Page Not Found, no such path</p>
        <p><% (show myPathHost ) %></p>
        <p><% (show myPathSlug ) %></p>
        </div>
    (Just Path{..}) -> do
      mPage <- query (GetPageBySourceIdsAndSlug pathSources myPageSlug)
      case mPage of
        Nothing -> do
          testPages <- query (GetPagesBySourceIds pathSources)
          appTemplate acid "Page Not Found" () $ <div>
            <p>Page Not Found, no such page</p>
            <p><% (show pathSources ) %></p>
            <p><% (show myPageSlug ) %></p>
            <p><% (show (map pageSlug testPages) ) %></p>
            </div>
        (Just myPage) -> do
          -- FIXME: needs title
          content <- getURLContent (pageURL myPage)
          let (metadata, body) = pageContentsToMetadataAndBody content
          appTemplate acid "The Page" () $ <% formatPage (pageFormat myPage) body %>




viewPath :: Acid -> PathHost -> PathSlug -> CtrlV Response
viewPath acid@Acid{..} pathHost pathSlug = do
                appTemplate acid "unfinished" () $ <p>unfinished</p>

-- END dropbox stuff

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
              defaultConf {
                httpConf = nullConf {
                      port      = 8080
                    , validator  = Nothing
                    , logAccess = Just logMAccess
                    , timeout = 30
                }
              }
              (AcidLocal Nothing initialCtrlVState)
              ()
              AdminViewAll
              "http://vrici.lojban.org:8080"
              (route acid "http://vrici.lojban.org:8080")

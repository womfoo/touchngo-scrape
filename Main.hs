import Control.Applicative
import qualified Data.Configurator as Config
import Data.Configurator.Types
import Data.Maybe
import qualified Data.Text as Text
import Data.Time
import Network.Browser
import Network.CGI.Protocol (formEncode,formDecode)
import Network.HTTP
import Network.URI
import Text.HTML.TagSoup

data Account = Account {
  userid :: String,
  passwd :: String
} deriving (Show)

u1 = "http://e-services.touchngo.com.my/e-Statement/"
u2 = "http://e-services.touchngo.com.my/e-Statement/index.cfm"
u3 = "http://e-services.touchngo.com.my/e-Statement/submit.cfm"
u4 = (++) "http://e-services.touchngo.com.my/e-Statement/printstatementdetails.cfm?mfgno="

getCSVLink [(TagOpen "a" [("href",url)])] = url
getCSVLink _                              = error "CSV link not found"

toParams (TagOpen _ attrs) = (name,value)
  where name  = fromJust (lookup "name" attrs)
        value = fromMaybe "" $ lookup "value" attrs

overrideParams account (name,value) = (name,newvalue)
   where newvalue  = fromMaybe value $ lookup name overrides
         overrides = [("userid",userid account)
                     ,("password",passwd account)
                     ,("Submit","Login")]

postReqWithParms url params = request $ postRequestWithBody url "application/x-www-form-urlencoded" (formEncode params)

getAccount :: Config -> IO Account
getAccount config = do
  u <- Config.lookup config $ Text.pack "userid"
  p <- Config.lookup config $ Text.pack "passwd"
  case Account <$> u <*> p of
    Just x -> return x
    _      -> error "error loading config"

main = do

  conf <- Config.load [Required "touchngo.cfg"]
  account <- getAccount conf

  (y,m,d) <- fmap (toGregorian . utctDay) getCurrentTime -- TODO: maybe pull 90 days only?

  csvdata <- browse $ do
    setMaxErrorRetries $ Just 10
    setAllowRedirects False -- need this to handle cookies

    (uri,resp) <- request $ getRequest u1

    let inputTags = filter (~== "<input>") $ parseTags (rspBody resp)
        rawParams = map toParams inputTags
        paramsWithLogin = map (overrideParams account) rawParams
    setAllowRedirects True -- we can now enable this as the session is now kept

    (uri2,resp2) <- postReqWithParms u2 paramsWithLogin

    (uri3,resp3) <- postReqWithParms u3 [("fromDate","1")
                                        ,("fromMonth","1")
                                        ,("fromYear","2012")
                                        ,("toDate",show d)
                                        ,("toMonth",show m)
                                        ,("toYear",show y)]

    let uri3Params = (formDecode . query) uri3
        mfgno = case (lookup "mfgno" uri3Params) of
                     Just n  -> n
                     _       -> error "mfgno not found"

    (uri4,resp4) <- postReqWithParms (u4 mfgno) [("exportCSV","Export to CSV")]

    let csvLink = getCSVLink $ filter (~== "<a>") $ parseTags (rspBody resp4)

    (uri5,resp5) <- request $ getRequest csvLink

    return $ rspBody resp5

  writeFile "output.csv" $! csvdata

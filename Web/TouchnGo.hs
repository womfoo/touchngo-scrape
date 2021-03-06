module Web.TouchnGo where

import Data.Maybe
import Data.Time
import Network.Browser
import Network.CGI.Protocol (formEncode,formDecode)
import Network.HTTP
import Network.URI
import Text.HTML.TagSoup

data Account = Account {
  userid :: String,
  passwd :: String
} deriving (Read,Show)

u1 = "http://e-services.touchngo.com.my/e-Statement/"
u2 = "http://e-services.touchngo.com.my/e-Statement/index.cfm"
u3 = "http://e-services.touchngo.com.my/e-Statement/submit.cfm"
u4 = (++) "http://e-services.touchngo.com.my/e-Statement/printstatementdetails.cfm?mfgno="

getCSVLink [TagOpen "a" [("href",url)]] = url
getCSVLink _                            = error "CSV link not found"

toParams (TagOpen _ attrs) = (name,value)
  where name  = fromJust (lookup "name" attrs)
        value = fromMaybe "" $ lookup "value" attrs

overrideParams account (name,value) = (name,newvalue)
   where newvalue  = fromMaybe value $ lookup name overrides
         overrides = [("userid",userid account)
                     ,("password",passwd account)
                     ,("Submit","Login")]

postReqWithParms url params = request $ postRequestWithBody url "application/x-www-form-urlencoded" (formEncode params)

scrapeTNG account = do
  (y,m,d) <- fmap (toGregorian . utctDay) getCurrentTime -- TODO: maybe pull 90 days only?
  browse $ do
    -- TODO: check response codes and remove errors
    setOutHandler (const (return ()))

    (uri,resp) <- request $ getRequest u1

    let inputTags = filter (~== "<input>") $ parseTags (rspBody resp)
        rawParams = map toParams inputTags
        paramsWithLogin = map (overrideParams account) rawParams

    (uri2,resp2) <- postReqWithParms u2 paramsWithLogin

    (uri3,resp3) <- postReqWithParms u3 [("fromDate","1")
                                        ,("fromMonth","1")
                                        ,("fromYear","2012")
                                        ,("toDate",show d)
                                        ,("toMonth",show m)
                                        ,("toYear",show y)]

    let uri3Params = (formDecode . tail . uriQuery) uri3
        mfgno = case lookup "mfgno" uri3Params of
                     Just n  -> n
                     _       -> error "mfgno not found"

    (uri4,resp4) <- postReqWithParms (u4 mfgno) [("exportCSV","Export to CSV")]

    let csvLink = getCSVLink $ filter (~== "<a>") $ parseTags (rspBody resp4)

    (uri5,resp5) <- request $ getRequest csvLink

    return (mfgno,rspBody resp5)



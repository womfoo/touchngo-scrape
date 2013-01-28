import Data.Maybe
import Data.Time
import Network.Browser
import Network.CGI.Protocol (formEncode)
import Network.HTTP
import Text.HTML.TagSoup

u1 = "http://e-services.touchngo.com.my/e-Statement/"
u2 = "http://e-services.touchngo.com.my/e-Statement/index.cfm"
u3 = "http://e-services.touchngo.com.my/e-Statement/submit.cfm"
u4 = "http://e-services.touchngo.com.my/e-Statement/printstatementdetails.cfm?mfgno=" ++ mfgno -- TODO: parse mfgno from u3 response

-- Hardcoding for now
userid   = "your-userid"
passwd   = "your-password"
mfgno    = "your-mfgno"

getCSVLink [(TagOpen "a" [("href",url)])] = url
getCSVLink _                              = error "CSV link not found"

toParams (TagOpen _ attrs) = (name,value)
  where name  = fromJust (lookup "name" attrs)
        value = fromMaybe "" $ lookup "value" attrs

overrideParams (name,value) = (name,newvalue)
   where newvalue  = fromMaybe value $ lookup name overrides
         overrides = [("userid",userid)
                     ,("password",passwd)
                     ,("Submit","Login")]

postReqWithParms url params = request $ postRequestWithBody url "application/x-www-form-urlencoded" (formEncode params)

main = do

  (y,m,d) <- fmap (toGregorian . utctDay) getCurrentTime -- TODO: maybe pull 90 days only?

  csvdata <- browse $ do
    setMaxErrorRetries $ Just 10
    setAllowRedirects False -- need this to handle cookies

    (uri,resp) <- request $ getRequest u1

    let inputTags = filter (~== "<input>") $ parseTags (rspBody resp)
        rawParams = map toParams inputTags
        paramsWithLogin = map overrideParams rawParams
    setAllowRedirects True -- we can now enable this as the session is now kept

    (uri2,resp2) <- postReqWithParms u2 paramsWithLogin

    (uri3,resp3) <- postReqWithParms u3 [("fromDate","1")
                                        ,("fromMonth","1")
                                        ,("fromYear","2012")
                                        ,("toDate",show d)
                                        ,("toMonth",show m)
                                        ,("toYear",show y)]

    (uri4,resp4) <- postReqWithParms u4 [("exportCSV","Export to CSV")]

    let csvLink = getCSVLink $ filter (~== "<a>") $ parseTags (rspBody resp4)

    (uri5,resp5) <- request $ getRequest csvLink

    return $ rspBody resp5

  writeFile "output.csv" $! csvdata

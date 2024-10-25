module Template where

-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = MkAttr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [MkAttr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link s1 s2) = HtmlTag "a" [MkAttr "href" s1]
    [HtmlString s2]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [
    HtmlTag "li" [] [HtmlString "Apples"], 
    HtmlTag "li" [] [HtmlString "Bananas"], 
    HtmlTag "li" [] [HtmlString "Oranges"]]

instance HTML a => HTML [a] where
  toHtml l = HtmlTag "ul" []  [HtmlTag "li" [] [toHtml x] | x <- l]

data AddressBook = MkBook [Contact]
data Contact = MkContact Name [Email]
data Name = MkName String String
data Email = MkMail String EmailType
data EmailType = Work | Private
    deriving(Show)

myAddressBook :: AddressBook
myAddressBook = MkBook [MkContact (MkName "Zeno" "Vandenbulcke") [MkMail "123@hotmail.be" Private, MkMail "lol" Work], MkContact (MkName "Zeno" "Vandenbulcke") [MkMail "123@hotmail.be" Private, MkMail "lol" Work]]


instance HTML AddressBook where
  toHtml (MkBook c)= toHtml c

instance HTML String where
    toHtml s = HtmlString s

instance HTML Email where
    toHtml (MkMail str t) = HtmlString (str ++ " | " ++ (show t))

instance HTML Name where
    toHtml (MkName fn ln) = HtmlTag "p" [] [HtmlString (fn ++ " " ++ ln)]

instance HTML Contact where
    toHtml (MkContact name mails) = HtmlTag "div" [] [(toHtml name), (toHtml mails)]

printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unwords (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ " " ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]

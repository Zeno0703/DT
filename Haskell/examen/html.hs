
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
  toHtml (Link link text) = HtmlTag "a" [MkAttr "href" link] [HtmlString text]

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
  toHtml l = HtmlTag "ul" [] [HtmlTag "li" [] [toHtml s] | s <- l]

data AddressBook = MkAddress [Contact]
data Contact = MkContact Name [Email]
data Name = MkName String String
data Email = MkEmail String EmailType
data EmailType = Work | Private deriving(Show)

myAddressBook :: AddressBook
myAddressBook = MkAddress [MkContact (MkName "Zeno" "Vandenbulcke") [MkEmail "123.be" Work, MkEmail "lol.be" Private], 
                          MkContact (MkName "Indra" "Beukeleirs") [MkEmail "123.be" Work, MkEmail "lol.be" Private]]

instance HTML AddressBook where
  toHtml (MkAddress c) = toHtml c

instance HTML Contact where
  toHtml (MkContact n eList) = HtmlTag "div" [] [toHtml n, toHtml eList]

instance HTML Name where
  toHtml (MkName v a) = HtmlString (v ++ " " ++ a)

instance HTML String where
  toHtml = HtmlString

instance HTML Email where
  toHtml (MkEmail e t) = HtmlTag "p" [] [HtmlString (e ++ " | " ++ show t)]


printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ " " ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]
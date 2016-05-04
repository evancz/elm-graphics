module Text exposing
  ( Text
  , fromString, empty, append, concat, join
  , link, Style, style, defaultStyle, Line(..)
  , typeface, monospace, height, color, bold, italic, line
  )
  -- where

{-| A library for styling and displaying text. While the
[`String`](String) library focuses on representing and manipulating
strings of character strings, the `Text` library focuses on how those
strings should look on screen. It lets you make text bold or italic,
set the typeface, set the text size, etc.

# Text
@docs Text

# Creating Text
@docs fromString, empty, append, concat, join

# Links and Style
@docs link, typeface, monospace, height, color, bold, italic, line, Line,
    style, Style, defaultStyle

-}

import Color exposing (Color, black)
import Regex
import String


{-| Represents styled text. It can be rendered with collages or with elements.
-}
type Text
  = Str String
  | Append Text Text
  | Link String Text
  | Meta String String Text


{-| Styles for lines on text. This allows you to add an underline, an overline,
or a strike out text:

    line Under   (fromString "underline")
    line Over    (fromString "overline")
    line Through (fromString "strike out")
-}
type Line = Under | Over | Through


{-| Represents all the ways you can style `Text`. If the `typeface` list is
empty or the `height` is `Nothing`, the users will fall back on their browser's
default settings. The following `Style` is black, 16 pixel tall, underlined, and
Times New Roman (assuming that typeface is available on the user's computer):

    { typeface = [ "Times New Roman", "serif" ]
    , height   = Just 16
    , color    = black
    , bold     = False
    , italic   = False
    , line     = Just Under
    }
-}
type alias Style =
  { typeface : List String
  , height   : Maybe Float
  , color    : Color
  , bold     : Bool
  , italic   : Bool
  , line     : Maybe Line
  }


{-| Plain black text. It uses the browsers default typeface and text height.
No decorations are used.

    { typeface = []
    , height = Nothing
    , color = black
    , bold = False
    , italic = False
    , line = Nothing
    }
-}
defaultStyle : Style
defaultStyle =
    { typeface = []
    , height = Nothing
    , color = black
    , bold = False
    , italic = False
    , line = Nothing
    }


{-| Convert a string into text which can be styled and displayed. To show the
string `"Hello World!"` on screen in italics, you could say:

    main = leftAligned (italic (fromString "Hello World!"))
-}
fromString : String -> Text
fromString =
  Str


{-| Text with nothing in it.

    empty = fromString ""
-}
empty : Text
empty =
  fromString ""


{-| Put two chunks of text together.

    append (fromString "hello ") (fromString "world") == fromString "hello world"
-}
append : Text -> Text -> Text
append =
  Append


{-| Put many chunks of text together.

    concat
      [ fromString "type "
      , bold (fromString "Maybe")
      , fromString " = Just a | Nothing"
      ]
-}
concat : List Text -> Text
concat texts =
  List.foldr append empty texts


{-| Put many chunks of text together with a separator.

    chunks : List Text
    chunks = List.map fromString ["lions","tigers","bears"]

    join (fromString ", ") chunks == fromString "lions, tigers, bears"
-}
join : Text -> List Text -> Text
join seperator texts =
  concat (List.intersperse seperator texts)


{-| Set the style of some text. For example, if you design a `Style` called
`footerStyle` that is specifically for the bottom of your page, you could apply
it to text like this:

    style footerStyle (fromString "the old prince / 2007")
-}
style : Style -> Text -> Text
style sty text =
  text
    |> color sty.color
    |> typeface sty.typeface
    |> (if sty.bold then bold else identity)
    |> (if sty.italic then italic else identity)
    |> maybeAdd line sty.line
    |> maybeAdd height sty.height


maybeAdd : (a -> Text -> Text) -> Maybe a -> Text -> Text
maybeAdd add maybeValue text =
  case maybeValue of
    Nothing ->
      text

    Just value ->
      add value text


{-| Provide a list of preferred typefaces for some text.

    ["helvetica","arial","sans-serif"]

Not every browser has access to the same typefaces, so rendering will use the
first typeface in the list that is found on the user's computer. If there are
no matches, it will use their default typeface. This works the same as the CSS
font-family property.
-}
typeface : List String -> Text -> Text
typeface faces text =
  case faces of
    [] ->
      text

    _ ->
      Meta "font-family" (typefacesToString faces) text


typefacesToString : List String -> String
typefacesToString faces =
  "'" ++ String.join "', '" faces ++ "'"


{-| Switch to a monospace typeface. Good for code snippets.

    monospace (fromString "foldl (+) 0 [1,2,3]")
-}
monospace : Text -> Text
monospace text =
  Meta "font-family" "monospace" text


{-| Create a link by providing a URL and the text of the link.

    link "http://elm-lang.org" (fromString "Elm Website")
-}
link : String -> Text -> Text
link =
  Link


{-| Set the height of some text.

    height 40 (fromString "Title")
-}
height : Float -> Text -> Text
height px text =
  Meta "font-size" (toString px ++ "px") text


{-| Set the color of some text.

    color red (fromString "Red")
-}
color : Color -> Text -> Text
color color text =
  Meta "color" (colorToCss color) text


colorToCss : Color -> String
colorToCss color =
  let
    {red,green,blue,alpha} =
      Color.toRgb color
  in
    "rgba("
    ++ toString red ++ ", "
    ++ toString green ++ ", "
    ++ toString blue ++ ", "
    ++ toString alpha ++ ")"


{-| Make text bold.

    fromString "sometimes you want " ++ bold (fromString "emphasis")
-}
bold : Text -> Text
bold text =
  Meta "font-weight" "bold" text


{-| Make text italic.

    fromString "make it " ++ italic (fromString "important")
-}
italic : Text -> Text
italic text =
  Meta "font-style" "italic" text


{-| Put lines on text.

    line Under   (fromString "underlined")
    line Over    (fromString "overlined")
    line Through (fromString "strike out")
-}
line : Line -> Text -> Text
line lineTag text =
  let
    decoration =
      case lineTag of
        Under ->
          "underline"

        Over ->
          "overline"

        Through ->
          "line-through"
  in
    Meta "text-decoration" decoration text


toHtmlString : Text -> String
toHtmlString text =
  toHtmlStringHelp Nothing "" text


toHtmlStringHelp : Maybe String -> String -> Text -> String
toHtmlStringHelp maybeHref styles text =
  case text of
    Str string ->
      string
        |> replace "\"" "&#34;"
        |> replace "'"  "&#39;"
        |> replace "<"  "&#60;"
        |> replace ">"  "&#62;"
        |> String.lines
        |> List.map (replace " " "&nbsp;")
        |> String.join "<br>"
        |> wrap maybeHref styles

    Append a b ->
      wrap maybeHref styles (toHtmlString a ++ toHtmlString b)

    Link href text ->
      toHtmlStringHelp (Just (Maybe.withDefault href maybeHref)) styles text

    Meta key value text ->
      toHtmlStringHelp maybeHref (styles ++ key ++ ":" ++ value ++ ";") text


replace : String -> String -> String -> String
replace from to str =
  Regex.replace Regex.All (Regex.regex from) (\_ -> to) str


wrap : Maybe String -> String -> String -> String
wrap maybeHref styles insides =
  let
    linkedInsides =
      case maybeHref of
        Nothing ->
          insides

        Just href ->
          "<a href=\"" ++ href ++ "\">" ++ insides ++ "</a>"
  in
    if styles == "" then
      linkedInsides

    else
      "<span style=\"" ++ styles ++ "\">" ++ linkedInsides ++ "</span>"

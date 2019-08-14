module Singularis.Page exposing (Route(..),extractRoute)

import Browser.Navigation
import Singularis.View.Polygon as Polygon
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>), Parser)
import Singularis.Page.Oracle as Oracle
import Url.Parser.Query as Query
import Dict

type Route
    = Home
    | Oracle Oracle.Model

extractRoute : Url -> Route
extractRoute input=
    {input| path = ""}
    |> Parser.parse matchRoute
    |> Maybe.withDefault Home

equals : String -> String -> Query.Parser (Maybe ())
equals value name =
    (Query.enum name <| Dict.fromList [(value,())])

matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
    [ Parser.query <|
        Query.map2
            (\maybeOkey question -> 
                case maybeOkey of
                    Nothing -> Home
                    Just () -> question |> Oracle.init |> Oracle
                     
            )
            ("page" |> equals "oracle")
            (Query.string "q" |> Query.map (Maybe.withDefault ""))
    , Parser.query ("page" |> equals "oracle")
        |> Parser.map
            ( Maybe.map  (always ("" |> Oracle.init |> Oracle) )
                >> Maybe.withDefault Home
            )
    ]
    

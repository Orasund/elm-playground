module Singularis.Page exposing (Config, Route(..), extractRoute, getPageName)

import Browser.Navigation exposing (Key)
import Dict
import Random exposing (Seed)
import Singularis.Page.Ai as Ai
import Singularis.Page.Oracle as Oracle
import Singularis.View.Polygon as Polygon
import Time exposing (Posix)
import Url exposing (Url)
import Url.Parser as Parser exposing ((<?>), Parser)
import Url.Parser.Query as Query


type alias Config =
    { key : Key
    , time : Posix
    , scale : Float
    , seed : Seed
    , text : String
    }


type Route
    = Home
    | Oracle Oracle.Model
    | Ai Ai.Model


extractRoute : Config -> Url -> Route
extractRoute config input =
    { input | path = "" }
        |> Parser.parse (matchRoute config)
        |> Maybe.withDefault Home


equals : String -> String -> Query.Parser (Maybe ())
equals value name =
    Query.enum name <| Dict.fromList [ ( value, () ) ]


getPageName : Url -> String
getPageName input =
    { input | path = "" }
        |> Parser.parse
            (Parser.query <|
                Query.map
                    (\page ->
                        case page of
                            Just "ai" ->
                                "Ai"

                            Just "oracle" ->
                                "Oracle"

                            _ ->
                                "Home"
                    )
                    (Query.string "page")
            )
        |> Maybe.withDefault "Error"


matchRoute : Config -> Parser (Route -> a) a
matchRoute { time, seed } =
    Parser.query <|
        Query.map2
            (\page question ->
                case page of
                    Just "ai" ->
                        seed |> Ai.init |> Ai

                    Just "oracle" ->
                        question |> Oracle.init time |> Oracle

                    _ ->
                        Home
            )
            (Query.string "page")
            (Query.string "q" |> Query.map (Maybe.withDefault ""))

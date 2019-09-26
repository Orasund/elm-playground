module EmojiAnalysis exposing (main)

import Browser
import Char
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Set
import Task


type Page
    = Chat
    | Profil
    | Analyse


type Model
    = Waiting
    | Processing
    | Done
        { chat : List Message
        , emojis : Dict String (Dict Char Int)
        , analyse : List Message
        , page : Page
        }


type alias Message =
    { name : String
    , msg : String
    }


type WaitingMsg
    = InitialButtonPressed
    | FileSelected File


type Msg
    = WaitingSpecific WaitingMsg
    | FileRead String
    | PageChanged Page


init : () -> ( Model, Cmd Msg )
init _ =
    ( Waiting
    , Cmd.none
    )


singleLineParser : Parser (Maybe Message)
singleLineParser =
    (Parser.succeed Message
        |. Parser.variable
            { start = Char.isDigit
            , inner = \c -> Char.isDigit c || c == '.'
            , reserved = Set.empty
            }
        |. Parser.symbol ", "
        |. Parser.variable
            { start = Char.isDigit
            , inner = \c -> Char.isDigit c || c == ':'
            , reserved = Set.empty
            }
        |. Parser.symbol " - "
        |= Parser.variable
            { start = Char.isAlphaNum
            , inner = \c -> Char.isAlphaNum c || c == ' '
            , reserved = Set.empty
            }
        |. Parser.symbol ":"
        |= Parser.variable
            { start = always True
            , inner = (/=) '\n'
            , reserved = Set.empty
            }
    )
        |> Parser.map
            (\message ->
                let
                    newMsg : String
                    newMsg =
                        message.msg |> String.filter (Char.toCode >> (\c -> c > 5024))
                in
                if newMsg == "" then
                    Nothing

                else
                    Just { message | msg = newMsg }
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( WaitingSpecific waitingMsg, Waiting ) ->
            case waitingMsg of
                InitialButtonPressed ->
                    ( model, Select.file [ "text/plain" ] (WaitingSpecific << FileSelected) )

                FileSelected file ->
                    ( Processing, file |> File.toString |> Task.perform FileRead )

        ( FileRead file, Processing ) ->
            let
                chat : List Message
                chat =
                    file
                        |> String.split "\n"
                        |> List.filterMap
                            (Parser.run singleLineParser
                                >> Result.withDefault Nothing
                            )
            in
            ( Done
                { chat = chat
                , emojis =
                    chat
                        |> List.foldl
                            (\message ->
                                Dict.update message.name
                                    (Maybe.map
                                        (\dict -> message.msg
                                            |> String.toList
                                            |> List.foldl
                                                (\c -> Dict.update c
                                                    (Maybe.map ((+) 1)
                                                        >> Maybe.withDefault 1
                                                        >> Just
                                                    )
                                                )
                                                dict
                                        )
                                        >> Maybe.withDefault Dict.empty
                                        >> Just
                                    )
                            )
                            Dict.empty
                , page = Chat
                , analyse = []
                }
            , Cmd.none
            )
        
        (PageChanged page,Done m) ->
            (Done {m|page = page},Cmd.none)

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [] <|
        case model of
            Waiting ->
                Input.button
                    [ Element.centerX
                    , Element.centerY
                    , Element.padding 10
                    ]
                <|
                    { onPress = Just <| WaitingSpecific <| InitialButtonPressed
                    , label = Element.text <| "Upload a File"
                    }

            Processing ->
                Element.none

            Done { chat, emojis,analyse,page } ->
                let
                    menuButton : Page -> String -> Element Msg
                    menuButton p name =
                        Input.button []
                            {onPress = Just (PageChanged p)
                            ,label = Element.text <| name
                            }
                in
                
                Element.column [Element.centerX,Element.spacing 10] <|
                [
                Element.row
                    [ Element.centerX
                    , Element.width<| Element.fill
                    , Element.spaceEvenly
                    ] <|
                    [ menuButton Chat "Chat"
                    , menuButton Profil "Profil"

                    ]
                , case page of
                    Chat ->
                        chat
                        |> List.map
                            (\{ name, msg } -> name ++ ":" ++ msg |> Element.text)
                        |> Element.column []
                    Profil ->
                        emojis
                        |> Dict.toList
                        |> List.map
                            (\( name, dict ) ->
                                dict
                                    |> Dict.toList
                                    |> List.sortBy Tuple.second
                                    |> List.map
                                        (\( c, n ) ->
                                            String.fromChar c ++ ":" ++ String.fromInt n |> Element.text
                                        )
                                    |> (::) (Element.text name)
                                    |> Element.column [Element.width <| Element.px <| 100]
                            )
                        |> Element.row [Element.alignTop]
                    Analyse ->
                        Element.column []<|
                        [Input.multiline []
                            { onChange : String -> msg
                            , text : String
                            , placeholder : Maybe (Placeholder msg)
                            , label : Label msg
                            , spellcheck : Bool
                            }
                        analyse
                        |> List.map
                            (\{ name, msg } -> name ++ ":" ++ msg |> Element.text)
                        |> Element.column []
                        ]
                ]
                


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

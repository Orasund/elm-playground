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
        , chatForm : String
        , analysedChat : List Message
        , page : Page
        }


type alias Message =
    { name : String
    , msg : String
    }


type WaitingMsg
    = InitialButtonPressed
    | FileSelected File


type DoneMsg
    = UpdateForm String
    | PageChanged Page
    | StartAnalysing


type Msg
    = WaitingSpecific WaitingMsg
    | FileRead String
    | DoneSpecific DoneMsg


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
                                        (\dict ->
                                            message.msg
                                                |> String.toList
                                                |> List.foldl
                                                    (\c ->
                                                        Dict.update c
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
                , chatForm = ""
                , analysedChat = []
                }
            , Cmd.none
            )

        ( DoneSpecific doneMsg, Done doneModel ) ->
            case doneMsg of
                PageChanged page ->
                    ( Done { doneModel | page = page }, Cmd.none )

                UpdateForm string ->
                    ( Done { doneModel | chatForm = string }, Cmd.none )

                StartAnalysing ->
                    let
                        users : List String
                        users =
                            doneModel.emojis
                                |> Dict.keys

                        chat : List String
                        chat =
                            doneModel.chatForm
                                |> String.split "\n"

                        {- |> List.filterMap
                           (String.filter (Char.toCode >> (\c -> c > 5024))
                               >> (\string ->
                                       if string == "" then
                                           Nothing

                                       else
                                           Just string
                                  )
                           )
                        -}
                        emojiToUser : Char -> Maybe String
                        emojiToUser emoji =
                            users
                                |> List.map
                                    (\key ->
                                        ( let
                                            dict : Dict Char Int
                                            dict =
                                                doneModel.emojis
                                                    |> Dict.get key
                                                    |> Maybe.withDefault Dict.empty

                                            sum : Float
                                            sum =
                                                dict
                                                    |> Dict.values
                                                    |> List.sum
                                                    |> toFloat
                                          in
                                          dict
                                            |> Dict.get emoji
                                            |> Maybe.withDefault 0
                                            |> toFloat
                                            |> (\a -> a / sum)
                                        , key
                                        )
                                    )
                                |> List.sortBy Tuple.first
                                |> List.reverse
                                |> List.head
                                |> Maybe.map Tuple.second

                        analysedChat : List Message
                        analysedChat =
                            case users of
                                [] ->
                                    []

                                default :: _ ->
                                    chat
                                        |> List.map
                                            (\message ->
                                                { msg = message
                                                , name =
                                                    message
                                                        |> String.filter (Char.toCode >> (\c -> c > 5024))
                                                        |> String.toList
                                                        |> List.map
                                                            (emojiToUser
                                                                >> Maybe.withDefault default
                                                            )
                                                        |> List.foldl
                                                            (\name ->
                                                                Dict.update name
                                                                    (Maybe.map ((+) 1)
                                                                        >> Maybe.withDefault 1
                                                                        >> Just
                                                                    )
                                                            )
                                                            Dict.empty
                                                        |> Dict.toList
                                                        |> List.sortBy Tuple.second
                                                        |> List.reverse
                                                        |> List.head
                                                        |> Maybe.map Tuple.first
                                                        |> Maybe.withDefault default
                                                }
                                            )
                    in
                    ( Done { doneModel | analysedChat = analysedChat }
                    , Cmd.none
                    )

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

            Done { chat, emojis, page, chatForm, analysedChat } ->
                let
                    menuButton : Page -> String -> Element Msg
                    menuButton p name =
                        Input.button []
                            { onPress = Just (DoneSpecific <| PageChanged p)
                            , label = Element.text <| name
                            }

                    menu : Element Msg
                    menu =
                        Element.row
                            [ Element.centerX
                            , Element.width <| Element.fill
                            , Element.spaceEvenly
                            ]
                        <|
                            [ menuButton Chat "Chat"
                            , menuButton Profil "Profil"
                            , menuButton Analyse "Analyse"
                            ]
                in
                Element.column [ Element.centerX, Element.spacing 10 ] <|
                    [ menu
                    , case page of
                        Chat ->
                            chat
                                |> List.map
                                    (\{ name, msg } -> name ++ ":" ++ msg |> Element.text)
                                |> Element.column [ Element.width <| Element.px 600 ]

                        Profil ->
                            emojis
                                |> Dict.toList
                                |> List.map
                                    (\( name, dict ) ->
                                        let
                                            sum : Float
                                            sum =
                                                dict
                                                    |> Dict.values
                                                    |> List.sum
                                                    |> toFloat
                                        in
                                        dict
                                            |> Dict.toList
                                            |> List.sortBy Tuple.second
                                            |> List.reverse
                                            |> List.map
                                                (\( c, n ) ->
                                                    String.fromChar c ++ ":" ++ String.fromInt ((toFloat n * 100) / sum |> floor) ++ "%" |> Element.text
                                                )
                                            |> (::) (Element.text name)
                                            |> Element.column [ Element.alignTop, Element.spacing 10 ]
                                    )
                                |> Element.row [ Element.alignTop, Element.spacing 10 ]

                        Analyse ->
                            Element.column [ Element.centerX, Element.spacing 10 ] <|
                                [ Input.multiline [ Element.width <| Element.px 600, Element.height <| Element.px 400 ]
                                    { onChange = DoneSpecific << UpdateForm
                                    , text = chatForm
                                    , placeholder = Nothing
                                    , label = Input.labelAbove [] <| Element.text "Copy chat in here"
                                    , spellcheck = False
                                    }
                                , Input.button []
                                    { onPress = Just (DoneSpecific <| StartAnalysing)
                                    , label = Element.text <| "Start Analysing"
                                    }
                                , analysedChat
                                    |> List.map
                                        (\{ name, msg } ->
                                            (name ++ ":" ++ msg)
                                                |> Element.text
                                                |> List.singleton
                                                |> Element.paragraph []
                                        )
                                    |> Element.column [ Element.width <| Element.px 600 ]
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

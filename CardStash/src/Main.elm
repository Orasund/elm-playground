module Main exposing (..)

import Browser exposing (Document)
import Data.Deck as Deck exposing (Deck)
import Gen.Enum.Face as Face
import Gen.Record.Model as Model exposing (Model)
import Html exposing (Html)
import Html.Events as Events
import Random exposing (Seed, initialSeed)


type Msg
    = GotSeed Seed
    | Swap { first : Bool }
    | Stash { first : Bool }
    | Unstash { first : Bool }


initModel : Seed -> Model
initModel seed =
    let
        ( decks, _ ) =
            Random.step Deck.generator seed
    in
    { decks = decks, stash = Nothing, seed = seed }


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel (Random.initialSeed 42), Random.generate GotSeed Random.independentSeed )


toString card visible =
    -- if visible then
    card.face
        |> Face.toString
        |> String.toList
        |> List.head
        |> Maybe.map String.fromChar
        |> Maybe.withDefault ""
        |> List.repeat card.amount
        |> String.concat



--else
--  "?"


viewDeck : { first : Bool } -> Deck -> Model -> Html Msg
viewDeck first deck model =
    let
        onClick =
            (if first.first then
                [ Stash first, Swap first ]

             else
                [ Swap first, Stash first ]
            )
                |> List.filter (\msg -> isValid msg model)
                |> List.head
    in
    case deck of
        ( head, headVisible ) :: tail ->
            [ Html.button
                (onClick
                    |> Maybe.map (\fun -> [ Events.onClick fun ])
                    |> Maybe.withDefault []
                )
                [ Html.text (toString head headVisible) ]
            , tail
                |> List.map
                    (\( card, visible ) ->
                        toString card visible
                    )
                |> String.concat
                |> Html.text
            ]
                |> Html.div []

        [] ->
            Html.div [] []


view : Model -> Document Msg
view model =
    let
        ( firstDeck, secondDeck ) =
            model.decks
    in
    { title = "Card Stash"
    , body =
        [ viewDeck { first = True } firstDeck model
        , viewDeck { first = False } secondDeck model
        , model.stash
            |> Maybe.map
                (\card ->
                    [ Html.text ("Stash: " ++ toString card True) ]
                        |> Html.button
                            ([ Unstash { first = False }, Unstash { first = True } ]
                                |> List.filter
                                    (\msg ->
                                        isValid msg model
                                    )
                                |> List.head
                                |> Maybe.map
                                    (\msg ->
                                        [ Events.onClick msg ]
                                    )
                                |> Maybe.withDefault []
                            )
                )
            |> Maybe.withDefault (Html.text "Stash:")
            |> List.singleton
            |> Html.div []
        , Html.button [ Events.onClick (GotSeed model.seed) ] [ Html.text "Restart" ]
        ]
    }


isValid : Msg -> Model -> Bool
isValid msg model =
    let
        getter isFirst =
            if isFirst then
                Tuple.first

            else
                Tuple.second

        stackable firstCard secondCard =
            (firstCard.face == secondCard.face)
                || (Face.next firstCard.face == Just secondCard.face)
                || (Face.prev firstCard.face == Just secondCard.face)
                || (Face.next secondCard.face == Just firstCard.face)
                || (Face.prev secondCard.face == Just firstCard.face)
    in
    case msg of
        GotSeed seed ->
            True

        Swap { first } ->
            case ( getter first model.decks, getter (not first) model.decks ) of
                ( ( firstCard, _ ) :: _, ( secondCard, _ ) :: _ ) ->
                    stackable firstCard secondCard

                ( ( firstCard, _ ) :: _, _ ) ->
                    True

                _ ->
                    False

        Stash { first } ->
            case getter first model.decks of
                ( firstCard, _ ) :: _ ->
                    case model.stash of
                        Nothing ->
                            True

                        Just secondCard ->
                            secondCard.face == firstCard.face

                _ ->
                    False

        Unstash { first } ->
            case ( model.stash, getter first model.decks ) of
                ( Just firstCard, ( secondCard, _ ) :: _ ) ->
                    stackable firstCard secondCard

                ( Just _, _ ) ->
                    True

                _ ->
                    False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        getter isFirst =
            if isFirst then
                Tuple.first

            else
                Tuple.second

        mapper isFirst =
            if isFirst then
                Tuple.mapFirst

            else
                Tuple.mapSecond
    in
    if isValid msg model then
        case msg of
            GotSeed seed ->
                ( initModel seed, Cmd.none )

            Swap { first } ->
                case getter first model.decks of
                    ( head, _ ) :: tail ->
                        ( { model
                            | decks =
                                model.decks
                                    |> mapper first
                                        (\_ ->
                                            case tail of
                                                ( secondHead, _ ) :: secondTail ->
                                                    ( secondHead, True ) :: secondTail

                                                [] ->
                                                    []
                                        )
                                    |> mapper (not first)
                                        (\list ->
                                            case list of
                                                [] ->
                                                    [ ( head, True ) ]

                                                ( secondHead, _ ) :: secondTail ->
                                                    if secondHead.face == head.face then
                                                        ( { face = head.face, amount = secondHead.amount + head.amount }, True )
                                                            :: secondTail

                                                    else
                                                        ( head, True ) :: list
                                        )
                          }
                        , Cmd.none
                        )

                    [] ->
                        ( model, Cmd.none )

            Stash { first } ->
                case getter first model.decks of
                    ( head, _ ) :: tail ->
                        ( { model
                            | decks =
                                model.decks
                                    |> mapper first
                                        (\_ ->
                                            case tail of
                                                ( secondHead, _ ) :: secondTail ->
                                                    ( secondHead, True ) :: secondTail

                                                [] ->
                                                    []
                                        )
                            , stash =
                                model.stash
                                    |> Maybe.map
                                        (\card ->
                                            { face = card.face, amount = card.amount + head.amount }
                                        )
                                    |> Maybe.withDefault head
                                    |> Just
                          }
                        , Cmd.none
                        )

                    [] ->
                        ( model, Cmd.none )

            Unstash { first } ->
                case model.stash of
                    Just card ->
                        ( { model
                            | decks =
                                model.decks
                                    |> mapper first
                                        (\list ->
                                            case list of
                                                [] ->
                                                    [ ( card, True ) ]

                                                ( secondHead, _ ) :: secondTail ->
                                                    if secondHead.face == card.face then
                                                        ( { face = card.face, amount = secondHead.amount + card.amount }, True )
                                                            :: secondTail

                                                    else
                                                        ( card, True ) :: list
                                        )
                            , stash =
                                Nothing
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

    else
        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

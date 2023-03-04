module Main exposing (..)

import Action exposing (Action)
import Browser exposing (Document)
import Card exposing (Card)
import Dict
import Game exposing (Game)
import Game.Area
import Game.Card
import Game.Entity
import Html
import Html.Attributes
import Html.Events
import Layout
import Random exposing (Generator, Seed)
import Set
import Time
import View


type alias Model =
    { actions : List Action
    , game : Game
    , selected : Maybe Card
    , seed : Seed
    , showIntro : Bool
    }


type Msg
    = NextAction
    | SelectedCardType (Maybe Card)
    | Submit
    | GotSeed Seed
    | HideIntro
    | Restart


initialModel : Seed -> Model
initialModel seed =
    { actions = [ Action.Shuffle, Action.DrawCards 7 ]
    , game = Game.init
    , selected = Nothing
    , seed = seed
    , showIntro = True
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( Random.initialSeed 42 |> initialModel
    , Random.independentSeed |> Random.generate GotSeed
    )


view : Model -> Document Msg
view model =
    let
        cardHeight =
            200

        cardWidth =
            cardHeight * 2 / 3

        spacing =
            32

        padding =
            32

        width =
            cardWidth * 3 + spacing * 2 + padding * 2
    in
    { title = "Enough For Now"
    , body =
        [ [ [ "Score: "
                ++ String.fromInt model.game.score
                |> Html.text
                |> Layout.el
                    [ Html.Attributes.style "padding" "16px 32px"
                    , Html.Attributes.style "background-color" "white"
                    , Html.Attributes.style "border-radius" "16px"
                    , Html.Attributes.style "border" "solid 1px rgba(0,0,0,0.2)"
                    ]
            , Html.text "Restart"
                |> Layout.buttonEl { onPress = Just Restart, label = "Restart" }
                    []
            ]
                |> Layout.row
                    [ Html.Attributes.style "width" (String.fromFloat (width - padding * 2) ++ "px")
                    , Html.Attributes.style "z-index" "1"
                    , Layout.spaceBetween
                    ]
          , [ model.game.hand
                |> Dict.values
                |> List.concat
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.filter
                    (\( _, card ) ->
                        model.selected /= Just card
                    )
                |> List.indexedMap
                    (\i ( cardId, card ) ->
                        ( cardId, card )
                            |> View.card [ Html.Events.onClick (SelectedCardType (Just card)) ]
                                True
                            |> Game.Entity.move
                                ( toFloat i
                                    * (cardWidth * 2 + spacing * 2)
                                    / ((model.game.hand
                                            |> Dict.values
                                            |> List.concat
                                            |> List.length
                                       )
                                        - 1
                                        |> toFloat
                                      )
                                , 0
                                )
                            |> Game.Entity.mapZIndex ((+) 50)
                    )
                |> Game.Area.pileAbove ( padding, cardHeight + spacing * 2 + padding )
                    ( "hand", \attrs -> [ Html.text "" ] |> Html.div attrs )
            , model.game.drawPile
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i ( cardId, card ) ->
                        View.card [] False ( cardId, card )
                            |> Game.Entity.move ( 0, toFloat i * -4 )
                    )
                |> Game.Area.pileAbove ( padding, padding )
                    ( "Draw Pile", \attrs -> Game.Card.empty attrs "Draw Pile" )
            , model.game.discardPile
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i tuple ->
                        View.card [] True tuple
                            |> Game.Entity.move ( 0, toFloat i * -4 )
                    )
                |> Game.Area.pileAbove ( cardWidth * 2 + spacing * 2 + padding, padding )
                    ( "Discard Pile", \attrs -> Game.Card.empty attrs "Discard Pile" )
            , model.game.graveyard
                |> List.filterMap (Game.getCardFrom model.game)
                |> List.indexedMap
                    (\i tuple ->
                        View.card [] True tuple
                            |> Game.Entity.move ( 0, toFloat i * -4 )
                    )
                |> Game.Area.pileAbove ( cardWidth + spacing + padding, padding )
                    ( "Graveyard", \attrs -> Layout.none |> Layout.el attrs )
            , [ ( "DrawPileInfo"
                , \attrs ->
                    if [] == model.game.drawPile then
                        Layout.none

                    else
                        model.game.drawPile
                            |> List.filterMap (Game.getCardFrom model.game)
                            |> List.foldl
                                (\( _, card ) ->
                                    Dict.update (Card.cardType card)
                                        (\maybe ->
                                            maybe
                                                |> Maybe.map ((+) 1)
                                                |> Maybe.withDefault 1
                                                |> Just
                                        )
                                )
                                Dict.empty
                            |> (\count ->
                                    Card.values
                                        |> List.filterMap
                                            (\card ->
                                                Dict.get (Card.cardType card) count
                                                    |> Maybe.map
                                                        (\amount ->
                                                            Card.emoji card
                                                                ++ " "
                                                                ++ String.fromInt amount
                                                                |> Html.text
                                                                |> Layout.el []
                                                        )
                                            )
                               )
                            |> Layout.row
                                (Layout.centered
                                    ++ [ Layout.spacing 4
                                       , Html.Attributes.style "background-color" "white"
                                       , Html.Attributes.style "padding" "16px 8px 8px 8px"
                                       , Html.Attributes.style "border-radius" "8px"
                                       , Html.Attributes.style "border" "solid 1px rgba(0,0,0,0.2)"
                                       ]
                                    ++ attrs
                                )
                            |> Layout.el [ Html.Attributes.style "width" (String.fromFloat cardWidth ++ "px"), Layout.contentCentered ]
                )
                    |> Game.Entity.new
                    |> Game.Entity.move ( padding, cardHeight - 8 + padding )
              ]
            , model.selected
                |> Maybe.map
                    (\cardType ->
                        model.game.hand
                            |> Dict.get (Card.cardType cardType)
                            |> Maybe.withDefault []
                            |> List.filterMap (Game.getCardFrom model.game)
                            |> (\list ->
                                    list
                                        |> List.indexedMap
                                            (\i ( cardId, card ) ->
                                                ( cardId, card )
                                                    |> View.card [ Html.Events.onClick Submit ] True
                                                    |> (\entity ->
                                                            if i < (List.length list - 2) then
                                                                entity
                                                                    |> Game.Entity.move ( toFloat i * 8 - 50, 50 )
                                                                    |> Game.Entity.rotate (-pi / 4 + pi * toFloat i / 16)
                                                                    |> Game.Entity.mapZIndex ((+) (90 + i))

                                                            else
                                                                entity
                                                                    |> Game.Entity.move ( toFloat (i - (List.length list - 2)) * 32 - 16, 0 )
                                                                    |> Game.Entity.mapZIndex ((+) (100 + i))
                                                       )
                                                    |> Game.Entity.mapCustomTransformations ((++) [ Game.Entity.scale 1.5 ])
                                            )
                               )
                            |> Game.Area.pileAbove ( cardWidth + spacing + padding, padding + cardHeight / 2 )
                                ( "selectedCard"
                                , \_ -> Layout.none
                                )
                            |> (::)
                                (( "Play both to activate"
                                 , \attrs ->
                                    Html.text "Play both to activate"
                                        |> Layout.el []
                                        |> Layout.el
                                            (attrs
                                                ++ Layout.centered
                                                ++ [ Html.Attributes.style "font-size" "2em"
                                                   , Html.Attributes.style "font-weight" "bold"
                                                   , Html.Attributes.style "width" ((String.fromFloat <| cardWidth * 3 + spacing * 2 + padding * 2) ++ "px")
                                                   , Html.Events.onClick (SelectedCardType Nothing)
                                                   ]
                                            )
                                 )
                                    |> Game.Entity.new
                                    |> Game.Entity.move ( 0, padding + cardHeight * 2 )
                                    |> Game.Entity.mapZIndex ((+) 102)
                                )
                    )
                |> Maybe.withDefault []
            ]
                |> List.concat
                |> Game.Area.toHtml
                    [ Html.Attributes.style "height" (String.fromInt (cardHeight * 2 + spacing * 2 + padding * 2) ++ "px")
                    , Html.Attributes.style "width" (String.fromFloat width ++ "px")
                    ]
          ]
            |> Layout.column (Html.Attributes.style "height" "100%" :: Layout.centered)
        , (if model.showIntro then
            [ Html.text "How to Play"
                |> Layout.el [ Html.Attributes.style "font-size" "2em" ]
            , [ "Survive as many turns as possible."
              , "Eat Food" ++ Card.emoji Card.Food ++ " to end your turn and draw 7 new cards."
              , "Get more Food " ++ Card.emoji Card.Food ++ " by making a fire with Stones " ++ Card.emoji Card.Stone ++ "."
              , "If you don't have the right cards, use Wood " ++ Card.emoji Card.Wood ++ " to draw more."
              , "Each turn more Fear" ++ Card.emoji Card.Fear ++ " will creep into your deck."
              , "You will have to embrace the fears to survive."
              ]
                |> List.map Html.text
                |> List.map (Layout.el [])
                |> Layout.column []
            , Html.text "Hint: Try to play just enough cards to survive the next round." |> Layout.el []
            ]
                |> Layout.column
                    [ Html.Attributes.style "background-color" "white"
                    , Html.Attributes.style "padding" "32px"
                    , Html.Attributes.style "border-radius" "32px"
                    , Layout.spacing 32
                    ]
                |> Layout.el
                    (Layout.centered
                        ++ [ Html.Attributes.style "z-index" "101"
                           , Html.Attributes.style "position" "relative"
                           , Html.Attributes.style "top" "0"
                           , Layout.fill
                           ]
                    )

           else
            Layout.none
          )
            |> Layout.el
                ((if model.selected /= Nothing || model.showIntro then
                    [ Html.Attributes.style "backdrop-filter" "blur(4px)"
                    ]

                  else
                    [ Html.Attributes.style "backdrop-filter" "blur(0px)"
                    ]
                 )
                    ++ [ Html.Attributes.style "height" "100%"
                       , Html.Attributes.style "width" "100%"
                       , Html.Attributes.style "transition" "backdrop-filter 2s"
                       , Html.Events.onClick (SelectedCardType Nothing)
                       , Html.Attributes.style "position" "absolute"
                       , Html.Attributes.style "top" "0"
                       , Html.Attributes.style "z-index"
                            (if model.selected /= Nothing || model.showIntro then
                                "100"

                             else
                                "0"
                            )
                       ]
                )
        , Html.node "style" [] [ """
:root,body {
    
    height:100%;background-color:#f4f3ee
}

:root {
    --button-color:  """ ++ View.green ++ """
}



button {
  background-color: var(--button-color);
  border-width: 0px;
  border-radius: 16px;
  padding: 16px 32px;
  font-weight: bold;
}

button:hover {
  filter: brightness(0.95)
}

button:focus {
  filter: brightness(0.90)
}

button:active {
  filter: brightness(0.80)
}
        """ |> Html.text ]
        ]
    }


applyGeneratorTo : Model -> Generator Model -> Model
applyGeneratorTo model gen =
    let
        ( m, seed ) =
            Random.step gen model.seed
    in
    { m | seed = seed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextAction ->
            case model.actions of
                head :: tail ->
                    ( model.game
                        |> Game.applyAction head
                        |> Random.map
                            (\( game, actions ) ->
                                { model | game = game, actions = actions ++ tail }
                            )
                        |> applyGeneratorTo model
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        Submit ->
            model.selected
                |> Maybe.map
                    (\cardType ->
                        case
                            model.game.hand
                                |> Dict.get (Card.cardType cardType)
                                |> Maybe.withDefault []
                                |> List.reverse
                        of
                            cardId1 :: cardId2 :: _ ->
                                ( { model
                                    | game = model.game |> Game.moveToArea (Set.fromList [ cardId1, cardId2 ])
                                    , actions =
                                        [ Action.Wait
                                        , Action.ClearArea
                                        ]
                                            ++ Action.fromCard cardType
                                            ++ model.actions
                                    , selected = Nothing
                                  }
                                , Cmd.none
                                )

                            _ ->
                                ( model, Cmd.none )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        SelectedCardType cardType ->
            if model.actions == [] then
                ( { model | selected = cardType, showIntro = False }, Cmd.none )

            else
                ( model, Cmd.none )

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        HideIntro ->
            ( { model | showIntro = False }, Cmd.none )

        Restart ->
            ( initialModel model.seed, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 500 (\_ -> NextAction)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module Flask.Main exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Flask.Data exposing (baseMult, cardHeight, cardWidth, fontMult, spacingMult)
import Flask.Data.Base as Base exposing (Base(..))
import Flask.Data.Composition as Composition exposing (Composition)
import Flask.Data.Effect as Effect exposing (Effect(..))
import Flask.Data.Element as Elem
import Flask.View.Card as Card
import Framework
import Framework.Button as Button
import Framework.Card
import Framework.Color as Color
import Framework.Grid as Grid
import Framework.Input as Input
import Html exposing (Html)


type alias Card =
    { name : String
    , composition : Composition
    , hasDesc : Bool
    , img : String
    , amount : Int
    }


emptyCard : Int -> Card
emptyCard amount =
    { name = " "
    , composition = Composition.empty
    , hasDesc = True
    , img = ""
    , amount = amount
    }


type alias Model =
    { cards : Array Card
    , cardsPerPage : Int
    , showGui : Bool
    , editing : Int
    }


type Msg
    = ToggledShowGui
    | AddComponent Base
    | RemoveComponent Base
    | IncreaseAmount
    | DecreaseAmount
    | ChangedName String
    | ChangedImg String
    | DeletedSelected
    | AddCard
    | Selected Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        editedCard : Card
        editedCard =
            model.cards
                |> Array.get model.editing
                |> Maybe.withDefault (emptyCard 1)
    in
    case msg of
        ToggledShowGui ->
            ( { model | showGui = not model.showGui }, Cmd.none )

        IncreaseAmount ->
            ( { model
                | cards =
                    model.cards
                        |> Array.set model.editing
                            { editedCard
                                | amount = min 3 <| editedCard.amount + 1
                            }
              }
            , Cmd.none
            )

        DecreaseAmount ->
            ( { model
                | cards =
                    model.cards
                        |> Array.set model.editing
                            { editedCard
                                | amount = max 1 <| editedCard.amount - 1
                            }
              }
            , Cmd.none
            )

        AddComponent base ->
            ( { model
                | cards =
                    model.cards
                        |> Array.set model.editing
                            { editedCard
                                | composition =
                                    editedCard.composition
                                        |> Composition.insert base
                            }
              }
            , Cmd.none
            )

        RemoveComponent base ->
            ( { model
                | cards =
                    model.cards
                        |> Array.set model.editing
                            { editedCard
                                | composition =
                                    editedCard.composition
                                        |> Composition.remove base
                            }
              }
            , Cmd.none
            )

        ChangedName name ->
            ( { model 
                | cards = model.cards 
                    |> Array.set model.editing 
                        { editedCard | name = name } 
              }
            , Cmd.none
            )
        ChangedImg img ->
            ( { model 
                | cards = model.cards 
                    |> Array.set model.editing 
                        { editedCard | img = img } 
              }
            , Cmd.none
            )

        DeletedSelected ->
            ( { model
                | editing = max 0 <| model.editing - 1
                , cards =
                    Array.append
                        (model.cards |> Array.slice 0 model.editing)
                        (model.cards |> Array.slice (model.editing + 1) (model.cards |> Array.length))
              }
            , Cmd.none
            )
        
        AddCard ->
            ( { model
                | editing = model.cards |> Array.length
                , cards =  model.cards |> Array.push (emptyCard 1)
                }
            , Cmd.none
            )
        
        Selected index ->
            ( { model | editing = index},Cmd.none)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards =
            [ { name = "Double Sale"
              , composition = [ Y1, Y1 ] |> Composition.fromList
              , hasDesc = True
              , img = ""
              , amount = 3
              }
            , { name = "Double Exchange"
              , composition = [ G1, G1 ] |> Composition.fromList
              , hasDesc = True
              , img = ""
              , amount = 3
              }
            , { name = "Returing Attack"
              , composition = [ R1, Y2 ] |> Composition.fromList
              , hasDesc = True
              , img = ""
              , amount = 3
              }
            ]
                |> Array.fromList
      , cardsPerPage = 10
      , showGui = True
      , editing = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


card : Card -> List (Element msg)
card ({ name, hasDesc, img, amount } as config) =
    let
        composition =
            config.composition
                |> Composition.toList
                |> List.map Base.card
                |> List.foldl
                    (\input { cost, effects } ->
                        { cost = ( input.cost ++ (cost |> Tuple.first), input.discard + (cost |> Tuple.second) )
                        , effects = input.effect :: effects
                        }
                    )
                    { cost = ( [], 0 ), effects = [] }
    in
    Card.view
        { name = name
        , cost = composition.cost |> Tuple.mapFirst Elem.order
        , effects = composition.effects |> Effect.simplify
        , hasDesc = hasDesc
        , code = config.composition |> Composition.toString
        , img = img
        }
        |> List.repeat amount


view : Model -> Html Msg
view model =
    let
        cards : List (Element Msg)
        cards =
            model.cards
                |> Array.toList
                |> (if model.showGui then
                        List.indexedMap
                            (\i c ->
                                c
                                    |> card
                                    |> List.head
                                    |> Maybe.map
                                        ( \label ->
                                            Input.button [Element.alignLeft] <|
                                            {label = Element.el
                                            (Color.info ++ [ Element.inFront <|
                                                Element.el
                                                    (if i == model.editing
                                                    then
                                                        Framework.Card.simple
                                                        ++ Color.info
                                                        ++ [ Border.roundEach
                                                                { topLeft = 0
                                                                , bottomLeft =0
                                                                , topRight = round <| 16 * baseMult
                                                                , bottomRight = round <| 16 * baseMult
                                                                }
                                                           , Element.centerY
                                                           , Element.alignLeft
                                                           ]
                                                    else
                                                        Framework.Card.simple
                                                        ++ Color.info
                                                        ++ [ Border.roundEach
                                                                { topLeft = round <| 16 * baseMult
                                                                , bottomLeft = round <| 16 * baseMult
                                                                , topRight = 0
                                                                , bottomRight = 0
                                                                }
                                                           , Element.centerY
                                                           , Element.alignRight
                                                           ]
                                                    )
                                                <|
                                                    Element.text (String.fromInt c.amount ++ "x")
                                            , Element.width <| Element.px <| cardWidth
                                            , Element.height <| Element.px <| cardHeight
                                            
                                            ] ++ (  if i == model.editing
                                                    then 
                                                        [ Border.color <| Color.cyan
                                                        , Border.width <| round <| 2 * spacingMult
                                                        ]
                                                    else 
                                                        []
                                                )
                                            ) <| label
                                            , onPress = Just (Selected i)
                                            }
                                            
                                            
                                        )
                            )
                            >> List.filterMap identity

                    else
                        List.map card >> List.concat
                   )

        emptyCards : Int
        emptyCards =
            model.cardsPerPage - (cards |> List.length) |> modBy model.cardsPerPage

        displayCards : Element Msg
        displayCards =
            Element.paragraph [ Element.width <| Element.fill ] <|
                List.concat
                    [ cards
                    , if model.showGui then
                        [ Element.el
                            [ Element.height <| Element.px <| cardHeight
                            , Element.alignLeft
                            , Element.width <| Element.px <| cardWidth
                            , Element.inFront <|
                                Input.button
                                    (Button.simple
                                        ++ Color.success
                                        ++ [ Border.rounded <| round <| 16 * baseMult
                                           , Element.centerY
                                           , Element.alignLeft
                                           ]
                                    )
                                <|
                                    { onPress = Just <| AddCard
                                    , label = Element.text <| "+"
                                    }
                            , Element.centerY
                            ]
                          <| Element.none
                        ]

                      else
                        emptyCard emptyCards |> card
                    ]

        editedCard : Card
        editedCard =
            model.cards
                |> Array.get model.editing
                |> Maybe.withDefault (emptyCard 1)

        numberInput : { onIncrease : msg, onDecrease : msg, value : Int, label : String } -> Element msg
        numberInput { onDecrease, onIncrease, value, label } =
            Element.row [ Element.spacing 5, Element.width <| Element.fill ] <|
                [ Element.el Input.label <| Element.text label
                , Element.row ([ Element.width <| Element.fill ] ++ Grid.simple)
                    [ Input.button
                        ([ Element.width <| Element.fill, Element.alignLeft ] ++ Button.simple)
                        { onPress = Just <| onDecrease
                        , label = Element.text <| "-"
                        }
                    , Element.el [ Font.center, Element.width <| Element.fillPortion 2 ] <|
                        Element.text <|
                            String.fromInt <|
                                value
                    , Input.button
                        ([ Element.width <| Element.fill, Element.alignRight ]
                            ++ Button.simple
                            ++ Color.success
                        )
                        { onPress = Just <| onIncrease
                        , label = Element.text <| "+"
                        }
                    ]
                ]
    in
    (if model.showGui then
        Framework.layout [] << Element.el Framework.container

     else
        Element.layout []
    )
    <|
        if model.showGui then
            Element.row (Grid.spacedEvenly ++ [ Element.width Element.fill ]) <|
                [ Element.el
                    [ Element.width Element.fill] <|
                    displayCards
                , Element.column (Grid.simple ++ [ Element.width Element.shrink, Element.alignRight ]) <|
                    [ Element.column (Framework.Card.simple ++ Grid.simple) <|
                        (editedCard.composition
                            |> (\{ g1, g2, r1, r2, b1, b2, y1, y2 } ->
                                    [ Input.text Input.simple
                                        { onChange = ChangedName
                                        , text = editedCard.name
                                        , placeholder = Nothing
                                        , label = Input.labelLeft Input.label <| Element.text "Name"
                                        }
                                    , Input.text Input.simple
                                        { onChange = ChangedImg
                                        , text = editedCard.img
                                        , placeholder = Nothing
                                        , label = Input.labelLeft Input.label <| Element.text "Image Link"
                                        }
                                    , numberInput
                                        { onIncrease = IncreaseAmount
                                        , onDecrease = DecreaseAmount
                                        , value = editedCard.amount
                                        , label = "Amount"
                                        }
                                    , Input.checkbox [] <|
                                        { onChange =
                                            \b ->
                                                (if b then
                                                    AddComponent

                                                 else
                                                    RemoveComponent
                                                )
                                                    G2
                                        , icon = Input.defaultCheckbox
                                        , checked = g2
                                        , label = Input.labelLeft Input.label <| Element.text "Plant"
                                        }
                                    , Input.checkbox [] <|
                                        { onChange =
                                            \b ->
                                                (if b then
                                                    AddComponent

                                                 else
                                                    RemoveComponent
                                                )
                                                    Y2
                                        , icon = Input.defaultCheckbox
                                        , checked = y2
                                        , label = Input.labelLeft Input.label <| Element.text "Reboot"
                                        }
                                    , numberInput
                                        { onIncrease = AddComponent B2
                                        , onDecrease = RemoveComponent B2
                                        , value = b2
                                        , label = "Invent"
                                        }
                                    , numberInput
                                        { onIncrease = AddComponent B1
                                        , onDecrease = RemoveComponent B1
                                        , value = b1
                                        , label = "Research"
                                        }
                                    , numberInput
                                        { onIncrease = AddComponent Y1
                                        , onDecrease = RemoveComponent Y1
                                        , value = y1
                                        , label = "Sale"
                                        }
                                    , numberInput
                                        { onIncrease = AddComponent G1
                                        , onDecrease = RemoveComponent G1
                                        , value = g1
                                        , label = "Exchange"
                                        }
                                    , numberInput
                                        { onIncrease = AddComponent R1
                                        , onDecrease = RemoveComponent R1
                                        , value = r1
                                        , label = "Attack"
                                        }
                                    , numberInput
                                        { onIncrease = AddComponent R2
                                        , onDecrease = RemoveComponent R2
                                        , value = r2
                                        , label = "Sabotage"
                                        }
                                    , Input.button (Button.simple ++ Color.danger ++ [ Element.alignRight ]) <|
                                        { onPress = Just DeletedSelected
                                        , label = Element.text <| "Remove"
                                        }
                                    ]
                               )
                        )
                    , Input.button (Button.simple ++ Color.primary) <|
                        { onPress = Just ToggledShowGui
                        , label = Element.text <| "Drucken"
                        }
                    ]
                ]

        else
            Element.column [ Element.spacing <| round <| 2 * spacingMult, Element.width Element.fill ] <|
                [ displayCards
                , Input.button (Button.simple ++ Color.primary) <|
                    { onPress = Just ToggledShowGui
                    , label = Element.text <| "Bearbeiten"
                    }
                ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

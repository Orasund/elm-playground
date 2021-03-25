module HermeticMind.BraidSigil exposing (..)

import Browser
import Element
import Element.Background as Background
import Element.Font as Font
import HermeticMind.Data.Alphabet as Alphabet
import HermeticMind.View.BraidSigil as BraidSigil
import Html exposing (Html)
import List.Extra as List
import Widget
import Widget.Customize as Customize
import Widget.Material as Material
import Widget.Material.Typography as Typography



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


zoom : number
zoom =
    2



--4


radius : number
radius =
    60


size : number
size =
    radius * 3


width : number
width =
    size


height : Float
height =
    size * 1.25


withCircle : Bool
withCircle =
    True


isGerman : Bool
isGerman =
    False


interactive : Bool
interactive =
    True


debugMode : Bool
debugMode =
    False


withRunes : Bool
withRunes =
    False


withText : Bool
withText =
    True


withBorder : Bool
withBorder =
    False


paths =
    --[ "Glueck", "Liebe", "Geld", "Erfolg" ]
    --[ "Fortune", "Love", "Money", "Success", "Health", "Protection", "Energy", "Divination" ]
    [ "Gegenwart", "Ziel", "Kraft", "Bedeutung", "Dankbarkeit", "Werte", "Gedanken", "Zukunft" ]



{--[ "teat"
    , "aaaasssd"
    , "bede"
    , "uede"
    , "ebebebebe"
    , "aeiaei"
    , "aeiou"
    ]--}
{--[ "Gegenwart"
        , "Ziel"
        , "Werte"
        , "Kraft"
        , "Bedeutung"
        , "Dankbarkeit"
        , "Gedanken"
        , "Zukunft"
        ]--}
--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init () =
    ( "", Cmd.none )



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = ChangedText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ChangedText string ->
            ( string, Cmd.none )



--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    let
        viewFun =
            BraidSigil.view
                { width = width
                , height = height
                , radius = radius
                , zoom = zoom
                , asAlphabet =
                    if isGerman then
                        Alphabet.german

                    else
                        Alphabet.english
                , withCircle = withCircle
                , debugMode = debugMode
                , withRunes = withRunes
                , withText = withText
                , withBorder = withBorder
                }
    in
    if interactive then
        [ "Sigil Generator"
            |> Element.text
            |> Element.el Typography.h6
            |> Widget.asItem
        , Widget.fullBleedItem (Material.fullBleedItem Material.defaultPalette)
            { text = "Write a single word into the input field."
            , onPress = Nothing
            , icon = always Element.none
            }
        , Widget.textInput (Material.textInput Material.defaultPalette)
            { chips = []
            , text = model
            , placeholder = Nothing
            , label = "Word"
            , onChange = ChangedText
            }
            |> Widget.asItem
        , Widget.divider (Material.fullBleedDivider Material.defaultPalette)
        , viewFun
            model
            |> Element.html
            |> Element.el [ Element.centerX ]
            |> Widget.asItem
        , Widget.divider (Material.fullBleedDivider Material.defaultPalette)
        , [ Element.text "This work is licensed under a "
          , Element.link [ Font.underline, Font.color <| Element.rgb255 100 80 146 ]
                { url = "https://creativecommons.org/licenses/by-nc/4.0/"
                , label = Element.text "Creative Commons Attribution-NonCommercial 4.0 International License"
                }
          , Element.text " by "
          , Element.link [ Font.underline, Font.color <| Element.rgb255 100 80 146 ]
                { url = "https://www.etsy.com/shop/HermeticMind"
                , label = Element.text "DI Lucas Payr"
                }
          , Element.text "."
          ]
            |> Element.paragraph []
            |> Widget.asItem
        ]
            |> Widget.itemList
                (Material.cardColumn Material.defaultPalette
                    |> Customize.elementColumn
                        [ Element.width <| Element.px <| round <| zoom * width * 1.5
                        , Element.centerX
                        , Element.centerY
                        ]
                    |> Customize.mapContent
                        (Customize.element
                            [ Element.width <| Element.px <| round <| zoom * width * 1.5
                            ]
                        )
                )
            |> Element.layout [ Background.color <| Element.rgb255 64 64 64 ]

    else
        paths
            |> List.map viewFun
            |> Html.div []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module Singularis.Page.Ai exposing (Model, Msg, init, subscriptions, update, view)

import Element exposing (Element)
import Element.Background as Background
import Element.Input as Input
import Random exposing (Seed)
import Singularis.Data exposing (maxInformation)
import Singularis.View.Ai as Ai
import Singularis.View.Element as Element
import Time


type alias Model =
    { seed : Seed
    , time : Float
    , symmetry : Float
    , information : Int
    , infoPerSec : Int
    , energy : Int
    , base : Int
    , lastCicleSym : Float
    , lastCicleInfo : Int
    }


type Msg
    = Tick
    | SecPassed
    | MinPassed
    | SymmetryChanged Float
    | InformationChanged Int
    | BaseChanged Int
    | InfoPerSecChanged Int
    | EnergyChanged Int


init : Seed -> Model
init seed =
    { seed = seed
    , time = 0
    , symmetry = 0
    , information = 2
    , base = 3
    , infoPerSec = 0
    , energy = 0
    , lastCicleSym = 0
    , lastCicleInfo = 2
    }


update : Msg -> Model -> Model
update msg ({ time } as model) =
    case msg of
        Tick ->
            { model | time = time + 0.02 }

        SecPassed ->
            {- information = energy^2 -}
            let
                symmetry : Float
                symmetry =
                    if model.infoPerSec == 0 then
                        0

                    else if model.information > model.energy ^ 2 then
                        (toFloat <| model.energy ^ 2)
                            / (toFloat <| (+) model.infoPerSec <| model.information)

                    else if model.energy ^ 2 > model.information then
                        (toFloat <| model.information)
                            / (toFloat <| (+) model.infoPerSec <| model.energy ^ 2)

                    else if (model.information == 0) && (model.energy ^ 2 == 0) then
                        0

                    else
                        1
            in
            { model
                | symmetry = symmetry
                , information =
                    model.information
                        |> (+)
                            -(round <|
                                (*) (1 - symmetry + model.symmetry / 2) <|
                                    toFloat <|
                                        model.energy
                             )
                        |> (+) ((*) model.infoPerSec <| round <| symmetry)
                        |> (+) model.infoPerSec
                        |> clamp 0 maxInformation
            }

        MinPassed ->
            { model
                | lastCicleSym = model.symmetry
                , lastCicleInfo = model.information
            }

        SymmetryChanged symmetry ->
            { model | symmetry = symmetry }

        InformationChanged information ->
            { model | information = information }

        BaseChanged base ->
            { model | base = base }

        InfoPerSecChanged infoPerSec ->
            { model | infoPerSec = infoPerSec }

        EnergyChanged energy ->
            { model | energy = energy }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 (always Tick)
        , Time.every 1000 (always SecPassed)
        , Time.every 10000 (always MinPassed)
        ]


view : Float -> Model -> Element Msg
view scale { seed, time, symmetry, information, base, infoPerSec, energy, lastCicleSym, lastCicleInfo } =
    Element.column [ Element.centerX, Element.width <| Element.fill ] <|
        [ Element.section scale <| "Artifical Singularity"
        , Element.row
            [ Element.centerX
            , Element.width <| Element.fill
            , Element.spaceEvenly
            ]
          <|
            [ Element.column [] <|
                [ Element.subsection scale <| "Remaining"
                , Element.section scale <| String.fromInt <| maxInformation - information
                ]
            , Element.el [] <|
                Element.html <|
                    Ai.view scale
                        { time = time
                        , seed = seed
                        , symmetry = lastCicleSym
                        , base = base
                        }
                    <|
                        lastCicleInfo
            , Element.column [] <|
                [ Element.subsection scale <| "Symmetry"
                , Element.section scale <| (String.fromInt <| round <| (*) 100 <| symmetry) ++ "%"
                ]
            ]
        , Input.radioRow
            [ Element.spaceEvenly
            , Element.width <| Element.fill
            ]
            { onChange = BaseChanged
            , options =
                [ Input.option 3 <| Element.text <| "Spirit"
                , Input.option 4 <| Element.text <| "Matter"
                , Input.option 6 <| Element.text <| "Knowledge"
                ]
            , selected = Just base
            , label = Input.labelLeft [] <| Element.text <| "Base "
            }
        , Element.slider scale
            { onChange = round >> InfoPerSecChanged
            , label = "Data Per Sec"
            , min = 0
            , max = 2 ^ 4
            , value = toFloat <| infoPerSec
            }
        , Element.slider scale
            { onChange = round >> EnergyChanged
            , label = "Nodes"
            , min = 0
            , max = 2 ^ 6
            , value = toFloat <| energy
            }
        , Element.paragraph [] <|
          [ Element.text <| "symmetry denotes how much data can be processed. A Intelligence can overfit, meaning to much data is feed to fast. in that case the remaining data will increase (instead of decresing). Nodes allow for better processing power."

          ]
        ]

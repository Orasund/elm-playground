module OracleCards.BraidSigil exposing (..)

import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import OracleCards.Data.Alphabet as Alphabet
import OracleCards.View.BraidSigil as BraidSigil
import Pixels
import Point2d
import StaticArray
import Svg
import Svg.Attributes as Attributes



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


zoom =
    4


radius =
    60


size =
    radius * 3


width =
    size


height =
    size


withCircle =
    True


isGerman =
    True



--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------


asAlphabet =
    if isGerman then
        Alphabet.german

    else
        Alphabet.english


main : Html msg
main =
    let
        paths =
            {--[ "Glueck", "Liebe", "Geld", "Erfolg" ]--}
            [ "Gegenwart"
            , "Ziel"
            , "Kraft"
            , "Bedeutung"
            , "Dankbarkeit"
            , "Werte"
            , "Gedanken"
            , "Zukunft"
            ]

        options =
            { width = width
            , height = height
            , radius = radius
            }
    in
    paths
        |> List.map
            (\string ->
                [ case
                    string
                        |> String.toList
                        |> List.map asAlphabet
                  of
                    head :: nextIndex :: tail ->
                        nextIndex
                            :: tail
                            |> List.foldl
                                (\i2 ( state, out ) ->
                                    let
                                        ( newState, newOut ) =
                                            BraidSigil.line options
                                                state
                                                i2
                                    in
                                    ( newState
                                    , newOut :: out
                                    )
                                )
                                (let
                                    ( newState, newOut ) =
                                        BraidSigil.line options
                                            (BraidSigil.init options
                                                head
                                                nextIndex
                                            )
                                            head
                                 in
                                 ( newState
                                 , newOut
                                    :: []
                                   --[ BraidSigil.initCircle options head nextIndex ]
                                 )
                                )
                            |> (\( state, out ) ->
                                    let
                                        ( newState, newOut ) =
                                            BraidSigil.line options
                                                state
                                                head
                                    in
                                    ( newState
                                    , newOut :: out
                                    )
                               )
                            |> (\( state, out ) ->
                                    (BraidSigil.line options
                                        { state
                                            | visited = state.visited |> StaticArray.set head -1
                                        }
                                        head
                                        |> Tuple.second
                                    )
                                        :: BraidSigil.initCircle options head nextIndex
                                        :: out
                               )
                            |> List.concat
                            |> (if withCircle then
                                    List.append
                                        (Circle2d.atPoint (Point2d.pixels (size / 2) (size / 2))
                                            (Pixels.pixels <| radius)
                                            |> Svg.circle2d
                                                [ Attributes.fill <| "none"
                                                , Attributes.stroke <| "black"
                                                , Attributes.strokeWidth <| String.fromFloat <| BraidSigil.strokeWidth
                                                ]
                                            |> List.singleton
                                        )

                                else
                                    identity
                               )

                    _ ->
                        []
                ]
                    |> List.concat
                    |> Svg.svg
                        [ Attributes.width <| (String.fromFloat <| zoom * size) ++ "px"
                        , Attributes.height <| (String.fromFloat <| zoom * size) ++ "px"
                        , Attributes.version <| "1.1"
                        , Attributes.viewBox <|
                            "0 0 "
                                ++ String.fromFloat size
                                ++ " "
                                ++ String.fromFloat size
                        ]
            )
        |> Html.div []

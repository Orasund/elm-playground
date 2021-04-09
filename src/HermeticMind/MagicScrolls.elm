module HermeticMind.MagicScrolls exposing (main)

import Arc2d
import Direction2d
import Element
import Element.Background as Background
import Element.Font as Font
import Geometry.Svg as Svg
import HermeticMind.Data.Alphabet as Alphabet
import HermeticMind.Data.Turtle as Turtle
import HermeticMind.GreekMagicSymbols as GreekMagicSymbols
import HermeticMind.View.BraidSigil as BraidSigil
import HermeticMind.View.MagicSquareSigil as MagicSquareSigil
import Html exposing (Html)
import LineSegment2d
import List.Extra as List
import Point2d
import String.Extra as String
import Svg
import Svg.Attributes as Attributes



--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------


type Font
    = Mystical
    | Hebrew
    | GreekMagic


type Sigil
    = MagicSquare
    | Braid


inputText =
    "Alive"


isGerman =
    False


asAlphabet =
    if isGerman then
        Alphabet.german

    else
        Alphabet.english


sigilRadius =
    180


sigilSize =
    sigilRadius * 2.5


width =
    841


height =
    toFloat <| floor <| width * sqrt 2



--1189
--


globalPadding =
    70


font : Font
font =
    Hebrew


sigilStyle : Sigil
sigilStyle =
    MagicSquare



--------------------------------------------------------------------------------
-- Main Program
--------------------------------------------------------------------------------


sigil : String -> Html msg
sigil =
    case sigilStyle of
        Braid ->
            BraidSigil.view
                { width = sigilSize
                , height = sigilSize
                , radius = sigilRadius
                , zoom = 1
                , asAlphabet = asAlphabet
                , withCircle = True
                , debugMode = False
                , withRunes = False
                , withText = False
                , withBorder = False
                }

        MagicSquare ->
            MagicSquareSigil.view
                { size = sigilSize
                , zoom = 1
                , strokeWidth = 5
                , alphabet = asAlphabet
                }


charToHebrew : Char -> Maybe String
charToHebrew char =
    case char of
        'B' ->
            Just "בּ"

        'V' ->
            Just "ב"

        'G' ->
            Just "גּ"

        'D' ->
            Just "דּ"

        'H' ->
            Just "ה"

        'W' ->
            Just "ו"

        'U' ->
            Just "וּ"

        'O' ->
            Just "וֹ"

        'Z' ->
            Just "ז"

        'T' ->
            Just "ט"

        'Y' ->
            Just "י"

        'I' ->
            Just "י"

        'E' ->
            Just "י"

        'K' ->
            Just "ך"

        'L' ->
            Just "ל"

        'M' ->
            Just "ם"

        'N' ->
            Just "נ"

        'S' ->
            Just "ס"

        'P' ->
            Just "פ"

        'F' ->
            Just "פ"

        'R' ->
            Just "ר"

        _ ->
            Nothing


stringToSyllables : String -> List String
stringToSyllables =
    String.toList
        >> List.foldl
            (\char ( current, out ) ->
                if
                    (char == 'a')
                        || (char == 'i')
                        || (char == 'o')
                        || (char == 'u')
                        || (char == 'e')
                then
                    ( [], (char :: current |> List.reverse) :: out )

                else
                    ( char :: current, out )
            )
            ( [], [] )
        >> (\( a, b ) ->
                if a |> List.isEmpty then
                    b

                else
                    (a |> List.reverse) :: b
           )
        >> List.reverse
        >> List.map String.fromList
        >> Debug.log "out"


spell : String -> String
spell string =
    let
        word =
            case font of
                Hebrew ->
                    string
                        |> String.toList
                        |> List.filterMap charToHebrew

                _ ->
                    string
                        |> String.toList
                        |> List.map (List.singleton >> String.fromList)

        length =
            word
                |> List.length

        wordList =
            word
                |> List.permutations
                |> List.tail
                |> Maybe.withDefault []
                |> List.map String.concat

        nrOfWords =
            wordList |> List.length

        maxCharCount =
            400
    in
    wordList
        |> List.take (maxCharCount // length)
        |> String.join " "
        |> String.toTitleCase


readableSpell : String -> String
readableSpell string =
    let
        length =
            string |> String.length

        wordList =
            string
                |> stringToSyllables
                |> List.permutations
                |> List.tail
                |> Maybe.withDefault []
                |> List.map String.concat

        nrOfWords =
            wordList |> List.length

        minCharCount =
            400
    in
    wordList
        |> List.repeat (minCharCount // (length * nrOfWords))
        |> List.concat
        |> String.join " "
        |> String.toTitleCase


border =
    let
        strokeWidth =
            5

        lineWidth =
            strokeWidth

        pointSize =
            5

        padding =
            globalPadding
    in
    { position = Point2d.unsafe { x = pointSize * 2, y = pointSize * 3 } --{ x = width / 2, y = padding + pointSize * 2 }
    , direction = Direction2d.positiveX
    , lineFun =
        \{ from, to } ->
            let
                segment =
                    LineSegment2d.from from to
            in
            [ segment
                |> Svg.lineSegment2d
                    [ Attributes.stroke "black"
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    , arcFun =
        \{ around, by, from } ->
            let
                arc =
                    Arc2d.sweptAround around by from
            in
            [ arc
                |> Svg.arc2d
                    [ Attributes.fill <| "none"
                    , Attributes.stroke <| "black"
                    , Attributes.strokeWidth <| String.fromFloat <| strokeWidth
                    ]
            ]
    }
        |> Turtle.forwardBy (width - padding * 2 + pointSize * 2)
        --(width / 2 - padding)
        |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.positiveY, radius = pointSize })
        |> Turtle.andThen (Turtle.forwardBy (height - padding * 2.5 + pointSize * 2))
        |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.negativeX, radius = pointSize })
        |> Turtle.andThen (Turtle.forwardBy (width - padding * 2 + pointSize * 2))
        |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.negativeY, radius = pointSize })
        |> Turtle.andThen (Turtle.forwardBy (height - padding * 2.5 + pointSize * 2))
        |> Turtle.andThen (Turtle.arcLeftTo { direction = Direction2d.positiveX, radius = pointSize })
        |> Tuple.second
        |> Svg.svg
            [ Attributes.width <| (String.fromFloat <| width - padding * 2 + pointSize * 6) ++ "px"
            , Attributes.height <| (String.fromFloat <| height - padding * 2.5 + pointSize * 6) ++ "px"
            , Attributes.version <| "1.1"
            ]


main : Html msg
main =
    [ case font of
        Hebrew ->
            Element.none
                |> Element.el
                    [ Element.width <| Element.fill
                    , Element.height <| Element.px 60
                    , inputText
                        |> String.toUpper
                        |> String.toList
                        |> List.filterMap charToHebrew
                        |> String.concat
                        |> Element.text
                        |> Element.el
                            [ Font.family
                                [ Font.typeface "Times New Roman"
                                ]
                            , Element.centerX
                            , Font.size 100
                            ]
                        |> Element.inFront
                    ]

        Mystical ->
            Element.none
                |> Element.el
                    [ Element.width <| Element.fill
                    , Element.height <| Element.px 40
                    , inputText
                        |> String.toUpper
                        |> Element.text
                        |> Element.el
                            [ Font.family
                                [ Font.typeface "Mycrazyfont"
                                ]
                            , Font.letterSpacing -10
                            , Element.centerX
                            , Font.size 100
                            ]
                        |> Element.inFront
                    ]

        GreekMagic ->
            inputText
                |> String.toUpper
                |> String.words
                |> List.map
                    (String.toList
                        >> List.filterMap (GreekMagicSymbols.fromChar 60 >> Maybe.map (Element.html >> Element.el []))
                        >> Element.row [ Element.spacing 1 ]
                    )
                |> Element.wrappedRow [ Element.centerX ]
                |> Element.el
                    [ Element.width <| Element.fill
                    ]
    , Element.none
        |> Element.el
            [ Element.height <| Element.px (sigilRadius * 2)
            , Element.width <| Element.fill
            , sigil inputText
                |> Element.html
                |> Element.el
                    [ Element.centerX
                    , Element.moveUp ((sigilSize - sigilRadius * 2) / 2)
                    ]
                |> Element.behindContent
            ]
    , [ case font of
            Hebrew ->
                inputText
                    |> String.toUpper
                    |> spell
                    |> Element.text
                    |> List.singleton
                    |> Element.paragraph
                        [ Font.family
                            [ Font.typeface "Times New Roman"
                            ]
                        , Font.size 34
                        , Font.center
                        ]

            Mystical ->
                inputText
                    |> String.toUpper
                    |> spell
                    |> Element.text
                    |> List.singleton
                    |> Element.paragraph
                        [ Font.family
                            [ Font.typeface "Mycrazyfont"
                            ]
                        , Font.size 20
                        , Font.center
                        , Font.letterSpacing -1
                        ]

            GreekMagic ->
                inputText
                    |> String.toUpper
                    |> spell
                    |> String.words
                    |> List.map
                        (String.toList
                            >> List.filterMap (GreekMagicSymbols.fromChar 16 >> Maybe.map (Element.html >> Element.el []))
                            >> Element.row [ Element.spacing 1, Element.paddingXY 0 2 ]
                        )
                    |> Element.wrappedRow [ Element.spaceEvenly, Element.centerX ]
      , inputText
            |> String.toLower
            |> readableSpell
            |> Element.text
            |> List.singleton
            |> Element.paragraph
                [ Font.family
                    [ Font.typeface "Dancing Script"
                    ]
                , Font.center
                ]
      ]
        |> Element.column [ Element.spaceEvenly, Element.height <| Element.fill ]
        |> Element.el
            [ Element.height <| Element.fill
            , Element.width <| Element.fill
            , Element.paddingXY (globalPadding // 2) 0
            ]
    ]
        |> Element.column
            [ Element.paddingXY 0 (globalPadding // 2)
            , Element.spacing 50

            --, Background.color (Element.rgb255 127 127 127)
            , Element.height <| Element.fill
            ]
        |> Element.el
            [ Element.width <| Element.px <| round <| width
            , Element.height <| Element.fill --Element.px <| round <| height --
            , Element.centerX
            , Background.color (Element.rgb255 255 255 255)
            , border
                |> Element.html
                |> Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                |> Element.behindContent
            , Element.clip
            , Element.padding globalPadding
            ]
        |> Element.layout
            [ Background.color (Element.rgb255 64 64 64)
            , Element.padding 0
            ]

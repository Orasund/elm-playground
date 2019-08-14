module Singularis.Data.Answer exposing (name, Answer, getAnswerImage,AnswerImage, AnswerType(..), list,fromQuestion,intToArkana,arkanaToAnswer)


type alias Answer =
    { nr : Int, flipped : Bool }

type alias AnswerImage =
    ( Maybe { cross : Bool, answerType : AnswerType }, Bool )

name : {nr: Int , flipped : Bool } -> String
name {nr,flipped} =
    let
        title : String
        title =
            case nr+1 of
                1 -> "Will"
                2 -> "Passion"
                3 -> "Action"
                4 -> "Realisation"
                5 -> "Inspriation"
                6 -> "Love"
                7 -> "Victory"
                8 -> "Balance"
                9 -> "Wisdom"
                10 -> "Fortune"
                11 -> "Strength"
                12 -> "Sacrifice"
                13 -> "Transformation"
                14 -> "Initiative"
                15 -> "Fate"
                16 -> "Decision"
                17 -> "Hope"
                18 -> "Illusion"
                19 -> "Happiness"
                20 -> "Restart"
                21 -> "Reward"
                _ -> "Error"
    in 
    "#"++(String.fromInt<| nr+1) ++ " "
        ++title
        ++if flipped then
            " (inverted)"
          else
            ""


fromQuestion : String -> Answer
fromQuestion =
    String.toList
        >> List.map Char.toCode
        >> List.sum
        >> modBy 42
        >> intToArkana

intToArkana : Int -> Answer
intToArkana nr =
    { nr = nr |> modBy 21, flipped = nr > 20 }

list : List AnswerImage
list =
    List.range 0 41
        |> List.map (intToArkana >> arkanaToAnswer)


type AnswerType
    = Empty
    | Vertical
    | VerticalDouble
    | Single
    | SingleAndVerticalTop
    | SingleAndVerticalBottom
    | SingleAndVerticalBoth
    | Double
    | DoubleAndVertical
    | DoubleAndVerticalBoth



answerToArkana : AnswerImage -> Answer
answerToArkana ( maybeA, line ) =
    { nr =
        case maybeA of
            Just { cross, answerType } ->
                answerType
                    |> answerTypeToInt
                    |> (+) 1
                    |> (+)
                        (if cross then
                            10

                         else
                            0
                        )

            Nothing ->
                0
    , flipped =
        line
    }

getAnswerImage : Answer -> AnswerImage
getAnswerImage =
    arkanaToAnswer

arkanaToAnswer : Answer -> AnswerImage
arkanaToAnswer { nr, flipped } =
    if nr == 0 then
        ( Nothing
        , flipped
        )

    else
        ( Just
            { cross = nr > 10
            , answerType =
                (nr - 1)
                    |> modBy 10
                    |> intToAnswerType
            }
        , flipped
        )


intToAnswerType : Int -> AnswerType
intToAnswerType int =
    case int of
        0 ->
            Empty

        1 ->
            Vertical

        2 ->
            VerticalDouble

        3 ->
            Single

        4 ->
            SingleAndVerticalTop

        5 ->
            SingleAndVerticalBottom

        6 ->
            SingleAndVerticalBoth

        7 ->
            Double

        8 ->
            DoubleAndVertical

        9 ->
            DoubleAndVerticalBoth

        _ ->
            intToAnswerType 0


answerTypeToInt : AnswerType -> Int
answerTypeToInt t =
    case t of
        Empty ->
            0

        Vertical ->
            1

        VerticalDouble ->
            2

        Single ->
            3

        SingleAndVerticalTop ->
            4

        SingleAndVerticalBottom ->
            5

        SingleAndVerticalBoth ->
            6

        Double ->
            7

        DoubleAndVertical ->
            8

        DoubleAndVerticalBoth ->
            9

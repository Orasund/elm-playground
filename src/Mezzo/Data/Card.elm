module Mezzo.Data.Card exposing (Card, CardPart, CardSort(..), Suit, black, blue, fromPart, fromParts, red, suitToString, toParts, validAfter, white)


cardMaxValue =
    3


type alias Suit =
    { red : Bool
    , blue : Bool
    , yellow : Bool
    }


suitToString : Suit -> String
suitToString suit =
    case ( suit.red, suit.blue, suit.yellow ) of
        ( False, False, True ) ->
            "Yellow"

        ( False, True, False ) ->
            "Blue"

        ( False, True, True ) ->
            "Green"

        ( True, False, False ) ->
            "Red"

        ( True, False, True ) ->
            "Orange"

        ( True, True, False ) ->
            "Violet"

        ( True, True, True ) ->
            "Wild"

        ( False, False, False ) ->
            "Colorless"


red =
    { white | red = True }


blue =
    { white | blue = True }


white =
    { red = False
    , blue = False
    , yellow = False
    }


black =
    { red = True
    , blue = True
    , yellow = True
    }


negativeSuit : Suit -> Suit
negativeSuit s =
    { red = not s.red
    , blue = not s.blue
    , yellow = not s.yellow
    }


mergeSuits : Suit -> Suit -> Suit
mergeSuits s1 s2 =
    let
        intersection =
            { red = s1.red && s2.red
            , blue = s1.blue && s2.blue
            , yellow = s1.yellow && s2.yellow
            }

        union =
            { red = s1.red || s2.red
            , blue = s1.blue || s2.blue
            , yellow = s1.yellow || s2.yellow
            }
    in
    if intersection == white then
        if s1 == white then
            s2

        else if s2 == white then
            s1

        else
            union

    else if s1 == black then
        negativeSuit s2

    else if s2 == black then
        negativeSuit s1

    else
        intersection


type CardSort
    = Valued Int
    | Add


type alias Card =
    { suit : ( Suit, Maybe Suit )
    , sort : CardSort
    }


fromPart : CardPart -> Card
fromPart part =
    { suit = ( part.suit, Nothing )
    , sort = part.sort
    }


type alias CardPart =
    { suit : Suit
    , sort : CardSort
    }


toParts : Card -> ( CardPart, CardPart )
toParts card =
    case card.suit of
        ( p1, Nothing ) ->
            ( { sort = card.sort, suit = p1 }
            , { sort = card.sort, suit = p1 }
            )

        ( p1, Just p2 ) ->
            ( { sort = card.sort, suit = p1 }
            , { sort = card.sort, suit = p2 }
            )


fromParts : ( CardPart, CardPart ) -> Card
fromParts ( p1, p2 ) =
    let
        suit =
            if
                {--(p1.sort == p2.sort)
                    ||--}
                (p1.suit == white)
                    || (p2.suit == white)
                    || (p1.suit == black)
                    || (p2.suit == black)
            then
                ( mergeSuits p1.suit p2.suit, Nothing )

            else
                ( p1.suit, Just p2.suit )
    in
    case ( p1.sort, p2.sort ) of
        ( Valued v1, Valued v2 ) ->
            let
                ( smaller, bigger ) =
                    if v1 > v2 then
                        ( v2, v1 )

                    else
                        ( v1, v2 )

                value =
                    if smaller == 0 then
                        bigger

                    else if smaller + 1 == bigger then
                        if bigger == cardMaxValue then
                            0

                        else
                            bigger + 1

                    else if smaller == bigger then
                        if bigger == 0 then
                            cardMaxValue

                        else
                            bigger - 1

                    else
                        round <| (toFloat bigger + toFloat smaller) / 2
            in
            { suit =
                if smaller == cardMaxValue then
                    if p1.suit == p2.suit then
                        ( negativeSuit p1.suit, Nothing )

                    else
                        ( mergeSuits p1.suit p2.suit, Nothing )

                else
                    suit
            , sort =
                if bigger == 0 then
                    Add

                else
                    Valued value
            }

        ( Add, Valued v ) ->
            { suit = suit
            , sort = Valued v
            }

        ( Valued v, Add ) ->
            { suit = suit
            , sort = Valued v
            }

        ( Add, Add ) ->
            { suit = suit
            , sort = Add
            }


validAfter : Card -> Card -> Bool
validAfter c1 c2 =
    let
        ( a1, b1 ) =
            c1.suit
                |> Tuple.mapSecond (Maybe.withDefault (c1.suit |> Tuple.first))

        ( a2, b2 ) =
            c2.suit
                |> Tuple.mapSecond (Maybe.withDefault (c2.suit |> Tuple.first))
    in
    (c1.sort == c2.sort)
        || ((a1 == black) || (a2 == black))
        || ((a1 == a2) || (a1 == b2) || (b1 == a2) || (b1 == b2))

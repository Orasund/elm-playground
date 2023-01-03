module Internal exposing (..)

{-| -}


eps : Float
eps =
    --Just a magic number (= 2^-43)
    1.1368683772161603e-13


isZero : Float -> Bool
isZero f1 =
    abs f1 < eps


equal : Float -> Float -> Bool
equal f1 f2 =
    abs (f1 - f2) < eps


plus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
plus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    length ( x1 - x2, y1 - y2 )


length : ( Float, Float ) -> Float
length ( x, y ) =
    sqrt (x * x + y * y)


{-| natural logarithm
-}
ln : Float -> Float
ln =
    logBase e


sinh : Float -> Float
sinh x =
    (e ^ x - e ^ -x) / 2


arsinh : Float -> Float
arsinh x =
    ln (x + sqrt (x * x + 1))


cosh : Float -> Float
cosh x =
    (e ^ x + e ^ -x) / 2


{-| input must be < 1.
-}
artanh : Float -> Maybe Float
artanh x =
    if x < 1 then
        ln ((1 + x) / (1 - x))
            / 2
            |> Just

    else
        Nothing


tanh : Float -> Float
tanh x =
    sinh x / cosh x


{-| converts a line AB to a general form ax + by + c = 0 such that A and B solve the equation.
-}
lineToGeneralForm : ( ( Float, Float ), ( Float, Float ) ) -> { x : Float, y : Float, c : Float }
lineToGeneralForm ( ( x0, y0 ), ( x1, y1 ) ) =
    --https://en.wikipedia.org/wiki/Line_(geometry)
    if equal x1 x0 then
        { x = 1, y = 0, c = x1 }

    else
        { x = (y1 - y0) / (x1 - x0), y = -1, c = (x1 * y0 - x0 * y1) / (x1 - x0) }


normalVector : ( ( Float, Float ), ( Float, Float ) ) -> ( Float, Float )
normalVector ( ( x1, y1 ), ( x2, y2 ) ) =
    ( y2 - y1, x1 - x2 )


{-| can't intersect parallel lines
-}
lineIntersection : ( ( Float, Float ), ( Float, Float ) ) -> ( ( Float, Float ), ( Float, Float ) ) -> Maybe ( Float, Float )
lineIntersection ( ( x1, y1 ), ( x2, y2 ) ) ( ( x3, y3 ), ( x4, y4 ) ) =
    --https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    if (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4) |> isZero then
        Nothing

    else
        ( ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4))
            / ((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
        , ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4))
            / ((x1 - x2) * (y3 - y4) - (y1 - y2 * (x3 - x4)))
        )
            |> Just


intersectLineWithUnitCircle : { x : Float, y : Float, c : Float } -> ( ( Float, Float ), ( Float, Float ) )
intersectLineWithUnitCircle equation =
    if isZero equation.y then
        --we can assume that b is not zero
        --if a and b where zero, then the input is not a line
        -- bx + c = 0 (1)
        -- x² + y² = 1  (2)
        -->(1) x = -c/b (3)
        -->(3 in 2) c²/b² + y² = 1
        -->y² = 1 - c²/b²
        -->y = (-+)sqrt (1 - c²/b²)
        let
            _ =
                equation |> Debug.log "equation"

            c =
                equation.c |> Debug.log "y=0"

            b =
                equation.x

            x =
                -c / b
        in
        ( ( x, -(sqrt (1 - c * c / b * b)) )
        , ( x, sqrt (1 - c * c / b * b) )
        )

    else
        -- ay + bx + c = 0 (1)
        -- x² + y² = 1  (2)
        -->(1) y = -(bx + c)/a (3)
        -->(3 in 2) x² + (bx + c)²/a² = 1
        -->x²+(b²x² + 2bxc + c²)/a² = 1
        -->x²+(b²/a²)x² + (2bc/a²)x + c²/a² = 1
        -->(1+b²/a²)x² + (2bc/a²)x + (c²/a² - 1) = 0
        -->x² + ((2bc/a²)/(1+b²/a²))x + (c²/a² - 1)/(1+b²/a²) = 0
        -->x² + ((2bc/a²)/((a²+b²)/a²))x + ((c² - a²)/a²)/((a²+b²)/a²) = 0
        -->x² + (2bc/(a²+b²))*x - (c²- a²)/(a²+b²) = 0
        let
            a =
                equation.y

            b =
                equation.x

            c =
                equation.c

            p =
                2 * b * c / (a * a + b * b)

            q =
                (c * c - a * a) / (a * a + b * b)

            x1 =
                -p / 2 + sqrt ((p / 2) * (p / 2) - q)

            x2 =
                -p / 2 - sqrt ((p / 2) * (p / 2) - q)

            y x =
                -(b * x + c) / a
        in
        ( ( x1, y x1 )
        , ( x2, y x2 )
        )

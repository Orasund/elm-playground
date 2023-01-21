module Internal exposing (..)

{-| -}


type HyperboloidPoint
    = HyperboloidPoint ( Float, Float, Float )


type PoincarePoint
    = PoincarePoint ( Float, Float )


eps : Float
eps =
    --Just a magic number (= 2^-43)
    1.1368683772161603e-13


isZero : Float -> Bool
isZero f1 =
    abs f1 < eps


safeSqrt : Float -> Float
safeSqrt f =
    if isZero f then
        0

    else
        sqrt f


equal : Float -> Float -> Bool
equal f1 f2 =
    abs (f1 - f2) < eps


plus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
plus ( x, y ) =
    Tuple.mapBoth ((+) x) ((+) y)


minus : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
minus ( x, y ) =
    plus ( -x, -y )


scaleBy : Float -> ( Float, Float ) -> ( Float, Float )
scaleBy c =
    Tuple.mapBoth ((*) c) ((*) c)


negate : ( Float, Float ) -> ( Float, Float )
negate ( x, y ) =
    ( -x, -y )


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    length ( x1 - x2, y1 - y2 )


length : ( Float, Float ) -> Float
length ( x, y ) =
    safeSqrt (x * x + y * y)


{-| natural logarithm
-}
ln : Float -> Float
ln =
    logBase e


arsinh : Float -> Float
arsinh x =
    ln (x + sqrt (x * x + 1))


arcosh : Float -> Float
arcosh x =
    ln (x + sqrt (x * x - 1))


{-| input must be < 1.
-}
artanh : Float -> Maybe Float
artanh x =
    if abs x < 1 then
        ln ((1 + x) / (1 - x))
            / 2
            |> Just

    else
        Nothing


tanh : Float -> Float
tanh x =
    --https://www.redcrab-software.com/en/calculator/Tanh
    (e ^ (2 * x) - 1)
        / (e ^ (2 * x) + 1)


{-| converts a line AB to a general form ax + by + c = 0 such that A and B solve the equation.
-}
lineToGeneralForm : ( ( Float, Float ), ( Float, Float ) ) -> { x : Float, y : Float, c : Float }
lineToGeneralForm ( ( x0, y0 ), p2 ) =
    --https://www.topcoder.com/thrive/articles/Geometry%20Concepts%20part%202:%20%20Line%20Intersection%20and%20its%20Applications
    let
        ( x, y ) =
            normalVector ( ( x0, y0 ), p2 )
    in
    { x = x
    , y = y
    , c = -x * x0 - y * y0
    }


normalVector : ( ( Float, Float ), ( Float, Float ) ) -> ( Float, Float )
normalVector ( ( x1, y1 ), ( x2, y2 ) ) =
    ( y2 - y1, x1 - x2 )


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    ( x / length ( x, y ), y / length ( x, y ) )


vecTo : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
vecTo ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


{-| can't intersect parallel lines
-}
lineIntersection : { x : Float, y : Float, c : Float } -> { x : Float, y : Float, c : Float } -> Maybe ( Float, Float )
lineIntersection equ1 equ2 =
    --https://www.topcoder.com/thrive/articles/Geometry%20Concepts%20part%202:%20%20Line%20Intersection%20and%20its%20Applications
    -- source uses representation ax+by=c, but we are using ax+by+c=0
    -- therefore we need to negate c
    -- using line equations because algorithm for 4 points is numerical unstable.
    let
        det =
            equ1.x
                * equ2.y
                - equ1.y
                * equ2.x
                |> Debug.log "det"
    in
    if isZero det then
        Nothing

    else
        ( (-equ2.y * equ1.c + equ1.y * equ2.c) / det
        , (-equ1.x * equ2.c + equ2.x * equ1.c) / det
        )
            |> Just


intersectLineWithUnitCircle : { x : Float, y : Float, c : Float } -> Maybe ( ( Float, Float ), ( Float, Float ) )
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
            c =
                equation.c

            b =
                equation.x

            x =
                -c / b

            sq =
                1 - c * c / b * b
        in
        if isZero sq || sq > 0 then
            ( ( x, -(safeSqrt sq) )
            , ( x, safeSqrt sq )
            )
                |> Just

        else
            Nothing

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

            sq =
                (p * p / 4) - q

            x1 =
                -p / 2 + safeSqrt sq

            x2 =
                -p / 2 - safeSqrt sq

            y x =
                -(b * x + c) / a
        in
        if isZero sq || sq > 0 then
            ( ( x1, y x1 )
            , ( x2, y x2 )
            )
                |> Just

        else
            Nothing


innerProduct : ( Float, Float ) -> ( Float, Float ) -> Float
innerProduct ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2

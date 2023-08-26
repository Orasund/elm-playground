module Expression exposing (..)


type Operator
    = PlusOp
    | TimesOp
    | MinusOp
    | DividedOp


type Symbol
    = NumberSymbol Int
    | OpSymbol Operator
    | VarSymbol
    | ErrSymbol


type Expression
    = Number Int
    | Op Operator Expression
    | Error
    | DivisionByZero


invert : Operator -> Operator
invert op =
    case op of
        PlusOp ->
            MinusOp

        MinusOp ->
            PlusOp

        TimesOp ->
            DividedOp

        DividedOp ->
            TimesOp


toSymbols : Expression -> List Symbol
toSymbols exp =
    case exp of
        Number int ->
            [ NumberSymbol int ]

        Op op exp2 ->
            toSymbols exp2 ++ [ OpSymbol op ]

        Error ->
            [ ErrSymbol ]

        DivisionByZero ->
            []


delete : Expression -> Expression
delete exp =
    case exp of
        Number i ->
            i
                |> String.fromInt
                |> String.dropRight 1
                |> String.toInt
                |> Maybe.withDefault 0
                |> Number

        Op _ exp2 ->
            exp2

        Error ->
            Number 0

        DivisionByZero ->
            DivisionByZero


opToString : Operator -> String
opToString op =
    case op of
        PlusOp ->
            "+"

        TimesOp ->
            "*"

        MinusOp ->
            "-"

        DividedOp ->
            "/"


toString : Expression -> String
toString expression =
    case expression of
        Number int ->
            String.fromInt int

        Op operator exp ->
            toString exp
                ++ opToString operator

        Error ->
            "ERR"

        DivisionByZero ->
            "YOU WIN"

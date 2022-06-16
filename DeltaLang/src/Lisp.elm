module Lisp exposing (..)

import Dict exposing (Dict)
import Json.Value as JsonValue exposing (JsonValue(..))


type Exp
    = FunctionExp String (List Exp)
    | ValueExp Value


type Value
    = SymbolValue JsonValue
    | EmptyValue


type ParserError
    = FunctionNameExpected JsonValue
    | ListExpected JsonValue


type EvaluationError
    = NameNotFound String
    | InvalidListLength Int (List Exp)


type alias Context =
    Dict String (List Value -> Result EvaluationError Value)


type alias Extensions =
    Dict
        String
        ({ args : List Exp, context : Context, evalFun : Context -> Exp -> Result EvaluationError Value }
         -> Result EvaluationError Value
        )


parseExp : JsonValue -> Result ParserError Exp
parseExp jsonValue =
    case jsonValue of
        ArrayValue list ->
            case list of
                [] ->
                    EmptyValue |> ValueExp |> Ok

                [ JsonValue.StringValue string ] ->
                    FunctionExp string [] |> Ok

                [ head ] ->
                    head |> SymbolValue |> ValueExp |> Ok

                (JsonValue.StringValue string) :: tail ->
                    tail
                        |> internalRun parseExp
                        |> Result.map (FunctionExp string)

                head :: _ ->
                    head |> FunctionNameExpected |> Err

        _ ->
            jsonValue |> ListExpected |> Err


baseDialect :
    Dict
        String
        ({ args : List Exp, context : Context, evalFun : Context -> Exp -> Result EvaluationError Value }
         -> Result EvaluationError Value
        )
baseDialect =
    [ ( "let"
      , \{ args, context, evalFun } ->
            case args of
                [ ValueExp (SymbolValue (JsonValue.StringValue string)), e1, e2 ] ->
                    e1
                        |> evalFun context
                        |> Result.andThen
                            (\value ->
                                e2
                                    |> evalFun (Dict.insert string (\_ -> Ok value) context)
                            )

                _ ->
                    InvalidListLength 3 args |> Err
      )
    ]
        |> Dict.fromList


eval : Context -> Exp -> Result EvaluationError Value
eval =
    evalWithExtensions baseDialect


evalWithExtensions : Extensions -> Context -> Exp -> Result EvaluationError Value
evalWithExtensions extensions context exp =
    case exp of
        FunctionExp string list ->
            case context |> Dict.get string of
                Just fun ->
                    list
                        |> internalRun (evalWithExtensions extensions context)
                        |> Result.andThen fun

                Nothing ->
                    string |> NameNotFound |> Err

        ValueExp value ->
            value |> Ok


internalRun : (a -> Result err b) -> List a -> Result err (List b)
internalRun fun list =
    list
        |> List.foldl
            (\a result ->
                case result of
                    Err _ ->
                        result

                    Ok l ->
                        a
                            |> fun
                            |> Result.map (\b -> b :: l)
            )
            (Ok [])
        |> Result.map List.reverse

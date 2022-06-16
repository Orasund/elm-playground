module Lisp exposing (..)

import Dict exposing (Dict)
import Json.Value as JsonValue exposing (JsonValue(..))


type Exp
    = VariableExp { name : String, args : List Exp }
    | ValueExp JsonValue


type ParserError
    = FunctionNameExpected JsonValue
    | ListValueExpected JsonValue


type EvaluationError
    = NameNotFound String
    | InvalidListLength Int (List Exp)
    | BoolExpected JsonValue Exp
    | ListExpected JsonValue Exp
    | ParsingError ParserError


type alias Context =
    Dict String (List JsonValue -> Result EvaluationError JsonValue)


type alias Extension =
    { args : List Exp
    , context : Context
    , evalFun : Context -> Exp -> Result EvaluationError JsonValue
    }
    -> Result EvaluationError JsonValue


type alias Extensions =
    Dict String Extension


parseExp : JsonValue -> Result ParserError Exp
parseExp jsonValue =
    case jsonValue of
        ArrayValue list ->
            case list of
                [] ->
                    JsonValue.NullValue |> ValueExp |> Ok

                [ JsonValue.StringValue string ] ->
                    VariableExp { name = string, args = [] } |> Ok

                [ head ] ->
                    head |> ValueExp |> Ok

                (JsonValue.StringValue string) :: tail ->
                    tail
                        |> internalRun parseExp
                        |> Result.map (\l -> VariableExp { name = string, args = l })

                head :: _ ->
                    head |> FunctionNameExpected |> Err

        _ ->
            jsonValue |> ListValueExpected |> Err


baseDialect : Extensions
baseDialect =
    [ ( "let"
      , \{ args, context, evalFun } ->
            case args of
                [ ValueExp (JsonValue.ArrayValue list), body ] ->
                    list
                        |> List.foldl
                            (\v0 ->
                                Result.andThen
                                    (\c ->
                                        case v0 of
                                            JsonValue.ArrayValue [ JsonValue.StringValue string, v1 ] ->
                                                v1
                                                    |> parseExp
                                                    |> Result.mapError ParsingError
                                                    |> Result.andThen (evalFun c)
                                                    |> Result.map (\v -> c |> Dict.insert string (\_ -> Ok v))

                                            JsonValue.ArrayValue l ->
                                                InvalidListLength 2 (l |> List.map ValueExp) |> Err

                                            _ ->
                                                ListExpected v0 (ValueExp (JsonValue.ArrayValue list)) |> Err
                                    )
                            )
                            (Ok context)
                        |> Result.andThen
                            (\newContext ->
                                body
                                    |> evalFun newContext
                            )

                _ ->
                    InvalidListLength 3 args |> Err
      )
    , ( "if"
      , \{ args, context, evalFun } ->
            case args of
                [ e0, e1, e2 ] ->
                    e0
                        |> evalFun context
                        |> Result.andThen
                            (\v ->
                                case v of
                                    BoolValue bool ->
                                        (if bool then
                                            e1

                                         else
                                            e2
                                        )
                                            |> evalFun context

                                    _ ->
                                        BoolExpected v e0 |> Err
                            )

                _ ->
                    InvalidListLength 3 args |> Err
      )
    ]
        |> Dict.fromList


eval : Context -> Exp -> Result EvaluationError JsonValue
eval =
    evalWithExtensions baseDialect


evalWithExtensions : Extensions -> Context -> Exp -> Result EvaluationError JsonValue
evalWithExtensions extensions context exp =
    case exp of
        VariableExp args ->
            case extensions |> Dict.get args.name of
                Just fun ->
                    { args = args.args, context = context, evalFun = evalWithExtensions extensions }
                        |> fun

                Nothing ->
                    case context |> Dict.get args.name of
                        Just fun ->
                            args.args
                                |> internalRun (evalWithExtensions extensions context)
                                |> Result.andThen fun

                        Nothing ->
                            args.name |> NameNotFound |> Err

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

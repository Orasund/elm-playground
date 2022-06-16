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


letFun :
    { args : List Exp
    , context : Context
    , evalFun : Context -> Exp -> Result EvaluationError JsonValue
    }
    -> Result EvaluationError JsonValue
letFun { args, context, evalFun } =
    case args of
        [ e, body ] ->
            e
                |> evalFun context
                |> Result.andThen
                    (\v ->
                        case v of
                            JsonValue.ArrayValue list ->
                                list
                                    |> List.foldl
                                        (\v1 ->
                                            Result.andThen
                                                (\c ->
                                                    case v1 of
                                                        JsonValue.ArrayValue [ JsonValue.StringValue string, v2 ] ->
                                                            c |> Dict.insert string (\_ -> Ok v2) |> Ok

                                                        JsonValue.ArrayValue l ->
                                                            InvalidListLength 2 (l |> List.map ValueExp) |> Err

                                                        _ ->
                                                            ListExpected v1 (ValueExp (JsonValue.ArrayValue list)) |> Err
                                                )
                                        )
                                        (Ok context)

                            _ ->
                                ListExpected v e |> Err
                    )
                |> Result.andThen
                    (\newContext ->
                        body
                            |> evalFun newContext
                    )

        _ ->
            InvalidListLength 3 args |> Err


ifFun :
    { args : List Exp
    , context : Context
    , evalFun : Context -> Exp -> Result EvaluationError JsonValue
    }
    -> Result EvaluationError JsonValue
ifFun { args, context, evalFun } =
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


baseDialect : Dict String Extension
baseDialect =
    [ ( "let", letFun )
    , ( "if", ifFun )
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

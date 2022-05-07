module Handlebars exposing (..)

import Array exposing (Array)
import Css exposing (Ex)
import Dict exposing (Dict)
import Dict.Any as AnyDict exposing (AnyDict)
import Json.Decode as D exposing (Decoder)
import Parser
import Result.Extra
import Set exposing (Set)


type alias Path =
    List String


pathToString : Path -> String
pathToString =
    String.join "."


type alias ExpHelper =
    List Value -> Result String Value


type alias BlockHelper =
    { path : Path
    , value : Value
    , throw : String -> Error
    , content : Path -> Result Error String
    }
    -> Result Error String


type SubExp
    = LookUp Path --some.path (Normalized:  ../not.allowed)
    | Helper String ( SubExp, List SubExp ) --helper a b c


type alias Template =
    { expression : Expression
    , expHelpers : Dict String ExpHelper
    , blockHelpers : Dict String BlockHelper
    , root : Path
    }


type Expression
    = Text String
    | Variable SubExp --{{subExp}}
    | Block String SubExp Expression --{{#name subExp }} exp {{/name}}


type Value
    = StringValue String
    | BooleanValue Bool
    | ArrayValue (Array Value)
    | ObjectValue (Dict String Value)


getValue : Path -> Value -> Maybe Value
getValue path value =
    case ( value, path ) of
        ( ObjectValue dict, [ string, "@key" ] ) ->
            dict
                |> Dict.get string
                |> Maybe.map (\_ -> StringValue string)

        ( ObjectValue dict, head :: tail ) ->
            dict
                |> Dict.get head
                |> Maybe.andThen (getValue tail)

        ( ArrayValue array, [ string, "@index" ] ) ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)
                |> Maybe.map (\_ -> StringValue string)

        ( ArrayValue array, [ string, "@first" ] ) ->
            string == "0" |> BooleanValue |> Just

        ( ArrayValue array, [ string, "@last" ] ) ->
            array
                |> Array.length
                |> (+) -1
                |> String.fromInt
                |> (==) string
                |> BooleanValue
                |> Just

        ( ArrayValue array, [ string ] ) ->
            string
                |> String.toInt
                |> Maybe.andThen (\i -> Array.get i array)

        ( _, _ :: _ ) ->
            Nothing

        ( _, [] ) ->
            Just value


type Error
    = PathNotFound Path
    | StringExpected ( SubExp, Value )
    | HelperNotFound String
    | BlockHelperNot String
    | FromHelper { helper : String, error : String }


evalSubExp : Template -> Value -> SubExp -> Result Error Value
evalSubExp template value e1 =
    case e1 of
        LookUp path ->
            value
                |> getValue path
                |> Maybe.map Ok
                |> Maybe.withDefault (path |> PathNotFound |> Err)

        Helper name ( head, tail ) ->
            case template.expHelpers |> Dict.get name of
                Just fun ->
                    head
                        :: tail
                        |> List.map (evalSubExp template value)
                        |> Result.Extra.combine
                        |> Result.andThen
                            (\list ->
                                list
                                    |> fun
                                    |> Result.mapError
                                        (\error ->
                                            FromHelper { helper = name, error = error }
                                        )
                            )

                Nothing ->
                    name |> HelperNotFound |> Err


eval : Template -> Value -> Result Error String
eval template value =
    case template.expression of
        Text string ->
            Ok string

        Variable subExp ->
            subExp
                |> evalSubExp template value
                |> Result.andThen
                    (\v ->
                        case v of
                            StringValue string ->
                                Ok string

                            _ ->
                                Err (StringExpected ( subExp, v ))
                    )

        Block string subExp exp ->
            case template.blockHelpers |> Dict.get string of
                Just fun ->
                    subExp
                        |> evalSubExp template value
                        |> Result.andThen
                            (\arg ->
                                fun
                                    { path = template.root
                                    , value = arg
                                    , throw = \err -> FromHelper { helper = string, error = err }
                                    , content =
                                        \path ->
                                            value
                                                |> eval { template | expression = exp, root = path }
                                    }
                            )

                Nothing ->
                    BlockHelperNot string |> Err

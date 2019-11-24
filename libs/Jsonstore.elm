module Jsonstore exposing (Json, bool, decode, decodeList, delete, dict, encode, encodeList, float, get, insert, int, map, object, string, toJson, update, with, withList, withMaybe)

import Dict exposing (Dict)
import Http exposing (Error, Resolver)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Task exposing (Task)


type Json a
    = Json ( D.Decoder a, a -> Value )


map : (a -> b) -> (b -> a) -> Json a -> Json b
map dFun eFun (Json ( d, e )) =
    Json ( D.map dFun d, eFun >> e )


int : Json Int
int =
    Json ( D.int, E.int )


float : Json Float
float =
    Json ( D.float, E.float )


string : Json String
string =
    Json ( D.string, E.string )


bool : Json Bool
bool =
    Json ( D.bool, E.bool )


dict : Json a -> Json (Dict String a)
dict (Json ( d, e )) =
    Json ( d |> D.dict, E.dict identity e )


type JsonObject obj a
    = JsonObject ( Decoder obj, List ( String, a -> Value ) )


with : String -> Json a -> (obj -> a) -> JsonObject (a -> fun) obj -> JsonObject fun obj
with name (Json json) value (JsonObject ( d, e )) =
    JsonObject
        ( d
            |> D.map2 (\f fun -> fun f)
                (D.field name (json |> Tuple.first))
        , e |> (::) ( name, \o -> (json |> Tuple.second) (o |> value) )
        )


withList : String -> Json a -> (obj -> List a) -> JsonObject (List a -> fun) obj -> JsonObject fun obj
withList name (Json json) value (JsonObject ( d, e )) =
    JsonObject
        ( d
            |> D.map2 (\f fun -> fun f)
                ((D.map (Maybe.withDefault []) << D.maybe << D.field name) (json |> Tuple.first |> D.list))
        , e |> (::) ( name, \o -> E.list (json |> Tuple.second) (o |> value) )
        )


withMaybe : String -> Json a -> (obj -> Maybe a) -> JsonObject (Maybe a -> fun) obj -> JsonObject fun obj
withMaybe name (Json json) value (JsonObject ( d, e )) =
    JsonObject
        ( d
            |> D.map2 (\f fun -> fun f)
                (D.maybe <| D.field name (json |> Tuple.first))
        , e
            |> (::)
                ( name
                , \o ->
                    (Maybe.map (json |> Tuple.second)
                        >> Maybe.withDefault E.null
                    )
                        (o |> value)
                )
        )


object : obj -> JsonObject obj a
object fun =
    JsonObject ( D.succeed fun, [] )


toJson : JsonObject obj obj -> Json obj
toJson =
    \(JsonObject ( d, e )) ->
        Json
            ( d
            , e
                |> (\l ->
                        \obj ->
                            l
                                |> List.map (\( name, fun ) -> ( name, fun obj ))
                                |> List.reverse
                                |> E.object
                   )
            )


encodeList : Json a -> List a -> Value
encodeList (Json ( _, fun )) =
    E.list fun


decodeList : Json a -> D.Decoder (List a)
decodeList (Json ( fun, _ )) =
    D.list fun


decode : Json a -> D.Decoder a
decode (Json ( fun, _ )) =
    fun


encode : Json a -> a -> Value
encode (Json ( _, fun )) =
    fun



-------------------------------------
-- HTTP
-------------------------------------


resolve : D.Decoder a -> Resolver Error a
resolve decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case D.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody <| D.errorToString <| err)


resolveWhatever : Resolver Error ()
resolveWhatever =
    Http.stringResolver <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ _ ->
                    Ok ()


insert : String -> Value -> Task Error ()
insert url value =
    Http.task
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.jsonBody value
        , resolver = resolveWhatever
        , timeout = Nothing
        }


delete : String -> Task Error ()
delete url =
    Http.task
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = resolveWhatever
        , timeout = Nothing
        }


get : String -> Decoder a -> Task Error (Maybe a)
get url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = resolve <| D.field "result" <| D.nullable <| decoder
        , timeout = Nothing
        }


update : { url : String, decoder : Decoder a, value : Maybe a -> Maybe Value } -> Task Error ()
update { url, decoder, value } =
    get url decoder
        |> Task.andThen
            (value
                >> Maybe.map (insert url)
                >> Maybe.withDefault (Task.succeed ())
            )

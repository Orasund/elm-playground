module Gen.Union.RemoteData exposing (..)

{-| This module contains the RemoteData Union.

@docs RemoteData


## Is

@docs isWaiting, isResult, isFailure, isSuccess


## If

@docs ifResult, ifFailure, ifSuccess


## Map

@docs mapResult, mapFailure, mapSuccess

-}

-- This is a generated file. DO NOT CHANGE ANYTHING IN HERE.


{-| RemoteData type
-}
type RemoteData a1 a3
    = Waiting
    | Result (Result String a1)
    | Failure String
    | Success a3



-------------------------------------------------------------------------------
-- IS
-------------------------------------------------------------------------------


{-| Compute if the value is a Waiting.

Useful if you don't want to do a full case distinction inside an if condition.

-}
isWaiting : RemoteData a1 a3 -> Bool
isWaiting remoteData =
    case remoteData of
        Waiting ->
            True

        Result _ ->
            False

        Failure _ ->
            False

        Success _ ->
            False


{-| Compute if the value is a Result.

Useful if you don't want to do a full case distinction inside an if condition.

-}
isResult : RemoteData a1 a3 -> Bool
isResult remoteData =
    case remoteData of
        Waiting ->
            False

        Result _ ->
            True

        Failure _ ->
            False

        Success _ ->
            False


{-| Compute if the value is a Failure.

Useful if you don't want to do a full case distinction inside an if condition.

-}
isFailure : RemoteData a1 a3 -> Bool
isFailure remoteData =
    case remoteData of
        Waiting ->
            False

        Result _ ->
            False

        Failure _ ->
            True

        Success _ ->
            False


{-| Compute if the value is a Success.

Useful if you don't want to do a full case distinction inside an if condition.

-}
isSuccess : RemoteData a1 a3 -> Bool
isSuccess remoteData =
    case remoteData of
        Waiting ->
            False

        Result _ ->
            False

        Failure _ ->
            False

        Success _ ->
            True



-------------------------------------------------------------------------------
-- IF
-------------------------------------------------------------------------------


{-| Calls a function, if the value is a Result.

You can turn the function into a getter by passing the identity as function

-}
ifResult : (Result String a1 -> out) -> RemoteData a1 a3 -> Maybe out
ifResult fun remoteData =
    case remoteData of
        Waiting ->
            Nothing

        Result t ->
            Just (fun t)

        Failure a ->
            Nothing

        Success a ->
            Nothing


{-| Calls a function, if the value is a Failure.

You can turn the function into a getter by passing the identity as function

-}
ifFailure : (String -> out) -> RemoteData a1 a3 -> Maybe out
ifFailure fun remoteData =
    case remoteData of
        Waiting ->
            Nothing

        Result a ->
            Nothing

        Failure t ->
            Just (fun t)

        Success a ->
            Nothing


{-| Calls a function, if the value is a Success.

You can turn the function into a getter by passing the identity as function

-}
ifSuccess : (a3 -> out) -> RemoteData a1 a3 -> Maybe out
ifSuccess fun remoteData =
    case remoteData of
        Waiting ->
            Nothing

        Result a ->
            Nothing

        Failure a ->
            Nothing

        Success t ->
            Just (fun t)



-------------------------------------------------------------------------------
-- MAP
-------------------------------------------------------------------------------


{-| Map the argument of Result.
-}
mapResult : (Result String a1 -> Result String b1) -> RemoteData a1 a3 -> RemoteData b1 a3
mapResult fun remoteData =
    case remoteData of
        Waiting ->
            Waiting

        Result t ->
            Result (fun t)

        Failure a ->
            Failure a

        Success a ->
            Success a


{-| Map the argument of Failure.
-}
mapFailure : (String -> String) -> RemoteData a1 a3 -> RemoteData a1 a3
mapFailure fun remoteData =
    case remoteData of
        Waiting ->
            Waiting

        Result a ->
            Result a

        Failure t ->
            Failure (fun t)

        Success a ->
            Success a


{-| Map the argument of Success.
-}
mapSuccess : (a3 -> b3) -> RemoteData a1 a3 -> RemoteData a1 b3
mapSuccess fun remoteData =
    case remoteData of
        Waiting ->
            Waiting

        Result a ->
            Result a

        Failure a ->
            Failure a

        Success t ->
            Success (fun t)



-- Generated with [Elm-pen](https://orasund.github.io/elm-pen) Version 0.0.6

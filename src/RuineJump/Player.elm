module RuineJump.Player exposing (FaceingDirection(..), Player, PlayerAction(..), fall)


type alias Player =
    { pos : ( Int, Int ), action : PlayerAction, faceing : FaceingDirection }


type FaceingDirection
    = FaceingLeft
    | FaceingRight


type PlayerAction
    = Standing
    | Falling


fall : Player -> Player
fall ({ pos, action } as player) =
    let
        ( x, y ) =
            pos
    in
    { player | pos = ( x, y + 1 ), action = Falling }

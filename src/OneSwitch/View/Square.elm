module OneSwitch.View.Square exposing (empty, view)

import OneSwitch.Data.Square exposing (Square(..))
import PixelEngine.Tile as Tile


view : Square -> Tile.Tile msg
view square =
    case square of
        Wall ->
            wall

        Player ->
            player

        Enemy ->
            enemy

        ActivePlayer ->
            activePlayer

        InactiveEnemy ->
            inactiveEnemy

        LookedDoor ->
            lookedDoor

        OpenDoor ->
            openDoor

        Health ->
            health

        Key ->
            key

        PowerDown ->
            powerDown

        PowerUp ->
            powerUp

        Swap ->
            swap

        Weapon ->
            weapon

        Lava ->
            lava



--------------------------------------------------------------------------------


empty : Tile.Tile msg
empty =
    Tile.fromPosition ( 0, 0 )


wall : Tile.Tile msg
wall =
    Tile.fromPosition ( 1, 0 )


player : Tile.Tile msg
player =
    Tile.fromPosition ( 0, 1 ) |> Tile.movable "player"


enemy : Tile.Tile msg
enemy =
    Tile.fromPosition ( 1, 1 )


activePlayer : Tile.Tile msg
activePlayer =
    Tile.fromPosition ( 3, 0 ) |> Tile.movable "player"


inactiveEnemy : Tile.Tile msg
inactiveEnemy =
    Tile.fromPosition ( 2, 0 )


lookedDoor : Tile.Tile msg
lookedDoor =
    Tile.fromPosition ( 2, 1 )


openDoor : Tile.Tile msg
openDoor =
    Tile.fromPosition ( 3, 1 )


health : Tile.Tile msg
health =
    Tile.fromPosition ( 0, 2 )


key : Tile.Tile msg
key =
    Tile.fromPosition ( 1, 2 )


powerDown : Tile.Tile msg
powerDown =
    Tile.fromPosition ( 2, 2 )


powerUp : Tile.Tile msg
powerUp =
    Tile.fromPosition ( 3, 2 )


swap : Tile.Tile msg
swap =
    Tile.fromPosition ( 0, 3 )


weapon : Tile.Tile msg
weapon =
    Tile.fromPosition ( 1, 3 )


lava : Tile.Tile msg
lava =
    Tile.fromPosition ( 2, 3 )

module OneSwitch.Data.Square exposing (Square(..), swap)


type Square
    = Wall
    | Player
    | Enemy
    | ActivePlayer
    | InactiveEnemy
    | LookedDoor
    | OpenDoor
    | Health
    | Key
    | PowerDown
    | PowerUp
    | Swap
    | Weapon
    | Lava


swap : Square -> Square
swap square =
    case square of
        Wall ->
            Lava

        Player ->
            ActivePlayer

        Enemy ->
            InactiveEnemy

        ActivePlayer ->
            Player

        InactiveEnemy ->
            Enemy

        LookedDoor ->
            OpenDoor

        OpenDoor ->
            LookedDoor

        Health ->
            Health

        Key ->
            Key

        PowerDown ->
            PowerDown

        PowerUp ->
            PowerUp

        Swap ->
            Swap

        Weapon ->
            Weapon

        Lava ->
            Wall

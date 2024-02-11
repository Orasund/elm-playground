module Config exposing (bugAmount, gameOverCooldownInMs, gridSize, hideBugs, leafAmount, maxTurns, stoneAmount)


gridSize : Int
gridSize =
    5


maxTurns : Int
maxTurns =
    5


leafAmount : Int
leafAmount =
    gridSize


stoneAmount : Int
stoneAmount =
    gridSize ^ 2 // 4


bugAmount : Int
bugAmount =
    6


hideBugs =
    True


gameOverCooldownInMs : Float
gameOverCooldownInMs =
    50

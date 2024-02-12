module Config exposing (bugAmount, gameOverCooldownInMs, gridSize, hideBugs, leafAmount, startingGuesses, stoneAmount)


gridSize : Int
gridSize =
    5


startingGuesses : Int
startingGuesses =
    5


leafAmount : Int
leafAmount =
    gridSize


stoneAmount : Int
stoneAmount =
    gridSize ^ 2 // 4


bugAmount : Int
bugAmount =
    4


hideBugs =
    True


gameOverCooldownInMs : Float
gameOverCooldownInMs =
    50

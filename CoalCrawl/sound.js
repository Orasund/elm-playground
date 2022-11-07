
var volume = 1

function loadSound(file, name) {
    createjs.Sound.registerSound("assets/sounds/" + file, name);
}

function playSound(name) {
    var instance = createjs.Sound.play(name, { volume: 0.25 * volume })
}

function setVolume(amount) {
    volume = amount
}
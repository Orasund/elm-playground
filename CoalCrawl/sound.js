
function loadSound(file, name) {
    createjs.Sound.registerSound("assets/sounds/" + file, name);
}

function playSound(name) {
    createjs.Sound.play(name);
}
// Get our canvas element
var canvas = document.getElementById("main-canvas");
var width = 960;
var height = 500;

// Create a WebGL 2D platform on the canvas:
var platform = Stardust.platform("webgl-2d", canvas, width, height);
// ... Load data and render your visualization

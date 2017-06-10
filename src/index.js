'use strict';

// require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');
require('./style.css')

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token
// var app = Elm.Main.embed(mountNode);
// receive something from Elm

// https://hackernoon.com/how-elm-ports-work-with-a-picture-just-one-25144ba43cdd

// <script type="text/x-mathjax-config">
//   MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
// </script>
//     <script type="text/javascript" async
//             src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML">
//     </script>

var app = Elm.Main.fullscreen(
    {
        width:  window.innerWidth,
        height: window.innerHeight
    }
  );


app.ports.render.subscribe(function(rendered_text) {
    document.getElementById('rendered_text2').innerHTML = rendered_text
});

  app.ports.toJs.subscribe(function (str) {
    console.log("From Elm: " + str);
    var obj = JSON.parse(str)
    console.log("JSON object = " + JSON.stringify(obj))
  });


  // MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});

/**
  app.ports.render.subscribe(function(rendered_text) {
        document.getElementById('rendered_text2').innerHTML = rendered_text
        MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
    });
**/

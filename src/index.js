'use strict';

// require('ace-css/css/ace.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
// require('./index.html');
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

  document.getElementById("rendered_text2").style.visibility = "visible";

  var count = 0;

  app.ports.toJs.subscribe(function (str) {
    //console.log("From Elm: " + str);
    var settings = JSON.parse(str)
    // console.log("JSON object = " + JSON.stringify(settings))

    var reader_height = (settings.height - 180)+ "px"
    var editor_height = (settings.height - 180)+ "px"

    var reader_width = (0.4*settings.width - 65 ) + "px"
    var reader_left =  (0.20*settings.width + 0)+ "px"

    var editor_width = (0.4*settings.width - 65) + "px"
    var editor_left = (0.6*settings.width + 5)+ "px"
    // console.log("editor_width: " + editor_width)

     switch(settings.page) {
        case "HomePage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = "6000px";
            break;
        case "ReaderPage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = reader_left;
            document.getElementById("rendered_text2").style.width = reader_width;
            document.getElementById("rendered_text2").style.height = reader_height;
            //document.getElementById("rendered_text2").style.background_color = pink;
            break;
        case "EditorPage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = editor_left;
            document.getElementById("rendered_text2").style.width = editor_width;
            document.getElementById("rendered_text2").style.height = editor_height;
            //document.getElementById("rendered_text2").style.background_color = yellow;
            break;
        default:
            document.getElementById("rendered_text2").style.visibility = "hidden";
    }


})


  app.ports.render.subscribe(function(rendered_text) {

      requestAnimationFrame(function() {
        document.getElementById('rendered_text2').innerHTML = rendered_text
        count = count + 1;
        console.log("(" + count + ") Rendering ...")
        // MathJax.Hub.Queue(["Typeset", MathJax.Hub, "rendered_text"]);
        MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
      })

      // requestAnimationFrame(function() {
      //   MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
      // })

  })

  // function updateMathContent(s) {
  //   var math = MathJax.Hub.getAllJax("mathdiv")[0];
  //   MathJax.Hub.Queue(["Text", math, s]);
  // }

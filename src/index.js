'use strict';

require('font-awesome/css/font-awesome.css');
require('./css/style.css')
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// https://hackernoon.com/how-elm-ports-work-with-a-picture-just-one-25144ba43cdd

  var app = Elm.Main.fullscreen(
      {
          width:  window.innerWidth,
          height: window.innerHeight
      }
    );


  app.ports.render.subscribe(function(rendered_text) {

      requestAnimationFrame(function() {

        var asciidoctor = Asciidoctor();
        var content = rendered_text
        var html = asciidoctor.convert(content);

        document.getElementById('rendered_text2').innerHTML = html
        MathJax.Hub.Queue(["Typeset", MathJax.Hub, "rendered_text2"]);

      })

  })

  document.getElementById("rendered_text2").style.visibility = "hidden";

  var count = 0;

  app.ports.toJs.subscribe(function (str) {
    console.log("app.ports.toJs: " + str);
    var settings = JSON.parse(str)
    console.log("JSON object = " + JSON.stringify(settings))

    var reader_height = (settings.height - 130)+ "px"
    var editor_height = (settings.height - 130)+ "px"

    var reader_width = (Math.max(0.5*settings.width, 300)) + "px"
    var reader_left =  300 + "px"

    var editor_width = (0.4*settings.width - 63) + "px"
    var editor_left = (0.6*settings.width + 1)+ "px"

     switch(settings.page) {
        case "HomePage":
            document.getElementById("rendered_text2").style.visibility = "hidden";
            document.getElementById("rendered_text2").style.left = reader_left;
            document.getElementById("rendered_text2").style.width = "100px";
            document.getElementById("rendered_text2").style.height = "100px";
            console.log(":HomePage")
            break;
        case "ReaderPage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = reader_left;
            document.getElementById("rendered_text2").style.width = reader_width;
            document.getElementById("rendered_text2").style.height = reader_height;
            console.log(":ReaderPage")
            break;
        case "EditorPage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = editor_left;
            document.getElementById("rendered_text2").style.width = editor_width;
            document.getElementById("rendered_text2").style.height = editor_height;
            console.log(":EditorPage")
            break;
        default:
            document.getElementById("rendered_text2").style.visibility = "hidden";
    }

    if ((settings.signed_in == false) && (settings.page != "ReaderPage")) {
      document.getElementById("rendered_text2").style.visibility = "hidden";
    }


})

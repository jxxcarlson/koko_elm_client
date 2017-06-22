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

    var toc_width = 300
    var textarea_width = 0.5*(settings.width - toc_width)
    var fudgeFactor = 70

    var reader_left =  toc_width
    // if (Math.min(settings.width - 300) < 700 {
    //   var reader_width = Math.min(settings.width - 300
    // } else {
    //   var reader_width = 700
    // }
    var reader_width = settings.width - 360
    console.log ("READER  WIDTH: "  + reader_width)
    var reader_height = (settings.height - 190)

    var editor_left = toc_width + textarea_width
    var editor_width = (settings.width - (toc_width + textarea_width)) - fudgeFactor
    var editor_height = (settings.height - 190)

     switch(settings.page) {
        case "HomePage":
            document.getElementById("rendered_text2").style.visibility = "hidden";
            document.getElementById("rendered_text2").style.left = reader_left + "px";
            document.getElementById("rendered_text2").style.width = "100px";
            document.getElementById("rendered_text2").style.height = "100px";
            console.log(":HomePage")
            break;
        case "ReaderPage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = reader_left + "px";
            document.getElementById("rendered_text2").style.width = reader_width + "px";
            document.getElementById("rendered_text2").style.height = reader_height + "px";
            console.log(":ReaderPage")
            break;
        case "EditorPage":
            document.getElementById("rendered_text2").style.visibility = "visible";
            document.getElementById("rendered_text2").style.left = editor_left + "px";
            document.getElementById("rendered_text2").style.width = editor_width + "px";
            document.getElementById("rendered_text2").style.height = editor_height + "px";
            console.log(":EditorPage")
            break;
        default:
            document.getElementById("rendered_text2").style.visibility = "hidden";
    }

    if ((settings.signed_in == false) && (settings.page != "ReaderPage")) {
      document.getElementById("rendered_text2").style.visibility = "hidden";
    }


})

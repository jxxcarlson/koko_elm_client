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


  var request_in_progress = false;
  var current_content = '';

  app.ports.render.subscribe(function(content) {

      requestAnimationFrame(function() {

        count = count + 1
        console.log("Render count: " + count)

        var asciidoctor = Asciidoctor();

       ///

      //  var render_text = function(content) {
      //     request_in_progress = true;
      //     console.log("Rendering ... ")
      //     var millisecondsToWait = 3000;
      //     setTimeout(function() {
      //         console.log("Completed! " );
      //         request_in_progress = false;
      //         if (content !== current_content) {
      //           // render_text(current_text);
      //           document.getElementById('rendered_text2').innerHTML = asciidoctor.convert(content);
      //         }
      //     }  , millisecondsToWait);
      //  }

      // render_text(content)

    ///

     document.getElementById('rendered_text2').innerHTML = asciidoctor.convert(content);



        MathJax.Hub.Queue(["Typeset", MathJax.Hub, "rendered_text2"]);

      })

  })

  document.getElementById("rendered_text2").style.visibility = "hidden";

  var count = 0;

  function setAppearance(settings) {

    var toc_width = 300
    var textarea_width = 0.5*(settings.width - toc_width)
    var fudgeFactor = 70

    var reader_left =  toc_width
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
  }

  app.ports.toJs.subscribe(function (str) {
    // console.log("app.ports.toJs: " + str);
    var settings = JSON.parse(str)
    // console.log("JSON object = " + JSON.stringify(settings))
    setAppearance(settings)

})

app.ports.persist.subscribe(function (str) {
  console.log("I will put this in local storage: " + str);
  var userSettings = JSON.parse(str)
  console.log("userSettings = " + JSON.stringify(userSettings))
  localStorage.setItem("username", userSettings.username);
  localStorage.setItem("token", userSettings.token);
  localStorage.setItem("email", userSettings.email);
  localStorage.setItem("name", userSettings.name);

   // prepareLocalStorage();

})

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function reconnect(localStorageAsString) {
  console.log('Request to reconnect received.');
  await sleep(300);
  app.ports.reconnectUser.send(localStorageAsString);
  console.log('Request to reconnect EXECUTED.');
}

app.ports.askToReconnectUser.subscribe(function (str) {
  console.log("app.ports.reconnectUser received: " + str);
  if (str == "reconnectUser") {
    var localStorageAsString = JSON.stringify(localStorage)
    console.log("ask to reconnect user with data: " + localStorageAsString)
    app.ports.reconnectUser.send(localStorageAsString);

    // app.ports.reconnectUser.send(localStorageAsString);
    // reconnect(localStorageAsString)
    app.ports.toElm.send("Yada yada!");
  } else {
    console.log("I dont't unerstand that: " + str)
  }

})



// app.ports.localStorageToElm.send(prepareLocalStorage());

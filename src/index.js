'use strict';

require('font-awesome/css/font-awesome.css');
require('./css/extra.css')
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


  var asciidoctor = Asciidoctor();

  function typesetNow(){
    console.log("calling MathJax.Hub.Queue ... ")
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
  }

  var request_in_progress = false;
  var current_content = '';

  var render_asciidoc = function(content) {
      request_in_progress = true;
      console.log("Rendering ... ")
      var millisecondsToWait = 100;
      setTimeout(function() {
          console.log("Completed! " );
          request_in_progress = false;
          if (content !== current_content) {
            document.getElementById('rendered_text2').innerHTML = asciidoctor.convert(content);
            typesetNow()
          }
      }  , millisecondsToWait);
   }

   var render_latex = function(content) {
       request_in_progress = true;
       console.log("Rendering ... ")
       var millisecondsToWait = 100;
       setTimeout(function() {
           console.log("Completed! " );
           request_in_progress = false;
           if (content !== current_content) {
             document.getElementById('rendered_text2').innerHTML = content;
             typesetNow()
           }
       }  , millisecondsToWait);
    }

   var render_plain = function(content) {
       request_in_progress = true;
       console.log("Rendering ... ")
       var millisecondsToWait = 100;
       setTimeout(function() {
           console.log("Completed! " );
           request_in_progress = false;
           if (content !== current_content) {
             document.getElementById('rendered_text2').innerHTML = "<pre>\n" + content + "\n</pre>\n\n";
           }
       }  , millisecondsToWait);
    }

  app.ports.render.subscribe(function(data) {

      requestAnimationFrame(function() {

          count = count + 1
          console.log("Render count: " + count)
          console.log("DocType = " + data.textType)
          console.log("content: " + data.content)
          switch (data.textType) {

            case "adoc":
               render_asciidoc(data.content)
               break;
            case "plain":
               render_plain(data.content)
               break;
            case "latex":
                render_latex(data.content)
                break;
            default:
              console.log("Default rendering ... asciidoc")
              render_asciidoc(data.content)
          }


      })

  })

  document.getElementById("rendered_text2").style.visibility = "hidden";

  var count = 0;

  function setAppearance(settings) {

    var toc_width = 300
    var textarea_width = 0.5*(settings.width - toc_width)
    var fudgeFactor = 70

    var reader_left =  toc_width
    var reader_width = Math.min(600, (settings.width -360)) // settings.width - 360
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

app.ports.fileSelected.subscribe(function (id) {
  var node = document.getElementById(id);
  if (node === null) {
    return;
  }
  // If your file upload field allows multiple files, you might
  // want to consider turning this into a `for` loop.
  var file = node.files[0];
  var reader = new FileReader();

  // FileReader API is event based. Once a file is selected
  // it fires events. We hook into the `onload` event for our reader.
  reader.onload = (function(event) {
    // The event carries the `target`. The `target` is the file
    // that was selected. The result is base64 encoded contents of the file.
    var base64encoded = event.target.result;
    // We build up the `ImagePortData` object here that will be passed to our Elm
    // runtime through the `fileContentRead` subscription.
    var portData = {
      contents: base64encoded,
      filename: file.name
    };

    // We call the `fileContentRead` port with the file data
    // which will be sent to our Elm runtime via Subscriptions.
    app.ports.fileContentRead.send(portData);
  });

  // Connect our FileReader with the file that was selected in our `input` node.
  reader.readAsDataURL(file);
});



// app.ports.localStorageToElm.send(prepareLocalStorage());

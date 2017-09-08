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

  function typesetNow(){
    console.log("** calling MathJax.Hub.Queue ... ")
    MathJax.Hub.Queue(["Typeset", MathJax.Hub, function(){app.ports.getRenderedText.send(document.getElementById('rendered_text2').innerHTML)}]);
  }

  var request_in_progress = false;
  var current_content = '';

  var asciidoctor = Asciidoctor();

  var render_asciidoc = function(content) {
      console.log("content length = " + content.length)
      request_in_progress = true;
      console.log("Rendering ... ")
      var millisecondsToWait = 100;
      setTimeout(function() {
          console.log("RENDER AS ASCIIDOC " );
          request_in_progress = false;
          if (content !== current_content) {
            var rt = asciidoctor.convert(content, {safe: 'safe', attributes: 'icons=font'})
            app.ports.getRenderedText.send(rt); // Send rendered text to Elm
            current_content = content
          }
      }  , millisecondsToWait);
   }

   var render_asciidoc_latex = function(content) {
       console.log("content length = " + content.length)
       request_in_progress = true;
       var millisecondsToWait = 100;
       setTimeout(function() {
           request_in_progress = false;
           if (content !== current_content) {
             document.getElementById('rendered_text2').innerHTML = asciidoctor.convert(content, {safe: 'safe', attributes: 'icons=font'});
             typesetNow()
             current_content = content
           }
       }  , millisecondsToWait);
    }

   var render_latex = function(content) {
       console.log("content length = " + content.length)
       request_in_progress = true;
       var millisecondsToWait = 100;
       setTimeout(function() {
           request_in_progress = false;
           if (content !== current_content) {
             document.getElementById('rendered_text2').innerHTML = content;
             typesetNow()
             current_content = content
           }
       }  , millisecondsToWait);
    }

   var render_plain = function(content) {
       console.log("plain content length = " + content.length)
       request_in_progress = true;
       var millisecondsToWait = 100;
       setTimeout(function() {
           request_in_progress = false;
           if (content !== current_content) {
             app.ports.getRenderedText.send("<pre>\n" + content + "\n</pre>\n\n");
             current_content = content
           }
       }  , millisecondsToWait);
    }

  app.ports.render.subscribe(function(data) {

      requestAnimationFrame(function() {

          count = count + 1
          switch (data.textType) {

            case "adoc":
               render_asciidoc(data.content)
               break;
            case "adoc_latex":
               render_asciidoc_latex(data.content)
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


function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}


// PERSIST AND RECONNECT USER

app.ports.persist.subscribe(function (str) {
  console.log("I will put this in local storage: " + str);
  var userSettings = JSON.parse(str)
  console.log("userSettings = " + JSON.stringify(userSettings))
  localStorage.setItem("username", userSettings.username);
  localStorage.setItem("token", userSettings.token);
  localStorage.setItem("id", userSettings.id);
  localStorage.setItem("email", userSettings.email);
  localStorage.setItem("name", userSettings.name);
  localStorage.setItem("blurb", userSettings.blurb);

   // prepareLocalStorage();

})

async function reconnect(localStorageAsString) {
  console.log('Request to reconnect received.');
  await sleep(300);
  app.ports.reconnectUser.send(localStorageAsString);
  console.log('Request to reconnect EXECUTED.');
}

app.ports.disconnectUser.subscribe(function () {
  console.log("app.ports.disconnectUser command received");
  localStorage.clear()
  console.log("local storage cleared")
})

app.ports.askToReconnectUser.subscribe(function (str) {
  console.log("app.ports.reconnectUser received: " + str);
  if (str == "reconnectUser") {
    var localStorageAsString = JSON.stringify(localStorage)
    console.log("ask to reconnect user with data: " + localStorageAsString)
    app.ports.reconnectUser.send(localStorageAsString);
    app.ports.toElm.send("Yada yada!");
  } else {
    console.log("I dont't unerstand that: " + str)
  }

})


// FILE UPLOAD I

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

// FILE UPLOAD II (@zghor)

   app.ports.fileUpload.subscribe(function(id) {
     var node = document.getElementById(id);
     if (node === null) {
       return;
     }
     var file = node.files[0];
     var reader = new FileReader();
     reader.onload = (function(event) {
       var xhr = new XMLHttpRequest();
       xhr.open("POST", "/upload/endpoint");

       var formData = new FormData();
       formData.append("file", file);
       xhr.send(formData);
       xhr.onload = function(e) {
         if (xhr.readyState === 4) {
           if (xhr.status === 200) {
             app.ports.fileUploaded.send(true);
           } else {
             app.ports.fileUploaded.send(false);
           }
         }
       }
     });
     reader.readAsArrayBuffer(file);
   });



// app.ports.localStorageToElm.send(prepareLocalStorage());

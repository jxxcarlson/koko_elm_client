'use strict';

require('font-awesome/css/font-awesome.css');
require('./css/extra.css')
require('./index.html');


// https://hackernoon.com/how-elm-ports-work-with-a-picture-just-one-25144ba43cdd

  var Elm = require('./Main.elm');
  var mountNode = document.getElementById('main');

  var app = Elm.Main.fullscreen(
      {
          width:  window.innerWidth,
          height: window.innerHeight
      }
    );

  var request_in_progress = false;
  var current_content = '';
  var asciidoctor = Asciidoctor();
  var count = 0;

//   // Main function called through ports to render text. 
//   app.ports.putTextToRender.subscribe(function(data) {

//     console.log("port pttr, id:: " + data.id)
//     console.log("     type:: " + data.textType)
//     console.log("     length:: " + data.content.length)

//     if (data.force == true) {
//        console.log("DEBOUNCE = TRUE")
//     } else {
//       console.log("DEBOUNCE = FALSE")
//     }

//     requestAnimationFrame(function() {

//         count = count + 1
//         switch (data.textType) {

//           case "adoc":
//              render_asciidoc(data.content)
//              break;
//           case "adoc_latex":
//              render_asciidoc_latex(data.content)
//              break;
//           case "plain":
//              render_plain(data.content)
//              break;
//           case "latex":
//               // force = true
//               render_latex(true, data.idList, data.content)
//               break;
//           default:
//             console.log("Default rendering ... asciidoc")
//             render_asciidoc(data.content)
//         }
//     })
// })

var processDocumentContent = function(data) {

  if (data.force == true) {
     console.log("DEBOUNCE = TRUE")
  } else {
    console.log("DEBOUNCE = FALSE")
  }

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
            // force = true
            render_latex(true, data.idList, data.content)
            break;
        default:
          console.log("Default rendering ... asciidoc")
          render_asciidoc(data.content)
      }
  })
}

// INFO FOR OUTSIDE

app.ports.infoForOutside.subscribe(msg => {
  if (msg.tag == "PutTextToRender") {
    processDocumentContent(msg.data)
   
  } 

})


document.getElementById("rendered_text2").style.visibility = "hidden";

  var render_asciidoc = function(content) {
      console.log(":: render_asciidoc, length = " + content.length)
      request_in_progress = true;
      var millisecondsToWait = 100;
      setTimeout(function() {
          request_in_progress = false;
          if (content !== current_content) {
            var rt = asciidoctor.convert(content, {safe: 'safe', attributes: 'icons=font'})
            app.ports.infoForElm.send({ tag: "RenderedText", data: rt });
            console.log("port rad:: send to Elm, id:: " + data.id)
            current_content = content
          }
      }  , millisecondsToWait);
   }

   var render_asciidoc_latex = function(content) {
       console.log("render_asciidoc_latex, content length = " + content.length)
           if (content !== current_content) {
             document.getElementById('rendered_text2').innerHTML = asciidoctor.convert(content, {safe: 'safe', attributes: 'icons=font'});
             typeset()
             current_content = content
             var rt = document.getElementById('rendered_text2').innerHTML
             app.ports.infoForElm.send({ tag: "RenderedText", data: rt });
           }
    }

  var send_rendered_text = function() {
    var rt = document.getElementById('rendered_text2').innerHTML  
    app.ports.infoForElm.send({ tag: "RenderedText", data: rt });
  }

   function typeset() {
    MathJax.Hub.Queue( ["Typeset", MathJax.Hub, send_rendered_text] );
  }

  function typeset2() {
    console.log(":: typesetting document ... ")
    MathJax.Hub.Queue( ["Typeset", MathJax.Hub, ] );
  }


  var render_latex = function(force, idList, content) {
    document.getElementById('rendered_text2').innerHTML = content;
    typeset()
  }

   var render_plain = function(content) {
       console.log("rende_plain,  content length = " + content.length)
       request_in_progress = true;
       var millisecondsToWait = 100;
       setTimeout(function() {
           request_in_progress = false;
           if (content !== current_content) {
             app.ports.infoForElm.send({ tag: "RenderedText", data: "<pre>\n" + content + "\n</pre>\n\n" });
             current_content = content
           }
       }  , millisecondsToWait);
    }


  





// PERSIST AND RECONNECT USER

app.ports.saveUserLogin.subscribe(function (str) {
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


app.ports.saveUserState.subscribe(function (str) {
  console.log("I will put the documentStack in local storage");
  var data = JSON.parse(str)

  console.log("data.documentStack = " + data.documentStack )
  console.log("data.currentDocumentId (1) = " + data.currentDocumentId )

  localStorage.setItem("documentStack", data.documentStack);
  localStorage.setItem("currentDocumentId", data.currentDocumentId);
})

app.ports.saveDocumentStack.subscribe(function (str) {
  console.log("xxx I will put the documentStack in local storage");
  var data = JSON.parse(str)
  console.log("data.documentStack = " + data.documentStack )
  localStorage.setItem("documentStack", data.documentStack);
})

app.ports.saveCurrentDocumentId.subscribe(function (str) {
  console.log("xxx I will put the CurrentDocumentId in local storage");
  var data = JSON.parse(str)
  console.log("data.currentDocumentId (2)= " + data.currentDocumentId )
  localStorage.setItem("currentDocumentId", data.currentDocumentId);
})

app.ports.askToRecoverUserState.subscribe(function (str) {
  console.log("app.ports.askToRecoverUserState received: " + str);
  if (str == "recoverUserState") {
    var localStorageAsString = JSON.stringify(localStorage)
    console.log("ask to recover user state with data: " + localStorageAsString)
    app.ports.recoverUserState.send(localStorageAsString);
  } else {
    console.log("I don't unerstand that: " + str)
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

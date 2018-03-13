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
  switch(msg.tag) {

     case "PutTextToRender":
       processDocumentContent(msg.data)
       break;
      
    case "UserData":
       processUserData(msg.data)
       break; 

    case "UserState":
      processUserState(msg.data)
      break; 

    case "SaveDocumentStack":
      processDocumentStack(msg.data)
      break;

    case "AskToRecoverUserState":
      askToRecoverUserState(msg.data)
      break;

    case "AskToReconnectUser":
      askToReconnectUser(msg.data)
      break;   
  }
  
})

// LogErr "Expecting a List at _.documentStack but instead got: \"559,628\""

var askToReconnectUser = function (str) {
  console.log("reconnectUser");
  app.ports.infoForElm.send({tag: "ReconnectUser", data: localStorage})
}

var askToRecoverUserState = function(str) {
  console.log("RecoverUserState" + JSON.stringify(localStorage));
  app.ports.infoForElm.send({tag: "RecoverUserState", data: localStorage});
}
 
var processDocumentStack = function(data) {
  console.log("xxx I will put the documentStack in local storage"); 
  localStorage.setItem("documentStack", data.documentStack);
}

var processUserData = function(userSettings) {
  console.log("I will put this stuff in local storage" );
  localStorage.setItem("username", userSettings.username);
  localStorage.setItem("token", userSettings.token);
  localStorage.setItem("id", userSettings.id);
  localStorage.setItem("email", userSettings.email);
  localStorage.setItem("name", userSettings.name);
  localStorage.setItem("blurb", userSettings.blurb);
}

var processUserState = function(data) {
  console.log("I will put the documentStack in local storage");
  localStorage.setItem("documentStack", data.documentStack);
  localStorage.setItem("currentDocumentId", data.currentDocumentId);
}


document.getElementById("rendered_text2").style.visibility = "hidden";

  var render_asciidoc = function(content) {
      request_in_progress = true;
      var millisecondsToWait = 100;
      setTimeout(function() {
          request_in_progress = false;
          if (content !== current_content) {
            var rt = asciidoctor.convert(content, {safe: 'safe', attributes: 'icons=font'})
            app.ports.infoForElm.send({ tag: "RenderedText", data: rt });
            current_content = content
          }
      }  , millisecondsToWait);
   }

   var render_asciidoc_latex = function(content) {
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





// async function reconnect(localStorageAsString) {
//   console.log('Request to reconnect received.');
//   await sleep(300);
//   app.ports.reconnectUser.send(localStorageAsString);
//   console.log('Request to reconnect EXECUTED.');
// }

app.ports.disconnectUser.subscribe(function () {
  console.log("app.ports.disconnectUser command received");
  localStorage.clear()
  console.log("local storage cleared")
})




// app.ports.askToRecoverUserState.subscribe(function (str) {
//   console.log("app.ports.askToRecoverUserState received: " + str);
//   if (str == "recoverUserState") {
//     var localStorageAsString = JSON.stringify(localStorage)
//     console.log("ask to recover user state with data: " + localStorageAsString)
//     app.ports.recoverUserState.send(localStorageAsString);
//   } else {
//     console.log("I don't unerstand that: " + str)
//   }

// })


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

// To use the examples, swap the commenting on the next two lines

    var _user$project$Native_FileReader = function() {
    // var _simonh1000$file_reader$Native_FileReader = function() {

        var scheduler = _elm_lang$core$Native_Scheduler;
    
        function useReader(method, fileObjectToRead) {
            return scheduler.nativeBinding(function(callback){
    
                /*
                 * Test for existence of FileReader using
                 * if(window.FileReader) { ...
                 * http://caniuse.com/#search=filereader
                 * main gap is IE10 and 11 which do not support readAsBinaryFile
                 * but we do not use this API either as it is deprecated
                 */
                var reader = new FileReader();
    
                reader.onload = function(evt) {
                    return callback(scheduler.succeed(evt.target.result));
                };
    
                reader.onerror = function() {
                    return callback(scheduler.fail({ctor : 'ReadFail'}));
                };
    
                // Error if not passed an objectToRead or if it is not a Blob
                if (!fileObjectToRead || !(fileObjectToRead instanceof Blob)) {
                    return callback(scheduler.fail({ctor : 'NoValidBlob'}));
                }
    
                if (reader[method]) {
                    var result = reader[method](fileObjectToRead);
                    // prevent memory leak by nullifying fileObjectToRead
                    fileObjectToRead = null;
                    return result;
                } else {
                    return callback(scheduler.fail({ctor : 'ReadFail'}));
                }
            });
        }
    
        // readAsTextFile : Value -> Task error String
        var readAsTextFile = function(fileObjectToRead){
            return useReader("readAsText", fileObjectToRead);
        };
    
        // readAsArrayBuffer : Value -> Task error String
        var readAsArrayBuffer = function(fileObjectToRead){
            return useReader("readAsArrayBuffer", fileObjectToRead);
        };
    
        // readAsDataUrl : Value -> Task error String
        var readAsDataUrl = function(fileObjectToRead){
            return useReader("readAsDataURL", fileObjectToRead);
        };
    
        var filePart = function(name, blob) {
            return {
                _0: name,
                _1: blob
            }
        };
    
        var rawBody = function (mimeType, blob) {
            return {
                ctor: "StringBody",
                _0: mimeType,
                _1: blob
            };
        };
    
        return {
            readAsTextFile : readAsTextFile,
            readAsArrayBuffer : readAsArrayBuffer,
            readAsDataUrl: readAsDataUrl,
            filePart: F2(filePart),
            rawBody: F2(rawBody)
        };
    }();
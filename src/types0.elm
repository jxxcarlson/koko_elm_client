module Types exposing (..)

import Http
import Navigation


type alias Document =
    { id : Int
    , title : String
    , author : String
    , identifier : String
    , author_identifier : String
    , text : String
    }


type alias DocumentId =
    String


type alias Author =
    { name : String, identifier : String, url : String, photo_url : String }


type alias User =
    { name : String, username : String, email : String, password : String, token : String }


type alias Model =
    { page : Page
    , info : String
    , errorMsg : String
    , current_user : User
    , registerUser : Bool
    , -- Text inputs
      input_text : String
    , author_identifier : String
    , selectedAuthor : Author
    , authors : List Author
    , documents : List Document
    , selectedDocument : Document
    , editor_text : String
    }


type Msg
    = SelectDocument Document
    | SelectAuthor Author
    | Input String
    | KeyUp Int
    | GetDocuments (Result Http.Error String)
    | GetAuthor (Result Http.Error String)
    | GetAuthors (Result Http.Error String)
    | GetAllAuthors
      -- | OnLocationChange Navigation.Location
    | GoToEditor
    | GoToReader
    | GoToNewDocument
    | GoToLogin
    | UpdateSelectedDocument String
    | InputTitle String
    | InputAuthor String
    | DoSaveDocument
    | SaveDocument (Result Http.Error String)
    | Email String
    | Password String
    | Name String
    | Username String
    | Login
    | Register
    | Signout
    | ToggleRegister
    | GetTokenCompleted (Result Http.Error String)


type Page
    = ReaderPage
    | EditorPage
    | NewDocumentPage
    | LoginPage
    | NotFoundPage

module Main exposing (main)

import Browser


type alias Model =
    { title : String
    }


type Msg
    = Never



-- MODEL / INIT


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { title = "simple demo"
            }
    in
    ( model, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        _ =
            Debug.log "hello" "hello"
    in
    { title = model.title
    , body = []
    }

module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onInput)
import Html.Lazy


type alias Model =
    { title : String
    , fred : String
    }


type Msg
    = DontActuallyUpdateTheModel String



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
            , fred = "fred" -- NEVER CHANGES!!!!
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
        lotsOfFreds : String -> Html Msg
        lotsOfFreds s =
            let
                -- We should only see this once, but it happens every time
                -- you type in the textarea
                _ =
                    Debug.log "actually calling lotsOfFreds" ""
            in
            Html.pre [] [ Html.text (String.join "\n" (List.repeat 100000 s)) ]

        body =
            [ Html.textarea
                [ onInput DontActuallyUpdateTheModel ]
                [ Html.text "type in here to repro bug (and open debugger)" ]
            , Html.Lazy.lazy lotsOfFreds model.fred
            ]
    in
    { title = model.title
    , body = body
    }

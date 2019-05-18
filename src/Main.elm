module Main exposing (..)

import Browser
import Html exposing (Html)
import List exposing (..)
import List.Extra exposing (lift2)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)


--import Color


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { svg : List (Svg Msg)
    , backgroundSvg : Maybe (Svg Msg)
    , level : Int
    , transformers : List Transformation
    }


type Transformation
    = Transformation Scale Translate Rotation


type alias Scale =
    Float


type alias Translate =
    ( Float, Float )


type alias Rotation =
    Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( { svg = [ whiteSquare ]
      , backgroundSvg = Just blackSquare
      , level = 1
      , transformers = [ Transformation 0.3333 ( 0, 0 ) 0 ]
      }
    , Cmd.none
    )


type Msg
    = RenderNext Int Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RenderNext level now ->
            let
                newModel =
                    { model | svg = [ whiteSquare ] }
            in
                ( newModel, Cmd.none )



--will apply fractal function and append to model List


view : Model -> Svg Msg
view model =
    svg
        [ width (String.fromInt fullWidth)
        , height (String.fromInt fullHeight)
        , viewBox ("0 0 " ++ (String.fromInt fullWidth) ++ " " ++ (String.fromInt fullHeight))
        ]
        ([ Maybe.withDefault (Svg.text "") model.backgroundSvg ]
            ++ model.svg
        )


fullWidth =
    600


fullHeight =
    600


blackSquare : Svg Msg
blackSquare =
    rect
        [ x "0"
        , y "0"
        , width "600"
        , height "600"
        , fill "black"
        ]
        []


whiteSquare : Svg Msg
whiteSquare =
    rect
        [ x "200"
        , y "200"
        , width "200"
        , height "200"
        , fill "white"
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 (RenderNext (model.level + 1))

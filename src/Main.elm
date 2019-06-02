module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Html
import List exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)
import Task


--import Color


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { fractalSvg : List (List (Svg Msg))
    , backgroundSvg : Maybe (Svg Msg)
    , level : Int
    , baseTransform : Translate
    , transformers : List Transformation
    , maxDepth : Int
    }


type alias Transformation =
    { scale : Scale
    , translate : Translate
    , rotation : Rotation
    }


type alias Scale =
    Float


type alias Translate =
    ( Float, Float )


type alias Rotation =
    Int


fullWidth =
    600


fullHeight =
    600


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fractalSvg = [ [ whiteSquare ] ]
      , backgroundSvg = Just blackSquare
      , level = 0
      , transformers =
            [ Transformation 0.3333 ( -2 / 3, -2 / 3 ) 0
            , Transformation 0.3333 ( 1 / 3, -2 / 3 ) 0
            , Transformation 0.3333 ( 4 / 3, -2 / 3 ) 0
            , Transformation 0.3333 ( 4 / 3, 1 / 3 ) 0
            , Transformation 0.3333 ( 4 / 3, 4 / 3 ) 0
            , Transformation 0.3333 ( 1 / 3, 4 / 3 ) 0
            , Transformation 0.3333 ( -2 / 3, 4 / 3 ) 0
            , Transformation 0.3333 ( -2 / 3, 1 / 3 ) 0
            ]
      , baseTransform = ( fullWidth / 3, fullHeight / 3 )
      , maxDepth = 5
      }
    , Cmd.none
    )


type Msg
    = RenderNext Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RenderNext now ->
            let
                newModel =
                    if model.level < model.maxDepth then
                        { model
                            | fractalSvg =
                                if model.level > 0 then
                                    (calcNewLevel (model.level) model.transformers (Maybe.withDefault [] (List.head model.fractalSvg))) :: model.fractalSvg
                                else
                                    model.fractalSvg
                            , level = model.level + 1
                        }
                    else
                        model
            in
                ( newModel, Cmd.none )



--will apply fractal function and append to model List


calcNewLevel : Int -> List Transformation -> List (Svg Msg) -> List (Svg Msg)
calcNewLevel level transformers baseSvgs =
    List.concatMap
        (\svg ->
            List.concatMap
                (\transformation ->
                    [ g
                        [ transform
                            ("translate"
                                ++ translationToString (translationNextLevel (transformation.translate) transformation.scale level)
                                ++ " scale("
                                ++ String.fromFloat (transformation.scale)
                                ++ ")"
                            )
                        , fill "red"
                        ]
                        [ svg ]
                    ]
                )
                transformers
        )
        baseSvgs


translationNextLevel : Translate -> Scale -> Int -> Translate
translationNextLevel ( x, y ) scale level =
    ( (x * fullWidth * scale), y * fullHeight * scale )


translationToString : Translate -> String
translationToString ( x, y ) =
    "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width (String.fromInt fullWidth)
            , height (String.fromInt fullHeight)
            , viewBox ("0 0 " ++ (String.fromInt fullWidth) ++ " " ++ (String.fromInt fullHeight))
            ]
            ([ Maybe.withDefault (Svg.text "") model.backgroundSvg ]
                ++ [ g
                        [ transform ("translate " ++ translationToString model.baseTransform) ]
                        (List.concat model.fractalSvg)
                   ]
            )
        , text ("Depth = " ++ (String.fromInt model.level))
        ]


blackSquare : Svg Msg
blackSquare =
    rect
        [ x "0"
        , y "0"
        , width (String.fromInt (fullWidth))
        , height (String.fromInt (fullHeight))
        , fill "black"
        ]
        []


whiteSquare : Svg Msg
whiteSquare =
    rect
        [ width (String.fromInt (fullWidth // 3))
        , height (String.fromInt (fullHeight // 3))
        , fill "red"
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 RenderNext

module Leaf exposing (Model, Msg(..), State(..), add, area, init, leaf2, main, myShapes, pointsFromDotFaild, pointsFromRandomDotOrg, sub, tryAgain, update, view)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)


myShapes model =
    if model.score == 5 then
        [ rect 192 128 |> filled black
        , text "Successful"
            |> centered
            |> bold
            |> size 28
            |> filled green
            |> move ( 0, 15 )
        , group
            [ roundedRect 60 20 5
                |> filled green
                |> move ( 0, -14.5 )
            , text "Try again!"
                |> centered
                |> size 10
                |> filled black
                |> move ( 0, -18 )
            ]
            |> notifyTap Restart
        ]

    else
        [ rect 60 150
            |> filled (rgb 238 238 228)
            |> move ( -90, 0 )
        , rect 170 20
            |> filled black
            |> move ( 25, 60 )
        , text "Swipe the leaves to the left to complete the task!"
            |> alignLeft
            |> size 6.1
            |> filled green
            |> move ( -57.5, 54 )
        , roundedRect 30 20 4
            |> filled black
            |> move ( -77, 50 )
        , text "Timer:"
            |> size 8
            |> filled white
            |> move ( -88, 52 )
        , text (String.fromFloat (model.time - model.currentTime))
            |> alignLeft
            |> size 7
            |> filled white
            |> move ( -87, 43 )
        , rect 300 120
            |> filled blue
            |> move ( 90, -10 )
        , rect 22 63
            |> filled black
            |> move ( -77.5, -5 )
        , roundedRect 4 7 3
            |> filled black
            |> move ( -64, 45 )
        , roundedRect 30 20 4
            |> outlined (solid 2) green
            |> move ( -77, 50 )
        , triangle 5.5
            |> filled (rgb 230 125 50)
            |> rotate (degrees 30)
            |> move ( -77, 33 )
        , triangle 5.5
            |> outlined (solid 0.6) black
            |> rotate (degrees 30)
            |> move ( -77, 33 )
        , triangle 5.5
            |> filled (rgb 230 125 50)
            |> rotate (degrees 90)
            |> move ( -77, -43 )
        , triangle 5.5
            |> outlined (solid 0.6) black
            |> rotate (degrees 90)
            |> move ( -77, -43 )
        , text "You only have 30 seconds to complete the task."
            |> alignLeft
            |> size 6.0
            |> outlined (solid 0.3) red
            |> move ( -57.5, 45 )
        ]
            ++ [ if (model.time - model.currentTime >= 30) && (model.score < 5) then
                    tryAgain

                 else
                    [] |> group
               ]
            ++ (case model.state of
                    Waiting ->
                        model.points
                            |> List.map
                                (\pos ->
                                    leaf2
                                        |> move pos
                                        |> move ( 2 * sin model.time, 2 * sin model.time )
                                        |> notifyMouseDownAt (MouseDownAt pos)
                                )

                    Grabbed delta mouseAt ->
                        (model.points
                            |> List.map
                                (\pos ->
                                    leaf2
                                        |> move pos
                                        |> move ( 2 * sin model.time, 2 * sin model.time )
                                )
                        )
                            ++ [ leaf2
                                    |> move (add delta mouseAt)
                               , area
                                    |> move ( -3, 0 )
                                    |> notifyMouseUp Stop
                                    |> notifyLeave Dissappear
                                    |> notifyLeave Add
                                    |> notifyMouseMoveAt MouseMoveTo
                               ]
               )


type Msg
    = Tick Float GetKeyState
    | MouseDownAt ( Float, Float ) ( Float, Float )
    | MouseMoveTo ( Float, Float )
    | Stop
    | Add
    | Dissappear
    | Restart


type State
    = Waiting
    | Grabbed
        ( Float, Float )
        -- offset from down position to draw position
        ( Float, Float )



-- current mouse position


type alias Model =
    { time : Float
    , points : List ( Float, Float )
    , state : State
    , score : Int
    , currentTime : Float
    }


update msg model =
    case msg of
        Tick t _ ->
            if (model.time - model.currentTime >= 30) && (model.score < 5) then
                { model | points = pointsFromDotFaild, time = t }

            else
                { model | time = t }

        Restart ->
            { model
                | currentTime = model.time
                , points = pointsFromRandomDotOrg
                , state = Waiting
                , score = 0
            }

        MouseDownAt orig mouseAt ->
            { model
                | points = List.filter (\pos -> pos /= orig) model.points
                , state = Grabbed (sub orig mouseAt) mouseAt
            }

        MouseMoveTo new ->
            case model.state of
                Grabbed delta _ ->
                    { model | state = Grabbed delta new }

                _ ->
                    model

        Stop ->
            case model.state of
                Grabbed delta mouseAt ->
                    { model
                        | state = Waiting
                        , points = add delta mouseAt :: model.points
                    }

                _ ->
                    model

        Dissappear ->
            case model.state of
                Grabbed delta mouseAt ->
                    { model
                        | state = Waiting
                        , points = add delta ( 10000, 100000 ) :: model.points
                    }

                _ ->
                    model

        Add ->
            { model | score = model.score + 1 }


sub ( x, y ) ( u, v ) =
    ( x - u, y - v )


add ( x, y ) ( u, v ) =
    ( x + u, y + v )


init : Model
init =
    { time = 0
    , currentTime = 0.0
    , points = pointsFromRandomDotOrg
    , state = Waiting
    , score = 0
    }


main =
    gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }


view model =
    collage 192 128 (myShapes model)


tryAgain =
    [ rect 192 128 |> filled black
    , text "Game Failed"
        |> centered
        |> bold
        |> size 28
        |> filled green
        |> move ( 0, 15 )
    , group
        [ roundedRect 60 20 5
            |> filled green
            |> move ( 0, -14.5 )
        , text "Try again!"
            |> centered
            |> size 10
            |> filled black
            |> move ( 0, -18 )
        ]
        |> notifyTap Restart
    ]
        |> group


pointsFromRandomDotOrg =
    [ ( -32, 10 )
    , ( -43, 50 )
    , ( 50, 10 )
    , ( 20, 35 )
    , ( 60, 60 )
    ]


pointsFromDotFaild =
    [ ( 100000, 1000000 )
    , ( 100000, 1000000 )
    , ( 100000, 1000000 )
    , ( 100000, 1000000 )
    , ( 100000, 1000000 )
    ]


leaf2 =
    group
        [ curve ( 7.36, -45.76 ) [ Pull ( 7.14, -52.16 ) ( 9.92, -58.56 ), Pull ( 11.04, -59.18 ) ( 12.16, -57.6 ), Pull ( 8.4, -51.68 ) ( 7.04, -45.76 ), Pull ( 14.24, -49.58 ) ( 21.44, -46.4 ), Pull ( 22.12, -42.72 ) ( 16, -39.04 ), Pull ( 22.0, -36.36 ) ( 22.4, -30.08 ), Pull ( 17.66, -30.23 ) ( 11.52, -32 ), Pull ( 12.879, -25.43 ) ( 7.04, -18.88 ), Pull ( 1.8, -25.11 ) ( 2.56, -31.36 ), Pull ( -2.719, -29.09 ) ( -8, -29.44 ), Pull ( -7.479, -35.56 ) ( -2.56, -39.68 ), Pull ( -5.499, -42.87 ) ( -7.04, -47.68 ), Pull ( 2.64, -50.72 ) ( 6.72, -45.76 ) ]
            |> filled darkOrange
        , curve ( 7.36, -45.76 ) [ Pull ( 7.14, -52.16 ) ( 9.92, -58.56 ), Pull ( 11.04, -59.18 ) ( 12.16, -57.6 ), Pull ( 8.4, -51.68 ) ( 7.04, -45.76 ), Pull ( 14.24, -49.58 ) ( 21.44, -46.4 ), Pull ( 22.12, -42.72 ) ( 16, -39.04 ), Pull ( 22.0, -36.36 ) ( 22.4, -30.08 ), Pull ( 17.66, -30.23 ) ( 11.52, -32 ), Pull ( 12.879, -25.43 ) ( 7.04, -18.88 ), Pull ( 1.8, -25.11 ) ( 2.56, -31.36 ), Pull ( -2.719, -29.09 ) ( -8, -29.44 ), Pull ( -7.479, -35.56 ) ( -2.56, -39.68 ), Pull ( -5.499, -42.87 ) ( -7.04, -47.68 ), Pull ( 2.64, -50.72 ) ( 6.72, -45.76 ) ]
            |> outlined (solid 0.5) black
        , curve ( 7.04, -45.12 ) [ Pull ( 6.38, -35.54 ) ( 6.72, -23.36 ) ]
            |> outlined (solid 0.5) black
        , curve ( 7.04, -41.28 ) [ Pull ( 12.639, -44.2 ) ( 18.24, -45.12 ) ]
            |> outlined (solid 0.5) black
        , curve ( 7.36, -37.76 ) [ Pull ( 13.76, -36.48 ) ( 20.16, -32 ) ]
            |> outlined (solid 0.5) black
        , curve ( 7.04, -39.36 ) [ Pull ( 0.64, -39.42 ) ( -5.76, -31.68 ) ]
            |> outlined (solid 0.5) black
        , curve ( 7.36, -42.88 ) [ Pull ( 0.96, -45.84 ) ( -5.44, -46.4 ) ]
            |> outlined (solid 0.5) black
        ]


area =
    group
        [ curve ( -96, 63.594 ) [ Pull ( -96, 0.2025 ) ( -96, -63.18 ), Pull ( -71.69, -63.39 ) ( -47.39, -63.59 ), Pull ( -47.18, -47.59 ) ( -46.98, -31.59 ), Pull ( -66.22, -31.59 ) ( -85.46, -31.59 ), Pull ( -86.07, 15.999 ) ( -86.68, 63.594 ), Pull ( -91.34, 63.594 ) ( -96, 63.594 ) ]
            |> filled (rgba 0 0 0 0.0001)
        , curve ( -86.27, 63.594 ) [ Pull ( -86.48, 44.556 ) ( -86.68, 25.518 ), Pull ( -71.29, 25.518 ) ( -55.89, 25.518 ), Pull ( -55.89, -19.03 ) ( -55.89, -63.59 ), Pull ( -51.44, -63.79 ) ( -46.98, -64 ), Pull ( -46.98, -0.202 ) ( -46.98, 63.594 ), Pull ( -66.63, 63.594 ) ( -86.27, 63.594 ) ]
            |> filled (rgba 0 0 0 0.0001)
            |> move ( -7, 0 )
        , curve ( -96, 64 ) [ Pull ( -71.69, 64 ) ( -47.39, 64 ), Pull ( -51.64, 49.215 ) ( -55.89, 34.43 ), Pull ( -73.51, 32 ) ( -91.13, 29.569 ), Pull ( -93.16, 46.582 ) ( -95.18, 63.594 ) ]
            |> filled (rgba 0 0 0 0.0001)
        , curve ( 56.708, 63.594 ) [ Pull ( 58.126, 0 ) ( 59.544, -63.59 ), Pull ( -1.417, -63.59 ) ( -62.37, -63.59 ), Pull ( -63.18, 0.2025 ) ( -64, 64 ), Pull ( -3.848, 63.797 ) ( 56.303, 63.594 ) ]
            |> filled (rgba 0 0 0 0.0001)
            |> move ( 60, 0 )
            |> scaleX 15
        , curve ( -52.25, -63.18 ) [ Pull ( -39.08, -63.18 ) ( -25.92, -63.18 ), Pull ( -23.29, -19.03 ) ( -20.65, 25.113 ), Pull ( -36.05, 30.379 ) ( -51.44, 35.645 ), Pull ( -51.84, -13.36 ) ( -52.25, -62.37 ) ]
            |> filled (rgba 0 0 0 0.0001)
            |> scaleX 1.2
        ]

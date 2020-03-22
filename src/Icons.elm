module Icons exposing (..)

import Element as E
import Svg
import Svg.Attributes as SA
import Types exposing (..)


userIcon : E.Element FrontendMsg
userIcon =
    let
        userSvg =
            Svg.svg
                [ SA.viewBox "0 0 24 24"
                , SA.width "24"
                , SA.height "24"
                , SA.strokeWidth "2"
                , SA.stroke "currentColor"
                , SA.fill "none"
                , SA.strokeLinecap "round"
                , SA.strokeLinejoin "round"
                ]
                [ Svg.rect
                    [ SA.x "0"
                    , SA.y "0"
                    , SA.width "24"
                    , SA.height "24"
                    , SA.stroke "none"
                    ]
                    []
                , Svg.circle
                    [ SA.cx "12"
                    , SA.cy "7"
                    , SA.r "4"
                    ]
                    []
                , Svg.path
                    [ SA.d "M5.5 21v-2a4 4 0 0 1 4 -4h5a4 4 0 0 1 4 4v2"
                    ]
                    []
                ]
    in
    userSvg |> E.html


settingsIcon : E.Element FrontendMsg
settingsIcon =
    let
        userSvg =
            Svg.svg
                [ SA.viewBox "0 0 24 24"
                , SA.width "24"
                , SA.height "24"
                , SA.strokeWidth "2"
                , SA.stroke "currentColor"
                , SA.fill "none"
                , SA.strokeLinecap "round"
                , SA.strokeLinejoin "round"
                ]
                [ Svg.rect
                    [ SA.x "0"
                    , SA.y "0"
                    , SA.width "24"
                    , SA.height "24"
                    , SA.stroke "none"
                    ]
                    []
                , Svg.path
                    [ SA.d "M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 0 0 2.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 0 0 1.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 0 0 -1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 0 0 -2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 0 0 -2.573 -1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 0 0 -1.065 -2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 0 0 1.066 -2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"
                    ]
                    []
                , Svg.circle
                    [ SA.cx "12"
                    , SA.cy "12"
                    , SA.r "3"
                    ]
                    []
                ]
    in
    userSvg |> E.html

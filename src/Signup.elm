module Signup exposing (User)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import VirtualDom exposing (..)


type alias User =
    { name : String
    , email : String
    , password : String
    , loggedIn : Bool
    }


initialModel : User
initialModel =
    User "" "" "" False


view : User -> Html msg
view user =
    div []
        [ h1 [ css [ paddingLeft (cm 3) ] ] [ Html.Styled.text "Sign up" ]
        , styledForm []
            [ div []
                [ Html.Styled.text "Name"
                , styledInput [ id "name", type_ "text" ] []
                ]
            , div []
                [ Html.Styled.text "Email"
                , styledInput [ id "email", type_ "email" ] []
                ]
            , div []
                [ Html.Styled.text "Password"
                , styledInput [ id "password", type_ "password" ] []
                ]
            , div []
                [ styledButton [ type_ "submit" ]
                    [ Html.Styled.text "Create my account" ]
                ]
            ]
        ]


styledForm : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
styledForm =
    styled Html.Styled.form
        [ borderRadius (px 5)
        , backgroundColor (hex "#f2f2f2")
        , padding (px 20)
        , Css.width (px 300)
        ]


styledInput : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
styledInput =
    styled Html.Styled.input
        [ display block
        , Css.width (px 260)
        , padding2 (px 12) (px 20)
        , margin2 (px 8) (px 0)
        , border (px 0)
        , borderRadius (px 4)
        ]


styledButton : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
styledButton =
    styled Html.Styled.button
        [ Css.width (px 300)
        , backgroundColor (hex "#397cd5")
        , color (hex "#fff")
        , padding2 (px 14) (px 20)
        , marginTop (px 10)
        , border (px 0)
        , borderRadius (px 4)
        , fontSize (px 16)
        ]


main : VirtualDom.Node msg
main =
    toUnstyled <| view initialModel

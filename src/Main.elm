module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, h2, nav, text, table, thead, tbody, tr, th, td, a, button, span)
import Html.Attributes exposing (class, href, attribute)
import Url exposing (Url)
import Route
import Route exposing (Route(..))
import MockData exposing (Firm, Lawyer, firms, lawyers)


-- MODEL

type alias Model =
    { key : Nav.Key
    , route : Route
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (Route.parse url), Cmd.none )


-- UPDATE

type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.parse url }, Cmd.none )


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Law Firm Admin"
    , body = [ layout model.route ]
    }


layout : Route -> Html Msg
layout route =
    div [ class "container" ]
        [ asideView route
        , div [ attribute "role" "main", class "main" ] [ viewRoute route ]
        ]


asideView : Route -> Html Msg
asideView route =
    let
        link label targetRoute =
            let
                isActive =
                    if route == targetRoute then "active" else ""

                hrefValue =
                    Route.toUrl targetRoute
            in
            a [ href hrefValue ]
                [ div [ class ("nav-link " ++ isActive) ] [ text label ] ]
    in
    Html.node "aside"
        [ class "aside" ]
        [ h1 [] [ text "Admin Panel" ]
        , nav []
            [ link "Firms" Firms
            , link "Lawyers" Lawyers
            , link "Acceptances" Acceptances
            , link "Login" Login
            ]
        ]


viewRoute : Route -> Html Msg
viewRoute route =
    case route of
        Firms ->
            div []
                [ firmHeaderView "Firms" buttonsView
                , firmsTable
                ]

        Lawyers ->
            div []
                [ headerView "Lawyers"
                , table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Name" ]
                            , th [] [ text "Specialty" ]
                            ]
                        ]
                    , tbody [] (List.map lawyerRow lawyers)
                    ]
                ]

        LawyerDetail id ->
            div []
                [ headerView ("Lawyer #" ++ String.fromInt id)
                , table []
                    [ thead []
                        [ tr []
                            [ th [] [ text "Name" ]
                            , th [] [ text "Email" ]
                            , th [] [ text "App User?"]
                            ]
                        ]
                    , tbody []
                        [ tr []
                            [ td [] [ text "John Doe"]
                            , td [] [ text "john.doe@gmail.com" ]
                            , td [] [ text "✔"]
                            ]
                        ]
                    ]
                ]

        Acceptances ->
            div []
                [ headerView "Acceptances"
                , div [ class "card" ]
                    [ span [] [ text "John Doe accepted on 2024-05-01" ] ]
                ]

        Login ->
            div []
                [ headerView "Login"
                , div [] [ text "Login form goes here..." ]
                ]

        NotFound ->
            div []
                [ headerView "404"
                , text "Page not found." ]


lawyerRow : Lawyer -> Html msg
lawyerRow lawyer =
    tr []
        [ td [] [ a [ href ("#/lawyers/" ++ String.fromInt lawyer.id) ] [ text lawyer.name ] ]
        , td [] [ text lawyer.specialty ]
        ]


firmsTable : Html msg
firmsTable =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "Firm Name" ]
                , th [] [ text "Version" ]
                , th [] [ text "Date" ]
                , th [] [ text "Accepted" ]
                ]
            ]
        , tbody [] (List.map firmRow firms)
        ]


firmRow : Firm -> Html msg
firmRow firm =
    tr []
        [ td [] [ text firm.name ]
        , td [] [ text firm.version ]
        , td [] [ text firm.date ]
        , td [ class (if firm.accepted then "text-green" else "text-gray") ]
            [ text (if firm.accepted then "✔" else "✖") ]
        ]


firmHeaderView : String -> Html msg -> Html msg
firmHeaderView titleText buttons =
    div [ class "header" ]
        [ h2 [] [ text titleText ]
        , buttons
        ]


headerView : String -> Html msg
headerView titleText =
    div [ class "header" ]
        [ h2 [] [ text titleText ] ]


buttonsView : Html msg
buttonsView =
    div [ class "buttons" ]
        [ button [] [ text "Edit" ]
        , button [] [ text "Add Firm" ]
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

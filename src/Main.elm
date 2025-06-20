module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, h2, nav, text, table, thead, tbody, tr, th, td, a, button, span)
import Html.Attributes exposing (class, href, attribute)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Route
import Route exposing (Route(..))
import Api exposing (Firm, getFirms, getLawyers)
import Http


-- MODELS

type alias Paginated a =
    { items : List a
    , page : Int
    , pageSize : Int
    , totalPages : Int
    , error : Maybe String
    , loading : Bool
    }

type alias Model =
    { key : Nav.Key
    , route : Route
    , firms : Paginated Api.Firm
    , lawyers : Paginated Api.Lawyer
    , showAddFirmModal : Bool
    , newFirmName : String
    , selectedLawyer : Maybe Api.LawyerDetail
    }


initPaginated : Paginated a
initPaginated =
    { items = []
    , page = 1
    , pageSize = 10
    , totalPages = 1
    , error = Nothing
    , loading = False
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route = Route.parse url

        initialCmd =
            case route of
                Firms ->
                    getFirms 1 10 GotFirms

                Lawyers ->
                    getLawyers 1 10 GotLawyers

                LawyerDetail id ->
                    Api.getLawyerDetail id GotLawyerDetail

                _ ->
                    Cmd.none
    in
    ( { key = key
      , route = route
      , firms = initPaginated
      , lawyers = initPaginated
      , showAddFirmModal = False
      , newFirmName = ""
      , selectedLawyer = Nothing
      }
    , initialCmd
    )


type Msg
    = GotFirms (Result Http.Error Api.PagedFirms)
    | LoadNextFirmPage
    | LoadPrevFirmPage
    | GotLawyers (Result Http.Error Api.PagedLawyers)
    | LoadNextLawyerPage
    | LoadPrevLawyerPage
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ShowAddFirmModal
    | HideAddFirmModal
    | UpdateNewFirmName String
    | SubmitNewFirm
    | FirmCreated (Result Http.Error Api.Firm)
    | GotLawyerDetail (Result Http.Error Api.LawyerDetail)


-- UPDATE

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
            let
                newRoute = Route.parse url
                newModel = { model | route = newRoute }
                cmd =
                    case newRoute of
                        Firms ->
                            getFirms model.firms.page model.firms.pageSize GotFirms

                        Lawyers ->
                            getLawyers model.lawyers.page model.lawyers.pageSize GotLawyers
                        
                        LawyerDetail id ->
                            Api.getLawyerDetail id GotLawyerDetail

                        _ ->
                            Cmd.none
            in
            ( newModel, cmd )

        GotFirms (Ok { firms, totalPages }) ->
            ( { model
                | firms =
                    { items = firms
                    , page = model.firms.page
                    , pageSize = model.firms.pageSize
                    , totalPages = totalPages
                    , error = Nothing
                    , loading = False
                    }
            }
            , Cmd.none
            )

        GotFirms (Err _) ->
            ( { model
                | firms =
                    { items = model.firms.items
                    , page = model.firms.page
                    , pageSize = model.firms.pageSize
                    , totalPages = model.firms.totalPages
                    , error = Just "Failed to load firms"
                    , loading = False
                    }
            }
            , Cmd.none
            )

        LoadNextFirmPage ->
            let
                nextPage = model.firms.page + 1
            in
            if nextPage > model.firms.totalPages then
                ( model, Cmd.none )
            else
                ( { model
                    | firms =
                        { items = model.firms.items
                        , page = nextPage
                        , pageSize = model.firms.pageSize
                        , totalPages = model.firms.totalPages
                        , error = model.firms.error
                        , loading = True
                        }
                }
                , getFirms nextPage model.firms.pageSize GotFirms
                )

        LoadPrevFirmPage ->
            let
                prevPage = max 1 (model.firms.page - 1)
            in
            ( { model
                | firms =
                    { items = model.firms.items
                    , page = prevPage
                    , pageSize = model.firms.pageSize
                    , totalPages = model.firms.totalPages
                    , error = model.firms.error
                    , loading = True
                    }
            }
            , getFirms prevPage model.firms.pageSize GotFirms
            )

        GotLawyers (Ok { users, totalPages }) ->
            ( { model
                | lawyers =
                    { items = users
                    , page = model.lawyers.page
                    , pageSize = model.lawyers.pageSize
                    , totalPages = totalPages
                    , error = Nothing
                    , loading = False
                    }
            }
            , Cmd.none
            )

        GotLawyers (Err _) ->
            ( { model
                | lawyers =
                    { items = model.lawyers.items
                    , page = model.lawyers.page
                    , pageSize = model.lawyers.pageSize
                    , totalPages = model.lawyers.totalPages
                    , error = Just "Failed to load lawyers"
                    , loading = False
                    }
            }
            , Cmd.none
            )

        LoadNextLawyerPage ->
            let
                nextPage = model.lawyers.page + 1
            in
            if nextPage > model.lawyers.totalPages then
                ( model, Cmd.none )
            else
                ( { model
                    | lawyers =
                        { items = model.lawyers.items
                        , page = nextPage
                        , pageSize = model.lawyers.pageSize
                        , totalPages = model.lawyers.totalPages
                        , error = model.lawyers.error
                        , loading = True
                        }
                }
                , getLawyers nextPage model.lawyers.pageSize GotLawyers
                )

        LoadPrevLawyerPage ->
            let
                prevPage = max 1 (model.lawyers.page - 1)
            in
            ( { model
                | lawyers =
                    { items = model.lawyers.items
                    , page = prevPage
                    , pageSize = model.lawyers.pageSize
                    , totalPages = model.lawyers.totalPages
                    , error = model.lawyers.error
                    , loading = True
                    }
            }
            , getLawyers prevPage model.lawyers.pageSize GotLawyers
            )

        ShowAddFirmModal ->
            ( { model | showAddFirmModal = True }, Cmd.none )

        HideAddFirmModal ->
            ( { model | showAddFirmModal = False, newFirmName = "" }, Cmd.none )

        UpdateNewFirmName name ->
            ( { model | newFirmName = name }, Cmd.none )

        SubmitNewFirm ->
            ( model, Api.postFirm model.newFirmName FirmCreated )

        FirmCreated (Ok _) ->
            ( { model | showAddFirmModal = False, newFirmName = "" }
            , getFirms model.firms.page model.firms.pageSize GotFirms
            )

        FirmCreated (Err _) ->
            ( { model | showAddFirmModal = False, newFirmName = "" }, Cmd.none )

        GotLawyerDetail (Ok lawyer) ->
            ( { model | selectedLawyer = Just lawyer }, Cmd.none )

        GotLawyerDetail (Err _) ->
            ( { model | selectedLawyer = Nothing }, Cmd.none )


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "Law Firm Admin"
    , body = [ layout model ]
    }

layout : Model -> Html Msg
layout model =
    div [ class "container" ]
        [ asideView model.route
        , div [ attribute "role" "main", class "main" ] [ viewRoute model ]
        , modalView model
        ]


modalView : Model -> Html Msg
modalView model =
    if model.showAddFirmModal then
        div [ class "modal-overlay" ]
            [ div [ class "modal" ]
                [ h2 [] [ text "Add Law Firm" ]
                , Html.input
                    [ Html.Attributes.placeholder "Firm Name"
                    , Html.Attributes.value model.newFirmName
                    , Html.Events.onInput UpdateNewFirmName
                    ]
                    []
                , div [ class "modal-buttons" ]
                    [ button [ onClick SubmitNewFirm ] [ text "Submit" ]
                    , button [ onClick HideAddFirmModal ] [ text "Cancel" ]
                    ]
                ]
            ]
    else
        text ""


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


viewRoute : Model -> Html Msg
viewRoute model =
    case model.route of
        Firms ->
            div []
                [ firmHeaderView "Firms" buttonsView
                , firmsTable model.firms
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
                    , tbody [] (List.map lawyerRow model.lawyers.items)
                    ]
                , div [ class "pagination" ]
                    [ button
                        [ onClick LoadPrevLawyerPage
                        , Html.Attributes.disabled (model.lawyers.page <= 1)
                        ]
                        [ text "Previous" ]
                    , span [] [ text ("Page " ++ String.fromInt model.lawyers.page ++ " of " ++ String.fromInt model.lawyers.totalPages) ]
                    , button
                        [ onClick LoadNextLawyerPage
                        , Html.Attributes.disabled (model.lawyers.page >= model.lawyers.totalPages)
                        ]
                        [ text "Next" ]
                    ]
                ]

        LawyerDetail _ ->
            case model.selectedLawyer of
                Just lawyer ->
                    div []
                        [ headerView ("Lawyer: " ++ lawyer.name)
                        , table []
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Name" ]
                                    , th [] [ text "Email" ]
                                    , th [] [ text "App User?" ]
                                    ]
                                ]
                            , tbody []
                                [ tr []
                                    [ td [] [ text lawyer.name ]
                                    , td [] [ text lawyer.email ]
                                    , td [] [ text (if lawyer.isAppUser then "✔" else "✘") ]
                                    ]
                                ]
                            ]
                        ]

                Nothing ->
                    div [] [ text "Loading lawyer details..." ]

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


lawyerRow : Api.Lawyer -> Html msg
lawyerRow lawyer =
    tr []
        [ td [] [ a [ href ("#/lawyers/" ++ lawyer.id) ] [ text lawyer.name ] ]
        , td [] [ text lawyer.email ]
        ]


firmsTable : Paginated Firm -> Html Msg
firmsTable paginated =
    div []
        [ table [ class "table" ]
            [ thead []
                [ tr [][ th [] [ text "Firm Name" ], th [] [ text "Created" ] ] ]
            , tbody [] (List.map firmRow paginated.items)
            ]
        , div [ class "pagination" ]
            [ button
                [ onClick LoadPrevFirmPage
                , Html.Attributes.disabled (paginated.page <= 1)
                ]
                [ text "Previous" ]
            , span [] [ text ("Page " ++ String.fromInt paginated.page ++ " of " ++ String.fromInt paginated.totalPages) ]
            , button
                [ onClick LoadNextFirmPage
                , Html.Attributes.disabled (paginated.page >= paginated.totalPages)
                ]
                [ text "Next" ]
            ]
        ]


firmRow : Firm -> Html msg
firmRow firm =
    tr []
        [ td [] [ text firm.name ]
        , td [] [ text firm.createdAt ]
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


buttonsView : Html Msg
buttonsView =
    div [ class "buttons" ]
        [ button [ onClick ShowAddFirmModal ] [ text "Add Firm" ] ]


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

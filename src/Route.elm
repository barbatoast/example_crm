module Route exposing (Route(..), parse, toUrl)

import Url exposing (Url)
import Url.Parser exposing (Parser, (</>), map, oneOf, s)
import Url.Parser as Parser


-- ROUTE TYPE

type Route
    = Firms
    | Lawyers
    | LawyerDetail String
    | Acceptances
    | Login
    | NotFound


-- ROUTE PARSER

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map LawyerDetail (s "lawyers" </> Parser.string)
        , map Lawyers (s "lawyers")
        , map Firms (s "firms")
        , map Firms (s "home")
        , map Acceptances (s "acceptances")
        , map Login (s "login")
        ]


-- PARSE FUNCTION
parse : Url -> Route
parse url =
    case url.fragment of
        Just "" -> Firms  -- default route
        Just frag ->
            frag
                |> String.dropLeft 1
                |> (\path -> Url.fromString ("http://dummy/" ++ path))
                |> Maybe.andThen (Parser.parse routeParser)
                |> Maybe.withDefault NotFound

        Nothing ->
            Firms


-- TO URL

toUrl : Route -> String
toUrl route =
    case route of
        Firms -> "#/home"
        Lawyers -> "#/lawyers"
        LawyerDetail id -> "#/lawyers/" ++ id
        Acceptances -> "#/acceptances"
        Login -> "#/login"
        NotFound -> "#/404"

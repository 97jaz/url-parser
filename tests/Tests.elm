module Tests exposing (..)

import UrlParser exposing (..)
import Navigation exposing (Location)
import Test exposing (..)
import Expect



-- TESTS


all : Test
all =
    describe "UrlParser"
        [ describe "Basic Parsing" testParsing
        ]


testParsing =
    [ parserTest "Home" "" HomeRoute
    , parserTest "About" "about" AboutRoute
    , parserTest "Token" "token/abc" (TokenRoute "abc")
    , parserTest "Users" "users" (UsersRoutes UsersRoute)
    , parserTest "User" "users/2" (UsersRoutes (UserRoute 2))
    , parserTest "Edit" "users/2/edit" (UsersRoutes (UserEditRoute 2))
    , parserTestWithQuery "Search" "search" "?page=1&options=a&options=b&ids=10&ids=20&state=NY&state=MA"
        (SearchRoute (Just 1) (Just "MA") [10, 20] ["a", "b"])
    ]


parserTestWithQuery name path query expectedRoute =
    describe name
        [ test (name ++ " in path") <|
            \() ->
                Expect.equal
                    (Just expectedRoute)
                    (parsePath routeParser { newLocation | pathname = "/" ++ path, search = query })
        , test (name ++ " in hash") <|
            \() ->
                Expect.equal
                    (Just expectedRoute)
                    (parseHash routeParser { newLocation | hash = "#/" ++ path, search = query })
        , test (name ++ "in hash without leading slash") <|
            \() ->
                Expect.equal
                    (Just expectedRoute)
                    (parseHash routeParser { newLocation | hash = "#" ++ path, search = query })
        ]

parserTest name path expectedRoute =
  parserTestWithQuery name path "" expectedRoute


-- ROUTES


type alias UserId =
    Int


type UserRoute
    = UsersRoute
    | UserRoute UserId
    | UserEditRoute UserId


type MainRoute
    = HomeRoute
    | AboutRoute
    | TokenRoute String
    | UsersRoutes UserRoute
    | NotFoundRoute
    | SearchRoute (Maybe Int) (Maybe String) (List Int) (List String)


-- PARSERS


routeParser =
    oneOf mainMatchers


usersMatchers =
    [ map UserEditRoute (int </> s "edit")
    , map UserRoute (int)
    , map UsersRoute top
    ]


mainMatchers =
    [ map HomeRoute top
    , map AboutRoute (s "about")
    , map TokenRoute (s "token" </> string)
    , map UsersRoutes (s "users" </> (oneOf usersMatchers))
    , map SearchRoute
        (s "search"
           <?> (intParam "page")
           <?> (stringParam "state")
           <?> (intParamValues "ids")
           <?> (stringParamValues "options"))
    ]



-- DUMMY LOCATION


newLocation : Location
newLocation =
    { hash = ""
    , host = "example.com"
    , hostname = "example.com"
    , href = ""
    , origin = ""
    , password = ""
    , pathname = ""
    , port_ = ""
    , protocol = "http"
    , search = ""
    , username = ""
    }

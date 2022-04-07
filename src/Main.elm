module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser
import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (cols, id, placeholder, rows)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, errorToString, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Task


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- Model definition


type alias Model =
    { text : String
    , error : String
    , user : User
    , users : List User
    }



-- App Messages


type Msg
    = NoOp
    | DecodeTextarea
    | TextAreaUpdate String
    | GetUser
    | GotUser (Result Http.Error User)
    | GetUsers
    | GotUsers (Result Http.Error (List User))



-- initialise the app


initUser : User
initUser =
    { id = 0
    , email = Nothing
    , name = ""
    , percentExcited = 0.1
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { text = userExample
      , error = ""
      , user = initUser
      , users = [ initUser ]
      }
    , [ Browser.Dom.focus "myarea" |> Task.attempt (\_ -> NoOp)
      , getUser
      ]
        |> Cmd.batch
    )



-- JSON encoders and decoders


type alias User =
    { id : Int
    , email : Maybe String
    , name : String
    , percentExcited : Float
    }


decodeUsers : Decoder (List User)
decodeUsers =
    Decode.list decodeUser


decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> required "id" int
        -- `null` decodes to `Nothing`
        |> required "email" (nullable string)
        |> optional "name" string "name attribute is `null` or not present"
        |> hardcoded 1.0


userExample : String
userExample =
    """{"id": 123, "email": "sam@example.com", "name": "Sam Sample"}"""



-- Update functions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DecodeTextarea ->
            ( userDecodeAction model model.text, Cmd.none )

        TextAreaUpdate input ->
            ( { model | text = input }, Cmd.none )

        GetUser ->
            ( model, getUser )

        GotUser result ->
            case result of
                Ok user ->
                    ( { model | user = user }, Cmd.none )

                Err err ->
                    ( { model | error = httpErrorToString err }, Cmd.none )

        GetUsers ->
            ( model, getUsers )

        GotUsers result ->
            case result of
                Ok users ->
                    ( { model | users = users }, Cmd.none )

                Err err ->
                    ( { model | error = httpErrorToString err }, Cmd.none )


userDecodeAction : Model -> String -> Model
userDecodeAction model userJSON =
    case decodeString decodeUser userJSON of
        Ok data ->
            { model | user = data, error = "" }

        Err err ->
            { model | error = errorToString err }



-- HTTP


getUser : Cmd Msg
getUser =
    Http.get
        { url = "/public/user.json"
        , expect = Http.expectJson GotUser decodeUser
        }


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = "/public/users.json"
        , expect = Http.expectJson GotUsers decodeUsers
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage



-- View functions


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row [ Row.centerXs ]
            [ Grid.col [ Col.md12 ]
                [ div []
                    [ textarea [ rows 10, cols 75, onInput TextAreaUpdate, placeholder userExample, id "myarea" ] [ text userExample ]
                    , div [] [ text ("Trying to decode `" ++ model.text ++ "`") ]
                    , button [ onClick DecodeTextarea ] [ text "Decode" ]
                    ]
                ]
            , Grid.col [ Col.md12 ] [ button [ onClick GetUser ] [ text "Get Single User" ] ]
            , Grid.col [ Col.md12 ] [ button [ onClick GetUsers ] [ text "Get Users" ] ]
            , Grid.col [ Col.md12 ] [ viewUser model.user ]
            , Grid.col [ Col.md12 ] [ div [] (List.map viewUser model.users) ]
            , Grid.col [ Col.md12 ] [ viewError model.error ]
            ]
        ]


viewUser : User -> Html Msg
viewUser user =
    div []
        [ Html.pre [] [ Html.text ("User ID: " ++ String.fromInt user.id) ]
        , Html.pre [] [ Html.text ("User Name: " ++ user.name) ]
        , Html.pre [] [ Html.text ("User Email: " ++ Maybe.withDefault "/" user.email) ]
        ]


viewError : String -> Html Msg
viewError error =
    if error /= "" then
        div [] [ Html.pre [] [ Html.text ("Decoding Error: " ++ error) ] ]

    else
        div [] []



-- Subscriptions


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Browser
import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, errorToString, int, list, nullable, string)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
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
    , getUser
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
        |> Pipeline.required "id" int
        -- `null` decodes to `Nothing`
        |> Pipeline.required "email" (nullable string)
        |> Pipeline.optional "name" string "name attribute is `null` or not present"
        |> Pipeline.hardcoded 1.0


encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "id", Encode.int <| user.id )
        , ( "email", Encode.string <| Maybe.withDefault "/" user.email )
        , ( "name", Encode.string <| user.name )
        , ( "percentExcited", Encode.float <| user.percentExcited )
        ]


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
                    let
                        text : String
                        text =
                            user
                                |> encodeUser
                                |> Encode.encode 0
                    in
                    ( { model | user = user, text = text }, Cmd.none )

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
                [ Card.config []
                    |> Card.block []
                        [ Block.titleH4 [] [ text "User JSON Data" ]
                        , Block.text [] [ textarea [ rows 5, cols 75, onInput TextAreaUpdate, placeholder userExample, value model.text ] [] ]
                        , Block.text [] [ text ("Trying to decode `" ++ model.text ++ "`") ]
                        , Block.link [ class "btn btn-primary", onClick DecodeTextarea ] [ text "Decode Input" ]
                        , Block.link [ class "btn btn-primary", onClick GetUser ] [ text "Get Single User" ]
                        , Block.link [ class "btn btn-primary", onClick GetUsers ] [ text "Get Users" ]
                        ]
                    |> Card.view
                , Card.config []
                    |> Card.block []
                        [ Block.titleH4 [] [ text "Decoded User" ]
                        , Block.text [] [ viewUser model.user ]
                        ]
                    |> Card.view
                , Card.config []
                    |> Card.block []
                        [ Block.titleH4 [] [ text "Decoded User List" ]
                        , Block.text [] (List.map viewUser model.users)
                        ]
                    |> Card.view
                ]
            , Grid.col [ Col.md12 ] [ viewError model.error ]
            ]
        ]


viewUser : User -> Html Msg
viewUser user =
    div []
        [ Html.pre [] [ Html.text ("User ID: " ++ String.fromInt user.id) ]
        , Html.pre [] [ Html.text ("User Name: " ++ user.name) ]
        , Html.pre [] [ Html.text ("User Email: " ++ Maybe.withDefault "/" user.email) ]
        , Html.pre [] [ Html.text ("Excited: " ++ String.fromFloat user.percentExcited) ]
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

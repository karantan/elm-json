module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, textarea)
import Html.Attributes exposing (cols, placeholder, rows)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, decodeString, errorToString, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }



-- Model definition


type alias Model =
    { text : String
    , error : String
    , user : User
    }



-- App Messages


type Msg
    = ButtonPressed
    | TextAreaUpdate String



-- initialise the app


init : () -> ( Model, Cmd Msg )
init _ =
    ( { text = userExample
      , error = ""
      , user =
            { id = 0
            , email = Nothing
            , name = ""
            , percentExcited = 0.1
            }
      }
    , Cmd.none
    )



-- JSON encoders and decoders


type alias User =
    { id : Int
    , email : Maybe String
    , name : String
    , percentExcited : Float
    }


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
        ButtonPressed ->
            ( userDecodeAction model model.text, Cmd.none )

        TextAreaUpdate input ->
            ( { model | text = input }, Cmd.none )


userDecodeAction : Model -> String -> Model
userDecodeAction model userJSON =
    case decodeString decodeUser userJSON of
        Ok data ->
            { model | user = data, error = "" }

        Err err ->
            { model | error = errorToString err }



-- View functions


view : Model -> Html Msg
view model =
    div []
        [ textarea [ rows 10, cols 75, onInput TextAreaUpdate, placeholder userExample ] [ text userExample ]
        , div [] [ text ("Trying to decode `" ++ model.text ++ "`") ]
        , button [ onClick ButtonPressed ] [ text "Decode" ]
        , viewUser model
        , viewError model
        ]


viewUser : Model -> Html Msg
viewUser model =
    div []
        [ Html.pre [] [ Html.text ("User ID: " ++ String.fromInt model.user.id) ]
        , Html.pre [] [ Html.text ("User Name: " ++ model.user.name) ]
        , Html.pre [] [ Html.text ("User Email: " ++ Maybe.withDefault "/" model.user.email) ]
        ]


viewError : Model -> Html Msg
viewError model =
    if model.error /= "" then
        div [] [ Html.pre [] [ Html.text ("Decoding Error: " ++ model.error) ] ]

    else
        div [] []



-- Subscriptions


subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

module Main exposing (Model(..), Msg(..), getRandomCatGif, gifDecoder, init, main, subscriptions, update, view, viewGif)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, index, string)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRandomCatGif 0 )



-- UPDATE


type Msg
    = MorePlease
    | GotGif (Result Http.Error String)
    | RegetImage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( Loading, Random.generate RegetImage (Random.int 0 15) )

        RegetImage val ->
            ( Loading, getRandomCatGif val )

        GotGif result ->
            case result of
                Ok url ->
                    ( Success url, Cmd.none )

                Err err ->
                    case err of
                        BadUrl str ->
                            ( Success <| "BadUrl:" ++ str, Cmd.none )

                        Timeout ->
                            ( Success "Timeout", Cmd.none )

                        NetworkError ->
                            ( Success "NetworkError", Cmd.none )

                        BadStatus status ->
                            ( Success <| "BadStatus:" ++ String.fromInt status, Cmd.none )

                        BadBody str ->
                            ( Success <| "BadBody:" ++ str, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Random Cats" ]
        , viewGif model
        ]


viewGif : Model -> Html Msg
viewGif model =
    case model of
        Failure ->
            div []
                [ text "I could not load a random cat for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        Loading ->
            text "Loading..."

        Success url ->
            div []
                [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                , img [ src url ] []
                , div [] [ text url ]
                ]



-- HTTP


getRandomCatGif : Int -> Cmd Msg
getRandomCatGif val =
    Http.get
        { url = "https://qiita.com/api/v2/items/59aa14fd53a89f531bdc/comments"
        , expect = Http.expectJson GotGif <| gifDecoder val
        }


gifDecoder : Int -> Decoder String
gifDecoder val =
    index val <| field "user" <| field "profile_image_url" string

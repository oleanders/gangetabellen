module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, p, span, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onInput, onSubmit)
import Random



---- MODEL ----


type alias Model =
    { tall : Int
    , riktigSvar : Int
    , input : String
    , resultat : Resultat
    , retning : Retning
    }


type Resultat
    = IkkeBesvart
    | RiktigSvar
    | FeilSvar
    | UgyldigSvar


type Retning
    = Venstre
    | Hoyre


init : ( Model, Cmd Msg )
init =
    ( Model 2 0 "" IkkeBesvart Venstre
    , Cmd.batch
        [ Random.generate NyttTall (Random.int 0 10)
        , Random.generate NyRetning (Random.uniform Venstre [ Hoyre ])
        ]
    )



---- UPDATE ----


type Msg
    = LagNyttTall
    | NyttTall Int
    | NyRetning Retning
    | OppdaterSvar String
    | SjekkSvar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LagNyttTall ->
            ( model, Random.generate NyttTall (Random.int 0 10) )

        NyttTall tall ->
            ( { model | tall = tall }, Cmd.none )

        NyRetning retning ->
            ( { model | retning = retning }, Cmd.none )

        OppdaterSvar input ->
            ( { model | input = input, resultat = IkkeBesvart }, Cmd.none )

        SjekkSvar ->
            let
                resultat =
                    if String.trim model.input == "" then
                        IkkeBesvart

                    else
                        case String.toInt <| String.trim model.input of
                            Nothing ->
                                UgyldigSvar

                            Just svar ->
                                if 6 * model.tall == svar then
                                    RiktigSvar

                                else
                                    FeilSvar
            in
            ( { model | resultat = resultat }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Seksgangen" ]
        , Html.form [ onSubmit SjekkSvar ]
            [ span [] [ text <| "6 x " ++ String.fromInt model.tall ++ " = " ]
            , Html.input [ onInput OppdaterSvar ] []
            ]
        , p []
            [ viewResultat model.resultat
            ]
        ]


viewResultat : Resultat -> Html Msg
viewResultat resultat =
    case resultat of
        IkkeBesvart ->
            text "Skriv inn et svar, og trykk Enter"

        FeilSvar ->
            text "Feil svar :("

        RiktigSvar ->
            text "Riktig svar!! :D"

        UgyldigSvar ->
            text "Ugyldig svar... du må kun bruke tall :-S"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

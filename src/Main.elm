module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, h1, p, span, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick, onInput, onSubmit)
import Random
import Task



---- MODEL ----


type alias Model =
    { gangetabell : Int
    , tall : Int
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
    ( { gangetabell = 6
      , tall = 0
      , input = ""
      , resultat = IkkeBesvart
      , retning = Venstre
      }
    , Cmd.batch
        [ Random.generate NyttTall (Random.int 0 10)
        , Random.generate NyRetning (Random.uniform Venstre [ Hoyre ])
        ]
    )



---- UPDATE ----


type Msg
    = NoOp
    | VelgNyGangetabell Int
    | LagNyttTall
    | NyttTall Int
    | NyRetning Retning
    | OppdaterSvar String
    | SjekkSvar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        VelgNyGangetabell nr ->
            let
                ( m, c ) =
                    update LagNyttTall model
            in
            ( { model | gangetabell = nr }, c )

        LagNyttTall ->
            ( { model | resultat = IkkeBesvart, input = "" }
            , Cmd.batch
                [ Random.generate NyttTall (Random.int 0 10)
                , Random.generate NyRetning (Random.uniform Venstre [ Hoyre ])
                ]
            )

        NyttTall tall ->
            ( { model | tall = tall, input = "" }, Cmd.none )

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
                                if model.gangetabell * model.tall == svar then
                                    RiktigSvar

                                else
                                    FeilSvar
            in
            ( { model | resultat = resultat }
            , Task.attempt (\_ -> NoOp) (Dom.focus "svar")
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        ( x, y ) =
            case model.retning of
                Venstre ->
                    ( String.fromInt model.gangetabell, String.fromInt model.tall )

                Hoyre ->
                    ( String.fromInt model.tall, String.fromInt model.gangetabell )
    in
    div []
        [ p []
            [ Html.a [ onClick <| VelgNyGangetabell 1 ] [ text <| gantetabelltekst 1, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 2 ] [ text <| gantetabelltekst 2, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 3 ] [ text <| gantetabelltekst 3, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 4 ] [ text <| gantetabelltekst 4, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 5 ] [ text <| gantetabelltekst 5, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 6 ] [ text <| gantetabelltekst 6, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 7 ] [ text <| gantetabelltekst 7, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 8 ] [ text <| gantetabelltekst 8, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 9 ] [ text <| gantetabelltekst 9, text " | " ]
            , Html.a [ onClick <| VelgNyGangetabell 10 ] [ text <| gantetabelltekst 10 ]
            ]
        , h1 [] [ text <| gantetabelltekst model.gangetabell ]
        , Html.form [ onSubmit SjekkSvar ]
            [ span [] [ text <| x ++ " x " ++ y ++ " = " ]
            , Html.input [ onInput OppdaterSvar, Html.Attributes.value model.input, id "svar" ] []
            , Html.input
                [ Html.Attributes.type_ "submit"
                , Html.Attributes.value "Sjekk svar"
                ]
                []
            ]
        , p []
            [ viewResultat model.resultat
            ]
        , p []
            [ Html.input
                [ Html.Attributes.value "Ny oppgave"
                , Html.Events.onClick LagNyttTall
                , Html.Attributes.type_ "button"
                ]
                []
            ]
        ]


gantetabelltekst : Int -> String
gantetabelltekst tall =
    case tall of
        1 ->
            "En-gangen"

        2 ->
            "To-gangen"

        3 ->
            "Tre-gangen"

        4 ->
            "Fire-gangen"

        5 ->
            "Fem-gangen"

        6 ->
            "Seks-gangen"

        7 ->
            "Syv-gangen"

        8 ->
            "Åtte-gangen"

        9 ->
            "Ni-gangen"

        10 ->
            "Ti-gangen"

        _ ->
            "Ugyldig verdi"


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

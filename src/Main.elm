module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, div, h1, p, span, text)
import Html.Attributes exposing (href, id)
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
    | Hurtigsvar String
    | SjekkSvar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        VelgNyGangetabell nr ->
            let
                ( _, cmd ) =
                    update LagNyttTall model
            in
            ( { model | gangetabell = nr }, cmd )

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

        Hurtigsvar input ->
            let
                ( m, cmd ) =
                    update SjekkSvar { model | input = input }
            in
            ( m, cmd )



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

        viewMenyKnapp nr =
            Html.a
                [ href <| "#" ++ String.fromInt nr
                , onClick <| VelgNyGangetabell nr
                ]
                [ text <| gantetabelltekst nr ]

        viewVelgEtTallKnapp t nr =
            let
                tall =
                    (t * 10) + nr
            in
            Html.input
                [ Html.Attributes.type_ "button"
                , Html.Attributes.value <| String.fromInt tall
                , onClick <| Hurtigsvar <| String.fromInt tall
                , Html.Attributes.style "width" "40px"
                ]
                []
    in
    div []
        [ p []
            (List.range 1 10
                |> List.map viewMenyKnapp
                |> List.intersperse (text " | ")
            )
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
        , p []
            (List.range 1 10
                |> List.repeat 10
                |> List.indexedMap (\i d -> List.map (viewVelgEtTallKnapp i) d)
                |> List.map (\d -> p [] d)
            )
        ]


gantetabelltekst : Int -> String
gantetabelltekst tall =
    String.fromInt tall ++ "-gangen"


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

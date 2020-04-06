module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type Ability
    = None
    | HealingOne
    | HealingTwo


type alias PlayerCard =
    { name : String
    , strength : Int
    , ability : Ability
    }


type alias EnemyCard =
    { name : String
    , strength : Int
    , draws : Int
    }


type alias Model =
    { playedCards : List PlayerCard
    , playerDeck : List PlayerCard
    , health : Int
    , currentEnemy : EnemyCard
    , enemyDeck : List EnemyCard
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        playerDeck : List PlayerCard
        playerDeck =
            [ { name = "Ralph", strength = 2, ability = None }
            , { name = "Donkey", strength = -1, ability = None }
            , { name = "Simba", strength = 1, ability = None }
            , { name = "Xiao Curly", strength = 0, ability = HealingOne }
            , { name = "Cocoa", strength = 0, ability = None }
            , { name = "Da YangYang", strength = 0, ability = HealingTwo }
            , { name = "Ernest", strength = 1, ability = None }
            , { name = "Sophie", strength = 0, ability = None }
            , { name = "Tutu", strength = 1, ability = None }
            , { name = "Panda-kun", strength = 0, ability = None }
            ]

        firstEnemy : EnemyCard
        firstEnemy =
            { name = "Thug", strength = 1, draws = 1 }

        enemyDeck : List EnemyCard
        enemyDeck =
            [ { name = "Thug", strength = 1, draws = 1 }
            , { name = "Thug", strength = 1, draws = 1 }
            , { name = "Big Thug", strength = 3, draws = 2 }
            , { name = "Big Thug", strength = 3, draws = 2 }
            , { name = "Big Thug", strength = 3, draws = 2 }
            ]

        initModel : Model
        initModel =
            { playedCards = []
            , playerDeck = playerDeck
            , health = 20
            , currentEnemy = firstEnemy
            , enemyDeck = enemyDeck
            }
    in
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = EndBattle
    | DrawCard
    | ActivateCard PlayerCard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        EndBattle ->
            Debug.todo "Code EndBattle"

        DrawCard ->
            Debug.todo "Code DrawCard"

        ActivateCard card ->
            Debug.todo "Code ActivateCard"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "my game" ]
        ]

module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, hr, li, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Random.List


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
    { seed : Random.Seed
    , playedCards : List PlayerCard
    , playerDeck : List PlayerCard
    , playerDiscard : List PlayerCard
    , health : Int
    , currentEnemy : EnemyCard
    , enemyDeck : List EnemyCard
    , enemyDiscard : List EnemyCard
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed : Random.Seed
        initialSeed =
            Random.initialSeed 0

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
            [ { name = "Thug", strength = 4, draws = 1 }
            , { name = "Thug", strength = 1, draws = 1 }
            , { name = "Big Thug", strength = 3, draws = 2 }
            , { name = "Big Thug", strength = 3, draws = 2 }
            , { name = "Big Thug", strength = 3, draws = 2 }
            ]

        initModel : Model
        initModel =
            { seed = initialSeed
            , playedCards = []
            , playerDeck = playerDeck
            , playerDiscard = []
            , health = 20
            , currentEnemy = firstEnemy
            , enemyDeck = enemyDeck
            , enemyDiscard = []
            }
    in
    ( initModel, Cmd.none )



-- UPDATE


type Msg
    = EndBattle
    | DrawCard
    | ActivateCard PlayerCard


shuffleNonEmptyList : a -> List a -> Random.Generator ( a, List a )
shuffleNonEmptyList top rest =
    Random.map
        (\shuffled ->
            case shuffled of
                [] ->
                    ( top, rest )

                shuffledTop :: shuffledRest ->
                    ( shuffledTop, shuffledRest )
        )
        (Random.List.shuffle (top :: rest))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndBattle ->
            let
                enemyDeck =
                    model.enemyDeck

                enemyDiscard =
                    model.currentEnemy :: model.enemyDiscard

                modelUpdatedDecks : Model
                modelUpdatedDecks =
                    case ( enemyDeck, enemyDiscard ) of
                        ( [], [] ) ->
                            model

                        ( topCard :: rest, _ ) ->
                            { model | currentEnemy = topCard, enemyDeck = rest, enemyDiscard = model.currentEnemy :: model.enemyDiscard }

                        ( [], topCard :: rest ) ->
                            let
                                ( ( drawnCard, newDeck ), newSeed ) =
                                    Random.step (shuffleNonEmptyList topCard rest) model.seed
                            in
                            { model | seed = newSeed, currentEnemy = drawnCard, enemyDeck = newDeck, enemyDiscard = [] }

                newModel : Model
                newModel =
                    { modelUpdatedDecks
                        | playedCards = []
                        , playerDiscard = List.append model.playerDiscard model.playedCards
                        , health = model.health - model.currentEnemy.strength + List.sum (List.map .strength model.playedCards)
                    }
            in
            ( newModel, Cmd.none )

        DrawCard ->
            let
                { playerDeck, playerDiscard } =
                    model

                newModel : Model
                newModel =
                    case ( playerDeck, playerDiscard ) of
                        ( [], [] ) ->
                            model

                        ( topCard :: rest, _ ) ->
                            { model | playedCards = topCard :: model.playedCards, playerDeck = rest }

                        ( [], topCard :: rest ) ->
                            let
                                ( ( drawnCard, newDeck ), newSeed ) =
                                    Random.step (shuffleNonEmptyList topCard rest) model.seed
                            in
                            { model | seed = newSeed, playedCards = drawnCard :: model.playedCards, playerDeck = newDeck, playerDiscard = [] }
            in
            ( newModel, Cmd.none )

        ActivateCard card ->
            Debug.todo "Code ActivateCard"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


renderEnemyCard : EnemyCard -> Html Msg
renderEnemyCard card =
    li [] [ text card.name ]


renderEnemyContainer : Model -> Html Msg
renderEnemyContainer model =
    div []
        [ div []
            [ h2 [] [ text model.currentEnemy.name ]
            , div [] [ text ("Strength: " ++ String.fromInt model.currentEnemy.strength) ]
            , div [] [ text ("Draws: " ++ String.fromInt model.currentEnemy.draws) ]
            ]
        , h2 [] [ text "Enemy Deck" ]
        , ul [] (List.map renderEnemyCard model.enemyDeck)
        , h2 [] [ text "Enemy Discard" ]
        , ul [] (List.map renderEnemyCard model.enemyDiscard)
        ]


renderPlayerCard : PlayerCard -> Html Msg
renderPlayerCard playerCard =
    li [] [ text (playerCard.name ++ " (" ++ String.fromInt playerCard.strength ++ ")") ]


renderPlayerContainer : Model -> Html Msg
renderPlayerContainer model =
    div []
        [ h2 [] [ text ("Player Health: " ++ String.fromInt model.health) ]
        , h2 [] [ text ("Played Cards (total: " ++ String.fromInt (List.sum (List.map .strength model.playedCards)) ++ ")") ]
        , button [ onClick EndBattle ] [ text "End Battle" ]
        , button [ onClick DrawCard ] [ text "Summon Hero!" ]
        , ul [] (List.map renderPlayerCard model.playedCards)
        , h2 [] [ text "Player Deck" ]
        , ul [] (List.map renderPlayerCard model.playerDeck)
        , h2 [] [ text "Player Discard" ]
        , ul [] (List.map renderPlayerCard model.playerDiscard)
        ]


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ div [ class "page-title" ] [ h1 [] [ text "Maplereach" ] ]
        , hr [] []
        , div [ class "enemy-container" ] [ renderEnemyContainer model ]
        , hr [] []
        , div [ class "player-container" ] [ renderPlayerContainer model ]
        ]

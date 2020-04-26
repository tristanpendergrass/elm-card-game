module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, hr, img, li, span, text, ul)
import Html.Attributes exposing (class, src)
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
            { name = "Cigarette Man", strength = 1, draws = 1 }

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
                updateEnemyCards : Model -> Model
                updateEnemyCards oldModel =
                    let
                        enemyDeck =
                            oldModel.enemyDeck

                        enemyDiscard =
                            oldModel.currentEnemy :: oldModel.enemyDiscard
                    in
                    case ( enemyDeck, enemyDiscard ) of
                        ( [], [] ) ->
                            oldModel

                        ( topCard :: rest, _ ) ->
                            { oldModel | currentEnemy = topCard, enemyDeck = rest, enemyDiscard = enemyDiscard }

                        ( [], topCard :: rest ) ->
                            let
                                ( ( drawnCard, newDeck ), newSeed ) =
                                    Random.step (shuffleNonEmptyList topCard rest) oldModel.seed
                            in
                            { oldModel | seed = newSeed, currentEnemy = drawnCard, enemyDeck = newDeck, enemyDiscard = [] }

                updatePlayerCards : Model -> Model
                updatePlayerCards oldModel =
                    { oldModel
                        | playedCards = []
                        , playerDiscard = List.append oldModel.playerDiscard oldModel.playedCards
                    }

                updateHealth : Model -> Model
                updateHealth oldModel =
                    { oldModel | health = model.health - model.currentEnemy.strength + List.sum (List.map .strength model.playedCards) }

                newModel : Model
                newModel =
                    model
                        |> updateEnemyCards
                        |> updatePlayerCards
                        |> updateHealth
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


renderPlayerHealth : Int -> Html Msg
renderPlayerHealth health =
    div [ class "player-health-container tooltip" ]
        [ span [ class "tooltip-text" ] [ text "Lose it all and your guys will get really sad" ]
        , div [ class "player-health" ]
            [ img [ src "./morale_icon.png" ] []
            , div [ class "player-health-text" ]
                [ span [ class "morale" ] [ text "Morale" ]
                , span [ class "health-number" ] [ text (String.fromInt health) ]
                ]
            ]
        ]


renderEnemyContainer : Model -> Html Msg
renderEnemyContainer model =
    let
        renderEnemyDeck : Int -> Html Msg
        renderEnemyDeck count =
            div [ class "deck-container" ]
                [ div [ class "deck-count tooltip" ]
                    [ span [] [ text (String.fromInt count) ]
                    , span [ class "tooltip-text" ] [ text "Cards in enemy deck" ]
                    ]
                , div [ class "deck-image enemy-deck" ] []
                ]

        renderEnemyDiscard : Int -> Html Msg
        renderEnemyDiscard count =
            div [ class "deck-container" ]
                [ div [ class "deck-count tooltip" ]
                    [ span [] [ text (String.fromInt count) ]
                    , span [ class "tooltip-text" ] [ text "Cards in enemy discard pile" ]
                    ]
                , div [ class "deck-image enemy-deck discard" ] []
                ]

        renderEnemyCard : EnemyCard -> Html Msg
        renderEnemyCard enemy =
            div [ class "card enemy-card" ]
                [ div [ class "border" ] []
                , div [ class "card-top" ] [ text enemy.name ]
                , div [ class "card-bottom" ]
                    [ img [ class "card-picture", src "./cigarette_man.png" ] []
                    , div [ class "card-info" ]
                        [ div [] [ text ("Stength: " ++ String.fromInt enemy.strength) ]
                        , div [] [ text ("Draws: " ++ String.fromInt enemy.draws) ]
                        ]
                    ]
                ]
    in
    div [ class "enemy-container" ]
        [ div [ class "info-container" ]
            [ renderEnemyDiscard (List.length model.enemyDiscard)
            , renderEnemyDeck (List.length model.enemyDeck)
            ]
        , div [ class "button-container" ]
            [ button [ onClick EndBattle ] [ text "End Battle" ]
            ]
        , div [ class "cards-container" ]
            [ renderEnemyCard model.currentEnemy
            ]
        ]


renderPlayerContainer : Model -> Html Msg
renderPlayerContainer model =
    let
        renderPlayerDeck : Int -> Html Msg
        renderPlayerDeck count =
            div [ class "deck-container" ]
                [ div [ class "deck-count tooltip" ]
                    [ span [] [ text (String.fromInt count) ]
                    , span [ class "tooltip-text" ] [ text "Cards in enemy deck" ]
                    ]
                , div [ class "deck-image player-deck" ] []
                ]

        renderPlayerDiscard : Int -> Html Msg
        renderPlayerDiscard count =
            div [ class "deck-container" ]
                [ div [ class "deck-count tooltip" ]
                    [ span [] [ text (String.fromInt count) ]
                    , span [ class "tooltip-text" ] [ text "Cards in enemy discard pile" ]
                    ]
                , div [ class "deck-image player-deck discard" ] []
                ]

        renderPlayerCard : PlayerCard -> Html Msg
        renderPlayerCard playerCard =
            div [ class "card player-card" ]
                [ div [ class "border" ] []
                , div [ class "card-top" ] [ text playerCard.name ]
                , div [ class "card-bottom" ]
                    [ img [ class "card-picture", src "./cigarette_man.png" ] []
                    , div [ class "card-info" ]
                        [ div [] [ text ("Stength: " ++ String.fromInt playerCard.strength) ]
                        ]
                    ]
                ]
    in
    div [ class "player-container" ]
        [ div [ class "info-container" ]
            [ renderPlayerDeck (List.length model.playerDeck)
            , renderPlayerDiscard (List.length model.playerDiscard)
            ]
        , div [ class "button-container" ]
            [ button [ onClick DrawCard ] [ text "Draw Card" ]
            ]
        , div [ class "cards-container" ] (List.map renderPlayerCard model.playedCards)
        ]


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ renderEnemyContainer model
        , hr [] []
        , renderPlayerContainer model
        ]

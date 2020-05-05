module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, hr, img, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, class, disabled, src, type_)
import Html.Events exposing (onCheck, onClick)
import Random
import Random.List



-- Utils


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


updatePhaseInModel : Model -> Phase -> Model
updatePhaseInModel model phase =
    { model | phase = phase }


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias ApplyAbility =
    FightModel -> FightModel


type Ability
    = Ability String ApplyAbility


type alias PlayerCard =
    { name : String
    , strength : Int
    , ability : Maybe Ability
    , picture : String
    }


type alias EnemyCard =
    { name : String
    , strength : Int
    , draws : Int
    }


type alias FightModel =
    { seed : Random.Seed
    , playerDeck : List PlayerCard
    , playerDiscard : List PlayerCard
    , health : Int
    , enemyDeck : List EnemyCard
    , enemyDiscard : List EnemyCard
    , currentEnemy : EnemyCard
    , cardsUsed : List PlayerCard
    , playedCards : List PlayerCard
    }


type FightOutcome
    = PlayerWon PlayerCard
    | PlayerLost (List PlayerCard) (List PlayerCard)


type alias RewardsModel =
    { seed : Random.Seed
    , playerDeck : List PlayerCard
    , playerDiscard : List PlayerCard
    , health : Int
    , enemyDeck : List EnemyCard
    , enemyDiscard : List EnemyCard

    -- other stuff
    , fightOutcome : FightOutcome
    }


type Phase
    = FightPhase FightModel
    | RewardsPhase RewardsModel


type alias Model =
    { phase : Phase
    }


healingOne : Ability
healingOne =
    Ability "Healing I" (\model -> { model | health = model.health + 1 })


healingTwo : Ability
healingTwo =
    Ability "Healing II" (\model -> { model | health = model.health + 2 })


defaultReward : PlayerCard
defaultReward =
    { name = "Renee", strength = 4, ability = Nothing, picture = "renee" }


defaultEnemy : EnemyCard
defaultEnemy =
    { name = "Grump", strength = 6, draws = 2 }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialSeed : Random.Seed
        initialSeed =
            Random.initialSeed 0

        playerDeck : List PlayerCard
        playerDeck =
            [ { name = "Cigarette Man", strength = 2, ability = Nothing, picture = "cigarette_man" }
            , { name = "Finger Guns", strength = 0, ability = Nothing, picture = "finger_guns" }
            , { name = "Spellpeep", strength = 1, ability = Just healingOne, picture = "spellpeep" }
            , { name = "The Nose", strength = 0, ability = Just healingTwo, picture = "the_nose" }
            ]

        firstEnemy : EnemyCard
        firstEnemy =
            defaultEnemy

        enemyDeck : List EnemyCard
        enemyDeck =
            [ { name = "Grump", strength = 1, draws = 1 }
            , { name = "Grump", strength = 1, draws = 1 }
            , { name = "Da Grump", strength = 2, draws = 2 }
            , { name = "Da Grump", strength = 2, draws = 2 }
            , { name = "Xiao Grump", strength = 0, draws = 1 }
            ]

        initModel : Model
        initModel =
            { phase =
                FightPhase
                    { seed = initialSeed
                    , playerDeck = playerDeck
                    , playerDiscard = []
                    , health = 20
                    , enemyDeck = enemyDeck
                    , enemyDiscard = []
                    , cardsUsed = []
                    , currentEnemy = firstEnemy
                    , playedCards = []
                    }
            }
    in
    ( initModel, Cmd.none )



-- UPDATE


type
    Msg
    -- Fight phase
    = EndBattle
    | DrawCard
    | ActivateCard PlayerCard
      -- Rewards phase
    | NextBattle
    | ToggleRemoveCard PlayerCard Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.phase of
        FightPhase fightModel ->
            updateFightPhase msg fightModel
                |> Tuple.mapFirst (updatePhaseInModel model)

        RewardsPhase rewardsModel ->
            updateRewardsPhase msg rewardsModel
                |> Tuple.mapFirst (updatePhaseInModel model)


updateFightPhase : Msg -> FightModel -> ( Phase, Cmd Msg )
updateFightPhase msg model =
    case msg of
        EndBattle ->
            let
                updateEnemyCards : FightModel -> FightModel
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

                updatePlayerCards : FightModel -> FightModel
                updatePlayerCards oldModel =
                    { oldModel
                        | playedCards = []
                        , playerDiscard = List.append oldModel.playerDiscard oldModel.playedCards
                    }

                strengthDifference : Int
                strengthDifference =
                    model.currentEnemy.strength
                        - List.sum (List.map .strength model.playedCards)

                updateHealth : FightModel -> FightModel
                updateHealth oldModel =
                    { oldModel | health = model.health - strengthDifference }

                convertToRewardsModel : FightModel -> RewardsModel
                convertToRewardsModel oldModel =
                    let
                        fightOutcome : FightOutcome
                        fightOutcome =
                            if strengthDifference > 0 then
                                PlayerLost model.playedCards []

                            else
                                PlayerWon defaultReward
                    in
                    { seed = oldModel.seed
                    , playerDeck = oldModel.playerDeck
                    , playerDiscard = oldModel.playerDiscard
                    , health = oldModel.health
                    , enemyDeck = oldModel.enemyDeck
                    , enemyDiscard = oldModel.enemyDiscard
                    , fightOutcome = fightOutcome
                    }

                newModel : RewardsModel
                newModel =
                    model
                        |> updateEnemyCards
                        |> updatePlayerCards
                        |> updateHealth
                        |> convertToRewardsModel
            in
            ( RewardsPhase newModel, Cmd.none )

        DrawCard ->
            let
                { playerDeck, playerDiscard } =
                    model

                newModel : FightModel
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
            ( FightPhase newModel, Cmd.none )

        ActivateCard card ->
            let
                updateCardsUsed : FightModel -> FightModel
                updateCardsUsed oldModel =
                    { oldModel | cardsUsed = card :: oldModel.cardsUsed }

                activateAbility : FightModel -> FightModel
                activateAbility oldModel =
                    case card.ability of
                        Nothing ->
                            oldModel

                        Just (Ability _ applyAbility) ->
                            applyAbility oldModel

                newModel : FightModel
                newModel =
                    if List.member card model.cardsUsed then
                        model

                    else
                        model |> updateCardsUsed |> activateAbility
            in
            ( FightPhase newModel, Cmd.none )

        _ ->
            ( FightPhase model, Cmd.none )


updateRewardsPhase : Msg -> RewardsModel -> ( Phase, Cmd Msg )
updateRewardsPhase msg model =
    case msg of
        NextBattle ->
            let
                ( ( drawnEnemy, newDeck ), nextSeed ) =
                    case ( model.enemyDeck, model.enemyDiscard ) of
                        ( [], [] ) ->
                            ( ( defaultEnemy, [] ), model.seed )

                        ( topCard :: rest, _ ) ->
                            ( ( topCard, rest ), model.seed )

                        ( [], topCard :: rest ) ->
                            Random.step (shuffleNonEmptyList topCard rest) model.seed

                newPlayerDiscard : List PlayerCard
                newPlayerDiscard =
                    case model.fightOutcome of
                        PlayerLost playedCards cardsToRemove ->
                            List.append
                                model.playerDiscard
                                (List.filter
                                    (\card -> not (List.member card cardsToRemove))
                                    playedCards
                                )

                        PlayerWon rewardCard ->
                            rewardCard :: model.playerDiscard

                newModel : FightModel
                newModel =
                    { seed = nextSeed
                    , playerDeck = model.playerDeck
                    , playerDiscard = newPlayerDiscard
                    , health = model.health
                    , enemyDeck = newDeck
                    , enemyDiscard = model.enemyDiscard
                    , currentEnemy = drawnEnemy
                    , cardsUsed = []
                    , playedCards = []
                    }
            in
            ( FightPhase newModel, Cmd.none )

        ToggleRemoveCard card isRemoved ->
            let
                updateFightOutcome : List PlayerCard -> List PlayerCard -> FightOutcome
                updateFightOutcome playedCards cardsToRemove =
                    if isRemoved then
                        PlayerLost playedCards (card :: cardsToRemove)

                    else
                        PlayerLost playedCards (List.filter ((/=) card) cardsToRemove)

                newModel : RewardsModel
                newModel =
                    case model.fightOutcome of
                        PlayerWon _ ->
                            model

                        PlayerLost playedCards cardsToRemove ->
                            { model | fightOutcome = updateFightOutcome playedCards cardsToRemove }
            in
            ( RewardsPhase newModel, Cmd.none )

        _ ->
            ( RewardsPhase model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


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
            [ img [ class "card-picture", src "./monster_icon.png" ] []
            , div [ class "card-info" ]
                [ div [] [ text ("Stength: " ++ String.fromInt enemy.strength) ]
                , div [] [ text ("Draws: " ++ String.fromInt enemy.draws) ]
                ]
            ]
        ]


renderEnemyContainer : FightModel -> Html Msg
renderEnemyContainer model =
    let
        strength : Int
        strength =
            model.currentEnemy.strength
    in
    div [ class "enemy-container" ]
        [ div [ class "info-container" ]
            [ renderEnemyDiscard (List.length model.enemyDiscard)
            , renderEnemyDeck (List.length model.enemyDeck)
            ]
        , div [ class "button-container" ]
            [ button [ onClick EndBattle ] [ text "End Battle" ]
            , div [ class "tooltip" ]
                [ div [ class "tooltip-text" ] [ text "Enemy Strength" ]
                , div
                    [ class "strength"
                    , class
                        (if strength > 9 || strength < -9 then
                            "smaller-text"

                         else
                            ""
                        )
                    ]
                    [ text (String.fromInt strength) ]
                ]
            ]
        , div [ class "cards-container" ]
            [ renderEnemyCard model.currentEnemy
            ]
        ]


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


renderPlayedCard : Bool -> PlayerCard -> Html Msg
renderPlayedCard isDisabled card =
    div [ class "played-card" ]
        [ renderPlayerCard card
        , case card.ability of
            Nothing ->
                div [] []

            Just (Ability abilityText _) ->
                div []
                    [ button
                        [ onClick (ActivateCard card)
                        , disabled isDisabled
                        ]
                        [ text abilityText ]
                    ]
        ]


renderRemovableCard : Bool -> PlayerCard -> Html Msg
renderRemovableCard isRemoved card =
    div [ class "played-card" ]
        [ renderPlayerCard card
        , label []
            [ text "Remove"
            , input
                [ type_ "checkbox"
                , checked isRemoved
                , onCheck (ToggleRemoveCard card)
                ]
                []
            ]
        ]


renderPlayerCard : PlayerCard -> Html Msg
renderPlayerCard playerCard =
    div [ class "card player-card" ]
        [ div [ class "border" ] []
        , div [ class "card-top" ] [ text playerCard.name ]
        , div [ class "card-bottom" ]
            [ img [ class "card-picture", src ("./" ++ playerCard.picture ++ ".png") ] []
            , div [ class "card-info" ]
                [ div [] [ text ("Stength: " ++ String.fromInt playerCard.strength) ]
                ]
            ]
        ]


renderPlayerHealth : Int -> Html Msg
renderPlayerHealth health =
    div [ class "player-health-container tooltip" ]
        [ span [ class "tooltip-text" ] [ text "Lose it all and your guys will get really sad" ]
        , div [ class "player-health" ]
            [ img [ src "./morale_icon.png" ] []
            , div [ class "player-health-text" ]
                [ div [ class "morale" ] [ text "Morale" ]
                , div [ class "health-number" ] [ text (String.fromInt health) ]
                ]
            ]
        ]


renderPlayerContainer : FightModel -> Html Msg
renderPlayerContainer model =
    let
        strength : Int
        strength =
            model.playedCards
                |> List.map .strength
                |> List.sum
    in
    div [ class "player-container" ]
        [ div [ class "info-container" ]
            [ renderPlayerDeck (List.length model.playerDeck)
            , renderPlayerDiscard (List.length model.playerDiscard)
            , renderPlayerHealth model.health
            ]
        , div [ class "button-container" ]
            [ div [ class "tooltip" ]
                [ div [ class "tooltip-text" ] [ text "Player Strength" ]
                , div
                    [ class "strength"
                    , class
                        (if strength > 9 || strength < -9 then
                            "smaller-text"

                         else
                            ""
                        )
                    ]
                    [ text (String.fromInt strength) ]
                ]
            , button [ onClick DrawCard ] [ text "Draw Card" ]
            ]
        , div [ class "cards-container" ]
            (List.map
                (\card ->
                    renderPlayedCard (List.member card model.cardsUsed) card
                )
                model.playedCards
            )
        ]


view : Model -> Html Msg
view topModel =
    case topModel.phase of
        FightPhase model ->
            div [ class "main-container" ]
                [ renderEnemyContainer model
                , renderPlayerContainer model
                ]

        RewardsPhase model ->
            div [ class "main-container" ]
                [ div [ class "enemy-container" ]
                    [ div [ class "info-container" ]
                        [ renderEnemyDiscard (List.length model.enemyDiscard)
                        , renderEnemyDeck (List.length model.enemyDeck)
                        ]
                    , div [ class "button-container" ]
                        []
                    , div [ class "cards-container" ] []
                    ]
                , div [ class "player-container" ]
                    [ div [ class "info-container" ]
                        [ renderPlayerDeck (List.length model.playerDeck)
                        , renderPlayerDiscard (List.length model.playerDiscard)
                        , renderPlayerHealth model.health
                        ]
                    , div [ class "button-container" ]
                        [ button [ onClick NextBattle ] [ text "Next Battle" ]
                        ]
                    , div [ class "cards-container" ]
                        (case model.fightOutcome of
                            PlayerLost playedCards cardsToRemove ->
                                List.map (\card -> renderRemovableCard (List.member card cardsToRemove) card) playedCards

                            PlayerWon card ->
                                [ renderPlayerCard card
                                ]
                        )
                    ]
                ]

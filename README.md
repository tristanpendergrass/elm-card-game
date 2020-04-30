Demo at https://www.tristanpendergrass.com/elm-card-game.

# Development

```
$ npm install -g parcel-bundler
$ npm start
```

This should start a

# Deployment

This command builds files in the /docs directory. If hosting on github pages, configure the repository to serve from there.

```
$ npm run build
```

# Todos

- [x] Player deck should just show card back with number indicating how many are in deck.
- [x] Add tooltip libary, with tooltip for number of cards in player deck.
- [x] Add an icon for enemy deck.
- [x] Add enemy deck visuals
- [x] Color code enemy and player card backs
- [x] Redo enemy container
- [x] Redo player container
- [x] Add visuals for player and enemy strength
- [x] Display player health on left hand side
- [ ] Icons for strength, draws
- [x] Add card art for player cards
- [x] Make heal ability work
- [x] Refactor ability to put logic for model change and isDisabled outside of view and update
- [ ] Add post-battle phase
- [ ] Fix bug where tooltip persists after moving mouse off of target if you move mouse onto tooltip itself
- [ ] Try out tailwindcss
- [ ] Let player claim reward in post-battle phase
- [ ] Let player remove cards in post-battle phase

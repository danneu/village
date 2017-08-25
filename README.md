
# village

Another one of my rapidly-built, [incremental "game"](https://www.reddit.com/r/incremental_games/) 
weekend abandonware prototypes for the sole purpose of playing with the [Elm](http://elm-lang.org/) language.

As usual, I threw it on Github so that I could host it on Github Pages. The code itself is sloppy and unorganized.

- Live Demo: <https://www.danneu.com/village/>

![screenshot](https://www.dropbox.com/s/h12xz5va86umnqo/pk6fyp9m.png?raw=1)

You build villagers, give them jobs, and then they harvest resources, train in dungeons, and (unimplemented) fight enemies.

Things I played with this time:

- [evancz/elm-graphics](https://github.com/evancz/elm-graphics): For rendering the village.
- [nphollon/update-clock](https://github.com/nphollon/update-clock): For the fixed time step (physics update).

## Development

Start the hot-reloading webpack dev server:

    npm start

Navigate to <http://localhost:3000>.

Any changes you make to your files (.elm, .js, .css, etc.) will trigger
a hot reload.

## Production

When you're ready to deploy:

    npm run build

This will create a `dist` folder:

    .
    ├── dist
    │   ├── index.html 
    │   ├── app-5df766af1ced8ff1fe0a.css
    │   └── app-5df766af1ced8ff1fe0a.js


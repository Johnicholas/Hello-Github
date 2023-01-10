let express = require("express");

// let tarotCardGenerator = require("./tarot-card-generator");
//let artGenerator = require("./art-generator");
//let pairOfDancersGenerator = require("./pair-of-dancers-generator");
//let photoboothGenerator = require("./photobooth-generator");
//let generator = require("./tattoo-generator");
let keepGenerator = require("./keep-generator");
let cityGenerator = require("./city-generator");

const app = express();

app.get(`/keep`, function (request, response) {
  response.set("Content-Type", "text/html");
  response.send(`
    <!DOCTYPE html>
    <html>
      <head>
        <title>Generator</title>
        <meta name="description" content="A cool thing made with JSVerify">
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="/style.css">
      </head>
      <body>
        <header>
          <h1>Generator</h1>
        </header>
        <main>
          <ul id="results">
            ${keepGenerator()}
          </ul>
        </main>
        <footer>
          by <a href="http://www.johnicholas.com">Johnicholas</a>
        </footer>
      </body>
    </html>
  `);
});

app.get(`/cities`, function (request, response) {
  response.set("Content-Type", "text/html");
  response.send(`
    <!DOCTYPE html>
    <html>
      <head>
        <title>Generator</title>
        <meta name="description" content="A cool thing made with JSVerify">
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="/style.css">
      </head>
      <body>
        <header>
          <h1>Generator</h1>
        </header>
        <main>
          <ul id="results">
            ${citiesGenerator()}
          </ul>
        </main>
        <footer>
          by <a href="http://www.johnicholas.com">Johnicholas</a>
        </footer>
      </body>
    </html>
  `);
});

app.get(`/`, function (request, response) {
  response.set("Content-Type", "text/html");
  response.send(`
    <!DOCTYPE html>
    <html>
      <head>
        <title>Generators</title>
        <meta name="description" content="A cool thing made with JSVerify">
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="/style.css">
      </head>
      <body>
        <header>
          <h1>Generators</h1>
        </header>
        <main>
          <ul id="results">
          <li><a href="keep">keep</a></li>
          <li><a href="cities">cities</a></li>
          </ul>
        </main>
        <footer>
          by <a href="http://www.johnicholas.com">Johnicholas</a>
        </footer>
      </body>
    </html>
  `);
});

/*
function page(route, title, f) {
  app.get(`/${route}`, function (request, response) {
    response.set("Content-Type", "text/html");
    response.send(`
      <!DOCTYPE html>
      <html>
        <head>
          <title>${title} Generator</title>
          <meta name="description" content="A cool thing made with JSVerify">
          <meta charset="utf-8">
          <meta http-equiv="X-UA-Compatible" content="IE=edge">
          <meta name="viewport" content="width=device-width, initial-scale=1">
          <link rel="stylesheet" href="/style.css">
        </head>
        <body>
          <header>
            <h1>${title} Generator</h1>
          </header>
          <main>
            <ul id="results">
              ${f()}
            </ul>
          </main>
          <footer>
            by <a href="http://www.johnicholas.com">Johnicholas</a>
          </footer>
        </body>
      </html>
    `);
  });
}

// page("", "Tarot Card", tarotCardGenerator);
//page("art", "Art-prompt", artGenerator);
//page("pair-of-dancers", "Pair of Dancers", pairOfDancersGenerator);
//page("photobooth", "Photobooth-prompt", photoboothGenerator);
*/

const listener = app.listen(8080, function () {
  console.log("Your app is listening on port " + listener.address().port);
});

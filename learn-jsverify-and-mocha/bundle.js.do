#!/bin/bash
# bundle.js needs to be (re)built (at least) if main.js changes
redo-ifchange main.js
# use browserify to (re)build bundle.js from main.js
browserify main.js -o $3

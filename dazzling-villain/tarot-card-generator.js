"use strict";

let jsc = require("jsverify");
let Articles = require("articles");
let pluralize = require("pluralize");

let lists = require("./lists");

// takes a number, n, and an Arbitrary, arb, and
// returns an Arbitrary that represents a n-tuple of arb
// (with replacement)
function pick_n_from(n, arb) {
  return jsc.tuple(new Array(n).fill(arb));
}

// takes an array of strings, choices, and
// returns an Arbitrary that represents a choice of one of those.
function pick_one(choices) {
  return jsc.oneof(choices.map(jsc.constant));
}

// r is a variadic function, that
// takes a sequence of strings and Arbitraries,
// wraps the strings in jsc.constant to form Arbitraries,
// and returns a Abitrary string that joins the tuple of Arbitraries with spaces.
function r(...args) {
  let arbitraries = Array.from(args).map((x) =>
    typeof x == "string" ? jsc.constant(x) : x
  );
  return jsc.tuple(arbitraries).smap((x) => x.join(" "));
}

// n is a variadic function, that
// takes a sequence of Arbitraries (usually but not necessarily invocations of r)
// and returns an arbitrary one of them.
function n(...args) {
  let arbitraries = Array.from(args).map((x) =>
    typeof x == "string" ? jsc.constant(x) : x
  );
  return jsc.oneof(arbitraries);
}

// Takes an Arbitrary, noun, and
// returns an Arbitrary that is "a" (or "an") followed by that noun
function a(noun) {
  return noun.smap(Articles.articlize);
}

// Takes an Arbitrary, noun, and
// returns an Arbitrary that is that noun followed by "s" (or "es"),
// pluralizing it.
function s(noun) {
  return noun.smap(pluralize);
}

// Takes an arbitrary and a count
// and attempts to draw count unique elements from the arbitrary
// returns an arbitrary that is a tuple
function without_replacement(arb, count) {
  return jsc.bless({
    generator: jsc.generator.bless(() => {
      let output = [];
      let effort = count + 100;
      let sampler = jsc.sampler(arb);
      for (let i = 0; i < effort; i++) {
        let sample = sampler();
        let found = false;
        for (let element of output) {
          if (JSON.stringify(sample) == JSON.stringify(element)) {
            found = true;
            break;
          }
        }
        if (!found) {
          output.push(sample);
          if (output.length == count) {
            break;
          }
        }
      }
      return output;
    })
  });
}

// takes an Arbitrary that generates a tuple
// returns an Arbitrary with tags around and between the samples to form a html list.
function html_list_of(arb) {
  return arb.smap((x) => {
    let output = [];
    output.push("<ul>");
    for (let i = 0; i < x.length; i += 1) {
      output.push("<li>");
      output.push(x[i]);
      output.push("</li>");
    }
    output.push("</ul>");
    return output.join(" ");
  });
}

let origin = r(
  n(
    r(
      `the`,
      pick_one(lists.property),
      pick_one(lists.person_thing),
      "of",
      pick_one(lists.action_process),
      "ING"
    ),
    r(
      `the`,
      pick_one(lists.person_thing),
      "of",
      pick_one(lists.property),
      s(pick_one(lists.person_thing))
    )
  ),
  `, Tarot Card::5 Stylized Character Art Shapes, Textures and Testing::3 OpenAIR library is a collection of Impulse Responses for auralization::2  --ar 1:2`
);

module.exports = jsc.sampler(origin);

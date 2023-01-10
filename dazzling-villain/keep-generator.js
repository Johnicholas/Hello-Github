"use strict";

let jsc = require("jsverify");
let lists = require("./lists");

// lift_and_join is a variadic function, that
// takes a sequence of strings and Arbitraries,
// wraps the strings in jsc.constant to form Arbitraries,
// and returns a Abitrary that joins them together.
function lift_and_join(...args) {
  let arbitraries = Array.from(args).map((x) =>
    typeof x == "string" ? jsc.constant(x) : x
  );
  return jsc.tuple(arbitraries).smap((x) => x.join(""));
}

function html_list_of(arb) {
  return arb.smap((x) => {
    console.log(x); // DEBUG

    let output = [];
    output.push("<ol>");
    for (let i = 0; i < x.length; i += 1) {
      output.push("<li>");
      output.push(x[i]);
      output.push("</li>");
    }
    output.push("</ol>");
    return output.join(" ");
  });
}

function pick_n_from(n, arb) {
  return jsc.tuple(new Array(n).fill(arb));
}

function pick_one(choices) {
  return jsc.oneof(choices.map(jsc.constant));
}

function keep_label_link(arb) {
  return arb.smap((label) => {
    return `<a href="https://keep.google.com/#label/${encodeURIComponent(
      label
    )}">${label}</a>`;
  });
}

function keep_search_link(arb) {
  return arb.smap((x) => {
    // x is something like text=foo,bar&tags=qux,quux
    let component = encodeURIComponent(x);
    // This is weird, and I'm not sure why Google does it this way, but it seems like they do.
    let replacedComponent = component.replace(/%/g, "%25");
    return `<a href="https://keep.google.com/#search/${replacedComponent}" target="_blank" rel="noopener noreferrer">${x}</a>`;
  });
}

function weighted(map_from_strings_to_weights) {
  // add up the values
  let total = 0;
  for (let key in map_from_strings_to_weights) {
    let weight = map_from_strings_to_weights[key];
    total += weight;
  }
  return jsc.integer(0, total).smap((x) => {
    // so, after picking a number, map it to a key
    let so_far = 0;
    for (let key in map_from_strings_to_weights) {
      let weight = map_from_strings_to_weights[key];
      if (x <= so_far + weight) {
        return key;
      } else {
        so_far += weight;
      }
    }
    return "UNEXPECTED";
  });
}

let alphabet_counts = {
  e: 21912,
  t: 16587,
  a: 14810,
  o: 14003,
  i: 13318,
  n: 12666,
  s: 11450,
  r: 10977,
  h: 10795,
  d: 7874,
  l: 7253,
  u: 5246,
  c: 4943,
  m: 4761,
  f: 4200,
  y: 3853,
  w: 3819,
  g: 3693,
  p: 3316,
  b: 2715,
  v: 2019,
  k: 1257,
  x: 315,
  q: 205,
  j: 188,
  z: 128
};

let total_count = 0;

for (const [_, count] of Object.entries(alphabet_counts)) {
  total_count += count;
}

// turn the counts into stop-here probabilities
for (const [key, count] of Object.entries(alphabet_counts)) {
  console.log(`  ${key}: ${total_count / count},`);
  total_count -= count;
}

let letter = weighted(alphabet_counts);

// let labels = html_list_of(
//   pick_n_from(10, keep_label_link(pick_one(lists.labels)))
// );

let queries = lift_and_join(
  "text=",
  letter,
  letter,
  "&tags=",
  pick_one(lists.labels)
);

let searches = html_list_of(pick_n_from(10, keep_search_link(queries)));

let origin = searches;

module.exports = jsc.sampler(origin);

"use strict";

let jsc = require("jsverify");

function html_list_of(arb) {
  return arb.smap((x) => {
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

let cities = weighted({
  "Chicago, IL": 899,
  "Richmond, CA": 475,
  "Minneapolis, MN": 367,
  "Omaha, NE": 267,
  "Philadelphia, PA": 211,
  "St. Paul, MN": 183,
  "Portland, OR": 145,
  "Eau Claire, WI": 130,
  "Seattle, WA": 128,
  "St. Louis, MO": 125,
  "Boston, MA": 94,
  "Baltimore, MD": 90,
  "Pittsburgh, PA": 79,
  "Washington, DC": 64,
  "Milwaukee, WI": 62,
  "Rochester, NY": 54,
  "Cleveland, OH": 48,
  "Madison, WI": 46,
  "Los Angeles, CA": 43,
  "New Orleans, LA": 32,
  "Hartford, CT": 30,
  "Cincinnati, OH": 28,
  "Cambridge, MA": 21,
  "Oakland, CA": 16,
  "Miami, FL": 14,
  "Alexandria, VA": 14,
  "San Francisco, CA": 11,
  "Atlanta, GA": 11,
  "Buffalo, NY": 10,
  "Honolulu, HI": 9,
  "San Juan, PR": 9,
  "Bridgeport, CT": 7,
  "Newark, NJ": 5,
  "Ann Arbor, MI": 4,
  "Arlington, VA": 3,
  "Gresham, OR": 3,
  "Providence, RI": 3
});

let origin = html_list_of(pick_n_from(10, cities));

module.exports = jsc.sampler(origin);

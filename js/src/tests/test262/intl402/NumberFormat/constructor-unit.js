// |reftest| skip-if(release_or_beta) -- Intl.NumberFormat-unified is not released yet
// Copyright 2019 Igalia, S.L. All rights reserved.
// This code is governed by the BSD license found in the LICENSE file.

/*---
esid: sec-initializenumberformat
description: Checks handling of the unit style.
features: [Intl.NumberFormat-unified]
---*/

assert.throws(TypeError, () => {
  new Intl.NumberFormat([], {
    style: "unit",
  })
});

for (const unit of ["test", "MILE", "kB"]) {
  for (const style of [undefined, "decimal", "currency", "unit"]) {
    assert.throws(RangeError, () => {
      new Intl.NumberFormat([], { style, unit })
    }, `{ style: ${style}, unit: ${unit} }`);
  }
}

const nf = new Intl.NumberFormat([], {
  style: "percent",
});
assert.sameValue(nf.resolvedOptions().style, "percent");
assert.sameValue("unit" in nf.resolvedOptions(), false);
assert.sameValue(nf.resolvedOptions().unit, undefined);

function check(unit) {
  const nf = new Intl.NumberFormat([], {
    style: "unit",
    unit,
  });
  const options = nf.resolvedOptions();
  assert.sameValue(options.style, "unit");
  assert.sameValue(options.unit, unit);
}

const units = [
  "acre",
  "bit",
  "byte",
  "celsius",
  "centimeter",
  "day",
  "degree",
  "fahrenheit",
  "fluid-ounce",
  "foot",
  "gallon",
  "gigabit",
  "gigabyte",
  "gram",
  "hectare",
  "hour",
  "inch",
  "kilobit",
  "kilobyte",
  "kilogram",
  "kilometer",
  "liter",
  "megabit",
  "megabyte",
  "meter",
  "mile",
  "mile-scandinavian",
  "millimeter",
  "milliliter",
  "millisecond",
  "minute",
  "month",
  "ounce",
  "percent",
  "petabyte",
  "pound",
  "second",
  "stone",
  "terabit",
  "terabyte",
  "week",
  "yard",
  "year",
];

for (const simpleUnit of units) {
  check(simpleUnit);
  for (const simpleUnit2 of units) {
    check(simpleUnit + "-per-" + simpleUnit2);
    check(simpleUnit2 + "-per-" + simpleUnit);
  }
}

reportCompare(0, 0);

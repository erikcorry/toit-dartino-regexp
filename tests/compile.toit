// Copyright (c) 2015, the Dartino project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.md file.

import expect show *
import string_utils.regexp

main:
  foo_bar
  simple
  newline
  utf
  case

foo_bar -> none:
  re := regexp.RegExp "foo.*bar" --case_sensitive=true --multiline=false
  expect (re.has_matching "foo bar")
  expect (re.has_matching "foobar")
  expect_not (re.has_matching "fooba")

simple -> none:
  TESTS := {
      "foo.*bar": [true,  // case sensitive.
                   false,  // multiline.
                   ["foo bar", "foobar", "foo.....bar", "foo æ bar", "xx foo xx bar xx"],
                   ["fooba", "fobar", "foo baR", "f oobar", "fæbar", "", "fbar", "f", "bar"]],
      "^foo.*bar": [true,  // case sensitive.
                    false,  // multiline.
                    ["foo bar", "foobar", "foo.....bar", "foo æ bar"],
                    ["xx foo xx bar", "fooba", "fobar", "foo baR", "f oobar", "fæbar", "", "fbar", "f", "bar"]],
      "foo.*bar\$": [true,  // case sensitive.
                     false,  // multiline.
                     ["foo bar", "foobar", "foo.....bar", "foo æ bar"],
                     ["foo xx bar xx", "fooba", "fobar", "foo baR", "f oobar", "fæbar", "", "fbar", "f", "bar"]],
      "^foo.*bar\$": [true,  // case sensitive.
                     false,  // multiline.
                     ["foo bar", "foobar", "foo.....bar", "foo æ bar"],
                     ["foo bar xx", "xx foo bar", "fooba", "fobar", "foo baR", "f oobar", "fæbar", "", "fbar", "f", "bar"]],
  }

  TESTS.do: | source vectors |
    re := regexp.RegExp source --case_sensitive=vectors[0] --multiline=vectors[1]
    vectors[2].do: | should_match |
      expect
          re.has_matching should_match
    vectors[3].do: | should_match |
      expect_not
          re.has_matching should_match

newline -> none:
  re := regexp.RegExp "foo.bar" --case_sensitive=true --multiline=false
  expect (re.has_matching "foo bar")
  expect_not (re.has_matching "foo\nbar")
  expect_not (re.has_matching "foo\rbar")

  re = regexp.RegExp "foo.bar" --case_sensitive=true --multiline=true
  expect (re.has_matching "foo bar")
  expect (re.has_matching "foo\nbar")
  expect (re.has_matching "foo\rbar")

utf -> none:
  re := regexp.RegExp "foo.+bar" --case_sensitive=true --multiline=false
  expect (re.has_matching "foo bar")
  expect_not (re.has_matching "foobar")
  expect (re.has_matching "fooæbar")
  expect (re.has_matching "foo€bar")
  expect (re.has_matching "foo☃bar")
  expect (re.has_matching "foo..bar")

  re = regexp.RegExp "^sø(en)?\$" --case_sensitive=true --multiline=false
  expect (re.has_matching "søen")
  expect_not (re.has_matching "søe")
  expect (re.has_matching "sø")

  re = regexp.RegExp "^sø*\$" --case_sensitive=true --multiline=false
  expect_not (re.has_matching "")
  expect (re.has_matching "s")
  expect (re.has_matching "sø")
  expect (re.has_matching "søø")
  expect_not (re.has_matching "sæø")
  expect_not (re.has_matching "søø.")

  re = regexp.RegExp "^s€*\$" --case_sensitive=true --multiline=false
  expect_not (re.has_matching "")
  expect (re.has_matching "s")
  expect (re.has_matching "s€")
  expect (re.has_matching "s€€")
  expect_not (re.has_matching "sæ€")
  expect_not (re.has_matching "s€€.")

  re = regexp.RegExp "^s☃*\$" --case_sensitive=true --multiline=false
  expect_not (re.has_matching "")
  expect (re.has_matching "s")
  expect (re.has_matching "s☃")
  expect (re.has_matching "s☃☃")
  expect_not (re.has_matching "s€☃")
  expect_not (re.has_matching "s☃☃.")

  re = regexp.RegExp "foo.bar" --case_sensitive=true --multiline=false
  expect (re.has_matching "foo bar")
  expect_not (re.has_matching "foobar")
  // A single . matching a UTF-8 sequence is not implemented yet.
  //expect (re.has_matching "fooæbar")
  //expect (re.has_matching "foo€bar")
  //expect (re.has_matching "foo☃bar")
  expect_not (re.has_matching "foo..bar")

case -> none:
  re := regexp.RegExp "foo.+bar" --case_sensitive=false --multiline=false
  expect (re.has_matching "foo bar")
  expect_not (re.has_matching "foobar")
  expect (re.has_matching "Foo BaR")
  expect_not (re.has_matching "Foo Bax")
  expect_not (re.has_matching "FOOBAR")

  re = regexp.RegExp "Søen" --case_sensitive=false --multiline=false
  expect (re.has_matching "Søen")
  expect (re.has_matching "søen")
  expect (re.has_matching "SøEN")
  expect (re.has_matching "SØEN")
  expect (re.has_matching "sØen")
  expect (re.has_matching "..sØen")
  expect_not (re.has_matching "soen")
  expect_not (re.has_matching "söen")

  re = regexp.RegExp "Sø*en" --case_sensitive=false --multiline=false
  expect (re.has_matching "Søen")
  expect (re.has_matching "Søøøen")
  expect (re.has_matching "søen")
  expect (re.has_matching "SøEN")
  expect (re.has_matching "SEN")
  expect (re.has_matching "SøØøØøØøøØØEN")
  expect (re.has_matching "sØen")
  expect (re.has_matching "..sØen")
  expect_not (re.has_matching "soen")
  expect_not (re.has_matching "söen")

  re = regexp.RegExp "Sø*en" --case_sensitive=false --multiline=false
  expect (re.has_matching "Søen")
  expect (re.has_matching "Søøøen")
  expect (re.has_matching "søen")
  expect (re.has_matching "SøEN")
  expect (re.has_matching "SEN")
  expect (re.has_matching "SøØøØøØøøØØEN")
  expect (re.has_matching "sØen")
  expect (re.has_matching "..sØen")
  expect_not (re.has_matching "soen")
  expect_not (re.has_matching "söen")

  // Sigma test. 'Σ', 'ς', 'σ'
  re = regexp.RegExp "Six Σ event" --case_sensitive=false --multiline=false
  expect_not (re.has_matching "Søen")
  expect_not (re.has_matching "Six . event")
  expect_not (re.has_matching "Six .. event")
  expect_not (re.has_matching "Six ... event")
  expect (re.has_matching "six Σ event")
  expect (re.has_matching "six ς event")
  expect (re.has_matching "six σ event")

  re = regexp.RegExp "Six Σ event" --case_sensitive=true --multiline=false
  expect_not (re.has_matching "Søen")
  expect_not (re.has_matching "Six . event")
  expect_not (re.has_matching "Six .. event")
  expect_not (re.has_matching "Six ... event")
  expect (re.has_matching "Six Σ event")
  expect_not (re.has_matching "Six ς event")
  expect_not (re.has_matching "Six σ event")

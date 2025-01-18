// Copyright (c) 2015, the Dartino project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.md file.

import expect show *
import regexp show RegExp
import regexp

main:
  foo-bar
  simple
  newline
  utf
  case
  char-class
  greedy
  look-around
  match
  vim:           RegExp.vim it
  vim --ed-mode: RegExp.ed it

foo-bar -> none:
  re := RegExp "foo.*bar"
  expect (re.has-matching "foo bar")
  expect (re.has-matching "foobar")
  expect-not (re.has-matching "fooba")

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
    re := RegExp source --case-sensitive=vectors[0] --multiline=vectors[1]
    vectors[2].do: | should-match |
      expect
          re.has-matching should-match
    vectors[3].do: | should-match |
      expect-not
          re.has-matching should-match

newline -> none:
  re := RegExp "foo.bar" --multiline=false
  expect (re.has-matching "foo bar")
  expect-not (re.has-matching "foo\nbar")
  expect-not (re.has-matching "foo\rbar")

  re = RegExp "foo.bar" --multiline=true
  expect (re.has-matching "foo bar")
  expect (re.has-matching "foo\nbar")
  expect (re.has-matching "foo\rbar")

utf -> none:
  re := RegExp "foo.+bar" --multiline=false
  expect (re.has-matching "foo bar")
  expect-not (re.has-matching "foobar")
  expect (re.has-matching "fooæbar")
  expect (re.has-matching "foo€bar")
  expect (re.has-matching "foo☃bar")
  expect (re.has-matching "foo..bar")

  re = RegExp "^sø(en)?\$"
  expect (re.has-matching "søen")
  expect-not (re.has-matching "søe")
  expect (re.has-matching "sø")

  re = RegExp "^sø*\$"
  expect-not (re.has-matching "")
  expect (re.has-matching "s")
  expect (re.has-matching "sø")
  expect (re.has-matching "søø")
  expect-not (re.has-matching "sæø")
  expect-not (re.has-matching "søø.")

  re = RegExp "^s€*\$"
  expect-not (re.has-matching "")
  expect (re.has-matching "s")
  expect (re.has-matching "s€")
  expect (re.has-matching "s€€")
  expect-not (re.has-matching "sæ€")
  expect-not (re.has-matching "s€€.")

  re = RegExp "^s☃*\$"
  expect-not (re.has-matching "")
  expect (re.has-matching "s")
  expect (re.has-matching "s☃")
  expect (re.has-matching "s☃☃")
  expect-not (re.has-matching "s€☃")
  expect-not (re.has-matching "s☃☃.")

  re = RegExp "foo.bar"
  expect (re.has-matching "foo bar")
  expect-not (re.has-matching "foobar")
  expect (re.has-matching "fooæbar")
  expect (re.has-matching "foo€bar")
  expect (re.has-matching "foo☃bar")
  expect-not (re.has-matching "foo..bar")

case -> none:
  re := RegExp "foo.+bar" --case-sensitive=false
  expect (re.has-matching "foo bar")
  expect-not (re.has-matching "foobar")
  expect (re.has-matching "Foo BaR")
  expect-not (re.has-matching "Foo Bax")
  expect-not (re.has-matching "FOOBAR")

  re = RegExp "Søen" --case-sensitive=false
  expect (re.has-matching "Søen")
  expect (re.has-matching "søen")
  expect (re.has-matching "SøEN")
  expect (re.has-matching "SØEN")
  expect (re.has-matching "sØen")
  expect (re.has-matching "..sØen")
  expect-not (re.has-matching "soen")
  expect-not (re.has-matching "söen")

  re = RegExp "Sø*en" --case-sensitive=false
  expect (re.has-matching "Søen")
  expect (re.has-matching "Søøøen")
  expect (re.has-matching "søen")
  expect (re.has-matching "SøEN")
  expect (re.has-matching "SEN")
  expect (re.has-matching "SøØøØøØøøØØEN")
  expect (re.has-matching "sØen")
  expect (re.has-matching "..sØen")
  expect-not (re.has-matching "soen")
  expect-not (re.has-matching "söen")

  re = RegExp "Sø*en" --case-sensitive=false
  expect (re.has-matching "Søen")
  expect (re.has-matching "Søøøen")
  expect (re.has-matching "søen")
  expect (re.has-matching "SøEN")
  expect (re.has-matching "SEN")
  expect (re.has-matching "SøØøØøØøøØØEN")
  expect (re.has-matching "sØen")
  expect (re.has-matching "..sØen")
  expect-not (re.has-matching "soen")
  expect-not (re.has-matching "söen")

  // Sigma test. 'Σ', 'ς', 'σ'
  re = RegExp "Six Σ event" --case-sensitive=false
  expect-not (re.has-matching "Søen")
  expect-not (re.has-matching "Six . event")
  expect-not (re.has-matching "Six .. event")
  expect-not (re.has-matching "Six ... event")
  expect (re.has-matching "six Σ event")
  expect (re.has-matching "six ς event")
  expect (re.has-matching "six σ event")

  re = RegExp "Six Σ event" --case-sensitive=true
  expect-not (re.has-matching "Søen")
  expect-not (re.has-matching "Six . event")
  expect-not (re.has-matching "Six .. event")
  expect-not (re.has-matching "Six ... event")
  expect (re.has-matching "Six Σ event")
  expect-not (re.has-matching "Six ς event")
  expect-not (re.has-matching "Six σ event")

  re = RegExp "foo[a-z]bar" --case-sensitive=false
  expect-not (re.has-matching "foo bar")
  expect-not (re.has-matching "FOO BAR")
  expect (re.has-matching "fooabar")
  expect (re.has-matching "fooAbar")
  expect (re.has-matching "foogbar")
  expect (re.has-matching "fooGbar")
  expect (re.has-matching "foozbar")
  expect (re.has-matching "fooZbar")
  expect-not (re.has-matching "foo@bar")
  expect-not (re.has-matching "foo{bar")
  expect-not (re.has-matching "foo[bar")
  expect-not (re.has-matching "foo`bar")

  re = RegExp "foo[M-d]bar" --case-sensitive=false
  expect-not (re.has-matching "foolbar")
  expect-not (re.has-matching "fooebar")
  expect-not (re.has-matching "fooLbar")
  expect-not (re.has-matching "fooEbar")
  expect (re.has-matching "foombar")
  expect (re.has-matching "fooMbar")
  expect (re.has-matching "foodbar")
  expect (re.has-matching "fooDbar")
  expect (re.has-matching "foo`bar")

char-class:
  re := RegExp "foo[z-ø]bar" --case-sensitive=false
  expect (re.has-matching "foozbar")
  expect (re.has-matching "fooZbar")
  expect-not (re.has-matching "fooxbar")
  expect-not (re.has-matching "fooXbar")
  expect (re.has-matching "fooøbar")
  expect (re.has-matching "fooØbar")
  expect (re.has-matching "fooµbar")      // 0xb5, mu in the Latin1 plane.
  expect (re.has-matching "fooμbar")      // 0x3bc, mu in the greek lower case area.
  expect (re.has-matching "fooΜbar")      // 0x39c, mu in the greek upper case area.
  expect-not (re.has-matching "fooMbar")  // Just a regular M.
  expect-not (re.has-matching "foo€bar")  // 3-byte Unicode char.
  expect-not (re.has-matching "foo☃bar")  // 4-byte Unicode char.

  re = RegExp "foo[^z-ø]bar" --case-sensitive=false
  expect-not (re.has-matching "foozbar")
  expect-not (re.has-matching "fooZbar")
  expect (re.has-matching "fooxbar")
  expect (re.has-matching "fooXbar")
  expect-not (re.has-matching "fooøbar")
  expect-not (re.has-matching "fooØbar")
  expect-not (re.has-matching "fooµbar")   // 0xb5, mu in the Latin1 plane.
  expect-not (re.has-matching "fooμbar")   // 0x3bc, mu in the greek lower case area.
  expect-not (re.has-matching "fooΜbar")   // 0x39c, mu in the greek upper case area.
  expect (re.has-matching "foo€bar")       // 3-byte Unicode char.
  expect (re.has-matching "foo☃bar")       // 4-byte Unicode char.
  expect (re.has-matching "fooMbar")       // Just a regular M.

greedy:
  re := RegExp "foo.{3,}bar"
  expect-not (re.has-matching "foobar")
  expect-not (re.has-matching "foo.bar")
  expect-not (re.has-matching "foo..bar")
  expect     (re.has-matching "foo...bar")
  expect     (re.has-matching "foo....bar")
  expect     (re.has-matching "foo.....bar")
  expect     (re.has-matching "foo......bar")

  // With Unicode characters we mustn't think we are 3 characters in when we
  // are actually only 3 bytes in.
  expect-not (re.has-matching "foo±bar")
  expect-not (re.has-matching "foo±±bar")
  expect     (re.has-matching "foo±±±bar")
  expect     (re.has-matching "foo±±±±bar")
  expect     (re.has-matching "foo±±±±±bar")
  expect     (re.has-matching "foo±±±±±±bar")

  // Try again with a . that can't match newlines
  re = RegExp "foo.{3,}bar"
  expect-not (re.has-matching "foo±bar")
  expect-not (re.has-matching "foo±±bar")
  expect     (re.has-matching "foo±±±bar")
  expect     (re.has-matching "foo±±±±bar")
  expect     (re.has-matching "foo±±±±±bar")
  expect     (re.has-matching "foo±±±±±±bar")

  // Don't let a negative character class match a part of a UTF-8 sequence.
  re = RegExp "foo.*[^f][^f]bar"
  expect-not (re.has-matching "foo€bar")

look-around -> none:
  // Positive look-ahead.
  re := RegExp "foo(?=[a-z]{5})bar"
  expect     (re.has-matching "foobarxx")
  expect     (re.has-matching "..foobarxx..")
  expect     (re.has-matching "..foobarrr..")
  expect     (re.has-matching "..foobarrrrr")
  expect-not (re.has-matching "..foobar....")
  expect-not (re.has-matching "..foobarx...")
  expect     (re.has-matching "..foofoobarxx..")

  // Negative look-ahead.
  re = RegExp "foo(?![a-z]{5})bar"
  expect-not (re.has-matching "foobarxx")
  expect-not (re.has-matching "..foobarxx..")
  expect-not (re.has-matching "..foobarrr..")
  expect-not (re.has-matching "..foobarrrrr")
  expect     (re.has-matching "..foobar....")
  expect     (re.has-matching "..foobarx...")
  expect-not (re.has-matching "..foofoobarxx..")

match -> none:
  // No ().
  re := RegExp ".x.y."
  m := re.first-matching ".x.y."
  expect-equals 0 m.index
  expect-equals 5 m.matched.size

  // Capturing ().
  re = RegExp ".(x.y)."
  m = re.first-matching ".x.y."
  expect-equals 0 m.index
  expect-equals 5 m.matched.size
  expect-equals ".x.y." m[0]
  expect-equals "x.y" m[1]
  expect-equals 1 (m.index 1)
  expect-equals 3 m[1].size

  m = re.first-matching ".x*y."
  expect-equals 0 m.index
  expect-equals 5 m.matched.size
  expect-equals ".x*y." m[0]
  expect-equals "x*y" m[1]
  expect-equals 1 (m.index 1)
  expect-equals 3 m[1].size

  // Capturing and non-capturing ().
  re = RegExp "(.)(?:x.y)(.)"
  m = re.first-matching ".0x1y2."
  expect-equals 1 m.index
  expect-equals 5 m.matched.size
  expect-equals "0x1y2" m[0]
  expect-equals "0" m[1]
  expect-equals "2" m[2]

  // Capturing () in a look-ahead.
  re = RegExp "foo(?=...(..))bar"
  m = re.first-matching "   foobar42  "
  expect-equals "foobar" m[0]  // Capture 0 is the whole match.
  expect-equals "42" m[1]      // Capture 1 is outside of capture 0!

  // Optional () in loop depends on last iteration.
  re = RegExp "(?:(foo)?(bar)?/)*"
  m = re.first-matching "foobar/foobar/foo/"
  expect-equals "foobar/foobar/foo/" m[0]
  expect-equals "foo" m[1]
  expect-equals null m[2]
  m = re.first-matching "foobar/foo/foobar/"
  expect-equals "foobar/foo/foobar/" m[0]
  expect-equals "foo" m[1]
  expect-equals "bar" m[2]

vim --ed-mode/bool=false [create] -> none:
  // In vim mode, plain () are literals.
  re := create.call "(foo-bar)"
  m := re.first-matching "(foo-bar)"
  expect-equals "(foo-bar)" m[0]

  re2 := create.call "\\(foo-bar\\)"
  m2 := re2.first-matching "(foo-bar)"
  expect-equals "foo-bar" m2[0]
  expect-equals "foo-bar" m2[1]

  re3 := create.call "fo\\+-bar+"
  m3 := re3.first-matching "foo-bar+"
  expect-equals "foo-bar+" m3[0]

  re4 := create.call "fo\\{2,3\\}-bar{2}"
  m4 := re4.first-matching "foo-bar{2}"
  expect-equals "foo-bar{2}" m4[0]

  // Even in vim mode, asterisk is not escaped.
  re5 := create.call "fo*bar\\*"
  m5 := re5.first-matching "foobar*"
  expect-equals "foobar*" m5[0]

  // Vim mode has \< and \> instead of \b.
  re6 := create.call "\\<foo\\>"
  m6 := re6.first-matching ".foo."
  expect-equals "foo" m6[0]
  m61 := re6.first-matching "foo."
  expect-equals "foo" m61[0]
  m62 := re6.first-matching ".foo"
  expect-equals "foo" m62[0]
  m63 := re6.first-matching "foo"
  expect-equals "foo" m63[0]
  m64 := re6.first-matching "foofoo"
  expect-equals null m64

  // Matches backslash in Vim mode, word boundary in ed-mode
  re7 := create.call "\\bfoo\\b"
  m7 := re7.first-matching ".foo."
  m71 := re7.first-matching "\x08foo\x08"
  if ed-mode:
    expect-equals "foo" m7[0]
    expect-equals "foo" m71[0]
  else:
    expect-equals null m7
    expect-equals "foo" m71[0][1..4]

  re8 := create.call "\\<.*\\>"
  m8 := re8.first-matching "foo"
  expect-equals "foo" m8[0]
  m81 := re8.first-matching " foo "
  expect-equals "foo" m81[0]
  m82 := re8.first-matching "foo bar"
  expect-equals "foo bar" m82[0]
  m83 := re8.first-matching "[foo bar]"
  expect-equals "foo bar" m83[0]
  m84 := re8.first-matching "øen"  // No unicode support in vim mode either.
  expect-equals "en" m84[0]

  re9 := create.call "fo\\?o"
  m9 := re9.first-matching "foo"
  expect-equals "foo" m9[0]
  m91 := re9.first-matching "fox"
  expect-equals "fo" m91[0]

  // Vim mode has \= as a synonym for \?.
  re10 := create.call "fo\\=o"
  m10 := re10.first-matching "foo"
  m101 := re10.first-matching "fox"
  if ed-mode:
    expect-equals null m10
    expect-equals null m101
  else:
    expect-equals "foo" m10[0]
    expect-equals "fo" m101[0]

  // \d is a literal d in ed mode, but a digit in vim mode.
  re11 := create.call "\\d\\+"
  m11 := re11.first-matching "123"
  m111 := re11.first-matching "1a3"
  m112 := re11.first-matching "a123"
  m113 := re11.first-matching "d123"
  if ed-mode:
    expect-equals null m11
    expect-equals null m111
    expect-equals null m112
    expect-equals "d" m113[0]
  else:
    expect-equals "123" m11[0]
    expect-equals "1" m111[0]
    expect-equals "123" m112[0]
    expect-equals "123" m113[0]

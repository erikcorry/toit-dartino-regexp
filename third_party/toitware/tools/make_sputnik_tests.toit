// Copyright (C) 2022 Toitware ApS. All rights reserved.
// Use of this source code is governed by an MIT-style license that can be
// found in the third_party/toitware/LICENSE file.

import host.directory show *
import host.file
import reader show BufferedReader

// We don't have the Sputnik copyright message in plain text because automated
// tools might conclude that this file is under the Sputnik copyright.
SPUTNIK_COPYRIGHT ::= rot13 "// Pbclevtug 2009 gur Fchgavx nhgubef.  Nyy evtugf erfreirq."
SPUTNIK_COVERAGE  ::= rot13 "// Guvf pbqr vf tbirearq ol gur OFQ yvprafr sbhaq va gur YVPRAFR svyr."

SPUTNIK_DIR ::= "../../../../test262"

main:
  if not file.is_directory SPUTNIK_DIR:
    print "Expects to be run from the third_party/toitware/tools/directory."
    print "Expects test262 to be checked out next to the regexp checkout."
    exit 1
  license := file.read_content "$SPUTNIK_DIR/LICENSE"
  out := file.Stream.for_write "../../sputnik/LICENSE"
  out.write license

  out = file.Stream.for_write "../../sputnik/tests/sputnik_test.toit"
  out_unicode := file.Stream.for_write "../../sputnik/tests/sputnik_test_unicode.toit"
  out_deseret := file.Stream.for_write "../../sputnik/tests/sputnik_test_deseret.toit"
  [out, out_unicode, out_deseret].do:
    condense_tests_to_one_file it --unicode=(it != out) --deseret=(it == out_deseret)

condense_tests_to_one_file out --unicode/bool --deseret/bool -> none:
  out.write """
      $SPUTNIK_COPYRIGHT
      $SPUTNIK_COVERAGE

      // Autogenerated by make_sputnik_tests.toit

      import expect show *
      import regexp show RegExp
      import regexp

      check match expected:
        expect_equals match.capture_count (expected.size - 1)
        actual := []
        (match.capture_count + 1).repeat:
          actual.add match[it]
        if actual != expected:
          print "Expected\\n\$expected, got\\n\$actual"
        expect_equals expected actual

      main:
        re := null
        m := null
        expected := null
      """

  REGEXP_TESTS := "$SPUTNIK_DIR/test/built-ins/RegExp"
  dir := DirectoryStream REGEXP_TESTS
  emitted_entry := ""
  while entry := dir.next:
    if entry.starts_with "S" and entry.ends_with ".js":
      fd := file.Stream.for_read "$REGEXP_TESTS/$entry"
      try:
        emitted_regexp := false
        reader := BufferedReader fd
        if reader.read_line != SPUTNIK_COPYRIGHT: continue
        if reader.read_line != SPUTNIK_COVERAGE: continue
        while line := reader.read_line:
          index := line.index_of "__executed = "
          if index >= 0:
            emitted_regexp = handle_executed_line out entry line index --unicode=unicode --deseret=deseret
          index = line.index_of "__expected = "
          if index >= 0:
            if emitted_regexp:
              handle_expected_line out entry line index --unicode=unicode --deseret=deseret
          if emitted_regexp and line.starts_with "assert(!__executed":
            out.write "  expect_equals null m\n"
        finally:
          fd.close

is_alpha char/int -> bool:
  return 'a' <= char <= 'z'
      or 'A' <= char <= 'Z'

handle_executed_line out filename/string line/string index/int --unicode/bool --deseret/bool -> bool:
  if line[index + 13] != '/':
    out.write "\n"
    out.write "  // $filename\n"
    out.write "  // Rejected for not using literal regexp: $line\n"
    return false
  problem_for_unicode := false
  for dash_index := -2; dash_index != -1 and dash_index < line.size - 1; dash_index = line.index_of "-" (dash_index < 0 ? 0 : dash_index + 1):
    if dash_index > 0 and (is_alpha line[dash_index - 1]):
      problem_for_unicode = true
    if 0 <= dash_index < line.size - 1 and (is_alpha line[dash_index + 1]):
      problem_for_unicode = true
  // The regexp constructs \w and \b are inherently ASCII-only.
  if (line.index_of "\\w") >= 0 or (line.index_of "\\b") >= 0 or (line.index_of "\\B") >= 0:
    problem_for_unicode = true

  if unicode and problem_for_unicode:
    out.write "\n"
    out.write "  // $filename\n"
    out.write "  // Rejected for being ASCII-specific: $line\n"
    return false

  case_independent := false
  multi_line := false
  close := line[index + 14..].index_of "/."
  if close == -1:
    close = line[index + 14..].index_of "/i."
    if close != -1: case_independent = true
  if close == -1:
    close = line[index + 14..].index_of "/m."
    if close != -1: multi_line = true
  if close == -1:
    out.write "\n"
    out.write "  // $filename\n"
    out.write "  // Rejected for using /g flag: $line\n"
    return false
  method_index := (line[index + 14 + close..].index_of ".")
  if method_index == -1: throw "Could not parse $line"
  method_index += index + 15 + close
  method := null
  if line[method_index..].starts_with "exec(":
    method = "exec"
  else if line[method_index..].starts_with "test(":
    method = "test"
  else:
    throw "Could not parse $line $line[method_index..]"
  re_text := line[index + 14..index + 14 + close]
  escaped := convert_from_js_regexp_to_toit_escape re_text --unicode_munge=unicode --deseret_munge=deseret
  out.write "\n"
  out.write "  // $filename\n"
  out.write "  re = RegExp \"$escaped\" --case_sensitive=$(not case_independent) --multiline=$multi_line\n"
  input_quote := line[method_index + 5]
  if input_quote != '"' and input_quote != '\'':
    out.write "  // Rejected input ($line[method_index + 5..]\n"
    return false
  end_quote := line[method_index + 6..].index_of --last "$(%c input_quote)"
  if end_quote == -1: throw "Where is the end: $line"
  input := line[method_index + 6..method_index + 6 + end_quote]
  input = convert_from_js_string_to_toit_escape input --unicode_munge=unicode --deseret_munge=deseret
  out.write "  m = re.first_matching \"$input\"\n"
  return true

handle_expected_line out filename/string line/string index/int --unicode/bool --deseret/bool -> none:
  if line[index + 13] != '[':
    out.write "  // Rejected test: '$line'\n"
    return
  end_index := line.index_of --last "]"
  array_contents := line[index + 14..end_index]
  expected := escape_js_array array_contents --unicode_munge=unicode --deseret_munge=deseret
  if expected:
    out.write "  expected = $expected\n"
    out.write "  check m expected\n"
  else:
    out.write "  // Rejected for containing non-literal expectation $line\n"

convert_from_js_regexp_to_toit_escape str/string --unicode_munge/bool=false --deseret_munge=false -> string:
  str = parse_js_regexp str
  str = toit_escape str --unicode_munge=unicode_munge --deseret_munge=deseret_munge
  return str

convert_from_js_string_to_toit_escape str/string --unicode_munge/bool=false --deseret_munge=false -> string:
  str = parse_js_string str
  str = toit_escape str --unicode_munge=unicode_munge --deseret_munge=deseret_munge
  return str

MUST_ESCAPE_TOIT ::= ByteArray 128:
  esc := it == '$' or it == '\\' or it == '"' or it == '\n' or it == '\t'
  hex := it < 32 or it == 127
  esc ? 2 : (hex ? 4 : 1)

must_escape_toit char/int? -> int:
  if not char:
    return 0  // Not at the start of a character in the string.
  if char >= MUST_ESCAPE_TOIT.size:
    if char >= 0x10000:
      return 10  // \u{012345}
    else:
      return 6   // \u1234
  else:
    return MUST_ESCAPE_TOIT[char]

MUNGE_MAP ::= {
    'a': "å",
    'c': "ç",
    'e': "é",
    'g': "ġ",
    'i': "ï",
    'l': "ł",
    'o': "ø",
    'u': "ü",
    'A': "Å",
    'C': "Ç",
    'E': "É",
    'G': "Ġ",
    'I': "Ï",
    'L': "Ł",
    'O': "Ø",
    'U': "Ü",
}

MUNGE_MAP_DESERET ::= {
    'a': "𐐰",
    'b': "𐐺",
    'c': "𐑅",
    'e': "é",
    'g': "𐑀",
    'i': "𐐮",
    'l': "𐑊",
    'o': "𐐲",
    'u': "𐐭",
    'A': "𐐈",
    'B': "𐐒",
    'C': "𐐝",
    'E': "É",
    'G': "𐐘",
    'I': "𐐆",
    'L': "𐐢",
    'O': "𐐊",
    'U': "𐐅",
}

/// Escapes a string so that it can be used in Toit source code.
toit_escape str/string --unicode_munge/bool=false --deseret_munge/bool=false -> string:
  MAP ::= deseret_munge ? MUNGE_MAP_DESERET : MUNGE_MAP
  double_quote_found := false
  str.do:
    if it == '"' and (str.index_of "\"\"\"") == -1:
      double_quote_found = true
      continue.do
    if (unicode_munge and MAP.contains it) or (must_escape_toit it) != 1:
      out_bytes := 0
      str.do:
        if unicode_munge and MAP.contains it:
          out_bytes += MAP[it].size
        else:
          out_bytes += must_escape_toit it
      ba := ByteArray out_bytes
      i := 0
      str.do: | char/int? |
        bytes := ?
        if unicode_munge and MAP.contains char:
          bytes = MAP[char].size
          replacement := MAP[char]
          ba.replace i replacement
          i += replacement.size
          continue.do
        bytes = must_escape_toit char
        if bytes == 1:
          ba[i++] = char
        else if bytes == 2:
          ba[i++] = '\\'
          ba[i++] = char == '\n' ? 'n' : char == '\t' ? 't' : char
        else if bytes == 4:
          ba[i++] = '\\'
          ba[i++] = 'x'
          ba[i++] = to_lower_case_hex (char >> 4) & 0xf
          ba[i++] = to_lower_case_hex (char & 0xf)
        else if bytes >= 6:
          ba[i++] = '\\'
          ba[i++] = 'u'
          if bytes == 10:
            ba[i++] = '{'
            ba[i++] = to_lower_case_hex (char >> 20) & 0xf
            ba[i++] = to_lower_case_hex (char >> 16) & 0xf
          ba[i++] = to_lower_case_hex (char >> 12) & 0xf
          ba[i++] = to_lower_case_hex (char >> 8) & 0xf
          ba[i++] = to_lower_case_hex (char >> 4) & 0xf
          ba[i++] = to_lower_case_hex (char & 0xf)
          if bytes == 10: ba[i++] = '}'
        else:
          assert: bytes == 0
      return ba.to_string
  if double_quote_found:
    return "\"\"$str\"\""
  return str

/**
Reads a regexp expressed in JS source and returns the string that will be
  passed to the regexp engine.  Most chars are passed unchanged.
*/
parse_js_regexp str -> string:
  l := []
  for i := 0; i < str.size; i++:
    char := str[i]
    if not char: continue
    if char == '\\':
      // A JS regexp can't end with an unescaped backslash so this should never
      // fail.
      next := str[i + 1]
      if next == '/':
        l.add '/'  // backslash-forward-slash becomes a single forward slash.
      else if next == '\\':
        l.add '\\'  // double backslash becomes a quad backslash.
        l.add '\\'
      else:
        l.add '\\'
        l.add next
      i++
    else:
      l.add char
  return string.from_runes l

JS_STRING_ESCAPES_ ::= {
  '\'': '\'',
  '"':  '"',
  '\\': '\\',
  'n':  '\n',
  'r':  '\r',
  't':  '\t',
  'b':  '\b',
  'f':  '\f',
  'v':  '\v',
}

/**
Reads a string expressed in JS source and returns the string that will be
  generated by the compiler.
*/
parse_js_string str -> string:
  l := []
  for i := 0; i < str.size; i++:
    char := str[i]
    if not char: continue
    if char == '"':
      l.add '"'
    else if char == '\'':
      l.add '\''
    else if char == '\\':
      // A JS source string can't end with an unescaped backslash so this
      // should never fail.
      next := str[i + 1]
      if JS_STRING_ESCAPES_.contains next:
        l.add JS_STRING_ESCAPES_[next]
      else if next == 'x':  // \xa2.
        c := (hex_char_to_value str[i + 2]) << 4
        c |= (hex_char_to_value str[i + 3])
        l.add c
        i += 2
      else if next == 'u':  // \u12a5.
        c := (hex_char_to_value str[i + 2]) << 12
        c |= (hex_char_to_value str[i + 3]) << 8
        c |= (hex_char_to_value str[i + 4]) << 4
        c |= (hex_char_to_value str[i + 5])
        l.add c
        i += 4
      else if next == '0':  // Octal escape.
        c := 0
        j := 1
        while j <= 3:
          if not str[i + j]:
            break
          if not '0' <= str[i + j] <= '9':
            break
          c <<= 3
          c += str[i + j] - '0'
          j++
        l.add c
        i += j - 1
      else:  // Random letter escaped for no reason, eg \a.
        l.add next
      i++
    else:
      l.add char
  return string.from_runes l

escape_js_array input/string --unicode_munge/bool=false --deseret_munge/bool=false -> List?:
  result := []
  for i := 0; i < input.size; i++:
    if input[i] == ' ': continue
    if input[i..].starts_with "undefined":
      result.add null
      i += 9
    else if input[i..].starts_with "__body,":
      return null
    else if input[i..] == "__strOriginal":
      return null
    else if input[i] == '\'' or input[i] == '"':
      i++
      start := i
      while i < input.size and input[i] != input[start - 1]:
        if input[i] == '\\': i++
        i++
      end := i
      i++
      converted := convert_from_js_string_to_toit_escape
          input[start..end]
          --unicode_munge=unicode_munge
          --deseret_munge=deseret_munge
      result.add "\"$converted\""
    else:
      throw "Lost in $input at $i $(%c input[i])"
    while i < input.size and input[i] == ' ': i++
    if i < input.size:
      if input[i] != ',':
        throw "Lost in $input at $i"
  return result

rot13 str -> string:
  ba := ByteArray str.size:
    c := str[it]
    if 'a' <= c <= 'm':
      c + 13
    else if 'n' <= c <= 'z':
      c - 13
    else if 'A' <= c <= 'M':
      c + 13
    else if 'N' <= c <= 'Z':
      c - 13
    else:
      c
  return ba.to_string

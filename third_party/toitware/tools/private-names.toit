// Copyright (C) 2021 Toitware ApS. All rights reserved.
// Use of this source code is governed by an MIT-style license that can be
// found in the third_party/toitware/LICENSE file.

import host.pipe
import reader show BufferedReader

main:
  reader := BufferedReader pipe.stdin
  line-number := 1
  while line := reader.read-line:
    if line.size == 0:
      print line
      continue
    in-word := is-word-char line[0]
    out-line := ""
    fragment-start := 0

    for col := 0; col <= line.size; col++:
      edge := ?
      if col < line.size:
        edge = (is-word-char line[col]) != in-word
        if col >= 2 and line[col - 2] == '\\': edge = true
      else:
        edge = true
      if edge:
        out-line += transform line[fragment-start..col]
        fragment-start = col
        in-word = not in-word
    print out-line

transform in/string -> string:
  return case-transform (private-transform in)

// Change from _name to name_ formats.
private-transform in/string -> string:
  if in[0] != '_': return in
  return in[1..] + "_"

// Change from camelCase to underscore_case
case-transform in/string -> string:
  if not 'a' <= in[0] <= 'z': return in
  fragment-start := 0
  out := ""
  for i := 0; i <= in.size; i++:
    edge := false
    if 1 <= i < in.size:
      edge = (is-lower-case in[i - 1]) and (is-upper-case in[i])
    else if i == in.size:
      edge = true
    if edge:
      fragment := in[fragment-start..i]
      if is-upper-case fragment[0]:
        out += "_$(to-lower fragment)"
      else:
        out += fragment
      fragment-start = i
  return out

to-lower in/string -> string:
  out := ByteArray in.size:
    c := in[it]
    if is-upper-case c:
      c + 32
    else:
      c
  return out.to-string

is-lower-case c/int -> bool:
  return 'a' <= c <= 'z'

is-upper-case c/int -> bool:
  return 'A' <= c <= 'Z'

is-word-char c/int -> bool:
  if 'a' <= c <= 'z': return true
  if 'A' <= c <= 'Z': return true
  if '0' <= c <= '9': return true
  if c == '_': return true
  return false

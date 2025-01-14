// Copyright (C) 2021 Toitware ApS.
// Use of this source code is governed by a Zero-Clause BSD license that can
// be found in the EXAMPLES_LICENSE file.

import case

main:
  name := "Donald Duck"

  up := case.to-upper name
  down := case.to-lower name

  print up    // DONALD DUCK
  print down  // donald duck

  name2 := "Øjvind Ørn"

  up = case.to-upper name2
  down = case.to-lower name2

  print up    // ØJVIND ØRN
  print down  // øjvind ørn

  name3 := "Ντόναλντ Ντακ"

  up = case.to-upper name3
  down = case.to-lower name3

  print up    // ΝΤΌΝΑΛΝΤ ΝΤΑΚ
  print down  // ντόναλντ ντακ

  name4 := "Straßenbahnführer"

  up = case.to-upper name4
  down = case.to-lower name4

  print up    // STRASSENBAHNFÜHRER
  print down  // straßenbahnführer

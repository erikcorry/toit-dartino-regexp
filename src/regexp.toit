// Copyright (c) 2015, the Dartino project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.md file.

import bytes show Buffer
import case

class MiniExpLabel_:
  // Initially points to -1 to indicate the label is neither linked (used) nor
  // bound (fixed to a location). When the label is linked, but not bound, it
  // has a negative value, determined by fixup_location(l), that indicates the
  // location of a reference to it, that will be patched when its location has
  // been bound.  When the label is bound, the negative value is used to patch
  // the chained locations that need patching, and the location is set to the
  // correct location for future use.
  static NO-LOCATION ::= -1
  location_ := NO-LOCATION

  is-bound -> bool:
    return location_ >= 0

  is-linked -> bool:
    return location_ < NO-LOCATION

  bind codes/List/*<int>*/ -> none:
    assert: not is-bound
    l /int := codes.size
    for forward-reference := location_; forward-reference != NO-LOCATION; :
      patch-location /int := decode-fixup_ forward-reference
      forward-reference = codes[patch-location]
      codes[patch-location] = l
    location_ = l

  location -> int:
    assert: is-bound
    return location_

  // The negative value is -(location + 2) so as to avoid NO_LOCATION, which is
  // -1.
  encode-fixup_ location/int -> int:
    return -(location + 2)

  // It's perhaps not intuitive that the encoding and decoding functions are
  // identical, but they are both just mirroring around -1.
  decode-fixup_ encoded/int -> int:
    return -(encoded + 2)

  link codes/List/*<int>*/ -> none:
    value/int := location_
    if not is-bound: location_ = encode-fixup_ codes.size
    // If the label is bound, this writes the correct (positive) location.
    // Otherwise it writes the previous link in the chain of forward references
    // that need fixing when the label is bound.
    codes.add value

// Registers.
ZERO-REGISTER_        ::= 0
NO-POSITION-REGISTER_ ::= 1
CURRENT-POSITION_     ::= 2
STRING-LENGTH_        ::= 3
STACK-POINTER_        ::= 4
FIXED-REGISTERS_      ::= 5

REGISTER-NAMES_ ::= [
  "ZERO",
  "NO_POSITION_REGISTER",
  "CURRENT_POSITION",
  "STRING_LENGTH",
  "STACK_POINTER",
  "TEMP_0",
  "TEMP_1",
  "TEMP_2",
  "TEMP_3",
  "TEMP_4",
  "TEMP_5",
  "TEMP_6",
  "TEMP_7",
  "TEMP_8",
  "TEMP_9",
  "TEMP_10",
  "TEMP_11",
  "TEMP_12",
  "TEMP_13",
  "TEMP_14",
  "TEMP_15",
]

// Used for capture registers that have not captured any position in the
// string.
NO-POSITION_ ::= -1

// Byte codes.
BC-GOTO_                        ::= 0  // label
BC-PUSH-REGISTER_               ::= 1  // reg
BC-PUSH-BACKTRACK_              ::= 2  // const
BC-POP-REGISTER_                ::= 3  // reg
BC-BACKTRACK-EQ_                ::= 4  // reg reg
BC-BACKTRACK-NE_                ::= 5  // reg reg
BC-BACKTRACK-GT_                ::= 6  // reg reg
BC-BACKTRACK-IF-NO-MATCH_       ::= 7  // constant-pool-offset
BC-BACKTRACK-IF-IN-RANGE_       ::= 8  // from to
BC-GOTO-IF-MATCH_               ::= 9  // char_code label
BC-GOTO-IF-IN-RANGE_            ::= 10 // from to label
BC-GOTO-IF-UNICODE-IN-RANGE_    ::= 11 // from to label
BC-GOTO-EQ_                     ::= 12 // reg reg label
BC-GOTO-GE_                     ::= 13 // reg reg label
BC-GOTO-IF-WORD-CHARACTER_      ::= 14 // position-offset label
BC-ADD-TO-REGISTER_             ::= 15 // reg const
BC-ADVANCE-BY-RUNE-WIDTH_       ::= 16 // reg
BC-COPY-REGISTER_               ::= 17 // dest-reg source-reg
BC-BACKTRACK-ON-BACK-REFERENCE_ ::= 18 // capture-reg
BC-BACKTRACK_                   ::= 19
BC-SUCCEED_                     ::= 20
BC-FAIL_                        ::= 21

// Format is name, number of register arguments, number of other arguments.
BYTE-CODE-NAMES_ ::= [
  "GOTO", 0, 1,
  "PUSH_REGISTER", 1, 0,
  "PUSH_BACKTRACK", 0, 1,
  "POP_REGISTER", 1, 0,
  "BACKTRACK_EQ", 2, 0,
  "BACKTRACK_NE", 2, 0,
  "BACKTRACK_GT", 2, 0,
  "BACKTRACK_IF_NO_MATCH", 0, 1,
  "BACKTRACK_IF_IN_RANGE", 0, 2,
  "GOTO_IF_MATCH", 0, 2,
  "GOTO_IF_IN_RANGE", 0, 3,
  "GOTO_IF_UNICODE_IN_RANGE", 0, 3,
  "GOTO_EQ", 2, 1,
  "GOTO_GE", 2, 1,
  "GOTO_IF_WORD_CHARACTER", 0, 2,
  "ADD_TO_REGISTER", 1, 1,
  "ADVANCE_BY_RUNE_WIDTH", 1, 1,
  "COPY_REGISTER", 2, 0,
  "BACKTRACK_ON_BACK_REFERENCE", 1, 1,
  "BACKTRACK", 0, 0,
  "SUCCEED", 0, 0,
  "FAIL", 0, 0,
]

CHAR-CODE-NO-BREAK-SPACE_ ::= 0xa0
CHAR-CODE-OGHAM-SPACE-MARK_ ::= 0x1680
CHAR-CODE-EN-QUAD_ ::= 0x2000
CHAR-CODE-HAIR-SPACE_ ::= 0x200a
CHAR-CODE-LINE-SEPARATOR_ ::= 0x2028
CHAR-CODE-PARAGRAPH-SEPARATOR_ ::= 0x2029
CHAR-CODE-NARROW-NO-BREAK-SPACE_ ::= 0x202f
CHAR-CODE-MEDIUM-MATHEMATICAL-SPACE_ ::= 0x205f
CHAR-CODE-IDEOGRAPHIC-SPACE_ ::= 0x3000
CHAR-CODE-ZERO-WIDTH-NO-BREAK-SPACE_ ::= 0xfeff
CHAR-CODE-LAST_ ::= 0x10ffff

class MiniExpCompiler_:
  pattern /string ::= ?
  case-sensitive /bool ::= ?
  registers /List ::= []
  capture-register-count /int := 0
  first-capture-register /int := -1
  codes_ /List ::= []
  extra-constants_ /List ::= []
  back-references_ /List ::= []
  pending-goto_ /MiniExpLabel_? := null

  constructor .pattern .case-sensitive:
    for i := 0; i < FIXED-REGISTERS_; i++:
      registers.add (i == NO-POSITION-REGISTER_ ? NO-POSITION_ : 0)

  codes -> List:
    flush-pending-goto
    return codes_

  constant-pool -> ByteArray:
    if extra-constants_.is-empty:
      return pattern.to-byte-array
    else:
      byte-array := ByteArray extra-constants_.size: extra-constants_[it]
      return pattern.to-byte-array + byte-array

  // The rune for the constant pool object at the given index.
  constant-pool-entry index/int -> int:
    if index < pattern.size: return pattern[index]
    first-byte := extra-constants_[index - pattern.size]
    if first-byte < 0x80: return first-byte
    byte-count := UTF-FIRST-CHAR-TABLE_[first-byte >> 4]
    result := first-byte & 0x1f
    if byte-count == 4: result &= 7
    (byte-count - 1).repeat:
      result <<= 6
      result |= extra-constants_[index + it] & 0x3f
    return result

  constant-pool-byte index/int -> int:
    if index < pattern.size: return pattern.at --raw index
    return extra-constants_[index - pattern.size]

  // Indexed by the top nibble of a UTF-8 byte this tells you how many bytes
  // long the UTF-8 sequence is.
  static UTF-FIRST-CHAR-TABLE_ ::= [
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 2, 2, 3, 4,
  ]

  emit_ code/int arg1/int?=null arg2/int?=null -> none:
    flush-pending-goto
    codes_.add code
    if arg1 != null: codes_.add arg1
    if arg2 != null: codes_.add arg2

  generate ast/MiniExpAst_ on-success/MiniExpLabel_ -> none:
    bind ast.label
    ast.generate this on-success

  bind label/MiniExpLabel_ -> none:
    if label == pending-goto_:
      pending-goto_ = null  // Peephole optimization.
    flush-pending-goto
    label.bind codes_

  link label/MiniExpLabel_ -> none: label.link codes_

  succeed -> none: emit_ BC-SUCCEED_

  fail -> none: emit_ BC-FAIL_

  allocate-working-register -> int: return allocate-constant-register 0

  allocate-constant-register value/int -> int:
    register := registers.size
    registers.add value
    return register

  // Returns negative numbers, starting at -1. This is so that we can
  // interleave allocation of capture registers and regular registers, but
  // still end up with the capture registers being contiguous.
  allocate-capture-registers -> int:
    capture-register-count += 2
    return -capture-register-count + 1

  add-back-reference b/BackReference_ -> none:
    back-references_.add b

  add-capture-registers -> none:
    first-capture-register = registers.size
    for i := 0; i < capture-register-count; i++:
      registers.add NO-POSITION_
    process-back-refences

  process-back-refences -> none:
    back-references_.do: | b/BackReference_ |
      // 1-based index (you can't refer back to capture zero).
      numeric-index := int.parse b.index
      if b.index[0] == '0' or numeric-index * 2 >= capture-register-count:
        // Web compatible strangeness - if the index is more than the number of
        // captures it turns into an octal character code escape.
        code-unit := 0
        octals-found := 0
        replace /MiniExpAst_? := null
        non-octals-found := false
        // The first 0-3 octal digits form an octal character escape, the rest
        // are literals.
        b.index.do --runes: | octal-digit |
          if (not non-octals-found) and
              '0' <= octal-digit <= '7' and
              code-unit * 8 < 0x100 and octals-found < 3:
            code-unit *= 8
            code-unit += octal-digit - '0'
            octals-found++
          else:
            pool-index := add-to-constant-pool octal-digit
            atom /MiniExpAst_ := Atom_ pool-index
            replace = (replace == null) ? atom : Alternative_ replace atom
            non-octals-found = true
        if octals-found != 0:
          pool-index := add-to-constant-pool code-unit
          atom /MiniExpAst_ := Atom_ pool-index
          replace = (replace == null) ? atom : Alternative_ atom replace
        b.replace-with-ast replace
      else:
        b.register = first-capture-register + numeric-index * 2

  // Raw register numbers are negative for capture registers, positive for
  // constant and working registers.
  register-number raw-register-number/int -> int:
    if raw-register-number >= 0: return raw-register-number
    return -(raw-register-number + 1) + first-capture-register

  add-to-constant-pool rune/int -> int:
    result := pattern.size + extra-constants_.size
    if rune < 0x80:
      extra-constants_.add rune
      return result
    str := string.from-runes [rune]
    str.size.repeat:
      extra-constants_.add (str.at --raw it)
    return result

  push-backtrack label/MiniExpLabel_ -> none:
    emit_ BC-PUSH-BACKTRACK_
    link label

  backtrack -> none:
    emit_ BC-BACKTRACK_

  push reg/int -> none:
    emit_ BC-PUSH-REGISTER_
        register-number reg

  pop reg/int -> none:
    emit_ BC-POP-REGISTER_
        register-number reg

  goto label/MiniExpLabel_ -> none:
    if pending-goto_ != label: flush-pending-goto
    pending-goto_ = label

  flush-pending-goto -> none:
    if pending-goto_ != null:
      codes_.add BC-GOTO_
      link pending-goto_
      pending-goto_ = null

  backtrack-if-equal register1/int register2/int -> none:
    emit_ BC-BACKTRACK-EQ_
         register-number register1
         register-number register2

  backtrack-if-not-equal register1/int register2/int -> none:
    emit_ BC-BACKTRACK-NE_
        register-number register1
        register-number register2

  add-to-register reg/int offset/int -> none:
    emit_ BC-ADD-TO-REGISTER_
        register-number reg
        offset

  copy-register dest-register/int source-register/int -> none:
    emit_ BC-COPY-REGISTER_
        register-number dest-register
        register-number source-register

  backtrack-on-back-reference-fail register/int case-sensitive/bool -> none:
    emit_ BC-BACKTRACK-ON-BACK-REFERENCE_
        register-number register
        case-sensitive ? 1 : 0

  backtrack-if-greater register1/int register2/int -> none:
    emit_ BC-BACKTRACK-GT_
        register-number register1
        register-number register2

  goto-if-greater-equal register1/int register2/int label/MiniExpLabel_ -> none:
    emit_ BC-GOTO-GE_
        register-number register1
        register-number register2
    link label

  backtrack-if-no-match constant-pool-offset/int -> none:
    emit_ BC-BACKTRACK-IF-NO-MATCH_ constant-pool-offset

  backtrack-if-in-range from/int to/int -> none:
    emit_ BC-BACKTRACK-IF-IN-RANGE_ from to

  goto-if-matches char-code/int label/MiniExpLabel_ -> none:
    emit_ BC-GOTO-IF-MATCH_  char-code
    link label

  goto-if-in-range from/int to/int label/MiniExpLabel_ -> none:
    if from == to:
      goto-if-matches from label
    else:
      emit_ BC-GOTO-IF-IN-RANGE_ from to
      link label

  goto-if-unicode-in-range from/int to/int label/MiniExpLabel_ -> none:
    emit_ BC-GOTO-IF-UNICODE-IN-RANGE_ from to
    link label

  // Adds a number of UTF-8 encoded runes to the current position, and stores
  // the result in the given destination register.  Can also step backwards.
  advance-by-rune-width destination-register/int count/int -> none:
    emit_ BC-ADVANCE-BY-RUNE-WIDTH_ destination-register count

  backtrack-if-not-at-word-boundary left/bool right/bool -> none:
    non-word-on-left := MiniExpLabel_
    word-on-left := MiniExpLabel_
    at-word-boundary := MiniExpLabel_
    do-backtrack := MiniExpLabel_

    if right and left:
      emit_ BC-GOTO-EQ_ CURRENT-POSITION_ ZERO-REGISTER_
      link non-word-on-left
      emit_ BC-GOTO-IF-WORD-CHARACTER_ -1
      link word-on-left

      bind non-word-on-left
      emit_ BC-BACKTRACK-EQ_ CURRENT-POSITION_ STRING-LENGTH_
      emit_ BC-GOTO-IF-WORD-CHARACTER_ 0
      link at-word-boundary
      bind do-backtrack
      backtrack

      bind word-on-left
      emit_ BC-GOTO-EQ_ CURRENT-POSITION_ STRING-LENGTH_
      link at-word-boundary
      emit_ BC-GOTO-IF-WORD-CHARACTER_ 0
      link do-backtrack
    else if left:
      emit_ BC-GOTO-EQ_ CURRENT-POSITION_ ZERO-REGISTER_
      link non-word-on-left
      emit_ BC-GOTO-IF-WORD-CHARACTER_ -1
      link do-backtrack

      bind non-word-on-left
      emit_ BC-BACKTRACK-EQ_ CURRENT-POSITION_ STRING-LENGTH_
      emit_ BC-GOTO-IF-WORD-CHARACTER_ 0
      link at-word-boundary
      bind do-backtrack
      backtrack
    else if right:
      emit_ BC-GOTO-EQ_ CURRENT-POSITION_ ZERO-REGISTER_
      link do-backtrack
      emit_ BC-GOTO-IF-WORD-CHARACTER_ -1
      link word-on-left

      bind do-backtrack
      backtrack

      bind word-on-left
      emit_ BC-GOTO-EQ_ CURRENT-POSITION_ STRING-LENGTH_
      link at-word-boundary
      emit_ BC-GOTO-IF-WORD-CHARACTER_ 0
      link do-backtrack

    bind at-word-boundary

// MiniExpAnalysis_ objects reflect properties of an AST subtree.  They are
// immutable and are reused to some extent.
class MiniExpAnalysis_:
  // Can this subtree match an empty string?  If we know that's not possible,
  // we can optimize away the test that ensures we are making progress when we
  // match repetitions.
  can-match-empty /bool ::= ?

  // Set to null if the AST does not match something with a fixed length.  That
  // fixed length thing has to be something that does not touch the stack.  This
  // is an important optimization that prevents .* from using huge amounts of
  // stack space when running.
  fixed-length /int? ::= ?

  // Can this subtree only match at the start of the regexp?  Can't pass all
  // tests without being able to spot this.
  anchored /bool ::= ?

  // Allows the AST to notify a surrounding loop (a quantifier higher up the
  // tree) that it has registers it expects to be saved on the back edge.
  registers-to-save/List?/*<int>*/ ::= ?

  static combine-registers left/List?/*<int>*/ right/List?/*<int>*/ -> List?/*<int>*/:
    if right == null or right.is-empty:
      return left
    else if left == null or left.is-empty:
      return right
    else:
      return left + right  // List concatenation.

  static combine-fixed-lengths left/MiniExpAnalysis_ right/MiniExpAnalysis_ -> int?:
    if left.fixed-length == null or right.fixed-length == null:
      return null
    else:
      return left.fixed-length + right.fixed-length

  constructor.orr left/MiniExpAnalysis_ right/MiniExpAnalysis_:
    can-match-empty = left.can-match-empty or right.can-match-empty
    // Even if both alternatives are the same length we can't handle a
    // disjunction without pushing backtracking information on the stack.
    fixed-length = null
    anchored = left.anchored and right.anchored
    registers-to-save = combine-registers left.registers-to-save right.registers-to-save

  constructor.andd left/MiniExpAnalysis_ right/MiniExpAnalysis_:
    can-match-empty = left.can-match-empty and right.can-match-empty
    fixed-length = combine-fixed-lengths left right
    anchored = left.anchored
    registers-to-save = combine-registers left.registers-to-save right.registers-to-save

  constructor.empty:
    can-match-empty = true
    fixed-length = 0
    anchored = false
    registers-to-save = null

  constructor.at-start:
    can-match-empty = true
    fixed-length = 0
    anchored = true
    registers-to-save = null

  constructor.lookahead body-analysis/MiniExpAnalysis_ positive/bool:
    can-match-empty = true
    fixed-length = 0
    anchored = positive and body-analysis.anchored
    registers-to-save = body-analysis.registers-to-save

  constructor.quantifier body-analysis/MiniExpAnalysis_ min/int max/int? regs/List/*<int>*/:
    can-match-empty = min == 0 or body-analysis.can-match-empty
    fixed-length = (min == 1 and max == 1) ? body-analysis.fixed-length : null
    anchored = min > 0 and body-analysis.anchored
    registers-to-save = combine-registers body-analysis.registers-to-save regs

  constructor.atom:
    can-match-empty = false
    fixed-length = 1
    anchored = false
    registers-to-save = null

  constructor.know-nothing:
    can-match-empty = true
    fixed-length = null
    anchored = false
    registers-to-save = null

  constructor.capture body-analysis/MiniExpAnalysis_ start/int end/int:
    can-match-empty = body-analysis.can-match-empty
    // We can't generate a capture without pushing backtracking information
    // on the stack.
    fixed-length = null
    anchored = body-analysis.anchored
    registers-to-save = combine-registers body-analysis.registers-to-save [start, end]

abstract class MiniExpAst_:
  // When generating code for an AST, note that:
  // * The previous code may fall through to this AST, but it might also
  //   branch to it.  The label has always been bound just before generate
  //   is called.
  // * It's not permitted to fall through to the bottom of the generated
  //   code. Always end with backtrack or a goto on_success.
  // * You can push any number of backtrack pairs (PC, position), but if you
  //   push anything else, then you have to push a backtrack location that will
  //   clean it up.  On entry you can assume there is a backtrack pair on the
  //   top of the stack.
  abstract generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none

  abstract analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_

  // Label is bound at the entry point for the AST tree.
  label ::= MiniExpLabel_

  abstract case-expand compiler/MiniExpCompiler_ -> MiniExpAst_

class Disjunction_ extends MiniExpAst_:
  left_/MiniExpAst_ ::= ?
  right_/MiniExpAst_ ::= ?

  constructor .left_ .right_:

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    try-right := MiniExpLabel_
    compiler.push-backtrack try-right
    compiler.generate left_ on-success
    compiler.bind try-right
    compiler.generate right_ on-success

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.orr
        left_.analyze compiler
        right_.analyze compiler

  case-expand compiler/MiniExpCompiler_ -> Disjunction_:
    l := left_.case-expand compiler
    r := right_.case-expand compiler
    if l == left_ and r == right_: return this
    return Disjunction_ l r

class EmptyAlternative_ extends MiniExpAst_:
  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.goto on-success

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.empty

  case-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    return this

class Alternative_ extends MiniExpAst_:
  left_/MiniExpAst_ ::= ?
  right_/MiniExpAst_ ::= ?

  constructor .left_ .right_:

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.generate left_ right_.label
    compiler.generate right_ on-success

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.andd
        left_.analyze compiler
        right_.analyze compiler

  case-expand compiler/MiniExpCompiler_ -> Alternative_:
    l := left_.case-expand compiler
    r := right_.case-expand compiler
    if l == left_ and r == right_: return this
    return Alternative_ l r

abstract class Assertion extends MiniExpAst_:
  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.empty

  case-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    return this

class AtStart_ extends Assertion:
  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.backtrack-if-not-equal CURRENT-POSITION_ ZERO-REGISTER_
    compiler.goto on-success

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.at-start

class AtEnd_ extends Assertion:
  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.backtrack-if-not-equal CURRENT-POSITION_ STRING-LENGTH_
    compiler.goto on-success

abstract class MultiLineAssertion_ extends Assertion:
  backtrack-if-not-newline compiler/MiniExpCompiler_ -> none:
    compiler.backtrack-if-in-range
        '\r' + 1
        CHAR-CODE-LINE-SEPARATOR_ - 1
    compiler.backtrack-if-in-range
        0
        '\n' - 1
    compiler.backtrack-if-in-range 
        '\n' + 1
        '\r' - 1
    compiler.backtrack-if-in-range
        CHAR-CODE-PARAGRAPH-SEPARATOR_ + 1
        CHAR-CODE-LAST_

class AtBeginningOfLine_ extends MultiLineAssertion_:
  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.goto-if-greater-equal ZERO-REGISTER_ CURRENT-POSITION_ on-success
    // We need to look one back to see if there was a newline there.  If we
    // backtrack, then that also restores the current position, but if we don't
    // backtrack, we have to fix it again.
    compiler.add-to-register CURRENT-POSITION_ -1
    backtrack-if-not-newline compiler
    compiler.add-to-register CURRENT-POSITION_ 1
    compiler.goto on-success

class AtEndOfLine_ extends MultiLineAssertion_:
  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.goto-if-greater-equal CURRENT-POSITION_ STRING-LENGTH_ on-success
    backtrack-if-not-newline compiler
    compiler.goto on-success

class WordBoundary_ extends Assertion:
  positive_ /bool ::= ?
  left_ /bool ::= ?
  right_ /bool ::= ?

  constructor .positive_ .left_ .right_:

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    // Positive word boundaries are much more common that negative ones, so we
    // will allow ourselves to generate some pretty horrible code for the
    // negative ones.
    if not positive_:
      assert: left_ and right_  // No syntax for asymmetric negative.
      compiler.push-backtrack on-success
    compiler.backtrack-if-not-at-word-boundary left_ right_
    if positive_:
      compiler.goto on-success
    else:
      // Pop the two stack position of the unneeded backtrack.
      compiler.pop CURRENT-POSITION_
      compiler.pop CURRENT-POSITION_
      // This overwrites the current position with the correct value.
      compiler.backtrack

class LookAhead_ extends Assertion:
  positive_ /bool ::= ?
  body_/MiniExpAst_ ::= ?
  subtree-registers_/List?/*<int>*/ := null

  saved-stack-pointer-register_/int
  saved-position_/int

  constructor .positive_ .body_ compiler/MiniExpCompiler_:
    saved-stack-pointer-register_ = compiler.allocate-working-register
    saved-position_ = compiler.allocate-working-register

  constructor.private_ .positive_ .body_ .saved-stack-pointer-register_ .saved-position_:

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    // Lookahead.  Even if the subexpression succeeds, the current position is
    // reset, and the backtracking stack is unwound so that we can never
    // backtrack into the lookahead.  On a failure of the subexpression, the
    // stack will be naturally unwound.
    body-succeeded := MiniExpLabel_
    succeed-on-failure := MiniExpLabel_
    undo-captures/MiniExpLabel_? := null
    compiler.copy-register saved-stack-pointer-register_ STACK-POINTER_
    compiler.copy-register saved-position_ CURRENT-POSITION_
    if not positive_:
      compiler.push-backtrack succeed-on-failure
    compiler.generate body_ body-succeeded

    compiler.bind body-succeeded
    compiler.copy-register STACK-POINTER_ saved-stack-pointer-register_
    compiler.copy-register CURRENT-POSITION_ saved-position_
    if not positive_:
      // For negative lookahead always zap the captures when the body succeeds
      // and the lookahead thus fails.  The captures are only needed for any
      // backrefs inside the negative lookahead.
      if subtree-registers_ != null:
        subtree-registers_.do: | register |
          compiler.copy-register register NO-POSITION-REGISTER_
      compiler.backtrack
      compiler.bind succeed-on-failure
    else:
      // For positive lookahead, the backtrack stack has been unwound, because
      // we don't ever backtrack into a lookahead, but if we backtrack past
      // this point we have to undo any captures that happened in there.
      // Register a backtrack to do that before continuing.
      if subtree-registers_ != null and not subtree-registers_.is-empty:
        undo-captures = MiniExpLabel_
        compiler.push-backtrack undo-captures

    compiler.goto on-success

    if undo-captures != null:
      compiler.bind undo-captures
      subtree-registers_.do: | register |
        compiler.copy-register register NO-POSITION-REGISTER_
      compiler.backtrack

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    body-analysis/MiniExpAnalysis_ := body_.analyze compiler
    subtree-registers_ = body-analysis.registers-to-save
    return MiniExpAnalysis_.lookahead body-analysis positive_

  case-expand compiler/MiniExpCompiler_ -> LookAhead_:
    b := body_.case-expand compiler
    if b == body_: return this
    return LookAhead_.private_ positive_ b saved-stack-pointer-register_ saved-position_

class Quantifier_ extends MiniExpAst_:
  min_ /int? ::= ?
  max_ /int? ::= ?
  greedy_ /bool ::= ?
  body_/MiniExpAst_ ::= ?
  counter-register_/int? := null
  start-of-match-register_/int? := null  // Implements 21.2.2.5.1 note 4.
  min-register_/int? := null
  max-register_/int? := null
  subtree-registers-that-need-saving_/List?/*<int>*/ := null
  optimized-greedy-register_/int? := null
  save-position-register_/int? := null
  body-length_/int? := null

  is-optimized-greedy_ -> bool: return optimized-greedy-register_ != null

  constructor
      --min /int?
      --max /int?
      --greedy /bool
      --body /MiniExpAst_
      --compiler /MiniExpCompiler_:
    min_ = min
    max_ = max
    greedy_ = greedy
    body_ = body
    if counter-check_:
      counter-register_ = compiler.allocate-working-register
      min-register_ = (min-check_ min_) ? (compiler.allocate-constant-register min_) : null
      max-register_ = (max-check_ max_) ? (compiler.allocate-constant-register max_) : null

  constructor.private_
      --min /int?
      --max /int?
      --greedy /bool
      --body /MiniExpAst_
      --counter-register /int?
      --start-of-match-register /int?
      --min-register /int?
      --max-register /int?
      --subtree-registers-that-need-saving /List?
      --optimized-greedy-register /int?
      --save-position-register /int?
      --body-length /int?:
    min_ = min
    max_ = max
    greedy_ = greedy
    body_ = body
    counter-register_ = counter-register
    start-of-match-register_ = start-of-match-register
    min-register_ = min-register
    max-register_ = max-register
    subtree-registers-that-need-saving_ = subtree-registers-that-need-saving
    optimized-greedy-register_ = optimized-greedy-register
    save-position-register_ = save-position-register
    body-length_ = body-length

  // We fall through to the top of this, when it is time to match the body of
  // the quantifier.  If the body matches successfully, we should go to
  // on_body_success, otherwise clean up and backtrack.
  generate-common compiler/MiniExpCompiler_ on-body-success/MiniExpLabel_ -> none:
    need-to-catch-didnt-match/bool := greedy_ or body-can-match-empty_ or
        counter-check_ or save-and-restore-registers_
    didnt-match := MiniExpLabel_

    if save-and-restore-registers_:
      subtree-registers-that-need-saving_.do: | reg |
        compiler.push reg
        compiler.copy-register reg NO-POSITION-REGISTER_

    if body-can-match-empty_:
      compiler.push start-of-match-register_
      compiler.copy-register start-of-match-register_ CURRENT-POSITION_

    if need-to-catch-didnt-match:
      compiler.push-backtrack didnt-match

    if counter-check_:
      compiler.add-to-register counter-register_ 1
      if max-check_ max_:
        compiler.backtrack-if-greater counter-register_ max-register_

    compiler.generate body_ on-body-success

    if need-to-catch-didnt-match:
      compiler.bind didnt-match
      if body-can-match-empty_:
        compiler.pop start-of-match-register_
      if counter-check_:
        compiler.add-to-register counter-register_ -1
      if save-and-restore-registers_:
        for i := subtree-registers-that-need-saving_.size - 1; i >= 0; i--:
          compiler.pop subtree-registers-that-need-saving_[i]
      if not greedy_: compiler.backtrack

  generate-fixed-length-greedy compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    new-iteration := MiniExpLabel_
    cant-advance-more := MiniExpLabel_
    continuation-failed := MiniExpLabel_
    after-cant-advance-more := MiniExpLabel_

    // Save the current position, so we know when the quantifier has been
    // unwound enough.
    compiler.copy-register optimized-greedy-register_ CURRENT-POSITION_
    if min_ != 0:
      assert: min_ < 3 and body-length_ < 3
      // Move the unwound-enough register forward by enough characters to cover
      // the minimum repetitions.  This doesn't actually check the characters.
      // It can advance beyond the end of the subject!
      compiler.advance-by-rune-width optimized-greedy-register_ (min_ * body-length_)

    // This backtrack doesn't trigger until the quantifier has eaten as much as
    // possible.  Unfortunately, whenever we backtrack in this simple system,
    // the old current position in the subject string is also rewound.  That's
    // not so convenient here, so we will have to save the current position in
    // a special register.  It would be faster to generate the body in a
    // special mode where we use a different backtrack instruction that just
    // rewinds the current position a certain number of characters instead of
    // popping it.
    compiler.push-backtrack cant-advance-more  // Also pushes the current position, which we don't need.

    // The loop.
    compiler.bind new-iteration
    compiler.copy-register save-position-register_ CURRENT-POSITION_
    compiler.generate body_ new-iteration

    // The greedy quantifier has eaten as much as it can.  Time to try the
    // continuation of the regexp after the quantifier.
    compiler.bind cant-advance-more  // Also restores the original position, which is wrong.
    compiler.copy-register CURRENT-POSITION_ save-position-register_  // Fix the position.

    if min_ != 0:
      // We may have to backtrack because we didn't get the minimum number of
      // matches.
      compiler.backtrack-if-greater optimized-greedy-register_ CURRENT-POSITION_

    compiler.goto after-cant-advance-more

    // The continuation of the regexp failed.  We backtrack the greedy
    // quantifier by one step and retry.
    compiler.bind continuation-failed
    compiler.advance-by-rune-width CURRENT-POSITION_ -body-length_
    // If we got back to where the quantifier started matching, then jump
    // to the continuation (we haven't pushed a backtrack, so if that fails, it
    // will backtrack further).
    // We don't have goto_if_equal, so use goto_if_greater_equal.
    compiler.bind after-cant-advance-more
    compiler.goto-if-greater-equal optimized-greedy-register_ CURRENT-POSITION_ on-success
    compiler.push-backtrack continuation-failed
    compiler.goto on-success

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    // We optimize loops of the form .* to avoid big backtrack stacks.
    if is-optimized-greedy_:
      generate-fixed-length-greedy compiler on-success
      return
    if min_ == 1 and max_ == 1:
      compiler.generate body_ on-success
      return
    // The above means if max_ is 1 then min_ must be 0, which simplifies
    // things.
    body-matched/MiniExpLabel_ := max_ == 1 ? on-success : MiniExpLabel_
    check-empty-match-label/MiniExpLabel_? := null
    on-body-success/MiniExpLabel_ := body-matched
    if body-can-match-empty_:
      check-empty-match-label = MiniExpLabel_
      on-body-success = check-empty-match-label
    if counter-check_:
      compiler.copy-register counter-register_ ZERO-REGISTER_

    if body-matched != on-success:
      compiler.bind body-matched

    if greedy_:
      generate-common compiler on-body-success

      if min-check_ min_:
        compiler.goto-if-greater-equal counter-register_ min-register_ on-success
        compiler.backtrack
      else:
        compiler.goto on-success
    else:
      // Non-greedy.
      try-body := MiniExpLabel_

      if min-check_ min_:
        // If there's a minimum and we haven't reached it we should not try to
        // run the continuation, but go straight to the body_.
        // TODO(erikcorry): if we had a goto_if_less we could save instructions
        // here.
        jump-to-continuation := MiniExpLabel_
        compiler.goto-if-greater-equal counter-register_ min-register_ jump-to-continuation
        compiler.goto try-body
        compiler.bind jump-to-continuation
      // If the continuation fails, we can try the body_ once more.
      compiler.push-backtrack try-body
      compiler.goto on-success

      // We failed to match the continuation, so lets match the body_ once more
      // and then try again.
      compiler.bind try-body
      generate-common compiler on-body-success

    if body-can-match-empty_:
      compiler.bind check-empty-match-label
      if min-check_ min_:
        compiler.goto-if-greater-equal min-register_ counter-register_ body-matched
      compiler.backtrack-if-equal start-of-match-register_ CURRENT-POSITION_
      compiler.goto body-matched

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    body-analysis/MiniExpAnalysis_ := body_.analyze compiler
    subtree-registers-that-need-saving_ = body-analysis.registers-to-save
    body-length_ = body-analysis.fixed-length
    if body-analysis.can-match-empty:
      start-of-match-register_ = compiler.allocate-working-register
    // Some limitations to prevent min * body_length being too high.
    else if max_ == null and greedy_ and body-length_ != null and (min_ == null or min_ < 3) and (body-length_ < 3 or min_ == null or min_ == 0):
      // This also put us in a mode where code is generated differently for
      // this AST.
      optimized-greedy-register_ = compiler.allocate-working-register
      save-position-register_ = compiler.allocate-working-register
    my-regs/List/*<int>*/ := [counter-register_, optimized-greedy-register_, save-position-register_].filter: it != null
    return MiniExpAnalysis_.quantifier body-analysis min_ max_ my-regs

  static max-check_ max -> bool: return max != 1 and max != null

  static min-check_ min -> bool: return min != 0

  counter-check_ -> bool: return (max-check_ max_) or (min-check_ min_)

  body-can-match-empty_ -> bool: return start-of-match-register_ != null

  save-and-restore-registers_ -> bool:
    return subtree-registers-that-need-saving_ != null and
           not subtree-registers-that-need-saving_.is-empty

  case-expand compiler/MiniExpCompiler_ -> Quantifier_:
    b := body_.case-expand compiler
    if b == body_: return this
    return Quantifier_.private_
        --min=min_
        --max=max_
        --greedy=greedy_
        --body=b
        --counter-register=counter-register_
        --start-of-match-register=start-of-match-register_
        --min-register=min-register_
        --max-register=max-register_
        --subtree-registers-that-need-saving=subtree-registers-that-need-saving_
        --optimized-greedy-register=optimized-greedy-register_
        --save-position-register=save-position-register_
        --body-length=body-length_

class Atom_ extends MiniExpAst_:
  constant-index_ /int ::= ?

  constructor .constant-index_:

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    code-unit/int := compiler.constant-pool-byte constant-index_
    compiler.backtrack-if-equal CURRENT-POSITION_ STRING-LENGTH_
    compiler.backtrack-if-no-match constant-index_
    compiler.add-to-register CURRENT-POSITION_ 1
    compiler.goto on-success

  case-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    if compiler.case-sensitive:
      return utf-expand compiler
    char-code/int := compiler.constant-pool-entry constant-index_
    equivalence := case.reg-exp-equivalence-class char-code
    if not equivalence or equivalence.size == 1:
      return utf-expand compiler
    // Need to expand to multiple characters.
    result := Disjunction_
        (Atom_ (compiler.add-to-constant-pool equivalence[0])).utf-expand compiler
        (Atom_ (compiler.add-to-constant-pool equivalence[1])).utf-expand compiler
    if equivalence.size > 2:
      result = Disjunction_
          result
          (Atom_ (compiler.add-to-constant-pool equivalence[2])).utf-expand compiler
    assert: equivalence <= 3
    return result

  utf-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    first-byte := compiler.constant-pool-byte constant-index_
    byte-count := MiniExpCompiler_.UTF-FIRST-CHAR-TABLE_[first-byte >> 4]
    if byte-count == 1: return this
    result := Alternative_
        Atom_ constant-index_
        Atom_ constant-index_ + 1
    if byte-count >= 3:
      result = Alternative_
          result
          Atom_ constant-index_ + 2
    if byte-count == 4:
      result = Alternative_
          result
          Atom_ constant-index_ + 3
    return result

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.atom

class Range_:
  from /int := 0
  to   /int := 0

  constructor .from .to:

  stringify -> string:
    return "$from-$to"

class CharClass_ extends MiniExpAst_:
  ranges_ /List ::= []
  positive_ /bool ::= ?

  constructor .positive_:

  constructor.private_ .ranges_ .positive_:

  // Here and elsewhere, "to" is inclusive.
  add from/int to/int -> none:
    ranges_.add
        Range_ from to

  static space-codes_/List/*<int>*/ := [
    -1,
    '\t', '\r',
    ' ', ' ',
    CHAR-CODE-NO-BREAK-SPACE_, CHAR-CODE-NO-BREAK-SPACE_,
    CHAR-CODE-OGHAM-SPACE-MARK_, CHAR-CODE-OGHAM-SPACE-MARK_,
    CHAR-CODE-EN-QUAD_, CHAR-CODE-HAIR-SPACE_,
    CHAR-CODE-LINE-SEPARATOR_, CHAR-CODE-PARAGRAPH-SEPARATOR_,
    CHAR-CODE-NARROW-NO-BREAK-SPACE_, CHAR-CODE-NARROW-NO-BREAK-SPACE_,
    CHAR-CODE-MEDIUM-MATHEMATICAL-SPACE_, CHAR-CODE-MEDIUM-MATHEMATICAL-SPACE_,
    CHAR-CODE-IDEOGRAPHIC-SPACE_, CHAR-CODE-IDEOGRAPHIC-SPACE_,
    CHAR-CODE-ZERO-WIDTH-NO-BREAK-SPACE_, CHAR-CODE-ZERO-WIDTH-NO-BREAK-SPACE_,
    CHAR-CODE-LAST_]

  add-spaces -> none:
    for i := 1; i < space-codes_.size - 1; i += 2:
      add space-codes_[i] space-codes_[i + 1]

  add-not-spaces -> none:
    for i := 0; i < space-codes_.size; i += 2:
      add space-codes_[i] + 1
          space-codes_[i + 1] - 1

  add-special char-code/int -> none:
    if char-code == 'd':
      add '0' '9'
    else if char-code == 'D':
      add 0
          '0' - 1
      add '9' + 1
          CHAR-CODE-LAST_
    else if char-code == 's':
      add-spaces
    else if char-code == 'S':
      add-not-spaces
    else if char-code == 'w':
      add '0' '9'
      add 'A' 'Z'
      add '_' '_'
      add 'a' 'z'
    else if char-code == 'W':
      add 0
          '0' - 1 
      add '9' + 1
          'A' - 1
      add 'Z' + 1
          '_' - 1
      add '_' + 1
          'a' - 1
      add 'z' + 1
          CHAR-CODE-LAST_

  fix-ranges ranges/List/*<Range_>*/ case-sensitive/bool -> List/*<Range_>*/:
    // There's a lot of punctuation and no case-sensitive characters before the
    // Unicode 'A' code point.
    ranges = ranges.copy
    if not case-sensitive and (ranges.any: it.to >= 'A'):
      old-size := ranges.size
      range-start := -1
      range-end := -1
      for i := 0; i < old-size; i++:
        range := ranges[i]
        // For each character, eg. in the range a-f, we need to find the
        // equivalent characters, eg. A-F, and add them to the character class.
        for j := range.from; j <= range.to; j++:
          // Get the characters that are equivalent to the current one.
          // For example, if j == 'A' we would expect to get ['a', 'A'] or
          // ['A', 'a'].  A reply of null means there the character is only
          // case-equivalent to itself.  There are never more than three
          // case equivalents.
          equivalents := case.reg-exp-equivalence-class j
          if equivalents and equivalents.size != 1:
            assert: equivalents.contains j
            if range-start >= 0 and equivalents.size == 2 and equivalents.contains range-end + 1 and j != range-end + 1:
              // There's just one case-equivalent character (other than j) and
              // it's the one we were expecting, so we merely need to extend
              // the range by one to cover the new character.
              // (We exclude the theoretical case where j is identical to the
              // range extension, because in that case we would not record the
              // other equivalent character.)
              range-end++
            else:
              // Flush the existing range of equivalents that we were
              // constructing.
              if range-start >= 0:
                ranges.add (Range_ range-start range-end)
                range-start = -1
              if equivalents.size > 2:
                // If there are > 2 equivalents, just add them all to the list
                // of ranges.
                equivalents.do: | equivalent |
                  ranges.add (Range_ equivalent equivalent)
              else:
                // Since there are two equivalents, and one of them is the
                // character we already have in a range, start constructing a
                // new range of equivalents.
                equivalents.do: | equivalent |
                  if equivalent != j:
                    range-start = range-end = equivalent
        // At the end, flush the equivalent range we were constructing.
        if range-start >= 0:
          ranges.add (Range_ range-start range-end)
          range-start = -1

    // Even if there is no case-independence work to do we still want to
    // consolidate ranges.
    ranges.sort --in-place: | a b | a.from - b.from

    // Now that they are sorted, check if it's worth merging
    // adjacent/overlapping ranges.
    for i := 1; i < ranges.size; i++:
      if ranges[i - 1].to + 1 >= ranges[i].from:
        ranges = merge-adjacent-ranges_ ranges
        break

    return ranges

  // Merge adjacent/overlapping ranges.
  merge-adjacent-ranges_ ranges/List -> List:
    last := ranges[0]
    new-ranges := [last]
    ranges.do: | range |
      if last.to + 1 >= range.from:
        last.to = max last.to range.to
      else:
        last = range
        new-ranges.add last
    return new-ranges

  case-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    ranges := fix-ranges ranges_ compiler.case-sensitive
    return CharClass_.private_ ranges positive_

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    compiler.backtrack-if-equal CURRENT-POSITION_ STRING-LENGTH_
    match := MiniExpLabel_
    match-unicode := MiniExpLabel_
    backtrack-on-in-range := MiniExpLabel_
    ranges_.do: | range |
      bytes := utf-8-bytes range.to
      if positive_:
        if bytes == 1:
          compiler.goto-if-in-range range.from range.to match
        else:
          compiler.goto-if-unicode-in-range range.from range.to match-unicode
      else:
        if bytes == 1:
          compiler.backtrack-if-in-range range.from range.to
        else:
          compiler.goto-if-unicode-in-range range.from range.to backtrack-on-in-range
    if positive_:
      compiler.backtrack
      if match-unicode.is-linked:
        compiler.bind match-unicode
        compiler.advance-by-rune-width CURRENT-POSITION_ 1
        compiler.goto on-success
      compiler.bind match
      compiler.add-to-register CURRENT-POSITION_ 1
      compiler.goto on-success
    else:
      compiler.advance-by-rune-width CURRENT-POSITION_ 1
      compiler.goto on-success
      if backtrack-on-in-range.is-linked:
        compiler.bind backtrack-on-in-range
        compiler.backtrack

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    return MiniExpAnalysis_.atom

// This class is used for all backslashes followed by numbers.  For web
// compatibility, if the number (interpreted as decimal) is smaller than the
// number of captures, then it will be interpreted as a back reference.
// Otherwise the number will be interpreted as an octal character code escape.
class BackReference_ extends MiniExpAst_:
  back-reference-index_/string
  register_/int? := null
  ast-that-replaces-us_/MiniExpAst_? := null

  constructor .back-reference-index_:

  index -> string: return back-reference-index_

  register= r/int -> none:
    register_ = r

  replace-with-ast ast/MiniExpAst_ -> none:
    ast-that-replaces-us_ = ast

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    if register_ == null:
      compiler.generate ast-that-replaces-us_ on-success
    else:
      compiler.backtrack-on-back-reference-fail register_ compiler.case-sensitive
      compiler.goto on-success

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    compiler.add-back-reference this
    return MiniExpAnalysis_.know-nothing

  case-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    return this

class Capture_ extends MiniExpAst_:
  capture-count_ /int ::= ?
  body_/MiniExpAst_ ::= ?
  start-register_/int? := null
  end-register_/int? := null

  constructor .capture-count_ .body_:

  constructor.private_ .capture-count_ .body_ .start-register_ .end-register_:

  allocate-registers compiler/MiniExpCompiler_ -> none:
    if start-register_ == null:
      start-register_ = compiler.allocate-capture-registers
      end-register_ = start-register_ - 1

  generate compiler/MiniExpCompiler_ on-success/MiniExpLabel_ -> none:
    undo-start := MiniExpLabel_
    write-end := MiniExpLabel_
    undo-end := MiniExpLabel_
    compiler.copy-register start-register_ CURRENT-POSITION_
    compiler.push-backtrack undo-start

    compiler.generate body_ write-end

    compiler.bind write-end
    compiler.copy-register end-register_ CURRENT-POSITION_
    compiler.push-backtrack undo-end
    compiler.goto on-success

    compiler.bind undo-start
    compiler.copy-register start-register_ NO-POSITION-REGISTER_
    compiler.backtrack

    compiler.bind undo-end
    compiler.copy-register end-register_ NO-POSITION-REGISTER_
    compiler.backtrack

  analyze compiler/MiniExpCompiler_ -> MiniExpAnalysis_:
    allocate-registers compiler
    body-analysis := body_.analyze compiler
    return MiniExpAnalysis_.capture body-analysis start-register_ end-register_

  case-expand compiler/MiniExpCompiler_ -> MiniExpAst_:
    b := body_.case-expand compiler
    if b == body_: return this
    return Capture_.private_ capture-count_ b start-register_ end-register_

interface Match:
  /// The regular expression that created this match.
  pattern -> RegExp

  /// The input string in which a match was found.
  input -> string

  /// The substring of the input that matched the regexp.
  matched -> string

  /// The start index of the match in the string.
  index -> int

  /// The end index of the match in the string.
  end-index -> int

  /**
  The start index of the nth capture in the string.
  The 0th capture is the index of the match of the whole regexp.
  */
  index index/int -> int?

  /**
  The end index of the nth capture in the string.
  The 0th capture is the index of the match of the whole regexp.
  */
  end-index index/int -> int?

  /**
  The string captured by the nth capture.
  The 0th capture is the entire substring that matched the regexp.
  */
  operator [] index/int -> string

  /**
  The number of captures in the regexp.
  */
  capture-count -> int

class MiniExpMatch_ implements Match:
  pattern /RegExp ::= ?
  input /string ::= ?
  registers_ /List/*<int>*/ ::= ?
  first-capture-reg_ /int ::= ?

  constructor .pattern .input .registers_ .first-capture-reg_:

  capture-count -> int: return (registers_.size - 2 - first-capture-reg_) >> 1

  matched -> string: return this[0]

  index -> int: return registers_[first-capture-reg_]

  end-index -> int: return registers_[first-capture-reg_ + 1]

  index index/int -> int?:
    if not 0 <= index <= capture-count: throw "OUT_OF_RANGE"
    result := registers_[first-capture-reg_ + index * 2]
    return result == NO-POSITION_ ? null : result

  end-index index/int -> int?:
    if not 0 <= index <= capture-count: throw "OUT_OF_RANGE"
    result := registers_[first-capture-reg_ + 1 + index * 2]
    return result == NO-POSITION_ ? null : result

  operator[] index/int -> string?:
    if not 0 <= index <= capture-count: throw "OUT_OF_RANGE"
    index *= 2
    index += first-capture-reg_
    if registers_[index] == NO-POSITION_: return null
    return input.copy registers_[index] registers_[index + 1]

JS-MODE_ ::= 0
VIM-MODE_ ::= 1
ED-MODE_ ::= 2

interface RegExp:
  pattern -> string
  multiline -> bool
  case-sensitive -> bool
  match-as-prefix a/string a1/int -> Match
  first-matching a/string -> Match
  has-matching a/string -> bool
  string-matching a/string -> string?
  all-matches subject/string start/int [block] -> bool
  number-of-captures -> int
  trace -> none

  constructor pattern/string --multiline/bool=true --case-sensitive/bool=true:
    return MiniExp_ pattern multiline case-sensitive --mode=JS-MODE_

  constructor.js pattern/string --multiline/bool=true --case-sensitive/bool=true:
    return MiniExp_ pattern multiline case-sensitive --mode=JS-MODE_

  constructor.ed pattern/string --multiline/bool=true --case-sensitive/bool=true:
    return MiniExp_ pattern multiline case-sensitive --mode=ED-MODE_

  constructor.vim pattern/string --multiline/bool=true --case-sensitive/bool=true:
    return MiniExp_ pattern multiline case-sensitive --mode=VIM-MODE_

class MiniExp_ implements RegExp:
  byte-codes_/List?/*<int>*/ := null
  initial-register-values_/List?/*<int>*/ := null
  first-capture-register_/int? := null
  sticky-entry-point_/int? := null
  constant-pool_/ByteArray? := null
  pattern /string ::= ?
  multiline /bool ::= ?
  case-sensitive /bool ::= ?
  trace_ /bool := false

  trace -> none:
    trace_ = true

  constructor .pattern .multiline .case-sensitive --mode/int:
    compiler := MiniExpCompiler_ pattern case-sensitive
    parser := MiniExpParser_ compiler pattern multiline --mode=mode
    ast/MiniExpAst_ := parser.parse
    generate-code_ compiler ast pattern

  match-as-prefix a/string a1/int=0 -> Match?:
    return match_ a a1 sticky-entry-point_

  first-matching a/string -> Match?:
    return match_ a 0 0

  has-matching a/string -> bool:
    return (match_ a 0 0) != null

  string-matching a/string -> string?:
    m/Match := match_ a 0 0
    if m == null: return null
    return m[0]

  /**
  Calls the block once for each match with the match as argument.
  Returns true iff there was at least one match.
  */
  all-matches subject/string start/int=0 [block] -> bool:
    at-least-once := false
    position := start
    if not 0 <= position <= subject.size: throw "OUT_OF_RANGE"
    while position <= subject.size:
      c := match_ subject position 0
      if c == null: return at-least-once
      current := c as MiniExpMatch_
      if current.index == current.end-index:
        position = current.end-index + 1
      else:
        position = current.end-index
      block.call current
      at-least-once = true
    return at-least-once

  number-of-captures -> int:
    return (initial-register-values_.size - 2 - first-capture-register_) >> 1

  match_ a/string start-position/int start-program-counter/int -> Match?:
    registers/List/*<int>*/ := initial-register-values_.copy
    interpreter := MiniExpInterpreter_ byte-codes_ constant-pool_ registers trace_
    if not interpreter.interpret a start-position start-program-counter:
      return null
    return MiniExpMatch_ this a registers first-capture-register_

  generate-code_ compiler/MiniExpCompiler_ ast/MiniExpAst_ source/string -> none:
    // Top level capture regs.
    top-level-capture-reg/int := compiler.allocate-capture-registers

    top-analysis/MiniExpAnalysis_ := ast.analyze compiler

    ast = ast.case-expand compiler

    compiler.add-capture-registers

    sticky-entry-point := MiniExpLabel_
    sticky-start := MiniExpLabel_
    fail-sticky := MiniExpLabel_

    start := MiniExpLabel_
    compiler.bind start

    fail := MiniExpLabel_
    compiler.push-backtrack fail

    compiler.bind sticky-start
    compiler.copy-register top-level-capture-reg CURRENT-POSITION_

    succeed := MiniExpLabel_
    compiler.generate ast succeed

    compiler.bind fail
    if not top-analysis.anchored:
      end := MiniExpLabel_
      compiler.goto-if-greater-equal CURRENT-POSITION_ STRING-LENGTH_ end
      compiler.advance-by-rune-width CURRENT-POSITION_ 1
      compiler.goto start
      compiler.bind end
    compiler.bind fail-sticky
    compiler.fail

    compiler.bind succeed
    compiler.copy-register top-level-capture-reg - 1 CURRENT-POSITION_
    compiler.succeed

    compiler.bind sticky-entry-point
    compiler.push-backtrack fail-sticky
    compiler.goto sticky-start

    byte-codes_ = compiler.codes
    constant-pool_ = compiler.constant-pool
    initial-register-values_ = compiler.registers
    first-capture-register_ = compiler.first-capture-register
    sticky-entry-point_ = sticky-entry-point.location

  disassemble_ -> none:
    print "\nDisassembly\n"
    labels /List/*<bool>*/ := List byte-codes_.size
    for i := 0; i < byte-codes_.size; :
      code /int := byte-codes_[i]
      if code == BC-PUSH-BACKTRACK_ or code == BC-GOTO_:
        pushed /int := byte-codes_[i + 1]
        if pushed >= 0 and pushed < byte-codes_.size: labels[pushed] = true
      i += BYTE-CODE-NAMES_[code * 3 + 1] + BYTE-CODE-NAMES_[code * 3 + 2] + 1
    for i := 0; i < byte-codes_.size; :
      if labels[i]: print "$i:"
      i += disassemble-single-instruction_ byte-codes_ first-capture-register_ i: print it
    print "\nEnd Disassembly\n"

  static disassemble-single-instruction_ byte-codes/List first-capture-register/int? i/int [block] -> int:
      code /int := byte-codes[i]
      regs /int := BYTE-CODE-NAMES_[code * 3 + 1]
      other-args /int := BYTE-CODE-NAMES_[code * 3 + 2]
      line /string := "$(%3d i):"
      joiner /string := " "
      arg-joiner /string := " "
      if code == BC-COPY-REGISTER_:
        joiner = " = "
      else if code == BC-ADD-TO-REGISTER_:
        arg-joiner = " += "
      else:
        line = "$(%3d i): $BYTE-CODE-NAMES_[code * 3]"
      for j := 0; j < regs; j++:
        reg/int := byte-codes[i + 1 + j]
        line = "$line$(j == 0 ? " " : joiner)$(register-name_ reg first-capture-register)"
      for j := 0; j < other-args; j++:
        line = "$line$arg-joiner$byte-codes[i + 1 + regs + j]"
      block.call line
      return regs + other-args + 1

  static register-name_ i/int first-capture-register/int? -> string:
    if first-capture-register and first-capture-register <= i:
      capture := i - first-capture-register
      if capture & 1 == 0:
        return "start$(capture / 2)"
      else:
        return "end$(capture / 2)"
    if i < REGISTER-NAMES_.size:
      return REGISTER-NAMES_[i]
    return "reg$i"

// Lexer tokens.
NONE ::= 0
QUANT ::= 1
BACKSLASH ::= 2
DOT ::= 3
L-PAREN ::= 4
R-PAREN ::= 5
L-SQUARE ::= 6
HAT ::= 7
DOLLAR ::= 8
PIPE ::= 9
BACK-REFERENCE ::= 10
WORD-BOUNDARY ::= 11
LEFT-WORD-BOUNDARY ::= 12
RIGHT-WORD-BOUNDARY ::= 13
NOT-WORD-BOUNDARY ::= 14
WORD-CHARACTER ::= 15
NOT-WORD-CHARACTER ::= 16
DIGIT ::= 17
NOT-DIGIT ::= 18
WHITESPACE ::= 19
NOT-WHITESPACE ::= 20
NON-CAPTURING ::= 21
LOOK-AHEAD ::= 22
NEGATIVE-LOOK-AHEAD ::= 23
OTHER ::= 24

class MiniExpParser_:
  compiler_ /MiniExpCompiler_ ::= ?
  source_ /string ::= ?
  multiline_ /bool ::= ?
  mode /int
  charcode-to-token_ /List ::= ?
  escapes_ /Map ::= ?
  control-characters_ /Map ::= ?

  capture-count_ /int := 0

  // State of the parser and lexer.
  position_/int := 0  // Location in source.
  last-token_/int := -1
  // This is the offset in the constant pool of the character data associated
  // with the token.
  last-token-index_/int := -1
  // Greedyness of the last single-character quantifier.
  last-was-greedy_/bool := false
  last-back-reference-index_/string? := null
  minimum-repeats_/int? := null
  maximum-repeats_/int? := null

  constructor .compiler_ .source_ .multiline_ --.mode/int:
    charcode-to-token_ = CHARCODE-TO-TOKEN[mode]
    escapes_ = ESCAPES[mode]
    control-characters_ = CONTROL-CHARACTERS[mode]

  parse -> MiniExpAst_:
    get-token
    ast/MiniExpAst_ := parse-disjunction
    expect-token NONE
    return ast

  at_ position/int -> int: return source_[position]

  has_ position/int -> bool: return source_.size > position

  error message/string -> none:
    throw
      FormatException "Error while parsing regexp: $message" source_ position_

  parse-disjunction -> MiniExpAst_:
    ast/MiniExpAst_ := parse-alternative
    while accept-token PIPE:
      ast = Disjunction_ ast parse-alternative
    return ast

  end-of-alternative -> bool:
    return last-token_ == PIPE or last-token_ == R-PAREN or
        last-token_ == NONE

  parse-alternative -> MiniExpAst_:
    if end-of-alternative:
      return EmptyAlternative_
    ast/MiniExpAst_ := parse-term
    while not end-of-alternative:
      ast = Alternative_ ast parse-term
    return ast

  try-parse-assertion -> MiniExpAst_?:
    if accept-token HAT:
      return multiline_ ? AtBeginningOfLine_ : AtStart_
    if accept-token DOLLAR:
      return multiline_ ? AtEndOfLine_ : AtEnd_
    if accept-token WORD-BOUNDARY: return WordBoundary_ true true true
    if accept-token LEFT-WORD-BOUNDARY: return WordBoundary_ true true false
    if accept-token RIGHT-WORD-BOUNDARY: return WordBoundary_ true false true
    if accept-token NOT-WORD-BOUNDARY: return WordBoundary_ false true true
    lookahead-ast/MiniExpAst_? := null
    if accept-token LOOK-AHEAD:
      lookahead-ast = LookAhead_ true parse-disjunction compiler_
    else if accept-token NEGATIVE-LOOK-AHEAD:
      lookahead-ast = LookAhead_ false parse-disjunction compiler_
    if lookahead-ast != null:
      expect-token R-PAREN
      // The normal syntax does not allow a quantifier here, but the web
      // compatible one does.  Slightly nasty hack for compatibility:
      if peek-token QUANT:
        quant/MiniExpAst_ := Quantifier_ --min=minimum-repeats_ --max=maximum-repeats_ --greedy=last-was-greedy_ --body=lookahead-ast --compiler=compiler_
        expect-token QUANT
        return quant
      return lookahead-ast
    return null

  parse-term -> MiniExpAst_:
    ast/MiniExpAst_? := try-parse-assertion
    if ast == null:
      ast = parse-atom
      if peek-token QUANT:
        quant/MiniExpAst_ := Quantifier_ --min=minimum-repeats_ --max=maximum-repeats_ --greedy=last-was-greedy_ --body=ast --compiler=compiler_
        expect-token QUANT
        return quant
    return ast

  parse-atom -> MiniExpAst_?:
    if peek-token OTHER:
      result := Atom_ last-token-index_
      expect-token OTHER
      return result
    if accept-token DOT:
      ast := CharClass_ false  // Negative char class.
      if not multiline_:
        ast.add '\n' '\n'
        ast.add '\r' '\r'
        ast.add CHAR-CODE-LINE-SEPARATOR_ CHAR-CODE-PARAGRAPH-SEPARATOR_
      return ast

    if peek-token BACK-REFERENCE:
      back-ref := BackReference_ last-back-reference-index_
      expect-token BACK-REFERENCE
      return back-ref

    if accept-token L-PAREN:
      ast/MiniExpAst_ := parse-disjunction
      ast = Capture_ capture-count_++ ast
      expect-token R-PAREN
      return ast
    if accept-token NON-CAPTURING:
      ast := parse-disjunction
      expect-token R-PAREN
      return ast

    char-class/CharClass_? := null
    digit-char-class := false
    if accept-token WORD-CHARACTER:
      char-class = CharClass_ true
    else if accept-token NOT-WORD-CHARACTER:
      char-class = CharClass_ false
    else if accept-token DIGIT:
      char-class = CharClass_ true
      digit-char-class = true
    else if accept-token NOT-DIGIT:
      char-class = CharClass_ false
      digit-char-class = true
    if char-class != null:
      char-class.add '0' '9'
      if not digit-char-class:
        char-class.add 'A' 'Z'
        char-class.add '_' '_'
        char-class.add 'a' 'z'
      return char-class

    if accept-token WHITESPACE:
      char-class = CharClass_ true
    else if accept-token NOT-WHITESPACE:
      char-class = CharClass_ false
    if char-class != null:
      char-class.add-spaces
      return char-class
    if peek-token L-SQUARE:
      return parse-character-class
    if peek-token NONE: error "Unexpected end of regexp"
    error "Unexpected token $last-token_"
    return null

  parse-character-class -> MiniExpAst_:
    char-class/CharClass_ := ?

    if has_ position_ and (at_ position_) == '^':
      position_++
      char-class = CharClass_ false
    else:
      char-class = CharClass_ true

    add-char-code := : | code |
      if code < 0:
        char-class.add-special (at_ -code + 1)
      else:
        char-class.add code code

    while has_ position_:
      code/int := at_ position_

      degenerate-range := false
      if code == ']':
        // End of character class.  This reads the terminating square bracket.
        get-token
        break
      // Single character or escape code representing a single character.
      // This also advanced position_.
      code = read-character-class-code_ (mode == ED-MODE_)

      // Check if there are at least 2 more characters and the next is a dash.
      if (not has_ position_ + 1) or
         (at_ position_) != '-' or
         (at_ position_ + 1) == ']':
        // No dash-something here, so it's not part of a range.  Add the code
        // and move on.
        add-char-code.call code
        continue
      // Found a dash, try to parse a range.
      position_++;  // Skip the dash.
      code2/int := read-character-class-code_ (mode == ED-MODE_)
      if code < 0 or code2 < 0:
        // One end of the range is not a single character, so the range is
        // degenerate.  We add either and and the dash, instead of a range.
        add-char-code.call code
        char-class.add '-' '-'
        add-char-code.call code2
      else:
        // Found a range.
        if code > code2: error "Character range out of order"
        char-class.add code code2
    expect-token OTHER;  // The terminating right square bracket.
    return char-class

  // Returns a character (possibly from a parsed escape) or a negative number
  // indicating the position of a character class special \s \d or \w.
  read-character-class-code_ ed-mode/bool -> int:
    code/int := at_ position_
    if code != '\\':
      position_ += utf-8-bytes code
      return code
    if not has_ position_ + 1: error "Unexpected end of regexp"
    code2/int := at_ position_ + 1
    if ed-mode:
      // Ed doesn't have \s, \d etc. inside character classes.
      position_++
      return code2
    lower/int := code2 | 0x20
    if (lower == 'd' or lower == 's' or lower == 'w'):
      answer := -position_
      position_ += 2
      return answer
    if code2 == 'c':
      // For web compatibility, the set of characters that can follow \c inside
      // a character class is different from the a-z_a-Z that are allowed outside
      // a character class.
      if has_ position_ + 2 and is-backslash-c-character (at_ position_ + 2):
        position_ += 3
        return at_ (position_ - 1) % 32
      // This makes little sense, but for web compatibility, \c followed by an
      // invalid character is interpreted as a literal backslash, followed by
      // the "c", etc.
      position_++
      return '\\'
    if '0' <= code2 and code2 <= '9':
      position_++
      return lex-integer 8 0x100
    position_ += 1 + (utf-8-bytes code2)
    if code2 == 'u':
      code = lex-hex 4
    else if code2 == 'x':
      code = lex-hex 2
    else if control-characters_.contains code2:
      code = control-characters_[code2]
    else:
      code = code2
    // In the case of a malformed escape we just interpret as if the backslash
    // was not there.
    if code == -1: code = code2
    return code

  expect-token token/int -> none:
    if token != last-token_:
      error "At position_ $(position_ - 1) expected $token, found $last-token_"
    get-token

  accept-token token/int -> bool:
    if token == last-token_:
      get-token
      return true
    return false

  peek-token token/int -> bool: return token == last-token_

  static CHARCODE-TO-TOKEN ::= [
      JS-CHARCODE-TO-TOKEN,
      VIM-CHARCODE-TO-TOKEN,
      VIM-CHARCODE-TO-TOKEN,  // Ed mode has same tokenization as vim mode.
  ]

  static VIM-CHARCODE-TO-TOKEN := create-vim-charcode-to-token_

  static create-vim-charcode-to-token_ -> List:
    result := JS-CHARCODE-TO-TOKEN.copy
    // In vim mode you need backslash to make the following special.
    result['?'] = OTHER
    result['{'] = OTHER
    result['+'] = OTHER
    result['|'] = OTHER
    result['('] = OTHER
    result[')'] = OTHER
    return result

  static JS-CHARCODE-TO-TOKEN ::= [
    OTHER, OTHER, OTHER, OTHER,      // 0-3
    OTHER, OTHER, OTHER, OTHER,      // 4-7
    OTHER, OTHER, OTHER, OTHER,      // 8-11
    OTHER, OTHER, OTHER, OTHER,      // 12-15
    OTHER, OTHER, OTHER, OTHER,      // 16-19
    OTHER, OTHER, OTHER, OTHER,      // 20-23
    OTHER, OTHER, OTHER, OTHER,      // 24-27
    OTHER, OTHER, OTHER, OTHER,      // 28-31
    OTHER, OTHER, OTHER, OTHER,      //  !"#
    DOLLAR, OTHER, OTHER, OTHER,     // $%&'
    L-PAREN, R-PAREN, QUANT, QUANT,  // ()*+,
    OTHER, OTHER, DOT, OTHER,        // ,-./
    OTHER, OTHER, OTHER, OTHER,      // 0123
    OTHER, OTHER, OTHER, OTHER,      // 4567
    OTHER, OTHER, OTHER, OTHER,      // 89:
    OTHER, OTHER, OTHER, QUANT,      // <=>?
    OTHER, OTHER, OTHER, OTHER,      // @ABC
    OTHER, OTHER, OTHER, OTHER,      // DEFG
    OTHER, OTHER, OTHER, OTHER,      // HIJK
    OTHER, OTHER, OTHER, OTHER,      // LMNO
    OTHER, OTHER, OTHER, OTHER,      // PQRS
    OTHER, OTHER, OTHER, OTHER,      // TUVW
    OTHER, OTHER, OTHER, L-SQUARE,   // XYZ[
    BACKSLASH, OTHER, HAT, OTHER,    // \]^_
    OTHER, OTHER, OTHER, OTHER,      // `abc
    OTHER, OTHER, OTHER, OTHER,      // defg
    OTHER, OTHER, OTHER, OTHER,      // hijk
    OTHER, OTHER, OTHER, OTHER,      // lmno
    OTHER, OTHER, OTHER, OTHER,      // pqrs
    OTHER, OTHER, OTHER, OTHER,      // tuvw
    OTHER, OTHER, OTHER, QUANT,      // xyz{
    PIPE, OTHER]                     // |}

  static JS-ESCAPES ::= {
    'b': WORD-BOUNDARY,
    'B': NOT-WORD-BOUNDARY,
    'w': WORD-CHARACTER,
    'W': NOT-WORD-CHARACTER,
    'd': DIGIT,
    'D': NOT-DIGIT,
    's': WHITESPACE,
    'S': NOT-WHITESPACE,
  }

  static VIM-ESCAPES ::= {
    '<': LEFT-WORD-BOUNDARY,
    '>': RIGHT-WORD-BOUNDARY,
    'w': WORD-CHARACTER,
    'W': NOT-WORD-CHARACTER,
    'd': DIGIT,
    'D': NOT-DIGIT,
    's': WHITESPACE,
    'S': NOT-WHITESPACE,
    '|': PIPE,
    '(': L-PAREN,
    ')': R-PAREN,
    '=': QUANT,  // \= is a synonym for \? in vim but not ed.
    '?': QUANT,
    '+': QUANT,
    '{': QUANT,
    '}': QUANT,
  }

  static ED-ESCAPES ::= {
    'b': WORD-BOUNDARY,
    'B': NOT-WORD-BOUNDARY,
    '<': LEFT-WORD-BOUNDARY,
    '>': RIGHT-WORD-BOUNDARY,
    'w': WORD-CHARACTER,
    'W': NOT-WORD-CHARACTER,
    's': WHITESPACE,
    'S': NOT-WHITESPACE,
    '|': PIPE,
    '(': L-PAREN,
    ')': R-PAREN,
    '?': QUANT,
    '+': QUANT,
    '{': QUANT,
    '}': QUANT,
  }

  static ESCAPES ::= [
    JS-ESCAPES,
    VIM-ESCAPES,
    ED-ESCAPES,
  ]

  static JS-CONTROL-CHARACTERS ::= {
    'b': '\b',
    'f': '\f',
    'n': '\n',
    'r': '\r',
    't': '\t',
    'v': '\v',
  }

  static VIM-CONTROL-CHARACTERS ::= JS-CONTROL-CHARACTERS

  static ED-CONTROL-CHARACTERS ::= { : }

  static CONTROL-CHARACTERS ::= [
    JS-CONTROL-CHARACTERS,
    VIM-CONTROL-CHARACTERS,
    ED-CONTROL-CHARACTERS,
  ]

  token-from-charcode code/int -> int:
    if code >= charcode-to-token_.size: return OTHER
    return charcode-to-token_[code]

  on-digit position_/int -> bool:
    if not has_ position_: return false
    if (at_ position_) < '0': return false
    return (at_ position_) <= '9'

  get-token -> none:
    if not has_ position_:
      last-token_ = NONE
      return
    last-token-index_ = position_
    code/int := at_ position_
    last-token_ = token-from-charcode code
    token := last-token_
    if token == BACKSLASH:
      lex-backslash
      if last_token_ == QUANT:  // Can only happen in vim and ed modes.
        lex-quantifier
        position_++
      return
    if token == L-PAREN:
      lex-left-parenthesis
    else if token == QUANT:
      lex-quantifier
    position_ += utf-8-bytes code

  // This may be a bug in Irregexp, but there are tests for it: \c _and \c0
  // work like \cc which means Control-C.  But only in character classes.
  static is-backslash-c-character code/int -> bool:
    if is-ascii-letter code: return true
    if '0' <= code <= '9': return true
    return code == '_'

  static is-ascii-letter code/int -> bool:
    if 'A' <= code <= 'Z': return true
    return 'a' <= code <= 'z'

  lex-backslash -> none:
    if not has_ (position_ + 1): error "\\ at end of pattern"
    next-code/int := at_ position_ + 1
    if escapes_.contains next-code:
      last-token_ = escapes_[next-code]
      position_ += last-token_ == QUANT ? 1 : 2
    else if control-characters_.contains next-code:
      position_ += 2
      last-token_ = OTHER
      last-token-index_ =
          compiler_.add-to-constant-pool control-characters_[next-code]
    else if next-code == 'c':
       if (has_ position_ + 2) and (is-ascii-letter (at_ position_ + 2)):
         last-token_ = OTHER
         last-token-index_ = compiler_.add-to-constant-pool (at_ position_ + 2) % 32
         position_ += 3
       else:
         // \c _is interpreted as a literal backslash and literal "c_".
         last-token_ = OTHER
         last-token-index_ = position_
         position_++
    else if on-digit position_ + 1:
      position_++
      last-back-reference-index_ = lex-integer-as-string
      last-token_ = BACK-REFERENCE
    else if next-code == 'x' or next-code == 'u':
      position_ += 2
      last-token_ = OTHER
      code-unit := lex-hex (next-code == 'x' ? 2 : 4)
      if code-unit == -1:
        last-token-index_ = position_ - 1
      else:
        last-token-index_ = compiler_.add-to-constant-pool code-unit
    else:
      last-token_ = OTHER
      last-token-index_ = position_ + 1
      position_ += 1 + (utf-8-bytes next-code)

  lex-hex chars/int -> int:
    if not has_ position_ + chars - 1: return -1
    total/int := 0
    for i := 0; i < chars; i++:
      total *= 16
      char-code := at_ position_ + i
      if char-code >= '0' and char-code <= '9':
        total += char-code - '0'
      else if (char-code >= 'A' and
                 char-code <= 'F'):
        total += 10 + char-code - 'A'
      else if (char-code >= 'a' and
                 char-code <= 'f'):
        total += 10 + char-code - 'a'
      else:
        return -1
    position_ += chars
    return total

  lex-integer-as-string -> string:
    b := Buffer
    while true:
      if not has_ position_: return b.to-string
      code := at_ position_
      if code >= '0' and code <= '9':
        b.write-byte code
        position_++
      else:
        return b.to-string

  lex-integer base/int max/int? -> int:
    total/int := 0
    while true:
      if not has_ position_: return total
      code := at_ position_
      if code >= '0' and code < '0' + base and (max == null or total * base < max):
        position_++
        total *= base
        total += code - '0'
      else:
        return total

  lex-left-parenthesis -> none:
    if not has_ position_ + 1: error "unterminated group"
    if (at_ position_ + 1) == '?':
      if not has_ position_ + 2: error "unterminated group"
      parenthesis-modifier/int := at_ position_ + 2
      if parenthesis-modifier == '=':
        last-token_ = LOOK-AHEAD
      else if parenthesis-modifier == ':':
        last-token_ = NON-CAPTURING
      else if parenthesis-modifier == '!':
        last-token_ = NEGATIVE-LOOK-AHEAD
      else:
        error "invalid group"
      position_ += 2
      return

  at-close-curly_ -> bool:
    if mode == VIM-MODE_ or mode == ED-MODE_:
      // Look for \} to terminate, not just }.
      result := has_ (position_ + 1)
          and (at_ (position_ + 1)) == '}'
          and (at_ position_) == '\\'
      if result: position_++
      return result
    return (at_ position_) == '}'

  lex-quantifier -> none:
    quantifier-code/int := at_ position_
    if quantifier-code == '{':
      parsed-repeats := false
      saved-position := position_
      if on-digit (position_ + 1):
        position_++
        // We parse the repeats in the lexer.  Forms allowed are {n}, {n,}
        // and {n,m}.
        minimum-repeats_ = lex-integer 10 null
        if has_ position_:
          if at-close-curly_:
            maximum-repeats_ = minimum-repeats_
            parsed-repeats = true
          else if (at_ position_) == ',':
            position_++
            if has_ position_:
              if at-close-curly_:
                maximum-repeats_ = null;  // No maximum.
                parsed-repeats = true
              else if on-digit position_:
                maximum-repeats_ = lex-integer 10 null
                if (has_ position_) and at-close-curly_:
                  parsed-repeats = true
      if parsed-repeats:
        if maximum-repeats_ != null and minimum-repeats_ > maximum-repeats_:
          error "numbers out of order in {} quantifier"
      else:
        // If parsing of the repeats fails then we follow JS in interpreting
        // the left brace as a literal.  TODO: vim and ed report an error here.
        position_ = saved-position
        last-token_ = OTHER
        return
    else if quantifier-code == '*':
      minimum-repeats_ = 0
      maximum-repeats_ = null;  // No maximum.
    else if quantifier-code == '+':
      minimum-repeats_ = 1
      maximum-repeats_ = null;  // No maximum.
    else:
      minimum-repeats_ = 0
      maximum-repeats_ = 1
    // In vim mode \= is a synonym for \?.
    if (has_ position_ + 1)
          and ((at_ position_ + 1) == '?' or (at_ position_ + 1) == '='):
      position_++
      last-was-greedy_ = false
    else:
      last-was-greedy_ = true

class MiniExpInterpreter_:
  byte-codes_/List/*<int>*/ ::= ?
  constant-pool_ /ByteArray ::= ?
  registers_/List/*<int>*/ ::= ?
  trace_/bool

  constructor .byte-codes_ .constant-pool_ .registers_ .trace_:

  stack/List/*<int>*/ := []
  stack-pointer/int := -1

  interpret subject/string start-position/int program-counter/int -> bool:
    registers_[STRING-LENGTH_] = subject.size
    registers_[CURRENT-POSITION_] = start-position

    while true:
      byte-code := byte-codes_[program-counter]
      if trace_:
        pos := registers_[CURRENT-POSITION_]
        i := pos
        while i < subject.size and subject[i] == null: i--
        line := "\"$subject[0..i]^$subject[i..]\"  @$(%-3d pos) $(%50s registers_)  "
        MiniExp_.disassemble-single-instruction_ byte-codes_ null program-counter: line += it
        print line
      program-counter++
      // TODO: Faster implementation.
      if byte-code == BC-GOTO_:
        program-counter = byte-codes_[program-counter]
      else if byte-code == BC-PUSH-REGISTER_:
        reg/int := registers_[byte-codes_[program-counter++]]
        stack-pointer++
        if stack-pointer == stack.size:
          stack.add reg
        else:
          stack[stack-pointer] = reg
      else if byte-code == BC-PUSH-BACKTRACK_:
        value/int := byte-codes_[program-counter++]
        stack-pointer++
        if stack-pointer == stack.size:
          stack.add value
        else:
          stack[stack-pointer] = value
        position/int := registers_[CURRENT-POSITION_]
        stack-pointer++
        if stack-pointer == stack.size:
          stack.add position
        else:
          stack[stack-pointer] = position
      else if byte-code == BC-POP-REGISTER_:
        registers_[byte-codes_[program-counter++]] = stack[stack-pointer--]
      else if byte-code == BC-BACKTRACK-EQ_:
        reg1/int := registers_[byte-codes_[program-counter++]]
        reg2/int := registers_[byte-codes_[program-counter++]]
        if reg1 == reg2:
          registers_[CURRENT-POSITION_] = stack[stack-pointer--]
          program-counter = stack[stack-pointer--]
      else if byte-code == BC-BACKTRACK-NE_:
        reg1/int := registers_[byte-codes_[program-counter++]]
        reg2/int := registers_[byte-codes_[program-counter++]]
        if reg1 != reg2:
          registers_[CURRENT-POSITION_] = stack[stack-pointer--]
          program-counter = stack[stack-pointer--]
      else if byte-code == BC-BACKTRACK-GT_:
        reg1/int := registers_[byte-codes_[program-counter++]]
        reg2/int := registers_[byte-codes_[program-counter++]]
        if reg1 > reg2:
          registers_[CURRENT-POSITION_] = stack[stack-pointer--]
          program-counter = stack[stack-pointer--]
      else if byte-code == BC-BACKTRACK-IF-NO-MATCH_:
        if (subject.at --raw registers_[CURRENT-POSITION_]) !=
            (constant-pool_[byte-codes_[program-counter++]]):
          registers_[CURRENT-POSITION_] = stack[stack-pointer--]
          program-counter = stack[stack-pointer--]
      else if byte-code == BC-BACKTRACK-IF-IN-RANGE_:
        code/int := subject.at --raw registers_[CURRENT-POSITION_]
        from/int := byte-codes_[program-counter++]
        to/int := byte-codes_[program-counter++]
        if from <= code and code <= to:
          registers_[CURRENT-POSITION_] = stack[stack-pointer--]
          program-counter = stack[stack-pointer--]
      else if byte-code == BC-GOTO-IF-MATCH_:
        code/int := subject.at --raw registers_[CURRENT-POSITION_]
        expected/int := byte-codes_[program-counter++]
        dest/int := byte-codes_[program-counter++]
        if code == expected: program-counter = dest
      else if byte-code == BC-GOTO-IF-IN-RANGE_:
        code/int := subject.at --raw registers_[CURRENT-POSITION_]
        from/int := byte-codes_[program-counter++]
        to/int := byte-codes_[program-counter++]
        dest/int := byte-codes_[program-counter++]
        if from <= code and code <= to: program-counter = dest
      else if byte-code == BC-GOTO-IF-UNICODE-IN-RANGE_:
        code/int := subject[registers_[CURRENT-POSITION_]]
        from/int := byte-codes_[program-counter++]
        to/int := byte-codes_[program-counter++]
        dest/int := byte-codes_[program-counter++]
        if from <= code and code <= to: program-counter = dest
      else if byte-code == BC-GOTO-EQ_:
        reg1/int := registers_[byte-codes_[program-counter++]]
        reg2/int := registers_[byte-codes_[program-counter++]]
        dest/int := byte-codes_[program-counter++]
        if reg1 == reg2: program-counter = dest
      else if byte-code == BC-GOTO-GE_:
        reg1/int := registers_[byte-codes_[program-counter++]]
        reg2/int := registers_[byte-codes_[program-counter++]]
        dest/int := byte-codes_[program-counter++]
        if reg1 >= reg2: program-counter = dest
      else if byte-code == BC-GOTO-IF-WORD-CHARACTER_:
        offset/int := byte-codes_[program-counter++]
        char-code/int :=
            subject.at --raw registers_[CURRENT-POSITION_] + offset
        dest/int := byte-codes_[program-counter++]
        if char-code >= '0':
          if char-code <= '9':
            program-counter = dest
          else if char-code >= 'A':
            if char-code <= 'Z':
              program-counter = dest
            else if char-code == '_':
              program-counter = dest
            else if (char-code >= 'a' and
                       char-code <= 'z'):
              program-counter = dest
      else if byte-code == BC-ADD-TO-REGISTER_:
        register-index/int := byte-codes_[program-counter++]
        registers_[register-index] += byte-codes_[program-counter++]
      else if byte-code == BC-ADVANCE-BY-RUNE-WIDTH_:
        register-index/int := byte-codes_[program-counter++]
        characters/int := byte-codes_[program-counter++]
        position := registers_[CURRENT-POSITION_]
        if characters > 0:
          characters.repeat:
            if position >= subject.size:
              position++
            else:
              code/int := subject.at --raw position
              position += MiniExpCompiler_.UTF-FIRST-CHAR-TABLE_[code >> 4]
        else if characters < 0:
          (-characters).repeat:
            position--
            if position < subject.size:
              code/int? := subject[position]
              while code == null:
                position--
                code = subject[position]
        registers_[register-index] = position
      else if byte-code == BC-COPY-REGISTER_:
        // We don't normally keep the stack pointer in sync with its slot in
        // the registers_, but we have to have it in sync here.
        registers_[STACK-POINTER_] = stack-pointer
        register-index/int := byte-codes_[program-counter++]
        value/int := registers_[byte-codes_[program-counter++]]
        registers_[register-index] = value
        stack-pointer = registers_[STACK-POINTER_]
      else if byte-code == BC-BACKTRACK-ON-BACK-REFERENCE_:
        register-index/int := byte-codes_[program-counter++]
        case-sensitive := byte-codes_[program-counter++] != 0
        if not check-back-reference subject case-sensitive register-index:
          // Backtrack.
          registers_[CURRENT-POSITION_] = stack[stack-pointer--]
          program-counter = stack[stack-pointer--]
      else if byte-code == BC-BACKTRACK_:
        registers_[CURRENT-POSITION_] = stack[stack-pointer--]
        program-counter = stack[stack-pointer--]
      else if byte-code == BC-FAIL_:
        return false
      else if byte-code == BC-SUCCEED_:
        return true
      else:
        assert: false

  check-back-reference subject/string case-sensitive/bool register-index/int -> bool:
    start/int := registers_[register-index]
    end/int := registers_[register-index + 1]
    if end == NO-POSITION_: return true
    length/int := end - start
    current-position/int := registers_[CURRENT-POSITION_]
    if current-position + end - start > subject.size: return false
    for i := 0; i < length; i++:
      x := subject.at --raw start + i
      y := subject.at --raw current-position + i
      if not case-sensitive:
        x = case.reg-exp-canonicalize x
        y = case.reg-exp-canonicalize y
      if x != y: return false
    registers_[CURRENT-POSITION_] += length
    return true

class FormatException:
  text /string ::= ?
  source /string ::= ?
  position /int ::= ?

  constructor .text .source .position:

  stringify -> string:
    return "$text\n$source\n$("-" * position)^"

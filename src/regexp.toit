// Copyright (c) 2015, the Dartino project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE.md file.

import bytes show Buffer

class MiniExpLabel:
  // Initially points to -1 to indicate the label is neither linked (used) nor
  // bound (fixed to a location). When the label is linked, but not bound, it
  // has a negative value, determined by fixupLocation(l), that indicates the
  // location of a reference to it, that will be patched when its location has
  // been bound.  When the label is bound, the negative value is used to patch
  // the chained locations that need patching, and the location is set to the
  // correct location for future use.
  static NO_LOCATION ::= -1
  location_ := NO_LOCATION

  isBound -> bool:
    return location_ >= 0

  bind codes/List/*<int>*/ -> none:
    assert: not isBound
    l /int := codes.size
    for forwardReference := location_; forwardReference != NO_LOCATION; :
      patchLocation /int := _decodeFixup forwardReference
      forwardReference = codes[patchLocation]
      codes[patchLocation] = l
    location_ = l

  location -> int:
    assert: isBound
    return location_

  // The negative value is -(location + 2) so as to avoid NO_LOCATION, which is
  // -1.
  _encodeFixup location/int -> int:
    return -(location + 2)

  // It's perhaps not intuitive that the encoding and decoding functions are
  // identical, but they are both just mirroring around -1.
  _decodeFixup encoded/int -> int:
    return -(encoded + 2)

  link codes/List/*<int>*/ -> none:
    value/int := location_
    if not isBound: location_ = _encodeFixup codes.size
    // If the label is bound, this writes the correct (positive) location.
    // Otherwise it writes the previous link in the chain of forward references
    // that need fixing when the label is bound.
    codes.add value

// Registers.
ZERO_REGISTER ::= 0
NO_POSITION_REGISTER ::= 1
CURRENT_POSITION ::= 2
STRING_LENGTH ::= 3
STACK_POINTER ::= 4
FIXED_REGISTERS ::= 5

REGISTER_NAMES ::= [
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
NO_POSITION ::= -1

// Byte codes.
GOTO ::= 0  // label
PUSH_REGISTER ::= 1  // reg
PUSH_BACKTRACK ::= 2  // const
POP_REGISTER ::= 3  // reg
BACKTRACK_EQ ::= 4  // reg reg
BACKTRACK_NE ::= 5  // reg reg
BACKTRACK_GT ::= 6  // reg reg
BACKTRACK_IF_NO_MATCH ::= 7  // constant-pool-offset
BACKTRACK_IF_IN_RANGE ::= 8  // from to
GOTO_IF_MATCH ::= 9  // charCode label
GOTO_IF_IN_RANGE ::= 10  // from to label
GOTO_EQ ::= 11 // reg reg label
GOTO_GE ::= 12 // reg reg label
GOTO_IF_WORD_CHARACTER ::= 13  // position-offset label
ADD_TO_REGISTER ::= 14 // reg const
COPY_REGISTER ::= 15 // dest-reg source-reg
BACKTRACK_ON_BACK_REFERENCE ::= 16 // capture-reg
BACKTRACK ::= 17
SUCCEED ::= 18
FAIL ::= 19

// Format is name, number of register arguments, number of other arguments.
BYTE_CODE_NAMES ::= [
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
  "GOTO_EQ", 2, 1,
  "GOTO_GE", 2, 1,
  "GOTO_IF_WORD_CHARACTER", 0, 2,
  "ADD_TO_REGISTER", 1, 1,
  "COPY_REGISTER", 2, 0,
  "BACKTRACK_ON_BACK_REFERENCE", 1, 1,
  "BACKTRACK", 0, 0,
  "SUCCEED", 0, 0,
  "FAIL", 0, 0,
]

CHAR_CODE_BACKSPACE ::= 8
CHAR_CODE_VERTICAL_TAB ::= 11
CHAR_CODE_FORM_FEED ::= 12
CHAR_CODE_NO_BREAK_SPACE ::= 0xa0
CHAR_CODE_OGHAM_SPACE_MARK ::= 0x1680
CHAR_CODE_EN_QUAD ::= 0x2000
CHAR_CODE_HAIR_SPACE ::= 0x200a
CHAR_CODE_LINE_SEPARATOR ::= 0x2028
CHAR_CODE_PARAGRAPH_SEPARATOR ::= 0x2029
CHAR_CODE_NARROW_NO_BREAK_SPACE ::= 0x202f
CHAR_CODE_MEDIUM_MATHEMATICAL_SPACE ::= 0x205f
CHAR_CODE_IDEOGRAPHIC_SPACE ::= 0x3000
CHAR_CODE_ZERO_WIDTH_NO_BREAK_SPACE ::= 0xfeff

class MiniExpCompiler:
  pattern /string ::= ?
  caseSensitive /bool ::= ?
  registers /List ::= []
  captureRegisterCount /int := 0
  firstCaptureRegister /int := -1
  _codes /List ::= []
  _extraConstants /List ::= []
  _backReferences /List ::= []
  _pendingGoto /MiniExpLabel? := null

  constructor .pattern .caseSensitive:
    for i := 0; i < FIXED_REGISTERS; i++:
      registers.add (i == NO_POSITION_REGISTER ? NO_POSITION : 0)

  codes -> List:
    flushPendingGoto
    return _codes

  constantPool -> string:
    if _extraConstants.is_empty:
      return pattern
    else:
      return pattern + (string.from_runes _extraConstants)

  constantPoolEntry index/int -> int:
    if index < pattern.size: return pattern[index]
    return _extraConstants[index - pattern.size]

  _emit code/int arg1/int?=null arg2/int?=null -> none:
    flushPendingGoto
    _codes.add code
    if arg1 != null: _codes.add arg1
    if arg2 != null: _codes.add arg2

  generate ast/MiniExpAst onSuccess/MiniExpLabel -> none:
    bind ast.label
    ast.generate this onSuccess

  bind label/MiniExpLabel -> none:
    if label == _pendingGoto:
      _pendingGoto = null  // Peephole optimization.
    flushPendingGoto
    label.bind _codes

  link label/MiniExpLabel -> none: label.link _codes

  succeed -> none: _emit SUCCEED

  fail -> none: _emit FAIL

  allocateWorkingRegister -> int: return allocateConstantRegister 0

  allocateConstantRegister value/int -> int:
    register := registers.size
    registers.add value
    return register

  // Returns negative numbers, starting at -1. This is so that we can
  // interleave allocation of capture registers and regular registers, but
  // still end up with the capture registers being contiguous.
  allocateCaptureRegisters -> int:
    captureRegisterCount += 2
    return -captureRegisterCount + 1

  addBackReference b/BackReference -> none:
    _backReferences.add b

  addCaptureRegisters -> none:
    firstCaptureRegister = registers.size
    for i := 0; i < captureRegisterCount; i++:
      registers.add NO_POSITION
    processBackRefences

  processBackRefences -> none:
    _backReferences.do: | b |
      // 1-based index (you can't refer back to capture zero).
      numericIndex := int.parse b.index
      if b.index[0] == '0' or numericIndex * 2 >= captureRegisterCount:
        // Web compatible strangeness - if the index is more than the number of
        // captures it turns into an octal character code escape.
        codeUnit := 0
        octalsFound := 0
        replace /MiniExpAst? := null
        nonOctalsFound := false
        // The first 0-3 octal digits form an octal character escape, the rest
        // are literals.
        b.index.codeUnits.do: | octalDigit |
          if (not nonOctalsFound) and
              '0' <= octalDigit <= '7' and
              codeUnit * 8 < 0x100 and octalsFound < 3:
            codeUnit *= 8
            codeUnit += octalDigit - '0'
            octalsFound++
          else:
            poolIndex := addToConstantPool octalDigit
            atom /MiniExpAst := Atom poolIndex
            replace = (replace == null) ? atom : Alternative replace atom
            nonOctalsFound = true
        if octalsFound != 0:
          poolIndex := addToConstantPool codeUnit
          atom /MiniExpAst := Atom poolIndex
          replace = (replace == null) ? atom : Alternative atom replace
        b.replaceWithAst replace
      else:
        b.register = firstCaptureRegister + numericIndex * 2

  // Raw register numbers are negative for capture registers, positive for
  // constant and working registers.
  registerNumber rawRegisterNumber/int -> int:
    if rawRegisterNumber >= 0: return rawRegisterNumber
    return -(rawRegisterNumber + 1) + firstCaptureRegister

  addToConstantPool codeUnit/int -> int:
    _extraConstants.add codeUnit
    return pattern.size + _extraConstants.size - 1

  pushBacktrack label/MiniExpLabel -> none:
    _emit PUSH_BACKTRACK
    link label

  backtrack -> none:
    _emit BACKTRACK

  push reg/int -> none:
    _emit PUSH_REGISTER
        registerNumber reg

  pop reg/int -> none:
    _emit POP_REGISTER
        registerNumber reg

  goto label/MiniExpLabel -> none:
    if _pendingGoto != label: flushPendingGoto
    _pendingGoto = label

  flushPendingGoto -> none:
    if _pendingGoto != null:
      _codes.add GOTO
      link _pendingGoto
      _pendingGoto = null

  backtrackIfEqual register1/int register2/int -> none:
    _emit BACKTRACK_EQ
         registerNumber register1
         registerNumber register2

  backtrackIfNotEqual register1/int register2/int -> none:
    _emit BACKTRACK_NE
        registerNumber register1
        registerNumber register2

  addToRegister reg/int offset/int -> none:
    _emit ADD_TO_REGISTER
        registerNumber reg
        offset

  copyRegister destRegister/int sourceRegister/int -> none:
    _emit COPY_REGISTER
        registerNumber destRegister
        registerNumber sourceRegister

  backtrackOnBackReferenceFail register/int caseSensitive/bool -> none:
    _emit BACKTRACK_ON_BACK_REFERENCE
        registerNumber register
        caseSensitive ? 1 : 0

  backtrackIfGreater register1/int register2/int -> none:
    _emit BACKTRACK_GT
        registerNumber register1
        registerNumber register2

  gotoIfGreaterEqual register1/int register2/int label/MiniExpLabel -> none:
    _emit GOTO_GE
        registerNumber register1
        registerNumber register2
    link label

  backtrackIfNoMatch constant_pool_offset/int -> none:
    _emit BACKTRACK_IF_NO_MATCH constant_pool_offset

  backtrackIfInRange from/int to/int -> none:
    _emit BACKTRACK_IF_IN_RANGE from to

  gotoIfMatches charCode/int label/MiniExpLabel -> none:
    _emit GOTO_IF_MATCH  charCode
    link label

  gotoIfInRange from/int to/int label/MiniExpLabel -> none:
    if from == to:
      gotoIfMatches from label
    else:
      _emit GOTO_IF_IN_RANGE from to
      link label

  backtrackIfNotAtWordBoundary -> none:
    non_word_on_left := MiniExpLabel
    word_on_left := MiniExpLabel
    at_word_boundary := MiniExpLabel
    do_backtrack := MiniExpLabel

    _emit GOTO_EQ CURRENT_POSITION ZERO_REGISTER
    link non_word_on_left
    _emit GOTO_IF_WORD_CHARACTER -1
    link word_on_left

    bind non_word_on_left
    _emit BACKTRACK_EQ CURRENT_POSITION STRING_LENGTH
    _emit GOTO_IF_WORD_CHARACTER 0
    link at_word_boundary
    bind do_backtrack
    backtrack

    bind word_on_left
    _emit GOTO_EQ CURRENT_POSITION STRING_LENGTH
    link at_word_boundary
    _emit GOTO_IF_WORD_CHARACTER 0
    link do_backtrack

    bind at_word_boundary

// MiniExpAnalysis objects reflect properties of an AST subtree.  They are
// immutable and are reused to some extent.
class MiniExpAnalysis:
  // Can this subtree match an empty string?  If we know that's not possible,
  // we can optimize away the test that ensures we are making progress when we
  // match repetitions.
  canMatchEmpty /bool ::= ?

  // Set to null if the AST does not match something with a fixed length.  That
  // fixed length thing has to be something that does not touch the stack.  This
  // is an important optimization that prevents .* from using huge amounts of
  // stack space when running.
  fixedLength /int? ::= ?

  // Can this subtree only match at the start of the regexp?  Can't pass all
  // tests without being able to spot this.
  anchored /bool ::= ?

  // Allows the AST to notify a surrounding loop (a quantifier higher up the
  // tree) that it has registers it expects to be saved on the back edge.
  registersToSave/List?/*<int>*/ ::= ?

  static combineRegisters left/List/*<int>*/ right/List/*<int>*/ -> List/*<int>*/:
    if right == null or right.is_empty:
      return left
    else if left == null or left.is_empty:
      return right
    else:
      return left + right  // List concatenation.

  static combineFixedLengths left/MiniExpAnalysis right/MiniExpAnalysis -> int?:
    if left.fixedLength == null or right.fixedLength == null:
      return null
    else:
      return left.fixedLength + right.fixedLength

  constructor.orr left/MiniExpAnalysis right/MiniExpAnalysis:
    canMatchEmpty = left.canMatchEmpty or right.canMatchEmpty
    // Even if both alternatives are the same length we can't handle a
    // disjunction without pushing backtracking information on the stack.
    fixedLength = null
    anchored = left.anchored and right.anchored
    registersToSave = combineRegisters left.registersToSave right.registersToSave

  constructor.andd left/MiniExpAnalysis right/MiniExpAnalysis:
    canMatchEmpty = left.canMatchEmpty and right.canMatchEmpty
    fixedLength = combineFixedLengths left right
    anchored = left.anchored
    registersToSave = combineRegisters left.registersToSave right.registersToSave

  constructor.empty:
    canMatchEmpty = true
    fixedLength = 0
    anchored = false
    registersToSave = null

  constructor.atStart:
    canMatchEmpty = true
    fixedLength = 0
    anchored = true
    registersToSave = null

  constructor.lookahead bodyAnalysis/MiniExpAnalysis positive/bool:
    canMatchEmpty = true
    fixedLength = 0
    anchored = positive and bodyAnalysis.anchored
    registersToSave = bodyAnalysis.registersToSave

  constructor.quantifier bodyAnalysis/MiniExpAnalysis min/int max/int regs/List/*<int>*/:
    canMatchEmpty = min == 0 or bodyAnalysis.canMatchEmpty
    fixedLength = (min == 1 and max == 1) ? bodyAnalysis.fixedLength : null
    anchored = min > 0 and bodyAnalysis.anchored
    registersToSave = combineRegisters bodyAnalysis.registersToSave regs

  constructor.atom:
    canMatchEmpty = false
    fixedLength = 1
    anchored = false
    registersToSave = null

  constructor.knowNothing:
    canMatchEmpty = true
    fixedLength = null
    anchored = false
    registersToSave = null

  constructor.capture bodyAnalysis/MiniExpAnalysis start/int end/int:
    canMatchEmpty = bodyAnalysis.canMatchEmpty
    // We can't generate a capture without pushing backtracking information
    // on the stack.
    fixedLength = null
    anchored = bodyAnalysis.anchored
    registersToSave = combineRegisters bodyAnalysis.registersToSave [start, end]

abstract class MiniExpAst:
  // When generating code for an AST, note that:
  // * The previous code may fall through to this AST, but it might also
  //   branch to it.  The label has always been bound just before generate
  //   is called.
  // * It's not permitted to fall through to the bottom of the generated
  //   code. Always end with backtrack or a goto onSuccess.
  // * You can push any number of backtrack pairs (PC, position), but if you
  //   push anything else, then you have to push a backtrack location that will
  //   clean it up.  On entry you can assume there is a backtrack pair on the
  //   top of the stack.
  abstract generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none

  abstract analyze compiler/MiniExpCompiler -> MiniExpAnalysis

  // Label is bound at the entry point for the AST tree.
  label ::= MiniExpLabel

class Disjunction extends MiniExpAst:
  _left/MiniExpAst ::= ?
  _right/MiniExpAst ::= ?

  constructor ._left ._right:

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    tryRight := MiniExpLabel
    compiler.pushBacktrack tryRight
    compiler.generate _left onSuccess
    compiler.bind tryRight
    compiler.generate _right onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.orr
        _left.analyze compiler
        _right.analyze compiler

class EmptyAlternative extends MiniExpAst:
  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.goto onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.empty

class Alternative extends MiniExpAst:
  _left/MiniExpAst ::= ?
  _right/MiniExpAst ::= ?

  constructor ._left ._right:

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.generate _left _right.label
    compiler.generate _right onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.andd
        _left.analyze compiler
        _right.analyze compiler

abstract class Assertion extends MiniExpAst:
  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.empty

class AtStart extends Assertion:
  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.backtrackIfNotEqual CURRENT_POSITION ZERO_REGISTER
    compiler.goto onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.atStart

class AtEnd extends Assertion:
  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.backtrackIfNotEqual CURRENT_POSITION STRING_LENGTH
    compiler.goto onSuccess

abstract class MultiLineAssertion extends Assertion:
  backtrackIfNotNewline compiler/MiniExpCompiler -> none:
    compiler.backtrackIfInRange
        '\r' + 1
        CHAR_CODE_LINE_SEPARATOR - 1
    compiler.backtrackIfInRange
        0
        '\n' - 1
    compiler.backtrackIfInRange 
        '\n' + 1
        '\r' - 1
    compiler.backtrackIfInRange
        CHAR_CODE_PARAGRAPH_SEPARATOR + 1
        0xffff

class AtBeginningOfLine extends MultiLineAssertion:
  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.gotoIfGreaterEqual ZERO_REGISTER CURRENT_POSITION onSuccess
    // We need to look one back to see if there was a newline there.  If we
    // backtrack, then that also restores the current position, but if we don't
    // backtrack, we have to fix it again.
    compiler.addToRegister CURRENT_POSITION -1
    backtrackIfNotNewline compiler
    compiler.addToRegister CURRENT_POSITION 1
    compiler.goto onSuccess

class AtEndOfLine extends MultiLineAssertion:
  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.gotoIfGreaterEqual CURRENT_POSITION STRING_LENGTH onSuccess
    backtrackIfNotNewline compiler
    compiler.goto onSuccess

class WordBoundary extends Assertion:
  _positive /bool ::= ?

  constructor ._positive:

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    // Positive word boundaries are much more common that negative ones, so we
    // will allow ourselves to generate some pretty horrible code for the
    // negative ones.
    if not _positive:
      compiler.pushBacktrack onSuccess
    compiler.backtrackIfNotAtWordBoundary
    if _positive:
      compiler.goto onSuccess
    else:
      // Pop the two stack position of the unneeded backtrack.
      compiler.pop CURRENT_POSITION
      compiler.pop CURRENT_POSITION
      // This overwrites the current position with the correct value.
      compiler.backtrack

class LookAhead extends Assertion:
  _positive /bool ::= ?
  _body/MiniExpAst ::= ?
  _subtreeRegisters/List?/*<int>*/ := null

  _savedStackPointerRegister/int
  _savedPosition/int

  constructor ._positive ._body compiler/MiniExpCompiler:
    _savedStackPointerRegister = compiler.allocateWorkingRegister
    _savedPosition = compiler.allocateWorkingRegister

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    // Lookahead.  Even if the subexpression succeeds, the current position is
    // reset, and the backtracking stack is unwound so that we can never
    // backtrack into the lookahead.  On a failure of the subexpression, the
    // stack will be naturally unwound.
    body_succeeded := MiniExpLabel
    succeed_on_failure := MiniExpLabel
    undoCaptures/MiniExpLabel? := null
    compiler.copyRegister _savedStackPointerRegister STACK_POINTER
    compiler.copyRegister _savedPosition CURRENT_POSITION
    if not _positive:
      compiler.pushBacktrack succeed_on_failure
    compiler.generate _body body_succeeded

    compiler.bind body_succeeded
    compiler.copyRegister STACK_POINTER _savedStackPointerRegister
    compiler.copyRegister CURRENT_POSITION _savedPosition
    if not _positive:
      // For negative lookahead always zap the captures when the body succeeds
      // and the lookahead thus fails.  The captures are only needed for any
      // backrefs inside the negative lookahead.
      if _subtreeRegisters != null:
        _subtreeRegisters.do: | register |
          compiler.copyRegister register NO_POSITION_REGISTER
      compiler.backtrack
      compiler.bind succeed_on_failure
    else:
      // For positive lookahead, the backtrack stack has been unwound, because
      // we don't ever backtrack into a lookahead, but if we backtrack past
      // this point we have to undo any captures that happened in there.
      // Register a backtrack to do that before continuing.
      if _subtreeRegisters != null and not _subtreeRegisters.is_empty:
        undoCaptures = MiniExpLabel
        compiler.pushBacktrack undoCaptures

    compiler.goto onSuccess

    if undoCaptures != null:
      compiler.bind undoCaptures
      _subtreeRegisters.do: | register |
        compiler.copyRegister register NO_POSITION_REGISTER
      compiler.backtrack

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    bodyAnalysis/MiniExpAnalysis := _body.analyze compiler
    _subtreeRegisters = bodyAnalysis.registersToSave
    return MiniExpAnalysis.lookahead bodyAnalysis _positive

class Quantifier extends MiniExpAst:
  _min /int ::= ?
  _max /int ::= ?
  _greedy /bool ::= ?
  _body/MiniExpAst ::= ?
  _counterRegister/int := -1
  _startOfMatchRegister/int? := null  // Implements 21.2.2.5.1 note 4.
  _minRegister/int := -1
  _maxRegister/int := -1
  _subtreeRegistersThatNeedSaving/List?/*<int>*/ := null
  _optimizedGreedyRegister/int? := null
  _savePositionRegister/int? := null
  _bodyLength/int? := null

  _isOptimizedGreedy -> bool: return _optimizedGreedyRegister != null

  constructor
      ._min
      ._max
      ._greedy
      ._body
      compiler/MiniExpCompiler:
    if _counterCheck:
      _counterRegister = compiler.allocateWorkingRegister
      _minRegister = compiler.allocateConstantRegister _min
      _maxRegister = compiler.allocateConstantRegister _max

  // We fall through to the top of this, when it is time to match the body of
  // the quantifier.  If the body matches successfully, we should go to
  // onBodySuccess, otherwise clean up and backtrack.
  generateCommon compiler/MiniExpCompiler onBodySuccess/MiniExpLabel -> none:
    needToCatchDidntMatch/bool := _greedy or _bodyCanMatchEmpty or
        _counterCheck or _saveAndRestoreRegisters
    didntMatch := MiniExpLabel

    if _saveAndRestoreRegisters:
      _subtreeRegistersThatNeedSaving.do: | reg |
        compiler.push reg
        compiler.copyRegister reg NO_POSITION_REGISTER

    if _bodyCanMatchEmpty:
      compiler.push _startOfMatchRegister
      compiler.copyRegister _startOfMatchRegister CURRENT_POSITION

    if needToCatchDidntMatch:
      compiler.pushBacktrack didntMatch

    if _counterCheck:
      compiler.addToRegister _counterRegister 1
      if _maxCheck:
        compiler.backtrackIfGreater _counterRegister _maxRegister

    compiler.generate _body onBodySuccess

    if needToCatchDidntMatch:
      compiler.bind didntMatch
      if _bodyCanMatchEmpty:
        compiler.pop _startOfMatchRegister
      if _counterCheck:
        compiler.addToRegister _counterRegister -1
      if _saveAndRestoreRegisters:
        for i := _subtreeRegistersThatNeedSaving.size - 1; i >= 0; --i:
          compiler.pop _subtreeRegistersThatNeedSaving[i]
      if not _greedy: compiler.backtrack

  generateFixedLengthGreedy compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    newIteration := MiniExpLabel
    cantAdvanceMore := MiniExpLabel
    continuationFailed := MiniExpLabel

    // Save the current position, so we know when the quantifier has been
    // unwound enough.
    compiler.copyRegister _optimizedGreedyRegister CURRENT_POSITION
    if _min != 0:
      compiler.addToRegister
          _optimizedGreedyRegister
          _min * _bodyLength

    // This backtrack doesn't trigger until the quantifier has eaten as much as
    // possible.  Unfortunately, whenever we backtrack in this simple system,
    // the old current position in the subject string is also rewound.  That's
    // not so convenient here, so we will have to save the current position in
    // a special register.  It would be faster to generate the body in a
    // special mode where we use a different backtrack instruction that just
    // rewinds the current position a certain number of characters instead of
    // popping it.
    compiler.pushBacktrack cantAdvanceMore

    // The loop.
    compiler.bind newIteration
    compiler.copyRegister _savePositionRegister CURRENT_POSITION
    compiler.generate _body newIteration

    // The greedy quantifier has eaten as much as it can.  Time to try the
    // continuation of the regexp after the quantifier.
    compiler.bind cantAdvanceMore

    if _min != 0:
      compiler.backtrackIfGreater _optimizedGreedyRegister _savePositionRegister

    compiler.addToRegister _savePositionRegister _bodyLength
    compiler.copyRegister CURRENT_POSITION _savePositionRegister

    // The continuation of the regexp failed.  We backtrack the greedy
    // quantifier by one step and retry.
    compiler.bind continuationFailed
    compiler.addToRegister CURRENT_POSITION -_bodyLength
    // If we got back to where the quantifier started matching, then jump
    // to the continuation (we haven't pushed a backtrack, so if that fails, it
    // will backtrack further).
    // We don't have gotoIfEqual, so use gotoIfGreaterEqual.
    compiler.gotoIfGreaterEqual _optimizedGreedyRegister CURRENT_POSITION onSuccess
    compiler.pushBacktrack continuationFailed
    compiler.goto onSuccess

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    // We optimize loops of the form .* to avoid big backtrack stacks.
    if _isOptimizedGreedy:
      generateFixedLengthGreedy compiler onSuccess
      return
    if _min == 1 and _max == 1:
      compiler.generate _body onSuccess
      return
    // The above means if _max is 1 then _min must be 0, which simplifies
    // things.
    bodyMatched/MiniExpLabel := _max == 1 ? onSuccess : MiniExpLabel
    checkEmptyMatchLabel/MiniExpLabel? := null
    onBodySuccess/MiniExpLabel := bodyMatched
    if _bodyCanMatchEmpty:
      checkEmptyMatchLabel = MiniExpLabel
      onBodySuccess = checkEmptyMatchLabel
    if _counterCheck:
      compiler.copyRegister _counterRegister ZERO_REGISTER

    if bodyMatched != onSuccess:
      compiler.bind bodyMatched

    if _greedy:
      generateCommon compiler onBodySuccess

      if _minCheck:
        compiler.gotoIfGreaterEqual _counterRegister _minRegister onSuccess
        compiler.backtrack
      else:
        compiler.goto onSuccess
    else:
      // Non-greedy.
      tryBody := MiniExpLabel

      if _minCheck:
        // If there's a minimum and we haven't reached it we should not try to
        // run the continuation, but go straight to the _body.
        // TODO(erikcorry): if we had a gotoIfLess we could save instructions
        // here.
        jumpToContinuation := MiniExpLabel
        compiler.gotoIfGreaterEqual _counterRegister _minRegister jumpToContinuation
        compiler.goto tryBody
        compiler.bind jumpToContinuation
      // If the continuation fails, we can try the _body once more.
      compiler.pushBacktrack tryBody
      compiler.goto onSuccess

      // We failed to match the continuation, so lets match the _body once more
      // and then try again.
      compiler.bind tryBody
      generateCommon compiler onBodySuccess

    if _bodyCanMatchEmpty:
      compiler.bind checkEmptyMatchLabel
      if _minCheck:
        compiler.gotoIfGreaterEqual _minRegister _counterRegister bodyMatched
      compiler.backtrackIfEqual _startOfMatchRegister CURRENT_POSITION
      compiler.goto bodyMatched

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    bodyAnalysis/MiniExpAnalysis := _body.analyze compiler
    _subtreeRegistersThatNeedSaving = bodyAnalysis.registersToSave
    _bodyLength = bodyAnalysis.fixedLength
    if bodyAnalysis.canMatchEmpty:
      _startOfMatchRegister = compiler.allocateWorkingRegister
    else if _max == null and _greedy and _bodyLength != null:
      // This also put us in a mode where code is generated differently for
      // this AST.
      _optimizedGreedyRegister = compiler.allocateWorkingRegister
      _savePositionRegister = compiler.allocateWorkingRegister
    myRegs/List/*<int>*/ := [_counterRegister, _optimizedGreedyRegister, _savePositionRegister].filter: it != null
    return MiniExpAnalysis.quantifier bodyAnalysis _min _max myRegs

  _maxCheck -> bool: return _max != 1 and _max != null

  _minCheck -> bool: return _min != 0

  _counterCheck -> bool: return _maxCheck or _minCheck

  _bodyCanMatchEmpty -> bool: return _startOfMatchRegister != null

  _saveAndRestoreRegisters:
    return _subtreeRegistersThatNeedSaving != null and
           not _subtreeRegistersThatNeedSaving.is_empty

class Atom extends MiniExpAst:
  _constantIndex /int ::= ?

  constructor ._constantIndex:

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.backtrackIfEqual CURRENT_POSITION STRING_LENGTH
    match/MiniExpLabel? := null
    charCode/int := compiler.constantPoolEntry _constantIndex
    if not compiler.caseSensitive:
      // TODO: Non-ASCII case insensitivity.
      if 'a' <= charCode <= 'z' or 'A' <= charCode <= 'Z':
        match = MiniExpLabel
        compiler.gotoIfMatches (charCode ^ 0x20) match
    compiler.backtrackIfNoMatch _constantIndex
    if match != null: compiler.bind match
    compiler.addToRegister CURRENT_POSITION 1
    compiler.goto onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.atom

class CharClass extends MiniExpAst:
  _ranges /List ::= []
  _positive /bool ::= ?

  constructor ._positive:

  // Here and elsewhere, "to" is inclusive.
  add from/int to/int -> none:
    _ranges.add from
    _ranges.add to

  static _spaceCodes/List/*<int>*/ := [
    -1,
    '\t', '\r',
    ' ', ' ',
    CHAR_CODE_NO_BREAK_SPACE, CHAR_CODE_NO_BREAK_SPACE,
    CHAR_CODE_OGHAM_SPACE_MARK, CHAR_CODE_OGHAM_SPACE_MARK,
    CHAR_CODE_EN_QUAD, CHAR_CODE_HAIR_SPACE,
    CHAR_CODE_LINE_SEPARATOR, CHAR_CODE_PARAGRAPH_SEPARATOR,
    CHAR_CODE_NARROW_NO_BREAK_SPACE, CHAR_CODE_NARROW_NO_BREAK_SPACE,
    CHAR_CODE_MEDIUM_MATHEMATICAL_SPACE, CHAR_CODE_MEDIUM_MATHEMATICAL_SPACE,
    CHAR_CODE_IDEOGRAPHIC_SPACE, CHAR_CODE_IDEOGRAPHIC_SPACE,
    CHAR_CODE_ZERO_WIDTH_NO_BREAK_SPACE, CHAR_CODE_ZERO_WIDTH_NO_BREAK_SPACE,
    0x10000]

  addSpaces -> none:
    for i := 1; i < _spaceCodes.size - 1; i += 2:
      add _spaceCodes[i] _spaceCodes[i + 1]

  addNotSpaces -> none:
    for i := 0; i < _spaceCodes.size; i += 2:
      add _spaceCodes[i] + 1
          _spaceCodes[i + 1] - 1

  addSpecial charCode/int -> none:
    if charCode == 'd':
      add '0' '9'
    else if charCode == 'D':
      add 0
          '0' - 1
      add '9' + 1
          0xffff
    else if charCode == 's':
      addSpaces
    else if charCode == 'S':
      addNotSpaces
    else if charCode == 'w':
      add '0' '9'
      add 'A' 'Z'
      add '_' '_'
      add 'a' 'z'
    else if charCode == 'W':
      add 0
          '0' - 1 
      add '9' + 1
          'A' - 1
      add 'Z' + 1
          '_' - 1
      add '_' + 1
          'a' - 1
      add 'z' + 1
          0xffff

  caseInsensitiveRanges oldRanges/List/*<int>*/ -> List/*<int>*/:
    ranges/List/*<int>*/ := []
    // TODO: Non-ascii.
    letters := List 27: false  // 26 letters of alphabet plus a "known elephant in Cairo".
    for i := 0; i < oldRanges.size; i += 2:
      start/int := oldRanges[i]
      end/int := oldRanges[i + 1]
      if start <= 'z' and end >= 'A':
        for j := start; j <= end; j++:
          if 'A' <= j <= 'Z':
            letters[j - 'A'] = true
          else if 'a' <= j <= 'z':
            letters[j - 'a'] = true
    for i := 0; i < oldRanges.size; i += 2:
      start/int := oldRanges[i]
      end/int := oldRanges[i + 1]

      // Any part before 'A':
      e := min end 'A' - 1
      if start <= e:
        ranges.add start
        ranges.add e
      // Any part between 'Z' and 'a':
      s := max start 'Z' + 1
      e = min end 'a' - 1
      if s <= e:
        ranges.add s
        ranges.add e
      // Any part after 'z':
      s = max start 'z' + 1
      if s <= end:
        ranges.add s
        ranges.add end
    start := -1
    for i := 0; i < 27; i++:
      if letters[i]:
        if start == -1:
          start = i
      else:
        if start != -1:
          ranges.add 'A' + start
          ranges.add 'A' + i - 1
          ranges.add 'a' + start
          ranges.add 'a' + i - 1
          start = -1
    // TODO(erikcorry): Sort and merge ranges.
    return ranges

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    compiler.backtrackIfEqual CURRENT_POSITION STRING_LENGTH
    ranges/List/*<int>*/ := _ranges
    if not compiler.caseSensitive:
      ranges = caseInsensitiveRanges _ranges
    match := MiniExpLabel
    if _positive:
      for i := 0; i < ranges.size; i += 2:
        compiler.gotoIfInRange ranges[i] ranges[i + 1] match
      compiler.backtrack
      compiler.bind match
    else:
      for i := 0; i < ranges.size; i += 2:
        compiler.backtrackIfInRange ranges[i] ranges[i + 1]
    compiler.addToRegister CURRENT_POSITION 1
    compiler.goto onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    return MiniExpAnalysis.atom

// This class is used for all backslashes followed by numbers.  For web
// compatibility, if the number (interpreted as decimal) is smaller than the
// number of captures, then it will be interpreted as a back reference.
// Otherwise the number will be interpreted as an octal character code escape.
class BackReference extends MiniExpAst:
  _backReferenceIndex/string
  _register/int? := null
  _astThatReplacesUs/MiniExpAst? := null

  constructor ._backReferenceIndex:

  index -> string: return _backReferenceIndex

  set register r/int -> none:
    _register = r

  replaceWithAst ast/MiniExpAst -> none:
    _astThatReplacesUs = ast

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    if _register == null:
      compiler.generate _astThatReplacesUs onSuccess
    else:
      compiler.backtrackOnBackReferenceFail _register compiler.caseSensitive
      compiler.goto onSuccess

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    compiler.addBackReference this
    return MiniExpAnalysis.knowNothing

class Capture extends MiniExpAst:
  _captureCount /int ::= ?
  _body/MiniExpAst ::= ?
  _startRegister/int? := null
  _endRegister/int? := null

  constructor ._captureCount ._body:

  allocateRegisters compiler/MiniExpCompiler -> none:
    if _startRegister == null:
      _startRegister = compiler.allocateCaptureRegisters
      _endRegister = _startRegister - 1

  generate compiler/MiniExpCompiler onSuccess/MiniExpLabel -> none:
    undoStart := MiniExpLabel
    writeEnd := MiniExpLabel
    undoEnd := MiniExpLabel
    compiler.copyRegister _startRegister CURRENT_POSITION
    compiler.pushBacktrack undoStart

    compiler.generate _body writeEnd

    compiler.bind writeEnd
    compiler.copyRegister _endRegister CURRENT_POSITION
    compiler.pushBacktrack undoEnd
    compiler.goto onSuccess

    compiler.bind undoStart
    compiler.copyRegister _startRegister NO_POSITION_REGISTER
    compiler.backtrack

    compiler.bind undoEnd
    compiler.copyRegister _endRegister NO_POSITION_REGISTER
    compiler.backtrack

  analyze compiler/MiniExpCompiler -> MiniExpAnalysis:
    allocateRegisters compiler
    bodyAnalysis := _body.analyze compiler
    return MiniExpAnalysis.capture bodyAnalysis _startRegister _endRegister

interface Match:
  pattern -> RegExp
  input -> string
  start -> int
  end -> int
  group index/int -> string?
  operator [] index/int -> string

class MiniExpMatch implements Match:
  pattern/RegExp ::= ?
  input /string ::= ?
  _registers/List/*<int>*/ ::= ?
  _firstCaptureReg /int ::= ?

  constructor .pattern .input ._registers ._firstCaptureReg:

  groupCount -> int: return (_registers.size - 2 - _firstCaptureReg) >> 1

  start -> int: return _registers[_firstCaptureReg]

  end -> int: return _registers[_firstCaptureReg + 1]

  group index/int -> string?:
    if index > groupCount:
      throw "OUT_OF_RANGE"
    index *= 2
    index += _firstCaptureReg
    if _registers[index] == NO_POSITION: return null
    return input.copy _registers[index] _registers[index + 1]

  operator[] index/int -> string?: return group index

interface RegExp:
  pattern -> string
  is_multiline -> bool
  is_case_sensitive -> bool
  match_as_prefix a/string a1/int -> Match
  first_matching a/string -> Match
  has_matching a/string -> bool
  string_matching a/string -> string?
  all_matches subject/string start/int [block] -> bool

class _MiniExp implements RegExp:
  _byteCodes/List?/*<int>*/ := null
  _initialRegisterValues/List?/*<int>*/ := null
  _firstCaptureRegister/int? := null
  _stickyEntryPoint/int? := null
  _constantPool/string? := null
  pattern /string ::= ?
  is_multiline /bool ::= ?
  is_case_sensitive /bool ::= ?

  constructor .pattern/string .is_multiline .is_case_sensitive:
    compiler := MiniExpCompiler pattern is_case_sensitive
    parser := MiniExpParser compiler pattern is_multiline
    ast/MiniExpAst := parser.parse
    _generateCode compiler ast pattern

  match_as_prefix a/string a1/int=0 -> Match?:
    return _match a a1 _stickyEntryPoint

  first_matching a/string -> Match?:
    return _match a 0 0

  has_matching a/string -> bool:
    return (_match a 0 0) != null

  string_matching a/string -> string?:
    m/Match := _match a 0 0
    if m == null: return null
    return m[0]

  /**
  Calls the block once for each match with the match as argument.
  Returns true iff there was at least one match.
  */
  all_matches subject/string start/int=0 [block] -> bool:
    at_least_once := false
    position := start
    if not 0 <= position <= subject.size: throw "OUT_OF_RANGE"
    while position <= subject.size:
      current := _match subject position 0
      if current == null: return at_least_once
      if current.start == current.end:
        position = current.end + 1
      else:
        position = current.end
      block.call current
      at_least_once = true
    return at_least_once

  _match a/string startPosition/int startProgramCounter/int -> Match?:
    registers/List/*<int>*/ := _initialRegisterValues.copy
    interpreter := MiniExpInterpreter _byteCodes _constantPool registers
    if not interpreter.interpret a startPosition startProgramCounter:
      return null
    return MiniExpMatch this a registers _firstCaptureRegister

  _generateCode compiler/MiniExpCompiler ast/MiniExpAst source/string -> none:
    // Top level capture regs.
    topLevelCaptureReg/int := compiler.allocateCaptureRegisters

    topAnalysis/MiniExpAnalysis := ast.analyze compiler

    compiler.addCaptureRegisters

    stickyEntryPoint := MiniExpLabel
    stickyStart := MiniExpLabel
    failSticky := MiniExpLabel

    start := MiniExpLabel
    compiler.bind start

    fail := MiniExpLabel
    compiler.pushBacktrack fail

    compiler.bind stickyStart
    compiler.copyRegister topLevelCaptureReg CURRENT_POSITION

    succeed := MiniExpLabel
    compiler.generate ast succeed

    compiler.bind fail
    if not topAnalysis.anchored:
      end := MiniExpLabel
      compiler.gotoIfGreaterEqual CURRENT_POSITION STRING_LENGTH end
      compiler.addToRegister CURRENT_POSITION 1
      compiler.goto start
      compiler.bind end
    compiler.bind failSticky
    compiler.fail

    compiler.bind succeed
    compiler.copyRegister topLevelCaptureReg - 1 CURRENT_POSITION
    compiler.succeed

    compiler.bind stickyEntryPoint
    compiler.pushBacktrack failSticky
    compiler.goto stickyStart

    _byteCodes = compiler.codes
    _constantPool = compiler.constantPool
    _initialRegisterValues = compiler.registers
    _firstCaptureRegister = compiler.firstCaptureRegister
    _stickyEntryPoint = stickyEntryPoint.location

// Lexer tokens.
NONE ::= 0
QUANT ::= 1
BACKSLASH ::= 2
DOT ::= 3
L_PAREN ::= 4
R_PAREN ::= 5
L_SQUARE ::= 6
HAT ::= 7
DOLLAR ::= 8
PIPE ::= 9
BACK_REFERENCE ::= 10
WORD_BOUNDARY ::= 11
NOT_WORD_BOUNDARY ::= 12
WORD_CHARACTER ::= 13
NOT_WORD_CHARACTER ::= 14
DIGIT ::= 15
NOT_DIGIT ::= 16
WHITESPACE ::= 17
NOT_WHITESPACE ::= 18
NON_CAPTURING ::= 19
LOOK_AHEAD ::= 20
NEGATIVE_LOOK_AHEAD ::= 21
OTHER ::= 22

class MiniExpParser:
  // The constant pool is used to look up character data when the regexp is
  // running.  It consists of the regexp source with some characters appended
  // to handle escapes that are not literally present in the regexp input.
  _compiler /MiniExpCompiler ::= ?
  _source /string ::= ?
  _isMultiLine /bool ::= ?

  _captureCount /int := 0
  _constantPool /string := ""

  // State of the parser and lexer.
  _position/int := 0  // Location in source.
  _lastToken/int := -1
  // This is the offset in the constant pool of the character data associated
  // with the token.
  _lastTokenIndex/int := -1
  // Greedyness of the last single-character quantifier.
  _lastWasGreedy/bool := false
  _lastBackReferenceIndex/string? := null
  _minimumRepeats/int? := null
  _maximumRepeats/int? := null

  constructor ._compiler ._source ._isMultiLine:

  parse -> MiniExpAst:
    getToken
    ast/MiniExpAst := parseDisjunction
    expectToken NONE
    return ast

  _at _position/int -> int: return _source[_position]

  _has _position/int -> bool: return _source.size > _position

  error message/string -> none:
    throw
      FormatException "Error while parsing regexp: $message" _source _position

  parseDisjunction -> MiniExpAst:
    ast/MiniExpAst := parseAlternative
    while acceptToken PIPE:
      ast = Disjunction ast parseAlternative
    return ast

  endOfAlternative -> bool:
    return _lastToken == PIPE or _lastToken == R_PAREN or
        _lastToken == NONE

  parseAlternative -> MiniExpAst:
    if endOfAlternative:
      return EmptyAlternative
    ast/MiniExpAst := parseTerm
    while not endOfAlternative:
      ast = Alternative ast parseTerm
    return ast

  tryParseAssertion -> MiniExpAst?:
    if acceptToken HAT:
      return _isMultiLine ? AtBeginningOfLine : AtStart
    if acceptToken DOLLAR:
      return _isMultiLine ? AtEndOfLine : AtEnd
    if acceptToken WORD_BOUNDARY: return WordBoundary true
    if acceptToken NOT_WORD_BOUNDARY: return WordBoundary false
    lookaheadAst/MiniExpAst? := null
    if acceptToken LOOK_AHEAD:
      lookaheadAst = LookAhead true parseDisjunction _compiler
    else if acceptToken NEGATIVE_LOOK_AHEAD:
      lookaheadAst = LookAhead false parseDisjunction _compiler
    if lookaheadAst != null:
      expectToken R_PAREN
      // The normal syntax does not allow a quantifier here, but the web
      // compatible one does.  Slightly nasty hack for compatibility:
      if peekToken QUANT:
        quant/MiniExpAst := Quantifier _minimumRepeats _maximumRepeats _lastWasGreedy lookaheadAst _compiler
        expectToken QUANT
        return quant
      return lookaheadAst
    return null

  parseTerm -> MiniExpAst:
    ast/MiniExpAst := tryParseAssertion
    if ast == null:
      ast = parseAtom
      if peekToken QUANT:
        quant/MiniExpAst := Quantifier _minimumRepeats _maximumRepeats _lastWasGreedy ast _compiler
        expectToken QUANT
        return quant
    return ast

  parseAtom -> MiniExpAst?:
    if peekToken OTHER:
      result := Atom _lastTokenIndex
      expectToken OTHER
      return result
    if acceptToken DOT:
      ast := CharClass false  // Negative char class.
      ast.add '\n' '\n'
      ast.add '\r' '\r'
      ast.add CHAR_CODE_LINE_SEPARATOR CHAR_CODE_PARAGRAPH_SEPARATOR
      return ast

    if peekToken BACK_REFERENCE:
      backRef := BackReference _lastBackReferenceIndex
      expectToken BACK_REFERENCE
      return backRef

    if acceptToken L_PAREN:
      ast/MiniExpAst := parseDisjunction
      ast = Capture _captureCount++ ast
      expectToken R_PAREN
      return ast
    if acceptToken NON_CAPTURING:
      ast := parseDisjunction
      expectToken R_PAREN
      return ast

    charClass/CharClass? := null
    digitCharClass := false
    if acceptToken WORD_CHARACTER:
      charClass = CharClass true
    else if acceptToken NOT_WORD_CHARACTER:
      charClass = CharClass false
    else if acceptToken DIGIT:
      charClass = CharClass true
      digitCharClass = true
    else if acceptToken NOT_DIGIT:
      charClass = CharClass false
      digitCharClass = true
    if charClass != null:
      charClass.add '0' '9'
      if not digitCharClass:
        charClass.add 'A' 'Z'
        charClass.add '_' '_'
        charClass.add 'a' 'z'
      return charClass

    if acceptToken WHITESPACE:
      charClass = CharClass true
    else if acceptToken NOT_WHITESPACE:
      charClass = CharClass false
    if charClass != null:
      charClass.addSpaces
      return charClass
    if peekToken L_SQUARE:
      return parseCharacterClass
    if peekToken NONE: error "Unexpected end of regexp"
    error "Unexpected token $_lastToken"
    return null

  parseCharacterClass -> MiniExpAst:
    charClass/CharClass := ?

    if (_has _position) and (_at _position) == '^':
      _position++
      charClass = CharClass false
    else:
      charClass = CharClass true

    addCharCode := : | code |
      if code < 0:
        charClass.addSpecial(_at(-code + 1))
      else:
        charClass.add code code

    while _has _position:
      code/int := _at _position
      degenerateRange := false
      if code == ']':
        // End of character class.  This reads the terminating square bracket.
        getToken
        break
      // Single character or escape code representing a single character.
      code = _readCharacterClassCode

      // Check if there are at least 2 more characters and the next is a dash.
      if (not _has _position + 1) or
         (_at _position) != '-' or
         (_at _position + 1) == ']':
        // No dash-something here, so it's not part of a range.  Add the code
        // and move on.
        addCharCode.call code
        continue
      // Found a dash, try to parse a range.
      _position++;  // Skip the dash.
      code2/int := _readCharacterClassCode
      if code < 0 or code2 < 0:
        // One end of the range is not a single character, so the range is
        // degenerate.  We add either and and the dash, instead of a range.
        addCharCode.call code
        charClass.add '-' '-'
        addCharCode.call code2
      else:
        // Found a range.
        if code > code2: error "Character range out of order"
        charClass.add code code2
    expectToken OTHER;  // The terminating right square bracket.
    return charClass

  // Returns a character (possibly from a parsed escape) or a negative number
  // indicating the position of a character class special \s \d or \w.
  _readCharacterClassCode -> int:
    code/int := _at _position
    if code != '\\':
      _position++
      return code
    if not _has _position + 1: error "Unexpected end of regexp"
    code2/int := _at _position + 1
    lower/int := code2 | 0x20
    if (lower == 'd' or lower == 's' or lower == 'w'):
      answer := -_position
      _position += 2
      return answer
    if code2 == 'c':
      // For web compatibility, the set of characters that can follow \c inside
      // a character class is different from the a-zA-Z that are allowed outside
      // a character class.
      if _has(_position + 2) and isBackslashCCharacter(_at(_position + 2)):
        _position += 3
        return _at(_position - 1) % 32
      // This makes little sense, but for web compatibility, \c followed by an
      // invalid character is interpreted as a literal backslash, followed by
      // the "c", etc.
      _position++
      return '\\'
    if '0' <= code2 and code2 <= '9':
      _position++
      return lexInteger 8 0x100
    _position += 2
    if code2 == 'u':
      code = lexHex 4
    else if code2 == 'x':
      code = lexHex 2
    else if CONTROL_CHARACTERS.containsKey(code2):
      code = CONTROL_CHARACTERS[code2]
    else:
      code = code2
    // In the case of a malformed escape we just interpret as if the backslash
    // was not there.
    if code == -1: code = code2
    return code

  expectToken token/int -> none:
    if token != _lastToken:
      error "At _position $(_position - 1) expected $token, found $_lastToken"
    getToken

  acceptToken token/int -> bool:
    if token == _lastToken:
      getToken
      return true
    return false

  peekToken token/int -> bool: return token == _lastToken

  static CHARCODE_TO_TOKEN ::= [
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
    L_PAREN, R_PAREN, QUANT, QUANT,  // ()*+,
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
    OTHER, OTHER, OTHER, L_SQUARE,   // XYZ[
    BACKSLASH, OTHER, HAT, OTHER,    // \]^_
    OTHER, OTHER, OTHER, OTHER,      // `abc
    OTHER, OTHER, OTHER, OTHER,      // defg
    OTHER, OTHER, OTHER, OTHER,      // hijk
    OTHER, OTHER, OTHER, OTHER,      // lmno
    OTHER, OTHER, OTHER, OTHER,      // pqrs
    OTHER, OTHER, OTHER, OTHER,      // tuvw
    OTHER, OTHER, OTHER, QUANT,      // xyz{
    PIPE, OTHER]                     // |}

  static ESCAPES ::= {
    'b': WORD_BOUNDARY,
    'B': NOT_WORD_BOUNDARY,
    'w': WORD_CHARACTER,
    'W': NOT_WORD_CHARACTER,
    'd': DIGIT,
    'D': NOT_DIGIT,
    's': WHITESPACE,
    'S': NOT_WHITESPACE,
  }

  static CONTROL_CHARACTERS ::= {
    'b': CHAR_CODE_BACKSPACE,
    'f': CHAR_CODE_FORM_FEED,
    'n': '\n',
    'r': '\r',
    't': '\t',
    'v': CHAR_CODE_VERTICAL_TAB,
  }

  static tokenFromCharcode code/int -> int:
    if code >= CHARCODE_TO_TOKEN.size: return OTHER
    return CHARCODE_TO_TOKEN[code]

  onDigit _position/int -> bool:
    if not _has _position: return false
    if (_at _position) < '0': return false
    return (_at _position) <= '9'

  getToken -> none:
    if not _has _position:
      _lastToken = NONE
      return
    _lastTokenIndex = _position
    code/int := _at _position
    _lastToken = tokenFromCharcode code
    token := _lastToken
    if token == BACKSLASH:
      lexBackslash
      return
    if token == L_PAREN:
      lexLeftParenthesis
    else if token == QUANT:
      lexQuantifier
    _position++

  // This may be a bug in Irregexp, but there are tests for it: \c_ and \c0
  // work like \cc which means Control-C.  But only in character classes.
  static isBackslashCCharacter code/int -> bool:
    if isAsciiLetter code: return true
    if '0' <= code <= '9': return true
    return code == '_'

  static isAsciiLetter code/int -> bool:
    if 'A' <= code <= 'Z': return true
    return 'a' <= code <= 'z'

  lexBackslash -> none:
    if not _has(_position + 1): error "\\ at end of pattern"
    nextCode/int := _at _position + 1
    if ESCAPES.containsKey nextCode:
      _position += 2
      _lastToken = ESCAPES[nextCode]
    else if CONTROL_CHARACTERS.containsKey nextCode:
      _position += 2
      _lastToken = OTHER
      _lastTokenIndex =
          _compiler.addToConstantPool CONTROL_CHARACTERS[nextCode]
    else if nextCode == 'c':
       if (_has _position + 2) and (isAsciiLetter (_at _position + 2)):
         _lastToken = OTHER
         _lastTokenIndex = _compiler.addToConstantPool (_at _position + 2) % 32
         _position += 3
       else:
         // \c_ is interpreted as a literal backslash and literal "c_".
         _lastToken = OTHER
         _lastTokenIndex = _position
         _position++
    else if onDigit _position + 1:
      _position++
      _lastBackReferenceIndex = lexIntegerAsString
      _lastToken = BACK_REFERENCE
    else if nextCode == 'x' or nextCode == 'u':
      _position += 2
      _lastToken = OTHER
      codeUnit := lexHex(nextCode == 'x' ? 2 : 4)
      if codeUnit == -1:
        _lastTokenIndex = _position - 1
      else:
        _lastTokenIndex = _compiler.addToConstantPool codeUnit
    else:
      _lastToken = OTHER
      _lastTokenIndex = _position + 1
      _position += 2

  lexHex chars/int -> int:
    if not _has _position + chars - 1: return -1
    total/int := 0
    for i := 0; i < chars; i++:
      total *= 16
      charCode := _at _position + i
      if charCode >= '0' and charCode <= '9':
        total += charCode - '0'
      else if (charCode >= 'A' and
                 charCode <= 'F'):
        total += 10 + charCode - 'A'
      else if (charCode >= 'a' and
                 charCode <= 'f'):
        total += 10 + charCode - 'a'
      else:
        return -1
    _position += chars
    return total

  lexIntegerAsString -> string:
    b := Buffer
    while true:
      if not _has _position: return b.to_string
      code := _at _position
      if code >= '0' and code <= '9':
        b.put_byte code
        _position++
      else:
        return b.to_string

  lexInteger base/int max/int? -> int:
    total/int := 0
    while true:
      if not _has _position: return total
      code := _at _position
      if code >= '0' and code < '0' + base and (max == null or total * base < max):
        _position++
        total *= base
        total += code - '0'
      else:
        return total

  lexLeftParenthesis -> none:
    if not _has _position + 1: error "unterminated group"
    if (_at _position + 1) == '?':
      if not _has _position + 2: error "unterminated group"
      parenthesisModifier/int := _at _position + 2
      if parenthesisModifier == '=':
        _lastToken = LOOK_AHEAD
      else if parenthesisModifier == ':':
        _lastToken = NON_CAPTURING
      else if parenthesisModifier == '!':
        _lastToken = NEGATIVE_LOOK_AHEAD
      else:
        error "invalid group"
      _position += 2
      return

  lexQuantifier -> none:
    quantifierCode/int := _at _position
    if quantifierCode == '{':
      parsedRepeats := false
      savedPosition := _position
      if onDigit(_position + 1):
        _position++
        // We parse the repeats in the lexer.  Forms allowed are {n}, {n,}
        // and {n,m}.
        _minimumRepeats = lexInteger 10 null
        if _has _position:
          if (_at _position) == '}':
            _maximumRepeats = _minimumRepeats
            parsedRepeats = true
          else if (_at _position) == ',':
            _position++
            if _has _position:
              if (_at _position) == '}':
                _maximumRepeats = null;  // No maximum.
                parsedRepeats = true
              else if onDigit _position:
                _maximumRepeats = lexInteger 10 null
                if (_has _position) and (_at _position) == '}':
                  parsedRepeats = true
      if parsedRepeats:
        if _maximumRepeats != null and _minimumRepeats > _maximumRepeats:
          error "numbers out of order in {} quantifier"
      else:
        // If parsing of the repeats fails then we follow JS in interpreting
        // the left brace as a literal.
        _position = savedPosition
        _lastToken = OTHER
        return
    else if quantifierCode == '*':
      _minimumRepeats = 0
      _maximumRepeats = null;  // No maximum.
    else if quantifierCode == '+':
      _minimumRepeats = 1
      _maximumRepeats = null;  // No maximum.
    else:
      _minimumRepeats = 0
      _maximumRepeats = 1
    if (_has _position + 1) and (_at _position + 1) == '?':
      _position++
      _lastWasGreedy = false
    else:
      _lastWasGreedy = true

disassemble codes/List/*<int>*/ -> none:
  print("\nDisassembly\n")
  labels/List/*<bool>*/ := List codes.size
  for i := 0; i < codes.size; :
    code/int := codes[i]
    if code == PUSH_BACKTRACK or code == GOTO:
      pushed/int := codes[i + 1]
      if pushed >= 0 and pushed < codes.size: labels[pushed] = true
    i += BYTE_CODE_NAMES[code * 3 + 1] + BYTE_CODE_NAMES[code * 3 + 2] + 1
  for i := 0; i < codes.size; :
    if labels[i]: print "$i:"
    i += disassembleSingleInstruction codes i null
  print "\nEnd Disassembly\n"

disassembleSingleInstruction codes/List/*<int>*/ i/int registers/List?/*<int>*/ -> int:
    code/int := codes[i]
    regs/int := BYTE_CODE_NAMES[code * 3 + 1]
    otherArgs/int := BYTE_CODE_NAMES[code * 3 + 2]
    line/string := "$i: $BYTE_CODE_NAMES[code * 3]"
    for j := 0; j < regs; j++:
      reg/int := codes[i + 1 + j]
      line = "$line $REGISTER_NAMES[reg]"
      if registers != null: line = "$line:$registers[reg]"
    for j := 0; j < otherArgs; j++:
      line = line + " " + codes[i + 1 + regs + j].toString
    print line
    return regs + otherArgs + 1

class MiniExpInterpreter:
  _byteCodes/List/*<int>*/ ::= ?
  _constantPool /string ::= ?
  _registers/List/*<int>*/ ::= ?

  constructor ._byteCodes ._constantPool ._registers:

  stack/List/*<int>*/ := []
  stackPointer/int := 0

  interpret subject/string startPosition/int programCounter/int -> bool:
    _registers[STRING_LENGTH] = subject.size
    _registers[CURRENT_POSITION] = startPosition
    while true:
      byteCode := _byteCodes[programCounter]
      programCounter++
      // TODO: Faster implementation.
      if byteCode == GOTO:
        programCounter = _byteCodes[programCounter]
      else if byteCode == PUSH_REGISTER:
        reg/int := _registers[_byteCodes[programCounter++]]
        if stackPointer == stack.size:
          stack.add reg
          stackPointer++
        else:
          stack[stackPointer++] = reg
      else if byteCode == PUSH_BACKTRACK:
        value/int := _byteCodes[programCounter++]
        if stackPointer == stack.size:
          stack.add value
          stackPointer++
        else:
          stack[stackPointer++] = value
        position/int := _registers[CURRENT_POSITION]
        if stackPointer == stack.size:
          stack.add position
          stackPointer++
        else:
          stack[stackPointer++] = position
      else if byteCode == POP_REGISTER:
        _registers[_byteCodes[programCounter++]] = stack[--stackPointer]
      else if byteCode == BACKTRACK_EQ:
        reg1/int := _registers[_byteCodes[programCounter++]]
        reg2/int := _registers[_byteCodes[programCounter++]]
        if reg1 == reg2:
          _registers[CURRENT_POSITION] = stack[--stackPointer]
          programCounter = stack[--stackPointer]
      else if byteCode == BACKTRACK_NE:
        reg1/int := _registers[_byteCodes[programCounter++]]
        reg2/int := _registers[_byteCodes[programCounter++]]
        if reg1 != reg2:
          _registers[CURRENT_POSITION] = stack[--stackPointer]
          programCounter = stack[--stackPointer]
      else if byteCode == BACKTRACK_GT:
        reg1/int := _registers[_byteCodes[programCounter++]]
        reg2/int := _registers[_byteCodes[programCounter++]]
        if reg1 > reg2:
          _registers[CURRENT_POSITION] = stack[--stackPointer]
          programCounter = stack[--stackPointer]
      else if byteCode == BACKTRACK_IF_NO_MATCH:
        if (subject.at --raw _registers[CURRENT_POSITION]) !=
            (_constantPool.at --raw _byteCodes[programCounter++]):
          _registers[CURRENT_POSITION] = stack[--stackPointer]
          programCounter = stack[--stackPointer]
      else if byteCode == BACKTRACK_IF_IN_RANGE:
        code/int := subject.at --raw _registers[CURRENT_POSITION]
        from/int := _byteCodes[programCounter++]
        to/int := _byteCodes[programCounter++]
        if from <= code and code <= to:
          _registers[CURRENT_POSITION] = stack[--stackPointer]
          programCounter = stack[--stackPointer]
      else if byteCode == GOTO_IF_MATCH:
        code/int := subject.at --raw _registers[CURRENT_POSITION]
        expected/int := _byteCodes[programCounter++]
        dest/int := _byteCodes[programCounter++]
        if code == expected: programCounter = dest
      else if byteCode == GOTO_IF_IN_RANGE:
        code/int := subject.at --raw _registers[CURRENT_POSITION]
        from/int := _byteCodes[programCounter++]
        to/int := _byteCodes[programCounter++]
        dest/int := _byteCodes[programCounter++]
        if from <= code and code <= to: programCounter = dest
      else if byteCode == GOTO_EQ:
        reg1/int := _registers[_byteCodes[programCounter++]]
        reg2/int := _registers[_byteCodes[programCounter++]]
        dest/int := _byteCodes[programCounter++]
        if reg1 == reg2: programCounter = dest
      else if byteCode == GOTO_GE:
        reg1/int := _registers[_byteCodes[programCounter++]]
        reg2/int := _registers[_byteCodes[programCounter++]]
        dest/int := _byteCodes[programCounter++]
        if reg1 >= reg2: programCounter = dest
      else if byteCode == GOTO_IF_WORD_CHARACTER:
        offset/int := _byteCodes[programCounter++]
        charCode/int :=
            subject.at --raw _registers[CURRENT_POSITION] + offset
        dest/int := _byteCodes[programCounter++]
        if charCode >= '0':
          if charCode <= '9':
            programCounter = dest
          else if charCode >= 'A':
            if charCode <= 'Z':
              programCounter = dest
            else if charCode == '_':
              programCounter = dest
            else if (charCode >= 'a' and
                       charCode <= 'z'):
              programCounter = dest
      else if byteCode == ADD_TO_REGISTER:
        registerIndex/int := _byteCodes[programCounter++]
        _registers[registerIndex] += _byteCodes[programCounter++]
      else if byteCode == COPY_REGISTER:
        // We don't normally keep the stack pointer in sync with its slot in
        // the _registers, but we have to have it in sync here.
        _registers[STACK_POINTER] = stackPointer
        registerIndex/int := _byteCodes[programCounter++]
        value/int := _registers[_byteCodes[programCounter++]]
        _registers[registerIndex] = value
        stackPointer = _registers[STACK_POINTER]
      else if byteCode == BACKTRACK_ON_BACK_REFERENCE:
        registerIndex/int := _byteCodes[programCounter++]
        case_sensitive := _byteCodes[programCounter++] != 0
        if not checkBackReference subject case_sensitive registerIndex:
          // Backtrack.
          _registers[CURRENT_POSITION] = stack[--stackPointer]
          programCounter = stack[--stackPointer]
      else if byteCode == BACKTRACK:
        _registers[CURRENT_POSITION] = stack[--stackPointer]
        programCounter = stack[--stackPointer]
        return true
      else if byteCode == FAIL:
        return false
      else:
        assert: false

  checkBackReference subject/string caseSensitive/bool registerIndex/int -> bool:
    start/int := _registers[registerIndex]
    end/int := _registers[registerIndex + 1]
    if end == NO_POSITION: return true
    length/int := end - start
    currentPosition/int := _registers[CURRENT_POSITION]
    if currentPosition + end - start > subject.size: return false
    for i := 0; i < length; i++:
      x := subject.at --raw start + i
      y := subject.at --raw currentPosition + i
      if not caseSensitive:
        x = reg_exp_canonicalize_ x
        y = reg_exp_canonicalize_ y
      if x != y: return false
    _registers[CURRENT_POSITION] += length
    return true

class FormatException:
  text /string ::= ?
  source /string ::= ?
  position /int ::= ?

  constructor .text .source .position:

  stringify -> string:
    return "$text\n$source\n$("-" * position)^"

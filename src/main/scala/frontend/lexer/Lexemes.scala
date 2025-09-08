package nop.frontend.lexer

import scala.collection.mutable.ArrayBuffer

type Lexeme = WithSpan[Token];

class Lexemes(lexemes: ArrayBuffer[Lexeme], eofSpan: Span):
  private var offset: Int = 0
  private def eof: Lexeme = WithSpan(Token.EOF, eofSpan)

  type State = Int

  /// Check if end of input is reached.
  def isEof: Boolean = offset >= lexemes.length

  /// Peek next lexeme without extracting it. If there's no lexeme, return EOF lexeme.
  def peek: Lexeme = peekNth(0)

  /// Peek `n`th lexeme. If there's not enough lexemes, return EOF lexeme.
  def peekNth(n: Int): Lexeme = lexemes.lift(offset + n).getOrElse(eof)

  /// Peek `n` next lexemes. Any extra lexemes are replaced with EOF lexeme.
  def peekN(n: Int): Seq[Lexeme] = for i <- 0 to n yield peekNth(i)

  /// Get next lexeme. If there's not enough lexemes, return EOF lexeme.
  def next: Lexeme =
    val res = peek
    skipN(1)
    res

  /// Skip `n` next lexemes.
  def skipN(n: Int) =
    offset = (offset + n).min(lexemes.length)

  /// Get lexemes state to save. Used for backtracking.
  def getState: State = offset

  /// Set given lexemes state.
  def setState(state: State) =
    offset = state

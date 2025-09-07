package nop.frontend.lexer

import scala.collection.mutable.ArrayBuffer
import scala.util.boundary.break;
import scala.util.boundary

extension (c: Char)
  def isIdentifierStart = c.isLetter || c == '_'
  def isIdentifierChar  = c.isLetterOrDigit || c == '_'
  def isWhitespaceButNotNewline = c.isWhitespace && c != '\n'

private class Lexer(input: String):
  private var offset: Int = 0

  private def peekChar: Option[Char] = if offset < input.length then Some(input(offset)) else None
  private def nextChar: Option[Char] = peekChar.map(c => { offset += 1; c })

  private def peekChars(n: Int): Option[String] =
    if offset + n <= input.length
    then Some(input.slice(offset, offset + n))
    else None

  private def skipWhile(pred: Char => Boolean): Unit =
    while peekChar.filter(pred).isDefined do nextChar

  private def getWhile(pred: Char => Boolean): WithSpan[String] =
    val b   = offset
    var res = String()
    while peekChar.filter(pred).isDefined do res += nextChar.get
    val span = Span(b, offset)
    WithSpan(res, span)

  private def eofSpan: Span = Span(input.length - 1, input.length - 1)

  private def skipWhitespaceAndComments =
    var running = true
    while running do
      skipWhile(_.isWhitespaceButNotNewline)
      running = peekChars(2) match
        case Some("//") => skipWhile(_ != '\n'); true
        case _          => false

  private final val SINGLE_CHAR_TOKENS: List[(Char, Token)] =
    ('\n', Token.EOL) ::
      ('(', Token.LParen) ::
      (')', Token.RParen) ::
      ('{', Token.LBrace) ::
      ('}', Token.RBrace) ::
      ('.', Token.Dot) ::
      (',', Token.Comma) ::
      ('+', Token.Plus) ::
      ('*', Token.Mul) ::
      Nil

  private final val TWO_CHAR_TOKENS: List[(Char, Char, Token, Token)] =
    ('=', '=', Token.Assign, Token.Eq) ::
      (':', '=', Token.Colon, Token.Define) ::
      ('-', '>', Token.Minus, Token.Arrow) ::
      ('<', '=', Token.Less, Token.LessEq) ::
      ('>', '=', Token.Greater, Token.GreaterEq) ::
      ('!', '=', Token.Exclam, Token.NotEq) ::
      ('<', '=', Token.Less, Token.LessEq) ::
      ('>', '=', Token.Greater, Token.GreaterEq) ::
      ('&', '&', Token.Amper, Token.And) ::
      ('|', '|', Token.Vbar, Token.Or) ::
      Nil

  private final val KEYWORDS: List[(String, Token)] =
    ("fn", Token.Fn) ::
      ("type", Token.Type) ::
      ("mut", Token.Mut) ::
      ("ret", Token.Ret) ::
      ("true", Token.True) ::
      ("false", Token.False) ::
      ("for", Token.For) ::
      ("do", Token.Do) ::
      ("if", Token.If) ::
      ("then", Token.Then) ::
      ("else", Token.Else) ::
      ("struct", Token.Struct) ::
      Nil

  def lex: Lexemes =
    val lexemes: ArrayBuffer[Lexeme] = ArrayBuffer()
    boundary:
      while true do
        next match
          case None         => break()
          case Some(lexeme) => lexemes.addOne(lexeme)
    Lexemes(lexemes, eofSpan)

  private def lexSingleChar(c: Char): Option[Lexeme] =
    SINGLE_CHAR_TOKENS.find(p => p._1 == c) match
      case Some((_, tok)) =>
        val o = offset
        nextChar
        Some(WithSpan(tok, Span(o, o)))
      case None => None

  private def lexTwoChar(c1: Char): Option[Lexeme] =
    TWO_CHAR_TOKENS.find(p => p._1 == c1) match
      case Some((_, c2, tok1, tok2)) =>
        val o1 = offset
        nextChar
        if peekChar.exists(_ == c2) then
          val o2 = offset
          nextChar
          Some(WithSpan(tok2, Span(o1, o2)))
        else Some(WithSpan(tok1, Span(o1, o1)))
      case None => None

  private def lexId(c: Char): Option[Lexeme] =
    if !c.isIdentifierStart then None
    else
      val id = getWhile(_.isIdentifierChar)
      val res =
        KEYWORDS
          .find(_._1 == id.value)
          .map(p => id.replace(p._2))
          .getOrElse(id.map(Token.Id(_)))
      Some(res)

  private def lexNum(c: Char): Option[Lexeme] =
    if !c.isDigit then None
    else
      val raw = getWhile(_.isDigit)
      val res = raw.map(s => Token.Num(s.toInt))
      Some(res)

  private final val lexers =
    lexSingleChar ::
      lexTwoChar ::
      lexId ::
      lexNum ::
      Nil

  def next: Option[WithSpan[Token]] =
    skipWhitespaceAndComments
    peekChar.map(c =>
      lexers
        .foldLeft(None)((acc, lexer) => acc.orElse(lexer(c)))
        .getOrElse(throw RuntimeException(s"lexer got unknown char $c"))
    )

def lex(s: String): Lexemes = Lexer(s).lex

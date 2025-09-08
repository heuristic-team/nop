package nop
package frontend
package parser

import typesystem.Field
import typesystem.Type
import typesystem.TypeDecl

import lexer.Lexeme
import lexer.Lexemes
import lexer.Span
import lexer.WithSpan
import lexer.Token

import ast.*

import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

extension [A, B](e: Either[A, B]) {
  def replace[C](x: C): Either[A, C] = e.map(_ => x)

  def mapLeft[C](f: A => C): Either[C, B] =
    e match
      case Right(x) => Right(x)
      case Left(x)  => Left(f(x))

  def andThen[C](f: => Either[A, C]): Either[A, C] = e.flatMap(_ => f)

  def *>[C](f: => Either[A, C]): Either[A, C] = e.andThen(f)
  def <*(f: => Either[A, ?]): Either[A, B]    = e.flatMap(f.replace)
}

private def matchToken(tokens: Token*): Token => Boolean =
  t => tokens.exists(_.ordinal == t.ordinal)

class Parser(lexemes: Lexemes) {
  def parse: Res[(List[FnDecl], List[TypeDecl])] =
    def parseDecl[T](p: => Res[T]): Res[(T, List[FnDecl], List[TypeDecl])] =
      for {
        decl         <- p
        _            <- get(matchToken(Token.EOL, Token.EOF), Token.EOL, Token.EOF)
        (fns, types) <- parse
      } yield (decl, fns, types)

    eatNewlines
    if lexemes.isEof then Right((Nil, Nil))
    else {
      val WithSpan(token, span) = lexemes.peek
      token match
        case Token.Fn =>
          for (fn, fns, types) <- parseDecl(parseFnDecl) yield (fn +: fns, types)
        case Token.Type | Token.Struct =>
          for (ty, fns, types) <- parseDecl(parseTypeDecl) yield (fns, ty +: types)
        case t =>
          Left(ParseError(Token.Fn :: Token.Type :: Token.Struct :: Nil, t, span))
    }

  /** Eat token if `matcher` is satisfied.
    *
    * Returns `true` if the condition was satisfied and token is eaten.
    */
  private def eatIf(matcher: Token => Boolean): Boolean =
    val WithSpan(token, _) = lexemes.peek
    val res                = matcher(token)
    if res then lexemes.next
    res

  /** Eat tokens while `matcher` is satisfied. */
  @tailrec private def eatWhile(matcher: Token => Boolean): Unit =
    if matcher(lexemes.peek.value) then
      lexemes.next
      eatWhile(matcher)

  /** Extracts the lexeme or returns a `ParseError` with specified `expected` otherwise. Lexeme is
    * eaten in each case.
    */
  private def get(matcher: Token => Boolean, expected: Any*): Res[Lexeme] =
    val lexeme @ WithSpan(token, span) = lexemes.next
    if matcher(token) then Right(lexeme) else Left(ParseError(expected, token, span))

  /** Extracts the lexeme or returns a `ParseError` with specified `expected` otherwise. Lexeme is
    * eaten in each case.
    */
  private def get(token: Token, expected: Any*): Res[Lexeme] =
    val lexeme @ WithSpan(actualToken, span) = lexemes.next
    if token == actualToken
    then Right(lexeme)
    else
      Left(
        ParseError(
          if expected.isEmpty then token :: Nil else expected,
          actualToken,
          span,
        )
      )

  /** Extracts the lexeme and maps its token or returns a `ParseError` with specified `expected`
    * otherwise. Lexeme is eaten in each case.
    */
  private def getMap[T](matcher: Token => Option[T], expected: Any*): Res[WithSpan[T]] =
    val lexeme @ WithSpan(token, span) = lexemes.next
    matcher(token) match
      case Some(res) => Right(lexeme.replace(res))
      case None      => Left(ParseError(expected, token, span))

  /** Parse an identifier and extract the literal and span from it. Useful when parsing names of any
    * kind.
    */
  private def parseId: Res[WithSpan[String]] =
    def extractor(t: Token) = t match
      case Token.Id(id) => Some(id)
      case _            => None

    getMap(extractor, "identifier")

  private def eatNewlines = eatWhile(_ == Token.EOL)

  private def between[T](l: => Res[?], r: => Res[?], p: => Res[T]): Res[T] = l *> p <* r

  private def parseSepEndByUntil[T](
      p: => Res[T],
      sep: Token,
      end: Token,
      initialWhitespace: => Unit = eatNewlines,
      preItemWhitespace: => Unit = eatNewlines,
      whitespaceBetweenItemAndSep: => Unit = eatNewlines,
      whitespaceAfterSep: => Unit = eatNewlines,
  ): Res[List[T]] = {
    var res: List[T] = Nil
    initialWhitespace
    boundary:
      while lexemes.peek.value != end do {
        preItemWhitespace

        p match
          case Left(err)   => break(Left(err))
          case Right(item) => res :+= item

        whitespaceBetweenItemAndSep

        val WithSpan(token, span) = lexemes.peek

        if token == sep then lexemes.next
        else if token == end then {}
        else break(Left(ParseError(sep :: end :: Nil, token, span)))

        whitespaceAfterSep
      }
      Right(res)
  }

  /** Parse struct type declaration fields (`{ FIELDS }`).
    *
    * Expects the first token to be an identifier or `}`.
    */
  private def parseStructTypeDeclFields: Res[List[Field]] = {
    def field: Res[Field] =
      eatNewlines
      for {
        name <- parseId.map(_.value)
        _ = eatNewlines

        _ <- get(Token.Colon)
        _ = eatNewlines

        ty <- parseTypeReference
        _ = eatNewlines
      } yield Field(name, ty)

    parseSepEndByUntil(field, Token.Comma, Token.RBrace) <* get(Token.RBrace)
  }

  /** Parse struct type declaration (`struct NAME { FIELDS }`).
    *
    * Expects the first token to be `struct`.
    */
  private def parseStructTypeDecl: Res[TypeDecl] =
    for {
      kwSpan <- get(Token.Struct).map(_.span)
      _ = eatNewlines

      name <- parseId
      _ = eatNewlines

      _          <- get(Token.LBrace)
      fields     <- parseStructTypeDeclFields
      rbraceSpan <- get(Token.RBrace).map(_.span)

      value = Type.Struct(name, fields)
      span  = Span(kwSpan.start, rbraceSpan.end)
    } yield TypeDecl(name, WithSpan(value, span))

  /** Parse type. This can be a primitive type or type alias reference.
    *
    * Expects the first token to be an identifier.
    */
  private def parseTypeReference: Res[WithSpan[Type]] =
    for {
      WithSpan(id, span) <- parseId.mapLeft(_.expect("type"))
      ty = Type.primitiveFromString(id).getOrElse(Type.Alias(id))
    } yield WithSpan(ty, span)

  /** Parse type alias declaration (`type NAME = TYPE`).
    *
    * Expects the first token to be `type`.
    */
  private def parseTypeAliasDecl: Res[TypeDecl] =
    for {
      _     <- get(Token.Type)
      name  <- parseId
      _     <- get(Token.Assign)
      value <- parseTypeReference
    } yield TypeDecl(name, value)

  /** Parse type declaration (struct declaration or type alias declaration).
    *
    * Expects the first token to be `type` or `struct`.
    */
  private def parseTypeDecl: Res[TypeDecl] =
    val WithSpan(token, span) = lexemes.peek
    token match {
      case Token.Type   => parseTypeAliasDecl
      case Token.Struct => parseStructTypeDecl
      case t            => Left(ParseError("type declaration" :: Nil, t, span))
    }

  /** Parse function declaration (name, parameters, return type and body).
    *
    * Expects the first token to be `fn`.
    */
  private def parseFnDecl: Res[FnDecl] =
    for {
      _      <- get(Token.Fn)
      name   <- parseId
      params <- parseFnParams
      WithSpan(ty_token, ty_span) = lexemes.peek
      ty <- ty_token match
        case Token.Id(_) => parseTypeReference
        case _           => Right(WithSpan(Type.Unit, ty_span))
      _ <- get(Token.Assign)
      _ = eatNewlines
      body <- parseTopLevelExpr
    } yield FnDecl(name, ty, params, body)

  /** Parse function declaration parameters list (`(mut a: int, b: bool)` with support for trailing
    * comma).
    *
    * Expects the first token to be `(`.
    */
  private def parseFnParams: Res[List[FnParam]] = {
    def param: Res[FnParam] =
      val isMut = eatIf(_ == Token.Mut)
      eatNewlines
      for {
        name <- parseId
        _ = eatNewlines

        _ <- get(Token.Colon)
        _ = eatNewlines

        ty <- parseTypeReference
        _ = eatNewlines
      } yield FnParam(isMut, name, ty)

    between(
      get(Token.LParen),
      get(Token.RParen),
      parseSepEndByUntil(param, Token.Comma, Token.RParen),
    )
  }

  /** Parse any expression and provide it with information about statement position.
    *
    * Expects the first token to be either `mut`, identifier, `ret`, or anything that is expected by
    * `parse_expr`.
    */
  private def parseTopLevelExpr: Res[Expr] =
    lexemes.peekN(3).map(_.value) match {
      case Token.Mut :: Token.Id(_) :: Token.Define :: _
        | Token.Id(_) :: Token.Define :: _
        | Token.Mut :: Token.Id(_) :: Token.Colon :: _
        | Token.Id(_) :: Token.Colon :: _ => parseDeclarationExpr
      case Token.Ret :: _ => parseRetExpr
      case _              => parseExpr(inStmtPos = true)
    }

  /** Parse return (`ret` or `ret EXPR`).
    *
    * Expects the first token to be `ret`.
    */
  private def parseRetExpr: Res[Expr] =
    for {
      WithSpan(_, kw_span) <- get(Token.Ret)
      state = lexemes.getState
      value = parseExpr(inStmtPos = false).toOption
      span = value match
        case Some(e) => Span(kw_span.start, e.span.end)
        case None =>
          lexemes.setState(state)
          kw_span
    } yield Ret(value, span)

  /** Parse variable declaration (`NAME := VALUE` or `NAME: TYPE = VALUE` with optional `mut` before
    * name).
    *
    * Expects the first token to be either `mut` or an identifier.
    */
  private def parseDeclarationExpr: Res[Expr] =
    val isMut = eatIf(_ == Token.Mut)

    for {
      name <- parseId
      WithSpan(token, span) = lexemes.peek
      ty <- token match {
        case Token.Colon =>
          lexemes.next // eat ':'
          parseTypeReference <* get(Token.Assign)
        case Token.Define => Right(lexemes.next.replace(Type.Undef))
        case t            => Left(ParseError(Token.Colon :: Nil, t, span))
      }
      value <- parseExpr(inStmtPos = false)
    } yield Declaration(isMut, name, ty, value)

  /** Parse local expression. This can be a simple term or a binary expression chain, but not a
    * variable declaration or return.
    *
    * Expects that the input starts with some kind of term.
    */
  private def parseExpr(inStmtPos: Boolean): Res[Expr] =
    parseTerm(inStmtPos).flatMap(parseFullExpr(0, _))

  /** Parse term. Term can be:
    *
    *   - an expression in parentheses
    *   - a block
    *   - a conditional expression
    *   - a loop
    *   - a boolean or integer literal
    *   - a reference to some entity by its name
    *
    * Expects that the first token is the one expected by at least one of these parsers.
    */
  private def parseTerm(inStmtPos: Boolean): Res[Expr] = {
    val WithSpan(token, span) = lexemes.peek
    val term = token match {
      case Token.LParen =>
        lexemes.next
        parseExpr(inStmtPos) <* get(Token.RParen)
      case Token.LBrace => parseBlock
      case Token.If     => parseConditional(inStmtPos)
      case Token.For    => parseLoop
      case Token.True | Token.False =>
        lexemes.next
        Right(BoolLit(token == Token.True, span))
      case Token.Id(name) =>
        lexemes.next
        Right(Ref(Type.Undef, WithSpan(name, span)))
      case Token.Num(value) =>
        lexemes.next
        Right(
          NumLit(
            Type.I64, // TODO: unhardcode this
            WithSpan(value, span),
          )
        )
      case t => Left(ParseError("term" :: Nil, t, span))
    }

    term.flatMap(parsePostfixOperators)
  }

  /** Parse postfix operators like member reference and call.
    *
    * Expects the first token to be anything. If it is not `.` or `(`, simply returns passed
    * expression unmodified.
    */
  @tailrec private def parsePostfixOperators(expr: Expr): Res[Expr] = {
    // this should be written to enable tailrec optimization
    inline def rec(res: Res[Expr]): Res[Expr] =
      res match
        case Right(e)      => parsePostfixOperators(e)
        case l: Left[?, ?] => l

    lexemes.peek.value match {
      case Token.LParen => rec(parseCallExpr(expr))
      case Token.Dot    => rec(parseMemberRefExpr(expr))
      case _            => Right(expr)
    }
  }

  /** Parse call **operator**. This means that the callee is already parsed, for example:
    *
    * `foo.bar(1, 2, 3)` - here callee is `foo.bar` and should already be parsed, thus the input to
    * `parse_call_expr` should be `(1, 2, 3)`.
    *
    * Expects the first token to be `(`.
    */
  private def parseCallExpr(callee: Expr): Res[Expr] =
    for {
      _          <- get(Token.LParen)
      args       <- parseSepEndByUntil(parseExpr(inStmtPos = false), Token.Comma, Token.RParen)
      rparenSpan <- get(Token.RParen).map(_.span)
      span = Span(callee.span.start, rparenSpan.end)
    } yield Call(Type.Undef, callee, args, span)

  /** Parse member reference **operator**. This means that the target is already parsed, for
    * example:
    *
    * `foo.bar` - here target is `foo` and should already be parsed, thus the input to
    * `parse_member_ref_expr` should be `.bar`.
    *
    * Expects the first token to be `.`.
    */
  private def parseMemberRefExpr(target: Expr): Res[Expr] =
    (get(Token.Dot) *> parseId)
      .map(name => MemberRef(Type.Undef, target, name))

  /** Parse block (`{ EXPR* }`).
    *
    * Expects the first token to be `{`
    */
  private def parseBlock: Res[Expr] =
    for {
      lbraceOffset <- get(Token.LBrace).map(_.span.start)
      _ = eatNewlines
      body <- parseSepEndByUntil(
        parseTopLevelExpr,
        Token.EOL,
        Token.RBrace,
        whitespaceBetweenItemAndSep = {},
      )
      rbraceOffset <- get(Token.RBrace).map(_.span.end)
      span = Span(lbraceOffset, rbraceOffset)
    } yield Block(Type.Undef, body, span)

  /** Parse loop (`for ... do ...`).
    *
    * Expects the first token to be `for`.
    */
  private def parseLoop: Res[Expr] =
    for {
      kwSpan <- get(Token.For).map(_.span)
      cond   <- parseExpr(inStmtPos = false)
      _      <- get(Token.Do)
      body   <- parseExpr(inStmtPos = true)
      span = Span(kwSpan.start, body.span.end)
    } yield While(cond, body, span)

  /** Parse conditional expression (`if ... then ...` or `if ... then ... else ...`).
    *
    * Expects the first token to be `if`.
    *
    * `in_stmt_pos` is required to pass that information to the expression itself so typecheck knows
    * when to ignore return types of its branches and allow omitted `else` branch in statement
    * position.
    */
  private def parseConditional(inStmtPos: Boolean): Res[Expr] =
    for {
      kwSpan <- get(Token.If).map(_.span)
      cond   <- parseExpr(inStmtPos = false)
      _      <- get(Token.Then)
      onTrue <- parseExpr(inStmtPos)
      onFalse <-
        if eatIf(_ == Token.Else) then parseExpr(inStmtPos).map(Some.apply) else Right(None)
    } yield If(Type.Undef, cond, onTrue, onFalse, kwSpan, inStmtPos)

  /** Get precedence of the binary operator corresponding to the current token, if such operator
    * exists (for example, there is no operator for `(`).
    */
  private def getCurOpPrec: Option[Precedence] =
    BinaryOp.fromToken(lexemes.peek.value).map(_.prec)

  /** Parse expression after its first term has been parsed.
    *
    * For example, when parsing `1 + 2`, `parse_full_expr` will be called after `parse_term` has
    * eaten `1`. Thus the input will be `+ 2` and `lhs` is `Num(1)`.
    *
    * If the first token is not an operator, it simply returns `lhs`.
    */
  private def parseFullExpr(prevPrec: Precedence, lhs: Expr): Res[Expr] = {
    val curPrec = getCurOpPrec match
      case None                          => return Right(lhs)
      case Some(prec) if prec < prevPrec => return Right(lhs)
      case Some(prec)                    => prec

    val newLhs = for {
      op       <- getMap(BinaryOp.fromToken, "binary operator")
      nextTerm <- parseTerm(inStmtPos = false)
      rhs <- op.value.assoc match
        case Associativity.Left =>
          if getCurOpPrec.filter(curPrec < _).isDefined
          then parseFullExpr(curPrec + 1, nextTerm)
          else Right(nextTerm)
        case Associativity.Right =>
          if getCurOpPrec.filter(curPrec <= _).isDefined
          then parseFullExpr(curPrec, nextTerm)
          else Right(nextTerm)
    } yield Binary(Type.Undef, op, lhs, rhs)

    newLhs.flatMap(parseFullExpr(curPrec, _))
  }
}

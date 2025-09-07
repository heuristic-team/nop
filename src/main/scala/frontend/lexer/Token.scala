package nop.frontend.lexer

enum Token {
  case EOF
  case Id(val s: String)
  case Num(val i: Int)
  case Assign    // `=`
  case Define    // `:=`
  case Eq        // `==`
  case NotEq     // `!=`
  case And       // `&&`
  case Or        // `||`
  case Less      // `<`
  case LessEq    // `<=`
  case Greater   // `>`
  case GreaterEq // `>=`
  case Arrow     // `->`
  case Fn        // `fn`
  case Type      // `type`
  case Mut       // `mut`
  case Ret       // `ret`
  case True      // `true`
  case False     // `false`
  case For       // `for`
  case Do        // `do`
  case If        // `if`
  case Then      // `then`
  case Else      // `else`
  case Struct    // `struct`
  case EOL       //
  case LParen    // `(`
  case RParen    // `)`
  case LBrace    // `{`
  case RBrace    // `}`
  case Dot       // `.`
  case Comma     // `,`
  case Colon     // `:`
  case Plus      // `+`
  case Minus     // `-`
  case Mul       // `*`
  case Exclam    // `!`
  case Amper     // `&`
  case Vbar      // `|`

  override def toString: String =
    this match {
      case EOF       => "end of input"
      case Id(_)     => "identifier"
      case Num(_)    => "number"
      case Assign    => "`=`"
      case Define    => "`:=`"
      case Arrow     => "`->`"
      case Eq        => "`==`"
      case NotEq     => "`!=`"
      case And       => "`&&`"
      case Or        => "`||`"
      case Less      => "`<`"
      case LessEq    => "`<=`"
      case Greater   => "`>`"
      case GreaterEq => "`>=`"
      case Fn        => "`fn`"
      case Type      => "`type`"
      case Mut       => "`mut`"
      case Ret       => "`ret`"
      case True      => "`true`"
      case False     => "`false`"
      case For       => "`for`"
      case Do        => "`do`"
      case If        => "`if`"
      case Then      => "`then`"
      case Else      => "`else`"
      case Struct    => "`struct`"
      case EOL       => "end of line"
      case LParen    => "`(`"
      case RParen    => "`)`"
      case LBrace    => "`{`"
      case RBrace    => "`}`"
      case Dot       => "`.`"
      case Comma     => "`,`"
      case Colon     => "`:`"
      case Plus      => "`+`"
      case Minus     => "`-`"
      case Mul       => "`*`"
      case Exclam    => "`!`"
      case Amper     => "`&`"
      case Vbar      => "`|`"
    }
}

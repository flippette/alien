package o1.util

// “Proper” strings: trimmed and non-empty

private[o1] object proper:

  opaque type ProperString <: String = String

  extension (stringThatMustBeALiteral: String)
    inline def p: ProperString =
      Proper.literal(stringThatMustBeALiteral)

  object Proper:

    def string(vanilla: String): Option[ProperString] =
      val content = vanilla.trim
      if content.isEmpty then None else Some(content)

    def string(vanilla: String, default: ProperString): ProperString =
      string(vanilla) getOrElse default

    def string(vanilla: Option[String], default: ProperString): ProperString =
      vanilla.flatMap(string) getOrElse default

    // Trim and validate the given literal with a macro:
    inline def literal(inline literal: String): ProperString =
      ${ trimLiteral('literal) }

    import scala.quoted.*
    private def trimLiteral(expr: Expr[String])(using Quotes): Expr[ProperString] =
      import quotes.reflect.*
      def reportImproper(literal: String) = report.errorAndAbort(s"""The literal "$literal" is not a ProperString""")
      def reportNonLiteral(expr: Expr[?]) = report.errorAndAbort(s"""The expression "${expr.show}" is not a ProperString literal""")
      expr.asTerm match
        case Inlined(_, _, Literal(StringConstant(literal))) =>
          Proper.string(literal).map( Expr(_) ) getOrElse reportImproper(literal)
        case otherwiseNotALiteralAtAll =>
          reportNonLiteral(expr)

  end Proper

end proper



package o1.util

import scala.quoted.*

// Convenience macros for reflection.

/** A macro that returns a string describing the structure (abstract syntax tree) of the given Scala expression. */
inline def ast(inline expr: Any): String = ${ exprToAST('expr) }

private def exprToAST(expr: Expr[Any])(using Quotes): Expr[String] = Expr(expr.show)


/** A macro that prints out the given expressions. Literals are printed as they are, other expressions
  * as `"expr=value"`. Potentially useful for debugging. */
inline def report(inline exprs: Any*): Unit = ${exprsToReport('exprs)}  // Directly based on: https://github.com/softwaremill/scala3-macro-debug

private def exprsToReport(exprs: Expr[Seq[Any]])(using quotes: Quotes): Expr[Unit] =
  import quotes.reflect.*
  def cleanReplIds(id: String) = if id.startsWith("scala.") then id.drop(id.lastIndexOf(".") + 1) else id
  def exprWithValue(expr: Expr[?]): Expr[String] = '{ ${ Expr(cleanReplIds(expr.show)) } + "=" + $expr }
  def toReportPiece(expr: Expr[?]): Expr[String] = expr.asTerm match
    case Literal(const: Constant) => Expr(const.value.toString)
    case nonLiteral               => exprWithValue(expr)
  val pieceExprs: Seq[Expr[String]] = exprs match
    case Varargs(exprs) => exprs.map(toReportPiece)
    case singleExpr     => List(exprWithValue(singleExpr))
  val reportExpr = pieceExprs.reduceLeftOption( (expr1, expr2) => '{ $expr1 + ", " + $expr2 })
  '{ println(${ reportExpr getOrElse '{ "" } }) }


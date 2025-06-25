package lang

import lang.syntax.*
import lang.syntax.Syntax.*
import lang.syntax.simple.*
import munit.FunSuite

class ParserTest extends FunSuite {
  def parseAndEquate(input: String, expected: Term): Unit = {
    Parser.parse(input) match {
      case Left(err) => fail(s"Parsing failed: $err")
      case Right(term) =>
        assert(term == expected, s"Expected $expected but got $term")
    }
  }
  test("number") {
    val input = """1"""
    parseAndEquate(input, Number(1))
  }
  test("bool") {
    val input = """true"""
    parseAndEquate(input, Bool(true))
  }
  test("unit") {
    val input = """()"""
    parseAndEquate(input, UnitVal())
  }
  test("var") {
    val input = """x"""
    parseAndEquate(input, Var("x"))
  }
  test("lambda identity int -> int") {
    val input = s"""(${lambdaS.parser} x: int. x)"""
    parseAndEquate(input, Lambda(Var("x"), IntType(), Var("x")))
  }
  test("lambda unnannotated") {
    val input = s"""(${lambdaS.parser} x. x)"""
    parseAndEquate(input, Lambda(Var("x"), Unknown(), Var("x")))
  }
  test("parens") {
    val input = s"""((1))"""
    parseAndEquate(input, Number(1))
  }

  /** fix | asc | app | pair | binOp | unOp | fst | snd | inj1 | inj2 | ite |
    * let | caseE | bterm
    */

  test("let") {
    val input = s"""let x = 1 in x"""
    parseAndEquate(input, Let(Var("x"), Number(1), Var("x")))
  }
  test("binOp") {
    val input = s"""1 + 2"""
    parseAndEquate(input, BinOp(Number(1), Number(2), "+"))
  }
  test("unOp") {
    val input = s"""${notS.parser} true"""
    parseAndEquate(input, UnOp(Bool(true), s"${notS.parser}"))
  }
  test("binOp nested") {
    val input = s"""1 + 2 * 3"""
    parseAndEquate(
      input,
      BinOp(Number(1), BinOp(Number(2), Number(3), "*"), "+")
    )
  }
  test("unOp nested") {
    val input = s"""${notS.parser} ${notS.parser} true"""
    parseAndEquate(
      input,
      UnOp(UnOp(Bool(true), s"${notS.parser}"), s"${notS.parser}")
    )
  }
  test("pair") {
    val input = s"""(1, 2)"""
    parseAndEquate(input, Pair(Number(1), Number(2)))
  }
  test("pair nested") {
    val input = s"""(1, (2, 3))"""
    parseAndEquate(input, Pair(Number(1), Pair(Number(2), Number(3))))
  }
  test("pair projection 1") {
    val input = s"""fst (1, 2)"""
    parseAndEquate(input, First(Pair(Number(1), Number(2))))
  }
  test("pair projection 2") {
    val input = s"""snd (1, 2)"""
    parseAndEquate(input, Second(Pair(Number(1), Number(2))))
  }
  test("inl") {
    val input = s"""inl {bool} 1"""
    parseAndEquate(input, Inl(Number(1), BoolType()))
  }
  test("inr") {
    val input = s"""inr {bool} 1"""
    parseAndEquate(input, Inr(Number(1), BoolType()))
  }
  test("inl unnanotated") {
    val input = s"""inl 1"""
    parseAndEquate(input, Inl(Number(1), Unknown()))
  }
  test("inr unnanotated") {
    val input = s"""inr 1"""
    parseAndEquate(input, Inr(Number(1), Unknown()))
  }
  test("caseE") {
    val input = s"""case inl {bool} 1 of {x => x} {y => y}"""
    parseAndEquate(
      input,
      Case(Inl(Number(1), BoolType()), Var("x"), Var("x"), Var("y"), Var("y"))
    )
  }
  test("caseE unnanotated") {
    val input = s"""case inl 1 of {x => x} {y => y}"""
    parseAndEquate(
      input,
      Case(Inl(Number(1), Unknown()), Var("x"), Var("x"), Var("y"), Var("y"))
    )
  }
  test("fix") {
    val input = s"""fix x: int -> int. x"""
    parseAndEquate(
      input,
      Fix(Var("x"), FuncType(IntType(), IntType()), Var("x"))
    )
  }
  test("fix nested") {
    val input = s"""fix x: int. fix y: int. x + y"""
    parseAndEquate(
      input,
      Fix(
        Var("x"),
        IntType(),
        Fix(Var("y"), IntType(), BinOp(Var("x"), Var("y"), "+"))
      )
    )
  }

  test("app lambda") {
    val input = s"""(${lambdaS.parser} x: int. x) 1"""
    parseAndEquate(input, App(Lambda(Var("x"), IntType(), Var("x")), Number(1)))
  }
  test("asc") {
    val input = s"""1 :: int"""
    parseAndEquate(input, Asc(Number(1), IntType()))
  }
  test("asc nested") {
    val input = s"""1 :: int :: ?"""
    parseAndEquate(input, Asc(Asc(Number(1), IntType()), Unknown()))
  }
  test("if then else (ite)") {
    val input = s"""if true then 1 else 2"""
    parseAndEquate(input, Ite(Bool(true), Number(1), Number(2)))
  }
  test("if then else comparison") {
    val input = s"""if 1 < 2 then 1 else 2"""
    parseAndEquate(
      input,
      Ite(BinOp(Number(1), Number(2), "<"), Number(1), Number(2))
    )
  }
}

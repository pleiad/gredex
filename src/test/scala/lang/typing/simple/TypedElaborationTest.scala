package lang.typing.simple

import lang.Parser
import lang.syntax.Syntax.*
import lang.syntax.Var
import lang.syntax.simple.*
import lang.typing.*
import lang.typing.simple.*
import munit.FunSuite

class TypedElaborationTest extends FunSuite {
  def elaborateAndCompare(input: String, expected: TypingDerivation): Unit = {
    Parser.parse(input) match {
      case Left(err) => fail(s"Parsing failed: $err")
      case Right(term) =>
        val got = TypedElaboration(term)
        assertEquals(got, expected, s"Expected $expected but got $term")
    }
  }

  def elaborate(input: String): TypingDerivation = {
    Parser.parse(input) match {
      case Left(err)   => fail(s"Parsing failed: $err")
      case Right(term) => TypedElaboration(term)
    }
  }

  test("lambda") {
    val input = s"""(${lambdaS.parser} x: int. x)"""
    val expected =
      IAsc(
        ILambda(IVar("x", IntType()), IVar("x", IntType())),
        FuncType(IntType(), IntType())
      )
    elaborateAndCompare(input, expected)
  }

  test("number") {
    val input = """1"""
    val expected = IAsc(INumber(1), IntType())
    elaborateAndCompare(input, expected)
  }
  test("boolean") {
    val input = """true"""
    val expected = IAsc(IBool(true), BoolType())
    elaborateAndCompare(input, expected)
  }
  test("unit") {
    val input = """()"""
    val expected = IAsc(IUnit(), UnitType())
    elaborateAndCompare(input, expected)
  }

  test("app") {
    val input = s"""(${lambdaS.parser}x: int. x) 1"""
    val expected =
      IApp(
        IAsc(
          ILambda(IVar("x", IntType()), IVar("x", IntType())),
          FuncType(IntType(), IntType())
        ),
        IAsc(INumber(1), IntType())
      )
    elaborateAndCompare(input, expected)
  }

  test("addition") {
    val input = s"""1 + 2"""
    val expected =
      IBinOp(IAsc(INumber(1), IntType()), IAsc(INumber(2), IntType()), "+")

    elaborateAndCompare(input, expected)
  }

  test("addition nested") {
    val input = """(1 + 2) + (3 + 4)"""
    val expected =
      IBinOp(
        IBinOp(IAsc(INumber(1), IntType()), IAsc(INumber(2), IntType()), "+"),
        IBinOp(IAsc(INumber(3), IntType()), IAsc(INumber(4), IntType()), "+"),
        "+"
      )

    elaborateAndCompare(input, expected)
  }

  test("let") {
    val input = s"""let x = 1 in x"""
    val expected =
      ILet(
        IVar("x", IntType()),
        IAsc(INumber(1), IntType()),
        IVar("x", IntType())
      )
    elaborateAndCompare(input, expected)
  }
  test("if") {
    val input = s"""if true then 1 else 2"""
    val expected =
      IIte(
        IAsc(IBool(true), BoolType()),
        IAsc(INumber(1), IntType()),
        IAsc(INumber(2), IntType())
      )
    elaborateAndCompare(input, expected)
  }
  test("pair") {
    val input = s"""(1, 2)"""
    val expected =
      IPair(IAsc(INumber(1), IntType()), IAsc(INumber(2), IntType()))

    elaborateAndCompare(input, expected)
  }
  test("pair projection 1") {
    val input = s"""fst (1, 2)"""
    val expected =
      IFirst(
        IPair(IAsc(INumber(1), IntType()), IAsc(INumber(2), IntType()))
      )
    elaborateAndCompare(input, expected)
  }
  test("test pair projection 2") {
    val input = s"""snd (1, 2)"""
    val expected =
      ISecond(
        IPair(IAsc(INumber(1), IntType()), IAsc(INumber(2), IntType()))
      )
    elaborateAndCompare(input, expected)
  }
  test("sum") {
    val input = s"""inl {bool} 1"""
    val expected =
      IInl(IAsc(INumber(1), IntType()), BoolType())
    elaborateAndCompare(input, expected)
  }
  test("case") {
    val input = s"""case inl {bool} 1 of {x => x < 1} {y => y}"""
    val expected =
      ICase(
        IInl(IAsc(INumber(1), IntType()), BoolType()),
        IVar("x", IntType()),
        IBinOp(IVar("x", IntType()), IAsc(INumber(1), IntType()), "<"),
        IVar("y", BoolType()),
        IVar("y", BoolType())
      )
    elaborateAndCompare(input, expected)
  }
  test("fix") {
    val input = s"""fix x: int -> int. x"""
    val expected =
      IFix(
        IVar("x", FuncType(IntType(), IntType())),
        IVar("x", FuncType(IntType(), IntType()))
      )
    elaborateAndCompare(input, expected)
  }
  test("fix wrong") {
    val input = s"""fix x: int -> int. 1"""

    intercept[IInteriorError] {
      elaborate(input)
    }
  }
}

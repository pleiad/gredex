package lang.runtime.simple

import lang.Parser
import lang.syntax.Syntax
import lang.syntax.simple.*
import lang.typing.*
import lang.typing.simple.*
import Syntax.*
import lang.typing.simple.evidence.Evidence

class RuntimeTest extends munit.FunSuite {
  def elaborateAndRun(
      input: String,
      cb: (Term, Result) => Unit
  ) = {
    Parser.parse(input) match {
      case Left(err) => fail(s"Parsing failed: $err")
      case Right(term) =>
        cb(
          term,
          SimpleReducer(
            TypedElaboration(term),
            stepSize = Integer.MAX_VALUE
          ).reduce
        )
    }
  }
  def successRun(input: String, expected: TypingDerivation): Unit = {
    elaborateAndRun(
      input,
      (term, result) =>
        result match {
          case Result(Left(got), confs, _, _) =>
            assertEquals(got, expected, s"Expected $expected but got $term")
          case Result(Right(err), _, _, _) =>
            fail(s"Reduction failed: $err")
        }
    )
  }
  def failRun(input: String): Unit = {
    elaborateAndRun(
      input,
      (term, result) =>
        result match {
          case Result(Left(got), confs, _, _) =>
            fail(s"Reduction should fail but got $got")
          case Result(Right(err), _, _, _) =>
            assert(true)
        }
    )
  }
  /* numbers */
  test("number") {
    val input = """1"""
    val expected = IAsc(INumber(1), IntType())
    successRun(input, expected)
  }
  test("addition") {
    val input = """1 + 2"""
    val expected = IAsc(INumber(3), IntType())
    successRun(input, expected)
  }
  test("addition nested") {
    val input = """(1 + 2) + (3 + 4)"""
    val expected = IAsc(INumber(10), IntType())
    successRun(input, expected)
  }
  /* booleans */
  test("boolean") {
    val input = """true"""
    val expected = IAsc(IBool(true), BoolType())
    successRun(input, expected)
  }
  test("boolean negation") {
    val input = s"""(not true)"""
    val expected = IAsc(IBool(false), BoolType())
    successRun(input, expected)
  }
  test("boolean negation nested") {
    val input = s"""(not (not (not true)))"""
    val expected = IAsc(IBool(false), BoolType())
    successRun(input, expected)
  }
  test("conditional then branch") {
    val input = s"""if true then 1 else 2"""
    val expected = IAsc(INumber(1), IntType())
    successRun(input, expected)
  }
  test("conditional else branch") {
    val input = s"""if not true then 1 else 2"""
    val expected = IAsc(INumber(2), IntType())
    successRun(input, expected)
  }
  test("conditional nested") {
    val input = s"""if true then if false then 1 else 2 else 3"""
    val expected = IAsc(INumber(2), IntType())
    successRun(input, expected)
  }
  test("conditional error") {
    val input = s"""if (2 :: ?) then 1 else 2"""
    failRun(input)
  }
  /* functions */
  test("lambda") {
    val input = s"""(${lambdaS.parser} x: int. x)"""
    val expected =
      IAsc(
        ILambda(IVar("x", IntType()), IVar("x", IntType())),
        FuncType(IntType(), IntType())
      )
    successRun(input, expected)
  }
  test("lambda application") {
    val input = s"""(${lambdaS.parser} x: int. x) 1"""
    val expected =
      IAsc(INumber(1), IntType())
    successRun(input, expected)
  }
  test("lambda nested application") {
    val input =
      s"""((${lambdaS.parser} x: int. (${lambdaS.parser} y: int. x + y)) 1) 2"""
    val expected =
      IAsc(INumber(3), IntType())
    successRun(input, expected)
  }
  test("lambda runtime error") {
    val input = s"""(${lambdaS.parser} x: ?. x + 1) true"""
    failRun(input)
  }
  /* ascriptions */
  test("ascription") {
    val input = s"""1 :: int"""
    val expected = IAsc(INumber(1), IntType())
    successRun(input, expected)
  }
  test("ascription nested") {
    val input = s"""1 :: int :: ?"""
    val expected = IAsc(INumber(1), Unknown())
    successRun(input, expected)
  }
  test("ascription function") {
    val input =
      s"""(${lambdaS.parser} x. x) :: ? -> int :: ? -> ? :: int -> ? :: ? -> ?"""
    val expected = IAsc(
      ILambda(IVar("x", Unknown()), IVar("x", Unknown())),
      FuncType(Unknown(), Unknown()),
      Evidence(FuncType(IntType(), IntType()), FuncType(IntType(), IntType()))
    )
    successRun(input, expected)
  }
  test("ascription number to boolean error") {
    val input = s"""1 :: ? :: bool"""
    failRun(input)
  }
  test("ascription function error") {
    val input =
      s"""(${lambdaS.parser} x. x) :: int -> int :: ? -> ? :: ? -> bool"""
    failRun(input)
  }
  /* fix */
  test("fix") {
    val input = s"""fix f. (${lambdaS.parser} x. f x)"""
    val elab = Parser
      .parse(input)
      .map(TypedElaboration(_))
      .getOrElse(fail("Parsing failed"))
    val expected = IAsc(
      ILambda(
        IVar("x", Unknown()),
        IApp(IAsc(elab, FuncType(Unknown(), Unknown())), IVar("x", Unknown()))
      ),
      Unknown()
    )
    successRun(input, expected)
  }

  test("fix even") {
    val input =
      s"""(fix isEven. (${lambdaS.parser} x. if x == 0 then true else (if (x-1) == 0 then false else isEven (x - 2)))) 7"""
    val expected = IAsc(
      IBool(false),
      Unknown()
    )
    successRun(input, expected)
  }
  test("fix taut inline") {
    val input =
      s"""((fix f. (${lambdaS.parser}n. (${lambdaS.parser}h. if n == 0 then h else ((f (n-1)) (h false) ) ))) 1) (${lambdaS.parser}x. x)""".stripMargin
    val expected = IAsc(
      IBool(false),
      Unknown()
    )
    successRun(input, expected)
  }
  /* let */
  test("let") {
    val input = s"""let x = 1 in x + 2"""
    val expected = IAsc(INumber(3), IntType())
    successRun(input, expected)
  }
  test("let taut") {
    val input =
      s"""let taut = fix f. (${lambdaS.parser}n. (${lambdaS.parser}h. if n == 0 then h else ((f (n-1)) (h false) ) )) in
         | (taut 1) :: ((bool -> bool)-> bool) (${lambdaS.parser}x. x)""".stripMargin
    val expected = IAsc(
      IBool(false),
      BoolType()
    )
    successRun(input, expected)
  }

  /* pairs */
  test("pair") {
    val input = s"""(1, true)"""
    val expected =
      IAsc(IPair(INumber(1), IBool(true)), PairType(IntType(), BoolType()))
    successRun(input, expected)
  }
  test("pair first") {
    val input = s"""fst (1, true)"""
    val expected = IAsc(INumber(1), IntType())
    successRun(input, expected)
  }
  test("pair second") {
    val input = s"""snd (1, true)"""
    val expected = IAsc(IBool(true), BoolType())
    successRun(input, expected)
  }
  test("pair nested first first") {
    val input = s"""fst fst ((1, true), (2, false))"""
    val expected = IAsc(INumber(1), IntType())
    successRun(input, expected)
  }

  /* sums */
  test("sum left") {
    val input = s"""inl 1"""
    val expected =
      IAsc(IInl(INumber(1), Unknown()), SumType(IntType(), Unknown()))
    successRun(input, expected)
  }
  test("sum right") {
    val input = s"""inr true"""
    val expected =
      IAsc(IInr(IBool(true), Unknown()), SumType(Unknown(), BoolType()))
    successRun(input, expected)
  }
  test("sum left annotated") {
    val input = s"""inl {bool} 1"""
    val expected =
      IAsc(IInl(INumber(1), BoolType()), SumType(IntType(), BoolType()))
    successRun(input, expected)
  }
  test("sum left nested") {
    val input = s"""inl (inl 1)"""
    val expected =
      IAsc(
        IInl(IInl(INumber(1), Unknown()), Unknown()),
        SumType(SumType(IntType(), Unknown()), Unknown())
      )
    successRun(input, expected)
  }
  test("case left") {
    val input = s"""case inl {bool} 1 of {x => x>0} {y => not y}"""
    val expected = IAsc(IBool(true), BoolType())
    successRun(input, expected)
  }
  test("case right") {
    val input = s"""case inr {int} true of {x => x>0} {y => not y}"""
    val expected = IAsc(IBool(false), BoolType())
    successRun(input, expected)
  }
}


sealed trait Expr
case class Number(n: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
}

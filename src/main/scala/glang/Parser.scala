package glang

import glang.syntax.Syntax.*
import glang.syntax.{Type, Var}
import glang.syntax.simple.*

import scala.util.parsing.combinator.*
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class GExprParser
    extends StandardTokenParsers
    with PackratParsers
    with ImplicitConversions {

  lexical.reserved += ("if", "then", "else", "true", "false", "let", "in", "unit", "int", "string", "bool", "unit", "fst", "snd", "inl", "inr", "case", "as", "of", "fix", "()", "not")
  lexical.delimiters ++= (s": . <  > -> => + - * ★ & / ( ) [ ] { } = ; , ,, ${symbols
      .map { _.parser }
      .mkString(" ")} :: : ? ⋆ T" split ' ')

  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
    // println("trying " + name + " at " + in)
    val r = p(in)
    // println(name + " --> " + r)

    r
  }

  lazy val intType: PackratParser[IntType] = """int""" ^^ { _ => IntType() }
  lazy val boolType: PackratParser[BoolType] = """bool""" ^^ { _ => BoolType() }
  lazy val unitType: PackratParser[UnitType] = """unit""" ^^ { _ => UnitType() }
  lazy val funcType: PackratParser[FuncType] = tpe ~ ("->" ~> tpe) ^^ {
    case t1 ~ t2 => FuncType(t1, t2)
  }

  lazy val pairType: PackratParser[PairType] = tpe ~ ("x" ~> tpe) ^^ {
    case t1 ~ t2 => PairType(t1, t2)
  }

  lazy val sumType: PackratParser[SumType] = tpe ~ ("+" ~> tpe) ^^ {
    case t1 ~ t2 => SumType(t1, t2)
  }
  lazy val unkType: PackratParser[Unknown] = "?" ^^ { _ => Unknown() }
  lazy val unkTypeAlt: PackratParser[Unknown] = ("★" | "*") ^^ { _ =>
    Unknown()
  }

  lazy val btpe: PackratParser[Type] =
    log(pairType)("pairType") | log(sumType)("sumType") | log(funcType)(
      "funcType"
    ) | log(intType)("intType") | log(
      boolType
    )("boolType") | unkType | unkTypeAlt | unitType

  lazy val tpe: PackratParser[Type] =
    log(btpe)("btpe") | "(" ~> log(tpe)("parent") <~ ")"

  lazy val term: PackratParser[Term] = fix | asc | log(app)(
    "app"
  ) | pair | binOp | unOp | fst | snd | inj1 | inj2 | ite | let | caseE | bterm
  lazy val bterm: PackratParser[Term] = log(lambda)(
    "lambda"
  ) ||| unit ||| boolean ||| number ||| variable ||| parens // ||| dyn

  lazy val number: PackratParser[Number] = numericLit ^^ { s =>
    Number(s.toInt)
  }
  lazy val unit: PackratParser[UnitVal] = "(" ~ ")" ^^ { s => UnitVal() }

  lazy val boolean: PackratParser[Bool] = ("true" | "false") ^^ { s =>
    Bool(s == "true")
  }

  lazy val lambda: PackratParser[Lambda] =
    (lambdaS.parser | "λ") ~> variable ~ (":" ~> tpe).? ~ ("." ~> term) ^^ {
      case x ~ ty ~ e => Lambda(x, ty.getOrElse(Unknown()), e)
    }
  // lazy val lambdaSimpl: PackratParser[Lambda] = lambdaS.parser ~> variable ~ (":" ~> tpe).?  ~ ("." ~> term) ^^ { case x ~ t ~ e  => Lambda(x, t.getOrElse(FuncType(Unknown(), Unknown())), e, t.isDefined) }

  lazy val asc: PackratParser[Asc] = term ~ ("::" ~> tpe) ^^ { case e ~ t =>
    Asc(e, t)
  }
  // lazy val asc: PackratParser[Asc] = term ~ (":" ~> tpe) ^^ { case e ~ t => Asc(e, t) }

  lazy val variable: PackratParser[Var] = log(ident)("ident") ^^ { case p =>
    Var(p)
  }

  lazy val binOpS: PackratParser[String] =
    (andS.parser | orS.parser | timesS.parser | minusS.parser | plusS.parser | ltS.parser | gtS.parser | eqS.parser) ^^ {
      case s => s
    }

  lazy val binOp: PackratParser[BinOp] = term ~ binOpS ~ term ^^ {
    case t1 ~ op ~ t2 => BinOp(t1, t2, op)
  }

  lazy val unOpS: PackratParser[String] = (notS.parser | "not") ^^ { case s =>
    s
  }

  lazy val unOp: PackratParser[UnOp] = unOpS ~ term ^^ { case op ~ t1 =>
    UnOp(t1, op)
  }

  lazy val innerPair: PackratParser[(Term, Term)] = term ~ "," ~ term ^^ {
    case t1 ~ comma ~ t2 => (t1, t2)
  }

  lazy val pair: PackratParser[Pair] =
    log(("(" ~> innerPair <~ ")"))("pair") ^^ { case p => Pair(p._1, p._2) }

  lazy val ite: PackratParser[Ite] =
    "if" ~ term ~ "then" ~ term ~ "else" ~ term ^^ {
      case "if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3 => Ite(t1, t2, t3)
    }

  lazy val parens: PackratParser[Term] = "(" ~> term <~ ")"

  // lazy val dyn: PackratParser[Term] = "{"~> term <~"}" ^^ { case t => dynify(t) }

  lazy val let: PackratParser[Let] =
    "let" ~ variable ~ "=" ~ term ~ "in" ~ term ^^ {
      case "let" ~ x ~ "=" ~ t2 ~ "in" ~ t1 => Let(x, t2, t1)
    }

  lazy val caseE: PackratParser[Case] =
    ("case" ~> term <~ "of") ~ ("{" ~> variable) ~ ("=>" ~> term <~ "}") ~ ("{" ~> variable) ~ ("=>" ~> term <~ "}") ^^ {
      case t ~ x1 ~ t1 ~ x2 ~ t2 => Case(t, x1, t1, x2, t2)
    }

  lazy val fix: PackratParser[Fix] =
    ("fix" ~> variable) ~ (":" ~> tpe).? ~ ("." ~> term) ^^ { case x ~ ty ~ t =>
      Fix(x, ty.getOrElse(Unknown()), t)
    }

  lazy val app: PackratParser[App] = log(term)("t1") ~ log(term)("t2") ^^ {
    case t1 ~ t2 => App(t1, t2)
  }

  lazy val fst: PackratParser[First] = log("fst" ~> term)("fst") ^^ { case t =>
    First(t)
  }

  lazy val snd: PackratParser[Second] = "snd" ~> term ^^ { case t => Second(t) }

  lazy val inj1: PackratParser[Inl] =
    ("inl" ~> ("{" ~> tpe <~ "}").? ~ term) ^^ { case t ~ e =>
      Inl(e, t.getOrElse(Unknown()))
    }

  lazy val inj2: PackratParser[Inr] =
    ("inr" ~> ("{" ~> tpe <~ "}").? ~ term) ^^ { case t ~ e =>
      Inr(e, t.getOrElse(Unknown()))
    }

}
object Parser extends GExprParser {
  def parse(prog: String): Either[String, Term] =
    phrase(term)(new lexical.Scanner(prog)) match {
      case x: NoSuccess => Left("[" + x.next.pos + "] failure: " + x.msg)
      case Success(result, next) => Right(result)
    }
}

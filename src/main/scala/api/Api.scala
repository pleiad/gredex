package api

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import akka.http.scaladsl.Http
import lang.syntax.{Syntax, TypeError}

import scala.io.StdIn
import spray.json.*
import lang.Parser
import lang.typing.{
  IConfLatex,
  IOptions,
  LatexDerivationTree,
  LatexJudgment,
  VariableNotFoundException
}
import lang.typing.simple.TypedElaboration
import lang.runtime.simple.SimpleReducer

final case class Program(
    program: String,
    hideEvidences: Boolean = false,
    fromStep: Int = 0,
    stepSize: Int = 20
)

object JsonProtocol extends DefaultJsonProtocol {
  implicit val programFormat: RootJsonFormat[Program] = jsonFormat4(Program)
  /*implicit val latexJudgmentFormat: RootJsonFormat[LatexJudgment] = jsonFormat1(
    LatexJudgment
  )
  implicit val latexDerivationTreeFormat: RootJsonFormat[LatexDerivationTree] =
    jsonFormat4(LatexDerivationTree)*/
}

object Api {
  import JsonProtocol.*

  implicit val system: ActorSystem[_] = ActorSystem(Behaviors.empty, "Api")
  implicit val executionContext: ExecutionContext = system.executionContext

  def formatInput(s: String): String = {
    Syntax.symbols.foldLeft(s) { case (acc, sym) =>
      acc.replace(sym.pprint, sym.parser)
    }
  }

  def latexJudgmentToJson(j: LatexJudgment): JsString = {
    JsString(j.judgment)
  }

  def latexDerivationTreeToJson(t: LatexDerivationTree): JsObject = {
    JsObject(
      "term" -> JsString(t.term),
      "subtrees" -> JsArray(t.subtrees.map(latexDerivationTreeToJson).toVector),
      "judgments" -> JsArray(t.judgments.map(latexJudgmentToJson).toVector),
      "name" -> JsString(t.name)
    )
  }

  def iConfLatexToJson(conf: IConfLatex): JsObject = {
    JsObject(
      "tree" -> latexDerivationTreeToJson(conf.tree),
      "env" -> JsString(conf.env),
      "store" -> JsString(conf.store)
    )
  }

  def main(args: Array[String]): Unit = {
    val route: Route =
      concat(
        post {
          path("api" / "typecheck") {
            cors() {
              entity(as[Program]) { f =>
                Parser.parse(formatInput(f.program)) match {
                  case Right(term) =>
                    try {

                      implicit val o: IOptions = IOptions(f.hideEvidences)
                      val iterm = TypedElaboration(term)

                      val latex = iterm.toLatex
                      val tree = iterm.getLatexDerivationTree

                      complete(
                        JsObject(
                          "status" -> JsString("OK"),
                          "program" -> JsString(f.program),
                          "intrinsicTerm" -> JsString(latex),
                          "tree" -> latexDerivationTreeToJson(tree)
                        )
                      )
                    } catch {
                      case e =>
                        e.printStackTrace()
                        complete(
                          JsObject(
                            "status" -> JsString("KO"),
                            "error" -> JsString(e.getMessage)
                          )
                        )
                    }

                  case Left(error) =>
                    complete(
                      JsObject(
                        "status" -> JsString("KO"),
                        "error" -> JsString(("Parse error: " + error))
                      )
                    )
                }

              }
            }
          }
        },
        post {
          path("api" / "reduce") {
            cors() {
              entity(as[Program]) { f =>
                Parser.parse(formatInput(f.program)) match {
                  case Right(term) =>
                    try {
                      implicit val o: IOptions = IOptions(f.hideEvidences)
                      val reducer = SimpleReducer(
                        TypedElaboration(term),
                        fromStep = f.fromStep,
                        stepSize = f.stepSize
                      )

                      val r = reducer.reduce

                      val error: Option[String] = r.r match {
                        case Left(a)  => None
                        case Right(e) => Some(e.getMessage())
                      }

                      val confs = r.configurations
                      complete(
                        JsObject(
                          "status" -> JsString("OK"),
                          "program" -> JsString(f.program),
                          "confs" -> JsArray(
                            confs
                              .drop(f.fromStep)
                              .map(iConfLatexToJson)
                              .toVector
                          ),
                          "error" -> error.map(JsString).getOrElse(JsNull),
                          "finished" -> JsBoolean(r.finished),
                          "step" -> JsNumber(r.step)
                        )
                      )

                    } catch {
                      case e: VariableNotFoundException =>
                        complete(
                          JsObject(
                            "status" -> JsString("KO1"),
                            "error" -> JsString(e.getMessage)
                          )
                        )
                      case e: TypeError =>
                        complete(
                          JsObject(
                            "status" -> JsString("KO2"),
                            "error" -> JsString(e.getMessage)
                          )
                        )
                      case e: Throwable =>
                        complete(
                          JsObject(
                            "status" -> JsString("KO3"),
                            "error" -> JsString(e.getMessage)
                          )
                        )
                    }

                  case Left(error) =>
                    complete(
                      JsObject(
                        "status" -> JsString("KO"),
                        "error" -> JsString(("Parse error: " + error))
                      )
                    )
                }

              }
            }
          }
        },
        getFromResourceDirectory("public"),
        path(Remaining) { _ =>
          getFromResource("public/index.html")
        }
      )

    val bindingFuture = Http().newServerAt("localhost", 8080).bind(route)
    println(s"Server online at http://localhost:8080/\nPress ENTER to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }

}

package o.tizenotodik.het

import o.tizenotodik.het.O1SaltToUdash.Parse

import scala.util.parsing.combinator.JavaTokenParsers

object O1SaltToUdash {

  def main(args: Array[String]): Unit = {

    println(
      Parse.UdashAsText.render(
        Parse.parseAll(Parse.uml,
          """@startuml
            |salt
            |{
            |  Just plain text
            |  [This is my button]
            |  ()  Unchecked radio
            |}
            |@enduml""".stripMargin)
      )
    )
  }

  sealed trait Elements

  case class PlainText(s: String) extends Elements

  case class Button(s: String) extends Elements

  case class UncheckedRadio(s: String) extends Elements


  object Parse extends JavaTokenParsers {

    override protected val whiteSpace = """[\t ]+""".r

    def uml = start ~ nl ~ salt ~ nl ~ "{" ~ nl ~> inside <~ nl ~ "}" ~ nl ~ end

    def someNewLine = """\n+""".r

    def nl = someNewLine

    def start = "@startuml"

    def salt = "salt"

    def end = "@enduml"

    def inside = repsep(button | uncheckedRadio | plainText, nl)

    def wordList: Parser[List[String]] = rep1("""\w+""".r)

    def plainText: Parser[PlainText] = wordList ^^ (wordList => PlainText(wordList.mkString(" ")))

    def button: Parser[Button] = "[" ~> wordList <~ "]" ^^ (wordList => Button(wordList.mkString(" ")))

    def uncheckedRadio: Parser[UncheckedRadio] = "()" ~> wordList ^^ (wordList => UncheckedRadio(wordList.mkString(" ")))

    object UdashAsText {
      def render(value: Parse.ParseResult[List[Elements]]) = value match {
        case Success(result, _) => "div(\n" + result.map {

          case PlainText(s) => s"""label("$s")"""
          case UncheckedRadio(s) => s"""RadioButtons("$s")""" // todo https://guide.udash.io/ext/bootstrap
          case Button(s) => s"""button("$s")"""

        }.mkString(",\n") + "\n)"
      }
    }

  }

}

import java.awt.Desktop
import java.io._
import scala.io.Source

object Compiler {
  //variable declarations
  var currentToken: String = ""
  var wholeFile: String = ""
  var fileArr: Array[Char] = Array()

  val lexer = new Lexer
  val syner = new Syner
  val semer = new MySemanticAnalyzer

  var atEnd: Boolean = false
  var lineNum: Int = 0 //count # lines for debug/error message purposes
  var currentLine: String = ""
  var tokens = List()
  var pos: Int = -1
  var count: Int = 0
  var filename: String = ""

  def currentT(t: String) = {
    currentToken = t
  }

  def main(args: Array[String]): Unit = {
    checkUserInput(args)
    filename = args(0)
    fileArr = wholeFile.toCharArray
    while (lexer.pos < fileArr.length - 1 && !atEnd) {
      lexer.getNextToken()
      syner.gittex()

      if (currentToken.equalsIgnoreCase(CONSTANTS.DOCE))
        atEnd = true
    } //end while not done with file
    semer.analyze()
    openHTMLFileInBrowser(args(0))
  } //end main


  //read the file into one giant string
  def getText(f: String) = {
    val src = scala.io.Source.fromFile(f)
    wholeFile = try src.mkString finally src.close()
  }

  //make sure the user input is good
  def checkUserInput(args: Array[String]) = {
    //if they didn't provide any arguments...
    if (args.length == 0) {
      println("Error: You need to provide a file!")
      System.exit(1)
    }

    //if there are too many arguments
    if (args.length >= 1) {
      println("Error: You may only provide one input file at a time.")
      System.exit(1)
    }

    //if the file is of the wrong type...
    if (!args(0).endsWith(".gtx")) {
      println("Error: Input file must be of filetype .gtx")
      System.exit(1)
    }
  }

}
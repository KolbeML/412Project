

object Compiler{
  //variable declarations
  var currentT : String = ""
  var wholeFile : String = ""

  val lexer = new Lexer
  val syner = new MySyntaxAnalyzer
  val semer = new MySemanticAnalyzer
  var atEnd : Boolean = false
  var lineNum = 0 //count # lines for debug/error message purposes

  var filename: String = ""


  def currentT(t : String) = {

  }

  def main(args : Array[String]) = {
    filename = args(0)

    checkUserInput(args)
    getText(args(0))

    lexer.start(wholeFile)

    //loops through the file
    while (lexer.filePosition < lexer.filesize && !atEnd) {
      lexer.getNextToken()
      syner.gittex()

      if (currentT.equalsIgnoreCase("\\END")) {
        atEnd = true
      }
    }
    semer.symantics()
  }

  //read the file into one giant string
  def getText(f : String) = {
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
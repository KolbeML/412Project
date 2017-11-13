import java.awt.Desktop
import java.io._

object Compiler{
  //variable declarations
  var currentT : String = ""
  var wholeFile : String = ""
  var fileArr : Array[Char] = Array()

  val lexer = new Lexer
  val syner = new Syner
  val semer = new MySemanticAnalyzer
  var atEnd : Boolean = false
  var lineNum : Int = 0 //count # lines for debug/error message purposes
  var tokens = List()

  var pos : Int = -1
  var size : Int = 0
  var count : Int = 0


  def currentT(t : String) = {

  }

  def main(args: Array[String]): Unit = {
    checkUserInput(args)
    getText(args(0))
    fileArr = wholeFile.toCharArray
    lexer.getNextToken()

  }

//  def main(args : Array[String]) = {
//    lexer.nextC = 0
//
//    while(fileArr(lexer.nextC + 1) != Nil) {
//      lexer.getNextToken()
//      lexer.tempT :: tokens
//    }
//    lexer.gittex()
//
//    semer.initStack()
//    semer.variableRes()
//
//    val output = new File(filename + ".html")
//
//    val bw = new BufferedWriter(new FileWriter(output))
//    while(semer.convertStack(count) != Nil) {
//      bw.write(semer.convertStack(count))
//      count += 1
//    }
//    bw.close()
//
//    openHTMLFileInBrowser(args(0))
//  }

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

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }


}
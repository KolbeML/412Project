class MySemanticAnalyzer {

  import scala.collection.mutable.Stack
  import java.io._
  import java.awt.Desktop
  import java.io.{File, IOException}

  class MySemanticAnalyzer {
    var outputS = Stack[String]()
    var parse = Stack[String]()
    var vName = new Array[String](10)
    var defn = new Array[String](10)
    var nextT: String = ""
    var output: String = ""
    var count: Int = 0
    var printed: Boolean = false

    def analyze(): Unit = {
      //preps for method
      parse = Compiler.syner.parse.reverse
      nextT = parse.pop()

      lexIt()
    }

    //while statment to translante text
    def lexIt() {
      while (!parse.isEmpty) {
        if (nextT.equalsIgnoreCase(CONSTANTS.DOCB)) {
          outputS.push("<html>")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.TITLEB)) {
          outputS.push("<head>")
          outputS.push("<title>")
          outputS.push(parse.pop())
          outputS.push("</title>")
          outputS.push("</head>")
          parse.pop()
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.HEADING)) {
          outputS.push("<h1>")
          outputS.push(parse.pop())
          outputS.push("</h1>")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.PARAB)) {
          outputS.push("<p>")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.PARAE)) {
          outputS.push("</p>")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.BOLD)) {
          outputS.push("<b>")
          outputS.push(parse.pop())
          outputS.push("</b>")
          parse.pop()
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
          outputS.push("<li>")
          nextT = parse.pop()
          if (nextT.contains("\n") && !parse.isEmpty && !nextT.equalsIgnoreCase(CONSTANTS.DOCE)) {
            outputS.push(nextT)
          }
          else {
            if (!nextT.equalsIgnoreCase(CONSTANTS.DOCE))
              lexIt()
          }
          outputS.push("</li>")
          if (!parse.isEmpty)
            nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
          outputS.push("<br>")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          val temp = parse.pop()
          parse.pop()
          parse.pop()
          nextT = parse.pop()
          parse.pop()

          outputS.push("<a href = \"")
          outputS.push(nextT)
          outputS.push("\">")
          outputS.push(temp)
          outputS.push("</a> ")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
          val temp = parse.pop()
          parse.pop()
          parse.pop()
          nextT = parse.pop()
          parse.pop()

          outputS.push("<img src =\"")
          outputS.push(nextT)
          outputS.push("\" alt=\"")
          outputS.push(temp)
          outputS.push("\">")
          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.DEFB)) {
          var name = parse.pop()
          parse.pop()
          val mean = parse.pop()
          parse.pop()
          name = name.filter(!" ".contains(_))
          //var and def are pased to arrays
          val defed = name.indexOf(name)
          if (defed != -1) {
            vName(defed) = name
            defn(defed) = mean
          }
          else {
            vName(count) = name
            defn(count) = mean
            count += 1
          }

          nextT = parse.pop()
        }
        else if (nextT.equalsIgnoreCase(CONSTANTS.USEB)) {
          var name: String = parse.pop()
          parse.pop()
          name = name.filter(!" ".contains(_))
          //test for var
          if (vName.contains(name))
            outputS.push(defn(vName.indexOf(name)))
          else {
            println("Semantic error: Undefined variable")
            System.exit(1)
          }
          nextT = parse.pop()
        }

        else if (nextT.equalsIgnoreCase(CONSTANTS.DOCE)) {
          outputS.push("</html>")
        }
        else {
          outputS.push(nextT)
          nextT = parse.pop()
        }
      }

      //print output stack to file
      val output = outputS.reverse.mkString
      val print = new PrintWriter(new File(Compiler.filename + ".html"))
      print.write(output)
      print.close

      //calls html to open
      if (!printed) {
        openHTMLFileInBrowser(Compiler.filename + ".html")
        printed = true
      }
    }
  }

  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr: String) = {
    val file: File = new File(htmlFileStr.trim)
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

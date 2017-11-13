import scala.collection.mutable.Stack

class Syner extends SyntaxAnalyzer {

  var parse = Stack[String]()
  var found: Boolean = false;

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      Compiler.lexer.getNextToken()
    }
    else {
      println("Error: No beginning!")
      System.exit(1)
    }
    title()
    body()
    docENode()
  }

  override def paragraph(): Unit = {
    parBNode()
    variableDefine()
    while (!Compiler.currentToken.equals(CONSTANTS.DOCE) && !Compiler.currentToken.equals(CONSTANTS.PARAE)) {
      innerText()
    }
    parENode()
  }

  def innerItem(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
      link()
      innerItem()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      found = true
    }
    else if (Compiler.lexer.isValid(Compiler.currentToken)) {
      text()
      innerItem()
    }
    else
      found = true
  }

  def innerText(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      if (parse.contains(CONSTANTS.PARAB)) {
        parENode()
      }
      else {
        println("Syntax Error: \\PARAB was never defined")
        System.exit(1)
      }
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      heading()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      bold()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      listItem()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      image()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      variableUse()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
      link()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      innerText()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      return
    }
    else
      posText()
  }

  override def link(): Unit = {
    openBrack()
    text()
    closeBrack()
    openPar()
    text()
    closePar()
  }

  override def body(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      variableDefine()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      paragraph()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      newline()
      body()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {

    }
    else {
      innerText()
      body()
    }
  }

  override def bold(): Unit = {
    boldNode()
    posText()
    boldNode()
  }

  def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parse.push(CONSTANTS.NEWLINE)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted \\\\")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def title(): Unit = {
    titleNode()
    text()
    closeBrack()
  }

  def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      defBNode()
      text()
      equals()
      text()
      closeBrack()
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parse.push(CONSTANTS.IMAGEB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted ![")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  override def variableUse(): Unit = {
    usebNode()
    text()
    closeBrack()
  }

  override def heading(): Unit = {
    headerBNode()
    text()
  }

  override def listItem(): Unit = {
    listItemNode()
    innerItem()
  }

  //non-trait definitions:

  def text(): Unit = {
    if (Compiler.lexer.isValid(Compiler.currentToken)) {
      parse.push(Compiler.currentToken)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted text")
      println(Compiler.currentToken + "Was found instead")
      System.exit(1)
    }
  }

  def posText(): Unit = {
    if (Compiler.lexer.isValid(Compiler.currentToken)) {
      parse.push(Compiler.currentToken)
      Compiler.lexer.getNextToken()
    }
  }

  def closeBrack(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("]")) {
      parse.push("]")
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted ]")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def openBrack(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("[")) {
      parse.push("[")
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted [")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def openPar(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
      parse.push("(")
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted (")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def closePar(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(")")) {
      parse.push(")")
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted )")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def equals(): Unit = {
    if (Compiler.currentToken.equals(CONSTANTS.EQUALS)) {
      parse.push("=")
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted =")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def docBNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      parse.push(CONSTANTS.DOCB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted \\BEGIN")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def docENode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      parse.push(CONSTANTS.DOCE)
      Compiler.lexer.getNextToken()
      if (Compiler.lexer.nextC.equals('\n'))
        return
      else {
        println("Syntax Error: document keeps going after 'END' symbol")
        System.exit(1)
      }
    }
    else {
      println("Syntax Error: No \\END")
      println(Compiler.currentToken + "Was found instead")
      System.exit(1)
    }
  }

  def titleNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parse.push(CONSTANTS.TITLEB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted \\TITLE[")
      println("Found " + Compiler.currentToken + "instead.")
      System.exit(1)
    }
  }

  def headerBNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parse.push(CONSTANTS.HEADING)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted #")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def parBNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parse.push(CONSTANTS.PARAB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: wanted \\PARAB")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def parENode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
      parse.push(CONSTANTS.PARAE)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted \\PARAE")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def boldNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parse.push(CONSTANTS.BOLD)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted *")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def listItemNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parse.push(CONSTANTS.LISTITEM)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted + ")
      println("Found " + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def imagebNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parse.push(CONSTANTS.IMAGEB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted ![")
      println("Found" + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def usebNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parse.push(CONSTANTS.USEB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted \\USE[")
      println("Found" + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

  def defBNode(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parse.push(CONSTANTS.DEFB)
      Compiler.lexer.getNextToken()
    }
    else {
      println("Syntax Error: Wanted \\DEF[")
      println("Found" + Compiler.currentToken + " instead")
      System.exit(1)
    }
  }

}
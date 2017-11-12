import scala.collection.mutable.ArrayBuffer

//Overall Goal:
//The lexer processes the input stream into tokens.
//We want to identify if a given token is a special character,
//regular character, or unrecognized character.

import LexicalAnalyzer.LexicalAnalyzer

//For debugging purposes, everything is in alphabetical order atm. Will update to more logical
//order before final submission.

class Lexer extends LexicalAnalyzer
{
  var foundT : Boolean = false //did we find the next token?
  var isText : Boolean = false //is it text?
  var hasError: Boolean = false //is there an error?
  var lexeme = new ArrayBuffer[Char](100) //holds lexeme
  var lexemes: List[String] = List("\\BEGIN", //begins document
    "\\END",  //ends document
    "\\TITLE[", //begins title
    "#",   //heading
    "\\PARAB", //begin paragraph
    "PARAE", //ends paragraph
    "*", //bold text
    "+", //list item for unordered list
    "\\", //newline
    "[", //open bracket
    "![", //image
    "]", //end bracket
    "(", //open parentheses (for link/img address)
    ")", //close parentheses (for link/img address)
    "\\DEF[", //defining a variable
    "=", //for variable definitions
    "\\USE[") //set usage for variable
  var lexLength: Int = 0 //length of the lexeme
  def getError: Boolean = hasError //do we need to determine what the error is? - based on having an error
  var hasLexemes : Boolean = false //do we have the lexemes?
  var nextC: Char = ' ' //holds next character
  var pos: Int = 0 //current position in the given line
  def resetError() = hasError = false //resets value of hasError to false
  var srcLn: String = "" //the line we are currently working from
  def setError() = hasError = true //sets value of hasError to true
  var tempC : Char = ' ' //holds temp character
  var tempP : Int = 0 //holds temp position
  var tempT : String = "" //holds temp token

  //Add character to lexeme - unless it's a newline
  def addChar() : Unit =
  {
    if (lexLength <= srcLn.length())
    {
      if (!isNewline(nextC))
      {
        lexLength += 1
        lexeme += nextC
      }
    }
  }

  //check what the char is and call appropriate method
  def addLexC() : Unit =
  {
    nextC match
    {
      case '[' => findEndBracket()
      case '*' => idStar()
      case '+' => getList()
      case '\\' => idBSlash()
      case '(' => getAddr()
      case '!' => getImg()
      case '#' => getHead()
      case ']' => lexeme += nextC
      case _ => println("Lexical Error in line " + Compiler.lineNum + ": what is this: " + nextC + " ")
    }
  }

  def addLexType() : Unit =
  {
    nextC = tempC
    pos = tempP
    lexeme += nextC
    while(!isNewline(nextC) && !nextC.toString.equalsIgnoreCase("]"))
    {
      getChar()

      if(!isLex(nextC))
        addChar()

      else
        addLexC()
    }
  }

  //Self explanatory :P
  def addNewline() : Unit =
  {
    lexeme += nextC
    lexeme += nextC
    lookUp(lexeme.mkString)
  }

  //For when we need a closing bracket
  def findEndBracket() : Unit =
  {
    lexeme += nextC
    while (!nextC.toString.equals(CONSTANTS.BRACKETE) && !isNewline(nextC))
    {
      getChar()
      addChar()
    }

    if (isNewline(nextC) && !nextC.toString.equals(CONSTANTS.BRACKETE))
    {
      setError()
      println("Syntax Error in line " + Compiler.lineNum + "Missing closing bracket.")
    }
  }

  //For when we find a web address
  def getAddr() : Unit =
  {
    lexeme += nextC
    while (!nextC.toString.equals(CONSTANTS.ADDRESSE) && !isNewline(nextC))
    {
      getChar()
      addChar()
    }

    if (isNewline(nextC) && !nextC.toString.equals(CONSTANTS.BRACKETE))
    {
      setError()
      println("Lexical Error in line " + Compiler.lineNum + ": Missing closing parentheses")
    }
  }

  //Take next character from source line; if line done, make the next character a newline
  def getChar() : Char =
  {
    if (pos < srcLn.length())
    {
      nextC = srcLn.charAt(pos)
      pos += 1
    }

    else
      nextC = '\n'
    return nextC
  }

  //gets headings, defined by # symbol
  def getHead() : Unit =
  {
    lexeme += nextC

    while (!isNewline(nextC))
    {
      getChar()
      addChar()
    }
  }

  //For when we've found an image, marked by !
  def  getImg() : Unit =
  {
    lexeme += nextC
    while (!isNewline(nextC) && !nextC.toString.equals(CONSTANTS.ADDRESSE) )
    {
      getChar()
      addChar()
    }
  }

  //for when we've found a +, indicating a list item
  def getList() : Unit =
  {
    lexeme += nextC // add '+'
  }

  //Get and classify next token
  def getNextToken() : Unit =
  {
    lexeme.clear()
    lexLength = 0
    foundT = false
    getChar()

    if(isNewline(nextC))
      setCurrentT("\n")

    while(!isNewline(nextC) && !foundT)
    {
      if(isLex(nextC))
      {
        addLexC()
        setCurrentT(lexeme.mkString)
        if(!isLex(nextC))
          getChar()
      }

      else if(isValid(nextC.toString))
      {
        while(!isNewline(nextC) && !foundT )
        {
          if(isSpace(nextC)) {
            setCurrentT(lexeme.mkString)
            foundT = true
          }

          else
          {
            addChar() // addChar will handle non-lexeme supported chars.
            getChar()
          }
        }
      }

      else
      {
        if(isSpace(nextC))
        {
          if(!lexeme.contains(" ") && !isNewline(nextC))
          {
            setCurrentT(lexeme.mkString)
            foundT = true
          }
        }
      }

      if(!lexeme.mkString.contains("\\n"))
      {
        setCurrentT(lexeme.mkString)
        foundT = true
      }
    }
  }

  //called when we find double backslashes (\\)
  def idBSlash() : Unit =
  {
    tempP = pos
    tempC = nextC
    getChar()

    if(nextC.toString.equalsIgnoreCase("\\")) //if it's a newline...
      addNewline()

    else
      addLexType()
  }

  //Called when we pull up an asterisk (*), indicating bold text
  def idStar() : Unit =
  {
    var tempPos : Int = pos
    var tempChar : Char = nextC
    var foundTrailing : Boolean = false //gotta find that matching star
    getChar()

    if(nextC.toString.equalsIgnoreCase("\\\\")) //if it's a newline...
      addNewline()

    else
      addLexType()

    while(!foundTrailing || isNewline(nextC)) {
        getChar()

        if (!nextC.toString.equalsIgnoreCase("*")) //if it isnt a star
          addChar()

        else {
          tempPos = pos
          tempChar = nextC
          getChar()

          if (nextC.toString.equalsIgnoreCase("*"))
          {
            lexeme += nextC
            lexeme += nextC
            foundTrailing = true
          }

          else
          {
            println("Lexical Error in line " + Compiler.lineNum + ": Unmatched asterisk - needs its partner!)")
          }

        }
    }

    else
    {
      nextC = tempChar
      pos = tempPos
      lexeme += nextC

      while(!foundTrailing || isNewline(nextC))
      {
        getChar()
        if(!nextC.toString.equalsIgnoreCase())
          addChar()

        else
        {
          foundTrailing = true
          lexeme += nextC
        }
      }

      if(!foundTrailing)
      {
        println("Lexical Error in line " + Compiler.lineNum + ": Missing second asterisk)")
        setError()
      }
    }
  }

  //Is the current character a newline?
  def isNewline(c : Char) : Boolean =
  {
    c.toString.contains("\n")
  }

  //Is it one of our special characters?
  def isLex(nextChar : Char): Boolean =
  {
    nextChar match
    {
      case '[' =>  true
      case ']' =>  true
      case '!' =>  true
      case '(' =>  true
      case ')' =>  true
      case '*' => true
      case '#' => true
      case '\\' => true
      case '+' => true
      case  _  =>  false
    }
  }

  def isSpace(c : Char) : Boolean =
  {
    nextC.toString.equalsIgnoreCase(" ")
  }

  def isValid(s : String) : Boolean =
  {
    s match
    {
      case Patterns.generalTextPattern(_) => true
      case _ => false
    }
  }

  //is given string in a lexeme?
  def lookUp(t : String) : Boolean =
  {
    if (lexemes.contains(t))
      true

    else if(isText)
    {
      isText = false
      return true
    }

    else
    {
      Compiler.syner.setError()
      println("Line " + Compiler.lineNum + ": Lexical error: what is " + t + " ?")
      setError()
      lexeme.clear()
      return false
    }
  }

  //Get & Set current token
  def setCurrentT(t : String) : Unit =
  {
    Compiler.currentT(t)
  }

  //new line! Re-initialize everything.
  def start(l : String) : Unit =
  {
    srcLn = l
    pos = 0
    lexeme.clear()
    getNextToken()
    hasLexemes = false
  }

}

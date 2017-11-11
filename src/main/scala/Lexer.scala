import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

import LexicalAnalyzer.LexicalAnalyzer

//For debugging purposes, everything is in alphabetical order atm. Will update to more logical
//order before final submission.

class Lexer extends LexicalAnalyzer
{
  var foundT : Boolean = false //did we find the next token?
  var isText : Boolean = false //is it text?
  var hasError: Boolean = false //is there an error?
  var lexeme = new ArrayBuffer[Char](100) //holds lexeme
  var lexemes = new ListBuffer[String] //list of lexemes
  var lexLength: Int = 0 //length of the lexeme
  def getError: Boolean = hasError //do we need to determine what the error is? - based on having an error
  var hasLexemes : Boolean = false //do we have the lexemes?
  var nextC: Char = ' ' //holds next character
  var pos: Int = 0 //current position in the given line
  def resetError() = hasError = false //resets value of hasError to false
  var sourceLn: String = "" //the line we are currently working from
  def setError() = hasError = true //sets value of hasError to true
  var tempC : Char = ' ' //holds temp character
  var tempP : Int = 0 //holds temp position
  var tempT : String = "" //holds temp token

  //Add character to lexeme - unless it's a newline
  def addChar() : Unit =
  {
    if (lexLength <= sourceLn.length())
    {
      if (!isLast(nextC))
      {
        lexLength += 1
        lexeme += nextC
      }
    }
  }

  def addLexC() : Unit =
  {
    nextC match
    {
      case '[' => findEndBracket()
      case '*' => idStar()
      case '+' => getList()
      case '\\' => idSlash()
      case '(' => getAddr()
      case '!' => getImg()
      case '#' => getHead()
      case ']' => lexeme += nextC
      case _ => println("Error: encountered unknown lexem: " + nextC + " ")
    }
  }

  def addLexType() : Unit =
  {
    nextC = tempC
    pos = tempP
    lexeme += nextC // only add one \
    while(!isLast(nextC) && !nextC.toString.equalsIgnoreCase("]"))
    {
      getChar()

      if(!isLex(nextC))
        addChar()

      else
        addLexC()
    }
  }

  def addNewline() : Unit =
  {
    lexeme += nextC
    lexeme += nextC
    lookUp(lexeme.mkString)
  }

  def findEndBracket() : Unit =
  {
    lexeme += nextC
    while (!nextC.toString.equals(CONSTANTS.BRACKETE) && !isLast(nextC))
    {
      getChar()
      addChar()
    }

    if (isLast(nextC) && !nextC.toString.equals(CONSTANTS.BRACKETE))
    {
      setError()
      println("Line: " + Compiler.lineCount + " Syntax error: Missing closing bracket.")
    }
  }

  def getAddr() : Unit =
  {
    lexeme += nextC
    while (!nextC.toString.equals(CONSTANTS.ADDRESSE) && !isLast(nextC))
    {
      getChar()
      addChar()
    }

    if (isLast(nextC) && !nextC.toString.equals(CONSTANTS.BRACKETE))
    {
      setError()
      println("Line: " + Compiler.lineCount + " Syntax error: Missing closing parentheses")
    }
  }

  //Take next character from source line; if line done, make the next character a newline
  def getChar() : Char =
  {
    if (pos < sourceLn.length())
    {
      nextC = sourceLn.charAt(pos)
      if (Compiler.debugMode)
        println("Next char is: " + nextC)
      pos += 1
    }

    else
      nextC = '\n'
    return nextC
  }

  def getHead() : Unit =
  {
    lexeme += nextC // add '#'

    while (!isLast(nextC)) // headings on their own line
    {
      getChar()
      addChar()
    }
  }

  def  getImg() : Unit =
  {
    lexeme += nextC
    while (!isLast(nextC) && !nextC.toString.equals(CONSTANTS.ADDRESSE) )
    {
      getChar()
      addChar()
    }
  }

  def getList() : Unit =
  {
    lexeme += nextC // add '+'
  }

  //Get and classify next token
  def getToken() : Unit =
  {
    lexeme.clear()
    lexLength = 0
    foundT = false
    getChar()

    if(isLast(nextC))
      setCurrentT("\n")

    while(!isLast(nextC) && !foundT)
    {
      if(isLex(nextC))
      {
        addLexC() // addChar will handle finding valid lexemes in tokens.
        setCurrentT(lexeme.mkString)
        if(!isLex(nextC))
          getChar()
      }

      else if(isValid(nextC.toString))
      {
        // all supported characters...   The only allowed plain text in our language is: A-Z, a-z, 0-9, commas, period, quotes, colons, question marks, underscores and forward slashes.
        while(!isLast(nextC) && !foundT )
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
          if(!lexeme.contains(" ") && !isLast(nextC))
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

  def idSlash() : Unit =
  {
    tempP = pos
    tempC = nextC
    getChar()

    if(nextC.toString.equalsIgnoreCase(CONSTANTS.NEWLINE)) //if it's a newline...
      addNewline()

    else
      addLexType()
  }

  def idStar() : Unit =
  {
    var tempPos : Int = pos
    var tempChar : Char = nextC
    var foundTrailing : Boolean = false

    getChar()

    if(nextC.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
    {
      lexeme += nextC
      lexeme += nextC

      while(!foundTrailing || isLast(nextC)) {
        getChar()

        if (!nextC.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
          addChar()

        else
        {
          tempPos = pos // repeat to find bold closing
          tempChar = nextC
          getChar()

          if (nextC.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
          {
            lexeme += nextC
            lexeme += nextC
            foundTrailing = true
          }

          else
          {
            println("Line : " + Compiler.lineCount + " Lexical Error: Unclosed Asterisk. Expected a second asterisk.)")
          }
        }
      }
    }

    else
    {
      nextC = tempChar
      pos = tempPos
      lexeme += nextC

      while(!foundTrailing || isLast(nextC))
      {
        getChar()
        if(!nextC.toString.equalsIgnoreCase(CONSTANTS.ITALICS))
          addChar()

        else
        {
          foundTrailing = true
          lexeme += nextC
        }
      }

      if(!foundTrailing)
      {
        println("Line : " + Compiler.lineCount + " Lexical Error: Unclosed Asterisk. Expected a second asterisk.)")
        setError()
      }
    }
  }

  def initializeLex() : Unit =
  {
    lexemes = CONSTANTS.validLexemes
  }

  //Is the current character a newline?
  def isLast(c : Char) : Boolean =
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

  def lookUp(t : String) : Boolean =
  {
    if (Compiler.debugMode)
      println("Candidate Token: " + t)

    if (lexemes.contains(t))
      true

    else if(isText)
    {
      if (Compiler.debugMode)
        println("isText: Valid Token: " + t + " found.")
      isText = false
      return true
    }

    else
    {
      Compiler.Parser.setError()
      println("Line " + Compiler.lineCount + ": LEXICAL ERROR - " + t + " is not recognized.")
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
    initializeLex()
    sourceLn = l
    pos = 0
    lexeme.clear()
    getToken()
    hasLexemes = false
  }

}

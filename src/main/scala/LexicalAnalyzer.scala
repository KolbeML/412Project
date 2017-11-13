package LexicalAnalyzer

//Skeleton for the lexer

trait LexicalAnalyzer {
  def addChar(): Unit

  def getChar(): Char

  def getNextToken(): Unit

  def lookUp(): Boolean
}

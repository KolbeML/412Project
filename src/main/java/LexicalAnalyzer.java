//imports I'll probably need
import java.lang.*;
import java.util.List;
import java.util.ArrayList;

public class LexicalAnalyzer {
    //taken from simplecompiler...will need to update
    public void start(String line){

        initializeLexems(); //doesn't do anything yet
        sourceLine = line;
        position = 0;

        getChar();
        getNextToken();
    }

    //also taken from simplecompiler, will need to update.
    public void getNextToken() {
        lexLength = 0;

        // Ignore spaces and add the first character to the token
        getNonBlank();
        addChar();
        getChar();

        // Continue gathering characters for the token
        while((nextChar != '\n') && (nextChar != ' ')){
            addChar();
            getChar();
        }

        // Convert the gathered character array token into a String
        String newToken = new String(lexeme);

        if(lookup(newToken.substring(0, lexLength)))
            Compiler.currentToken_$eq(newToken.substring(0, lexLength));
    }

    //another from simplecompiler
    private boolean lookup(String candidateToken){
        if(!lexems.contains(candidateToken)){
            Compiler.Parser().setError();
            System.out.println("LEXICAL ERROR - '" + candidateToken + "' is not recognized.");
            return false;
        }
        return true;
    }

    //another from simplecompiler
    private void getChar(){
        if (position < sourceLine.length())
            nextChar = sourceLine.charAt(position++);
        else nextChar = '\n';
    }

    //another from simplecompiler
    private void addChar(){
        if(lexLength <= 98){
            lexeme[lexLength++] = nextChar;
            lexeme[lexLength] = 0;
        }
        else{
            System.out.println("LEXICAL ERROR - The found lexeme is too long!");

            if(!isSpace(nextChar)){
                while(!isSpace(nextChar)){
                    getChar();
                }
            }
            lexLength = 0;
            getNonBlank();
            addChar();

        }
    }

    //add lexems to language
    private void initializeLexems() {
        //ex: lexems.add("\n");
    }
}


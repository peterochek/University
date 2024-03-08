import org.junit.jupiter.api.Test;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class LexicalAnalyzerTest {

    @Test
    public void testInputs() throws ParseException {
        testTokens("val x  : Int = 5;", List.of(Token.VAL, Token.NAME, Token.COLON, Token.TYPE, Token.EQ, Token.NUMBER, Token.SEMICOLON, Token.END));
        testTokens("0 -0 1 -100 100 1000000 4567", List.of(Token.NUMBER, Token.NUMBER, Token.NUMBER, Token.NUMBER, Token.NUMBER, Token.NUMBER, Token.NUMBER, Token.END));
        testTokens("==", List.of(Token.EQ, Token.EQ, Token.END));
        testTokens("var val = 45", List.of(Token.VAR, Token.VAL, Token.EQ, Token.NUMBER, Token.END));
        testTokens("var val", List.of(Token.VAR, Token.VAL, Token.END));
        testTokens("", List.of(Token.END));
        testTokens("vam", List.of(Token.NAME, Token.END));
    }

    @Test
    public void testGood() {
        assertCorrect("   \t\tval x: Int = 5;");
        assertCorrect(" val x: Int = 5;");
        assertCorrect("val x: Int=5;");
        assertCorrect("val x:Int=5;");
        assertCorrect("val x:Int= 5;");
        assertCorrect("val x :Int = 5;");
        assertCorrect("val x : Int= 5;");
        assertCorrect("val x :Int= 5;");
        assertCorrect("val x:Int= 5;");
        assertCorrect("val x : Int =5;");
        assertCorrect("val x :Int=5;");
        assertCorrect("val x: Int= 5;");
        assertCorrect("val x: Int =5;");
        assertCorrect("val x:Int\n= 5;");
        assertCorrect("val x:Int=\t5;");
        assertCorrect("val x:Int= -5;");
        assertCorrect("val x : Int = \t-5;");
        assertCorrect("var x: Int = -5;");
        assertCorrect("var x : Int= -5;");
        assertCorrect("var x: Int=-5;");
        assertCorrect("var x : Int =-5;");
        assertCorrect("var x:Int\n= -5;");
        assertCorrect("var x:Int= -5;");
        assertCorrect("var x: Int= \t-5;");
        assertCorrect("var x: Int= -5; var y: Int= 314;");
        assertCorrect("val x:Int = 5; var y: Int= 314;");
    }

    private void assertCorrect(String input) {
        assertDoesNotThrow(() -> getTokens(input));
    }

    private void assertIncorrect(String input) {
        assertThrows(ParseException.class, () -> getTokens(input));
    }

    private void testTokens(String input, List<Token> expected) throws ParseException {
        List<Token> actual = getTokens(input);
        assertEquals(expected, actual);
    }


    private List<Token> getTokens(String input) throws ParseException {
        List<Token> tokens = new ArrayList<>();
        LexicalAnalyzer lex = new LexicalAnalyzer(input);
        Token token;
        do {
            lex.nextToken();
            token = lex.curToken();
            tokens.add(token);
        } while (token != Token.END);
        return tokens;
    }
}
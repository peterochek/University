import org.junit.jupiter.api.Test;

import java.text.ParseException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

class ParserTest {

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
        assertCorrect("val x: Int=-5;");
        assertCorrect("val vam : Int =-5;");
        assertCorrect("var int:Int\n= -5;");
        assertCorrect("val varr:Int= -5;");
        assertCorrect("var valr: Int= \t-5;");
        assertCorrect("var x: Int= -5; var y: Int= 314;");
        assertCorrect("val x:Int = 5; var y: Int= 314;");
    }

    @Test
    public void testBad() {
        assertIncorrect("   \t\tvam x: Int = 5;"); // var -> vam keyword
        assertIncorrect(" val x: Int == 5;"); // == -> =
        assertIncorrect("val x: Int=5"); // ; at the end
        assertIncorrect("val x:In=5;"); // In -> Int
        assertIncorrect("val x:Intt= 5;"); // Intt -> Int
        assertIncorrect("val Int :Int = 5;"); // Int -> literal (name)
        assertIncorrect("val val : Int= 5;"); // val -> literal (name)
        assertIncorrect("val var :Int= 5;"); // var -> literal (name)
        assertIncorrect("val 5ch:Int= 5;"); // 5ch -> literal (starts not with digit)
        assertIncorrect("val  : Int =5;"); // empty -> literal
        assertIncorrect("val x :=5;"); // empty -> Type (Int)
        assertIncorrect("val x: Int= ;"); // empty -> number
        assertIncorrect(" x: Int =5;"); // keyword at the beginning
        assertIncorrect("8  val x:Int\n= 5;"); // 8 -> incorrect token
        assertIncorrect("val x:Int=\t5.;"); // 5. -> 5 (incorrect number)
        assertIncorrect("val x:Int= 0-5;"); // 0-5 -> 5 incorrect number literal
        assertIncorrect("val x : Int = \t--05;"); // --05 -> 5 incorrect number literal
    }

    private void assertCorrect(String input) {
        assertDoesNotThrow(() -> parse(input));
    }

    private void assertIncorrect(String input) {
        assertThrows(ParseException.class, () -> parse(input));
    }

    private Tree parse(String input) throws ParseException {
        return new Parser().parse(input);
    }
}
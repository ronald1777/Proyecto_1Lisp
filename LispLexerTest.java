import org.junit.*;
import java.util.Arrays;
import java.util.List;

public class LispLexerTest {
    private LispLexer lexer;

    @Test
    public void testVerificarParentesis_ExpresionCorrecta() {
        lexer = new LispLexer();
        Assert.assertTrue(lexer.verificarParentesis("(+ 1 2)"));
        Assert.assertTrue(lexer.verificarParentesis("(- 9 (* 3 5))"));
    }

    @Test
    public void testVerificarParentesis_ExpresionIncorrecta() {
        lexer = new LispLexer();
        Assert.assertFalse(lexer.verificarParentesis("(+ 1 2")); // Falta cierre
        Assert.assertFalse(lexer.verificarParentesis(")(+ 1 2)(")); // Cierre sin apertura
    }

    @Test
    public void testTokenizar_ExpresionSimple() {
        lexer = new LispLexer();
        List<String> esperado = Arrays.asList("(", "+", "1", "2", ")");
        Assert.assertEquals(esperado, lexer.tokenizar("(+ 1 2)"));
    }

    @Test
    public void testTokenizar_ExpresionCompuesta() {
        lexer = new LispLexer();
        List<String> esperado = Arrays.asList("(", "-", "9", "(", "*", "3", "5", ")", ")");
        Assert.assertEquals(esperado, lexer.tokenizar("(- 9 (* 3 5))"));
    }
}
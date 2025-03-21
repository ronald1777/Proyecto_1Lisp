import org.junit.*;


public class LispLexerTest {
    private Lexer lexer;

    @Test
    public void testVerificarParentesis_ExpresionCorrecta() {
        lexer = new Lexer("(+ 1 2)");
        Assert.assertTrue(lexer.verificarParentesis());
    }

    @Test
    public void testVerificarParentesis_ExpresionIncorrecta() {
        lexer = new Lexer("(+ 1 2"); // Falta cierre
        Assert.assertFalse(lexer.verificarParentesis());
    }

    
}

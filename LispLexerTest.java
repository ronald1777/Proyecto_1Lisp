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
}
import java.util.ArrayList;
import java.util.List;

public class Parser {
    private Lexer lexer;
    private LispInterpreter.Token currentToken;

    public Parser(Lexer lexer) {
        this.lexer = lexer;
        this.lexer.reset();
        this.currentToken = lexer.nextToken();
    }

    public void eat(LispInterpreter.TokenType type) {
        if (currentToken.getType() == type) {
            currentToken = lexer.nextToken();
        } else {
            throw new IllegalArgumentException("Se esperaba " + type + ", pero se encontró " + currentToken.getType());
        }
    }

    public Node parse() {
        if (currentToken.getType() == LispInterpreter.TokenType.EOF) {
            throw new IllegalArgumentException("Error: Expresión vacía");
        }
        if (currentToken.getType() != LispInterpreter.TokenType.LPAREN) {
            throw new IllegalArgumentException("Error: Expresión LISP inválida, debe comenzar con '('");
        }
        return parseExpression();
    }

    private Node parseExpression() {
        if (currentToken.getType() == LispInterpreter.TokenType.LPAREN) {
            eat(LispInterpreter.TokenType.LPAREN);
            List<Node> nodes = new ArrayList<>();
            while (currentToken.getType() != LispInterpreter.TokenType.RPAREN && currentToken.getType() != LispInterpreter.TokenType.EOF) {
                nodes.add(parseExpression());
            }
            if (currentToken.getType() != LispInterpreter.TokenType.RPAREN) {
                throw new IllegalArgumentException("Error: Paréntesis de cierre sin apertura previa");
            }
            eat(LispInterpreter.TokenType.RPAREN);
            return new Node(nodes);
        } else if (currentToken.getType() == LispInterpreter.TokenType.SYMBOL) {
            String value = currentToken.getValue();
            eat(LispInterpreter.TokenType.SYMBOL);
            try {
                double num = Double.parseDouble(value);
                return new Node(num);
            } catch (NumberFormatException e) {
                return new Node(value);
            }
        } else {
            throw new IllegalArgumentException("Token inesperado: " + currentToken);
        }
    }
}
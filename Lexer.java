import java.util.ArrayList;
import java.util.List;

public class Lexer {
    private String input;
    private int position;
    private List<LispInterpreter.Token> tokens;

    public Lexer(String input) {
        this.input = input;
        this.position = 0;
        this.tokens = new ArrayList<>();
        tokenize();
    }

    public boolean verificarParentesis() {
        int contador = 0;
        for (int i = 0; i < input.length(); i++) {
            char caracter = input.charAt(i);
            if (caracter == '(') {
                contador++;
            } else if (caracter == ')') {
                contador--;
                if (contador < 0) {
                    return false;
                }
            }
        }
        return contador == 0;
    }

    private void tokenize() {
        StringBuilder tokenActual = new StringBuilder();
        int i = 0;
        while (i < input.length()) {
            char caracter = input.charAt(i);
            if (caracter == '(') {
                if (tokenActual.length() > 0) {
                    tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.SYMBOL, tokenActual.toString()));
                    tokenActual.setLength(0);
                }
                tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.LPAREN, "("));
            } else if (caracter == ')') {
                if (tokenActual.length() > 0) {
                    tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.SYMBOL, tokenActual.toString()));
                    tokenActual.setLength(0);
                }
                tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.RPAREN, ")"));
            } else if (caracter == ' ') {
                if (tokenActual.length() > 0) {
                    tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.SYMBOL, tokenActual.toString()));
                    tokenActual.setLength(0);
                }
            } else {
                tokenActual.append(caracter);
            }
            i++;
        }
        if (tokenActual.length() > 0) {
            tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.SYMBOL, tokenActual.toString()));
        }
        tokens.add(new LispInterpreter.Token(LispInterpreter.TokenType.EOF, ""));
    }

    public LispInterpreter.Token nextToken() {
        if (position < tokens.size()) {
            return tokens.get(position++);
        }
        return new LispInterpreter.Token(LispInterpreter.TokenType.EOF, "");
    }

    public void reset() {
        position = 0;
    }
}
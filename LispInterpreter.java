import java.util.List;
import java.util.Scanner;
import java.util.ArrayList;

public class LispInterpreter {
    public enum TokenType {
        LPAREN,
        RPAREN,
        SYMBOL,
        NUMBER,
        EOF
    }

    public static class Token {
        private TokenType type;
        private String value;

        public Token(TokenType type, String value) {
            this.type = type;
            this.value = value;
        }

        public TokenType getType() {
            return type;
        }

        public String getValue() {
            return value;
        }

        @Override
        public String toString() {
            return "Token(" + type + ", " + value + ")";
        }
    }

    private Environment env;

    public LispInterpreter() {
        this.env = new Environment();
    }

    private Object evaluate(Node node, Environment env) {
        if (!node.isList()) {
            Object value = node.getValue();
            if (value instanceof Double) {
                return value;
            } else if (value instanceof String) {
                return env.lookup((String) value);
            }
        }

        List<Node> list = node.getList();
        if (list.isEmpty()) {
            throw new IllegalArgumentException("Lista vacía no válida");
        }

        String operator = list.get(0).getValue().toString().toUpperCase();
        List<Node> args = list.subList(1, list.size());

        switch (operator) {
            case "DEFUN":
                String funcName = args.get(0).getValue().toString();
                List<Node> paramNodes = args.get(1).getList();
                List<String> params = new ArrayList<>();
                for (Node paramNode : paramNodes) {
                    params.add(paramNode.getValue().toString());
                }
                Node body = args.get(2);
                env.define(funcName, new Environment.UserDefinedFunction(params, body, env));
                return null;

            case "COND":
                for (Node clause : args) {
                    List<Node> clauseList = clause.getList();
                    boolean test = (Boolean) evaluate(clauseList.get(0), env);
                    if (test) {
                        return evaluate(clauseList.get(1), env);
                    }
                }
                return null;

            case "IF":
                boolean test = (Boolean) evaluate(args.get(0), env);
                return evaluate(test ? args.get(1) : args.get(2), env);

            case "PROGN":
                Object result = null;
                for (Node expr : args) {
                    result = evaluate(expr, env);
                }
                return result;

            case "LET":
                Environment localEnv = new Environment(env);
                for (Node bind : args.get(0).getList()) {
                    String var = bind.getList().get(0).getValue().toString();
                    Object val = evaluate(bind.getList().get(1), env);
                    localEnv.define(var, val);
                }
                Object letResult = null;
                for (int i = 1; i < args.size(); i++) {
                    letResult = evaluate(args.get(i), localEnv);
                }
                return letResult;

            case "=":
                return evaluate(args.get(0), env).equals(evaluate(args.get(1), env));

            case "<":
                return ((Double)evaluate(args.get(0), env)) < ((Double)evaluate(args.get(1), env));

            case ">":
                return ((Double)evaluate(args.get(0), env)) > ((Double)evaluate(args.get(1), env));

            case "+":
                double sum = 0;
                for (Node arg : args) {
                    sum += ((Double)evaluate(arg, env));
                }
                return sum;

            case "-":
                double sub = (Double)evaluate(args.get(0), env);
                for (int i = 1; i < args.size(); i++) {
                    sub -= ((Double)evaluate(args.get(i), env));
                }
                return sub;

            case "*":
                double prod = 1;
                for (Node arg : args) {
                    prod *= ((Double)evaluate(arg, env));
                }
                return prod;

            case "/":
                double div = (Double)evaluate(args.get(0), env);
                for (int i = 1; i < args.size(); i++) {
                    div /= ((Double)evaluate(args.get(i), env));
                }
                return div;

            default:
                Environment.UserDefinedFunction func = (Environment.UserDefinedFunction)env.lookup(operator.toLowerCase());
                Environment funcEnv = new Environment(func.getDefinitionEnv());
                for (int i = 0; i < func.getParams().size(); i++) {
                    funcEnv.define(func.getParams().get(i), evaluate(args.get(i), env));
                }
                return evaluate(func.getBody(), funcEnv);
        }
    }

    public Object interpret(String expression) {
        Lexer lexer = new Lexer(expression);
        if (!lexer.verificarParentesis()) {
            throw new IllegalArgumentException("Expresión inválida: paréntesis desbalanceados");
        }
        Parser parser = new Parser(lexer);
        Node parsed = parser.parse();
        return evaluate(parsed, env);
    }

    public void runREPL() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Lisp Interpreter REPL (type 'exit' to quit)");
        while (true) {
            System.out.print("> ");
            String input = scanner.nextLine().trim();
            if (input.equalsIgnoreCase("exit")) break;
            if (input.isEmpty()) continue;
            try {
                System.out.println(interpret(input));
            } catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
            }
        }
        scanner.close();
        System.out.println("Goodbye!");
    }

    public static void main(String[] args) {
        new LispInterpreter().runREPL();
    }
}
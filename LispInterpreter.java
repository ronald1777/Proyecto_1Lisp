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

        Object operatorObj = evaluate(list.get(0), env);
        if (operatorObj instanceof Environment.UserDefinedFunction) {
            Environment.UserDefinedFunction func = (Environment.UserDefinedFunction) operatorObj;
            List<Node> args = list.subList(1, list.size());

            List<Object> argValues = new ArrayList<>();
            for (Node arg : args) {
                argValues.add(evaluate(arg, env));
            }

            Environment funcEnv = new Environment(func.getDefinitionEnv());
            List<String> params = func.getParams();
            if (params.size() != args.size()) {
                throw new IllegalArgumentException("Número incorrecto de argumentos para la función: se esperaban " + params.size() + ", se recibieron " + args.size());
            }

            for (int i = 0; i < params.size(); i++) {
                funcEnv.define(params.get(i), argValues.get(i));
            }

            return evaluate(func.getBody(), funcEnv);
        }

        String operator = (String) operatorObj;
        List<Node> args = list.subList(1, list.size());

        switch (operator.toUpperCase()) {
            case "+":
                double sum = 0;
                for (Node arg : args) {
                    sum += ((Number) evaluate(arg, env)).doubleValue();
                }
                return sum;

            case "-":
                if (args.isEmpty()) {
                    throw new IllegalArgumentException("- requiere al menos un argumento");
                }
                double result = ((Number) evaluate(args.get(0), env)).doubleValue();
                for (int i = 1; i < args.size(); i++) {
                    result -= ((Number) evaluate(args.get(i), env)).doubleValue();
                }
                return result;

            case "*":
                double product = 1;
                for (Node arg : args) {
                    product *= ((Number) evaluate(arg, env)).doubleValue();
                }
                return product;

            case "/":
                if (args.isEmpty()) {
                    throw new IllegalArgumentException("/ requiere al menos un argumento");
                }
                double divResult = ((Number) evaluate(args.get(0), env)).doubleValue();
                for (int i = 1; i < args.size(); i++) {
                    double divisor = ((Number) evaluate(args.get(i), env)).doubleValue();
                    if (divisor == 0) {
                        throw new IllegalArgumentException("División por cero");
                    }
                    divResult /= divisor;
                }
                return divResult;

            case "QUOTE":
                if (args.size() != 1) {
                    throw new IllegalArgumentException("QUOTE requiere exactamente un argumento");
                }
                return args.get(0);

            case "DEFINE":
                if (args.size() != 2) {
                    throw new IllegalArgumentException("DEFINE requiere exactamente dos argumentos");
                }
                String symbol = (String) args.get(0).getValue();
                Object value = evaluate(args.get(1), env);
                env.define(symbol, value);
                return null;

            case "DEFUN":
                if (args.size() != 3) {
                    throw new IllegalArgumentException("DEFUN requiere exactamente tres argumentos: nombre, lista de parámetros y cuerpo");
                }
                String funcName = (String) args.get(0).getValue();
                Node paramListNode = args.get(1);
                if (!paramListNode.isList()) {
                    throw new IllegalArgumentException("El segundo argumento de DEFUN debe ser una lista de parámetros");
                }
                List<Node> paramNodes = paramListNode.getList();
                List<String> params = new ArrayList<>();
                for (Node paramNode : paramNodes) {
                    if (!paramNode.isList() && paramNode.getValue() instanceof String) {
                        params.add((String) paramNode.getValue());
                    } else {
                        throw new IllegalArgumentException("Los parámetros de DEFUN deben ser símbolos");
                    }
                }
                Node body = args.get(2);
                env.define(funcName, new Environment.UserDefinedFunction(params, body, env));
                return null;

            default:
                throw new IllegalArgumentException("Operador no soportado: " + operator);
        }
    }

    public Object interpret(String expression) {
        Lexer lexer = new Lexer(expression);
        if (!lexer.verificarParentesis()) {
            throw new IllegalArgumentException("Expresión LISP inválida: paréntesis desbalanceados");
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
            if (input.equalsIgnoreCase("exit")) {
                break;
            }
            if (input.isEmpty()) {
                continue;
            }
            try {
                Object result = interpret(input);
                if (result instanceof Node) {
                    System.out.println(result);
                } else if (result != null) {
                    System.out.println(result);
                }
            } catch (Exception e) {
                System.out.println("Error: " + e.getMessage());
            }
        }
        scanner.close();
        System.out.println("Goodbye!");
    }

    public static void main(String[] args) {
        LispInterpreter interpreter = new LispInterpreter();
        interpreter.runREPL();
    }
}
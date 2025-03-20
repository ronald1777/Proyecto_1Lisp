import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Environment {
    private Map<String, Object> variables;
    private Environment parent; // Para soportar entornos anidados (scopes)

    public Environment() {
        this.variables = new HashMap<>();
        this.parent = null;
    }

    public Environment(Environment parent) {
        this.variables = new HashMap<>();
        this.parent = parent;
    }

    // Clase interna para representar funciones definidas por el usuario
    public static class UserDefinedFunction {
        private List<String> params; // Lista de nombres de parámetros
        private Node body; // Cuerpo de la función (una expresión Lisp)
        private Environment definitionEnv; // Entorno en el que se definió la función

        public UserDefinedFunction(List<String> params, Node body, Environment definitionEnv) {
            this.params = params;
            this.body = body;
            this.definitionEnv = definitionEnv;
        }

        public List<String> getParams() {
            return params;
        }

        public Node getBody() {
            return body;
        }

        public Environment getDefinitionEnv() {
            return definitionEnv;
        }
    }

    public void define(String name, Object value) {
        variables.put(name, value);
    }

    public Object lookup(String name) {
        Environment current = this;
        while (current != null) {
            if (current.variables.containsKey(name)) {
                return current.variables.get(name);
            }
            current = current.parent;
        }
        throw new IllegalArgumentException("Símbolo no definido: " + name);
    }
}
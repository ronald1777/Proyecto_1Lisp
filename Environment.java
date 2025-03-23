import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Environment {
    private Map<String, Object> variables;
    private Environment parent;

    public Environment() {
        this.variables = new HashMap<>();
        this.parent = null;
    }

    public Environment(Environment parent) {
        this.variables = new HashMap<>();
        this.parent = parent;
    }

    public static class UserDefinedFunction {
        private List<String> params;
        private Node body;
        private Environment definitionEnv;

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

        @Override
        public String toString() {
            return "<UserDefinedFunction: params=" + params + " body=" + body + ">";
        }
    }

    public void define(String name, Object value) {
        if (variables.containsKey(name)) {
            System.out.println("Advertencia: Redefiniendo " + name);
        }
        variables.put(name, value);
    }

    public Object lookup(String name) {
        if (variables.containsKey(name)) {
            return variables.get(name);
        } else if (parent != null) {
            return parent.lookup(name);
        }
        throw new IllegalArgumentException("Símbolo no definido: " + name);
    }

    public boolean exists(String name) {
        return variables.containsKey(name) || (parent != null && parent.exists(name));
    }

    @Override
    public String toString() {
        return "Variables: " + variables.toString();
    }
}

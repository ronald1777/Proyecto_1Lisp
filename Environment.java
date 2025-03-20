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
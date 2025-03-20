import java.util.List;

public class Node {
    private Object value;
    private boolean isList;

    public Node(Object value) {
        this.value = value;
        this.isList = false;
    }

    public Node(List<Node> nodes) {
        this.value = nodes;
        this.isList = true;
    }

    public boolean isList() {
        return isList;
    }

    public Object getValue() {
        return value;
    }

    public List<Node> getList() {
        if (!isList) {
            throw new IllegalStateException("Este nodo no es una lista");
        }
        return (List<Node>) value;
    }

    @Override
    public String toString() {
        if (!isList) {
            return value.toString();
        } else {
            List<Node> nodes = getList();
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < nodes.size(); i++) {
                sb.append(nodes.get(i).toString());
                if (i < nodes.size() - 1) {
                    sb.append(" ");
                }
            }
            sb.append(")");
            return sb.toString();
        }
    }
}
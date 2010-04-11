import java.util.Map;
import java.util.HashMap;

public class TreeNode {
  private TreeNode left;
  private TreeNode right;
  private String name;
  private Object value;

  TreeNode(String name) {
    name = name;
    left = null;
    right = null;
  }

  TreeNode(String name, TreeNode l, TreeNode r) {
    name = name;
    left = l;
    right = r;
  }

  void Dump() {
    if (left != null) {
      left.Dump();
    }

    if (right != null) {
      right.Dump();
    }

    System.out.println(name);
  }
}

public class SearchMap {
  public static void main(String[] args) {
    Map<Object, Object> m = new HashMap<Object, Object>();

    m.put("one", new Integer(1));
    m.put("two", new Integer(2));
    m.put("three", new Integer(3));

    System.out.println(m.get(args[0]));
  }
}

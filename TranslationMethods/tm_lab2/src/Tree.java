import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Tree {
    private static int vertexId = -1;
    private final String node;

    public List<Tree> children = new ArrayList<>();

    public Tree(String node, Tree... trees) {
        this.node = node;
        this.children = Arrays.asList(trees);
    }

    public Tree(String node) {
        this.node = node;
    }

    public String getNode() {
        return node;
    }

    @Override
    public String toString() {
        return node + (children.isEmpty() ? "" : children.toString());
    }

    public void toDot(String fileName) throws IOException {
        StringBuilder sb = new StringBuilder("digraph G {\n");
        traverse(this, -1, sb);
        sb.append("}\n");
        try (PrintWriter out = new PrintWriter(fileName + ".dot")) {
            out.println(sb);
        }

        String dotFileName = fileName + ".dot";
        String pngFileName = fileName + ".png";

        ProcessBuilder builder = new ProcessBuilder(
                "dot", "-Tpng", dotFileName, "-o", pngFileName);
        builder.redirectErrorStream(true);
        Process process = builder.start();
        try {
            process.waitFor();
        } catch (InterruptedException e) {
            System.out.println("Error occurred: " + e);
        }
    }

    private static void traverse(Tree t, int prev, StringBuilder sb) {
        vertexId++;
        sb.append(String.format("%d [label = \"%s\"]\n", vertexId, t.getNode()));
        if (prev != -1) {
            sb.append(String.format("%d -> %d\n", prev, vertexId));
        }
        int p = vertexId;
        for (Tree ch : t.children) {
            traverse(ch, p, sb);
        }
    }
}
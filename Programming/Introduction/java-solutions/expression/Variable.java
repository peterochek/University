package expression;

import java.math.BigInteger;

public class Variable implements EE {
    private final String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public String toMiniString() {
        return name;
    }

    @Override
    public int evaluate(int x) {
        return x;
    }

    @Override
    public int evaluate (int x, int y, int z) {
        int result = -1;
        if ("x".equals(name)) {
            result = x;
        } else if ("y".equals(name)) {
            result = y;
        } else if ("z".equals(name)) {
            result = z;
        }
        return result;
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        return x;
    }

    @Override
    public boolean equals(Object exp) {
        if (exp instanceof Variable) {
            return (this.name.equals(((Variable) exp).name));
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }
}

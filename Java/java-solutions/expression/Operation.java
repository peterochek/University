package expression;

import java.math.BigInteger;
import java.util.Objects;

public abstract class Operation implements EE {
    private final EE left;
    private final EE right;

    public Operation(EE left, EE right) {
        this.left = left;
        this.right = right;
    }

    protected abstract int calculate(int left, int right);
    protected abstract BigInteger calculate(BigInteger left, BigInteger right);

    protected abstract String symbol();

    protected abstract int priority();

    public String toString() {
        return "(" + left.toString() +
                " " +
                this.symbol() +
                " " +
                right.toString() +
                ")";
    }

    private String exprString(Expression e, boolean br) {
        if (br) {
            return "(" + e.toMiniString() + ")";
        } else {
            return e.toMiniString();
        }
    }

    private boolean isLower(Expression e) {
        return (e instanceof Operation && ((Operation) e).priority() < this.priority());
    }

    private boolean br(Expression e) {
        return (e instanceof Operation &&
                ((this instanceof Subtract && ((Operation) e).priority() == 0) || this instanceof Divide)) ||
                (e instanceof Divide && this.priority() == 1);
    }

    public String toMiniString() {
        return exprString(this.left, isLower(this.left)) + " " + symbol() + " " +
                exprString(this.right, isLower(this.right) || br(this.right));
    }

    public int evaluate(int x) {
        int left = this.left.evaluate(x);
        int right = this.right.evaluate(x);
        return calculate(left, right);
    }

    public BigInteger evaluate(BigInteger x) {
        BigInteger left = this.left.evaluate(x);
        BigInteger right = this.right.evaluate(x);
        return calculate(left, right);
    }
    public int evaluate(int x, int y, int z) {
        int left = this.left.evaluate(x, y, z);
        int right = this.right.evaluate(x, y, z);
        return calculate(left, right);
    }

    @Override
    public boolean equals(Object exp) {
        if (exp != null && exp.getClass() == getClass()) {
            return ((Operation) exp).left.equals(this.left) && ((Operation) exp).right.equals(this.right)
                    && ((Operation) exp).symbol().equals(this.symbol());
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        int left = this.left.hashCode();
        int right = this.right.hashCode();
        return Objects.hash(left, right, this.symbol());
    }
}

package expression;

import java.math.BigInteger;
import java.util.Objects;

public class Const implements EE {
    private final BigInteger value;

    public Const(int value) {
        this.value = BigInteger.valueOf(value);
    }

    public Const(BigInteger bigIntValue) {
        this.value = bigIntValue;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    @Override
    public String toMiniString() {
        return value.toString();
    }

    @Override
    public int evaluate(int x) {
        return value.intValue();
    }

    @Override
    public BigInteger evaluate(BigInteger x) {
        return value;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return value.intValue();
    }

    @Override
    public boolean equals(Object exp) {
        if (exp instanceof Const) {
            return (Objects.equals(value, ((Const) exp).value));
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }
}

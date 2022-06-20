package expression;

import java.math.BigInteger;

public class Subtract extends Operation {
    public Subtract(EE left, EE right) {
        super(left, right);
    }

    @Override
    public int calculate(int left, int right) {
        return left - right;
    }

    @Override
    public BigInteger calculate(BigInteger left, BigInteger right) {
        return left.subtract(right);
    }

    @Override
    public String symbol() {
        return "-";
    }

    @Override
    public int priority() {
        return 0;
    }
}

package expression;

import java.math.BigInteger;

public interface EE extends Expression, BigIntegerExpression, TripleExpression {
    String toString();

    String toMiniString();

    BigInteger evaluate(BigInteger x);
    int evaluate(int x, int y, int z);
    int evaluate(int x);
}

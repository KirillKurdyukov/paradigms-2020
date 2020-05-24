package cljtest.parsing;

import jstest.ArithmeticTests;

import java.util.function.DoubleBinaryOperator;
import java.util.function.LongBinaryOperator;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureBitwiseParsingTest extends ClojureObjectParsingTest {
    static {
        PARSED.rename("&", "And");
        PARSED.rename("|", "Or");
        PARSED.rename("^", "Xor");
    }

    public ClojureBitwiseParsingTest(final BitwiseTests tests, final boolean hard) {
        super(tests, hard);
        priorities.put("&", 50);
        priorities.put("|", 30);
        priorities.put("^", 10);
    }

    public static void main(final String... args) {
        new ClojureBitwiseParsingTest(new BitwiseTests(), mode(args, ClojureBitwiseParsingTest.class)).run();
    }

    static DoubleBinaryOperator logic(final LongBinaryOperator op) {
        return (a, b) -> Double.longBitsToDouble(op.applyAsLong(Double.doubleToLongBits(a), Double.doubleToLongBits(b)));
    }

    public static class BitwiseTests extends ArithmeticTests {{
        binary("&", logic((a, b) -> a & b));
        binary("|", logic((a, b) -> a | b));
        binary("^", logic((a, b) -> a ^ b));
        tests(
                f("&", vx, vy),
                f("|", vx, vy),
                f("^", vx, vy),
                f("&", vx, f("-", vy, vz)),
                f("|", vx, f("-", vy, vz)),
                f("^", vx, f("-", vy, vz))
        );
    }}
}

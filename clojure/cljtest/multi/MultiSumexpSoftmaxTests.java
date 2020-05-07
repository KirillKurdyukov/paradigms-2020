package cljtest.multi;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class MultiSumexpSoftmaxTests extends MultiTests {
    public MultiSumexpSoftmaxTests(final boolean testMulti) {
        super(testMulti);
        final int arity = testMulti ? -1 : 2;
        any("sumexp", arity, MultiSumexpSoftmaxTests::sumexp);
        any("softmax", arity, args -> Math.exp(args.get(0)) / sumexp(args));
        tests(
                f("sumexp", vx, vy),
                f("softmax", vx, vy),
                f("sumexp", vx, c(3)),
                f("softmax", vx, c(3)),
                f("softmax", vx, f("sumexp", vy, vz)),
                f("sumexp", vx, f("softmax", vy, vz)),
                f("/", vz, f("softmax", vx, vy)),
                f("+", f("sumexp", f("+", vx, c(10)), f("*", vz, f("*", vy, c(0)))), c(2))
        );
        randomMulti(testMulti, "sumexp", "softmax");
    }

    private static double sumexp(final List<Double> args) {
        return args.stream().mapToDouble(Math::exp).sum();
    }
}

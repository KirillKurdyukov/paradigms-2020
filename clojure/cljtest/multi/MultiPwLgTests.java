package cljtest.multi;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class MultiPwLgTests extends MultiTests {
    public MultiPwLgTests(final boolean testMulti) {
        super(testMulti);
        binary("pw", Math::pow);
        binary("lg", (a, b) -> Math.log(Math.abs(b)) / Math.log(Math.abs(a)));
        tests(
                f("pw", vx, vy),
                f("lg", vx, vy),
                f("pw", vx, f("-", vy, vz)),
                f(
                        "pw",
                        c(2),
                        f("+", c(1), f("*", c(2), f("-", vy, vz)))
                ),
                f(
                        "lg",
                        f("+", c(2), f("*", c(4), f("-", vx, vz))),
                        f("+", c(1), f("*", c(2), f("-", vy, vz)))
                )
        );
    }
}

package cljtest.object;

import cljtest.multi.MultiSquareSqrtTests;
import cljtest.functional.ClojureFunctionalExpressionTest;
import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureObjectSquareSqrtTest extends ClojureObjectExpressionTest {
    public static final Dialect PARSED = ClojureObjectExpressionTest.PARSED.copy()
            .rename("square", "Square")
            .rename("sqrt", "Sqrt");

    protected ClojureObjectSquareSqrtTest(final boolean testMulti) {
        super(new Language(PARSED, ClojureFunctionalExpressionTest.UNPARSED, new MultiSquareSqrtTests(testMulti)));
    }

    public static void main(final String... args) {
        new ClojureObjectSquareSqrtTest(mode(args, ClojureObjectSquareSqrtTest.class)).run();
    }
}

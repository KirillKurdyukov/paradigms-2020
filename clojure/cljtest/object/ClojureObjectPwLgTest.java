package cljtest.object;

import cljtest.functional.ClojureFunctionalExpressionTest;
import cljtest.multi.MultiPwLgTests;
import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureObjectPwLgTest extends ClojureObjectExpressionTest {
    public static final Dialect PARSED = ClojureObjectExpressionTest.PARSED.copy()
            .rename("pw", "Pw")
            .rename("lg", "Lg");

    protected ClojureObjectPwLgTest(final boolean testMulti) {
        super(new Language(PARSED, ClojureFunctionalExpressionTest.UNPARSED, new MultiPwLgTests(testMulti)));
    }

    public static void main(final String... args) {
        new ClojureObjectPwLgTest(mode(args, ClojureObjectPwLgTest.class)).run();
    }
}

package cljtest.object;

import cljtest.functional.ClojureFunctionalExpressionTest;
import cljtest.multi.MultiSumAvgTests;
import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureObjectSumAvgTest extends ClojureObjectExpressionTest {
    public static final Dialect PARSED = ClojureObjectExpressionTest.PARSED.copy()
            .rename("sum", "Sum")
            .rename("avg", "Avg");

    protected ClojureObjectSumAvgTest(final boolean testMulti) {
        super(new Language(PARSED, ClojureFunctionalExpressionTest.UNPARSED, new MultiSumAvgTests(testMulti)));
    }

    public static void main(final String... args) {
        new ClojureObjectSumAvgTest(mode(args, ClojureObjectSumAvgTest.class)).run();
    }
}

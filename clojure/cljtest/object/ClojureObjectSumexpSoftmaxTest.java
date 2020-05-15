package cljtest.object;

import cljtest.functional.ClojureFunctionalExpressionTest;
import cljtest.multi.MultiSumexpSoftmaxTests;
import jstest.Language;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureObjectSumexpSoftmaxTest extends ClojureObjectExpressionTest {
    public static final Dialect PARSED = ClojureObjectExpressionTest.PARSED.copy()
            .rename("sumexp", "Sumexp")
            .rename("softmax", "Softmax");

    protected ClojureObjectSumexpSoftmaxTest(final boolean testMulti) {
        super(new Language(PARSED, ClojureFunctionalExpressionTest.UNPARSED, new MultiSumexpSoftmaxTests(testMulti)));
    }

    public static void main(final String... args) {
        new ClojureObjectSumexpSoftmaxTest(mode(args, ClojureObjectSumexpSoftmaxTest.class)).run();
    }
}

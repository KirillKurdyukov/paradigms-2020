package prtest.tree;

import prtest.PrologScript;
import prtest.map.Settings;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologTreeMinMaxTest extends PrologTreeTest {
    public PrologTreeMinMaxTest(final boolean updates) {
        super(updates);
    }

    class MinMaxTreeTest extends TreeTest {
        public MinMaxTreeTest(final Settings settings) {
            super(settings);
        }

        @Override
        protected void check() {
            super.check();
            prolog.assertQuery(expected.isEmpty() ? null : expected.firstKey(), "map_minKey", actual, PrologScript.V);
            prolog.assertQuery(expected.isEmpty() ? null : expected.lastKey(), "map_maxKey", actual, PrologScript.V);
        }
    }

    @Override
    protected MapTest<?> test(final Settings settings) {
        return new MinMaxTreeTest(settings);
    }

    public static void main(final String... args) {
        new PrologTreeMinMaxTest(hasUpdates(args, PrologTreeMinMaxTest.class)).test();
    }
}

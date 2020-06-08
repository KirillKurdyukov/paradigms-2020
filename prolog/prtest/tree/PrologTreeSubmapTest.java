package prtest.tree;

import prtest.PrologScript;
import prtest.map.Settings;

import java.util.function.IntConsumer;
import java.util.function.Predicate;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologTreeSubmapTest extends PrologTreeTest {
    public PrologTreeSubmapTest(final boolean updates) {
        super(updates);
    }

    class ReplaceTreeTest extends TreeTest {
        public ReplaceTreeTest(final Settings settings) {
            super(settings);
            actions.clear();

            actions(() -> randomBound(f -> randomBound(t -> subsetSize(f, t))));
            if (updates) {
                actions(this::putExisting, this::putMissing);
            }
        }

        private boolean randomBound(final IntConsumer cation) {
            final Predicate<IntConsumer> generator = random.nextBoolean() ? this::existingKey : this::uniqueKey;
            return generator.test(cation);
        }

        private void subsetSize(final int b1, final int b2) {
            final int from = Math.min(b1, b2);
            final int to   = Math.max(b2, b1);

            settings.log("map_submapSize", "[%s, %s)", from, to);
            prolog.assertQuery(expected.subMap(from, to).size(), "map_submapSize", actual, from, to, PrologScript.V);
        }
    }

    @Override
    protected MapTest<?> test(final Settings settings) {
        return new ReplaceTreeTest(settings);
    }

    public static void main(final String... args) {
        new PrologTreeSubmapTest(hasUpdates(args, PrologTreeSubmapTest.class)).test();
    }
}

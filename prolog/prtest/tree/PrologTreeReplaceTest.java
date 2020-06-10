package prtest.tree;

import prtest.Value;
import prtest.map.Settings;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologTreeReplaceTest extends PrologTreeTest {
    public PrologTreeReplaceTest(final boolean updates) {
        super(updates);
    }

    class ReplaceTreeTest extends TreeTest {
        public ReplaceTreeTest(final Settings settings) {
            super(settings);
            actions(
                    () -> existingKey(this::replace),
                    () -> uniqueKey(this::replace),
                    () -> removedKey(this::replace)
            );
        }

        private void replace(final int key) {
            final Value value = randomValue();
            expected.replace(key, value);
            update("map_replace", key, value);
        }
    }

    @Override
    protected MapTest<?> test(final Settings settings) {
        return new ReplaceTreeTest(settings);
    }

    public static void main(final String... args) {
        new PrologTreeReplaceTest(hasUpdates(args, PrologTreeReplaceTest.class)).test();
    }
}

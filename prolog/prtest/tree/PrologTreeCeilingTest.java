package prtest.tree;

import prtest.PrologScript;
import prtest.map.Settings;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologTreeCeilingTest extends PrologTreeTest {
    public PrologTreeCeilingTest(final boolean updates) {
        super(updates);
    }

    class ReplaceTreeTest extends TreeTest {
        public ReplaceTreeTest(final Settings settings) {
            super(settings);
            actions.clear();
            actions(() -> existingKey(this::ceiling), () -> uniqueKey(this::ceiling));
            if (updates) {
                actions(this::putExisting, this::putMissing);
            }
        }

        protected void ceiling(final int key) {
            settings.log("map_ceilingKey", "%s", key);
            prolog.assertQuery(expected.ceilingKey(key), "map_ceilingKey", actual, key, PrologScript.V);
        }
    }

    @Override
    protected MapTest<?> test(final Settings settings) {
        return new ReplaceTreeTest(settings);
    }

    public static void main(final String... args) {
        new PrologTreeCeilingTest(hasUpdates(args, PrologTreeCeilingTest.class)).test();
    }
}

package prtest.tree;

import prtest.PrologScript;
import prtest.map.Settings;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologTreeFloorTest extends PrologTreeTest {
    public PrologTreeFloorTest(final boolean updates) {
        super(updates);
    }

    class ReplaceTreeTest extends TreeTest {
        public ReplaceTreeTest(final Settings settings) {
            super(settings);
            actions.clear();

            actions(() -> existingKey(this::floor), () -> uniqueKey(this::floor));
            if (updates) {
                actions(this::putExisting, this::putMissing);
            }
        }

        protected void floor(final int key) {
            settings.log("map_floorKey", "%s", key);
            prolog.assertQuery(expected.floorKey(key), "map_floorKey", actual, key, PrologScript.V);
        }
    }

    @Override
    protected MapTest<?> test(final Settings settings) {
        return new ReplaceTreeTest(settings);
    }

    public static void main(final String... args) {
        new PrologTreeFloorTest(hasUpdates(args, PrologTreeFloorTest.class)).test();
    }
}

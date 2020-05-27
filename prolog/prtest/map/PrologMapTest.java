package prtest.map;

import base.Randomized;
import base.TestCounter;
import prtest.PrologScript;
import prtest.Value;

import java.util.*;
import java.util.function.BooleanSupplier;
import java.util.function.IntConsumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public abstract class PrologMapTest extends Randomized {
    protected final PrologScript prolog;
    protected final boolean updates;
    private final TestCounter counter = new TestCounter();

    public PrologMapTest(final boolean updates, final String file) {
        prolog = new PrologScript(file);
        this.updates = updates;
    }

    protected void test() {
        run();
        counter.printStatus(getClass(), toString());
    }

    @Override
    public String toString() {
        return super.getClass().getSimpleName() + "(" + updates + ")";
    }

    private void run() {
        for (int i = 0; i < 10; i++) {
            runTest(new Settings(i, 10, 10, true));
        }
        runTest(new Settings(100, 10000, 100, false));
        runTest(new Settings(200, 10000, 0, false));
    }

    private void runTest(final Settings settings) {
        test(settings).run();
    }

    protected abstract MapTest<?> test(Settings settings);

    protected static int mode(final String[] args, final Class<?> type, final String... modes) {
        if (args.length == 0) {
            System.err.println("No arguments found");
        } else if (args.length > 1) {
            System.err.println("Only one argument expected, " + args.length + " found");
        } else if (!Arrays.asList(modes).contains(args[0])) {
            System.err.println("First argument should be one of: \"" + String.join("\", \"", modes) + "\", found: \"" + args[0] + "\"");
        } else {
            return Arrays.asList(modes).indexOf(args[0]);
        }
        System.err.println("Usage: java -ea " + type.getName() + " {" + String.join("|", modes) + "}");
        System.exit(0);
        return -1;
    }

    protected abstract class MapTest<T> {
        protected final Settings settings;
        protected final T expected;
        protected final NavigableSet<Integer> existingKeys = new TreeSet<>();
        final Set<Integer> removedKeys = new TreeSet<>();
        final List<Integer> testKeys = new ArrayList<>();
        protected Value actual;
        protected final List<BooleanSupplier> actions = new ArrayList<>();

        public MapTest(final Settings settings, final T expected) {
            this.settings = settings;
            this.expected = expected;
            if (updates) {
                actions(this::putExisting, this::putMissing, this::removeExisting, this::removeMissing);
            }
        }

        protected void actions(final BooleanSupplier... actions) {
            this.actions.addAll(List.of(actions));
        }

        public void run() {
            settings.start();

            final List<Entry> entries = new ArrayList<>();
            for (int i = 0; i < settings.size; i++) {
                final Entry entry = new Entry(uniqueKey(), randomValue());
                entries.add(entry);
                existingKeys.add(entry.getKey());
            }

            entries.sort(Comparator.comparing(Entry::getKey));

            settings.log("build", "%s", entries);
            actual = build(entries);

            testKeys.addAll(existingKeys);
            for (int i = 0; i < 10; i++) {
                uniqueKey(testKeys::add);
            }

            check();

            if (!actions.isEmpty()) {
                for (int i = 0; i < settings.updates; i++) {
                    settings.tick(i);
                    while (!randomItem(actions).getAsBoolean()) {
                        // Empty
                    }
                    check();
                }
            }
        }

        protected abstract Value build(List<Entry> entries);

        protected boolean putExisting() {
            return existingKey(this::put);
        }

        protected boolean putMissing() {
            return uniqueKey(this::put);
        }

        private void put(final int key) {
            final Value value = randomValue();
            update("map_put", key, value);

            existingKeys.add(key);
            testKeys.add(key);
            removedKeys.remove(key);
            putImpl(key, value);
        }

        protected void get(final Value expected, final int key) {
            prolog.assertQuery(expected, "map_get", actual, key, PrologScript.V);
        }

        protected abstract void putImpl(final int key, final Value value);

        protected boolean removeExisting() {
            return existingKey(this::remove);
        }

        protected boolean removeMissing() {
            return uniqueKey(this::remove);
        }

        private void remove(final int key) {
            existingKeys.remove(key);
            removedKeys.add(key);
            update("map_remove", key);
            removeImpl(key);
        }

        protected abstract void removeImpl(int key);

        protected boolean existingKey(final IntConsumer consumer) {
            return random(existingKeys, consumer);
        }

        protected boolean removedKey(final IntConsumer consumer) {
            return random(removedKeys, consumer);
        }

        private boolean random(final Set<Integer> keys, final IntConsumer consumer) {
            if (keys.isEmpty()) {
                return false;
            }

            consumer.accept(random(keys));
            return true;
        }

        protected Integer random(final Set<Integer> ints) {
            return ints.stream()
                    .skip(randomInt(0, ints.size()))
                    .findFirst().orElseThrow();
        }

        protected Value randomValue() {
            return Value.string(randomString(ENGLISH));
        }

        private int randomKey() {
            return randomInt(-settings.range, settings.range);
        }

        protected int uniqueKey() {
            while (true) {
                final int key = randomKey();
                if (!existingKeys.contains(key)) {
                    return key;
                }
            }
        }

        protected boolean uniqueKey(final IntConsumer consumer) {
            consumer.accept(uniqueKey());
            return true;
        }

        protected void check() {
            counter.nextTest();
            settings.log("check", "%s", expected);
//            settings.log("", "%s", actual);
            Collections.shuffle(testKeys, random);
            check(testKeys);
            counter.passed();
        }

        protected abstract void check(List<Integer> testKeys);

        protected void update(final String name, final Object... args) {
            settings.log(name, "(map, %s, V)", Stream.of(args).map(Object::toString).collect(Collectors.joining(", ")));
            final Object[] fullArgs = new Object[args.length + 2];
            fullArgs[0] = actual;
            System.arraycopy(args, 0, fullArgs, 1, args.length);
            fullArgs[args.length + 1] = PrologScript.V;
            actual = prolog.solveOne(name, fullArgs);
        }
    }
}

package prtest;

import alice.tuprolog.Int;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;
import alice.tuprolog.Theory;

import java.util.List;
import java.util.ListIterator;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologUtil {
    private static final Term[] NO_TERMS = new Term[0];
    private static final Struct EMPTY_LIST = PrologUtil.pure("[]");

    private static long benchmark = 0;

    /** Utility class. */
    private PrologUtil() {}

    public static Struct pure(final String functor) {
        return Struct.of(functor, NO_TERMS);
    }

    public static Struct list(final List<? extends Term> items) {
        Struct current = EMPTY_LIST;
        for (final ListIterator<? extends Term> i = items.listIterator(items.size()); i.hasPrevious(); ) {
            current = Struct.of(".", new Term[]{i.previous(), current});
        }
        return current;
    }

    public static long measure(final String description, final Runnable action) {
        final long start = System.currentTimeMillis();
        action.run();
        final long time = System.currentTimeMillis() - start;
        System.out.printf("%s done in %dms%n", description, time);
        return time;
    }

    @SuppressWarnings("deprecation")
    public static long benchmark() {
        if (benchmark == 0) {
            final PrologScript prolog = new PrologScript();
            prolog.addTheory(new Theory(
                    "fib_benchmark(N, R) :- fib_benchmark_table(N, R), !.\n" +
                            "fib_benchmark(1, 1).\n" +
                            "fib_benchmark(2, 1).\n" +
                            "fib_benchmark(N, R) :-\n" +
                            "    N > 0,\n" +
                            "    N1 is N - 1, fib_benchmark(N1, R1),\n" +
                            "    N2 is N - 2, fib_benchmark(N2, R2),\n" +
                            "    R is mod(R1 + R2, 100000000),\n" +
                            "    assertz(fib_benchmark_table(N, R)).\n"
            ));
            benchmark = PrologUtil.measure("Benchmark", () -> prolog.call("fib_benchmark", Int.of(2_000)));
        }
        return benchmark;
    }
}

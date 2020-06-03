package prtest.primes;

import java.util.LinkedHashSet;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologUniqueTest extends PrologPrimesTest {
    public PrologUniqueTest(final int max) {
        super(max);
    }

    @Override
    public void runTests() {
        super.runTests();
        checkDivisors("unique_prime_divisors", false, LinkedHashSet::new);
    }

    public static void main(final String... args) {
        new PrologUniqueTest(primeMode(args)).run();
    }
}

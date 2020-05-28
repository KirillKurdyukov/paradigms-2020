package prtest.primes;

import prtest.PrologScript;
import prtest.PrologUtil;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologNthTest extends PrologPrimesTest {
    public PrologNthTest(final int max) {
        super(max);
    }

    @Override
    public void runTests() {
        super.runTests();
        PrologUtil.measure("checkNth", this::checkNth);
    }

    private void checkNth() {
        for (int i = 0; primes[i] * primes[i] < max * 10; i += 10) {
            prolog.assertQuery(primes[i], "nth_prime", i + 1, PrologScript.V);
        }
    }

    public static void main(final String... args) {
        new PrologNthTest(primeMode(args)).run();
    }
}

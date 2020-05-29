package prtest.primes;

import prtest.PrologScript;
import prtest.PrologUtil;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologLcmTest extends PrologPrimesTest {
    public PrologLcmTest(final int max) {
        super(max);
    }

    @Override
    public void runTests() {
        super.runTests();
        PrologUtil.measure("checkLcm", this::checkLcm);
    }

    private void checkLcm() {
        for (int a = 1; a < 10; a++) {
            for (int b = 1; b < 10; b++) {
                checkLcm(a, b);
            }
        }
        for (int i = 0; i < 1000; i++) {
            checkLcm(randomN(), randomN());
        }
    }

    private void checkLcm(final int a, final int b) {
        prolog.assertQuery(lcm(a, b), "lcm", a, b, PrologScript.V);
    }

    private static long lcm(final int a, final int b) {
        return a * (long) b / gcd(Math.min(a, b), Math.max(a, b));
    }

    private static int gcd(final int a, final int b) {
        return a == 0 ? b : gcd(b % a, a);
    }

    public static void main(final String... args) {
        new PrologLcmTest(primeMode(args)).run();
    }
}

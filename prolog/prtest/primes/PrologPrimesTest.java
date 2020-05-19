package prtest.primes;

import alice.tuprolog.Int;
import alice.tuprolog.Struct;
import base.Asserts;
import expression.BaseTest;
import prtest.PrologScript;
import prtest.PrologUtil;

import java.util.*;
import java.util.function.Supplier;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologPrimesTest extends BaseTest {
    final int max;
    final int[] primes;
    final BitSet isPrime = new BitSet();
    public static String SUFFIX = ".pl";

    protected final PrologScript prolog;

    public PrologPrimesTest(final int max) {
        this.max = max;
        prolog = new PrologScript("primes" + SUFFIX);
        primes = primes(max);
        Arrays.stream(primes).forEach(isPrime::set);
    }

    private static int[] primes(final int max) {
        final int[] primes = new int[(int) (2 * max / Math.log(max))];
        primes[0] = 2;
        int t = 1;
        int h = 0;
        for (int i = 3; i <= max; i += 2) {
            primes[t++] = i;
            if (primes[h] * primes[h] <= i) {
                h++;
            }
            for (int j = 0; j < h; j++) {
                if (i % primes[j] == 0) {
                    t--;
                    break;
                }
            }
        }
        return Arrays.copyOf(primes, t);
    }

    public void test() {
        final long limit = PrologUtil.benchmark() * 20;
        final long time = PrologUtil.measure("Tests", this::runTests);
        Asserts.assertTrue(String.format("Time limit exceeded: %sms instead of %sms", time, limit), time <= limit);
    }

    public void runTests() {
        PrologUtil.measure("Init", () -> System.out.println("Init: " + prolog.test(Struct.of("init", Int.of(max)))));
        PrologUtil.measure("checkPrimes", this::checkPrimes);
        PrologUtil.measure("checkComposites", this::checkComposites);

        checkDivisors("prime_divisors", ArrayList::new);
    }

    void checkDivisors(final String function, final Supplier<Collection<Int>> factory) {
        PrologUtil.measure(function, () -> {
            for (int i = 1; i < 10; i++) {
                checkDivisors(function, i, factory);
            }
            checkDivisors(function, 255, factory);
            checkDivisors(function, 256, factory);

            for (int i = 0; i < primes.length / 10; i++) {
                checkDivisors(function, randomN(), factory);
            }
        });
    }

    int randomN() {
        return randomInt(max - 1) + 1;
    }

    private void checkComposites() {
        for (int i = 0; i < primes.length; i++) {
            checkPrime(randomN());
        }
    }

    private void checkPrimes() {
        for (final int prime : primes) {
            checkPrime(prime);
        }
    }

    private void checkPrime(final int value) {
        counter.nextTest();
        prolog.assertResult(isPrime.get(value), "prime", Int.of(value));
        prolog.assertResult(!isPrime.get(value), "composite", Int.of(value));
        counter.passed();
    }

    private void checkDivisors(final String function, final int n, final Supplier<Collection<Int>> factory) {
        counter.nextTest();
        prolog.assertCall(PrologUtil.list(divisors(n, factory.get())), function, Int.of(n));
        counter.passed();
    }

    private List<Int> divisors(final int n, final Collection<Int> divisors) {
        int value = n;
        for (final int prime : primes) {
            if (prime * prime > n) {
                break;
            }
            while (value % prime == 0) {
                divisors.add(Int.of(prime));
                value /= prime;
            }
        }
        if (value > 1) {
            divisors.add(Int.of(value));
        }
        return List.copyOf(divisors);
    }

    public static void main(final String... args) {
        new PrologPrimesTest(primeMode(args)).run();
    }

    protected static int primeMode(final String[] args) {
        return (int) (1000 * Math.pow(100, BaseTest.mode(args, "easy", "hard", "bonus")));
    }
}

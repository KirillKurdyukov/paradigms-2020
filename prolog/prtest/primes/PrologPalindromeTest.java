package prtest.primes;

import prtest.PrologUtil;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologPalindromeTest extends PrologPrimesTest {
    public PrologPalindromeTest(final int max) {
        super(max);
    }

    @Override
    public void runTests() {
        super.runTests();
        PrologUtil.measure("checkPalindrome", this::checkPalindrome);
    }

    private void checkPalindrome() {
        for (int radix = 2; radix <= 10; radix++){
            for (int i = 1; i < 10; i++) {
                checkPalindrome(i, radix);
            }
            for (final int prime : primes) {
                if (palindrome(prime, radix)) {
                    checkPalindrome(prime, radix);
                }
            }
            for (int i = 0; i < 1000; i++) {
                checkPalindrome(randomN(), radix);
            }
        }
    }

    private void checkPalindrome(final int n, final int radix) {
        prolog.assertResult(isPrime.get(n) && palindrome(n, radix), "prime_palindrome", n, radix);
    }

    private static boolean palindrome(final int n, final int radix) {
        int reversed = 0;
        for (int value = n; value > 0; value /= radix) {
            reversed = reversed * radix + value % radix;
        }
        return n == reversed;
    }

    public static void main(final String... args) {
        new PrologPalindromeTest(primeMode(args)).run();
    }
}

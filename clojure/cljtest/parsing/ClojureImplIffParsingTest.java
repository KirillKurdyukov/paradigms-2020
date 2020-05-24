package cljtest.parsing;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ClojureImplIffParsingTest extends ClojureBitwiseParsingTest {
    static {
        PARSED.rename("=>", "Impl");
        PARSED.rename("<=>", "Iff");
    }

    public ClojureImplIffParsingTest(final ImplIffTests tests, final boolean hard) {
        super(tests, hard);
        priorities.put("=>", -9);
        priorities.put("<=>", 5);
    }

    public static void main(final String... args) {
        new ClojureImplIffParsingTest(new ImplIffTests(), mode(args, ClojureImplIffParsingTest.class)).run();
    }

    public static class ImplIffTests extends BitwiseTests {{
        binary("=>", logic((a, b) -> ~a | b));
        binary("<=>", logic((a, b) -> ~(a ^ b)));
        tests(
                f("=>", vx, vy),
                f("=>", vx, f("=>", vy, vz)),
                f("=>", f("=>", vx, vy), vz),
                f("<=>", vx, vy),
                f("<=>", vx, f("<=>", vy, vz)),
                f("<=>", f("<=>", vx, vy), vz)
        );
    }}
}

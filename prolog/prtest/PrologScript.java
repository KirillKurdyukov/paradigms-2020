package prtest;

import alice.tuprolog.*;
import alice.tuprolog.exceptions.NoMoreSolutionException;
import alice.tuprolog.exceptions.NoSolutionException;
import base.Asserts;

import java.nio.file.Path;
import java.util.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class PrologScript {
    public static final Var V = Var.of("V");
    public static Path PROLOG_ROOT = Path.of(".");

    private final Prolog prolog = new Prolog();

    public PrologScript() {
        prolog.addExceptionListener(exceptionEvent -> {throw new PrologException(exceptionEvent.getMsg());});
        prolog.addOutputListener(event -> System.out.print(event.getMsg()));
    }

    public PrologScript(final String file) {
        this();
        consult(file);
    }

    private static PrologException error(final Throwable cause, final String format, final Object... arguments) {
        return new PrologException(String.format(format, arguments), cause);
    }

    public void consult(final String file) {
        final String path = PROLOG_ROOT.resolve(file).toString();
        try {
            if (!prolog.solve(Struct.of("consult", PrologUtil.pure(path))).isSuccess()) {
                throw error(null, "Error opening '%s'", path);
            }
        } catch (final PrologException e) {
            throw error(e, "Error opening '%s': %s", path, e.getMessage());
        }
    }

    public boolean test(final Term term) {
        return prolog.solve(term).isSuccess();
    }

    private List<Term> solve(final Var var, final Term term) {
        SolveInfo info = prolog.solve(term);
        final List<Term> values = new ArrayList<>();
        try {
            while (info.isSuccess()) {
                values.add(info.getVarValue(var.getName()));
                if (!info.hasOpenAlternatives()) {
                    return values;
                }
                info = prolog.solveNext();
            }
            return values;
        } catch (final NoSolutionException | NoMoreSolutionException e) {
            throw new AssertionError(e);
        }
    }

    public void solveNone(final Var var, final Struct term) {
        final List<Term> values = solve(var, term);
        if (!values.isEmpty()) {
            throw Asserts.error("No solutions expected for %s in %s%n  found: %d %s", var, term, values.size(), values);
        }
    }

    public Term solveOne(final Var var, final Term term) {
        final List<Term> values = solve(var, term);
        if (values.size() != 1) {
            throw Asserts.error("Exactly one solution expected for %s in %s%n  found: %d %s", var, term, values.size(), values);
        }
        return values.get(0);
    }

    private static Struct query(final String name, final Term[] args) {
        final Term[] fullArgs = Arrays.copyOf(args, args.length + 1);
        fullArgs[args.length] = PrologScript.V;
        return Struct.of(name, fullArgs);
    }

    public void assertCall(final Object value, final String name, final Term... args) {
        if (value != null) {
            if (!Objects.equals(value, call(name, args))) {
                throw Asserts.error("%s:%n  expected `%s`,%n    actual `%s`", query(name, args), value, call(name, args));
            }
        } else {
            solveNone(PrologScript.V, query(name, args));
        }
    }

    public Term call(final String name, final Term... args) {
        return solveOne(V, query(name, args));
    }

    public void addTheory(final Theory theory) {
        prolog.addTheory(theory);
    }

    public void assertResult(final boolean expected, final String name, final Term... args) {
        final Struct struct = Struct.of(name, args);
        Asserts.assertEquals(struct.toString(), expected, test(struct));
    }
}

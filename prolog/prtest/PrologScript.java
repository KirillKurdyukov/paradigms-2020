package prtest;

import alice.tuprolog.*;
import alice.tuprolog.exceptions.NoMoreSolutionException;
import alice.tuprolog.exceptions.NoSolutionException;
import base.Asserts;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

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
            if (!test("consult", Value.string(path))) {
                throw error(null, "Error opening '%s'", path);
            }
        } catch (final PrologException e) {
            throw error(e, "Error opening '%s': %s", path, e.getMessage());
        }
    }

    public boolean test(final String rule, final Object... args) {
        return prolog.solve(Value.struct(rule, args).toTerm()).isSuccess();
    }

    private List<Term> solve(final Term term) {
        SolveInfo info = prolog.solve(term);
        final List<Term> values = new ArrayList<>();
        try {
            while (info.isSuccess()) {
                values.add(info.getVarValue(V.getName()));
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

    public void solveNone(final String rule, final Object... args) {
        final Term term = Value.struct(rule, args).toTerm();
        final List<Term> values = solve(term);
        if (!values.isEmpty()) {
            throw Asserts.error("No solutions expected for %s in %s%n  found: %d %s", V, term, values.size(), values);
        }
    }

    public Value solveOne(final String rule, final Object... args) {
        final Term term = Value.struct(rule, args).toTerm();
        final List<Term> values = solve(term);
        if (values.size() != 1) {
            throw Asserts.error("Exactly one solution expected for %s in %s%n  found: %d %s", V, term, values.size(), values);
        }
        return Value.term(values.get(0));
    }

    public void addTheory(final Theory theory) {
        prolog.addTheory(theory);
    }

    public void assertResult(final boolean expected, final String rule, final Object... args) {
        Asserts.assertEquals(Value.struct(rule, args).toString(), expected, test(rule, args));
    }

    public void assertQuery(final Object expected, final String rule, final Object... args) {
        if (expected != null) {
            final Term converted = Value.convert(expected).toTerm();
            final Term actual = solveOne(rule, args).toTerm();
            if (!converted.equals(actual)) {
                throw Asserts.error("%s:%n  expected `%s`,%n    actual `%s`", Value.struct(rule, args), converted, actual);
            }
        } else {
            solveNone(rule, args);
        }
    }
}

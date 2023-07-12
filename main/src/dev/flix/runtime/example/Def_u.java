package dev.flix.runtime.example;

import dev.flix.runtime.*;

public class Def_u {

    // def u(): Int32 \ Con =
    //     let name = v();
    //     let greetings = "Hello ${name}";
    //     do Con.print(greetings);
    //     String.length("${name}")

    public static Result apply(Locals_u fd) {

        // Our first task is to restore the program counter and the local variables.
        // We restore all local variables, but of course not every variable may have been initialized.
        // A smarter implementation would only restore local variables which (1) have been initialized and (2) are used.
        int pc = fd.pc;
        String name = fd.name;
        String greetings = fd.greetings;

        // We should think if the pc as pointing to one of N labels, e.g. label_i.
        // In bytecode, we simply jump to this label.
        // But in Java code we instead use an infinite loop and a switch to illustrate the same effect.

        // Q: Where do the entries in the table come from?
        // Answer: They come from each LetVal etc.

        // TODO: Q: How do we handle if-then-else and branch/jumpto? Not sure.

        jump:
        while (true) {
            switch (pc) {
                case 0:
                    // We begin by calling `v`. This can result in one of three outcomes:
                    // A Done value, a thunk, or a suspension.
                    // If we get a thunk, we continue to evaluate it until we are left with either a value or suspension.

                    Result vResult = Def_v.apply();

                    // -- below can be put into an unwind function on Result.
                    while (vResult instanceof Thunk) { // aka. "ForceTailCall".
                        vResult = ((Thunk) vResult).apply();
                    }
                    // --

                    // Invariant: We know that vResult must now be Done(v) or a Suspension.

                    if (vResult instanceof Done) {
                        name = (String) (((Done) vResult).result);
                        pc = 1;
                        continue jump;
                    } else if (vResult instanceof Suspension) {
                        // Build frame, and then return new suspension.
                        Suspension s = (Suspension) vResult;
                        var t = new Thunk_u(new Locals_u(1, name, greetings));
                        return new Suspension(s.effSym, s.effOp, s.effArg, s.prefix.push(t), s.resumption);
                    } else { /* impossible: we have already dealt with all the thunks. */ }

                    break;

                case 1:
                    greetings = "Hello " + name;
                    var prefix0 = new FramesNil();
                    var prefix = prefix0.push(new Thunk_u(new Locals_u(2, name, greetings)));
                    return new Suspension("Con", "print", greetings, prefix, new ResumptionNil());

                case 2:
                    return Done.mkInt32(name.length());
            }
        }

    }
}

/**
 * A thunk for `def u`. Simply holds a reference to the locals and implements `Thunk`.
 */
class Thunk_u implements Thunk {
    Locals_u locals;

    public Thunk_u(Locals_u locals) {
        this.locals = locals;
    }

    public Result apply() {
        return Def_u.apply(locals);
    }
}

/**
 * An object which holds the PC + local variables of `def u`.
 */
class Locals_u { // Aka. "FrameData".
    public final int pc;

    public final String name;
    public final String greetings;

    public Locals_u(int pc, String name, String greetings) {
        this.pc = pc;
        this.name = name;
        this.greetings = greetings;
    }
}

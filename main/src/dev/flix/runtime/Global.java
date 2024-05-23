package dev.flix.runtime;

import scala.Int;


/**
 * OBS: A interface-like copy of the class generated in
 * `main/src/ca/uwaterloo/flix/language/phase/jvm/GenGlobalClass.scala` with
 * exception bodies that allows the compiler to load the imports in i.e. channel
 * and check for types (the code here is never run).
 */
public final class Global {

    public static final long newId() {
        throw new RuntimeException("Global.newId should not be called on the mock class");
    }

    public static final String[] getArgs() {
        throw new RuntimeException("Global.getArgs should not be called on the mock class");
    }

    public static final void setArgs(String[] var0) {
        throw new RuntimeException("Global.setArgs should not be called on the mock class");
    }

    public static final long decAndCheck() {
        throw new RuntimeException("Global.deckAndCheck should not be called on the mock class");
    }

    public static final long setStepCounterField() {
        throw new RuntimeException("Global.setStepCounterField should not be called on the mock class");
    }

    public static final void throwEquivalentMutant() {
        throw new RuntimeException("Global.throwEquivalentMutant should not be called on the mock class");
    }

}

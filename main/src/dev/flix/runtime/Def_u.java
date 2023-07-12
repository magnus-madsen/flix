package dev.flix.runtime;

public class Def_u implements Result {

    //    def u(): Int32 \ Con =
    //      let name = v();
    //      let greetings = "Hello ${name}";
    //      do Con.print(greetings);
    //       String.length("${name}")

    public Result apply(FrameData_u fd) {

        int pc = fd.pc;
        String name = fd.name;
        String greetings = fd.greetings;
        // actually the PC is mapped to a switch that jumps to the appropriate label,
        // i.e. we have a mapping from PCs to labels.
        // The PCs and labels come from Stmt.LetVal... (?)
        jump: while(true) {
            switch (pc) {
                case 0:
                    Def_v v = new Def_v();
                    //v.arg = Unit

                    Result vResult = v.apply();
                    // -- below can be put into Unwind which returns only Null + Suspend
                    // "forceTrampoline" -- to reduce down to either Done or Suspend.
                    if (vResult instanceof Thunk) {
                        while (vResult instanceof Thunk) {
                            vResult = ((Thunk) vResult).apply();
                        }
                    }
                    // --

                    // Now vResult cannot be Thunk, it must be Done or Suspend.

                    if (vResult instanceof Done) {
                        name = (String) (((Done) vResult).result);
                        pc = 1;
                        continue jump;
                    } else if (vResult instanceof Suspend) {
                        // Build frame, and then return new suspension.
                        Suspend s = (Suspend) vResult;
                        var t = new Def_u_Frame( new FrameData_u(1, name, greetings));
                        return new Suspend(s.effName, s.op, s.opArgument, s.prefix.push(t), s.resumption);
                    } else { /* impossible: we have already dealt with all the thunks. */ }

                    break;

                case 1:
                    greetings = "Hello " + name;
                    var prefix0 = new FramesNil();
                    var prefix = prefix0.push(new Def_u_Frame( new FrameData_u(2, name, greetings)));
                    return new Suspend("Con", "print", greetings, prefix, new ResumptionNil());

                case 2:
                    return Done.mkInt32(name.length());
            }
        }

    }
}

class Def_u_Frame implements Thunk {
    FrameData_u frameData;
    Def_u defu = new Def_u();

    public Def_u_Frame(FrameData_u frameData) {
        this.frameData = frameData;
    }

    public Result apply() {
        return defu.apply(frameData);
    }
}

interface Frames {
    default Frames push(Thunk t) {
        return new FramesCons(t, this);
    }
}
class FramesNil implements Frames {}

record FramesCons(Thunk head, Frames tail) implements Frames {}

interface Resumption {}
class ResumptionNil implements Resumption {}
class ResumptionCons implements Resumption {
    final String effName;
    final Frames frames;
    final Resumption tail;

    public ResumptionCons(String effName, Frames frames, Resumption tail) {
        this.effName = effName;
        this.frames = frames;
        this.tail = tail;
    }
}

class Suspend implements Result {
    final String effName;
    final String op;
    final Object opArgument;


    final Frames prefix;
    final Resumption resumption;

    public Suspend(String effName, String op, Object opArgument, Frames prefix, Resumption resumption) {
        this.effName = effName;
        this.op = op;
        this.opArgument = opArgument;
        this.prefix = prefix;
        this.resumption = resumption;
    }
}

class FrameData_u {
    public int pc;

    public String name;
    public String greetings;

    public FrameData_u(int pc, String name, String greetings) {
        this.pc = pc;
        this.name = name;
        this.greetings = greetings;
    }
}

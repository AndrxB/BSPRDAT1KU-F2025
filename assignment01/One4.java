package assignment01;

import java.util.Map;

abstract class Aexpr {
    abstract public int eval(Map<String,Integer> env);
    abstract public Aexpr simplify();
    @Override abstract public String toString();
    @Override abstract public boolean equals(Object o);
}

class CstI extends Aexpr {
    final int i;
    CstI(int i) { this.i = i; }
    public int eval(Map<String,Integer> env) { return i; }
    public Aexpr simplify() { return this; }
    @Override public String toString() { return Integer.toString(i); }
    @Override public boolean equals(Object o) { return o.getClass() == getClass() && i == ((CstI)o).i; }
}

class Var extends Aexpr {
    final String x;
    Var(String x) { this.x = x; }
    public int eval(Map<String,Integer> env) { return env.get(x); }
    public Aexpr simplify() { return this; }
    @Override public String toString() { return x; }
    @Override public boolean equals(Object o) { return o.getClass() == getClass() && x.equals(((Var)o).x); }
}

class Add extends Aexpr {
    final Aexpr e1, e2;
    Add(Aexpr e1, Aexpr e2) { this.e1 = e1; this.e2 = e2; }
    public int eval(Map<String,Integer> env){ return e1.eval(env) + e2.eval(env); }
    public Aexpr simplify() {
        if(e1.equals(new CstI(0))) return e2.simplify();
        else if (e2.equals(new CstI(0))) return e1.simplify();
        else return this;
    }
    @Override public String toString() { return "(" + e1 + " + " + e2 + ")"; }
    @Override public boolean equals(Object o) {
        return o.getClass() == getClass()
            && ((Add)o).e1.equals(e1)
            && ((Add)o).e2.equals(e2);
    }
}

class Mul extends Aexpr {
    final Aexpr e1, e2;
    Mul(Aexpr e1, Aexpr e2) { this.e1 = e1; this.e2 = e2; }
    public int eval(Map<String,Integer> env){ return e1.eval(env) * e2.eval(env); }
    public Aexpr simplify() {
        if(e1.equals(new CstI(1))) return e2.simplify();
        else if(e2.equals(new CstI(1))) return e1.simplify();
        else if(e1.equals(new CstI(0)) || (e2.equals(new CstI(0))))
            return new CstI(0).simplify();
        else return this;
    }
    @Override public String toString() { return "(" + e1 + " * " + e2 + ")"; }
    @Override public boolean equals(Object o) {
        return o.getClass() == getClass()
            && ((Mul)o).e1.equals(e1)
            && ((Mul)o).e2.equals(e2);
    }
}

class Sub extends Aexpr {
    final Aexpr e1, e2;
    Sub(Aexpr e1, Aexpr e2) { this.e1 = e1; this.e2 = e2; }
    public int eval(Map<String,Integer> env){ return e1.eval(env) - e2.eval(env); }
    public Aexpr simplify() {
        if(e2.equals(new CstI(0))) return e1.simplify();
        else if (e1.equals(e2))
            return new CstI(0).simplify();
        else return this;
    }
    @Override public String toString() { return "(" + e1 + " - " + e2 + ")"; }
    @Override public boolean equals(Object o) {
        return o.getClass() == getClass()
            && ((Sub)o).e1.equals(e1)
            && ((Sub)o).e2.equals(e2);
    }
}

public class One4 {
    public static void main(String[] args) {
        Aexpr e = new Add(new CstI(17), new Var("z"));
        System.out.println(e.toString());
        var e1 = new Sub(new Sub(new CstI(420), new CstI(420)), new Mul(new Var("Hello!"), new CstI(17)));
        var e2 = new Sub(new Var("Chuck"), new Var("Norris"));
        var e3 = new Mul(new Add(e2, e1), new Var("nice"));
        System.out.println(e1.toString());
        System.out.println(e2.toString());
        System.out.println(e3.toString());
    }
}
package com.xadecimal.procedural;

import java.lang.Throwable;

public class ExReturn extends Throwable {

    public Object o; // 0
    public long l; // 1
    public double d; // 2
    public boolean b; // 3
    // nil 4

    public int type;

    public ExReturn(Object o) {
        super(null, null, false, false);
        this.type = 0;
        this.o = o;
    }

    public ExReturn(long l) {
        super(null, null, false, false);
        this.type = 1;
        this.l = l;
    }

    public ExReturn(double d) {
        super(null, null, false, false);
        this.type = 2;
        this.d = d;
    }

    public ExReturn(boolean b) {
        super(null, null, false, false);
        this.type = 3;
        this.b = b;
    }

    public ExReturn() {
        super(null, null, false, false);
        this.type = 4;
    }
}

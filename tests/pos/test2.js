function foo(n, y) {
    assume(n>=0);
    var x = 0;
    var z = 0;
    var b = 0;
    while (x <= n-1) {
        //pred(x>= 0);
        //pred(x>=0);
        x = x + 1;
        b = b + 2;
        if (y>=1) {
            z = z + x;
            //pred(z >= 0);
        }
        if (y<=0) {
            z = z + b;
            //pred(z >= 0);
        }
    }
    assert(z >= 0);
}

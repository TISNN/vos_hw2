function foo(n, y) {
    assume(n>=0);
    assume(y>=0);
    var x = 0;
    var z = 0;
    while (x <= n-1) {
        //pred(x>=0);
        //pred(y>=0);
        //pred(x <= n-1);
        x = x + 1;
        if (x+y>=0) {
            //pred(z>=0);
            //pred(z>=x);
            z = z + x;
        }
    }
    assert(z >= x);
}

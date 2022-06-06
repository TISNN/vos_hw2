function foo(n){
    assume(n>=0);
    var x = 0;
    var y = n;
    //pred(x >= 0)
    while (x <= n-1){
        // pred(y>=0);
        // pred(y<=0);
        // pred(x <= n);
        // pred(x >= n);
        // pred(n==1);
        y = y - 1;
        x = x + 1;
    }
    assert(y == 0);
    assert(x == n);
}

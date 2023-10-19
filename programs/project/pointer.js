function pointer() {
    var x = 0;

    var *y = &x;

    *y = 5;

    assert(x == 5);
}
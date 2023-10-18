function pointer() {
    var x = 0;
    var y = &x;
    assert(*y = 0);

    *y = 5;

    assert(x == 5);
    var *y = 1;
    x = &y;
    assert(x == 1);

    var z = *y + 1;
}
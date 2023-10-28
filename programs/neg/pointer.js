function pointer() {
    x[0] = 1; // expression
    ptr = 9; // pointer 

    x >>= ptr; // mapping x to pointer ptr


    assert(x==10);
}
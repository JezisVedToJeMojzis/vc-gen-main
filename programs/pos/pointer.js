function pointer() {
    var x = 0; // expression
    var ptr = 0; // pointer 

    x >>= ptr // mapping x to pointer ptr
    ptr <<= 5; // storing value 5 into ptr

    assert(x[5]==5);
}
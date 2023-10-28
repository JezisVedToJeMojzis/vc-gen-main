function pointer() {
    x[5] = 0; // expression
    ptr[5] = 0; // pointer 

    x >>= ptr // mapping x to pointer ptr
    ptr <<= 5; // storing value 5 into ptr

    assert(x[5]==5);
}
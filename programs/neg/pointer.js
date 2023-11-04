function pointer() {
    x >>= ptr[x]; // mapping x to pointer ptr
    
    ptr[x] <<= 5; // storing value 5 into ptr

    assert(x==5);
}
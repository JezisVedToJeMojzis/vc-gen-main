function pointer() {
    x >>= ptr; // mapping x to pointer ptr // x = *ptr
    
    ptr[x] <<= 5; // storing value 5 into ptr // *ptr = 5

    assert(x==5);
}
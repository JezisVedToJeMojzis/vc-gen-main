function pointer() {
    var x = 0;
    // * = ! and ~ = &
    var !ptr = ~x; // reference - y is made to point to the memory location of x

    
    !ptr = 5; // Dereferences the pointer y, and sets the value at the memory location pointed to by y (which is x) to 5 

    assert(x == 5);
}
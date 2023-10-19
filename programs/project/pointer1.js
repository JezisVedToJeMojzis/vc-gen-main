function pointer() {
    var x = 5;       
    var y = 0;          

    var !ptr = ~x;  //Assign the address of x to the pointer ptr

    y = !ptr;  // Use the pointer to access the value of x and assign it to y

    assert(y == 5);
}
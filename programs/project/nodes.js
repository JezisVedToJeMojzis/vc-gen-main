// Node structure
function Node() {
    this.value = null;
    this.next = null;
  }
  
  // Creating nodes
  var node3 = new Node();
  var node2 = new Node();
  var node1 = new Node();

  // Assign values
  node1.value = 1;
  node2.value = 2;
  node3.value = 3;
  
  // Linking nodes
  node1.next = node2;
  node2.next = node3;
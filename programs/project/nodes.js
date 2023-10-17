// Node structure
function Node(value) {
    this.value = value;
    this.next = null;
  }
  
  // Creating nodes
  var node3 = new Node(3);
  var node2 = new Node(2);
  var node1 = new Node(1);
  
  // Linking nodes
  node1.next = node2;
  node2.next = node3;
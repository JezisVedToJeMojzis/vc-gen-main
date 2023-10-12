function Node(data, next) {
    return {
        data: data,
        next: next
    };
}

// Create a simple linked list: 1 -> 2 -> 3
let node3 = Node(3, null);
let node2 = Node(2, node3);
let node1 = Node(1, node2);
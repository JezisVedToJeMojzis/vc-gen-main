function createLinkedList() {
    function Node(data, next) {
        return {
            data: data,
            next: next
        };
    }

    let node3 = Node(3, null);
    let node2 = Node(2, node3);
    let node1 = Node(1, node2);

    return node1; // Returns the head of the linked list
}
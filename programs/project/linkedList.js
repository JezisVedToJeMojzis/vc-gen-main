// basic linked list with 4 nodes
function createLinkedList() {
    return {
        head: {
            value: 1,
            next: {
                value: 2,
                next: {
                    value: 3,
                    next: {
                        value: 4,
                        next: null
                    }
                }
            }
        }
    };
}
-- Methods we need to consider:
-- 1. size() - This method returns the number of nodes present in the linked list.
-- 2. clear() - This method empties out the list.
-- 3. getLast() - This method returns the last node of the linked list.
-- 4. getFirst() - This method returns the first node of the linked list.
-- 5. appendNode(node) - This method will append the node in the linked list.
-- 6. insertAt(index) - This function will insert the node at specified index.
-- 7. removeFrom(index) - This function will remove the node from specified index.
-- 8. getNode(index) - This function will return the node from specified index.
-- 9. isEmpty() - This function will return true if our linked list is empty otherwise return false.

-- insertion into linked list
-- insert :: LinkedList -> Int -> LinkedList
-- insert list val = ...

module LinkedList
  ( LinkedList (..)
  , Node (..)
  , parseLinkedList
  ) where

import Data.Maybe

data Node a = Node
  { value :: a
  , next :: Maybe (Node a)
  }
  deriving (Show)

data LinkedList a = LinkedList
  { head :: Maybe (Node a)
  }
  deriving (Show)



# https://leetcode.com/problems/lru-cache/

import logging

L = logging.getLogger(__name__)
logging.basicConfig(level=logging.INFO)


# Node for doubly-linked list
class Node:
    def __init__(self, key, value, prev=None, next=None):
        self.key = key
        self.value = value
        self.next = next
        self.prev = prev

    def __str__(self):
        return f"kv: {self.key}, {self.value} prev: ({self.prev}) next ({self.next})"


# LRUCache is backed by a hashmap and a doubly lined list
class LRUCache:
    def __init__(self, capacity: int):
        print("\nnew cache size", capacity)
        assert capacity > 0
        self.capacity = capacity
        self.count = 0
        self.cache = {}
        self.lru_head = None
        self.lru_tail = None

    def evict(self):
        assert self.lru_tail is not None
        print(f"evict {self.lru_tail.key}")
        del self.cache[self.lru_tail.key]

        if self.count > 1:
            self.lru_tail.prev.next = None  # drop reference
            self.lru_tail = self.lru_tail.prev
        else:
            self.lru_head = None
            self.lru_tail = None

        self.count -= 1

    def touch(self, node):
        if self.count == 1:
            return

        # Move node to the top
        if not node.next:  # if already at the bottom
            self.lru_tail = node.prev  # update the tail

        if not node.prev:  # if already at the top
            pass  # self.lru_head.prev = node
        else:
            node.prev.next = node.next
            if node.next:
                node.next.prev = node.prev
            node.prev = None
            node.next = self.lru_head
            self.lru_head.prev = node
            self.lru_head = node

    def get(self, key: int) -> int:
        if key not in self.cache:
            print(f"get {key} = -1")
            return -1

        node = self.cache[key]
        self.touch(node)

        print(f"get {key} = {node.value}")
        return node.value

    def put(self, key: int, value: int) -> None:
        print("put", key, value)
        if key in self.cache:
            node = self.cache[key]
            node.value = value
            self.touch(node)
        else:
            if self.count + 1 > self.capacity:
                self.evict()

            node = Node(key, value)
            self.cache[key] = node
            if self.lru_head is None:
                self.lru_head = node
                self.lru_tail = node
            else:
                node.next = self.lru_head
                self.lru_head.prev = node
                self.lru_head = node

            self.count += 1

    def __str__(self):
        return f"head {self.lru_head} \ntail {self.lru_tail}"


# Your LRUCache object will be instantiated and called as such:
# obj = LRUCache(capacity)
# param_1 = obj.get(key)
# obj.put(key,value)


def test1():
    cache = LRUCache(3)
    cache.put(4, "v")
    cache.put(5, "v")
    cache.put(5, "t")
    cache.put(6, "t")
    cache.put(7, "t")


def test2():
    lRUCache = LRUCache(2)
    lRUCache.put(1, 1)
    lRUCache.put(2, 2)
    assert lRUCache.get(1) == 1
    lRUCache.put(3, 3)
    assert lRUCache.get(2) == -1
    lRUCache.put(4, 4)
    assert lRUCache.get(1) == -1
    assert lRUCache.get(3) == 3
    assert lRUCache.get(4) == 4


def test3():
    cache = LRUCache(1)
    cache.put(2, 1)
    assert cache.get(2) == 1
    cache.put(3, 2)
    assert cache.get(2) == -1
    assert cache.get(3) == 2


def test4():
    cache = LRUCache(2)
    cache.put(2, 1)
    cache.put(1, 1)
    cache.put(2, 3)
    cache.put(4, 1)
    assert cache.get(1) == -1
    assert cache.get(2) == 3


test1()
test2()
test3()
test4()

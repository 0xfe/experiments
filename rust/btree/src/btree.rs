use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    rc::Rc,
};

// We need the Debug trait so we can use println! on it, and
// the PartialEq trait so we can compare the enums.
#[derive(Debug, PartialEq, Clone, Copy)]
enum NodeType {
    ROOT,
    LEFT,
    RIGHT,
}

type Item<T: Ord + Clone + Copy> = Rc<RefCell<Node<T>>>;

// Down here we add the Ord trait because we want to be able to
// compare the elements during insertion / search. And the Copy trait
// allows you to return a copy of the elements, which encourages you
// to attach primitive types or references.
#[derive(Debug, Clone)]
pub struct Node<T: Ord + Clone + Copy> {
    pos: NodeType,
    left: Option<Item<T>>,
    right: Option<Item<T>>,
    parent: Option<Item<T>>,
    val: Option<T>,
}

impl<T: Ord + Clone + Copy> Node<T> {
    fn new() -> Item<T> {
        Rc::new(RefCell::new(Node {
            pos: NodeType::ROOT,
            left: None,
            right: None,
            parent: None,
            val: None,
        }))
    }
}

#[derive(Debug)]
pub struct BTree<T: Ord + Copy> {
    root: Item<T>,
}

#[derive(Debug)]
pub struct BTreeIter<'a, T: Ord + Copy> {
    btree: &'a BTree<T>,
    cur: Item<T>,
}

impl<T: Ord + Copy + Clone> BTree<T> {
    pub fn new() -> BTree<T> {
        BTree { root: Node::new() }
    }

    pub fn push(&mut self, item: T) {
        let mut cur = Rc::clone(&self.root);

        loop {
            let mut next = Rc::clone(&cur);
            let some_val = (*cur).borrow().val;

            if let Some(val) = some_val {
                if item <= val {
                    if let Some(left) = &(*cur).borrow().left {
                        // If there's already a left node, then move cur to it
                        next = Rc::clone(left);
                    } else {
                        // Otherwise, create a new node, and move to it
                        let node = Node::new();
                        (*node).borrow_mut().parent = Some(Rc::clone(&cur));
                        (*cur).borrow_mut().left = Some(Rc::clone(&node));
                        next = Rc::clone(&node);
                    }
                } else {
                    let some = (*cur).borrow();
                    if let Some(right) = &some.right {
                        // If there's already a right node, then move cur to it
                        next = Rc::clone(right);
                    } else {
                        drop(some);
                        // Otherwise, create a new node, and move to it
                        let node = Node::new();
                        (*node).borrow_mut().parent = Some(Rc::clone(&cur));
                        (*cur).borrow_mut().right = Some(Rc::clone(&node));
                        next = Rc::clone(&node);
                    }
                }
            } else {
                drop(some_val);
                (*next).borrow_mut().val = Some(item);
                break;
            }

            cur = Rc::clone(&next);
        }
    }

    pub fn iter(&self) -> BTreeIter<T> {
        BTreeIter {
            btree: self,
            cur: Rc::clone(&self.root),
        }
    }
}

/*
impl<T: Ord + Copy + Clone> Iterator for BTreeIter<'_, T> {
    // Need this alias because it's in the fn signature of the trait
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur.pos == NodeType::ROOT {
            None
        } else {
            Some(self.cur.val.unwrap())
        }
    }
}
*/

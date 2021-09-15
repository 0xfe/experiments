use std::{cell::RefCell, rc::Rc};

// Our nodes are refcounted refcells. Ref-counted because they
// have multiple owners, and refcells because the borrow checker is mean.
type Item<T> = Rc<RefCell<Node<T>>>;

// Down here we add the Ord trait because we want to be able to
// compare the elements during insertion / search. And the Copy trait
// allows you to return a copy of the elements, which encourages you
// to attach primitive types or references.
#[derive(Debug)]
pub struct Node<T: Ord + Copy> {
    left: Option<Item<T>>,
    right: Option<Item<T>>,
    parent: Option<Item<T>>,
    val: Option<T>,
}

impl<T: Ord + Copy> Node<T> {
    fn new() -> Item<T> {
        Rc::new(RefCell::new(Node {
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

impl<T: Ord + Copy> BTree<T> {
    pub fn new() -> BTree<T> {
        BTree { root: Node::new() }
    }

    pub fn push(&mut self, item: T) {
        let mut cur = Rc::clone(&self.root);

        loop {
            let mut next = Rc::clone(&cur);
            let some_val = (*cur).borrow().val;

            if let Some(val) = some_val {
                let some = (*cur).borrow();
                if item <= val {
                    if let Some(left) = &some.left {
                        // If there's already a left node, then move cur to it
                        next = Rc::clone(left);
                    } else {
                        // Unborrow so we can borrow_mut below.
                        drop(some);
                        // Otherwise, create a new node, and move to it
                        let node = Node::new();
                        (*node).borrow_mut().parent = Some(Rc::clone(&cur));
                        (*cur).borrow_mut().left = Some(Rc::clone(&node));
                        next = Rc::clone(&node);
                    }
                } else {
                    if let Some(right) = &some.right {
                        // If there's already a right node, then move cur to it
                        next = Rc::clone(right);
                    } else {
                        // Unborrow so we can borrow_mut below.
                        drop(some);
                        // Otherwise, create a new node, and move to it
                        let node = Node::new();
                        (*node).borrow_mut().parent = Some(Rc::clone(&cur));
                        (*cur).borrow_mut().right = Some(Rc::clone(&node));
                        next = Rc::clone(&node);
                    }
                }
            } else {
                (*next).borrow_mut().val = Some(item);
                break;
            }

            cur = Rc::clone(&next);
        }
    }

    // Return a breadth-first search iterator.
    pub fn bfs_iter(&self) -> BTreeBFSIter<T> {
        BTreeBFSIter {
            btree: self,
            q: vec![Rc::clone(&self.root)],
            cur: Rc::clone(&self.root),
        }
    }
}

// This is a breadth-first search iterator.
#[derive(Debug)]
pub struct BTreeBFSIter<'a, T: Ord + Copy> {
    btree: &'a BTree<T>,
    q: Vec<Item<T>>,
    cur: Item<T>,
}

impl<T: Ord + Copy> Iterator for BTreeBFSIter<'_, T> {
    // Need this alias because it's in the fn signature of the trait
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.q.len() == 0 {
            return None;
        }

        let item = self.q.remove(0);
        let node = (*item).borrow();

        if let Some(left) = &node.left {
            self.q.push(Rc::clone(&left));
        }

        if let Some(right) = &node.right {
            self.q.push(Rc::clone(&right));
        }

        if let Some(val) = &node.val {
            Some(*val)
        } else {
            self.next()
        }
    }
}

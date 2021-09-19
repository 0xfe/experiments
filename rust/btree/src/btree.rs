use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

// Our nodes are refcounted refcells. Ref-counted because they
// have multiple owners, and refcells because the borrow checker is mean.
type Item<T> = Rc<RefCell<Node<T>>>;
type WeakItem<T> = Weak<RefCell<Node<T>>>; // prevent cycles

// Down here we add the Ord trait because we want to be able to
// compare the elements during insertion / search. And the Copy trait
// allows you to return a copy of the elements, which encourages you
// to attach primitive types or references.
#[derive(Debug)]
pub struct Node<T> {
    left: Option<Item<T>>,
    right: Option<Item<T>>,
    parent: Option<WeakItem<T>>,
    val: Option<T>,
}

#[derive(Debug)]
pub struct BTree<T> {
    root: Item<T>,
}

impl<T: Ord + Copy> BTree<T> {
    pub fn new() -> BTree<T> {
        BTree {
            root: BTree::new_noderef(),
        }
    }

    fn new_noderef() -> Item<T> {
        Rc::new(RefCell::new(Node {
            left: None,
            right: None,
            parent: None,
            val: None,
        }))
    }

    // Insert a node into the binary tree.
    pub fn insert(&mut self, item: T) {
        let mut cur = Rc::clone(&self.root);

        loop {
            let mut next = Rc::clone(&cur);
            let some_val = (*cur).borrow().val;

            if let Some(val) = some_val {
                let mut node = (*cur).borrow_mut();
                if item <= val {
                    // You can also do:
                    //   if let Some(left) = &node.left {...}
                    if let Some(ref left) = node.left {
                        // If there's already a left node, then move cur to it
                        next = Rc::clone(left);
                    } else {
                        // Otherwise, create a new node, and move to it
                        let new_node = BTree::new_noderef();
                        (*new_node).borrow_mut().parent = Some(Rc::downgrade(&cur));
                        node.left = Some(Rc::clone(&new_node));
                        next = Rc::clone(&new_node);
                    }
                } else {
                    if let Some(ref right) = node.right {
                        // If there's already a right node, then move cur to it
                        next = Rc::clone(right);
                    } else {
                        // Otherwise, create a new node, and move to it
                        let new_node = BTree::new_noderef();
                        (*new_node).borrow_mut().parent = Some(Rc::downgrade(&cur));
                        node.right = Some(Rc::clone(&new_node));
                        next = Rc::clone(&new_node);
                    }
                }
            } else {
                (*next).borrow_mut().val = Some(item);
                break;
            }

            cur = next;
        }
    }

    // Return a breadth-first search iterator.
    pub fn bfs_iter(&self) -> BFSIter<T> {
        BFSIter {
            q: vec![Rc::clone(&self.root)],
            cur: Rc::clone(&self.root),
        }
    }

    // Return a depth-first search iterator.
    pub fn dfs_iter(&self) -> DFSIter<T> {
        DFSIter {
            stack: vec![Rc::clone(&self.root)],
            cur: Rc::clone(&self.root),
        }
    }
}

// Default to BFS Iterator
impl<T: Copy + Ord> IntoIterator for &BTree<T> {
    type Item = T;
    type IntoIter = BFSIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.bfs_iter()
    }
}

// This is a breadth-first search iterator.
#[derive(Debug)]
pub struct BFSIter<T: Copy> {
    q: Vec<Item<T>>,
    cur: Item<T>,
}

impl<T: Copy + Ord> Iterator for BFSIter<T> {
    // Need this alias because it's in the fn signature of the trait
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.q.len() == 0 {
            return None;
        }

        let item = self.q.remove(0);
        let node = (*item).borrow();

        // Alternate way to express the if let statements in DFSIter below
        node.left.as_ref().map(|l| self.q.push(Rc::clone(l)));
        node.right.as_ref().map(|r| self.q.push(Rc::clone(r)));

        node.val.or_else(|| self.next())
    }
}

#[derive(Debug)]
pub struct DFSIter<T: Copy> {
    stack: Vec<Item<T>>,
    cur: Item<T>,
}

impl<T: Copy + Ord> Iterator for DFSIter<T> {
    // Need this alias because it's in the fn signature of the trait
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.stack.pop()?;
        let node = (*item).borrow();

        if let Some(ref left) = node.left {
            self.stack.push(Rc::clone(&left));
        }

        if let Some(ref right) = node.right {
            self.stack.push(Rc::clone(&right));
        }

        node.val.or_else(|| self.next())
    }
}

#[derive(Debug, PartialEq)]
enum NodeType {
    ROOT,
    LEFT,
    RIGHT,
}

#[derive(Debug)]
pub struct Node<T: Ord + Copy> {
    pos: NodeType,
    left: Option<Box<Node<T>>>,
    right: Option<Box<Node<T>>>,
    parent: Option<Box<Node<T>>>,
    val: Option<T>,
}

#[derive(Debug)]
pub struct BTree<T: Ord + Copy> {
    root: Node<T>,
}

#[derive(Debug)]
pub struct BTreeIter<'a, T: Ord + Copy> {
    btree: &'a BTree<T>,
    cur: &'a Node<T>,
}

impl<T: Ord + Copy> BTree<T> {
    pub fn new() -> BTree<T> {
        BTree {
            root: Node {
                pos: NodeType::ROOT,
                left: None,
                right: None,
                parent: None,
                val: None,
            },
        }
    }

    pub fn iter(&self) -> BTreeIter<T> {
        BTreeIter {
            btree: self,
            cur: &self.root,
        }
    }
}

impl<T: Ord + Copy> Iterator for BTreeIter<'_, T> {
    // Need this because it's in the fn signature of the trait
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur.pos == NodeType::ROOT {
            None
        } else {
            Some(self.cur.val.unwrap())
        }
    }
}

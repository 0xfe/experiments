#[derive(Debug)]
pub struct Node<T>
where
    T: Ord,
{
    left: Option<Box<Node<T>>>,
    right: Option<Box<Node<T>>>,
    val: Option<T>,
}

#[derive(Debug)]
pub struct BTree<T>
where
    T: Ord,
{
    root: Node<T>,
}

impl<T: Ord> BTree<T> {
    pub fn new() -> BTree<T> {
        BTree {
            root: Node {
                left: None,
                right: None,
                val: None,
            },
        }
    }
}

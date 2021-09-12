mod btree;

fn main() {
    println!("Hello, world!");

    let tree: btree::BTree<&str> = btree::BTree::new();

    println!("btree {:?}", tree);
}

mod btree;

fn main() {
    println!("Hello, world!");

    let tree: btree::BTree<String> = btree::BTree::new();

    println!("btree {:?}", tree);
}

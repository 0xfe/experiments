mod btree;

fn main() {
    println!("Hello, world!");

    let tree: btree::BTree<&String> = btree::BTree::new();
    println!("btree {:?}", tree);

    let mut tree2: btree::BTree<&str> = btree::BTree::new();
    tree2.insert("hello");
    tree2.insert("world");
    tree2.insert("!");

    for s in tree2.bfs_iter() {
        println!("Node: {}", s);
    }
}

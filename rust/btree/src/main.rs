mod btree;

fn main() {
    println!("Hello, world!");

    let tree: btree::BTree<&str> = btree::BTree::new();
    println!("btree {:?}", tree);

    let mut tree2: btree::BTree<&String> = btree::BTree::new();
    let s = String::from("hello");
    let s2 = String::from("world");
    tree2.push(&s);
    tree2.push(&s2);
    // println!("btree2 {:?}", tree2);
}

mod btree;

fn main() {
    println!("Hello, world!");

    let tree: btree::BTree<&str> = btree::BTree::new();
    println!("btree {:?}", tree);

    let mut tree2: btree::BTree<&String> = btree::BTree::new();
    let s = String::from("hello");
    let s2 = String::from("world");
    let s3 = String::from("!");
    tree2.push(&s);
    tree2.push(&s2);
    tree2.push(&s3);
    // println!("btree2 {:?}", tree2);

    for i in tree2.bfs_iter() {
        println!("{}", i);
    }
}

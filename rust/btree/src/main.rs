mod btree;

fn main() {
    println!("Hello, world!");

    let tree: btree::BTree<&String> = btree::BTree::new();
    println!("btree {:?}", tree);

    let mut tree2: btree::BTree<&str> = btree::BTree::new();
    tree2.insert("hello");
    tree2.insert("!");
    tree2.insert("2");
    tree2.insert("1");
    tree2.insert("boo");
    tree2.insert("foo");
    tree2.insert("world");

    for s in tree2.bfs_iter() {
        println!("BFS: {}", s);
    }

    for s in tree2.dfs_iter() {
        println!("DFS: {}", s);
    }
}

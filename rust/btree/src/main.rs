mod btree;

fn main() {
    // Tree1 is a mutable reference.
    let tree1 = &mut btree::BTree::new();
    tree1.insert("hello");
    tree1.insert("!");
    tree1.insert("2");
    tree1.insert("1");
    tree1.insert("boo");
    tree1.insert("foo");
    tree1.insert("world");

    // Turn mutable reference into immutable reference because
    // the IntoIterator trait is only implemented for immutable
    // references.
    println!("Iterating over tree1 {:?}", tree1);
    for s in &*tree1 {
        println!("tree1 BFS: {}", s);
    }

    let c: Vec<&str> = tree1.bfs_iter().collect();
    println!("Collect: {:?}", c);

    let c: Vec<&str> = tree1.dfs_iter().collect();
    println!("Collect: {:?}", c);

    // Tree2 is a mutable value.
    let mut tree2 = btree::BTree::new();
    tree2.insert("hello");
    tree2.insert("!");
    tree2.insert("2");
    tree2.insert("1");
    tree2.insert("boo");
    tree2.insert("foo");
    tree2.insert("world");

    // These get the iterators directly from the instance, not using
    // the IntoIterator trait.
    for s in tree2.bfs_iter() {
        println!("tree3 BFS: {}", s);
    }

    for s in tree2.dfs_iter() {
        println!("tree3 DFS: {}", s);
    }
}

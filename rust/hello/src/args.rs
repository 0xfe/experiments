use std::env;

pub fn run() {
    let args: Vec<String> = env::args().collect();

    println!("Args: {:?}", args);

    let target = args[0].clone();
    println!("$0 = {}", target);

    #[derive(Debug, Clone)]
    struct MoStruct {
        i: i64,
        f: f64,
    }

    let mo = MoStruct{i: 4, f: 4.4};
    println!("mo {:?}", mo);

    let boo = mo.clone();
    println!("boo {:?}", boo);

    println!("{}, {}", boo.i, boo.f);
}

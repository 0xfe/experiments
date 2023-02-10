use std::net::TcpListener;

fn start() {
    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();

    for stream in listener.incoming() {
        println!("Connection established: {:?}", stream)
    }
}

pub fn run() {
    println!("starting server...");
    start();
}

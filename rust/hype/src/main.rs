use tokio::{
    io::AsyncReadExt,
    net::{TcpListener, TcpStream},
};

use hype::parser::Parser;

pub async fn dump_stream(mut stream: TcpStream) {
    let mut parser = Parser::new();

    loop {
        let mut buf = [0u8; 16];

        match stream.read(&mut buf).await {
            Ok(0) => {
                parser.parse_eof().unwrap();
                break;
            }
            Ok(n) => {
                parser.parse_buf(&buf[..n]).unwrap();
            }
            Err(e) => {
                println!("Error: {:?}", e);
                break;
            }
        }
    }

    println!("Parser output:\n{:?}", parser);
}

pub async fn start() {
    let listener = TcpListener::bind("127.0.0.1:4000").await.unwrap();

    loop {
        let (socket, _) = listener.accept().await.unwrap();
        dump_stream(socket).await;
        println!("Done");
    }
}

#[tokio::main]
async fn main() {
    start().await;
}

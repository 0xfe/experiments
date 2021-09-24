use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    net::TcpListener,
};

#[tokio::main]
async fn main() {
    println!("Hello, world!");

    let listener = TcpListener::bind("127.0.0.1:4040").await.unwrap();

    let (mut socket, addr) = listener.accept().await.unwrap();

    let (reader, mut writer) = socket.split();

    let mut reader = BufReader::new(reader);
    let mut line = String::new();

    println!("Connection from: {:?}", addr);
    loop {
        let bytes_read = reader.read_line(&mut line).await.unwrap();
        if bytes_read == 0 {
            break;
        }

        println!("Received: {:?}", line);
        writer.write_all(line.as_bytes()).await.unwrap();
        line.clear();
    }
}

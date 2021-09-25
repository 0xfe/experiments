use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    net::TcpListener,
    sync::broadcast,
};

#[tokio::main]
async fn main() {
    println!("Hello, world!");

    let listener = TcpListener::bind("127.0.0.1:4040").await.unwrap();

    let (tx, _rx) = broadcast::channel::<String>(10);

    loop {
        let (mut socket, addr) = listener.accept().await.unwrap();
        let tx = tx.clone();
        let mut rx = tx.subscribe();

        tokio::spawn(async move {
            let (reader, mut writer) = socket.split();

            let mut reader = BufReader::new(reader);
            let mut line = String::new();

            println!("Connection from: {:?}", addr);
            loop {
                tokio::select! {
                    result = reader.read_line(&mut line) => {
                        if result.unwrap() == 0 {
                            break;
                        }

                        tx.send(line.clone()).unwrap();
                        line.clear();
                    }

                    _ = rx.recv() => {
                        let msg = rx.recv().await.unwrap();

                        println!("Received: {:?}", msg);
                        writer.write_all(line.as_bytes()).await.unwrap();
                    }
                }
            }
        });
    }
}

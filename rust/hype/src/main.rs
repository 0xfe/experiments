use hype::server::Server;

#[tokio::main]
async fn main() {
    let server = Server::new("127.0.0.1".into(), 4000);
    server.start().await.unwrap();
}

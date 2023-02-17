use std::sync::Arc;

use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::{TcpListener, TcpStream},
};

use crate::{parser::Parser, response::Response, status};

pub struct Server {
    address: String,
    port: u16,
}

impl Server {
    async fn process_stream(mut stream: TcpStream) {
        println!("Connection: {:?}", stream);
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
                    if parser.is_complete() {
                        parser.parse_eof().unwrap();
                        break;
                    }
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                    break;
                }
            }
        }

        let request = parser.get_request();
        println!("request: {:?}", request);

        if request.method == "GET".to_string() && request.path == "/".to_string() {
            let mut response = Response::new(status::from(status::OK));
            response.set_body("<html>hi!</html>".into());
            let buf = response.serialize();

            stream.write_all(buf.as_bytes()).await.unwrap();
        }
    }

    pub fn new(address: String, port: u16) -> Self {
        Self { address, port }
    }

    pub async fn start(&self) -> Result<(), ()> {
        let hostport = format!("{}:{}", self.address, self.port);
        println!("Listening on {}...", hostport);
        let listener = TcpListener::bind(hostport).await.unwrap();

        loop {
            let (socket, _) = listener.accept().await.unwrap();
            tokio::spawn(async move { Server::process_stream(socket).await });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let server = Server::new("a".into(), 10);
    }
}

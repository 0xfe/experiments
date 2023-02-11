/*

Simple webserver that serves files out of the local directory.

GET / -- returns static text
GET /files/{filename} -- returns contents of filename (does not recurse directories)

*/

use std::{
    fs, io,
    io::{prelude::*, BufReader},
    net::{TcpListener, TcpStream},
    thread,
    time::Duration,
};

use threadpool::ThreadPool;

fn start() {
    let listener = TcpListener::bind("127.0.0.1:37878").unwrap();
    let pool = ThreadPool::new(20);

    for stream in listener.incoming() {
        println!("Connection established: {:?}", stream);
        pool.execute(move || handle_connection(stream.unwrap()));
    }
}

fn return_response(mut stream: TcpStream, status: String, contents: String) {
    let status_line = format!("HTTP/1.1 {}", status);
    let length = contents.len();
    let response = format!("{status_line}\r\nContent-Length: {length}\r\n\r\n{contents}");
    println!("Response: {}", response);
    stream.write_all(response.as_bytes()).unwrap();
}

fn get_file_contents(path: &String) -> Result<String, io::Error> {
    let parts: Vec<_> = path.split('/').collect();
    println!("{:?}", parts);

    if parts[1] == "files" {
        let filename = parts[2];
        let contents = fs::read_to_string(filename)?;
        return Ok(contents);
    }

    return Err(io::Error::new(io::ErrorKind::NotFound, "file not found"));
}

fn handle_connection(mut stream: TcpStream) {
    let reader = BufReader::new(&mut stream);

    let request: Vec<_> = reader
        .lines()
        .map(|l| l.unwrap())
        .take_while(|l| !l.is_empty())
        .collect();

    println!("Request: {:?}", request);

    let parts: Vec<_> = request[0].split_whitespace().collect();
    if parts[0] == "GET" {
        if parts[1] == "/" {
            return_response(
                stream,
                "200 OK".into(),
                "<!DOCTYPE html>\n<html><b>OKAAAAY!</b></html>\n".into(),
            );
        } else if parts[1] == "/sleep" {
            thread::sleep(Duration::from_secs(5));
            return_response(
                stream,
                "200 OK".into(),
                "<!DOCTYPE html>\n<html><b>Oh hi, good morning!YAWN!</b></html>\n".into(),
            );
        } else {
            let result = get_file_contents(&parts[1].into());

            if let Ok(contents) = result {
                return_response(stream, "200 OK".into(), contents.into());
            } else {
                return_response(
                    stream,
                    "404 NOT FOUND".into(),
                    "<!DOCTYPE html>\n<html><b>404 nothing to see here</b></html>\n".into(),
                );
            }
        }
    } else {
        return_response(
            stream,
            "500 SERVER ERROR".into(),
            "<!DOCTYPE html>\n<html><b>500 boo</b></html>\n".into(),
        );
    }
}

pub fn run() {
    println!("starting server...");
    start();
}

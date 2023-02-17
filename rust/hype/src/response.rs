use std::collections::HashMap;

use tokio::io::AsyncWriteExt;

use crate::status;

#[derive(Debug, Clone)]
pub struct Response<'a> {
    pub status: status::Status<'a>,
    pub headers: HashMap<String, String>,
    pub body: String,
}

impl<'a> Response<'a> {
    pub fn new(status: status::Status) -> Response {
        Response {
            status,
            headers: HashMap::new(),
            body: String::new(),
        }
    }

    pub fn set_header(&mut self, key: String, value: String) {
        self.headers.insert(key, value);
    }

    pub fn set_body(&mut self, body: String) {
        self.body = body;
    }

    pub fn serialize(&mut self) -> String {
        let status_line = format!("HTTP/1.1 {} {}", self.status.code, self.status.text);
        let length = self.body.len();
        if length > 0 {
            self.set_header("Content-Length".into(), length.to_string());
        }

        let headers: String = self
            .headers
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect::<Vec<String>>()
            .join("\r\n");

        format!("{status_line}\r\n{headers}\r\n\r\n{}", self.body)
    }
}

pub struct Writer<'a, T: AsyncWriteExt> {
    stream: &'a mut T,
}

#[allow(dead_code)]
impl<'a, T: AsyncWriteExt> Writer<'a, T> {
    pub fn new(stream: &'a mut T) -> Writer<T> {
        return Writer { stream };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut response = Response::new(status::from(status::OK));
        println!("{}", response.serialize())
    }

    #[test]
    fn it_works_with_body() {
        let mut response = Response::new(status::from(status::OK));
        response.set_body("<HTML><b>Hello world!</b></HTML>".into());
        println!("{}", response.serialize())
    }
}

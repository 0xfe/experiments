use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash)]
enum State {
    Start,
    InCommand,
    InHeaders,
    InBody,
}

lazy_static! {
    // map of target state -> prior state(s)
    static ref STATE_MACHINE: HashMap<State, Vec<State>> = HashMap::from([
        (State::InCommand, vec![State::Start]),
        (State::InHeaders, vec![State::InCommand]),
        (State::InBody, vec![State::InHeaders]),
]);
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedState,
    InvalidStateTransition,
    BadCommandLine(String),
    BadHeaderLine(String),
    UnexpectedEOF,
}

#[derive(Debug)]
pub struct Parser {
    state: State,
    buf: Vec<u8>,
    command: String,
    path: String,
    version: String,
    headers: HashMap<String, String>,
    body: Vec<u8>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            state: State::Start,
            buf: Vec::with_capacity(16384),
            command: String::new(),
            path: String::new(),
            version: String::new(),
            headers: HashMap::new(),
            body: Vec::new(),
        }
    }

    fn update_state(&mut self, target_state: State) -> Result<(), ParseError> {
        if !STATE_MACHINE
            .get(&target_state)
            .unwrap()
            .contains(&self.state)
        {
            return Err(ParseError::InvalidStateTransition);
        }

        self.state = target_state;

        Ok(())
    }

    fn commit_command(&mut self) -> Result<(), ParseError> {
        let command_line = std::str::from_utf8(&self.buf[..]).unwrap();
        let parts = command_line.split_ascii_whitespace().collect::<Vec<&str>>();

        if parts.len() != 3 {
            return Err(ParseError::BadCommandLine(command_line.into()));
        }

        self.command = parts[0].into();
        self.path = parts[1].into();
        self.version = parts[2].into();

        self.buf.clear();
        Ok(())
    }

    fn commit_header(&mut self) -> Result<(), ParseError> {
        let mut result: Result<(), ParseError> = Ok(());
        let header_line = std::str::from_utf8(&self.buf[..]).unwrap();

        if header_line == "\r" || header_line == "" {
            result = self.update_state(State::InBody);
        } else {
            if let Some((k, v)) = header_line.split_once(':') {
                self.headers.insert(k.into(), v.trim().into());
            } else {
                result = Err(ParseError::BadHeaderLine(header_line.into()));
            }
        }

        self.buf.clear();
        result
    }

    fn commit_line(&mut self) -> Result<(), ParseError> {
        let result: Result<(), ParseError>;

        match self.state {
            State::Start => result = Ok(()),
            State::InCommand => {
                result = self.commit_command();
                self.update_state(State::InHeaders)?;
            }
            State::InHeaders => {
                result = self.commit_header();
            }
            _ => {
                result = Err(ParseError::UnexpectedState);
            }
        }

        result
    }

    fn consume(&mut self, b: u8) -> Result<(), ParseError> {
        self.buf.push(b);
        Ok(())
    }

    pub fn parse_buf(&mut self, buf: &[u8]) -> Result<(), ParseError> {
        for c in buf {
            let ch = *c as char;
            match self.state {
                State::Start => {
                    if !ch.is_whitespace() {
                        self.consume(*c)?;
                        self.update_state(State::InCommand)?;
                    }
                }
                State::InCommand | State::InHeaders => {
                    if ch == '\n' {
                        self.commit_line()?;
                    } else {
                        self.consume(*c)?;
                    }
                }
                State::InBody => {
                    self.consume(*c)?;
                }
            }
        }

        Ok(())
    }

    pub fn parse_eof(&mut self) -> Result<(), ParseError> {
        if self.state == State::InBody {
            self.body = std::str::from_utf8(&self.buf[..]).unwrap().into();
            return Ok(());
        }

        Err(ParseError::UnexpectedEOF)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(buf: &str) {
        let mut parser = Parser::new();
        println!("buf:\n{}", buf);
        let result = parser.parse_buf(String::from(buf).as_bytes());
        assert_eq!(result, Ok(()));
    }

    #[test]
    fn it_works() {
        parse_ok(
            r##"POST / HTTP/1.1
Host: localhost:4000
Content-Length: 20

{"merchantID": "00"}"##,
        );
    }

    #[test]
    fn newline_prefixes() {
        parse_ok(
            r##"

POST / HTTP/1.1
Host: localhost:4000
Content-Length: 20

{"merchantID": "00"}"##,
        );
    }
}
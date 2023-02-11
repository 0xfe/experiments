use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};

fn parse_string() -> Result<()> {
    let data = r#"
        {
            "name": "bob",
            "age": "45",
            "phones": ["+1 (555) 555-5555" , "+1 (555) 555-5556" ]
        }
    "#;

    let v: Value = serde_json::from_str(data)?;

    println!(
        "Hi {}, You're {}. Please call me at {}.",
        v["name"], v["age"], v["phones"][0]
    );

    Ok(())
}

#[derive(Serialize, Deserialize, Debug)]
struct Address {
    street: String,
    city: String,
}

fn deserialize() {
    let data = r#"
        {
            "street": "21 Downing St.",
            "city": "London"
        }
    "#;

    let v: Address = serde_json::from_str(data).unwrap();
    println!("{:?}", v);
}

fn serialize() {
    let address = Address {
        street: "555 Prospect Pl.".into(),
        city: "Brooklyn".into(),
    };

    let data = serde_json::to_string(&address).unwrap();
    println!("{:?}", data);
}

pub fn run() {
    parse_string().unwrap();
    deserialize();
    serialize();
}

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

pub fn run() {
    parse_string().unwrap();
}

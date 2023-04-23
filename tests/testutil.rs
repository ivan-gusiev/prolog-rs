pub fn load_sample(name: &str) -> String {
    let path = format!("./samples/prolog/{name}");
    std::fs::read_to_string(path.as_str()).unwrap()
}

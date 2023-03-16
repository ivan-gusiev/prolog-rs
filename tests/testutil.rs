pub fn load_sample(name: &str) -> String {
    let path = format!("./prolog-samples/{name}");
    std::fs::read_to_string(path.as_str()).unwrap()
}

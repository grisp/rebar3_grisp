#[rustler::nif]
fn hello() -> &'static str {
    "Hello from a Rust NIF"
}

rustler::init!("{{{name}}}_rust_nif");

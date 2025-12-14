use reqwest::blocking;
use reqwest::header::{HeaderMap, CONTENT_LENGTH};
use crate::urls::url_base;
use std::time::Duration;

pub fn cliente_nuevo() -> Result<blocking::Client, reqwest::Error> {
    let mut def_head = HeaderMap::new();
    def_head.insert(CONTENT_LENGTH, "0".parse().unwrap());

    let client = blocking::Client::builder()
        .cookie_store(true)
        .default_headers(def_head)
        .connect_timeout(Duration::from_secs(30))
        .timeout(Duration::from_secs(600))
        .build()?;

    let _ = client.get(url_base()).send()?;
    Ok(client)
}

/// Cliente para pruebas (sin inicialización extra)
pub fn cliente_test() -> Result<blocking::Client, reqwest::Error> {
    let mut def_head = HeaderMap::new();
    def_head.insert(CONTENT_LENGTH, "0".parse().unwrap());

    let client = blocking::Client::builder()
        .cookie_store(true)
        .default_headers(def_head)
        .build()?;

    Ok(client)
}

use std::collections::HashMap;
use std::str::from_utf8;
use std::time::duration::Duration;
use std::io::timer::sleep;

use http::{StatusCode};

#[derive(Show)]
pub struct Response {
	body: Vec<u8>,
	status_code : StatusCode,
	status_message : String,
	headers: HashMap<String, String>,	
}

impl Response {
	pub fn init() -> Response {
		let mut r = Response { 
			body: Vec::new(), 
			status_code: StatusCode::Ok, 
			status_message: String::new(), 
			headers: HashMap::new()
		};
		sleep(Duration::seconds(5));
		r.headers.insert("Content-Length".to_string(), "0".to_string());
		r 
	}

	pub fn not_found(mut self) -> Response {
		self.status_code = StatusCode::NotFound;
		self.status_message = "Not found".to_string();
		self
	}

	pub fn bad_request(mut self) -> Response {
		self.status_code = StatusCode::BadRequest;
		self.status_message = "Bad request".to_string();
		self
	}

	pub fn set_body(&mut self, body: &[u8]) {
		self.headers.insert("Content-Length".to_string(), self.body.len().to_string());
		self.body.clear();
		self.body.push_all(body);
	}
	/*pub fn set_body(&mut self, body: Vec<u8>) {
		self.set_body(body.as_slice());
	}*/
	pub fn set_status_code(&mut self, status_code: StatusCode) {
		self.status_code = status_code;
	}
	pub fn set_status_message(&mut self, message: &str) {
		self.status_message = message.to_string();
	}
	pub fn get_header(key: &str) {

	}
	pub fn set_header(key: &str) {

	}
	pub fn to_buffer(self) -> Vec<u8> {
		let mut buf : Vec<u8> = Vec::new();
		buf.push_all("HTTP/1.1 ".as_bytes());
		buf.write_int(self.status_code as isize);
		buf.push_all(" ".as_bytes());
		buf.write_str(self.status_message.as_slice());
		buf.push_all("\r\n".as_bytes());
		for (key, val) in self.headers.iter() {
			buf.push_all(key.as_slice().as_bytes());
			buf.push_all(": ".as_bytes());
			buf.push_all(val.as_slice().as_bytes());
			buf.push_all("\r\n".as_bytes());
		}
		buf.push_all("\r\n".as_bytes());
		buf.push_all(self.body.as_slice());
		let b2 = buf.clone();
		let s1 = from_utf8(b2.as_slice());
		println!("buf: {:?} {:?}", buf, s1);
		buf
	}
}

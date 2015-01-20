#[derive(Show, PartialEq)]
pub enum Method {
	GET, POST, PUT, DELETE
}

impl Method {
	pub fn from_utf8(v: Vec<u8>) -> Option<Method> {
		match String::from_utf8(v) {
			Ok(s) => match s.as_slice() {
				"GET" => Some(Method::GET),
				"POST" => Some(Method::POST),
				"PUT" => Some(Method::PUT),
				_ => None
			},
			Err(s) => None
		}
	}
}
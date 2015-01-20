use std::io::{TcpStream, BytesReader};
use std::collections::HashMap;
use http::method::Method;
use std::str::FromStr;
use std::str::StrExt;
use std::str::from_utf8;
use std::str::from_utf8_unchecked;

#[derive(Show)]
pub struct Request {
	pub method: Method,
	pub http_minor : u32,
	pub http_major: u32,
	pub path: String,
	pub protocol: String,
	pub content_length: u32,
	pub body: Vec<u8>,
	pub query_params: HashMap<String, String>,
	pub headers: HashMap<String, String>,

}

impl Request {
	pub fn init() -> Request {
		Request { 
			method: Method::GET, 
			http_minor: 0, http_major: 0, 
			path: String::new(), 
			protocol: String::new(),
			content_length: 0,
			body: Vec::new(), 
			query_params: HashMap::new(),
			headers: HashMap::new()
		}
	}
	pub fn get_method(self) -> Method { self.method }
	pub fn get_path(self) -> String { self.path }
	pub fn get_query_param(&self, key: &str) -> Option<&String> {
		self.query_params.get(&key.to_string())
	}
	pub fn get_header(&self, key: &str) -> Option<&String> { 
		self.headers.get(&key.to_string())
	}
	pub fn get_body(self) -> Vec<u8> { self.body }
}

pub struct RequestParser<'a> {
	stream: &'a mut TcpStream,
	cur_lex: String,
	request: Request
}

impl<'a> RequestParser<'a> {
	pub fn init(stream: &'a mut TcpStream) -> RequestParser {
		RequestParser { cur_lex: String::new(), stream: stream, request: Request::init() }
	}
	fn next_byte(&mut self) -> u8 {
		let b = self.stream.bytes().next().unwrap().ok().unwrap();
		println!("nb: {}", b);
		b
	}
	fn match_next(&mut self, b: u8) -> bool {
		self.next_byte() == b
	}
	fn skip_spaces(&mut self) {
		self.stream.bytes()
			.map(|c| { c.ok().unwrap() }) //println!("skip_c: {}", *c); 
			.skip_while(|c| {*c != (' ' as u8) /*&& *c != ('\r' as u8) && *c != ('\n' as u8) */ } );
		/*for c in stream.bytes() {
			if (c != ' ' as u8) { return c; }
		}*/
	}

	fn take_until_equal_vec(&mut self, pat: u8, v: &mut Vec<u8>) {
		v.extend(self.stream.bytes()
			.map(|c| { c.ok().unwrap() })
			.take_while(|c| { /*println!("c: {}", *c);*/ *c != pat }));
	}

	fn take_until_equal(&mut self,  pat: u8) -> Vec<u8> {
		let mut v = Vec::new();
		self.take_until_equal_vec(pat, &mut v);
		v
	}

	fn parse_method(&mut self) -> bool {
		self.skip_spaces();
		let v : Vec<u8> = self.take_until_equal(' ' as u8);
		println!("v: {:?}", v);
		Method::from_utf8(v).map_or(false, |m| { self.request.method = m; true})
/*		
		for c in stream.bytes() {
			if (c != ' ') { cur_lex.push(from_utf32(c as u32)) }
			else break;
		}*/
	}
	
	fn parse_url(&mut self) -> bool {
		self.skip_spaces();
		let v : Vec<u8> = self.take_until_equal(' ' as u8);
		println!("v: {:?}", v);
		match String::from_utf8(v) {
			Ok(s) => {
				self.request.path = s;
				true
			},
			Err(_) => false
		}
	}
	fn parse_proto(&mut self) -> bool {
		self.skip_spaces();
		let v : Vec<u8> = self.take_until_equal('\r' as u8);
		println!("v: {:?}", v);
		String::from_utf8(v).ok().map_or(false, |p| {self.request.protocol = p; true})
	}
	fn parse_headers(&mut self) -> bool {
		//self.skip_spaces();
		let rn_pattern =  ['\r' as u8, '\n' as u8];
		loop {
			if !self.match_next('\n' as u8) { return false }
			let mut v = Vec::new();
			let b = self.next_byte();
			if b == ('\r' as u8) { return self.next_byte() == ('\n' as u8) }
			v.push(b);
			self.take_until_equal_vec(':' as u8, &mut v);
			println!("here2");
			match from_utf8(v.as_slice()) {
				Ok(key) => {
					self.skip_spaces();
					let v : Vec<u8> = self.take_until_equal('\r' as u8);
					match from_utf8(v.as_slice()) {
						Ok(value) => { 
							let tkey = key.trim();
							self.request.headers.insert(tkey.to_string(), value.trim().to_string()); 
							if tkey.as_slice() == "Content-Length" {
								match value.as_slice().parse() {
									Some(len) => { self.request.content_length = len; },
									None => { return false }
								}
							}
						},
						Err(_) => return false
					}
				}
				Err(_) => return false
			}
		}
	}
	fn parse_body(&mut self) -> bool {
		let len = self.request.content_length;
		if len > 0 {
			let v : Vec<Option<u8> > = self.stream
				.bytes().take(self.request.content_length as usize)
				.map(|r| { r.ok() }).collect();
			if v.iter().all(|b| { b.is_some() }) {
				self.request.body = v.iter().map(|b| { b.unwrap() }).collect();
				true
			}
			else { false }
		}
		else { true }
	}
	pub fn run(mut self) -> Option<Request> {
		/*let mut full_buf = Vec::new();
		let mut last_pos = 0u32;
		let mut read_buf = [0u8, 1024];
		let rn_pattern =  ['\r' as u8, 'n' as u8];
		let rnrn_pattern =  ['\r' as u8, 'n' as u8, '\r' as u8, 'n' as u8];
		let mut part_size = 0;
		let mut slice_from = full_buf.as_slice();
		let mut state = State::Method;*/
		if !self.parse_method() { return None }
		println!("Method: {:?}", self.request.method);		
		if !self.parse_url() { return None }
		println!("Path: {}", self.request.path);	
		if !self.parse_proto() { return None }
		println!("Protocol: {}", self.request.protocol);
		if !self.parse_headers() { return None }
		println!("Headers: {:?}", self.request.headers);
		if !self.parse_body() { return None }
		println!("Body: {:?}", self.request.body);
		//if !self.parse_path() { return None }
		Some(self.request)
		/*loop {
			match stream.read(buf) {
				Ok(bytes_read) => {
					for c in buf.slice_to(read_bytes) {
						match state {
							Method if c == ' ' as u8 => { Request.set_method(cur_lex); cur_lex = String::new(); State = Proto; }
							Method => { cur_lex.push(from_utf32(c as u32)) }
							Proto if c == ' ' as u8 => 
						}
					}
				}
				Err(kind) ={

				}
			}
		}*/
	}
	
					/*full_buf.push_all(buf.slice_to(bytes_read));
					match (0..full_buf.len()).find(|ind| => { full_buf.slice_from(ind).starts_from(rnrn_pattern) }) {

					}*/
}

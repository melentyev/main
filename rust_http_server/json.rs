use std::collections::HashMap;
use std::mem;
use std::fmt;
use std::fmt::Show;
use std::fmt::Formatter;

#[derive(Show)]
pub enum JsonNode {
	Null,
	Int(int),
	Float(f64),
	String(String),
	Object(HashMap<String, JsonNode>),
	Array(Vec<JsonNode>),
} 

impl JsonNode {
	pub fn get_string(self) -> Option<String> {
		match self {
			JsonNode::String(s) => Some(s),
			_ => None
		}
	} 
}

pub struct JsonParser<'a> {
	s: &'a str
}

impl<'a> JsonParser<'a> {

	pub fn run(inp: &str) -> Result<JsonNode, String> {
		let mut p = JsonParser {s: inp};
		p.parse()
	}
	pub fn skip_spaces(&mut self) {
		self.s = match self.s.chars().position(|c: char| { !c.is_whitespace() }) { 
			Some(p) => self.s.slice_from(p), 
			None => "" 
		}
	}

	pub fn consume(&mut self, n: uint) {
		self.s = self.s.slice_from(n)
	}

	pub fn parse(&mut self) -> Result<JsonNode, String> {
		println!("parse({})", self.s);
		self.skip_spaces();
		match self.get_next() {
			'{' => { self.consume(1); self.parse_object() },
			'[' => { self.consume(1); self.parse_array() },
			'\"' => { self.parse_string() },
			c if c.is_numeric() => { self.parse_int() },
			c if c == 'n' => { self.parse_null() },
			_ => Err("parse".to_string())
		}
	}
	
	pub fn parse_object(&mut self) -> Result<JsonNode, String> {
		let mut hm = HashMap::new();
		loop {
			match self.parse_string() {
				Ok(s) => { 
					self.skip_spaces(); 
					self.match_next(':');
					self.skip_spaces();
					match (self.parse()) {
						Ok(obj) => { hm.insert(s.get_string().unwrap(), obj); },
						Err(s) => { return Err(s) }
					}
					self.skip_spaces();
					match self.next() {
						'}' => { return Ok(JsonNode::Object(hm)) }
						',' => { ; }
						_ => { return Err("parse_object".to_string()) }
					}
				}
				Err(s) => { return Err(s) }
			}
		}
	}
	pub fn parse_array(&mut self) -> Result<JsonNode, String> {
		let mut a = Vec::new();
		loop {
			match self.parse() {
				Ok(val) => {
					a.push(val);
					self.skip_spaces();
					if self.is_eof() {
						return Err("parse_array".to_string())
					}
					let c = self.next();
					if c != ',' && c != ']' {
						return Err("parse_array".to_string())
					}
					if c == ']' {
						return Ok(JsonNode::Array(a))
					}
				},
				Err(s) => {
					return Err(s)
				}
			}
		}
	}
	
	pub fn parse_int(&mut self) -> Result<JsonNode, String> {
		println!("parse_int({})", self.s);
		self.skip_spaces();
		//while (self.match_next_p(|c| { c.is_numeric()))
		let p = match self.s.chars().position(|c: char| { !c.is_numeric() }) {
			Some(p) => p,
			None => self.s.len()
		};
		match from_str(self.s.slice_to(p)) {
			Some(x) => { self.consume(p); Ok(JsonNode::Int(x)) },
			None => Err("parse_int".to_string())
		}
	}

	pub fn parse_string(&mut self) -> Result<JsonNode, String> {
		println!("parse_string({})", self.s);
		let mut res = String::new();
		let mut escaped = false;
		self.skip_spaces();
		if (!self.match_next('"'))
		{
			return Err("parse_string".to_string());
		}
		loop {
			match self.next_opt() {
				None => { return Err("parse_string".to_string()) }
				Some('\"') if !escaped => { return Ok(JsonNode::String(res)) },
				Some('\\') if !escaped => { escaped = true },
				Some('\\') if escaped => { res.push('\\'); escaped = false },
				Some('n') if escaped => { res.push('\n'); escaped = false },
				Some('t') if escaped => { res.push('\t'); escaped = false },
				Some('r') if escaped => { res.push('\r'); escaped = false },
				Some('\"') if escaped => { res.push('\"'); escaped = false },
				Some(c) => res.push(c)
			}
		}
	}

	pub fn parse_null(&mut self) -> Result<JsonNode, String> {
		self.skip_spaces();
		if (self.match_next('n') && self.match_next('u') 
			&& self.match_next('l') && self.match_next('l')) {
			Ok(JsonNode::Null)
		}
		else {
			Err("parse_null".to_string())
		}
	}

	pub fn get_next(&self) -> char {
		self.s.char_at(0)
	}

	pub fn next(&mut self) -> char {
		let c = self.s.char_at(0);
		self.consume(1);
		c
	}

	pub fn next_opt(&mut self) -> Option<char> {
		if self.s.is_empty() { None } else { Some(self. next()) }
	}

	fn is_eof(&self) -> bool {
		self.s.is_empty()
	}
}


trait MatchChar {
    fn match_next(&mut self, c: char) -> bool;
}
trait MatchPred {
    fn match_next_p(&mut self, pred : |char| -> bool) -> bool;
}

impl<'a> MatchChar for JsonParser<'a> {
	fn match_next(&mut self, c: char) -> bool {
		!self.s.is_empty() && self.next() == c
	}
}
impl<'a> MatchPred for JsonParser<'a> {
	fn match_next_p(&mut self, pred : |char| -> bool) -> bool{
		!self.s.is_empty() && pred(self.next())
	}	
}

/*impl Show for JsonNode {
	fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
		match *self {
			JsonNode::Null => write!(f, "null"),
			JsonNode::Int(x) => write!(f, "Int({})", x),
			JsonNode::Float(x) => write!(f, "Float({})", x),
			JsonNode::String(ref s) => write!(f, "\"{}\"", s),
			JsonNode::Array(ref v) => write!(f, "Array({})", v),
			JsonNode::Object(ref hm) => write!(f, "Object({})", hm)
		};
		Ok(())		
	}
}*/
// Json ::= Int | JString | JObject | JArray
// JObject = '{' eps | KeyValue (',' KeyValue)* '}'
// JArray = '[' Json (',' KeyValue)* ']'
// KeyValue = String ':' Json
/*
fn test_json_object(o: Box<JsonNode>, depth: uint) -> Box<JsonNode> {
	let tabs = String::from_char(depth * 4u, ' ');
	match o {
		box JsonNode::Int(x) => println!("{}{}", tabs, x),
		box JsonNode::JsonObject(m) => { 
			println!("{}{}", tabs, '{');
			for (key, value) in m.iter() {  
				println!("{}", key);
			}
			println!("{}{}", tabs, '}');
		}
		_ => {}
	}
	o
}

pub fn jtest() {
	let mut o1 = HashMap::new();
	o1.insert("key1".to_string(), JsonNode::Int(20));
	let mut jn1 = JsonNode::Object(o1);
	jn1 = test_json_object(jn1, 0);
	test_json_object(jn1, 0);
	println!("hello, world here json");
}
*/
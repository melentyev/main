/*
struct Resource<'a, F> where F: Fn(String, String) {
	path: String,
	rc: F //RequestCallback,
}

struct Service<'a, F> where F: Fn(String, String) {
	resources: Vec<Resource<'a, F>>,
}

fn test_get<'a>(req: String, resp: String) {
	resp.as_slice();
}

fn run<'a, F>() where F: Fn(String, String) {
	//let mut res1 : Resource <'a, F> = Resource { path: "aaa".to_string(), rc: test_get };
	let mut res1 : Resource <'a, F> = Resource { path: "aaa".to_string(), rc: |a: String, b : String| {} };
	let mut s : Service<'a, F> = Service { resources: Vec::new() };
	s.resources.push(res1);
}
*/

struct Resource<'a> {
	path: String,
	rc: Box<Fn(&mut String, &mut String) + 'a> //RequestCallback,
}

struct Service<'a> {
	resources: Vec<Resource<'a>>,
}

fn test_get(req: &mut  String, resp: &mut  String) {
	resp.push('#');
	println!("{:?}{:?}{:?}", req, resp, req);
}

fn run<'a>() {
	//let mut res1 : Resource <'a, F> = Resource { path: "aaa".to_string(), rc: test_get };
	let mut res1 : Resource <'a> = Resource { 
		path: "aaa".to_string(), 
		rc: Box::new(|a: &mut  String, b: &mut String| { println!("{:?}{:?}", a, b); }) };
	let mut res2 : Resource <'a> = Resource { 
		path: "aaa".to_string(), 
		rc: Box::new(test_get) };
	let mut res3 : Resource <'a> = Resource { 
		path: "иии".to_string(), 
		rc: Box::new(test_get) };
	let mut s : Service<'a> = Service { resources: Vec::new() };
	s.resources.push(res1);
	s.resources.push(res2);
	s.resources.push(res3);
	(s.resources[0].rc)(&mut ("aaaa".to_string()), &mut ("bbbb".to_string()));
	(s.resources[1].rc)(&mut ("aaaa".to_string()), &mut ("bbbb".to_string()));
	(s.resources[2].rc)(&mut ("aaaaттт".to_string()), &mut ("bbbb".to_string()));
}


fn main() {
	run();
}
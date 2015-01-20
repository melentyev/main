use http::method::Method;
use http::request::Request; 
use http::response::Response;

//type RequestCallback<'a> = &'a Fn((Request, Response));

#[derive(Show)]
pub struct Resource<'a, F> where F: Fn(&'a mut Request, &'a mut Response) -> () {
	path: String,
	method: Method,
	request_callback: F //RequestCallback,
}

impl<'a, F> Resource<'a, F> where F: Fn(&'a mut Request, &'a mut Response) -> () {
	pub fn init<'r>(path: &str, method: Method, callback: F) -> Resource<'r, F> {
		Resource { path: path.to_string(), method: method, request_callback: callback}
	}
}
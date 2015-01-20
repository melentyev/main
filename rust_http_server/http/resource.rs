use std::sync::Arc;

use http::method::Method;
use http::request::Request; 
use http::response::Response;

//type RequestCallback<'a> = &'a Fn((Request, Response));

pub struct Resource {
	pub path: String,
	pub method: Method,
	request_callback: Box<Fn(&Request, &mut Response) + Send + Sync + 'static>
}

impl Resource {
	pub fn init(path: &str, method: Method, 
		callback: Box<Fn(&Request, &mut Response) + Send + Sync + 'static>) -> Resource
	{
		Resource { path: path.to_string(), method: method, request_callback: callback}
	}
	pub fn handle_request(&self, request: &Request, response: &mut Response) {
		(*self.request_callback)(request, response);
	}
}
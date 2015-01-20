pub mod request;
pub mod method;
pub mod resource;
pub mod service;
pub mod response;
pub mod tcpserver;


#[derive(Show)]
pub enum StatusCode {
	Ok = 200,
	BadRequest = 400,
	NotFound = 404,
}
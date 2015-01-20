use http::resource::Resource;
use http::response::Response;
use http::request::Request;
use http::request::RequestParser;
use http::tcpserver::{TcpServer, TcpServerUserData, ClientRoutine};

use std::sync::Arc;
use std::io::TcpStream;

struct ServiceClientRoutine {
	service: Box<Service>,
}

impl ClientRoutine for ServiceClientRoutine {
	fn run(&self, mut stream: TcpStream) {
		println!("ServiceClientRoutine::ClientRoutine");	
		let response = {
			let parser = RequestParser::init(&mut stream);
			match parser.run() {
				Some(request) => {
					println!("Request: {:?}", request);
					match self.service.find_resource(&request) {
						Some(resource) => {
							let mut response = Response::init();
							resource.handle_request(&request, &mut response);
							response
						},
						None => {
							Response::init().not_found()
						}
					}
				},
				None => Response::init().bad_request()
			}
		};
		println!("Response: {:?}", response);
		stream.write(response.to_buffer().as_slice());	
	}
}

pub struct Service {
	resources: Vec<Resource>,
	port: u16
}

impl Service {
	pub fn init(port: u16) -> Service {
		println!("Service::init({:?})", port);
		Service { port: port, resources: Vec::new() }
	}

	pub fn publish(&mut self, resource: Resource) {
		self.resources.push(resource);
	}

	pub fn start(service: Service) {
		println!("service.start()");
		let port = service.port;
		//let server = TcpServer::init(self.port, Box::new(rest_service_client_routine));
		let routine = Arc::new(ServiceClientRoutine { service: Box::new(service) });
		let server = TcpServer { port: port };//, client_routine: routine };
		server.run(routine);
	}

	pub fn find_resource<'a>(&'a self, request: &Request) -> Option<&'a Resource> {
		self.resources.iter()
			.find(|&: &res| { 
				res.path == request.path && res.method == request.method 
			})
	}
}

//impl<'a> TcpServerUserData for &'a(Service<'a>) {}
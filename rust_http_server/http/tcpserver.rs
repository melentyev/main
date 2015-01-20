use std::io::{TcpListener, TcpStream, Listener, Acceptor};
use std::io::net::tcp::TcpAcceptor;
use std::thread::Thread;
use std::sync::Arc;
//use http::service::{Service, rest_service_client_routine};
use http::service::{Service};

//type ClientRoutine = Box<FnMut((i32, TcpStream)) -> ()>;

pub trait TcpServerUserData {

}

pub trait ClientRoutine: Sized + Send {
	fn run(&self, s: TcpStream);
}

//pub struct TcpServer<'a, F, U> where F: Send + Fn(TcpStream, U), U : Send {
//pub struct TcpServer<U> where U : Send + TcpServerUserData {
pub struct TcpServer {
	pub port: u16,
	//pub client_routine: Box<Fn(&mut TcpStream, Arc<Service>) + Send + 'static>,
	//user_data: U,
}

//impl<'a, F, U> TcpServer<'a, F, U> where F: Send + Fn(TcpStream, U), U : Send {
//impl<U> TcpServer<U> where U : Send + TcpServerUserData {
//	pub fn init(port: u16, client_routine: Box<Fn(TcpStream, U) + Send + 'static>)//, user_data: U)
//		-> TcpServer<U>
impl TcpServer {
	//pub fn init(port: u16, client_routine: Box<Fn(&mut TcpStream, Arc<Service>) + Send + 'static>)//, user_data: U) -> TcpServer
	//	-> TcpServer
	//{
	//	TcpServer { port: port, client_routine: client_routine }//, user_data: user_data }
	//}

	pub fn run<T: ClientRoutine + Sized + Send + Sync>(&self, client_routine: Arc<T>) {//, data: Service) {
		println!("server.run()");
		let listener = TcpListener::bind(("127.0.0.1:".to_string() + self.port.to_string().as_slice()).as_slice()).unwrap();
		let mut acceptor = listener.listen().unwrap();
		//let routine = Arc::new(self.client_routine);
		//let data_arc = Arc::new(data);
		println!("here");
		for stream in acceptor.incoming() {
		    match stream {
		        Ok(stream) => {
		        	//let ut = self.user_tag;
		        	//let serv : &'a TcpServer<F> = self;
		        	//let (send, recv) : (Sender<&'a TcpServer<F>)
			        //let rout : &'a F = &(self.client_routine);
			        //let thread_routine = routine.clone();
			        let thread_routine = client_routine.clone();
			        //let thread_data = data_arc.clone();
			        println!("TcpServer::run::Thread::spawn");
		            Thread::spawn(move || {
		            	thread_routine.run(stream);
		                //rest_service_client_routine(stream, self.user_data)
		                //stream.read()
		            });
		        }
		        Err(e) => { }
		    }
		}
		drop(acceptor);
	}
}
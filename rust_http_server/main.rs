use std::collections::HashMap;
use std::io::{TcpListener, TcpStream};
use std::io::{Acceptor, Listener, BytesReader};
use std::sync::Arc;

use http::request::{Request, RequestParser};
use http::method::{Method};
use http::service::{Service};
use http::resource::{Resource};
use http::response::{Response};
use http::{StatusCode};
mod http;

/*enum State {
		Method,
		Proto_before,
		Proto,
		Proto_before,
		Version,
		Url,
		HeaderField,
		HeaderValue,
		Body,
	};
*/

fn get_method_handler(request : &Request, response : &mut Response) {
    let mut mes = String::new();
    let name = request.get_query_param("name");
    if (name.is_some()) 
    {
    	mes.push_str("<h1>Hello, ");
    	mes.push_str(name.unwrap().as_slice());
    }
    else
    {
        mes.push_str("<h1>It Works!</h1><p>Please, your name?</p>");
    }
    response.set_body(mes.as_slice().as_bytes());
    response.set_status_code(StatusCode::Ok);
    response.set_status_message("");
}

fn get_picture(request : &Request, response : &mut Response) {
    let path = "C:\\Users\\user\\Downloads\\";
    let name = request.get_query_param("name");
    let file = format!("{:?}{:?}", path, name);
}

fn main() {
	let res_get_method_handler = Resource::init("/testget", Method::GET, Box::new(get_method_handler));
	let res_get_picture = Resource::init("/testpic", Method::GET, Box::new(get_picture));

	let mut service = Service::init(1449);
    
    service.publish(res_get_method_handler);
    service.publish(res_get_picture);

    Service::start(service);

	//let k = json::JsonParser::run("  [  123,   53   ,\"asdsad\"    ]");
    //println!("k: {}", k);
}
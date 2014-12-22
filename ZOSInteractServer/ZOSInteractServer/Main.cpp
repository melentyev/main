#include "TcpServer.h"
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <iostream>
#include <iomanip>
#include <string>
#include <algorithm>
#include <set>
#include <functional>
#include "rest_framework.h"

using std::string;

Response get_method_handler(const Request& r)
{
	Response response;
	response.set_body("Password Protected Hello, World!" + r.get_query_param("name"));
	response.status_code = StatusCode::OK;

	return response;
}

int main(int argc, char **argv) 
{
#ifdef WIN32
    WSADATA ws;
    if (FAILED(WSAStartup(MAKEWORD(1, 1), &ws)))
    {
        int error = WSAGetLastError();
    }
#endif

	Resource resource("/resource", "GET", &get_method_handler);

	Service service(1488);
	//service.set_authentication_handler(&authentication_handler);
	service.publish(resource);
	service.start();

    /*unsigned short port = (unsigned short)9990;
    BlockingTcpServer *server = new BlockingTcpServer(port);
    server->Start();
    printf("Server ended successfully\n");
    exit(0);
*/
	
    return 0;
}
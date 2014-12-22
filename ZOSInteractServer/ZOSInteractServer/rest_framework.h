#include "TcpServer.h"
#include "StatusCode.h"
#include <ctype.h>
#include <algorithm>
#include <stdint.h>
#include <functional>
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <sstream>

#include "RestClient.h"

using std::string;
using std::vector;
using std::map;
using std::bind1st;

class StatusCode;
class Response;
class Service;

typedef Response(*resource_callback)(const Request&);

class BaseResource
{
public:
    BaseResource(const string &p_path);
    string path;
};

class Resource : public BaseResource
{
public:
    Resource(const std::string &p_path, const Method& verb,
        resource_callback callback);
    Response handle_request(const Request &request) const;
private:
    Method m_method;
    resource_callback m_callback;
};

class WebSocketResourceCallbacks
{

};

class WebSocketResource : public BaseResource
{
public:
    WebSocketResource(const std::string &p_path, 
        WebSocketResourceCallbacks callbacks);
private:
    WebSocketResourceCallbacks m_callbacks;
};

class Response
{
public:
    Response();
    void set_body(char buf[], int size);
    void set_body(const string& value);
    void set_header(const std::string& name, const std::string& value);
    std::string to_string();
    int status_code;
    std::string status_message;
private:
    std::string m_version;
    std::map<std::string, std::string> headers;
    std::string m_body;
};

class RestServerEvents : public TcpServerEvents
{
public:
    RestServerEvents();
    RestServerEvents(Service *s);
    virtual void client_accepted(int socket);
    virtual bool data_received(int socket, uint8_t data[], int recved);
    virtual bool data_sent(int socket);
    virtual void client_disconnected(int socket);
private:
    Service* m_service;
};

class Service
{
public:
    Service(uint16_t port);
    Service(const Service& original);
    virtual ~Service();
    void start(void);
    //void stop(void);
    void publish(const Resource& value);
    Resource& find_resource(const std::string &path);
    //void suppress(const Resource& value);
    //void set_logger(const Logger& value);

    //void set_authentication_handler(std::function< void(const Request&, Response&) > value);
    //void set_error_handler(std::function< void(const int, const Request&, Response&) > value);

    //Service& operator =(const Service& value);
    friend class RestClient;
private:
    friend class RestServerEvents;
    uint16_t m_port;
    vector<Resource> m_resources;
    RestServerEvents *m_callbacks;
    std::map<int, RestClient*> m_clients;
    NonBlockingTcpServer *m_server;
};
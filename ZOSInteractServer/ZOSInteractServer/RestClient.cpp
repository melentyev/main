#include "TcpServer.h"
#include "RestClient.h"
#include "StringUtil.h"
#include "rest_framework.h"
#include <iostream>
#include <sstream>
#include <istream>
#include <map>

RestClient::RestClient(int socket, Service *service) : m_socket(socket),
    scan_from(0), m_state_ind(0), body_recved(0), m_service(service)
{   
    received_bytes.clear();
    received_string.clear();
    state_seq.clear();
    ClientState st_arr[] = { WAIT_HEADERS_END, PROCESS_REQUEST_BODY };
    state_seq.assign(st_arr, st_arr + 2);
    state = state_seq[m_state_ind];
    m_request = new Request();
}

bool RestClient::body_received(uint8_t data[], int recved)
{
    if (recved + body_recved > m_request->content_length)
    {
        throw "Too many data received";
    }
    std::copy(data, data + recved, back_inserter(m_request->body));
    return recved + body_recved == m_request->content_length;
}

void RestClient::next_state()
{
    state = state_seq[++m_state_ind];
}

bool RestClient::has_request_body()
{
    return m_request->get_method() == "POST";
}

std::string parse_http_method(std::istream& stream)
{
    string method = "";
    stream >> method;
    stream.ignore(1);
    return method;
}

string parse_http_path(std::istream& stream, char &c)
{
    string path = "";
    for (c = stream.get(); c != ' '
        && c != '?'; c = stream.get())
    {
        path.push_back(c);
    }
    return path;
}

void parse_http_query_parameters(std::istream& stream, std::map<string, string> &parameters)
{
    string query_string = "";
    stream >> query_string;
    vector<string> query = StringUtil::split(query_string, "&", true);
    for (vector<string>::iterator it = query.begin(); it != query.end(); it++)
    {
        string::size_type index = (*it).find_first_of('=');
        string name = (*it).substr(0, index);
        string value = (*it).substr(index + 1, (*it).length());
        parameters.insert(make_pair(name, value));
    }
}

std::string parse_http_protocol(std::istream& stream)
{
    string protocol = "";
    for (char c = stream.get(); c != '/'; c = stream.get())
    {
        protocol.push_back(c);
    }
    return StringUtil::trim_whitespace(protocol);
}

std::string parse_http_version(std::istream& stream)
{
    string version = "";
    stream >> version;
    //socket.ignore(2);
    return version;
}

void RestClient::process_request_line_and_headers(string raw)
{
    vector<string> lines = StringUtil::split(raw, StringUtil::http_eol, true);
    std::istringstream ss(lines[0]);
    std::istream& stream = ss;
    std::string method, path;
    char last_char;
    m_request->set_method(parse_http_method(stream));
    m_request->set_path(parse_http_path(stream, last_char));
    if (last_char == '?')
    {
        parse_http_query_parameters(stream, m_request->query_params());
    }
    m_request->protocol() = parse_http_protocol(stream);
    m_request->version() = parse_http_version(stream);
    for (vector<string>::iterator it = lines.begin() + 1; it != lines.end(); it++)
    {
        vector<string> kv = StringUtil::split(*it, ":", false, 2);
        std::transform(kv.begin(), kv.end(), kv.begin(), StringUtil::trim_whitespace);
        if (kv.size() != 2 || kv[0].empty() || kv[1].empty() )
        {
            throw StatusCode::BAD_REQUEST;
        }
        m_request->set_header(kv[0], kv[1]);
        if (kv[0] == StringUtil::content_length)
        {
            sscanf(kv[0].c_str(), "%d", m_request->content_length);
        }
    }
}

void RestClient::request_processing_done()
{
    printf("got_request");
    Resource& resource = m_service->find_resource(m_request->get_path());
    Response resp = resource.handle_request(*m_request);
    string result_str = resp.to_string();
    finished_state();
    m_service->m_server->send(m_socket, result_str.c_str(), result_str.length());

}

RestClient::ClientState RestClient::get_state()
{
    return state;
}

void RestClient::process_request_body_state()
{
    state = PROCESS_REQUEST_BODY;
}

void RestClient::finished_state()
{
    state = FINISHED;
}

RestClient::~RestClient()
{
    delete m_request;
}
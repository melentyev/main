#ifndef _REST_CLIENT_H
#define _REST_CLIENT_H 1

#include <vector>
#include <stdint.h>

#include "Request.h"

using std::string;
using std::vector;

class Service;


class RestClient
{
public:
    enum ClientState
    {
        WAIT_HEADERS_END,
        PROCESS_REQUEST_BODY,
        WEB_SOCK_WAIT_MESSAGE,
        WEB_SOCK_WAIT_MESSAGE_END,
        WEB_SOCK_SENDING_MESSAGE,
        FINISHED
    };
    RestClient(int socket, Service *service);
    vector<uint8_t> received_bytes;
    string received_string;
    ClientState state;
    ClientState get_state();
    void next_state();
    void process_request_body_state();
    void finished_state();
    bool body_received(uint8_t data[], int recved);
    void process_request_line_and_headers(string raw);
    class Parser
    {
    public:
        Parser(string raw, RestClient* owner) : m_raw(raw), m_owner(owner), m_parse_pos(0) {}
        void parse_request_line(string method);
        bool parse_header();
    private:
        int m_parse_pos;
        string m_raw;
        RestClient *m_owner;
    };
    void request_processing_done();
    bool has_request_body();
    ~RestClient();
private:
    int m_socket;
    int scan_from;
    int m_state_ind;
    int body_recved;
    Request *m_request;
    vector<ClientState> state_seq;
    Service *m_service;
};

#endif
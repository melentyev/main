#include "rest_framework.h"
#include "RestClient.h"
#include "StringUtil.h"

RestServerEvents::RestServerEvents(Service *s) : m_service(s), TcpServerEvents()
{
    //
}

RestServerEvents::RestServerEvents() : m_service(NULL), TcpServerEvents()
{
    //
}

void RestServerEvents::client_accepted(int socket)
{
    m_service->m_clients[socket] = new RestClient(socket, this->m_service);
}

bool RestServerEvents::data_received(int socket, uint8_t data[], int recved)
{
    RestClient *cl = m_service->m_clients[socket];
    string headers_end = StringUtil::http_eol + StringUtil::http_eol;
    int pos;
    std::copy(data, data + recved, back_inserter(cl->received_string));    
    switch (cl->state)
    {
    case RestClient::WAIT_HEADERS_END:
        if ((pos = cl->received_string.find(headers_end)) != string::npos)
        {
            cl->process_request_body_state();
            cl->process_request_line_and_headers(cl->received_string.substr(0, pos));
            if (!cl->has_request_body())
            {
                cl->request_processing_done();
            }
        }
        break;
    case RestClient::PROCESS_REQUEST_BODY:
        if (cl->body_received(data, recved))
        {
            cl->request_processing_done();
        }
        break;
    default:
        break;
    }
    return false;
}

bool RestServerEvents::data_sent(int socket)
{
    if (m_service->m_clients[socket]->state == RestClient::FINISHED)
    {
        delete m_service->m_clients[socket];
        m_service->m_clients.erase(socket);
        return true;
    }
    return false;
}

void RestServerEvents::client_disconnected(int socket)
{
    delete m_service->m_clients[socket];
    m_service->m_clients.erase(socket);
}
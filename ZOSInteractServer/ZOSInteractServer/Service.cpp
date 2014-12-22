#include "rest_framework.h"

Service::Service(uint16_t port) : m_port(port)
{
    m_callbacks = new RestServerEvents(this);
}

void Service::publish(const Resource& value)
{
    m_resources.push_back(value);
}

Resource& Service::find_resource(const std::string &path)
{
    for (vector<Resource>::iterator it = m_resources.begin(); it != m_resources.end(); it++)
    {
        if (it->path == path)
        {
            return *it;
        }
    }
    throw StatusCode::NOT_FOUND;
}

void Service::start(void)
{
    m_server = new NonBlockingTcpServer(m_port, m_callbacks);
    m_server->Start();
}

Service::Service(const Service& original)
{
    this->m_port = original.m_port;
}

Service::~Service()
{
    //
}
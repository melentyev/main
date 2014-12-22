#include "rest_framework.h"

BaseResource::BaseResource(const std::string &p_path) : path(p_path)
{
    //
}

Resource::Resource(const std::string &p_path, const Method& verb, 
    resource_callback callback) 
    : BaseResource(p_path), m_method(verb), m_callback(callback)
{
    //
}

Response Resource::handle_request(const Request &request) const
{
    return m_callback(request);
}

WebSocketResource::WebSocketResource(const std::string &p_path, WebSocketResourceCallbacks callbacks) : BaseResource(path), m_callbacks(callbacks)
{
    //
}


#ifndef _TCP_SERVER
#define _TCP_SERVER 1

#define _XOPEN_SOURCE_EXTENDED 1
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <iostream>
#include <iomanip>
#include <string>
#include <algorithm>
#include <set>
#include <vector>
#include <functional>
#include <stdint.h>

#include "StatusCode.h"

#ifndef WIN32                      
#include <sys/types.h>             
#include <sys/socket.h>            
#include <sys/time.h>
#include <sys/ioctl.h>
#include <unistd.h>                
#include <arpa/inet.h>    
#include <netinet/in.h>
#include <net/rtrouteh.h>
#include <net/if.h>

#define SET_NONBLOCK(socket) { int dontblock = 1; return ioctl(socket, FIONBIO, (char *)&dontblock); }
#else               
#include <winsock.h>
#undef max
#include "dummy.h"            
#define SET_NONBLOCK(socket) { DWORD dw = true; return ioctlsocket(socket, FIONBIO, &dw); }
#endif

typedef std::vector<unsigned char> bytes;

class TcpServer
{
protected:
	unsigned short _port;
	int _serv_sock, _max_connections;
	struct sockaddr_in _server;
	virtual void serverLoop() = 0;
public:
	TcpServer(unsigned short port) : _port(port)
	{
		if ((_serv_sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		{
			throw("TcpServer()");
		}
		_max_connections = 5;
	}
	void Start()
	{
		_server.sin_family = AF_INET;
		_server.sin_port = htons(_port);
		_server.sin_addr.s_addr = INADDR_ANY;
		if (bind(_serv_sock, (struct sockaddr *)&_server, sizeof(_server)) < 0)
		{
			throw("Bind()");
		}
		if (listen(_serv_sock, _max_connections) != 0)
		{
			throw("Listen()");
		}
		serverLoop();
	}
	~TcpServer()
	{
		if (_serv_sock >= 0)
		{
			close(_serv_sock);
		}
	}
};

class BlockingTcpServer : public TcpServer
{
public:
	BlockingTcpServer(unsigned short port) : TcpServer(port) {}
protected:
	socklen_t _namelen;
	struct sockaddr_in _client;
	virtual void serverLoop()
	{
		int client_sock;
		char buf[256];
		//while (true)
		//{
		_namelen = sizeof(_client);
		if ((client_sock = accept(TcpServer::_serv_sock,
			(struct sockaddr *)&_client, &_namelen)) == -1)
		{
			throw("Accept()");
		}
		if (recv(client_sock, buf, sizeof(buf), 0) == -1)
		{
			throw("Recv()");
		}
		if (send(client_sock, buf, sizeof(buf), 0) < 0)
		{
			throw("Send()");
		}
		close(client_sock);
		//}
	}
};

class TcpServerEvents
{
public:
    virtual void client_accepted(int socket) = 0;
    virtual bool data_received(int socket, uint8_t data[], int recved) = 0;
    virtual bool data_sent(int socket) = 0;
    virtual void client_disconnected(int socket) = 0;
};

class NonBlockingTcpServer : public TcpServer
{
private:
	static const int BUF_SIZE = 1024;
    TcpServerEvents* m_callbacks;
	inline int set_non_block(int socket)
	{
        SET_NONBLOCK(socket);
	}
    void release_client(int socket)
    {
        close(socket);
        clients.erase(socket);
    }
public:
    NonBlockingTcpServer(unsigned short port, TcpServerEvents *callbacks) 
        : TcpServer(port), m_callbacks(callbacks) {}
    void send(int socket, const char *buf, int size)
    {
        writing_sockets.insert(socket);
        ::send(socket, buf, size, 0);
    }
protected:
	socklen_t _namelen;
	struct sockaddr_in _client;
    std::set<int> clients, writing_sockets;
    char buf[BUF_SIZE];

    bool handle_client_read(int socket)
    {
        int bytes_read = recv(socket, buf, BUF_SIZE, 0);
        if (bytes_read <= 0)
        {
            return true;
        }
        else 
        { 
            try
            {
                return m_callbacks->data_received(socket, (uint8_t*)buf, bytes_read);
            }
            catch (StatusCode::Value code)
            {
                if (code == StatusCode::METHOD_NOT_ALLOWED)
                {
                    std::cerr << "METHOD_NOT_ALLOWED: " + __LINE__ << std::endl;
                }
                else
                {
                    std::cerr << "StatusCode error" + __LINE__ << std::endl;
                }
            }
            catch (...)
            {
                std::cerr << "Unknown exception: " + __LINE__ << std::endl;
            }
            return true;
        }
    }

    bool handle_client_write(int socket)
    {
        try
        {
            return m_callbacks->data_sent(socket);
        }
        catch (...)
        {
            std::cerr << "Unknown exception: " + __LINE__ << std::endl;
            return true;
        }
    }

	virtual void serverLoop()
	{
		clients.clear();
        set_non_block(_serv_sock);
		while (1)
		{
			fd_set readset;
            fd_set writeset;
            FD_ZERO(&readset);
            FD_ZERO(&writeset);
			FD_SET(TcpServer::_serv_sock, &readset);

			for (std::set<int>::iterator it = clients.begin(); it != clients.end(); it++)
			{
				FD_SET(*it, &readset);
                FD_SET(*it, &writeset);
			}
			timeval timeout;
			timeout.tv_sec = 900;
			timeout.tv_usec = 0;

			int mx = clients.empty() ? TcpServer::_serv_sock : std::max(TcpServer::_serv_sock, *max_element(clients.begin(), clients.end()));
			if (select(mx + 1, &readset, &writeset, NULL, &timeout) <= 0)
			{
				throw("select");
			}

			if (FD_ISSET(TcpServer::_serv_sock, &readset))
			{
				int client_sock;
				if ((client_sock = accept(TcpServer::_serv_sock, NULL, NULL)) < 0)
				{
					throw("accept");
				}
                set_non_block(client_sock);

				clients.insert(client_sock);
                m_callbacks->client_accepted(client_sock);
			}

			for (std::set<int>::iterator it = clients.begin(); it != clients.end();)
			{
                if (FD_ISSET(*it, &readset) && handle_client_read(*it))
                {
                    std::set<int>::iterator itold = it++;
                    release_client(*itold);
                    continue;
                }
                else if (FD_ISSET(*it, &writeset) && writing_sockets.count(*it) > 0) 
                {
                    writing_sockets.erase(*it);
                    if (handle_client_write(*it))
                    {
                        std::set<int>::iterator itold = it++;
                        release_client(*itold);
                        continue;
                    }
                }
				it++;
			}
		}
	}
};
#endif
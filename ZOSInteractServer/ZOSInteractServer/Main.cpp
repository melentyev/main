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
#include <functional>

#ifndef WIN32                      
#include <sys/types.h>             
#include <sys/socket.h>            
#include <netinet/in.h>            
#include <unistd.h>                
#include <arpa/inet.h>             
#else                              
#include "dummy.h"                 
#endif                   

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

class NonBlockingTcpServer : public TcpServer
{
    static const int BUF_SIZE = 1024;
    inline int setNonBlock(int socket)
    {
        int dontblock = 1;
        return ioctl(socket, FIONBIO, (char *)&dontblock);
    }
public:
    NonBlockingTcpServer(unsigned short port) : TcpServer(port) {}
protected:
    socklen_t _namelen;
    struct sockaddr_in _client;
    typedef std::set<int> seti;
    virtual void serverLoop()
    {
        char buf[BUF_SIZE];
        seti clients;
        clients.clear();
        while (1)
        {
            // Заполняем множество сокетов
            fd_set readset;
            FD_ZERO(&readset);
            FD_SET(TcpServer::_serv_sock, &readset);

            //std::for_each(clients.begin(), clients.end(), std::bind2nd(FD_SET, &readset));
            for (std::set<int>::iterator it = clients.begin(); it != clients.end(); it++)
            {
                FD_SET(*it, &readset);
            }
            timeval timeout;
            timeout.tv_sec = 600;
            timeout.tv_usec = 0;

            int mx = std::max(TcpServer::_serv_sock, *max_element(clients.begin(), clients.end()));
            if (select(mx + 1, &readset, NULL, NULL, &timeout) <= 0)
            {
                throw("select");
            }

            if (FD_ISSET(TcpServer::_serv_sock, &readset))
            {
                int rc, client_sock;
                if ( (client_sock = accept(TcpServer::_serv_sock, NULL, NULL)) < 0)
                {
                    throw("accept");
                }
                setNonBlock(client_sock);

                clients.insert(client_sock);
            }

            for (std::set<int>::iterator it = clients.begin(); it != clients.end();)
            {
                if (FD_ISSET(*it, &readset))
                {
                    

                    int bytes_read = recv(*it, buf, BUF_SIZE, 0);
                    if (bytes_read <= 0)
                    {
                        std::set<int>::iterator itold = it++;
                        close(*itold);
                        clients.erase(*itold);
                        continue;
                    }
                    send(*it, buf, bytes_read, 0);
                }
                it++;
            }
        }
    }
};

int main(int argc, char **argv) 
{
    unsigned short port = (unsigned short)9990;
    BlockingTcpServer *server = new BlockingTcpServer(port);
    server->Start();
    printf("Server ended successfully\n");
    exit(0);


    return 0;
}
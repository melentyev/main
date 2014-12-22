#ifndef _DUMMY_H
#define _DUMMY_H 1
/*

typedef signed int ssize_t;
int accept(int socket, struct sockaddr *__restrict__address,
    socklen_t *__restrict__address_len);

int socket(int domain, int type, int protocol);
int bind(int socket, const struct sockaddr *address, socklen_t address_len);
int close(int socket);
int listen(int socket, int backlog);
ssize_t recv(int socket, void *buffer, size_t length, int flags);
ssize_t send(int socket, const void *buffer, size_t length, int flags);

typedef int* fd_set;
*/
//struct timeval {
//    int tv_sec;     // секунды
//    int tv_usec;    // микросекунды
//};
/*
int select(int n, fd_set *readfds, fd_set *writefds,
    fd_set *exceptfds, struct timeval *timeout);

int FD_CLR(int fd, fd_set *set);
int FD_ISSET(int fd, fd_set *set);
int FD_SET(int fd, fd_set *set);
int FD_ZERO(fd_set *set);
*/
int fcntl(int socket, int cmd, ...);
int ioctl(int fildes, int cmd, ... /* arg */);

//#define FIONBIO 1

#define close closesocket
typedef int socklen_t;


#endif 
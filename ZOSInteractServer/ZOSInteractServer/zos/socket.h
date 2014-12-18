                   ??=ifdef __COMPILER_VER__
                     ??=pragma filetag ("IBM-1047")
                   ??=endif
                   #pragma nomargins nosequence
                   #pragma checkout(suspend)
/**********************************************************************/
/*                                                                    */
/* Part Name:        socket.h                                         */
/*                                                                    */
/* Invocation Name: #include <socket.h>                               */
/*                                                                    */
/* Descriptive Name: socket header file for C socket library          */
/*                                                                    */
/* Function:         This part is a header file for items that        */
/*                   are internal to the C socket library for HPNS.   */
/*                   It contains structures and defines used for      */
/*                   communicating C socket requests between          */
/*                   TCP/IP and HPNS enabled applications.            */
/*                                                                    */
/* Copyright:                                                         */
/*   Licensed Materials - Property of IBM                             */
/*   This product contains "Restricted Materials of IBM"              */
/*   5694-A01 Copyright IBM Corp. 1991, 2008.                         */
/*   All rights reserved.                                             */
/*   US Government Users Restricted Rights -                          */
/*   Use, duplication or disclosure restricted by GSA ADP Schedule    */
/*   Contract with IBM Corp.                                          */
/*   See IBM Copyright Instructions.                                  */
/*                                                                    */
/* Status:    CSV1R10                                                 */
/*                                                                    */
/* Change Activity:                                                   */
/* CFD List:                                                          */
/*                                                                    */
/* $01= PN79365  HTCP310 960412 JABELL: Add MSG_KILLROUTE flag for    */
/*                                      Kill Route Message support.   */
/* $02= PN89887  HTCP320 961018 JABELL: SYSROUTE of PN83352.  Add IBM */
/*                                      socket option IgnoreSourceVIPA*/
/* $G1= D270.14  CSV2R7  980402 HARISHS: Sysplex Sockets              */
/* $L1= D312.25  CSV1R2  000522 SHAWLEY: TCP_NODELAY                  */
/* $L2= MV22271  CSV1R2  001129 MajieksJ: Bulk mode removal           */
/* $Q1= D316.7   CSV1R5  020612 KMPorter: CICS IPv6 Enhancements      */
/* $A1= PQ65310  CSV1R2  020819 MKDEUSER: IPPROTO_TCP define change   */
/* $A2= PQ66780  HIP6120 021024 VALLER  : Variant characters to octal */
/* $A3= PQ98210  HIP6140 050131 BKelsey : TCP_KEEPALIVE               */
/* $A4= PK15419  HIP6140 051121 BKelsey : Never map names of          */
/*                                        functions that are in       */
/*                                        EZACIC07 or EZACIC17        */
/*                                        (even in C++)               */
/* $Y1= D137554  R9BASE  060516 KMPorter: Add conditional defines.    */
/* $Y2= R9STKMLD CSV1R9  060926 KMPorter: MLDv2 and IGMPv3 support    */
/* $E1= RAAPICPS CSV1R10 070601 SLHUANG : SO_RCVTIMEO & SO_SNDTIMEO   */
/* $E2= D141386  RABASEH 070713 KMPorter: Move inttype.h & in.h back  */
/*                                        to top of header.           */
/*                                                                    */
/* End CFD List:                                                      */
/*                                                                    */
/**********************************************************************/
#ifndef _H_SOCKET
#define _H_SOCKET

/* bsdtypes.h defines FD_SETSIZE. The definition of FD_SETSIZE_MAX  */
/* must be before the include of bsdtypes.h, so that FD_SETSIZE is  */
/* defined to be the maximum number of sockets allowed.             */
#ifndef FD_SETSIZE_MAX
#define FD_SETSIZE_MAX 2000
#endif

#include <bsdtypes.h>
#include <bsdtime.h>
#include <uio.h>

/* @(#)socket.h 2.1 87/03/30 14:57:59 */
/*
 * Copyright (c) 1982,1985, 1986 Regents of the University of
 * California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *      @(#)socket.h    7.1 (Berkeley) 6/4/86
 */

#if defined (__CICS_IPV6) || \
    defined (__CICS_SOCKETS)                        /* @Q1A @Y2C @E2M*/
   #ifndef __inttypes                               /* @Q1A @Y2M @E2M*/
      #include <inttypes.h>                         /* @Q1A @Y2M @E2M*/
   #endif                                           /* @Q1A @Y2M @E2M*/
   #ifndef __netinet_in                             /* @Q1A @Y2M @E2M*/
      #include  <in.h>                              /* @Q1A @Y2M @E2M*/
   #endif                                           /* @Q1A @Y2M @E2M*/
#endif    /* end __CICS_IPV6 || __CICS_SOCKETS */   /* @Q1A @Y2C @E2M*/

/*
 * Definitions related to sockets: types, address families, options.
 */

/*
 * Types
 */
#define SOCK_STREAM     1       /* stream socket */
#define SOCK_DGRAM      2       /* datagram socket */
#define SOCK_RAW        3       /* raw-protocol interface */
#define SOCK_RDM        4       /* reliably-delivered message*/
#define SOCK_SEQPACKET  5       /* sequenced packet stream */

/*
 * Option flags per-socket.
 */
#define SO_DEBUG        0x0001  /* turn on debugging info recording */
#define SO_ACCEPTCONN   0x0002  /* socket has had listen() */
#define SO_REUSEADDR    0x0004  /* allow local address reuse */
#define SO_KEEPALIVE    0x0008  /* keep connections alive */
#define SO_DONTROUTE    0x0010  /* just use interface addresses */
#define SO_BROADCAST    0x0020  /* permit sending of broadcast msgs */
#define SO_USELOOPBACK  0x0040  /* bypass hardware when possible */
#define SO_LINGER       0x0080  /* linger on close if data present */
#define SO_OOBINLINE    0x0100  /* leave received OOB data in line */
#define TCP_NODELAY     0x0001  /* tcp_nodelay                   @L1A*/
#ifdef __CICS_SOCKETS           /* using ezacichd or cmanifes?   @A3A*/
#define TCP_KEEPALIVE   0x0008  /* tcp_keepalive                 @A3A*/
#endif                          /* ezacichd or cmanifes          @A3A*/

/*
 * Additional options, not kept in so_options.
 */
#define SO_SNDBUF       0x1001  /* send buffer size */
#define SO_RCVBUF       0x1002  /* receive buffer size */
#define SO_SNDLOWAT     0x1003  /* send low-water mark */
#define SO_RCVLOWAT     0x1004  /* receive low-water mark */
#define SO_SNDTIMEO     0x1005  /* send timeout */
#define SO_RCVTIMEO     0x1006  /* receive timeout */
#define SO_ERROR        0x1007  /* get error status and clear */
#define SO_TYPE         0x1008  /* get socket type */

/*
 * non-standard sockopts.
 */
#define SO_CLUSTERCONNTYPE 0x4001 /* get cluster connection type @G1A*/

/*
 * Getsockopt(So_ClusterConnType) Output values
 */
#define SO_CLUSTERCONNTYPE_NOCONN        0  /* No connection     @G1A*/
#define SO_CLUSTERCONNTYPE_NONE          1  /* Active connection and */
                                            /* the partner is not in */
                                            /* the same cluster  @G1A*/
#define SO_CLUSTERCONNTYPE_SAME_CLUSTER  2  /* Active connection and */
                                            /* the partner is in the */
                                            /* same cluster      @G1A*/
#define SO_CLUSTERCONNTYPE_SAME_IMAGE    4  /* Active connection and */
                                            /* the partner is in the */
                                            /* same MVS          @G1A*/
#define SO_CLUSTERCONNTYPE_INTERNAL      8  /* Active connection and */
                                            /* the partner is in the */
                                            /* internal cluster  @G1A*/

/*
 * Structure used for manipulating linger option.
 */
struct  linger {
        int     l_onoff;        /* option on/off */
        int     l_linger;       /* linger time */
};

/*
 * Level number for (get/set)sockopt() to apply to socket itself.
 */
#define SOL_SOCKET      0xffff  /* options for socket level */
#define IPPROTO_TCP     6       /* options for tcp/ip level      @A1C*/

#if defined (__CICS_IPV6) || defined (__CICS_SOCKETS)         /* @Y2C*/
  #ifndef __ipproto_ip                                        /* @Y2A*/
    #define __ipproto_ip 1                                    /* @Y2A*/
    #define IPPROTO_IP    0x0000  /* options for ip level        @Q1A*/
  #endif /* end of __ipproto_ip */                            /* @Y2A*/
#endif                                                        /* @Y2A*/

#ifdef __CICS_IPV6                                            /* @Q1A*/
  #ifndef __ipproto_ipv6                                      /* @Y2A*/
    #define __ipproto_ipv6 1                                  /* @Y2A*/
    #define IPPROTO_IPV6  0x0029  /* options for ipv6 level      @Q1A*/
  #endif /* end of __ipproto_ipv6 */                          /* @Y2A*/
#endif /* End __CICS_IPV6 */                                  /* @Q1A*/

/*
 * Address families.
 */
#define AF_UNSPEC       0       /* unspecified */
#define AF_UNIX         1       /* local to host (pipes, portals) */
#define AF_INET         2       /* internetwork: UDP, TCP, etc. */
#define AF_IMPLINK      3       /* arpanet imp addresses */
#define AF_PUP          4       /* pup protocols: e.g. BSP */
#define AF_CHAOS        5       /* mit CHAOS protocols */
#define AF_NS           6       /* XEROX NS protocols */
#define AF_NBS          7       /* nbs protocols */
#define AF_ECMA         8       /* european computer manufacturers */
#define AF_DATAKIT      9       /* datakit protocols */
#define AF_CCITT        10      /* CCITT protocols, X.25 etc */
#define AF_SNA          11      /* IBM SNA */
#define AF_DECnet       12      /* DECnet */
#define AF_DLI          13      /* Direct data link interface */
#define AF_LAT          14      /* LAT */
#define AF_HYLINK       15      /* NSC Hyperchannel */
#define AF_APPLETALK    16      /* Apple Talk */
#define AF_IUCV         17      /* IBM IUCV */

#define AF_MAX          18

#ifdef __CICS_IPV6                                            /* @Q1A*/
#define AF_INET6        19      /* IPv6 */                    /* @Q1A*/
#endif  /* End __CICS_IPV6 definitions  */                    /* @Q1A*/

#if defined (__CICS_SOCKETS)                                  /* @Y2A*/
  #ifndef __sa_family_t                                       /* @Y2A*/
     #define __sa_family_t  1                                 /* @Y2A*/
     typedef unsigned char sa_family_t;                       /* @Y2A*/
  #endif                                                      /* @Y2A*/
  #ifndef __sa_family_t                                       /* @Y2A*/
     #define __sa_family_t  1                                 /* @Y2A*/
     typedef unsigned char sa_family_t;                       /* @Y2A*/
  #endif                                                      /* @Y2A*/
  #ifndef __sockaddr                                          /* @Y2A*/
    #define __sockaddr 1                                      /* @Y2A*/
    struct sockaddr {                                         /* @Y2A*/
        unsigned char sa_len;                                 /* @Y2A*/
        sa_family_t   sa_family;                              /* @Y2A*/
        char          sa_data[14];  /* variable length data *//* @Y2A*/
    };                                                        /* @Y2A*/
  #endif                                                      /* @Y2A*/
#else                                                         /* @Y2A*/
/*
 * Structure used by kernel to store most
 * addresses.
 */
struct sockaddr {
        u_short sa_family;      /* address family */
        char    sa_data[14];    /* up to 14 bytes of direct address */
};
#endif /* end of __CICS_SOCKETS */                            /* @Y2A*/

/*
 * Structure used by kernel to pass protocol
 * information in raw sockets.
 */
struct sockproto {
        u_short sp_family;      /* address family */
        u_short sp_protocol;    /* protocol */
};

/*
 * Protocol families, same as address families for now.
 */
#define PF_UNSPEC       AF_UNSPEC
#define PF_UNIX         AF_UNIX
#define PF_INET         AF_INET
#define PF_IMPLINK      AF_IMPLINK
#define PF_PUP          AF_PUP
#define PF_CHAOS        AF_CHAOS
#define PF_NS           AF_NS
#define PF_NBS          AF_NBS
#define PF_ECMA         AF_ECMA
#define PF_DATAKIT      AF_DATAKIT
#define PF_CCITT        AF_CCITT
#define PF_SNA          AF_SNA
#define PF_DECnet       AF_DECnet
#define PF_DLI          AF_DLI
#define PF_LAT          AF_LAT
#define PF_HYLINK       AF_HYLINK
#define PF_APPLETALK    AF_APPLETALK
#define PF_IUCV         AF_IUCV

#define PF_MAX          AF_MAX

#ifdef __CICS_IPV6                                            /* @Q1A*/
/*
 * Additional protocol families
 */                                                           /* @Q1A*/
#define PF_INET6        AF_INET6                              /* @Q1A*/
#endif  /* End __CICS_IPV6 definitions  */                    /* @Q1A*/

/*
 * Portability Additions
 */                                                           /* @Q1A*/
#if defined (__CICS_IPV6) || \
    defined (__CICS_SOCKETS)                             /* @Q1A @Y2C*/
  /*
   * Desired design of maximum size and alignment for
   * sockaddr_storage structure
   */                                                         /* @Q1A*/
  #define _SS_MAXSIZE    128  /* platform specific max size    * @Q1A*/
  #define _SS_ALIGNSIZE  8    /* platform specific alignment   * @Q1A*/

  /*
   * Definitions used for sockaddr_storage structure paddings
   * design
   */                                                         /* @Q1A*/
  #define _SS_PAD1SIZE                                        \
                (_SS_ALIGNSIZE - (sizeof(uint8_t)+            \
                       sizeof(sa_family_t)))                  /* @Q1A*/
  #define _SS_PAD2SIZE                                        \
                (_SS_MAXSIZE   - (sizeof(uint8_t)+            \
                       sizeof(sa_family_t)+                   \
                       _SS_PAD1SIZE+                          \
                       _SS_ALIGNSIZE))                        /* @Q1A*/

  /*
   * Structure allows applications to allocate space for
   * either form of inet socket address
   */                                                         /* @Q1A*/
  struct sockaddr_storage {                                   /* @Q1A*/
      uint8_t      ss_len;          /* address length          * @Q1A*/
      sa_family_t  ss_family;       /* address family          * @Q1A*/
               /* following fields are implementation specific * @Q1A*/
      char         __ss_pad1[_SS_PAD1SIZE];                   /* @Q1A*/
               /* 6 byte pad, this is to make implementation   * @Q1A*/
               /* specific pad up to alignment field that      * @Q1A*/
               /* follows explicit in the data structure       * @Q1A*/
      #if (defined(__LL) || defined(_LP64))                   /* @Q1A*/
      int64_t      __ss_align;                                /* @Q1A*/
               /* storage alignment                            * @Q1A*/
      #else                                                   /* @Q1A*/
      double       __ss_align;                                /* @Q1A*/
               /* storage alignment                            * @Q1A*/
      #endif                                                  /* @Q1A*/
      char         __ss_pad2[_SS_PAD2SIZE];                   /* @Q1A*/
               /* 112 byte pad to achieve desired size         * @Q1A*/
  };                                                          /* @Q1A*/
#endif /* End __CICS_IPV6 || __CICS_SOCKETS */           /* @Q1A @Y2C*/

/*
 * Maximum queue length specifiable by listen.
 */
#define SOMAXCONN       10

/*
 * Message header for recvmsg and sendmsg calls.
 */
struct msghdr {
        caddr_t msg_name;       /* optional address */
        int     msg_namelen;    /* size of address */
        struct  iovec *msg_iov; /* scatter/gather array */
        int     msg_iovlen;     /* # elements in msg_iov */
        caddr_t msg_accrights;  /* access rights sent/received */
        int     msg_accrightslen;
};

/*
 * Client identification structure, used in getclientid(),
 * givesocket(), and takesocket() calls
 */
struct clientid {
   int domain;
   char name[8];
   char subtaskname[8];
   char reserved[20];
};

#define MSG_OOB         0x1     /* process out-of-band data */
#define MSG_PEEK        0x2     /* peek at incoming message */
#define MSG_DONTROUTE   0x4     /* send without using routing tables */
#define MSG_KILLROUTE   0x8     /* send a kill route message     @01A*/
#define MSG_WAITALL     0x40    /* wait for all data             @E1A*/

#define MSG_MAXIOVLEN   16

/*
 * Options for getibmsockopt(), setibmsockopt()
 */
#define SO_IGNOREINCOMINGPUSH 0x1  /* Push doesn't matter to sockets
                                      anyway, right?  Actually, this
                                      option makes TCP delay response
                                      to socket read until length is
                                      satisfied and/or internal buffer
                                      limit is reached and/or FIN is
                                      received.  To be safe, don't
                                      count on read ending until FIN
                                      received. */
#define SO_IGNORESOURCEVIPA   0x2  /* Ignore source VIPA addrs   @02A*/
#define SO_BULKMODE         0x8000 /* set/get options for IBM        */
                                   /* bulkmode sockets.              */
#define SO_NONBLOCKLOCAL    0x8001 /* don't block if local queue is  */
                                   /* empty.                         */

/*
 * Structure used for manipulating IBM bulkmode datagram sockets.
 */
struct  ibm_bulkmode_struct {
        int     b_onoff;          /* Option on/off                   */
        int     b_max_receive_queue_size; /* Maximum receiving queue */
                                          /* size (in bytes).        */
        int     b_max_send_queue_size; /* Maximum sending queue size */
                                       /* (in bytes).                */
        int     b_move_data;      /* For outbound sockets:           */
           /* non-zero: The data is moved into buffers in the queue. */
           /*           The client's buffers can be reused right away*/
           /* zero:     Pointers to the data are saved in the queue. */
           /*           The buffers should not be reused until the   */
           /*           queue has been flushed (generally by issuing */
           /*           an ibmsflush()).                             */
        int b_teststor;    /* For either inbound or outbound sockets:*/
           /* non-zero: The address of the message buffer and the    */
           /*           message buffer itself is checked for address-*/
           /*           ability during each socket call.  The error, */
           /*           EFAULT, is set if there is an exception.     */
           /* zero:     The above checking is not done by the socket */
           /*           library routines. If there is an addressing  */
           /*           exception, the normal runtime error handling */
           /*           environment will produce a message.  For     */
           /*           examples of this, see the BULKMODE.README    */
           /*           file.                                        */
           /*                                                        */
           /* The following are returned by getibmsockopt():         */
        int b_max_send_queue_size_avail; /* The max. send queue size */
                                  /* (in bytes) that we can set by   */
                                  /* b_max_send_queue_size option on */
                                  /* setibmsockopt().                */
        int b_num_IUCVs_sent;     /* Number of actual IUCVs issued in*/
                                  /* sending datagrams to TCPIP.     */
        int b_num_IUCVs_received; /* Number of actual IUCVs issued in*/
                                  /* receiving datagrams from TCPIP. */
};

/*-------------------------------------------------------------------*/
/* Structures used by the SETIBMOPT() and GETIBMOPT() calls          */
#define IBMTCP_IMAGE 1
struct ibm_tcpimage {
    unsigned short status;              /* TCPIP status              */
    unsigned short version;             /* TCPIP version             */
    char             name[8];           /* TCPIP name, blank padded  */
};
struct ibm_gettcpinfo {
    int tcpcnt;                         /* number of active TCPIPs   */
    struct ibm_tcpimage image[8];       /* up to 8 tcpimage structs  */
};

/*-------------------------------------------------------------------*/
/* Prototypes for all socket functions.                              */
/*-------------------------------------------------------------------*/

#ifndef _TCP31_PROTOS
#ifdef __cplusplus
extern "C" {
#endif
int accept(int s, struct sockaddr *addr, int *addrlen);
int bind(int s, struct sockaddr *name, int namelen);
int close(int s);
int connect(int s, struct sockaddr *name, int namelen);
void endhostent(void);
void endnetent(void);
void endprotoent(void);
void endservent(void);
int fcntl(int s, int cmd, int arg);
int getclientid(int domain, struct clientid *clientid);
int getdtablesize(void);
/*------------------------------------------------------------------*/
/* The following 12 are prototyped in netdb.h                       */
/* struct hostent *gethostbyaddr(char *addr, int addrlen,           */
/*                               int domain);                       */
/* struct hostent *gethostbyname(char *name);                       */
/* struct hostent *gethostent(void);                                */
/* struct netent *getnetbyaddr(unsigned long net, int type);        */
/* struct netent *getnetbyname(char *name);                         */
/* struct netent *getnetent(void);                                  */
/* struct protoent *getprotobyname(char *name);                     */
/* struct protoent *getprotobynumber(int proto);                    */
/* struct protoent *getprotoent(void);                              */
/* struct servent *getservbyname(char *name, char *proto);          */
/* struct servent *getservbyport(int port, char *proto);            */
/* struct servent *getservent(void);                                */
/* The previous 12 are prototyped in netdb.h                        */
/*------------------------------------------------------------------*/
unsigned long   gethostid(void);
int gethostname(char *name, int namelen);
int getibmopt(int cmd, struct ibm_gettcpinfo *buf);
int getibmsockopt(
        int s, int level, int optname, char *optval, int *optlen);
int getpeername(int s, struct sockaddr *name, int *namelen);
int getsockname(int s, struct sockaddr *name, int *namelen);
int getsockopt(
        int s, int level, int optname, char *optval, int *optlen);
int givesocket(int d, struct clientid *clientid);
int ibmsflush(int s);
/*------------------------------------------------------------------*/
/* The following 6 are prototyped in inet.h                         */
/* unsigned long inet_addr(char *cp);                               */
/* unsigned long inet_lnaof(struct in_addr in);                     */
/* struct in_addr inet_makeaddr(unsigned long net,                  */
/*                              unsigned long lna);                 */
/* unsigned long inet_netof(struct in_addr in);                     */
/* unsigned long inet_network(char *cp);                            */
/* char *inet_ntoa(struct in_addr in);                              */
/* The previous 4 are prototyped in inet.h                          */
/*------------------------------------------------------------------*/
int ioctl(int s,unsigned long cmd, char *arg);
int listen(int s, int backlog);
int maxdesc(int *totdesc, int *inetdesc);
int read(int s,char *buf, int len);
int readv(int s, struct iovec *iov, int iovcnt);
int recv(int s,char *buf, int len, int flags);
int recvfrom(int s,char *buf, int len, int flags,
        struct sockaddr *name, int *namelen);
int recvmsg(int s, struct msghdr msg[], int flags);
int select(int nfds, fd_set *readfds, fd_set *writefds,
        fd_set *exceptfds, struct timeval *timeout);
int selectex(int nfds, fd_set *readfds, fd_set *writefds,
        fd_set *exceptfds, struct timeval *timeout, int *ecbptr);
int send(int s, char *msg, int len, int flags);
int sendmsg(int s, struct msghdr msg[], int flags);
int sendto(int s, char *msg, int len, int flags,
        struct sockaddr *to, int totlen);
int sethostent(int stayopen);
int setibmopt(int cmd, struct ibm_tcpimage *buf);
int setibmsockopt(
        int s, int level, int optname, char *optval, int optlen);
int setnetent(int stayopen);
int setprotoent(int stayopen);
int setservent(int stayopen);
int setsockopt(
        int s, int level, int optname, char *optval, int optlen);
int shutdown(int s, int how);
void sock_debug(int onoff);
int  sock_debug_bulk_perf0(int onoff);                        /*@L2C*/
int  sock_do_bulkmode(int onoff);                             /*@L2C*/
void sock_do_teststor(int onoff);
int socket(int domain, int type, int protocol);
int takesocket(struct clientid *clientid, int hisdesc);
void tcperror(const char *string);  /* just like perror()   */
char *tcpserror(int errnum);        /* just like strerror() */
int write(int s, char *buf, int len);
int writev(int s, struct iovec *iov, int iovcnt);
#ifdef __cplusplus
int _gethostname(char *cp, int len);
}
                                                             /* 4@A4D*/
/*-----------------------------------------------------------*/
/* Special definitions to force name resolution by           */
/*     table lookup.                                         */
/*-----------------------------------------------------------*/
#ifdef RESOLVE_VIA_LOOKUP
#pragma map(endhostent,           "\174ENDHTEN")              /* @A2C*/
#pragma map(sethostent,           "\174SETHTEN")              /* @A2C*/
#endif
#pragma map(endnetent,            "ENDNETEN")
#pragma map(endprotoent,          "ENDPROTO")
#pragma map(endservent,           "ENDSERVE")
                                                             /* 2@A4D*/
#pragma map(getdtablesize,        "GETDTABL")
                                                             /* 2@A4D*/
#pragma map(getibmopt,            "GETIBMOP")
#pragma map(getibmsockopt,        "GETIBMSO")
                                                             /* 4@A4D*/
#pragma map(ibmsflush,            "IB\133SFLUS")              /* @A2C*/
                                                             /* 2@A4D*/
#pragma map(maxdesc,              "MAXDESC")
                                                            /* 10@A4D*/
/* See RESOLVE_VIA_LOOKUP section for sethostent */
#pragma map(setibmopt,            "SETIBMOP")
#pragma map(setibmsockopt,        "SETIBMSO")
#pragma map(setnetent,            "SETNETEN")
#pragma map(setprotoent,          "SETPROTO")
#pragma map(setservent,           "SETSERVE")
                                                             /* 2@A4D*/
#pragma map(sock_debug,           "SOCK\174DEB")              /* @A2C*/
#pragma map(sock_debug_bulk_perf0,"SOCKDBP0")
#pragma map(sock_do_bulkmode,     "SOCKDOBM")
#pragma map(sock_do_teststor,     "SOCKDOTS")
                                                             /* 2@A4D*/
#pragma map(tcperror,             "TCPERROR")
#pragma map(tcpserror,            "TCPSERRO")
                                                             /* 2@A4D*/
#pragma map(_gethostname,         "\174GETHOST")              /* @A2C*/
#endif /* __cplusplus */
#endif /* _TCP31_PROTOS */
#endif
#pragma checkout(resume)

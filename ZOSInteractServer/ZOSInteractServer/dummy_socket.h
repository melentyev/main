#define __socket_h 1
#pragma nomargins nosequence
#pragma checkout(suspend)
/***************************************************************
*  <sys/socket.h> header file                                  *
*                                                              *
* LICENSED MATERIALS - PROPERTY OF IBM                         *
*                                                              *
* 5694-A01                                                     *
*                                                              *
*     COPYRIGHT IBM CORP. 1988, 2008                           *
*                                                              *
* US GOVERNMENT USERS RESTRICTED RIGHTS - USE,                 *
* DUPLICATION OR DISCLOSURE RESTRICTED BY GSA ADP              *
* SCHEDULE CONTRACT WITH IBM CORP                              *
*                                                              *
* STATUS = HLE7750                                             *
***************************************************************/

/*** IBMCOPYR ********************************************************/
/*                                                                   */
/* Copyright:                                                        */
/*   Licensed Materials - Property of IBM                            */
/*   This product contains "Restricted Materials of IBM"             */
/*   5735-FAL     Copyright IBM Corp. 1991.                          */
/*   5735-HAL     Copyright IBM Corp. 1991.                          */
/*   All rights reserved.                                            */
/*   US Government Users Restricted Rights -                         */
/*   Use, duplication or disclosure restricted by GSA ADP Schedule   */
/*   Contract with IBM Corp.                                         */
/*   See IBM Copyright Instructions.                                 */
/*                                                                   */
/* Status:                                                           */
/*   Version 2, Release 1, Level 0                                   */
/*                                                                   */
/*** IBMCOPYR ********************************************************/

/*
* Copyright (c) 1982,1985, 1986 Regents of the University of
* California.
* All rights reserved.  The Berkeley software License Agreement
* specifies the terms and conditions for redistribution.
*
*      @(#)socket.h    7.1 (Berkeley) 6/4/86
*/

#define __UU
#ifdef __cplusplus
extern "C" {
#endif


#ifndef __EDC_LE
#define __EDC_LE 0x10000000
#endif

#if __TARGET_LIB__ >= __EDC_LE

#if !defined(__features_h)  || defined(__inc_features)
#include <features.h>
#endif


#if defined(_OE_SOCKETS) || \
    defined(__UU) || \
    defined(__SUSV3_POSIX)

#ifndef __types
#include <sys/types.h>
#endif

#ifndef __sys_uio
#include <sys/uio.h>
#endif

#if defined(__IPV6) || defined(__OE_9)
#ifndef __inttypes
#include <inttypes.h>
#endif
#endif /* __IPV6 or __OE_9 */

#ifdef __SUSV3_XSI
#ifndef __int64_t
#define  __int64_t  1

#if defined(_LP64)
    typedef    signed  long   int64_t; /* 64-bit signed    *
                                       * integral type    */
#elif defined(__LL)
    typedef signed long long  int64_t; /* 64-bit signed    *
                                       * integral type    */
#endif
#endif /* __int64_t */

#ifndef  __uint8_t
#define  __uint8_t  1
    typedef  unsigned  char  uint8_t;
#endif /* __uint8_t */
#endif /* __SUSV3_XSI */

#if __EDC_TARGET >= 0X22070000
#ifndef __socklen_t

#define __socklen_t  1
    typedef unsigned int socklen_t;

#endif
#endif /* __EDC_TARGET >= 0X22070000 */

    /*
    * Definitions related to sockets: types, address families,
    *  options.
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
    * Additional socket types
    */

#if __EDC_TARGET >= 0X22070000
#define SOCK_CONN_DGRAM 6        /* connection datagram */
#endif /* __EDC_TARGET >= 0X22070000*/

    /*
    * Option flags per-socket.
    */
#define SO_DEBUG        0x0001  /* turn on debugging info
    * recording */
#define SO_ACCEPTCONN   0x0002  /* socket has had listen() */
#define SO_REUSEADDR    0x0004  /* allow local address reuse */
#define SO_KEEPALIVE    0x0008  /* keep connections alive */
#define SO_DONTROUTE    0x0010  /* just use interface addresses*/
#define SO_BROADCAST    0x0020  /* permit sending of broadcast
    * msgs */
#define SO_USELOOPBACK  0x0040  /* bypass hardware when
    * possible */
#define SO_LINGER       0x0080  /* linger on close if data
    * present */
#define SO_OOBINLINE    0x0100  /* leave received OOB data in
    * line */

    /*
    * setpeer options
    */
#define SO_SET          0x0200  /* register preset address */
#define SO_UNSET        0x0400  /* de-register preset address */

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
#define SO_ACKNOW       0x7700  /* setsockopt() no-op value */

#if __EDC_TARGET >= 0X22070000
#define SO_CLUSTERCONNTYPE  0x00004001
#if __EDC_TARGET >=  __EDC_LE210
#define SO_SECINFO    0x00004002 /* Sender's Security Info
    Ancillary Data         */
#endif                            /* end __EDC_TARGET       */

    /*
    * SO_CLUSTERCONNTYPE Output Values
    */
#define SO_CLUSTERCONNTYPE_NOCONN         0
#define SO_CLUSTERCONNTYPE_NONE           1
#define SO_CLUSTERCONNTYPE_SAME_CLUSTER   2
#define SO_CLUSTERCONNTYPE_SAME_IMAGE     4
#define SO_CLUSTERCONNTYPE_INTERNAL       8

#if __EDC_TARGET >=  __EDC_LE210
#define SO_EioIfNewTP 5    /* Application requested notify if
    a new transport provider started */
#endif                            /* end __EDC_TARGET       */

#define SO_NOREUSEADDR  0x1000    /* prevent local address
    reuse */
#define SO_REUSEPORT    0x0200    /* allow local address & port
    reuse */
#define SO_CKSUMRECV    0x0800    /* defer checksum until
    receive */

#define SO_USE_IFBUFS   0x0400    /* Interface will supply
    buffers */

    /*
    * Additional non-standard options, not kept in so_options.
    */

#define _SO_PROPAGATEUSERID  0x4000 /* propagate userid */
#endif /* __EDC_TARGET >= 0X22070000*/

#ifdef __OE_3

    /*
    * Options for getibmsockopt(), setibmsockopt()
    */
#define SO_IGNOREINCOMINGPUSH 0x1  /* Delay response to socket */
    /* read until length is     */
    /* satisfied and/or internal*/
    /* buffer limit is reached  */
    /* and/or FIN is received.  */
#define SO_BULKMODE      0x8000    /* Set/get options for IBM  */
    /*  bulkmode sockets.       */
#define SO_NONBLOCKLOCAL 0x8001    /* Don't block if local     */
    /* queue is empty.          */
#define SO_CLOSE         0x01      /* used in clientid struct  */
#define _SO_SELECT       0x02      /* used in clientid struct  */
#define SO_IGNORESOURCEVIPA 0x0002 /* Ignore source IP addr    */
#define SO_OPTMSS           0x8003 /* Use of optimal segment   */
    /* size                     */
#define SO_OPTACK           0x8004 /* Use of optimal acknowlege*/

#if __EDC_TARGET >= __EDC_LE4105
#define SO_ACCEPTECONNABORTED 0x0006
    /* socket connection aborted */
#endif

    /* For C++, use pragma pack */
#ifdef __cplusplus
    /*
    * Client identification structure, used in getclientid(),
    * __getclientid(), givesocket(), and takesocket() calls
    */
    struct clientid {
        int domain;
        union  {
            char name[8];
            struct {
                int NameUpper;
                pid_t pid;
            } c_pid;
        } c_name;
        char subtaskname[8];

#pragma pack(1)
        struct  {
            char type;
            union  {
                char specific[19];
                struct  {
                    char unused[3];
                    int SockToken;
                } c_close;
            } c_func;
        } c_reserved;
#pragma pack(reset)
    };

    /* For C, use _Packed */
#else
    /*
    * Client identification structure, used in getclientid(),
    * __getclientid(), givesocket(), and takesocket() calls
    */
    struct clientid {
        int domain;
        union  {
            char name[8];
            struct {
                int NameUpper;
                pid_t pid;
            } c_pid;
        } c_name;
        char subtaskname[8];

        _Packed struct  {
            char type;
            union  {
                char specific[19];
                _Packed struct  {
                    char unused[3];
                    int SockToken;
                } c_close;
            } c_func;
        } c_reserved;
    };
#endif

#if __EDC_TARGET >=  __EDC_LE210
    /* used in recvmsg()
    *  SO_SECINFO Sender's Security Info Ancillary Data
    *     Level=SOL_SOCKET, Type=SO_SECINFO
    */
    struct __secsinfo  {
        uid_t  secseuid;        /* POSIX Effective UID        */
        gid_t  secsegid;        /* POSIX Effective GID        */
        char   secsuserid[9];   /* MVS User ID, null padded   */
        char   rsvd1;
        char   secsjobname[9];  /* MVS Job Name, blank padded */
        /*   out to 8 characters with */
        /*   a null in the 9th byte.  */
        char   rsvd2;
        char   rsvd3[16];
    };
#endif                            /* end __EDC_TARGET       */
    /*
    *  Structures uded by the setibmopt() and getibmopt() calls
    */
#define IBMTCP_IMAGE 1

    struct ibm_tcpimage {
        unsigned short status;        /* TCPIP status              */
        unsigned short version;       /* TCPIP version             */
        char             name[8];     /* TCPIP name, blank padded  */
    };

    struct ibm_gettcpinfo {
        int tcpcnt;                   /* number of active TCPIPs   */
        struct ibm_tcpimage image[8]; /* up to 8 tcpimage structs  */
    };

    /*
    * Structure used for manipulating IBM bulkmode datagram sockets.
    */
    struct ibm_bulkmode_struct {
        int b_onoff;          /* 1 indicates bulk mode is on;        */
        /* 0 indicates bulk mode is off.       */
        int b_max_receive_queue_size; /* The maximum receiving queue */
        /* size (in bytes).                    */
        int b_max_send_queue_size; /* The maximum sending queue size */
        /* (in bytes).                         */
        int b_move_data;      /* For outbound sockets:               */
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
        /*           environment will produce a message.          */
        /*                                                        */
        int b_max_send_queue_size_avail; /* The max. send queue size */
        /* (in bytes) that can be set by   */
        /* b_max_send_queue_size option on */
        /* setibmsockopt().                */
        int b_num_UNITs_sent;     /* Number of actual UNITs issued   */
        /* in sending datagrams to TCPIP.  */
        int b_num_UNITs_received; /* Number of actual UNITs issued   */
        /* in receiving datagrams from     */
        /* TCPIP.                          */
    };


#endif   /* __OE_3 */

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

    /*
    * Address families.
    */
#define AF_UNSPEC       0   /* unspecified */
#define AF_UNIX         1   /* local to host (pipes, portals)*/
#define AF_INET         2   /* internetwork: UDP, TCP, etc. */
#define AF_IMPLINK      3   /* arpanet imp addresses */
#define AF_PUP          4   /* pup protocols: e.g. BSP */
#define AF_CHAOS        5   /* mit CHAOS protocols */
#define AF_NS           6   /* XEROX NS protocols */
#define AF_NBS          7   /* nbs protocols */
#define AF_ECMA         8   /* european computer manufacturers */
#define AF_DATAKIT      9   /* datakit protocols */
#define AF_CCITT        10  /* CCITT protocols, X.25 etc */
#define AF_SNA          11  /* IBM SNA */
#define AF_DECnet       12  /* DECnet */
#define AF_DLI          13  /* Direct data link interface */
#define AF_LAT          14  /* LAT */
#define AF_HYLINK       15  /* NSC Hyperchannel */
#define AF_APPLETALK    16  /* Apple Talk */
#define AF_IUCV         17  /* IBM IUCV */

#define AF_MAX          18


#if __EDC_TARGET >= 0X22070000
    /*
    * Additional address families
    */
#define AF_ISO          AF_NBS

#define AF_LINK         18         /* Link layer interface */
#define AF_INTF         20         /* Debugging use only */
#define AF_RIF          21         /* raw interface */
#define AF_NETWARE      22
#define AF_NDD          23
#undef  AF_MAX
#define AF_MAX          30

#endif /* __EDC_TARGET >= 0X22070000*/

#if defined(__IPV6) || defined(__SUSV3_XSI)

#define AF_INET6        19         /* IPv6 */
#define AF_ROUTE        20         /* Internal Routing
    Protocol */

#endif  /* __IPV6 || __SUSV3_XSI  */

    /*
    * sockaddr structure
    */

#ifdef _OE_SOCKETS

#ifndef __sockaddr

#define __sockaddr 1
    struct sockaddr {
        unsigned char sa_len;
        unsigned char sa_family;
        char          sa_data[14];    /* variable length data */
    };
#endif

#else

#ifndef __sa_family_t

#define __sa_family_t  1
    typedef unsigned char sa_family_t;

#endif

#ifndef __sockaddr

#define __sockaddr 1
    struct sockaddr {
        unsigned char sa_len;
        sa_family_t   sa_family;
        char          sa_data[14];    /* variable length data */
    };
#endif

#endif       /* _OE_SOCKETS  */

#if defined(_OE_SOCKETS)

    /*
    * Structure used by kernel to pass protocol
    * information in raw sockets.
    */
#if !defined(__u_short)
#define __u_short   1
    typedef  unsigned short u_short;
#endif
    struct sockproto {
        u_short sp_family;      /* address family */
        u_short sp_protocol;    /* protocol */
    };

#endif  /* defined(_OE_SOCKETS)  */

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

#ifdef __IPV6
    /*
    * Additional protocol families
    */
#define PF_ROUTE        AF_ROUTE
#define PF_INET6        AF_INET6
#endif  /* End __IPV6 definitions  */

    /*
    * Portability Additions
    */
#if defined(__IPV6) || defined(__OE_9) || \
    defined(__SUSV3_XSI)
    /*
    * Desired design of maximum size and alignment for
    * sockaddr_storage structure
    */
#define _SS_MAXSIZE    128  /* platform specific max size    */
#define _SS_ALIGNSIZE  8    /* platform specific alignment   */

    /*
    * Definitions used for sockaddr_storage structure paddings
    * design
    */
#define _SS_PAD1SIZE                                        \
    (_SS_ALIGNSIZE - (sizeof(uint8_t)+\
    sizeof(sa_family_t)))
#define _SS_PAD2SIZE                                        \
    (_SS_MAXSIZE - (sizeof(uint8_t)+\
    sizeof(sa_family_t)+\
    _SS_PAD1SIZE + \
    _SS_ALIGNSIZE))

    /*
    * Structure allows applications to allocate space for
    * either form of inet socket address
    */
    struct sockaddr_storage {
        uint8_t      ss_len;          /* address length          */
        sa_family_t  ss_family;       /* address family          */
        /* following fields are implementation specific */
        char         __ss_pad1[_SS_PAD1SIZE];
        /* 6 byte pad, this is to make implementation   */
        /* specific pad up to alignment field that      */
        /* follows explicit in the data structure       */
#if (defined(__LL) || defined(_LP64))
        int64_t      __ss_align;
        /* storage alignment                            */
#else
        double       __ss_align;
        /* storage alignment                            */
#endif
        char         __ss_pad2[_SS_PAD2SIZE];
        /* 112 byte pad to achieve desired size         */
    };
#endif  /* __IPV6 || __OE_9 || __SUSV3_XSI */

    /*
    * Maximum queue length specifiable by listen.
    */
#define SOMAXCONN       10

    /*
    * Message header for recvmsg and sendmsg calls.
    */

#ifdef _OE_SOCKETS

    struct msghdr {
        caddr_t       msg_name;        /* optional address */
        int           msg_namelen;     /* size of address */
        struct iovec *msg_iov;         /* scatter/gather array */
        int           msg_iovlen;      /* # elements in msg_iov */
        caddr_t       msg_accrights;   /* access rights sent/ *
                                       * received */
        int           msg_accrightslen;
    };

#else     /* must be __UU  */

#ifdef _LP64
    struct msghdr {
        void         *msg_name;        /* optional address      */
        struct iovec *msg_iov;         /* scatter/gather array  */
        void         *msg_control;     /* ancillary data        */
        int           msg_flags;       /* flags on received msg */
        socklen_t     msg_namelen;     /* size of address       */
        int           msg_iovlen;      /* # elements in msg_iov */
        socklen_t     msg_controllen;  /* ancillary data length */
    };
#else
    struct msghdr {
        void         *msg_name;        /* optional address      */
#if __EDC_TARGET >= 0x22070000
        socklen_t   msg_namelen;     /* size of address       */
#else
        size_t      msg_namelen;     /* size of address       */
#endif                   /* __EDC_TARGET >= 0X22070000  */
        struct iovec *msg_iov;         /* scatter/gather array  */
        int           msg_iovlen;      /* # elements in msg_iov */
        void         *msg_control;     /* ancillary data        */
#if __EDC_TARGET >= 0x22070000
        socklen_t   msg_controllen;  /* ancillary data length */
#else
        size_t      msg_controllen;  /* ancillary data length */
#endif                   /* __EDC_TARGET >= 0X22070000  */
        int           msg_flags;       /* flags on received msg */
    };
#endif  /*  _LP64  */

    struct cmsghdr  {
#if __EDC_TARGET >= 0x22070000
        socklen_t  cmsg_len;   /* data byte count includes hdr */
#else
        size_t     cmsg_len;   /* data byte count includes hdr */
#endif                   /* __EDC_TARGET >= 0X22070000   */
        int      cmsg_level;     /* originating protocol         */
        int      cmsg_type;      /* protocol-specific type       */
    };


    /* macros for gaining access to ancillary data */

#define CMSG_DATA(cmsg)   \
    ((unsigned char *)((long)(cmsg)+sizeof(struct cmsghdr)))

#define CMSG_FIRSTHDR(mhdr)  ((mhdr)->msg_controllen > 0 ?  \
    (struct cmsghdr *)((mhdr)->msg_control) : \
    (struct cmsghdr *)0)

#define CMSG_NXTHDR(mhdr,cmsg)                              \
    (((long)(cmsg)+(cmsg)->cmsg_len >= \
    (long)((mhdr)->msg_control) + (mhdr)->msg_controllen) ? \
    (struct cmsghdr *)0 :                                    \
    (struct cmsghdr *)((long)(cmsg)+(cmsg)->cmsg_len))

#ifdef __IPV6

#define _CMSG_ALIGN(p) \
    (((unsigned long)(p)+(sizeof(int)-1)) \
    & ~(sizeof(int)-1))

#define CMSG_SPACE(length) \
    (_CMSG_ALIGN(sizeof(struct cmsghdr)) + \
    _CMSG_ALIGN(length))

#define CMSG_LEN(length) \
    (_CMSG_ALIGN(sizeof(struct cmsghdr)) + \
    length)

#endif /* IPv6 */


#endif    /*  _OE_SOCKETS  */

#if defined(__UU) || defined(__SUSV3_POSIX)
#define SCM_RIGHTS    0x01    /* access rights in data array */

#define MSG_CTRUNC    0x20    /* control data truncated      */
#define MSG_EOR       0x8     /* terminates a record         */
#define MSG_TRUNC     0x10    /* normal data truncated       */
#define MSG_WAITALL   0x40    /* wait for complete message   */
#endif  /* __UU || _SUSV3_POSIX */

#if __EDC_TARGET >= 0X22070000
#define MSG_NONBLOCK    0x4000 /* nonblocking request */
#define MSG_EOF         0x8000 /* send and close      */
#endif /* __EDC_TARGET >= 0X22070000*/

#define MSG_OOB         0x1     /* process out-of-band data */
#define MSG_PEEK        0x2     /* peek at incoming message */
#define MSG_DONTROUTE   0x4     /* send without using routing
    * tables */
#define MSG_ACK_GEN      0x40   /* generate UDP 'ACK packet'  */
#define MSG_ACK_TIMEOUT  0x20   /* caller expects packet within
    * standard ACK time interval */
#define MSG_ACK_EXPECTED 0x10   /* incoming packet should be
    * ACK                        */

#define MSG_MAXIOVLEN   16


    /*
    *  types of shutdown()
    */
#define SHUT_RD    0        /* disables further receive
    * operations */
#define SHUT_WR    1        /* disables further send
    * operations */
#define SHUT_RDWR  2        /* disables further send and
    * receive operations */

#if defined(__NATIVE_ASCII_F) && \
    (__EA_F >= __EA_F_4102_PQ63405) && \
    !defined(_OE_SOCKETS)
#pragma map (accept,              "\174\174A00404")
#pragma map (bind,                "\174\174A00406")
#pragma map (connect,             "\174\174A00407")
#pragma map (getpeername,         "\174\174A00408")
#pragma map (getsockname,         "\174\174A00409")
#pragma map (recvfrom,            "\174\174A00410")
#pragma map (sendto,              "\174\174A00411")
#pragma map (sendmsg,             "\174\174A00412")
#pragma map (recvmsg,             "\174\174A00413")

#else

#ifndef _NO_PRAGMA
#pragma map (getpeername, "\174\174GPEER")
#pragma map (getsockname, "\174\174GSKNA")
#pragma map (getsockopt, "\174\174GSKOP")
#pragma map (setsockopt, "\174\174STSOC")
#pragma map (socketpair, "\174\174SCKTP")
#endif /* _NO_PRAGMA */
#ifdef _OE_SOCKETS
#pragma map (recvmsg, "\174\174RCMSG")
#pragma map (sendmsg, "\174\174SENDM")
#else                 /* must be __UU */
#pragma map (recvmsg, "\174\174RCMS2")
#pragma map (sendmsg, "\174\174SNMS2")
#endif

#endif  /* __NATIVE_ASCII_F */

#ifndef _OE_SOCKETS   /* must be __UU */
#ifdef _NO_PROTO
    int     accept();
    int     bind();
    int     connect();
    int     getpeername();
    int     getsockname();
    int     getsockopt();
    int     listen();
    ssize_t recv();
    ssize_t recvfrom();
    ssize_t recvmsg();
    ssize_t send();
    ssize_t sendmsg();
    ssize_t sendto();
    int     setsockopt();
    int     shutdown();
    int     socket();
    int     socketpair();
#else
    int     accept(int, struct sockaddr * __restrict__,
        socklen_t * __restrict__);
    int     bind(int, const struct sockaddr *, socklen_t);
    int     connect(int, const struct sockaddr *, socklen_t);
    int     getpeername(int, struct sockaddr * __restrict__,
        socklen_t * __restrict__);
    int     getsockname(int, struct sockaddr * __restrict__,
        socklen_t * __restrict__);
    int     getsockopt(int, int, int, void * __restrict__,
        socklen_t * __restrict__);
    int     listen(int, int);
    ssize_t recv(int, void *, size_t, int);
    ssize_t recvfrom(int, void * __restrict__, size_t, int,
    struct sockaddr * __restrict__,
        socklen_t * __restrict__);
    ssize_t recvmsg(int, struct msghdr *, int);
    ssize_t send(int, const void *, size_t, int);
    ssize_t sendmsg(int, const struct msghdr *, int);
    ssize_t sendto(int, const void *, size_t, int,
        const struct sockaddr *, socklen_t);
    int     setsockopt(int, int, int, const void *, socklen_t);
    int     shutdown(int, int);
    int     socket(int, int, int);
    int     socketpair(int, int, int, int[2]);
#endif    /*  _NO_PROTO  */
#ifdef __AE_BIMODAL_F
#pragma map (__accept_a,            "\174\174A00404")
#pragma map (__accept_e,            "\174\174ACEPT")
#pragma map (__bind_a,              "\174\174A00406")
#pragma map (__bind_e,              "\174\174BIND\174")
#pragma map (__connect_a,           "\174\174A00407")
#pragma map (__connect_e,           "\174\174CONEC")
#pragma map (__getpeername_a,       "\174\174A00408")
#pragma map (__getpeername_e,       "\174\174GPEER")
#pragma map (__getsockname_a,       "\174\174A00409")
#pragma map (__getsockname_e,       "\174\174GSKNA")
#pragma map (__recvfrom_a,          "\174\174A00410")
#pragma map (__recvfrom_e,          "\174\174RFROM")
#pragma map (__sendto_a,            "\174\174A00411")
#pragma map (__sendto_e,            "\174\174SENDT")
#pragma map (__sendmsg_a,           "\174\174A00412")
#pragma map (__sendmsg_e,           "\174\174SENDM")
#pragma map (__recvmsg_a,           "\174\174A00413")
#pragma map (__recvmsg_e,           "\174\174RCMSG")

    __new4102(int, __accept_a, (int, struct sockaddr *, socklen_t *));
    __new4102(int, __accept_e, (int, struct sockaddr *, socklen_t *));
    __new4102(int, __bind_a, (int, const struct sockaddr *, socklen_t));
    __new4102(int, __bind_e, (int, const struct sockaddr *, socklen_t));
    __new4102(int, __connect_a, (int, const struct sockaddr *, socklen_t));
    __new4102(int, __connect_e, (int, const struct sockaddr *, socklen_t));
    __new4102(int, __getpeername_a, (int, struct sockaddr *, socklen_t *));
    __new4102(int, __getpeername_e, (int, struct sockaddr *, socklen_t *));
    __new4102(int, __getsockname_a, (int, struct sockaddr *, socklen_t *));
    __new4102(int, __getsockname_e, (int, struct sockaddr *, socklen_t *));
    __new4102(ssize_t, __recvfrom_a, (int, void *, size_t, int,
    struct sockaddr *, socklen_t *));
    __new4102(ssize_t, __recvfrom_e, (int, void *, size_t, int,
    struct sockaddr *, socklen_t *));
    __new4102(ssize_t, __sendto_a, (int, const void *, size_t, int,
        const struct sockaddr *, socklen_t));
    __new4102(ssize_t, __sendto_e, (int, const void *, size_t, int,
        const struct sockaddr *, socklen_t));
    __new4102(ssize_t, __sendmsg_a, (int, const struct msghdr *, int));
    __new4102(ssize_t, __sendmsg_e, (int, const struct msghdr *, int));
    __new4102(ssize_t, __recvmsg_a, (int, struct msghdr *, int));
    __new4102(ssize_t, __recvmsg_e, (int, struct msghdr *, int));

#endif

#endif    /* ifndef _OE_SOCKETS */

#ifdef __OE_3

#ifndef _NO_PRAGMA
#pragma map (getclientid, "\174\174GCLID")
#pragma map (__getclientid, "\174\174GCLIN")
#pragma map (getstablesize, "\174\174GSTBL")
#pragma map (getibmopt, "\174\174GIOPT")
#pragma map (getibmsockopt, "\174\174GISOC")
#pragma map (givesocket, "\174\174GVSOC")
#pragma map (ibmsflush, "\174\174IBMFL")
#pragma map (setibmopt, "\174\174SIOPT")
#pragma map (setibmsockopt, "\174\174STISO")
#pragma map (sock_debug, "\174\174SDEBG")
#pragma map (sock_debug_bulk_perf0, "\174\174SDEBP")
#pragma map (sock_do_bulkmode, "\174\174SDOBM")
#pragma map (sock_do_teststor, "\174\174SDOTT")
#pragma map (takesocket, "\174\174TASOC")
#endif /* _NO_PRAGMA */

#pragma map (tcperror, "PERROR")

#ifdef _NO_PROTO
    int  getclientid();
    int  __getclientid();
    int  getstablesize();
    int  getibmopt();
    int  getibmsockopt();
    int  givesocket();
    int  ibmsflush();
    int  maxdesc();
    int  setibmopt();
    int  setibmsockopt();
    void sock_debug();
    void sock_debug_bulk_perf0();
    void sock_do_bulkmode();
    void sock_do_teststor();
    int  takesocket();
    void tcperror();

#else
    int  getclientid(int, struct clientid *);
    int  __getclientid(int, struct clientid *);
    int  getstablesize(void);
    int  getibmopt(int, struct ibm_gettcpinfo *);
    int  getibmsockopt(int, int, int, char *, int *);
    int  givesocket(int, struct clientid *);
    int  ibmsflush(int);
    int  maxdesc(int *, int *);
    int  setibmopt(int, struct ibm_tcpimage *);
    int  setibmsockopt(int, int, int, char *, size_t);
    void sock_debug(int);
    void sock_debug_bulk_perf0(int);
    void sock_do_bulkmode(int);
    void sock_do_teststor(int);
    int  takesocket(struct clientid *, int);
    void tcperror(const char *);

#endif   /* _NO_PROTO */
#endif  /* __OE_3  */

#ifdef __OE_5

#if defined(__NATIVE_ASCII_F) && \
    (__EA_F >= __EA_F_4102_PQ63405)
#pragma map (accept_and_recv,      "\174\174A00405")
#else
#pragma map (accept_and_recv,      "\174\174ACC\174R")
#endif  /* __NATIVE_ASCII_F */

#if (__EDC_TARGET >= 0X22070000) || \
    (!defined(_NO_NEW_FUNC_CHECK))

#if ( __EDC_TARGET >= __EDC_LE4105 && defined(__LF) )
#pragma map (send_file, "\174\174SNDF\174O")
#else
#pragma map (send_file, "\174\174SEND\174F")
#endif /* __EDC_TARGET >= __EDC_LE4105 && defined(__LF) */

#endif /* __EDC_TARGET >= 0X22070000 */

#ifndef __socklen_t

#define __socklen_t  1
    typedef unsigned int socklen_t;

#endif

    /*
    *  structure for send_file()
    */

#if __EDC_TARGET >= 0X22070000

#if ( defined(__LF) || defined(_LP64) )
#pragma pack(4)
#endif /* __LF || _LP64 */

#ifdef _LP64
    struct sf_parms {
        int    socket_descriptor;
        int    header_length;
        int    header_alet;
        char   header_data_31[4];
        int    file_descriptor;
        off_t  file_bytes;
        off_t  file_offset;
        off_t  file_size;
        int    trailer_length;
        int    trailer_alet;
        char   trailer_data_31[4];
        off_t  bytes_sent;
        int    options;
        char   rsv1[12];
        void  *header_data;
        void  *trailer_data;
    };
#else
    struct sf_parms {
        int    socket_descriptor;
        size_t header_length;
        int    header_alet;
        void  *header_data;
        int    file_descriptor;
#ifndef __LF
        int    file_bytes_h;
#endif
        off_t  file_bytes;
#ifndef __LF
        int    file_offset_h;
#endif
        off_t  file_offset;
#ifndef __LF
        int    file_size_h;
#endif
        off_t  file_size;
        size_t trailer_length;
        int    trailer_alet;
        void  *trailer_data;
#ifndef __LF
        int    bytes_sent_h;
#endif
        off_t  bytes_sent;
        int    options;
        char   rsv1[12];
    };

#endif  /*  _LP64  */

#if ( defined(__LF) || defined(_LP64) )
#pragma pack(reset)
#endif /* __LF || _LP64 */

    /*
    *  value for sf_parms.options
    *  and the options field for send_file()
    */

#define  SF_REUSE  0x00000001
#define  SF_CLOSE  0x00000002
#endif /* __EDC_TARGET >= 0X22070000*/

#ifdef _NO_PROTO
    int    accept_and_recv();
#else
    int    accept_and_recv(int, int *, struct sockaddr *,
        socklen_t *, struct sockaddr *,
        socklen_t *, void *, size_t);
#endif   /* _NO_PROTO */

    __new27(int, send_file, (int *, struct sf_parms *, int));

#ifdef __AE_BIMODAL_F
#pragma map (__accept_and_recv_a, "\174\174A00405")
#pragma map (__accept_and_recv_e, "\174\174ACC\174R")

    __new4102(int, __accept_and_recv_a, (int, int *, struct sockaddr *,
        socklen_t *, struct sockaddr *,
        socklen_t *, void *, size_t));
    __new4102(int, __accept_and_recv_e, (int, int *, struct sockaddr *,
        socklen_t *, struct sockaddr *,
        socklen_t *, void *, size_t));

#endif

#endif  /* __OE_5  */

#ifdef _OPEN_SYS
#if __EDC_TARGET >= 0X41050000
    /*
    * __poecb_t structure used for the __poe() service
    */
    typedef struct __poecb_s {

        unsigned int __poe_options;     /* Options for __poe()    */
        unsigned int __poe_entry_type;  /* Port of Entry Type     */
        unsigned int __poe_entry_len;   /* Port of Entry Length   */
        char         __poe_resrv1[4];   /* reserved               */
        __pad31(__poe_resrv2, 4)         /* reserved 31-bit padding*/
            void        *__poe_entry_ptr;   /* Addr of Port of Entry  */

    } __poecb_t;

    /*
    *  __poe_options values
    */
#define _POE_THREAD     0x00000001  /* scope = thread    */
#define _POE_PROCESS    0x00000002  /* scope = process   */

    /*
    *   __poe_entry_type values
    */
#define _POE_SOCKET    1    /* socket file descriptor    */
#define _POE_FILE      2    /* non-socket file descriptor*/

    /*
    *   __poe_entry_len values
    */
#define _POE_SOCKET_LEN  4  /* socket descriptor length  */
#define _POE_FILE_LEN    4  /* non-socket desc length    */

#ifndef _NO_PRAGMA
#pragma map (__poe, "\174\174POE")
#endif /* _NO_PRAGMA */

    __new4105(int, __poe, (__poecb_t *));

#endif /* __EDC_TARGET >= 0X41050000*/

#endif  /* _OPEN_SYS */

#ifdef _ALL_SOURCE
#ifdef _NO_PROTO
    int     setpeer();
#else
    int     setpeer(int, struct sockaddr *, int, char *);
#endif    /*  _NO_PROTO  */

#define ALIGN(p)      (((u_long)(p) + (sizeof(void *) - 1)) \
    & ~(sizeof(void *)-1))

#endif /* _ALL_SOURCE */

#endif /* _OE_SOCKETS || __UU || __SUSV3_POSIX */

    /*
    *
    * Single UNIX Specification, Version 3
    *
    */

#ifdef __SUSV3_POSIX
#ifndef _NO_PRAGMA
#pragma map (sockatmark, "\174\174SCKATM")
#endif  /* _NO_PRAGMA */
    __new4109(int, sockatmark, (int));
#endif  /* __SUSV3_POSIX */

#ifdef __IPV6
#ifndef __netinet_in
#include <netinet/in.h>
#endif
#endif /* __IPV6 */

#endif     /*  __TARGET_LIB__ >= __EDC_LE  */

#ifdef __cplusplus
}
#endif

#pragma checkout(resume)
? ? = endif     /*  __socket_h   */

                   ??=ifdef __COMPILER_VER__
                     ??=pragma filetag ("IBM-1047")
                   ??=endif
                   #pragma nomargins nosequence
                   #pragma checkout(suspend)
#ifndef _H_IN
    #define _H_IN 1
/*** IBMCOPYR ********************************************************/
/*                                                                   */
/* Copyright:                                                        */
/*   Licensed Materials - Property of IBM                            */
/*   This product contains "Restricted Materials of IBM"             */
/*   5694-A01 (C) Copyright IBM Corp. 1991, 2007.                    */
/*   All rights reserved.                                            */
/*   US Government Users Restricted Rights -                         */
/*   Use, duplication or disclosure restricted by GSA ADP Schedule   */
/*   Contract with IBM Corp.                                         */
/*   See IBM Copyright Instructions.                                 */
/*                                                                   */
/* Status:    CSV1R9                                                 */
/*                                                                   */
/* Change Activity:                                                  */
/* CFD List:                                                         */
/*                                                                   */
/* Flag Reason   Release  Date   Origin    Description               */
/* ---- -------- -------- ------ --------  -----------------------   */
/* $Q1= D316.7   CSV1R5   020612 KMPorter: CICS IPv6 Enhancements    */
/* $Y1= R9STKMLD CSV1R9   060925 KMPorter: MLDv2 and IGMPv3 support  */
/*                                                                   */
/* End CFD List:                                                     */
/*                                                                   */
/*** IBMCOPYR ********************************************************/

/* @(#)in.h   2.1 87/03/30 14:49:40 */
/* adapted from BSD to AIX, changed u_long... to ulong... */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *  @(#)in.h  7.1 (Berkeley) 6/5/86
 */

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
  #ifndef _H_SOCKET                                           /* @Y1A*/
    #include <sys/socket.h>                                   /* @Y1A*/
  #endif /* end of _H_SOCKET */                               /* @Y1A*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/

/*
 * Constants and structures defined by the internet system,
 * Per RFC 790, September 1981.
 */

/*
 * Protocols
 */
#if defined (__CICS_IPV6) || defined (__CICS_SOCKETS)         /* @Y1A*/
  #ifndef __ipproto_ip                                        /* @Y1A*/
    #define __ipproto_ip 1                                    /* @Y1A*/
    #define  IPPROTO_IP      0         /* dummy for IP */     /* @Y1A*/
  #endif /* end of __ipproto_ip */                            /* @Y1A*/
#else                                                         /* @Y1A*/
#define  IPPROTO_IP          0         /* dummy for IP */
#endif                                                        /* @Y1A*/
#define  IPPROTO_ICMP        1         /* control message protocol */
#define  IPPROTO_GGP         2         /* gateway^2 (deprecated) */
#define  IPPROTO_TCP         6         /* tcp */
#define  IPPROTO_EGP         8         /* exterior gateway protocol */
#define  IPPROTO_PUP         12        /* pup */
#define  IPPROTO_UDP         17        /* user datagram protocol */
#define  IPPROTO_IDP         22        /* xns idp */

#ifdef __CICS_IPV6                                            /* @Q1A*/
  #ifndef __ipproto_ipv6                                      /* @Y1A*/
    #define __ipproto_ipv6 1                                  /* @Y1A*/
    #define IPPROTO_IPV6     41 /* IPv6 header */        /* @Q1A @Y1C*/
  #endif /* end of __ipproto_ipv6 */                          /* @Y1A*/
                                                              /* @Q1A*/
#define IPV6_V6ONLY          10                               /* @Q1A*/
#endif  /* end of __CICS_IPV6 defs */                         /* @Q1A*/

#define  IPPROTO_RAW         255       /* raw IP packet */
#define  IPPROTO_MAX         256


/*
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 * Ports > IPPORT_USERRESERVED are reserved
 * for servers, not necessarily privileged.
 */
#define  IPPORT_RESERVED          1024
#define  IPPORT_USERRESERVED 5000

/*
 * Link numbers
 */
#define  IMPLINK_IP          155
#define  IMPLINK_LOWEXPER    156
#define  IMPLINK_HIGHEXPER   158

#if defined (__CICS_IPV6) || defined (__CICS_SOCKETS)    /* @Q1A @Y1C*/
  #ifndef __uint8_t                                           /* @Q1A*/
     #define __uint8_t  1                                     /* @Q1A*/
     typedef unsigned char uint8_t;                           /* @Q1A*/
  #endif                                                      /* @Q1A*/
  #ifndef __uint32_t                                          /* @Q1A*/
     #define __uint32_t  1                                    /* @Q1A*/
     typedef unsigned int  uint32_t;                          /* @Q1A*/
  #endif                                                      /* @Q1A*/

  /*  Typedefs   */                                           /* @Q1A*/
  #ifndef __sa_family_t                                       /* @Q1A*/
    #define __sa_family_t  1                                  /* @Q1A*/
    typedef unsigned char sa_family_t;                        /* @Q1A*/
  #endif                                                      /* @Q1A*/
  #ifndef __in_port_t                                         /* @Q1A*/
     #define __in_port_t  1                                   /* @Q1A*/
     typedef unsigned short in_port_t;                        /* @Q1A*/
  #endif                                                      /* @Q1A*/

  struct in6_addr {                                           /* @Q1A*/
    union {                                                   /* @Q1A*/
      uint8_t  _S6_u8[16];                                    /* @Q1A*/
      uint32_t _S6_u32[4];                                    /* @Q1A*/
    } _S6_un;                                                 /* @Q1A*/
  };                                                          /* @Q1A*/
  #define s6_addr _S6_un._S6_u8                               /* @Q1A*/
  #define SIN6_LEN                                            /* @Q1A*/
  struct sockaddr_in6 {                                       /* @Q1A*/
    uint8_t         sin6_len;                                 /* @Q1A*/
    sa_family_t     sin6_family;                              /* @Q1A*/
    in_port_t       sin6_port;                                /* @Q1A*/
    uint32_t        sin6_flowinfo;                            /* @Q1A*/
    struct in6_addr sin6_addr;                                /* @Q1A*/
    uint32_t        sin6_scope_id;                            /* @Q1A*/
  };                                                          /* @Q1A*/

#endif   /* __CICS_IPV6 || __CICS_SOCKETS */             /* @Q1A @Y1C*/
/*
 * Internet address (a structure for historical reasons)
 */
struct in_addr {
    u_long s_addr;
};

/*
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
 */
#define  IN_CLASSA(i)        (((long)(i) & 0x80000000) == 0)
#define  IN_CLASSA_NET       0xff000000
#define  IN_CLASSA_NSHIFT    24
#define  IN_CLASSA_HOST      0x00ffffff
#define  IN_CLASSA_MAX       128

#define  IN_CLASSB(i)        (((long)(i) & 0xc0000000) == 0x80000000)
#define  IN_CLASSB_NET       0xffff0000
#define  IN_CLASSB_NSHIFT    16
#define  IN_CLASSB_HOST      0x0000ffff
#define  IN_CLASSB_MAX       65536

#define  IN_CLASSC(i)        (((long)(i) & 0xc0000000) == 0xc0000000)
#define  IN_CLASSC_NET       0xffffff00
#define  IN_CLASSC_NSHIFT    8
#define  IN_CLASSC_HOST      0x000000ff

#define  INADDR_ANY          (u_long)0x00000000
#define  INADDR_BROADCAST    (u_long)0xffffffff   /* must be masked */

#ifdef __CICS_IPV6                                            /* @Q1A*/
   #define IN6ADDR_ANY_INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0} /* @Q1A*/
   #define IN6ADDR_LOOPBACK_INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}
                                                              /* @Q1A*/
   #ifdef __LONGNAME__                                        /* @Q1A*/
     extern const struct in6_addr in6addr_any;                /* @Q1A*/
     extern const struct in6_addr in6addr_loopback;           /* @Q1A*/
   #endif /* __LONGNAME__ */                                  /* @Q1A*/
   #define _IS_ANYADDR6(p)      \
   (((p)->s6_addr[0] == 0x00) && \
    ((p)->s6_addr[1] == 0x00) && \
    ((p)->s6_addr[2] == 0x00) && \
    ((p)->s6_addr[3] == 0x00) && \
    ((p)->s6_addr[4] == 0x00) && \
    ((p)->s6_addr[5] == 0x00) && \
    ((p)->s6_addr[6] == 0x00) && \
    ((p)->s6_addr[7] == 0x00) && \
    ((p)->s6_addr[8] == 0x00) && \
    ((p)->s6_addr[9] == 0x00) && \
    ((p)->s6_addr[10] == 0x00) && \
    ((p)->s6_addr[11] == 0x00) && \
    ((p)->s6_addr[12] == 0x00) && \
    ((p)->s6_addr[13] == 0x00) && \
    ((p)->s6_addr[14] == 0x00) && \
    ((p)->s6_addr[15] == 0x00))                               /* @Q1A*/
   #define _IS_LOOPADDR6(p)     \
   (((p)->s6_addr[0] == 0x00) && \
    ((p)->s6_addr[1] == 0x00) && \
    ((p)->s6_addr[2] == 0x00) && \
    ((p)->s6_addr[3] == 0x00) && \
    ((p)->s6_addr[4] == 0x00) && \
    ((p)->s6_addr[5] == 0x00) && \
    ((p)->s6_addr[6] == 0x00) && \
    ((p)->s6_addr[7] == 0x00) && \
    ((p)->s6_addr[8] == 0x00) && \
    ((p)->s6_addr[9] == 0x00) && \
    ((p)->s6_addr[10] == 0x00) && \
    ((p)->s6_addr[11] == 0x00) && \
    ((p)->s6_addr[12] == 0x00) && \
    ((p)->s6_addr[13] == 0x00) && \
    ((p)->s6_addr[14] == 0x00) && \
    ((p)->s6_addr[15] == 0x01))                               /* @Q1A*/

  #define _IS_MULTIADDR6(p) \
         ((p)->s6_addr[0] == 0xff)                            /* @Q1A*/
  #define _IS_LINKLADDR6(p) \
        (((p)->s6_addr[0] == 0xfe) && \
        (((p)->s6_addr[1] & 0xc0) == 0x80))                   /* @Q1A*/
  #define _IS_SITELADDR6(p) \
        (((p)->s6_addr[0] == 0xfe) && \
        (((p)->s6_addr[1] & 0xc0) == 0xc0))                   /* @Q1A*/
  #define _IS_IPV4ADDR6(p) \
        (((p)->s6_addr[0] == 0x00) && \
         ((p)->s6_addr[1] == 0x00) && \
         ((p)->s6_addr[2] == 0x00) && \
         ((p)->s6_addr[3] == 0x00) && \
         ((p)->s6_addr[4] == 0x00) && \
         ((p)->s6_addr[5] == 0x00) && \
         ((p)->s6_addr[6] == 0x00) && \
         ((p)->s6_addr[7] == 0x00) && \
         ((p)->s6_addr[8] == 0x00) && \
         ((p)->s6_addr[9] == 0x00) && \
         ((p)->s6_addr[10] == 0xff) && \
         ((p)->s6_addr[11] == 0xff))                          /* @Q1A*/
  #define _IS_COMPATADDR6(p) \
        (((p)->s6_addr[0] == 0x00) && \
         ((p)->s6_addr[1] == 0x00) && \
         ((p)->s6_addr[2] == 0x00) && \
         ((p)->s6_addr[3] == 0x00) && \
         ((p)->s6_addr[4] == 0x00) && \
         ((p)->s6_addr[5] == 0x00) && \
         ((p)->s6_addr[6] == 0x00) && \
         ((p)->s6_addr[7] == 0x00) && \
         ((p)->s6_addr[8] == 0x00) && \
         ((p)->s6_addr[9] == 0x00) && \
         ((p)->s6_addr[10] == 0x00) && \
         ((p)->s6_addr[11] == 0x00) && \
         ((p)->s6_addr[12] != 0x00))                          /* @Q1A*/


  #define _MADDR6_SCOPE(p)\
         (((p)->s6_addr[1] & 0x0f))                           /* @Q1A*/
  #define _MADDR6_SCP_NODE   1                                /* @Q1A*/
  #define _MADDR6_SCP_LINK   2                                /* @Q1A*/
  #define _MADDR6_SCP_SITE   5                                /* @Q1A*/
  #define _MADDR6_SCP_ORG    8                                /* @Q1A*/
  #define _MADDR6_SCP_GLO   14                                /* @Q1A*/
  #define IN6_IS_ADDR_UNSPECIFIED(p)  _IS_ANYADDR6(p)         /* @Q1A*/
  #define IN6_IS_ADDR_LOOPBACK(p)    _IS_LOOPADDR6(p)         /* @Q1A*/
  #define IN6_IS_ADDR_MULTICAST(p)   _IS_MULTIADDR6(p)        /* @Q1A*/
  #define IN6_IS_ADDR_LINKLOCAL(p)   _IS_LINKLADDR6(p)        /* @Q1A*/
  #define IN6_IS_ADDR_SITELOCAL(p)   _IS_SITELADDR6(p)        /* @Q1A*/
  #define IN6_IS_ADDR_V4MAPPED(p)    _IS_IPV4ADDR6(p)         /* @Q1A*/
  #define IN6_IS_ADDR_V4COMPAT(p)   _IS_COMPATADDR6(p)        /* @Q1A*/
  #define IN6_IS_ADDR_MC_NODELOCAL(p)  \
             (_IS_MULTIADDR6(p)  && _MADDR6_SCOPE(p) ==\
                            _MADDR6_SCP_NODE)                 /* @Q1A*/
  #define IN6_IS_ADDR_MC_LINKLOCAL(p)  \
             (_IS_MULTIADDR6(p)  && _MADDR6_SCOPE(p) == \
                           _MADDR6_SCP_LINK)                  /* @Q1A*/
  #define IN6_IS_ADDR_MC_SITELOCAL(p)  \
             (_IS_MULTIADDR6(p)  && _MADDR6_SCOPE(p) == \
                    _MADDR6_SCP_SITE)                         /* @Q1A*/
  #define IN6_IS_ADDR_MC_ORGLOCAL(p)  \
             (_IS_MULTIADDR6(p)  && _MADDR6_SCOPE(p) == \
                    _MADDR6_SCP_ORG)                          /* @Q1A*/
  #define IN6_IS_ADDR_MC_GLOBAL(p)  \
             (_IS_MULTIADDR6(p)  && _MADDR6_SCOPE(p) == \
                    _MADDR6_SCP_GLO)                          /* @Q1A*/
#endif  /* end of __CICS_IPV6 defs */                         /* @Q1A*/

#ifndef  KERNEL
#define  INADDR_NONE         (u_long)0xffffffff   /* -1 return */
#endif

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
  /*  Typedefs   */                                           /* @Y1A*/
  #ifndef __sa_family_t                                       /* @Y1A*/
    #define __sa_family_t  1                                  /* @Y1A*/
    typedef unsigned char sa_family_t;                        /* @Y1A*/
  #endif                                                      /* @Y1A*/
  #ifndef __in_port_t                                         /* @Y1A*/
     #define __in_port_t  1                                   /* @Y1A*/
     typedef unsigned short in_port_t;                        /* @Y1A*/
  #endif                                                      /* @Y1A*/
  /*  Structures */                                           /* @Y1A*/
  struct sockaddr_in   {                                      /* @Y1A*/
      unsigned char  sin_len;                                 /* @Y1A*/
      sa_family_t    sin_family;                              /* @Y1A*/
      in_port_t      sin_port;                                /* @Y1A*/
      struct in_addr sin_addr;                                /* @Y1A*/
      unsigned char  sin_zero[8];                             /* @Y1A*/
  };                                                          /* @Y1A*/
#else                                                         /* @Y1A*/
/*
 * Socket address, internet style.
 */
struct sockaddr_in {
    short     sin_family;
    ushort    sin_port;
    struct    in_addr sin_addr;
    char sin_zero[8];
};
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/


struct ip_mreq {                                              /* @Q1A*/
         struct in_addr imr_multiaddr;                        /* @Q1A*/
         struct in_addr imr_interface;                        /* @Q1A*/
};                                                            /* @Q1A*/

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
struct ip_mreq_source {                                       /* @Y1A*/
         struct in_addr imr_multiaddr;                        /* @Y1A*/
         struct in_addr imr_sourceaddr;                       /* @Y1A*/
         struct in_addr imr_interface;                        /* @Y1A*/
};                                                            /* @Y1A*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
struct group_req {                                            /* @Y1A*/
         uint32_t                gr_interface;                /* @Y1A*/
         uint32_t                __gr_01;                     /* @Y1A*/
         struct sockaddr_storage gr_group;                    /* @Y1A*/
};                                                            /* @Y1A*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
struct group_source_req {                                     /* @Y1A*/
         uint32_t                gsr_interface;               /* @Y1A*/
         uint32_t                __gsr_01;                    /* @Y1A*/
         struct sockaddr_storage gsr_group;                   /* @Y1A*/
         struct sockaddr_storage gsr_source;                  /* @Y1A*/
};                                                            /* @Y1A*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
#define MCAST_INCLUDE 0                                       /* @Y1A*/
#define MCAST_EXCLUDE 1                                       /* @Y1A*/
#define MCAST_NUMSRC_MAX 64                                   /* @Y1A*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/

/*
 * Options for use with [gs]etsockopt at the IP level.
 */
#define  IP_OPTIONS     1         /* set/get IP per-packet options */

#define  IP_MULTICAST_IF           7                          /* @Q1A*/
#define  IP_MULTICAST_TTL          3                          /* @Q1A*/
#define  IP_MULTICAST_LOOP         4                          /* @Q1A*/
#define  IP_ADD_MEMBERSHIP         5                          /* @Q1A*/
#define  IP_DROP_MEMBERSHIP        6                          /* @Q1A*/

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
#define  IP_BLOCK_SOURCE           10                         /* @Y1A*/
#define  IP_UNBLOCK_SOURCE         11                         /* @Y1A*/
#define  IP_ADD_SOURCE_MEMBERSHIP  12                         /* @Y1A*/
#define  IP_DROP_SOURCE_MEMBERSHIP 13                         /* @Y1A*/

#define  MCAST_BLOCK_SOURCE        44                         /* @Y1A*/
#define  MCAST_JOIN_GROUP          40                         /* @Y1A*/
#define  MCAST_JOIN_SOURCE_GROUP   42                         /* @Y1A*/
#define  MCAST_LEAVE_GROUP         41                         /* @Y1A*/
#define  MCAST_LEAVE_SOURCE_GROUP  43                         /* @Y1A*/
#define  MCAST_UNBLOCK_SOURCE      45                         /* @Y1A*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/

#ifdef __CICS_IPV6                                            /* @Q1A*/
   #define IPV6_UNICAST_HOPS     3                            /* @Q1A*/
   #define IPV6_MULTICAST_IF     7                            /* @Q1A*/
   #define IPV6_MULTICAST_HOPS   9                            /* @Q1A*/
   #define IPV6_MULTICAST_LOOP   4                            /* @Q1A*/
   #define IPV6_JOIN_GROUP       5                            /* @Q1A*/
   #define IPV6_LEAVE_GROUP      6                            /* @Q1A*/
    struct ipv6_mreq {                                        /* @Q1A*/
       struct in6_addr ipv6mr_multiaddr;                      /* @Q1A*/
       unsigned int      ipv6mr_interface;                    /* @Q1A*/
    };                                                        /* @Q1A*/
    struct in6_pktinfo {                                      /* @Q1A*/
        struct in6_addr  ipi6_addr; /* src/dst IPv6 address      @Q1A*/
        unsigned int ipi6_ifindex;  /* send/recv interface index @Q1A*/
    };                                                        /* @Q1A*/
    #define IN6_ARE_ADDR_EQUAL(p,q) \
        (((p)->s6_addr[0]  == (q)->s6_addr[0])  && \
         ((p)->s6_addr[1]  == (q)->s6_addr[1])  && \
         ((p)->s6_addr[2]  == (q)->s6_addr[2])  && \
         ((p)->s6_addr[3]  == (q)->s6_addr[3])  && \
         ((p)->s6_addr[4]  == (q)->s6_addr[4])  && \
         ((p)->s6_addr[5]  == (q)->s6_addr[5])  && \
         ((p)->s6_addr[6]  == (q)->s6_addr[6])  && \
         ((p)->s6_addr[7]  == (q)->s6_addr[7])  && \
         ((p)->s6_addr[8]  == (q)->s6_addr[8])  && \
         ((p)->s6_addr[9]  == (q)->s6_addr[9])  && \
         ((p)->s6_addr[10] == (q)->s6_addr[10]) && \
         ((p)->s6_addr[11] == (q)->s6_addr[11]) && \
         ((p)->s6_addr[12] == (q)->s6_addr[12]) && \
         ((p)->s6_addr[13] == (q)->s6_addr[13]) && \
         ((p)->s6_addr[14] == (q)->s6_addr[14]) && \
         ((p)->s6_addr[15] == (q)->s6_addr[15]))              /* @Q1A*/
    #define INET6_ADDRSTRLEN 46                               /* @Q1A*/
#endif  /* end of __CICS_IPV6 defs */                         /* @Q1A*/

#ifdef __CICS_SOCKETS                                         /* @Y1A*/
    #define INET_ADDRSTRLEN 16                           /* @Q1A @Y1M*/
#endif /* end of __CICS_SOCKETS */                            /* @Y1A*/


#if !defined(vax) && !defined(ntohl) && !defined(lint)
/*
 * Macros for number representation conversion.
 */

#ifdef _TCP31_PROTOS
#define  ntohl(x)  (x)
#define  ntohs(x)  (x)
#define  htonl(x)  (x)
#define  htons(x)  (x)
#else
#ifdef __cplusplus
extern "C" {
#endif
unsigned long htonl (unsigned long);
unsigned short htons (unsigned short);
unsigned long ntohl (unsigned long);
unsigned short ntohs (unsigned short);
#ifdef __cplusplus
}
#endif
#pragma map(ntohl, "NTOHL")
#pragma map(ntohs, "NTOHS")
#pragma map(htonl, "HTONL")
#pragma map(htons, "HTONS")
#endif /* _TCP31_PROTOS */
#endif

#if !defined(ntohl) && (defined(vax) || defined(lint))
ushort   ntohs(), htons();
u_long    ntohl(), htonl();
#endif

#ifdef KERNEL
extern   struct domain inetdomain;
extern   struct protosw inetsw[];
struct   in_addr in_makeaddr();
u_long    in_netof(), in_lnaof();
#endif
#endif /* _H_IN */
#pragma checkout(resume)

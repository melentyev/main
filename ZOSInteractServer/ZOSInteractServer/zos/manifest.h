                   ??=ifdef __COMPILER_VER__
                     ??=pragma filetag ("IBM-1047")
                   ??=endif
                   #pragma nomargins nosequence
                   #pragma checkout(suspend)
/*********************************************************************/
/*                                                                   */
/* Part Name:        manifest.h                                      */
/*                                                                   */
/* Invocation Name: #include <manifest.h>                            */
/*                                                                   */
/* Descriptive Name: manifest header file for C socket library       */
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
/* Status:         CSV1R9                                            */
/*                                                                   */
/*  CHANGE ACTIVITY:                                                 */
/*                                                                   */
/* Flag Reason   Release  Date   Origin    Description               */
/* ---- -------- -------- ------ --------  ------------------------  */
/* @J1= MV19685  CSV3R10  990810 MajikesJ: Added longname for errno  */
/* @L1= D312.22  CSV1R2   000928 MajikesJ: Use LE resolver           */
/*                                         Remove asciitoebcdic,     */
/*                                         ebcdictoascii, dn_expand, */
/*                                         dn_skipname, res_mkquery, */
/*                                         res_query, res_querydomain*/
/*                                         and res_search            */
/* $Q1= D316.7   CSV1R5   020624 KMPorter: IPv6 enhancements         */
/* $A1= PQ66780  HIP6120  021024 VALLER  : Variant characters to     */
/*                                         octal                     */
/* $A2= PK15419  HIP6140  051121 BKelsey : Always map names of       */
/*                                         functions that are in     */
/*                                         EZACIC07 (even in C++)    */
/* $Y1= R9STKMLD CSV1R9   060929 KMPorter: MLDv2 and IGMPv3 support  */
/*                                                                   */
/*********************************************************************/

#ifndef __EZA_MANIFEST_H
  #define __EZA_MANIFEST_H

  /* Special mappings for VM and MVS */

  #if !defined(MVS) && !defined(VM)
  #define MVS
  #endif

  #if defined(MVS) && !defined(VM)
  #define VM
  #endif

  #ifdef VM
  #ifndef bzero
  #include <bsdtocms.h>
  #endif
  #ifndef NULL
  #define NULL ((void *)0)
  #endif
  #endif

  /* Long RPC names */

  /* Note: When compiling with C++, the pragma maps are done
   *       in the header file that has the corresponding function
   *       prototype.
   */
  #ifndef __cplusplus
  #pragma map(_authenticate,          "ATHNTCT")
  #pragma map(_null_auth,             "NLATH")
  #pragma map(_seterr_reply,          "STRRPLY")

  #pragma map(_svcauth_null,          "SVCTHNL")
  #pragma map(_svcauth_short,         "SVCTHSHR")
  #pragma map(_svcauth_unix,          "SVCTHUNX")
  #pragma map(_xdr_enum,              "\174XDRENUM")          /* @A1C*/
  #pragma map(_xdr_union,             "\174XDRUNN")           /* @A1C*/
  #pragma map(authnone_create,        "THNNCRT")
  #pragma map(authunix_create,        "THNXCRT")
  #pragma map(authunix_create_default,"THNXCRTD")
  #pragma map(clnt_broadcast,         "CLNTBRDC")
  #pragma map(clnt_create,            "CLNTCRT")
  #pragma map(clnt_pcreateerror,      "CLNTPCRT")
  #pragma map(clnt_perrno,            "CLNTPRN")
  #pragma map(clnt_perror,            "CLNTPRR")
  #pragma map(clnt_spcreateerror,     "CLNTSPCT")
  #pragma map(clnt_sperrno,           "CLNTSPN")
  #pragma map(clnt_sperror,           "CLNTSPR")
  #pragma map(clntraw_create,         "CLNTRWCR")
  #pragma map(clnttcp_create,         "CLNTCPCR")
  #pragma map(clntudp_bufcreate,      "CLNTDPBF")
  #pragma map(clntudp_create,         "CLNTDPCR")
  #pragma map(endrpcent,              "NDRPCNT")
  #pragma map(get_myaddress,          "GTMYDRS")
  #pragma map(getrpcbyname,           "GTRPCNAM")
  #pragma map(getrpcbynumber,         "GTRPCNUM")
  #pragma map(getrpcent,              "GTRPCNT")
  #pragma map(getrpcport,             "GTRPCPT")
  #pragma map(pmap_getmaps,           "PMPGTMPS")
  #pragma map(pmap_getport,           "PMPGTPRT")
  #pragma map(pmap_rmtcall,           "PMPRMTCL")
  #pragma map(pmap_unset,             "PMPUNST")
  #pragma map(reg_service,            "RGSRVC")
  #pragma map(registerrpc,            "RGSTRPC")
  #pragma map(rpc_createerr,          "RPCCRTR")
  #pragma map(setrpcent,              "STRPCNT")
  #pragma map(svc_fdset,              "SVCFDSET")
  #pragma map(svc_getreq,             "SVCGTRQ")
  #pragma map(svc_getreqset,          "SVCGTRQS")
  #pragma map(svc_register,           "SVCRGSTR")
  #pragma map(svc_sendreply,          "SVCSNDRP")
  #pragma map(svc_unregister,         "SVCUNRGS")
  #pragma map(svcerr_auth,            "SVCRATH")
  #pragma map(svcerr_decode,          "SVCRDCD")
  #pragma map(svcerr_noproc,          "SVCRNPRC")
  #pragma map(svcerr_noprog,          "SVCRNPRG")
  #pragma map(svcerr_progvers,        "SVCRPRGV")
  #pragma map(svcerr_systemerr,       "SVCRSYST")
  #pragma map(svcerr_weakauth,        "SVCRWKTH")
  #pragma map(svcfd_create,           "SVCFDCRT")
  #pragma map(svcraw_create,          "SVCRWCRT")
  #pragma map(svctcp_create,          "SVCTCPCR")
  #pragma map(svcudp_bufcreate,       "SVCDPBFC")
  #pragma map(svcudp_create,          "SVCDPCRT")
  #pragma map(x_callms,               "XDRCLMSG")
  #pragma map(x_opauth,               "XDROPQAT")
  #pragma map(x_pmaplst,              "XDRPMPLS")
  #pragma map(xdr_accepted_reply,     "XDRACPTD")
  #pragma map(xdr_array,              "XDRARY")
  #pragma map(xdr_authunix_parms,     "XDRATHNX")
  #pragma map(xdr_bytes,              "XDRBYTS")
  #pragma map(xdr_callhdr,            "XDRCLHDR")
  #pragma map(xdr_callmsg,            "XDRCLMSG")
  #pragma map(xdr_deskey,             "XDRDSKY")
  #pragma map(xdr_double,             "XDRDBL")
  #pragma map(xdr_float,              "XDRFLT")
  #pragma map(xdr_netobj,             "XDRNOBJ")
  #pragma map(xdr_opaque,             "XDROPQ")
  #pragma map(xdr_opaque_auth,        "XDROPQAT")
  #pragma map(xdr_pmaplist,           "XDRPMPLS")
  #pragma map(xdr_pointer,            "XDRPTR")
  #pragma map(xdr_reference,          "XDRRFRNC")
  #pragma map(xdr_rejected_reply,     "XDRRJCTD")
  #pragma map(xdr_replymsg,           "XDRRPLYM")
  #pragma map(xdr_short,              "XDRSHRT")
  #pragma map(xdr_string,             "XDRSTRNG")
  #pragma map(xdr_u_char,             "XDRUCHR")
  #pragma map(xdr_u_int,              "XDRUINT")
  #pragma map(xdr_u_long,             "XDRULNG")
  #pragma map(xdr_u_short,            "XDRUSHRT")
  #pragma map(xdr_vector,             "XDRVCTR")
  #pragma map(xdr_wrapstring,         "XDRWRPST")
  #pragma map(xdrmem_create,          "XDRMMCRT")
  #pragma map(xdrrec_create,          "XDRCCRT")
  #pragma map(xdrrec_endofrecord,     "XDRCENDF")
  #pragma map(xdrrec_eof,             "XDRCEF")
  #pragma map(xdrrec_skiprecord,      "XDRCSKPR")
  #pragma map(xdrstdio_create,        "XDRSTDCR")
  #pragma map(xprt_register,          "XPRTRGST")
  #pragma map(xprt_unregister,        "XPRTUNRG")
  #endif    /* ifndef __cplusplus */

  /* Special RPC defines for varying length enumeration types */

  #ifdef VM
  #ifdef enum_t
  #define xdr_enum \
    (_xdr_enum(-sizeof(enum_t)*((enum_t)(-1)>>(8*sizeof(enum_t)-1))))
  #else
  #define xdr_enum(xdrs, ep) \
    (_xdr_enum(sizeof(*(ep))*(2*((xdrs)->x_op!=XDR_ENCODE||*(ep)<0)-1))\
    ((xdrs), (caddr_t)(ep)))
  #endif
  #define xdr_union(xdrs, dscmp, unp, choices, dfault) \
    (!xdr_enum((xdrs), (dscmp)) ? FALSE: \
    _xdr_union((xdrs), *(dscmp), (unp), (choices), (dfault)))
  #endif

  /*-----------------------------------------------------------*/
  /* Various long system names used in C socket Library -      */
  /*     internal routines, variables                          */
  /*-----------------------------------------------------------*/

  #ifndef __cplusplus
  #pragma map(falprintf,       "FALPRINT")
  #pragma map(falfprintf,      "FALFPRIN")
  #pragma map(falsprintf,      "FALSPRIN")
  #pragma map(falperror,       "FALPERRO")
  #pragma map(faltcperror,     "FALTCPER")
  #pragma map(get_iflist,      "GET\174IFLI")                 /* @A1C*/
  #pragma map(getgroups,       "GTGRPS"  )
  #pragma map(gettimeofday,    "GTMFDY"  )
  #pragma map(sys_errlist,     "SYSERLST")
  #pragma map(strcasecmp,      "STRCASEC")
  #pragma map(strncasecmp,     "STRNCASE")
  #pragma map(h_errlist,       "H\174ERRLIS")                 /* @A1C*/
  #pragma map(h_errno,         "H\174ERRNO")              /* J1A @A1C*/
  #pragma map(hostalias,       "HOSTALIA")
  #pragma map(EZAMSGALL,       "EZAMSGAL")
  #endif   /* ifndef __cplusplus                                 @A2M*/
  #pragma map(in6addr_any,     "IN6AD\174AN") /* new in 1.6 @Q1A @A1C*/
  #pragma map(in6addr_loopback,"IN6AD\174LB") /* new in 1.6 @Q1A @A1C*/

  /*-----------------------------------------------------------*/
  /* External API calls                                        */
  /*-----------------------------------------------------------*/
  #ifndef __cplusplus
  /******     endhostent - see RESOLVE_VIA_LOOKUP section */
  #pragma map(endnetent,            "ENDNETEN")
  #pragma map(endprotoent,          "ENDPROTO")
  #pragma map(endservent,           "ENDSERVE")
  #pragma map(getdtablesize,        "GETDTABL")
  /******     gethostent - see RESOLVE_VIA_LOOKUP section */
  #pragma map(getibmopt,            "GETIBMOP") /* new in 3.2 */
  #pragma map(getibmsockopt,        "GETIBMSO")
  #pragma map(getnetbyaddr,         "GETNETAD")
  #pragma map(getnetbyname,         "GETNETBY")
  #pragma map(getnetent,            "GETNETEN")
  #pragma map(getprotobyname,       "GETPROTO")
  #pragma map(getprotobynumber,     "GETPRNUM")
  #pragma map(getprotoent,          "GETPRENT")
  #pragma map(getservbyname,        "GETSERVB")
  #pragma map(getservbyport,        "GETSRVPT")
  #pragma map(getservent,           "GETSERVE")
  /******     htonl - see in.h *****/
  /******     htons - see in.h *****/
  #pragma map(ibmsflush,            "IB$SFLUS")
  #pragma map(inet_addr,            "INET\174ADD")            /* @A1C*/
  #pragma map(inet_lnaof,           "INET\174LNA")            /* @A1C*/
  #pragma map(inet_makeaddr,        "NTMKDR")
  #pragma map(inet_netof,           "NTNTF")
  #pragma map(inet_network,         "INET\174NET")            /* @A1C*/
  #pragma map(inet_ntoa,            "INET\174NTA")            /* @A1C*/
  #pragma map(maxdesc,              "MAXDESC")  /* new in 3.2 */
  /******     ntohl - see in.h *****/
  /******     ntohs - see in.h *****/
  /******     sethostent - see RESOLVE_VIA_LOOKUP section */
  #pragma map(setibmopt,            "SETIBMOP") /* new in 3.2 */
  #pragma map(setibmsockopt,        "SETIBMSO")
  #pragma map(setnetent,            "SETNETEN")
  #pragma map(setprotoent,          "SETPROTO")
  #pragma map(setservent,           "SETSERVE")
  #pragma map(sock_debug,           "SOCK\174DEB")            /* @A1C*/
  #pragma map(sock_debug_bulk_perf0,"SOCKDBP0")
  #pragma map(sock_do_bulkmode,     "SOCKDOBM")
  #pragma map(sock_do_teststor,     "SOCKDOTS")
  #pragma map(tcperror,             "TCPERROR")   /* new in 3.2 */
  #pragma map(tcpserror,            "TCPSERRO")   /* new in 3.2 */
  #endif     /* ifndef __cplusplus */
  #pragma map(readv,                "READV")      /* new in 3.2  @A2M*/
  #pragma map(recvmsg,              "RECVMSG")    /* new in 3.2  @A2M*/
  #pragma map(selectex,             "SELECTEX")   /* new in 3.2  @A2M*/
  #pragma map(sendmsg,              "SENDMSG")    /* new in 3.2  @A2M*/
  #pragma map(writev,               "WRITEV")     /* new in 3.2  @A2M*/

  /*-----------------------------------------------------------------*/
  /* External API calls that are in EZACIC07                     @A2A*/
  /*-----------------------------------------------------------------*/

  #pragma map(accept,               "ACCEPT")                 /* @A2M*/
  #pragma map(bind,                 "BIND")                   /* @A2M*/
  #pragma map(close,                "SOCK\174CLO")            /* @A2M*/
  #pragma map(connect,              "CONNECT")                /* @A2M*/
  #pragma map(fcntl,                "FCNTL")                  /* @A2M*/
  #pragma map(freeaddrinfo,         "FREADINF") /*new in R6 @Q1A @A2M*/
  #pragma map(gai_strerror,         "GAI\174STRE")  /* @Q1A @A1C @A2M*/
  #pragma map(getaddrinfo,          "GETADINF") /*new in R6 @Q1A @A2M*/
  #pragma map(getclientid,          "GETCLIEN")               /* @A2M*/
  #pragma map(gethostbyaddr,        "GTHSTBYA")               /* @A2M*/
  #pragma map(gethostbyname,        "GTHSTBYN")               /* @A2M*/
  #pragma map(gethostid,            "GETHOSTI")               /* @A2M*/
  #pragma map(gethostname,          "GETHNAME")               /* @A2M*/
  #pragma map(getnameinfo,          "GETNMINF") /*new in R6 @Q1A @A2M*/
  #pragma map(getpeername,          "GETPEERN")               /* @A2M*/
  #pragma map(getsockname,          "GTSCKNM")                /* @A2M*/
  #pragma map(getsockopt,           "GETSOCKO")               /* @A2M*/
  #pragma map(givesocket,           "GIVESOCK")               /* @A2M*/
  #pragma map(if_freenameindex,     "IF\174FRENI")  /* @Q1A @A1C @A2M*/
  #pragma map(if_indextoname,       "IF\174NMTIX")  /* @Q1A @A1C @A2M*/
  #pragma map(if_nameindex,         "IF\174NMNDX")  /* @Q1A @A1C @A2M*/
  #pragma map(if_nametoindex,       "IF\174IXTNM")  /* @Q1A @A1C @A2M*/
  #pragma map(inet_ntop,            "INET\174NTP")  /* @Q1A @A1C @A2M*/
  #pragma map(inet_pton,            "INET\174PTN")  /* @Q1A @A1C @A2M*/
  #pragma map(initapi,              "INITAPI")                /* @A2A*/
  #pragma map(ioctl,                "IOCTL")                  /* @A2M*/
  #pragma map(listen,               "LISTEN")                 /* @A2M*/
  #pragma map(read,                 "READ")                   /* @A2M*/
  #pragma map(recv,                 "RECV")                   /* @A2M*/
  #pragma map(recvfrom,             "RECVFROM")               /* @A2M*/
  #pragma map(select,               "SELECT")                 /* @A2M*/
  #pragma map(send,                 "SEND")                   /* @A2M*/
  #pragma map(sendto,               "SENDTO")                 /* @A2M*/
  #pragma map(setsockopt,           "SETSOCKO")               /* @A2M*/
  #pragma map(shutdown,             "SHUTDOWN")               /* @A2M*/
  #pragma map(socket,               "SOCKET")                 /* @A2M*/
  #pragma map(takesocket,           "TAKESOCK")               /* @A2M*/
  #pragma map(write,                "WRITE")                  /* @A2M*/

  #ifdef __CICS_SOCKETS                                       /* @Y1A*/
  #pragma map(setipv4sourcefilter,  "SET4SF")                 /* @Y1A*/
  #pragma map(getipv4sourcefilter,  "GET4SF")                 /* @Y1A*/
  #pragma map(setsourcefilter,      "SETSF")                  /* @Y1A*/
  #pragma map(getsourcefilter,      "GETSF")                  /* @Y1A*/
  #endif /* end of __CICS_SOCKETS */                          /* @Y1A*/

  /*-----------------------------------------------------------*/
  /* Functions beginning with "_" are used when                */
  /*    RESOLVE_VIA_LOOKUP is defined. This directs functions  */
  /*    to look up values in a file rather that using the      */
  /*    name server.                                           */
  /*-----------------------------------------------------------*/
  #ifndef __cplusplus
  #pragma map(_endhtent,            "\174ENDHTEN")            /* @A1C*/
  #pragma map(_gethostname,         "\174GETHOST")            /* @A1C*/
  #pragma map(_gethtent,            "\174GETHTEN")            /* @A1C*/
  #pragma map(_getshort,            "\174GETSHOR")            /* @A1C*/
  #pragma map(_net_stayopen,        "\174NET\174STA")         /* @A1C*/
  #pragma map(_proto_stayopen,      "\174PROTO\174S")         /* @A1C*/
  #pragma map(_res_close,           "\174RES\174CLO")         /* @A1C*/
  #pragma map(_res_opcodes,         "\174RES\174OPC")         /* @A1C*/
  #pragma map(_res_resultcodes,     "\174RES\174RES")         /* @A1C*/
  #pragma map(_serv_stayopen,       "\174SERV\174ST")         /* @A1C*/
  #pragma map(_sethtent,            "\174SETHTEN")            /* @A1C*/
  #endif     /* ifndef __cplusplus */

  /*-----------------------------------------------------------*/
  /* Special definitions to force name resolution by           */
  /*     table lookup.                                         */
  /*-----------------------------------------------------------*/
  #ifdef RESOLVE_VIA_LOOKUP
  #ifndef __cplusplus
  #pragma map(endhostent,           "\174ENDHTEN")            /* @A1C*/
  #pragma map(gethostent,           "\174GETHTEN")            /* @A1C*/
  #pragma map(gthstbya,             "\174GTHBYAD")            /* @A1C*/
  #pragma map(gthstbyn,             "\174GTHBYNM")            /* @A1C*/
  #pragma map(sethostent,           "\174SETHTEN")            /* @A1C*/
  #endif  /* ifndef __cplusplus */
  #endif

  /* Long DPILIB names */

  #ifndef __cplusplus
  #pragma map(cDPIpacket,           "CDPIPACK")
  #pragma map(DPIdebug,             "DPIDEBUG")
  #pragma map(fDPIparse,            "FDPIPARS")
  #pragma map(lookup_host,          "LKPHST")
  #pragma map(mkDPIlist,            "MKDPILIS")
  #pragma map(mkDPIregister,        "MKDPIREG")
  #pragma map(mkDPIresponse,        "MKDPIRES")
  #pragma map(mkDPIset,             "MKDPISET")
  #pragma map(mkDPItrap,            "MKDPITRA")
  #pragma map(mkDPItrape,           "MKDPITRE")
  #pragma map(mkMIBquery,           "MKMIBQRY")
  #pragma map(pDPIpacket,           "PDPIPKT")
  #pragma map(query_DPI_port,       "QUERY\174DP")            /* @A1C*/
  #endif     /* ifndef __cplusplus */

#endif
#pragma checkout(resume)

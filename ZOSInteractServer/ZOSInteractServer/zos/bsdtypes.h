                   ??=ifdef __COMPILER_VER__
                     ??=pragma filetag ("IBM-1047")
                   ??=endif
                   #pragma nomargins nosequence
                   #pragma checkout(suspend)
#ifndef _H_BSDTYPES
    #define _H_BSDTYPES 1
/* $Header:types.h 9.1$ */
/* $ACIS:types.h 9.1$ */
/* $Source: /ibm/acis/usr/sys/h/RCS/types.h,v $ */

#ifndef _H_TYPES_
#define  _H_TYPES_

#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
static char *rcsidtypes = "$Header:types.h 9.1$";
#endif

/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *  @(#)types.h    7.1 (Berkeley) 6/4/86
 */

/*** IBMCOPYR ********************************************************/
/*                                                                   */
/* Copyright:                                                        */
/*   Licensed Materials - Property of IBM                            */
/*   This product contains "Restricted Materials of IBM"             */
/*   5735-FAL (C) Copyright IBM Corp. 1991.                          */
/*   5655-HAL (C) Copyright IBM Corp. 1991, 1996.                    */
/*   All rights reserved.                                            */
/*   US Government Users Restricted Rights -                         */
/*   Use, duplication or disclosure restricted by GSA ADP Schedule   */
/*   Contract with IBM Corp.                                         */
/*   See IBM Copyright Instructions.                                 */
/*                                                                   */
/* Status:                                                           */
/*   Version 3, Release 2, Level 0                                   */
/*********************************************************************/
/*                                                                   */
/* $MOD(res_init), COMP(DNS), PROD(TCPIP) :                          */
/*                                                                   */
/*                                                                   */
/* Flag Reason   Release  Date   Origin    Description               */
/* ---- -------- -------- ------ --------  --------------------------*/
/* $L1= D312.22  CSV1R2   000927 Majikesj: Allow resolv.h            */
/*                                                                   */
/*********************************************************************/
/*** IBMCOPYR ********************************************************/

/*
 * Basic system types and major/minor device constructing/busting\
 macros.
 */

/* major part of a device */
#define  major(x)  ((int)(((unsigned)(x)>>8)&0377))

/* minor part of a device */
#define  minor(x)  ((int)((x)&0377))

/* make a device number */
#define  makedev(x,y)   ((dev_t)(((x)<<8) | (y)))

typedef  unsigned char  u_char;
#define __u_char 1                /* needed for resolv.h         @L1A*/
typedef  unsigned short u_short;
#define __u_short 1               /* needed for resolv.h         @L1A*/
typedef  unsigned int   u_int;
#define __u_int  1                /* needed for resolv.h         @L1A*/
typedef  unsigned long  u_long;
#define __u_long 1                /* needed for resolv.h         @L1A*/
typedef  unsigned short ushort;        /* sys III compat */

#ifdef vax
typedef  struct    _physadr { int r[1]; } *physadr;
typedef  struct    label_t   {
    int  val[14];
} label_t;
#endif /* vax */
#if defined(ibm032) || defined(ibm370)
typedef  struct    _physadr { int r[1]; } *physadr;
typedef  struct    label_t   {
    int  val[11];
} label_t;
#endif /* defined(ibm032) || defined(ibm370) */

typedef  struct    _quad { long val[2]; } quad;
typedef  long daddr_t;
typedef  char *    caddr_t;
typedef  u_long    ino_t;
typedef  long swblk_t;
/* typedef  long size_t; */
/* typedef  long time_t; */
typedef  short     dev_t;
typedef  long off_t;
typedef  u_short   uid_t;
typedef  u_short   gid_t;
#ifdef __STDC__
typedef signed char prio_t;  /* priorities must be signed */
#else
typedef char prio_t;
#endif

#define  NBBY 8         /* number of bits in a byte */
/*
 * Select uses bit masks of file descriptors in longs.
 * These macros manipulate such bit fields (the filesystem macros use\
 chars).
 * FD_SETSIZE may be defined by the user, but the default here
 * should be >= NOFILE (param.h).
 * If FD_SETSIZE_MAX is defined, set FD_SETSIZE to this value.
 * socket.h defines this value to be the maximum number of
 * sockets supported.
 */
#ifndef  FD_SETSIZE
#  ifdef  FD_SETSIZE_MAX
#    define  FD_SETSIZE     FD_SETSIZE_MAX
#  else
#    define  FD_SETSIZE     256
#  endif
#endif

typedef long  fd_mask;
#define NFDBITS    (sizeof(fd_mask) * NBBY) /* bits per mask */
#ifndef howmany
#define  howmany(x, y)  (((x)+((y)-1))/(y))
#endif

typedef  struct fd_set {
    fd_mask   fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define  FD_SET(n, p)   ((p)->fds_bits[((unsigned)n)/NFDBITS] \
                        |= (1 << (((unsigned)n) % NFDBITS)))
#define  FD_CLR(n, p)   ((p)->fds_bits[((unsigned)n)/NFDBITS] \
                        &= ~(1 << (((unsigned)n) % NFDBITS)))
#define  FD_ISSET(n, p) ((p)->fds_bits[((unsigned)n)/NFDBITS] \
                        & (1 << (((unsigned)n) % NFDBITS)))
#define FD_ZERO(p) bzero((char *)(p), sizeof(*(p)))

#ifdef ibm032
/*
 * build a queue type appropriate to the particular machine
 * for historical reasons we use "caddr_t" on non-ibm032 machines.
 */
typedef struct qhdr {
    struct qhdr *link, *rlink;
} *queue_t;
#else /* ibm032 */
typedef caddr_t queue_t;
#endif /* ibm032 */
#endif
#endif /* _H_BSDTYPES */
#pragma checkout(resume)

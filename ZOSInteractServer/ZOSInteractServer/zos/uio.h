                   ??=ifdef __COMPILER_VER__
                     ??=pragma filetag ("IBM-1047")
                   ??=endif
                   #pragma nomargins nosequence
                   #pragma checkout(suspend)
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *  @(#)uio.h   7.1 (Berkeley) 6/4/86
 */

/*** IBMCOPYR ********************************************************/
/*                                                                   */
/* Copyright:                                                        */
/*   Licensed Materials - Property of IBM                            */
/*   This product contains "Restricted Materials of IBM"             */
/*   5735-FAL (C) Copyright IBM Corp. 1991.                          */
/*   5655-HAL (C) Copyright IBM Corp. 1991, 1994.                    */
/*   All rights reserved.                                            */
/*   US Government Users Restricted Rights -                         */
/*   Use, duplication or disclosure restricted by GSA ADP Schedule   */
/*   Contract with IBM Corp.                                         */
/*   See IBM Copyright Instructions.                                 */
/*                                                                   */
/* Status:                                                           */
/*   TCP/IP for MVS                                                  */
/*                                                                   */
/*** IBMCOPYR ********************************************************/

#ifndef _UIO_
#define _UIO_

struct iovec {
    caddr_t iov_base;
    int iov_len;
};

struct uio {
    struct  iovec *uio_iov;
    int uio_iovcnt;
    off_t   uio_offset;
    int uio_segflg;
    int uio_resid;
};

enum    uio_rw { UIO_READ, UIO_WRITE };

/*
 * Segment flag values (should be enum).
 */
#define UIO_USERSPACE   0       /* from user data space */
#define UIO_SYSSPACE    1       /* from system space */
#define UIO_USERISPACE  2       /* from user I space */
#endif
#pragma checkout(resume)

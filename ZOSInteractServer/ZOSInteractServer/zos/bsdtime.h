                   ??=ifdef __COMPILER_VER__
                     ??=pragma filetag ("IBM-1047")
                   ??=endif
                   #pragma nomargins nosequence
                   #pragma checkout(suspend)
/*** IBMCOPYR ********************************************************/
/*                                                                   */
/* Copyright:                                                        */
/*   Licensed Materials - Property of IBM                            */
/*   This product contains "Restricted Materials of IBM"             */
/*   5694-A01    Copyright IBM Corp. 1991, 2008                      */
/*   All rights reserved.                                            */
/*   US Government Users Restricted Rights -                         */
/*   Use, duplication or disclosure restricted by GSA ADP Schedule   */
/*   Contract with IBM Corp.                                         */
/*   See IBM Copyright Instructions.                                 */
/*                                                                   */
/* Status:                                                           */
/*   CSV1R10                                                         */
/*                                                                   */
/*** IBMCOPYR ********************************************************/

#ifndef _TYPES_BSDTIME_
#define _TYPES_BSDTIME_

#ifdef __CICS_SOCKETS
#if !defined(__time_t2)

      #define __time_t2    1

      /*
       *
       * C/370 already defines as long
       *
       */
      typedef long time_t ;                 /* arithmetic */

#endif
struct timeval {
time_t tv_sec; /* seconds */
long   tv_usec; /* and microseconds */
};
#else
struct timeval {
        long    tv_sec;         /* seconds */
        long    tv_usec;        /* and microseconds */
};
#endif /* end of __CICS_SCOKETS */
#endif
#pragma checkout(resume)

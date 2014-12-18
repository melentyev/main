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

/*
 * @(#)BSDtoCMS.h   2.1 87/04/30 12:43:54
 *
 *  BSDtoCMS.h - BSD to CMS porting include file
 *
 *  This include file is included by every file in /usr/include/bsd
 * (except itself) to cover the most common BSDisms for porting code
 * to CMS.
 */

#ifndef _H_BSDtoCMS
#define _H_BSDtoCMS

/* for those V7 isms regarding bytes */
#define bzero(t,s)  (memset((t),(0),(s)))
#define bcmp(f,t,s) (memcmp((t),(f),(s)))
#define bcopy(S, D, L)   (memcpy((D), (S), (L)))


#endif

#pragma checkout(resume)

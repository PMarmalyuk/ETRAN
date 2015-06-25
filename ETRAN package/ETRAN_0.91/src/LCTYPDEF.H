/*****************************************************************************

File Name:       LCTYPDEF.H
                 Commonly used data type abbreviations

Company:         LC Technologies, Inc.
                 9455 Silver King Court
                 Fairfax, VA 22031
                 (703) 385-7133

*****************************************************************************/
#ifndef LCTYPDEF_H_INCLUDED
#define LCTYPDEF_H_INCLUDED
/****************************************************************************/

#include <windows.h>
#include <tchar.h>

/****************************************************************************/
/* Variable Type Definitions:                                               */

#define  VOID    void
#define  CHAR    TCHAR
#define  INT     int
#define  BOOL    int
#define  LONG    long
#define  FLOAT   float
#define  DOUBLE  double
#define  BYTE    unsigned char
#define  UCHAR   _TUCHAR
#define  UINT    unsigned int
#define  USHORT  unsigned short  //xxx defined in windef.h
#define  ULONG   unsigned long   //xxx defined in windef.h
#define  STRUCT  struct

#define  SVOID   static void
#define  SCHAR   static TCHAR
#define  SINT    static int
#define  SBOOL   static int
#define  SFLOAT  static float
#define  SBYTE   static unsigned char
#define  SUCHAR  static _TUCHAR
#define  SUINT   static unsigned int
#define  SSTRUCT static struct

/****************************************************************************/
#endif   // LCTYPDEF_H_INCLUDED
/****************************************************************************/

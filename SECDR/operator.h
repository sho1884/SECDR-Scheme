/*
 * operator.h
 *
 * Coded by Atsushi Moriwaki.
 * Revised by Shoichi Hayashi.
 *
 */
#ifndef THINK_C
static char *rcsohid = 
"$Header: /usr/PDS/lang/Scheme/SECDR/RCS/operator.h,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $";
#endif
/* =====================================================================
 *
    Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    The author can be reached at s-haya@rst.fujixerox.co.jp or
    Shoichi Hayashi,
    SYSTEMS & COMMUNICATIONS LAB.  FUJI XEROX Co.,LTD.
    134 GODOCHO, HODOGAYA-KU, YOKOHAMA-SHI, KANAGAWA, 240, JAPAN
*/

/* Machine Transition Operator */
#define OP_LD        0
#define OP_TLD       1
#define OP_GLD       2
#define OP_LDC       3
#define OP_LDF       4
#define OP_AP        5
#define OP_TAP       6
#define OP_PUSH      7
#define OP_RTN       8
#define OP_SEL       9
#define OP_TSEL     10
#define OP_ASSIG    11
#define OP_TASSIG   12
#define OP_GASSIG   13
#define OP_DEF      14
#define OP_PUSHCONS 15
#define OP_SAVE     16
#define OP_EXEC     17
#define OP_STOP     18

/* numerical operators */
#define NUMOP_ADD    1
#define NUMOP_SUB    2
#define NUMOP_MUL    3
#define NUMOP_DIV    4
#define NUMOP_QUO    5
#define NUMOP_REM    6
#define NUMOP_EXP    7
#define NUMOP_LOG    8
#define NUMOP_SIN    9
#define NUMOP_COS    10
#define NUMOP_TAN    11
#define NUMOP_ASIN   12
#define NUMOP_ACOS   13
#define NUMOP_ATAN   14
#define NUMOP_SQRT   15
#define NUMOP_EXPT   16
#define NUMOP_ZEROP  17
#define NUMOP_POSP   18
#define NUMOP_NEGP   19
#define NUMOP_NEQ    20
#define NUMOP_LESS   21
#define NUMOP_GRE    22
#define NUMOP_LEQ    23
#define NUMOP_GEQ    24
#define NUMOP_FLOOR  25
#define NUMOP_ONEPLUS 26
#define NUMOP_ONEMINUS 27

#define NUMOP_MOD 30
#define NUMOP_ABS 31
#define NUMOP_MIN 32
#define NUMOP_MAX 33
#define NUMOP_EVEN 34
#define NUMOP_ODD 35
#define NUMOP_CEIL 36
#define NUMOP_TRUN 37
#define NUMOP_ROUND 38

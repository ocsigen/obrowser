///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  Caml Virtual Machine in JavaScript                                       //
//  (C) 2007 Benjamin Canou (Benjamin.Canou@gmail.com)                       //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  This program is free software: you can redistribute it and/or modify     //
//  it under the terms of the GNU General Public License as published by     //
//  the Free Software Foundation, either version 3 of the License, or        //
//  (at your option) any later version.                                      //
//                                                                           //
//  This program is distributed in the hope that it will be useful,          //
//  but WITHOUT ANY WARRANTY; without even the implied warranty of           //
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            //
//  GNU General Public License for more details.                             //
//                                                                           //
//  You should have received a copy of the GNU General Public License        //
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.    //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  INSTRUCTION CODES                                                        //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#define IACC0                       0
#define IACC1                       1
#define IACC2                       2
#define IACC3                       3
#define IACC4                       4
#define IACC5                       5
#define IACC6                       6
#define IACC7                       7
#define IACC                        8
#define IPUSH                       9
#define IPUSHACC0                   10
#define IPUSHACC1                   11
#define IPUSHACC2                   12
#define IPUSHACC3                   13
#define IPUSHACC4                   14
#define IPUSHACC5                   15
#define IPUSHACC6                   16
#define IPUSHACC7                   17
#define IPUSHACC                    18
#define IPOP                        19
#define IASSIGN                     20
#define IENVACC1                    21
#define IENVACC2                    22
#define IENVACC3                    23
#define IENVACC4                    24
#define IENVACC                     25
#define IPUSHENVACC1                26
#define IPUSHENVACC2                27
#define IPUSHENVACC3                28
#define IPUSHENVACC4                29
#define IPUSHENVACC                 30
#define IPUSH_RETADDR               31
#define IAPPLY                      32
#define IAPPLY1                     33
#define IAPPLY2                     34
#define IAPPLY3                     35
#define IAPPTERM                    36
#define IAPPTERM1                   37
#define IAPPTERM2                   38
#define IAPPTERM3                   39
#define IRETURN                     40
#define IRESTART                    41
#define IGRAB                       42
#define ICLOSURE                    43
#define ICLOSUREREC                 44
#define IOFFSETCLOSUREM2            45
#define IOFFSETCLOSURE0             46
#define IOFFSETCLOSURE2             47
#define IOFFSETCLOSURE              48
#define IPUSHOFFSETCLOSUREM2        49
#define IPUSHOFFSETCLOSURE0         50
#define IPUSHOFFSETCLOSURE2         51
#define IPUSHOFFSETCLOSURE          52
#define IGETGLOBAL                  53
#define IPUSHGETGLOBAL              54
#define IGETGLOBALFIELD             55
#define IPUSHGETGLOBALFIELD         56
#define ISETGLOBAL                  57
#define IATOM0                      58
#define IATOM                       59
#define IPUSHATOM0                  60
#define IPUSHATOM                   61
#define IMAKEBLOCK                  62
#define IMAKEBLOCK1                 63
#define IMAKEBLOCK2                 64
#define IMAKEBLOCK3                 65
#define IMAKEFLOATBLOCK             66
#define IGETFIELD0                  67
#define IGETFIELD1                  68
#define IGETFIELD2                  69
#define IGETFIELD3                  70
#define IGETFIELD                   71
#define IGETFLOATFIELD              72
#define ISETFIELD0                  73
#define ISETFIELD1                  74
#define ISETFIELD2                  75
#define ISETFIELD3                  76
#define ISETFIELD                   77
#define ISETFLOATFIELD              78
#define IVECTLENGTH                 79
#define IGETVECTITEM                80
#define ISETVECTITEM                81
#define IGETSTRINGCHAR              82
#define ISETSTRINGCHAR              83
#define IBRANCH                     84
#define IBRANCHIF                   85
#define IBRANCHIFNOT                86
#define ISWITCH                     87
#define IBOOLNOT                    88
#define IPUSHTRAP                   89
#define IPOPTRAP                    90
#define IRAISE                      91
#define ICHECK_SIGNALS              92
#define IJS_CALL1                   93
#define IJS_CALL2                   94
#define IJS_CALL3                   95
#define IJS_CALL4                   96
#define IJS_CALL5                   97
#define IJS_CALLN                   98
#define ICONST0                     99
#define ICONST1                     100
#define ICONST2                     101
#define ICONST3                     102
#define ICONSTINT                   103
#define IPUSHCONST0                 104
#define IPUSHCONST1                 105
#define IPUSHCONST2                 106
#define IPUSHCONST3                 107
#define IPUSHCONSTINT               108
#define INEGINT                     109
#define IADDINT                     110
#define ISUBINT                     111
#define IMULINT                     112
#define IDIVINT                     113
#define IMODINT                     114
#define IANDINT                     115
#define IORINT                      116
#define IXORINT                     117
#define ILSLINT                     118
#define ILSRINT                     119
#define IASRINT                     120
#define IEQ                         121
#define INEQ                        122
#define ILTINT                      123
#define ILEINT                      124
#define IGTINT                      125
#define IGEINT                      126
#define IOFFSETINT                  127
#define IOFFSETREF                  128
#define IISINT                      129
#define IGETMETHOD                  130
#define IBEQ                        131
#define IBNEQ                       132
#define IBLTINT                     133
#define IBLEINT                     134
#define IBGTINT                     135
#define IBGEINT                     136
#define IULTINT                     137
#define IUGEINT                     138
#define IBULTINT                    139
#define IBUGEINT                    140
#define IGETPUBMET                  141
#define IGETDYNMET                  142
#define ISTOP                       143
#define IEVENT                      144
#define IBREAK                      145

var instr_name = new Array (
    "ACC0", "ACC1", "ACC2", "ACC3", "ACC4", "ACC5", "ACC6", "ACC7", "ACC",
    "PUSH", "PUSHACC0", "PUSHACC1", "PUSHACC2", "PUSHACC3", "PUSHACC4",
    "PUSHACC5", "PUSHACC6", "PUSHACC7", "PUSHACC", "POP", "ASSIGN", "ENVACC1",
    "ENVACC2", "ENVACC3", "ENVACC4", "ENVACC", "PUSHENVACC1", "PUSHENVACC2",
    "PUSHENVACC3", "PUSHENVACC4", "PUSHENVACC", "PUSH_RETADDR", "APPLY",
    "APPLY1", "APPLY2", "APPLY3", "APPTERM", "APPTERM1", "APPTERM2", "APPTERM3",
    "RETURN", "RESTART", "GRAB", "CLOSURE", "CLOSUREREC", "OFFSETCLOSUREM2",
    "OFFSETCLOSURE0", "OFFSETCLOSURE2", "OFFSETCLOSURE", "PUSHOFFSETCLOSUREM2",
    "PUSHOFFSETCLOSURE0", "PUSHOFFSETCLOSURE2", "PUSHOFFSETCLOSURE",
    "GETGLOBAL", "PUSHGETGLOBAL", "GETGLOBALFIELD", "PUSHGETGLOBALFIELD",
    "SETGLOBAL", "ATOM0", "ATOM", "PUSHATOM0", "PUSHATOM", "MAKEBLOCK",
    "MAKEBLOCK1", "MAKEBLOCK2", "MAKEBLOCK3", "MAKEFLOATBLOCK", "GETFIELD0",
    "GETFIELD1", "GETFIELD2", "GETFIELD3", "GETFIELD", "GETFLOATFIELD",
    "SETFIELD0", "SETFIELD1", "SETFIELD2", "SETFIELD3", "SETFIELD",
    "SETFLOATFIELD", "VECTLENGTH", "GETVECTITEM", "SETVECTITEM",
    "GETSTRINGCHAR", "SETSTRINGCHAR", "BRANCH", "BRANCHIF", "BRANCHIFNOT",
    "SWITCH", "BOOLNOT", "PUSHTRAP", "POPTRAP", "RAISE", "CHECK_SIGNALS",
    "JS_CALL1", "JS_CALL2", "JS_CALL3", "JS_CALL4", "JS_CALL5", "JS_CALLN",
    "CONST0", "CONST1", "CONST2", "CONST3", "CONSTINT", "PUSHCONST0",
    "PUSHCONST1", "PUSHCONST2", "PUSHCONST3", "PUSHCONSTINT", "NEGINT",
    "ADDINT", "SUBINT", "MULINT", "DIVINT", "MODINT", "ANDINT", "ORINT",
    "XORINT", "LSLINT", "LSRINT", "ASRINT", "EQ", "NEQ", "LTINT", "LEINT",
    "GTINT", "GEINT", "OFFSETINT", "OFFSETREF", "ISINT", "GETMETHOD", "BEQ",
    "BNEQ", "BLTINT", "BLEINT", "BGTINT", "BGEINT", "ULTINT", "UGEINT",
    "BULTINT", "BUGEINT", "GETPUBMET", "GETDYNMET", "STOP", "EVENT", "BREAK"
);

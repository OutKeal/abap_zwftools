*&---------------------------------------------------------------------*
*&  Include           ZLJW_TRANS_TOP
*&---------------------------------------------------------------------*

TABLES: TRDIR,
  TLIBG,
  TADIR,
  T100,
  DD01L,
  DD04L,
  ZWFT_CAG.

*----------------------------------------------------------------------*
* IT & STRUCTURE
*----------------------------------------------------------------------*

DATA: BEGIN OF GT_M OCCURS 0.
  INCLUDE  STRUCTURE LXE_PCX_S1.
  DATA:   OBJTYPE  LIKE LXE_COLOB-OBJTYPE,  "LXE_ATTOB-OBJ_TYPE,
        OBJNAME  LIKE LXE_COLOB-OBJNAME,
        META_OBJTYPE LIKE LXE_ATTOB-META_OBJTYPE,
        ICO_SEL  LIKE ICON-ID,                     "####Ǯ #ȸ
        MARK.
DATA: END OF GT_M.

DATA: BEGIN OF GT_110 OCCURS 0.
  INCLUDE STRUCTURE ZWFT_CAG.
  DATA:   ICO_SEL LIKE ICON-ID,           "### Main## ####
        EDIT_GB,                        "#######
        MARK.
DATA: END OF GT_110.

DATA: BEGIN OF GT_LXE OCCURS 0,
  OBJECT   TYPE LXEOBJNAME ,
  OBJTYPE  TYPE LXEOBJTYPE,
END OF GT_LXE.

DATA : BEGIN OF GT_MESS OCCURS 0,
  ARBGB  LIKE T100-ARBGB  ,
  MSGNR  LIKE T100-MSGNR  ,
END OF GT_MESS.

*----------------------------------------------------------------------*
* ########
*----------------------------------------------------------------------*
CONSTANTS: C_CUSTMNR TYPE LXECUSTMNR VALUE '999999'.

DATA: G_OKCD  LIKE SY-UCOMM,
      G_UCOM  LIKE SY-UCOMM,
      G_ERROR.

DATA: G_SLANG     TYPE  LXEISOLANG,
      G_TLANG     TYPE  LXEISOLANG,
      G_2LANG     TYPE  LXEISOLANG.

DATA: G_ONLY_DEVCLS,                "####Ŭ#### #˻###Ǹ# #ԷµǾ## ####
      G_CLK_TABIX TYPE SY-TABIX,    "MAIN ȭ#鿡## ####Ǯ #ȸ ###### Ŭ#### ##
      G_PGM_TITLE TYPE TRDIRT-TEXT.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: PM_SLANG LIKE SY-LANGU OBLIGATORY DEFAULT '1' MATCHCODE OBJECT H_T002,
  PM_TLANG LIKE SY-LANGU OBLIGATORY DEFAULT 'E' MATCHCODE OBJECT H_T002.  "
*            pm_zlang LIKE sy-langu OBLIGATORY DEFAULT '1' MATCHCODE OBJECT h_t002.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-200.
  SELECT-OPTIONS: SO_PROG  FOR TRDIR-NAME,    " MATCHCODE OBJECT PROGNAME,
  SO_AREA  FOR TLIBG-AREA,    "FUNCTION_GROUP
  SO_MSAG  FOR T100-ARBGB,
  SO_DTEL  FOR DD04L-ROLLNAME,
  SO_DOMA  FOR DD01L-DOMNAME,
  SO_CLASS FOR TADIR-DEVCLASS.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
  SELECT-OPTIONS: SO_TEXT FOR ZWFT_CAG-TXT_KO NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 1100.

*SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE TEXT-002.
*PARAMETERS: PM_EXCEL AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK B7.

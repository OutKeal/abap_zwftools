*&---------------------------------------------------------------------*
*&  Include           ZLJW_TRANS_TOP
*&---------------------------------------------------------------------*

TABLES: trdir,
        tlibg,
        tadir,
        t100,
        dd01l,
        dd04l,
        zwft_cag.

*----------------------------------------------------------------------*
* IT & STRUCTURE
*----------------------------------------------------------------------*

DATA: BEGIN OF gt_m OCCURS 0.
        INCLUDE  STRUCTURE lxe_pcx_s1.
DATA: objtype      LIKE lxe_colob-objtype,  "LXE_ATTOB-OBJ_TYPE,
        objname      LIKE lxe_colob-objname,
        meta_objtype LIKE lxe_attob-meta_objtype,
        ico_sel      LIKE icon-id,                     "####Ǯ #ȸ
        mark.
DATA: END OF gt_m.

DATA: BEGIN OF gt_110 OCCURS 0.
        INCLUDE STRUCTURE zwft_cag.
DATA: ico_sel LIKE icon-id,           "### Main## ####
        edit_gb,                        "#######
        mark.
DATA: END OF gt_110.

DATA: BEGIN OF gt_lxe OCCURS 0,
        object  TYPE lxeobjname,
        objtype TYPE lxeobjtype,
      END OF gt_lxe.

DATA : BEGIN OF gt_mess OCCURS 0,
         arbgb LIKE t100-arbgb,
         msgnr LIKE t100-msgnr,
       END OF gt_mess.

*----------------------------------------------------------------------*
* ########
*----------------------------------------------------------------------*
CONSTANTS: c_custmnr TYPE lxecustmnr VALUE '999999'.

DATA: g_okcd  LIKE sy-ucomm,
      g_ucom  LIKE sy-ucomm,
      g_error.

DATA: g_slang TYPE  lxeisolang,
      g_tlang TYPE  lxeisolang,
      g_2lang TYPE  lxeisolang.

DATA: g_only_devcls,                "####Ŭ#### #˻###Ǹ# #ԷµǾ## ####
      g_clk_tabix   TYPE sy-tabix,    "MAIN ȭ#鿡## ####Ǯ #ȸ ###### Ŭ#### ##
      g_pgm_title   TYPE trdirt-text.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: pm_slang LIKE sy-langu OBLIGATORY DEFAULT '1' MATCHCODE OBJECT h_t002,
              pm_tlang LIKE sy-langu OBLIGATORY DEFAULT 'E' MATCHCODE OBJECT h_t002.  "
*            pm_zlang LIKE sy-langu OBLIGATORY DEFAULT '1' MATCHCODE OBJECT h_t002.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-200.
  SELECT-OPTIONS: so_prog  FOR trdir-name,    " MATCHCODE OBJECT PROGNAME,
  so_area  FOR tlibg-area,    "FUNCTION_GROUP
  so_msag  FOR t100-arbgb,
  so_dtel  FOR dd04l-rollname,
  so_doma  FOR dd01l-domname,
  so_class FOR tadir-devclass.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
  SELECT-OPTIONS: so_text FOR zwft_cag-txt_ko NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 1100.

*SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE TEXT-002.
*PARAMETERS: PM_EXCEL AS CHECKBOX.
*SELECTION-SCREEN END OF BLOCK B7.

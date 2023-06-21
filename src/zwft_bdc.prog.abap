*&---------------------------------------------------------------------*
*& Report  ZMMB000
*&
*&---------------------------------------------------------------------*
*& BDC batch input
*& Created by QZZhang
*&---------------------------------------------------------------------*

REPORT  ZMMB000 NO STANDARD PAGE HEADING.
TABLES: SSCRFIELDS,T100.
**====================================================================*
*
**<*> NOTES:
*> Titles and headers: Programmable Logic Data Interface (PLDI)
*> Selection texts:
* DISMOD: * Call mode
* EXT: Extended logic
* FILE1: Data file  (Tab delimited)
* FILE2: Logic Level 1 (Tab delimited
* FILE3: Logic Level 2 (Tab delimited)
* FILE_EXT: Extended logic (Tab delimited)
* SESSION: * Session name
* START: Data start at row
* TCODE: Transaction code
* MASK: Mask

*> Text symbols
* 001: Batch Input (BDC Session)
* 002: PLDI was created on June.27.1997 by budi_rachman@yahoo.com
* 003: Release 3.2
* 004: Call transaction
* 005: Max. 64 columns & 128 characters per cell
* 006: Max. 128 columns & 64 characters per cell
* 007: Max. 30 columns & 40 characters per cell
* 008: Data Structure
* 009: Max. 234 columns & 35 characters per cell
**====================================================================*
*

*$*$*******************************************************************
*$*$*                  Data declarations                              *
*$*$*******************************************************************

* Structure Max.30 cols and 40 chars per cell
DATA: BEGIN OF  T30X040 OCCURS 100,
  B01(40), U01(40), D01(40), I01(40), R01(40),
  B02(40), U02(40), D02(40), I02(40), R02(40),
  B03(40), U03(40), D03(40), I03(40), R03(40),
  B04(40), U04(40), D04(40), I04(40), R04(40),
  B05(40), U05(40), D05(40), I05(40), R05(40),
  B06(40), U06(40), D06(40), I06(40), R06(40),
END OF T30X040.

* Structure Max.128 cols and 64 chars per cell
DATA: BEGIN OF T128X64 OCCURS 1024,
  B001(64), R001(64), A001(64), C001(64),
  B002(64), R002(64), A002(64), C002(64),
  B003(64), R003(64), A003(64), C003(64),
  B004(64), R004(64), A004(64), C004(64),
  B005(64), R005(64), A005(64), C005(64),
  B006(64), R006(64), A006(64), C006(64),
  B007(64), R007(64), A007(64), C007(64),
  B008(64), R008(64), A008(64), C008(64),
  B009(64), R009(64), A009(64), C009(64),
  B010(64), R010(64), A010(64), C010(64),
  B011(64), R011(64), A011(64), C011(64),
  B012(64), R012(64), A012(64), C012(64),
  B013(64), R013(64), A013(64), C013(64),
  B014(64), R014(64), A014(64), C014(64),
  B015(64), R015(64), A015(64), C015(64),
  B016(64), R016(64), A016(64), C016(64),
  B017(64), R017(64), A017(64), C017(64),
  B018(64), R018(64), A018(64), C018(64),
  B019(64), R019(64), A019(64), C019(64),
  B020(64), R020(64), A020(64), C020(64),
  B021(64), R021(64), A021(64), C021(64),
  B022(64), R022(64), A022(64), C022(64),
  B023(64), R023(64), A023(64), C023(64),
  B024(64), R024(64), A024(64), C024(64),
  B025(64), R025(64), A025(64), C025(64),
  B026(64), R026(64), A026(64), C026(64),
  B027(64), R027(64), A027(64), C027(64),
  B028(64), R028(64), A028(64), C028(64),
  B029(64), R029(64), A029(64), C029(64),
  B030(64), R030(64), A030(64), C030(64),
  B031(64), R031(64), A031(64), C031(64),
  B032(64), R032(64), A032(64), C032(64),
END OF T128X64.

* Structure Max.64 cols and 128 chars per cell
DATA: BEGIN OF T64X128 OCCURS 1024,
  B001(128), R001(128), A001(128), C001(128),
  B002(128), R002(128), A002(128), C002(128),
  B003(128), R003(128), A003(128), C003(128),
  B004(128), R004(128), A004(128), C004(128),
  B005(128), R005(128), A005(128), C005(128),
  B006(128), R006(128), A006(128), C006(128),
  B007(128), R007(128), A007(128), C007(128),
  B008(128), R008(128), A008(128), C008(128),
  B009(128), R009(128), A009(128), C009(128),
  B010(128), R010(128), A010(128), C010(128),
  B011(128), R011(128), A011(128), C011(128),
  B012(128), R012(128), A012(128), C012(128),
  B013(128), R013(128), A013(128), C013(128),
  B014(128), R014(128), A014(128), C014(128),
  B015(128), R015(128), A015(128), C015(128),
  B016(128), R016(128), A016(128), C016(128),
END OF T64X128.

* Structure Max.234 cols and 35 chars per cell
DATA: BEGIN OF T234X35 OCCURS 1024,
      F01(35), A01(35),R01(35), A76(35), B01(35), I01(35),
      F02(35), A02(35),R02(35), A77(35), B02(35), I02(35),
      F03(35), A03(35),R03(35), A78(35), B03(35), I03(35),
      F04(35), A04(35),R04(35), A40(35), B04(35), I04(35),
      F05(35), A05(35),R05(35), A41(35), B05(35), I05(35),
      F06(35), A06(35),R06(35), A42(35), B06(35), I06(35),
      F07(35), A07(35),R07(35), A43(35), B07(35), I07(35),
      F08(35), A08(35),R08(35), A44(35), B08(35), I08(35),
      F09(35), A09(35),R09(35), A45(35), B09(35), I09(35),
      F10(35), A10(35),R10(35), A46(35), B10(35), I10(35),
      F11(35), A11(35),R11(35), A47(35), B11(35), I11(35),
      F12(35), A12(35),R12(35), A48(35), B12(35), I12(35),
      F13(35), A13(35),R13(35), A49(35), B13(35), I13(35),
      F14(35), A14(35),R14(35), A50(35), B14(35), I14(35),
      F15(35), A15(35),R15(35), A51(35), B15(35), I15(35),
      F16(35), A16(35),R16(35), A52(35), B16(35), I16(35),
      F17(35), A17(35),R17(35), A53(35), B17(35), I17(35),
      F18(35), A18(35),R18(35), A54(35), B18(35), I18(35),
      F19(35), A19(35),R19(35), A55(35), B19(35), I19(35),
      F20(35), A20(35),R20(35), A56(35), B20(35), I20(35),
      F21(35), A21(35),R21(35), A57(35), B21(35), I21(35),
      F22(35), A22(35),R22(35), A58(35), B22(35), I22(35),
      F23(35), A23(35),R23(35), A59(35), B23(35), I23(35),
      F24(35), A24(35),R24(35), A60(35), B24(35), I24(35),
      F25(35), A25(35),R25(35), A61(35), B25(35), I25(35),
      F26(35), A26(35),R26(35), A62(35), B26(35), I26(35),
      F27(35), A27(35),R27(35), A63(35), B27(35), I27(35),
      F28(35), A28(35),R28(35), A64(35), B28(35), I28(35),
      F29(35), A29(35),R29(35), A65(35), B29(35), I29(35),
      F30(35), A30(35),R30(35), A66(35), B30(35), I30(35),
      F31(35), A31(35),R31(35), A67(35), B31(35), I31(35),
      F32(35), A32(35),R32(35), A68(35), B32(35), I32(35),
      F33(35), A33(35),R33(35), A69(35), B33(35), I33(35),
      F34(35), A34(35),R34(35), A70(35), B34(35), I34(35),
      F35(35), A35(35),R35(35), A71(35), B35(35), I35(35),
      F36(35), A36(35),R36(35), A72(35), B36(35), I36(35),
      F37(35), A37(35),R37(35), A73(35), B37(35), I37(35),
      F38(35), A38(35),R38(35), A74(35), B38(35), I38(35),
      F39(35), A39(35),R39(35), A75(35), B39(35), I39(35),
END OF T234X35.


FIELD-SYMBOLS <PTR>.

CONSTANTS: EOF VALUE '@',                   "End of file
           DAT(3) VALUE 'DAT'.              "Data format

DATA:
  BEGIN OF I_LOGIC1 OCCURS 100,
    NAME  LIKE BDCDATA-FNAM,
    VALUE LIKE BDCDATA-FVAL,
  END OF I_LOGIC1.
DATA: I_LOGIC2   LIKE I_LOGIC1 OCCURS 100 WITH HEADER LINE,
      I_LOGIC3   LIKE I_LOGIC1 OCCURS 100 WITH HEADER LINE,
      I_BDCTABLE LIKE BDCDATA  OCCURS 100 WITH HEADER LINE,
      TEXT(40),                        "Text 40 chars
      CACAH TYPE I.                    "Jumlah transaksi
*DATA IT_MESS LIKE BDCMSGCOLL OCCURS 0.
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF ITAB_RESULT OCCURS 0,
        MESSTYP,"消息类型
        INDEX TYPE SY-TABIX,
        MESSAGE TYPE STRING,"消息
        BOX,
        LIGHT LIKE ICON-ID,
      END OF ITAB_RESULT.
TYPE-POOLS : SLIS.
DATA : FCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
DATA : IS_LAYOUT TYPE SLIS_LAYOUT_ALV.


SELECTION-SCREEN BEGIN OF BLOCK B100 WITH FRAME TITLE TEXT-100.
PARAMETERS: TEMPLATE LIKE RLGRAP-FILENAME.    "template file from SHDB
PARAMETERS: TEXTFILE LIKE RLGRAP-FILENAME.
SELECTION-SCREEN PUSHBUTTON /33(15) BUTOK USER-COMMAND ZGET.
SELECTION-SCREEN END   OF BLOCK B100.

SELECTION-SCREEN BEGIN OF BLOCK B101 WITH FRAME TITLE TEXT-101.
PARAMETERS:
  TCODE LIKE TSTC-TCODE,    "Transaction code
  FILE2 LIKE RLGRAP-FILENAME OBLIGATORY DEFAULT 'C:\',      "Logic-1
  FILE3 LIKE RLGRAP-FILENAME,                               "Logic-2
  FILE_EXT LIKE RLGRAP-FILENAME DEFAULT SPACE, "Extended logic
  FILE1 LIKE RLGRAP-FILENAME OBLIGATORY DEFAULT 'C:\',           "Data
  MASK DEFAULT '&'.                     "Skip processing

SELECT-OPTIONS:
  START FOR SY-TABIX DEFAULT 1 TO 999999. "Rentang data

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: MODE RADIOBUTTON GROUP CLBI.
SELECTION-SCREEN COMMENT  3(48) TEXT-001 .
PARAMETERS: TEMP RADIOBUTTON GROUP CLBI. "DEFAULT 'X'.
SELECTION-SCREEN COMMENT  54(20) TEXT-004 .
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT  3(29) TEXT-010 .
PARAMETERS:  SESSION LIKE D0100-MAPN DEFAULT SY-UNAME.
SELECTION-SCREEN POSITION 52.
SELECTION-SCREEN COMMENT  54(13) TEXT-011 .
PARAMETERS: DISMOD LIKE IBIPPARMS-CALLMODE DEFAULT 'A'.

SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END   OF BLOCK B101.

SELECTION-SCREEN BEGIN OF BLOCK BTCX WITH FRAME TITLE TEXT-008.




SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: S64X128 RADIOBUTTON GROUP BTCX.
SELECTION-SCREEN COMMENT  3(79) TEXT-005.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: S30X40 RADIOBUTTON GROUP BTCX.
SELECTION-SCREEN COMMENT  3(79) TEXT-007.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: S128X64 RADIOBUTTON GROUP BTCX.
SELECTION-SCREEN COMMENT  3(79) TEXT-006.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: S234X35 RADIOBUTTON GROUP BTCX.
SELECTION-SCREEN COMMENT  3(79) TEXT-009.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK BTCX.

SELECTION-SCREEN BEGIN OF BLOCK BTCI WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) TEXT-002.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK BTCI.

SELECTION-SCREEN BEGIN OF BLOCK BTCX1 WITH FRAME TITLE TEXT-003.

PARAMETERS: SPLIT LIKE SYST-TABIX.

SELECTION-SCREEN END   OF BLOCK BTCX1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE1.
  PERFORM CALL_FILE USING FILE1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE2.
  PERFORM CALL_FILE USING FILE2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE_EXT.
  PERFORM CALL_FILE USING FILE_EXT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILE3.
  PERFORM CALL_FILE USING FILE3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR TEMPLATE.
  PERFORM CALL_FILE USING TEMPLATE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR TEXTFILE.
  PERFORM CALL_FILE USING TEXTFILE.

AT SELECTION-SCREEN.
  IF SSCRFIELDS-UCOMM = 'ZGET'.
    PERFORM GENERATE_FORMAT_FILE USING TEMPLATE.
  ENDIF.

INITIALIZATION.
  MOVE '生成BDC格式文件' TO BUTOK.
  %_TCODE_%_APP_%-TEXT = '事务代码'.
  %_FILE1_%_APP_%-TEXT = '数据文件(Tab delimited)'.
  %_FILE2_%_APP_%-TEXT = 'BDC格式文件'.
  %_FILE3_%_APP_%-TEXT = 'Logic Level 2 (Tab delimited)'.
  %_FILE_EXT_%_APP_%-TEXT = 'Extended logic (Tab delimited)'.
  %_MASK_%_APP_%-TEXT = '数据标示符'.
  %_MODE_%_APP_%-TEXT = '模式'.
  %_START_%_APP_%-TEXT = '记录开始'.
  %_TEMPLATE_%_APP_%-TEXT = 'SHDB模板'.
  %_TEXTFILE_%_APP_%-TEXT = 'BDC格式文件'.
  %_DISMOD_%_APP_%-TEXT = '* 调用模式'.
  %B100000_BLOCK_1000 = '生成BDC格式文件'.
  %B101005_BLOCK_1000 = '数据导入'.
  %C001015_1000 = '生成会话'.
  %C004017_1000 = '调用事务'.
  %C010020_1000 = '会话名称'.
  %C011023_1000 = '调用模式'.
  %B008027_BLOCK_1000 = 'Data Structure(数据结构)'.
  %C005030_1000 = 'Max. 64 columns & 128 characters per cell(最大64列，每列最多128个字符)'.
  %C007034_1000 = 'Max. 30 columns & 40 characters per cell(最大30列，每列最多40个字符)'.
  %C006038_1000 = 'Max. 128 columns & 64 characters per cell(最大128列，每列最多64个字符)'.
  %C009042_1000 = 'Max. 234 columns & 35 characters per cell(最大234列，每列最多35个字符)'.
  %B003045_BLOCK_1000 = '使用说明'.
  %C002047_1000 = '①SHDB录制②修改字段值,常量加"=",变量设成&1,&2…③下载模板④生成BDC格式文件⑤数据导入'.
  %_SPLIT_%_APP_%-TEXT = '分割行数量'.

*$*$*******************************************************************
*$*$*            Call main routine                                    *
*$*$*******************************************************************

START-OF-SELECTION.

  PERFORM MAIN_PROGRAM.
  PERFORM CALL_REPORT.

AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'ZGET'.
      PERFORM GENERATE_FORMAT_FILE USING TEMPLATE.
  ENDCASE.
*---------------------------------------------------------------------*
*       FORM MAIN_PROGRAM                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM MAIN_PROGRAM.
*  20090521 added by wellszhang
  IF TCODE = '' OR TCODE IS INITIAL.
    MESSAGE S292(EU) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
  IF TEMP = 'X'.
    REFRESH: MESSTAB[],ITAB_RESULT[].
    CLEAR: MESSTAB,ITAB_RESULT.
  ENDIF.
*  end of modification

* Logic level-1, level-2 and extended logic are being used
  IF FILE3 NE SPACE AND FILE2 NE SPACE AND FILE_EXT NE SPACE.
    PERFORM LEVEL_PLDI USING 3.

* Only logic level-1 is being used
  ELSEIF FILE3 = SPACE AND FILE2 NE SPACE.
    PERFORM LEVEL_PLDI USING 1.

** Logic level-1 and level-2 are being used
  ELSEIF FILE3 NE SPACE AND FILE2 NE SPACE.
    PERFORM LEVEL_PLDI USING 2.

  ELSE.
    TCODE = '<?>'.

  ENDIF.

ENDFORM.                    "MAIN_PROGRAM

*---------------------------------------------------------------------*
*       FORM CALL_REPORT                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM CALL_REPORT.
*  20090521 added by wellszhang
  IF TEMP = 'X'.
    PERFORM SHOW_ALV.
  ENDIF.
*  end of modification
*> Report
  WRITE:/ 'Transaction code:', TCODE.
  IF FILE1 EQ SPACE.
    WRITE:/ 'Logic without data has been executed by', SY-UNAME.
  ELSEIF MODE NE SPACE.
    WRITE:/ 'There are ', CACAH LEFT-JUSTIFIED.
    WRITE:  'records have been executed by', SY-UNAME.
    WRITE:/ 'which used batch input transaction.'.
    WRITE:/ 'Batch input name:', SESSION, '.'.
    WRITE:/ 'Use SM35 for further execution.'.
  ELSE.
    WRITE:/ 'There are ', CACAH LEFT-JUSTIFIED.
    WRITE:  'records have been executed by', SY-UNAME.
    WRITE:/ 'which used online transaction type', DISMOD, '.'.
  ENDIF.

ENDFORM.                    "CALL_REPORT

*---------------------------------------------------------------------*
*       FORM LEVEL_PLDI                                               *
*---------------------------------------------------------------------*
*       处理导入文件的数据结构（确定列数和列宽度）                                                     *
*---------------------------------------------------------------------*
*  -->  LOGIC_LEVEL                                                   *
*---------------------------------------------------------------------*
FORM LEVEL_PLDI USING LOGIC_LEVEL.

  IF S64X128 = 'X'.
    PERFORM CALL_PLDI TABLES T64X128 USING LOGIC_LEVEL.
  ELSEIF S128X64 = 'X'.
    PERFORM CALL_PLDI TABLES T128X64 USING LOGIC_LEVEL.
  ELSEIF S234X35 = 'X'.
    PERFORM CALL_PLDI TABLES T234X35 USING LOGIC_LEVEL.
  ELSE.
    PERFORM CALL_PLDI TABLES T30X040 USING LOGIC_LEVEL.
  ENDIF.

ENDFORM.                    "LEVEL_PLDI

*---------------------------------------------------------------------*
*       FORM CALL_PLDI                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DATA_STRUCT                                                   *
*  -->  LOGIC_LEVEL                                                   *
*---------------------------------------------------------------------*
FORM CALL_PLDI TABLES DATA_STRUCT USING LOGIC_LEVEL.

  PERFORM ZGENERATE_DATA_CONVERTION
    TABLES
            DATA_STRUCT
            START
    USING
            TCODE
            FILE1
            FILE2
            FILE3
            SESSION
            MODE
            DISMOD
            LOGIC_LEVEL
            START-LOW
            FILE_EXT
            CACAH.

ENDFORM.                    "CALL_PLDI

*---------------------------------------------------------------------*
*       FORM ZGENERATE_DATA_CONVERTION                                *
*---------------------------------------------------------------------*
*       上传数据到内表                                                 *
*---------------------------------------------------------------------*
*  -->  ITAB                                                          *
*  -->  START                                                         *
*  -->  TCODE                                                         *
*  -->  FILEDATA                                                      *
*  -->  FILEBDC                                                       *
*  -->  FILECHILD                                                     *
*  -->  SESSION                                                       *
*  -->  MODE                                                          *
*  -->  DISMOD                                                        *
*  -->  LEVEL                                                         *
*  -->  AWAL                                                          *
*  -->  FILEEXTENDED                                                  *
*  -->  CACAH                                                         *
*---------------------------------------------------------------------*
FORM ZGENERATE_DATA_CONVERTION
   TABLES
            ITAB            "数据内表
            START           "数据转换范围表
   USING
            TCODE           "事务码
            FILEDATA        "数据文件路径
            FILEBDC         "已转换的BDC文件路径
            FILECHILD       "BDC子文件路径？？
            SESSION         "会话名称？？用户名
            MODE            "Batch Input (BDC Session)
            DISMOD          "transaction mode
            LEVEL           "逻辑结构层数
            AWAL            "数据转换开始行数
            FILEEXTENDED    "Extended logic文件路径
            CACAH.          "转换结果数

*######################################################################
*@ Rutin ini berfungsi untuk mengubah file logic dan file data menjadi
*@ bentuk yang dimengerti oleh ABAP, yaitu BDC table.
*######################################################################

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Standar WS_Upload yang mesti ada
*-----------------------------------*

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME            = FILEBDC
      FILETYPE            = DAT
    TABLES
      DATA_TAB            = I_LOGIC1
    EXCEPTIONS
      CONVERSION_ERROR    = 1
      FILE_OPEN_ERROR     = 2
      FILE_READ_ERROR     = 3
      INVALID_TABLE_WIDTH = 4
      INVALID_TYPE        = 5
      NO_BATCH            = 6
      UNKNOWN_ERROR       = 7
      OTHERS              = 8.

  IF FILEDATA <> SPACE.
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        FILENAME            = FILEDATA
        FILETYPE            = DAT
      TABLES
        DATA_TAB            = ITAB
      EXCEPTIONS
        CONVERSION_ERROR    = 1
        FILE_OPEN_ERROR     = 2
        FILE_READ_ERROR     = 3
        INVALID_TABLE_WIDTH = 4
        INVALID_TYPE        = 5
        NO_BATCH            = 6
        UNKNOWN_ERROR       = 7
        OTHERS              = 8.
  ELSE.
    ASSIGN COMPONENT 1 OF STRUCTURE ITAB TO <PTR>.
    <PTR> = '/'. APPEND ITAB.
*    START = 1 .
  ENDIF.
*-------------------------------------------------*

*if sy-subrc = 0.
* Tahap inisialisasi *
  PERFORM OPEN_PROG USING MODE SESSION.

* Pengecekan level logic *
  IF LEVEL = 1.
    IF SPLIT > 0.
      PERFORM LEVEL_1_WF TABLES ITAB START USING CACAH DISMOD TCODE MODE.
    ELSE.
      PERFORM LEVEL_1 TABLES ITAB START USING CACAH DISMOD TCODE MODE.
    ENDIF.


  ELSEIF LEVEL = 2.
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        FILENAME            = FILECHILD
        FILETYPE            = DAT
      TABLES
        DATA_TAB            = I_LOGIC2
      EXCEPTIONS
        CONVERSION_ERROR    = 1
        FILE_OPEN_ERROR     = 2
        FILE_READ_ERROR     = 3
        INVALID_TABLE_WIDTH = 4
        INVALID_TYPE        = 5
        NO_BATCH            = 6
        UNKNOWN_ERROR       = 7
        OTHERS              = 8.

    PERFORM LEVEL_2 TABLES ITAB START USING CACAH DISMOD TCODE MODE AWAL
 .

  ELSE.
    CALL FUNCTION 'WS_UPLOAD'
      EXPORTING
        FILENAME            = FILECHILD
        FILETYPE            = DAT
      TABLES
        DATA_TAB            = I_LOGIC2
      EXCEPTIONS
        CONVERSION_ERROR    = 1
        FILE_OPEN_ERROR     = 2
        FILE_READ_ERROR     = 3
        INVALID_TABLE_WIDTH = 4
        INVALID_TYPE        = 5
        NO_BATCH            = 6
        UNKNOWN_ERROR       = 7
        OTHERS              = 8.

    IF FILEEXTENDED EQ SPACE.
      CALL FUNCTION 'UPLOAD'
        EXPORTING
          FILETYPE            = DAT
        TABLES
          DATA_TAB            = I_LOGIC3
        EXCEPTIONS
          CONVERSION_ERROR    = 1
          FILE_OPEN_ERROR     = 2
          FILE_READ_ERROR     = 3
          INVALID_TABLE_WIDTH = 4
          INVALID_TYPE        = 5
          NO_BATCH            = 6
          UNKNOWN_ERROR       = 7
          OTHERS              = 8.
    ELSE.
      CALL FUNCTION 'WS_UPLOAD'
        EXPORTING
          FILENAME            = FILEEXTENDED
          FILETYPE            = DAT
        TABLES
          DATA_TAB            = I_LOGIC3
        EXCEPTIONS
          CONVERSION_ERROR    = 1
          FILE_OPEN_ERROR     = 2
          FILE_READ_ERROR     = 3
          INVALID_TABLE_WIDTH = 4
          INVALID_TYPE        = 5
          NO_BATCH            = 6
          UNKNOWN_ERROR       = 7
          OTHERS              = 8.
    ENDIF.

    PERFORM LEVEL_3 TABLES ITAB START USING CACAH DISMOD TCODE MODE AWAL
 .
  ENDIF.

  PERFORM CLOSE_PROG USING MODE.

ENDFORM.                    "ZGENERATE_DATA_CONVERTION

*---------------------------------------------------------------------*
*       FORM CALL_FILE                                                *
*---------------------------------------------------------------------*
*      调用文件选择窗口，读取文件路径                                                     *
*---------------------------------------------------------------------*
*  -->  FILENAME                                                      *
*---------------------------------------------------------------------*
FORM CALL_FILE USING FILENAME.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
*     PROGRAM_NAME  = SYST-REPID
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = 'PATH'
    IMPORTING
      FILE_NAME     = FILENAME.

ENDFORM.                    "CALL_FILE


**^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^****
*** PLDI (Programmable Logic Data Interface)
*** Fungsi-fungsi standar untuk PLDI
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Konversi tabel 2 kolom menjadi tabel BDC *
*------------------------------------------*

FORM DYNPRO USING DYNBEGIN NAME VALUE.

  IF DYNBEGIN =  'X'.
    CLEAR  I_BDCTABLE.
    MOVE: NAME  TO I_BDCTABLE-PROGRAM,
          VALUE TO I_BDCTABLE-DYNPRO ,
          'X'   TO I_BDCTABLE-DYNBEGIN.
    APPEND I_BDCTABLE.

  ELSE.

    CLEAR  I_BDCTABLE.
    MOVE: NAME    TO I_BDCTABLE-FNAM,
          VALUE   TO I_BDCTABLE-FVAL.
    APPEND I_BDCTABLE.

  ENDIF.

ENDFORM.                    "DYNPRO

*&---------------------------------------------------------------------
*
*&      Form  GENERATE_BDC
*&---------------------------------------------------------------------
*
*  Pemetaan dari logic file dan data file ke BDC map
*
*----------------------------------------------------------------------
*
*  -->  logic file dan data file
*  <--  BDC table
*----------------------------------------------------------------------
*
FORM GENERATE_BDC TABLES LOGIC_T STRUCTURE I_LOGIC1 USING DATA_T.

  FIELD-SYMBOLS <PTR>.
  DATA NX TYPE I.

  LOOP AT LOGIC_T FROM '2'.

    IF LOGIC_T-NAME = EOF OR
       ( LOGIC_T-NAME = SPACE AND LOGIC_T-VALUE = SPACE ).
      EXIT.

    ELSEIF LOGIC_T-NAME(1) = '<' OR LOGIC_T-VALUE = SPACE.
      CONTINUE.

    ELSEIF LOGIC_T-VALUE(1) = '&'.
      NX = LOGIC_T-VALUE+1(4).
      ASSIGN COMPONENT NX OF STRUCTURE DATA_T TO <PTR>.
      IF <PTR> NE MASK.
        PERFORM DYNPRO USING
          ' ' LOGIC_T-NAME <PTR>.
      ELSE.
        CONTINUE.
      ENDIF.

    ELSEIF LOGIC_T-VALUE(1) = '='.
      PERFORM DYNPRO USING
        ' ' LOGIC_T-NAME LOGIC_T-VALUE+1(131).

    ELSEIF LOGIC_T-NAME = 'BDC_OKCODE' OR LOGIC_T-NAME = SPACE.
      PERFORM DYNPRO USING
        ' ' 'BDC_OKCODE' LOGIC_T-VALUE.

    ELSEIF LOGIC_T-NAME = 'BDC_CURSOR' OR LOGIC_T-NAME = 'CURSOR'.
      PERFORM DYNPRO USING
        ' ' 'BDC_CURSOR' LOGIC_T-VALUE.

    ELSE.
      PERFORM DYNPRO USING
        'X' LOGIC_T-NAME LOGIC_T-VALUE.
    ENDIF.

  ENDLOOP.

ENDFORM.                               " GENERATE_BDC


*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Diproses secara On-Line (Call Transaction)
* atau menggunakan Batch Session
*--------------------------------------------*

FORM EXECUTE USING MODE TCODE DISMOD CACAH.
  DATA: EXE(100).
  IF MODE = 'X'.
    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        TCODE     = TCODE
      TABLES
        DYNPROTAB = I_BDCTABLE.
  ELSE.
    REFRESH MESSTAB[].
    CLEAR MESSTAB.
    CALL TRANSACTION TCODE
      USING I_BDCTABLE
      MODE DISMOD
      UPDATE 'S'
      MESSAGES INTO MESSTAB.
    IF SY-SUBRC <> 0 .
      CACAH = CACAH - 1 .
    ENDIF.

    DATA: L_MSTRING(480).
    ITAB_RESULT-INDEX = SY-TABIX.
    LOOP AT MESSTAB .
      SELECT SINGLE * FROM T100
               WHERE
                 SPRSL = MESSTAB-MSGSPRA
                 AND ARBGB = MESSTAB-MSGID
                 AND MSGNR = MESSTAB-MSGNR.

      L_MSTRING = T100-TEXT.
      IF L_MSTRING CS '&1'.
        REPLACE '&1'  WITH MESSTAB-MSGV1(37) INTO L_MSTRING.
        REPLACE '&2'  WITH MESSTAB-MSGV2(12) INTO L_MSTRING.
        REPLACE '&3'  WITH MESSTAB-MSGV3(12) INTO L_MSTRING.
        REPLACE '&4'  WITH MESSTAB-MSGV4(12) INTO L_MSTRING.
      ELSE.
        REPLACE '&'  WITH MESSTAB-MSGV1(12) INTO L_MSTRING.
        REPLACE '&'  WITH MESSTAB-MSGV2(12) INTO L_MSTRING.
        REPLACE '&'  WITH MESSTAB-MSGV3(12) INTO L_MSTRING.
        REPLACE '&'  WITH MESSTAB-MSGV4(12) INTO L_MSTRING.
      ENDIF.
      CONDENSE L_MSTRING.
      ITAB_RESULT-MESSTYP = MESSTAB-MSGTYP.
      ITAB_RESULT-MESSAGE = L_MSTRING.
      IF ITAB_RESULT-MESSTYP = 'E' OR ITAB_RESULT-MESSTYP = 'A'.
        ITAB_RESULT-LIGHT = '@5C@'.
*        EXIT.
      ELSEIF ITAB_RESULT-MESSTYP = 'W'.
        ITAB_RESULT-LIGHT = '@5D@'.
      ELSE.
        ITAB_RESULT-LIGHT = '@5B@'.
      ENDIF.
      APPEND ITAB_RESULT.
    ENDLOOP.


  ENDIF.
*  IF sy-subrc = 0 AND MODE <> 'X' .
*     EXE = 'Tcode: '.
*    WRITE TCODE TO EXE+7 LEFT-JUSTIFIED.
*    WRITE 'is being executed for record: ' TO EXE+12 LEFT-JUSTIFIED.
*    WRITE CACAH TO EXE+42 LEFT-JUSTIFIED.
*    PERFORM PROSES USING EXE.
*  ENDIF.
ENDFORM.                               " SAVE_IT


*~~~~~~~~~~~~~~*
* Inisialisasi *
*--------------*

FORM OPEN_PROG USING MODE SESSION.
  DATA: TEXT1(80).

  IF MODE = 'X'.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT = SY-MANDT
        GROUP  = SESSION
        USER   = SY-UNAME
        KEEP   = 'X'.
  ENDIF.

ENDFORM.                    "OPEN_PROG

*~~~~~~~~~*
* Closing *
*---------*

FORM CLOSE_PROG USING MODE.

  IF MODE = 'X'.
    CALL FUNCTION 'BDC_CLOSE_GROUP' .
  ENDIF.

ENDFORM.                               " CLOSE_PROG

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
* Indikator sedang melakukan proses *
*-----------------------------------*

FORM PROSES USING TEXT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT   = TEXT
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                    "PROSES


*~~~~~~~~~~~~~~~*
* Logic level 1 *
*---------------*
FORM LEVEL_1 TABLES ITAB START USING CACAH DISMOD TCODE MODE.

  LOOP AT ITAB.
    IF SY-TABIX IN START.
      ASSIGN COMPONENT 1 OF STRUCTURE ITAB TO <PTR>.
      IF <PTR> = EOF. EXIT. ENDIF.
      REFRESH I_BDCTABLE.
      PERFORM GENERATE_BDC TABLES I_LOGIC1 USING ITAB.
      ADD 1 TO CACAH.
      PERFORM EXECUTE USING MODE TCODE DISMOD CACAH.
    ENDIF.
  ENDLOOP.

ENDFORM.                                                    "LEVEL_1

*&---------------------------------------------------------------------*
*&      Form  LEVEL_1_WF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITAB       text
*      -->START      text
*      -->CACAH      text
*      -->DISMOD     text
*      -->TCODE      text
*      -->MODE       text
*----------------------------------------------------------------------*
FORM LEVEL_1_WF TABLES ITAB START USING CACAH DISMOD TCODE MODE.
  DATA COUNT TYPE SY-TABIX.
  CLEAR COUNT.
  LOOP AT ITAB.

    IF SY-TABIX IN START.
      COUNT = COUNT + 1.
      ASSIGN COMPONENT 1 OF STRUCTURE ITAB TO <PTR>.
      IF <PTR> = EOF. EXIT. ENDIF.
      REFRESH I_BDCTABLE.
      PERFORM GENERATE_BDC TABLES I_LOGIC1 USING ITAB.
      ADD 1 TO CACAH.
      PERFORM EXECUTE USING MODE TCODE DISMOD CACAH.

      IF COUNT >= SPLIT.
        PERFORM CLOSE_PROG USING MODE.
        PERFORM OPEN_PROG USING MODE SESSION.
        CLEAR COUNT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                                                    "LEVEL_1

*~~~~~~~~~~~~~~~*
* Logic level 2 *
*---------------*
FORM LEVEL_2 TABLES ITAB START USING CACAH DISMOD TCODE MODE AWAL.

  LOOP AT ITAB.                        "ere sy-tabix in start.
    IF SY-TABIX IN START.
      ASSIGN COMPONENT 1 OF STRUCTURE ITAB TO <PTR>.
      IF <PTR> = EOF. EXIT. ENDIF.
      TEXT = 'Processing at row '.
      WRITE SY-TABIX TO TEXT+18 LEFT-JUSTIFIED.
      PERFORM PROSES USING TEXT.

      IF <PTR> NE SPACE.
        IF SY-TABIX NE AWAL.
          PERFORM DYNPRO USING ' ' 'BDC_OKCODE' '/11'.
          PERFORM EXECUTE USING MODE TCODE DISMOD CACAH.
        ENDIF.
        REFRESH I_BDCTABLE.
        PERFORM GENERATE_BDC TABLES I_LOGIC1 USING ITAB.
        ADD 1 TO CACAH.
      ELSE.
        PERFORM GENERATE_BDC TABLES I_LOGIC2 USING ITAB.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM DYNPRO USING ' ' 'BDC_OKCODE' '/11'.
  PERFORM EXECUTE USING MODE TCODE DISMOD CACAH.
ENDFORM.                                                    "LEVEL_2

*~~~~~~~~~~~~~~~~*
* Extended logic *
*----------------*
FORM LEVEL_3 TABLES ITAB START USING CACAH DISMOD TCODE MODE AWAL.

  DATA: DATA_EXT(8192), TEMP(8192),
        SUB_ITEM VALUE SPACE.

  LOOP AT ITAB.

    IF SY-TABIX IN START.

      ASSIGN COMPONENT 1 OF STRUCTURE ITAB TO <PTR>.
** Check End-Of-File
      IF <PTR> = EOF.
        EXIT.
      ENDIF.

      TEXT = 'Processing at row '.
      WRITE SY-TABIX TO TEXT+18 LEFT-JUSTIFIED.
      PERFORM PROSES USING TEXT.

** Check if any sub-item data or extended data uses '#' symbol
      IF <PTR> = '#'.                  "->Sub-item or extended-data
        PERFORM GENERATE_BDC TABLES I_LOGIC3 USING ITAB.
        SUB_ITEM = 'X'.     "->Sub-item data flag is activated
        CONTINUE.
      ENDIF.

** Process BDC for header data
      IF <PTR> NE SPACE.
        IF SY-TABIX NE AWAL.
          READ TABLE I_LOGIC3 INDEX 2.
** There is no sub_item data or no extended data with '#' symbol
** or no extended logic
          IF I_LOGIC3 NE SPACE AND SUB_ITEM EQ SPACE.
** Read extended data in the last item
            MOVE: ITAB TO TEMP,
                  DATA_EXT TO ITAB.
            PERFORM GENERATE_BDC TABLES I_LOGIC3 USING ITAB.
            MOVE TEMP TO ITAB.
          ELSE.
** There is sub_item data or extended data with '#' symbol
** or no extended logic
** If sub-items are processed, it is saved automatically.
            PERFORM DYNPRO USING ' ' 'BDC_OKCODE' '/11'.
          ENDIF.
          PERFORM EXECUTE USING MODE TCODE DISMOD CACAH.
          CLEAR SUB_ITEM.              "->Reset flag sub-item
        ENDIF.
        REFRESH I_BDCTABLE.
        PERFORM GENERATE_BDC TABLES I_LOGIC1 USING ITAB.
        ADD 1 TO CACAH.
      ELSE.
** Check whether data contains sub_item data
        IF SUB_ITEM = 'X'.
** After all sub-items are processed, back to item.
          PERFORM DYNPRO USING ' ' 'BDC_OKCODE' '/3'.
        ENDIF.
        PERFORM GENERATE_BDC TABLES I_LOGIC2 USING ITAB.
** Store last data into temporary field
        MOVE ITAB TO DATA_EXT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  READ TABLE I_LOGIC3 INDEX 2.
  IF I_LOGIC3 NE SPACE AND SUB_ITEM EQ SPACE.
** No sub-item data or extended data with '#' symbol
** and there is extended logic
    MOVE: DATA_EXT TO ITAB.
    PERFORM GENERATE_BDC TABLES I_LOGIC3 USING ITAB.
  ELSE.
** There is sub-item or extended data, last extended data has '#'
** If sub-items are processed, it is saved automatically.
    PERFORM DYNPRO USING ' ' 'BDC_OKCODE' '/11'.
  ENDIF.
  PERFORM EXECUTE USING MODE TCODE DISMOD CACAH.
ENDFORM.                                                    "LEVEL_3

*&---------------------------------------------------------------------
*
*&      Form  GENERATE_FORMAT_FILE
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*      -->P_TEMPLATE  text
*----------------------------------------------------------------------
*
FORM GENERATE_FORMAT_FILE USING  TEMPLATE.
  DATA: BDCTAB TYPE BDCDATA OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF OUTTEXT OCCURS 0,
        FNAM LIKE BDCDATA-FNAM,
        FVAL LIKE BDCDATA-FVAL,
        END OF OUTTEXT.

  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME            = TEMPLATE
      FILETYPE            = DAT
    TABLES
      DATA_TAB            = BDCTAB
    EXCEPTIONS
      CONVERSION_ERROR    = 1
      FILE_OPEN_ERROR     = 2
      FILE_READ_ERROR     = 3
      INVALID_TABLE_WIDTH = 4
      INVALID_TYPE        = 5
      NO_BATCH            = 6
      UNKNOWN_ERROR       = 7
      OTHERS              = 8.
  IF SY-SUBRC NE 0 OR BDCTAB[] IS INITIAL.
    WRITE: 'ERROR OPEN TEMPLATE FILE, PLEASE CHECK AGAIN!'.
    EXIT.
  ENDIF.
  MOVE 'NAME' TO OUTTEXT-FNAM.
  MOVE 'VALUE' TO OUTTEXT-FVAL.
  INSERT OUTTEXT INDEX 1.
  DELETE BDCTAB INDEX 1.
  LOOP AT BDCTAB.
    IF NOT ( BDCTAB-PROGRAM IS INITIAL ).
      OUTTEXT-FNAM = BDCTAB-PROGRAM.
      OUTTEXT-FVAL = BDCTAB-DYNPRO.
      APPEND OUTTEXT.
      CONTINUE.
    ELSEIF ( BDCTAB-FNAM NE 'BDC_CURSOR' AND BDCTAB-FNAM NE 'BDC_SUBSCR' ).
      OUTTEXT-FNAM = BDCTAB-FNAM.
      OUTTEXT-FVAL = BDCTAB-FVAL.
      APPEND OUTTEXT.
    ENDIF.
  ENDLOOP.
  CLEAR OUTTEXT.
  MOVE '@' TO OUTTEXT-FNAM.
  APPEND OUTTEXT.
  CALL FUNCTION 'WS_DOWNLOAD'
   EXPORTING
*   BIN_FILESIZE                  = ' '
*   CODEPAGE                      = ' '
     FILENAME                      = TEXTFILE
     FILETYPE                      = 'DAT'
*   MODE                          = ' '
*   WK1_N_FORMAT                  = ' '
*   WK1_N_SIZE                    = ' '
*   WK1_T_FORMAT                  = ' '
*   WK1_T_SIZE                    = ' '
*   COL_SELECT                    = ' '
*   COL_SELECTMASK                = ' '
*   NO_AUTH_CHECK                 = ' '
* IMPORTING
*   FILELENGTH                    =
    TABLES
      DATA_TAB                      = OUTTEXT
*   FIELDNAMES                    =
* EXCEPTIONS
*   FILE_OPEN_ERROR               = 1
*   FILE_WRITE_ERROR              = 2
*   INVALID_FILESIZE              = 3
*   INVALID_TYPE                  = 4
*   NO_BATCH                      = 5
*   UNKNOWN_ERROR                 = 6
*   INVALID_TABLE_WIDTH           = 7
*   GUI_REFUSE_FILETRANSFER       = 8
*   CUSTOMER_ERROR                = 9
*   OTHERS                        = 10
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    MESSAGE S355(00).
  ENDIF.
  LOOP AT OUTTEXT.
    WRITE: /(20) OUTTEXT-FNAM, (20) OUTTEXT-FVAL.
  ENDLOOP.
ENDFORM.                    " GENERATE_FORMAT_FILE
*&---------------------------------------------------------------------*
*&      Form  SHOW_ALV
*&---------------------------------------------------------------------*
*20090521 added by WellsZhang
FORM SHOW_ALV .
  IS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  CLEAR FCAT.
  REFRESH FCAT.
  PERFORM FCAT_BUILT USING 'LIGHT' '结果'.
  PERFORM FCAT_BUILT USING 'MESSTYP' '类型'.
  PERFORM FCAT_BUILT USING 'INDEX' '索引'.
  PERFORM FCAT_BUILT USING 'MESSAGE' '信息'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM                = SY-REPID
*    I_CALLBACK_PF_STATUS_SET          = 'PF_STATUS_SET'
*    I_CALLBACK_HTML_TOP_OF_PAGE       = 'TOP_OF_PAGE'
*    I_CALLBACK_USER_COMMAND           = 'USER_COMMAND-ALV'
    IS_LAYOUT                         = IS_LAYOUT
    IT_FIELDCAT                       = FCAT[]
    I_SAVE                            = 'A'
*    I_HTML_HEIGHT_TOP                 = 12
   TABLES
     T_OUTTAB                          = ITAB_RESULT
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
           .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " SHOW_ALV
*&---------------------------------------------------------------------*
*&      Form  fcat_built
*&---------------------------------------------------------------------*

FORM FCAT_BUILT  USING    P_FIELDNAME P_SELTEXT .
  FCAT-FIELDNAME  = P_FIELDNAME.
  FCAT-SELTEXT_M  = P_SELTEXT.
  APPEND FCAT.
  CLEAR FCAT.
ENDFORM.                    " fcat_built
*end of modification

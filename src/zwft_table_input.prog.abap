*&---------------------------------------------------------------------*
*& Report ZWFT_TABLE_INPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZWFT_TABLE_INPUT MESSAGE-ID AT
LINE-SIZE 80
LINE-COUNT 65
NO STANDARD PAGE HEADING.


TABLES: dd02l,
  dd03l,
  dd01l,
  dd02t,
  dd17s,
  dd12t,
  dd04t.
TABLES : rlgrap.


DEFINE __progress_indicator.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
  EXPORTING
    percentage = &1
    TEXT       = &2.
END-OF-DEFINITION.

* Data field table
DATA: BEGIN OF struct OCCURS 10,
  POSITION LIKE dd03l-POSITION,
  fieldname(30),
  rollname LIKE dd03l-rollname,
  inttype TYPE C,
  leng    TYPE I,
END OF struct.

* The dynamic program source table
DATA: BEGIN OF inctabl OCCURS 0,
  LINE(72),
END OF inctabl.

DATA : BEGIN OF textpool_tab OCCURS 0.
  INCLUDE STRUCTURE TEXTPOOL.
DATA : END OF textpool_tab.

DATA: lng TYPE I,
      typesrting(6),
      winfile(128) TYPE C,
      answer(1)    TYPE C,
      w_text1(62)  TYPE C,
      w_text2(40)  TYPE C,
      q_return     LIKE syst-subrc,
      err_flag(1)  TYPE C,
      w_system(40) TYPE C,
      winsys(7)    TYPE C,
      zname(8)     TYPE C,
      w1(20)      TYPE C,
      w2(20)      TYPE C,
      w_line(80)   TYPE C.
*
SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-b00.
*
  SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
    PARAMETERS tab_name LIKE dd03l-tabname OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK b01.

  SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
*parameters: path(30)    type c default 'D:\SAPDATA\DATA'.
    PARAMETERS: path LIKE rlgrap-filename.
  SELECTION-SCREEN END OF BLOCK b02.
*
  SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
    PARAMETERS: p_exp RADIOBUTTON GROUP radi,
    p_imp RADIOBUTTON GROUP radi,
    p_clear     AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK b03.
*
SELECTION-SCREEN END OF BLOCK b00.
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR path.
PERFORM  get_window_file.

AT SELECTION-SCREEN ON tab_name.
PERFORM check_table_exists.
*
START-OF-SELECTION.

PERFORM authority_check.
PERFORM init_report_texts.
PERFORM request_confirmation.

END-OF-SELECTION.
IF answer = 'J'.
  PERFORM execute_program_function.
ENDIF.
*
TOP-OF-PAGE.
PERFORM write_header.


*&---------------------------------------------------------------------*
*& Form SELECT_FIELD_FROM_TABLE
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM select_field_from_table.
  CLEAR dd03l.
  SELECT POSITION fieldname rollname inttype leng
  INTO (dd03l-POSITION,dd03l-fieldname,dd03l-rollname,
  dd03l-inttype,dd03l-leng)
  FROM dd03l
  WHERE as4local = 'A'
  AND tabname = tab_name.
    MOVE-CORRESPONDING dd03l TO struct.
    APPEND struct.
  ENDSELECT.
*  DELETE struct WHERE position = 1.
  SORT struct BY POSITION.
ENDFORM.                               " SELECT_FIELD_FROM_TABLE
*&---------------------------------------------------------------------*
*& Form func_export.
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM func_export.
  inctabl-LINE = 'PROGRAM SUBPOOL.'.                     APPEND inctabl.
  inctabl-LINE = 'FORM DOWNLOAD.'.                       APPEND inctabl.
  CONCATENATE 'TABLES:' tab_name '.'
  INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.
  PERFORM make_itab.
  CONCATENATE 'DATA IT_TAB LIKE' tab_name 'OCCURS 0 WITH HEADER LINE.'
  INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.
  inctabl-LINE = ' CLEAR: it_TAB,IT_TAB[].'.             APPEND inctabl.
  CONCATENATE 'SELECT * FROM' tab_name
  INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.
  inctabl-LINE = ' INTO CORRESPONDING FIELDS OF TABLE it_tab.'.
  APPEND inctabl.

  inctabl-LINE = 'LOOP AT it_tab.'.                      APPEND inctabl.
  inctabl-LINE = 'MOVE-CORRESPONDING it_tab TO dyntab.'.
  APPEND inctabl.
  inctabl-LINE = 'APPEND dyntab.'.                       APPEND inctabl.
  inctabl-LINE = 'ENDLOOP.'.                             APPEND inctabl.
*  INCTABL-LINE = ' CALL FUNCTION ''WS_DOWNLOAD'''.       APPEND INCTABL.
  inctabl-LINE = ' CALL FUNCTION ''GUI_DOWNLOAD'''.       APPEND inctabl.
  inctabl-LINE = ' EXPORTING'.                           APPEND inctabl.
  CONCATENATE ' filename = ' '''' winfile '''' INTO inctabl-LINE.
  APPEND inctabl.
  inctabl-LINE = ' filetype = ''DAT'''.                  APPEND inctabl.
  inctabl-LINE = ' TABLES'.                              APPEND inctabl.
  inctabl-LINE = ' data_tab = DYNTAB.'.                  APPEND inctabl.

  inctabl-LINE = 'DESCRIBE TABLE DYNTAB LINES sy-index.'.
  APPEND inctabl.
  inctabl-LINE = 'FORMAT COLOR COL_NORMAL INTENSIFIED OFF.'.
  APPEND inctabl.
  inctabl-LINE = 'WRITE: /1 syst-vline,'.                APPEND inctabl.
  inctabl-LINE = '''EXPORT'','.                          APPEND inctabl.
  inctabl-LINE = '15 ''data line(s) have been exported'','.
  APPEND inctabl.
  inctabl-LINE = '68 syst-index,'.                       APPEND inctabl.
  inctabl-LINE = '80 syst-vline.'.                       APPEND inctabl.
  inctabl-LINE = 'ULINE.'.                               APPEND inctabl.

  inctabl-LINE = 'ENDFORM.'.                             APPEND inctabl.

  DATA mess TYPE string.
  GENERATE SUBROUTINE POOL inctabl NAME zname MESSAGE mess .
  PERFORM download IN PROGRAM (zname).

ENDFORM.                               "MAKE_PRG
*&---------------------------------------------------------------------*
*& Form make_itab
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*----------------------------------------------------------------------*
FORM make_itab.
  inctabl-LINE = 'DATA: BEGIN OF dyntab OCCURS 10,'.
  APPEND inctabl.
  LOOP AT struct.
    CHECK struct-fieldname(6) NE '.INCLU'.
    inctabl-LINE = struct-fieldname.   "APPEND inctabl.
    lng = STRLEN( struct-fieldname ).
    IF NOT struct-leng IS INITIAL .
      IF struct-inttype     = 'P'.
        typesrting = '(13)'.
*     ELSEIF struct-inttype = 'D'.
*       typesrting = '(10)'.
*     ELSEIF struct-inttype = 'T'.
*       typesrting = '(8)'.
      ELSE.
        typesrting(1) = '('.
        typesrting+1 = struct-leng.
        typesrting+5 = ')'.
        CONDENSE typesrting NO-GAPS.
      ENDIF.
      inctabl-LINE+lng = typesrting.
    ENDIF.
    inctabl-LINE+25 = ','. APPEND inctabl.

*     CLEAR: W1, W2.
*     CONCATENATE STRUCT-FIELDNAME 'LIKE' INTO W1 SEPARATED BY SPACE.
*     CONCATENATE TAB_NAME '-' STRUCT-FIELDNAME ',' INTO W2.
*     CONCATENATE W1 W2 INTO INCTABL-LINE SEPARATED BY SPACE.
*
*    APPEND INCTABL.


  ENDLOOP.
  inctabl-LINE = 'END OF dyntab. '.
  APPEND inctabl.

ENDFORM.                               " make_itab
*&---------------------------------------------------------------------*
*&      Form  BUILD_FILE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_file_name.
  MOVE path TO winfile.
*  write '\' to winfile+30.
*  write tab_name to winfile+31.
*  write '.TAB' to winfile+61(4).
  CONDENSE winfile NO-GAPS.


ENDFORM.                               " BUILD_FILE_NAME
*&---------------------------------------------------------------------*
*&      Form  INIT_REPORT_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_report_texts.

  READ TEXTPOOL syst-repid
  INTO textpool_tab LANGUAGE syst-langu.
  LOOP AT textpool_tab
  WHERE ID EQ 'R' OR ID EQ 'T'.
    REPLACE '&1............................'
    WITH tab_name INTO textpool_tab-entry.
    MODIFY textpool_tab.
  ENDLOOP.

ENDFORM.                               " INIT_REPORT_TEXTS
*&---------------------------------------------------------------------*
*&      Form  REQUEST_CONFIRMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM request_confirmation.

* import selected, confirm action
  IF p_imp = 'X'.
*   build message text for popup
    CONCATENATE 'Data for table'
    tab_name
    'will be imported' INTO w_text1 SEPARATED BY space.
*   check if delete existing selected, and change message text
    IF p_clear = ' '.
      w_text2 = 'and appended to the end of existing data'.
    ELSE.
      w_text2 = 'Existing Data will be deleted'.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'N'
      textline1      = w_text1
      textline2      = w_text2
      titel          = 'Confirm Import of Data'
      cancel_display = ' '
    IMPORTING
      answer         = answer
    EXCEPTIONS
      OTHERS         = 1.

  ELSE.
*   export selected, set answer to yes so export can continue
    answer = 'J'.
  ENDIF.

ENDFORM.                               " REQUEST_CONFIRMATION
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROGRAM_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_program_function.

  PERFORM select_field_from_table.
  PERFORM build_file_name.
  CLEAR: q_return,err_flag.

  IF p_imp = 'X'.
    PERFORM check_file_exists.
    CHECK err_flag = ' '.
    PERFORM func_import.
  ELSE.
    PERFORM func_export.
  ENDIF.


ENDFORM.                               " EXECUTE_PROGRAM_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_EXISTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_file_exists.

  CALL FUNCTION 'WS_QUERY'
  EXPORTING
    filename = winfile
    query    = 'FE'
  IMPORTING
    RETURN   = q_return
  EXCEPTIONS
    OTHERS   = 1.

  IF syst-subrc NE 0 OR q_return NE 1.
    err_flag = 'X'.
  ENDIF.

ENDFORM.                               " CHECK_FILE_EXISTS
*&---------------------------------------------------------------------*
*&      Form  FUNC_IMPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM func_import.
  inctabl-LINE = 'PROGRAM SUBPOOL.'.                  APPEND inctabl.
  inctabl-LINE = 'FORM UPLOAD.'.                      APPEND inctabl.
  CONCATENATE 'TABLES:' tab_name '.'
  INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.
  PERFORM make_itab.
  CONCATENATE 'DATA IT_TAB LIKE' tab_name 'OCCURS 0 WITH HEADER LINE.'
  INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.
  inctabl-LINE = ' CLEAR: it_TAB,IT_TAB[].'.         APPEND inctabl.

  inctabl-LINE = ' CALL FUNCTION ''WS_UPLOAD'''.     APPEND inctabl.
  inctabl-LINE = ' EXPORTING'.                       APPEND inctabl.
*  concatenate ' filename = ' ''''
*              winfile '''' into inctabl-line separated by space.
  CONCATENATE ' filename = ' ''''
  winfile '''' INTO inctabl-LINE.

  APPEND inctabl.
  inctabl-LINE = ' filetype = ''DAT'''.              APPEND inctabl.
  inctabl-LINE = ' TABLES'.                          APPEND inctabl.
  inctabl-LINE = ' data_tab = DYNTAB.'.              APPEND inctabl.

  IF p_clear = 'X'.
    CONCATENATE 'SELECT * FROM' tab_name INTO inctabl-LINE SEPARATED BY
    space.
    APPEND inctabl.
    inctabl-LINE = ' INTO CORRESPONDING FIELDS OF TABLE it_tab.'.
    APPEND inctabl.

    inctabl-LINE = 'LOOP AT it_tab.'.                  APPEND inctabl.
    CONCATENATE 'DELETE'
    tab_name
    'FROM it_tab.' INTO inctabl-LINE SEPARATED BY space.
    APPEND inctabl.
    inctabl-LINE = 'ENDLOOP.'.                         APPEND inctabl.
    inctabl-LINE = 'COMMIT WORK.'.                     APPEND inctabl.
  ENDIF.

  inctabl-LINE = 'LOOP AT dyntab.'.                   APPEND inctabl.
  CONCATENATE 'MOVE-CORRESPONDING dyntab TO '
  tab_name '.' INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.

  CONCATENATE 'MODIFY' tab_name '.'
  INTO inctabl-LINE SEPARATED BY space.
  APPEND inctabl.
*             'FROM dyntab.' INTO inctabl-line SEPARATED BY space.
* CONCATENATE 'MODIFY'
*             tab_name
*             'FROM dyntab.' INTO inctabl-line SEPARATED BY space.
* APPEND inctabl.
  inctabl-LINE = 'ENDLOOP.'.                          APPEND inctabl.

  inctabl-LINE = 'DESCRIBE TABLE DYNTAB LINES sy-index.'.
  APPEND inctabl.
  inctabl-LINE = 'FORMAT COLOR COL_NORMAL INTENSIFIED OFF.'.
  APPEND inctabl.
  inctabl-LINE = 'WRITE: /1 syst-vline,'.                APPEND inctabl.
  inctabl-LINE = '''IMPORT'','.                          APPEND inctabl.
  inctabl-LINE = '15 ''data line(s) have been imported'','.
  APPEND inctabl.
  inctabl-LINE = '68 syst-index,'.                       APPEND inctabl.
  inctabl-LINE = '80 syst-vline.'.                       APPEND inctabl.
  inctabl-LINE = 'ULINE.'.                               APPEND inctabl.

  inctabl-LINE = 'ENDFORM.'.                             APPEND inctabl.

  GENERATE SUBROUTINE POOL inctabl NAME zname.
  PERFORM upload IN PROGRAM (zname).

ENDFORM.                               " FUNC_IMPORT
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_header.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  ULINE.

  CONCATENATE syst-sysid
  syst-saprl
  syst-host INTO w_system SEPARATED BY space.

  WRITE : AT /1(syst-linsz) w_system CENTERED.
  WRITE : AT 1 syst-vline, syst-uname.
  syst-linsz = syst-linsz - 11.
  WRITE : AT syst-linsz syst-repid(010).
  syst-linsz = syst-linsz + 11.
  WRITE : AT syst-linsz syst-vline.

  LOOP AT textpool_tab WHERE ID EQ 'R'.
    WRITE : AT /1(syst-linsz) textpool_tab-entry CENTERED.
  ENDLOOP.
  WRITE : AT 1 syst-vline, syst-datum.
  syst-linsz = syst-linsz - 11.
  WRITE : AT syst-linsz syst-tcode(004).
  syst-linsz = syst-linsz + 11.
  WRITE : AT syst-linsz syst-vline.

  LOOP AT textpool_tab WHERE ID EQ 'T'.
    WRITE : AT /1(syst-linsz) textpool_tab-entry CENTERED.
  ENDLOOP.
  WRITE : AT 1 syst-vline, syst-uzeit.

  syst-linsz = syst-linsz - 11.
  WRITE : AT syst-linsz 'Page', syst-pagno.
  syst-linsz = syst-linsz + 11.
  WRITE : AT syst-linsz syst-vline.
  ULINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  LOOP AT textpool_tab WHERE ID EQ 'H'.
    WRITE : AT /1(syst-linsz) textpool_tab-entry.
  ENDLOOP.

  ULINE.

ENDFORM.                               " WRITE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CHECK_TABLE_EXISTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_table_exists.

  SELECT SINGLE * FROM dd02l
  INTO CORRESPONDING FIELDS OF dd02l
  WHERE tabname = tab_name.
  CHECK syst-subrc NE 0.
  MESSAGE e402(mo) WITH tab_name.

ENDFORM.                               " CHECK_TABLE_EXISTS
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check.
*  DATA : L_CHECK(1) TYPE C.
*  CLEAR: L_CHECK.
*  CASE SY-UNAME.
*    WHEN '19938'    OR 'JW_SEO'.                 "辑沥快
*    WHEN '16531'    OR 'PHILIP.LEE'.
*    WHEN '33001772' OR 'HSJAVA'.
*    WHEN '22605'    OR 'HN.CHO'.
*    WHEN 'Z2216'    OR 'JAEWK.OH'.
*    WHEN 'HKPRINCE' OR 'YUMIIS.KIM'.
*    WHEN OTHERS.
*         L_CHECK = 'X'.
*  ENDCASE.
*  IF L_CHECK = 'X'.
*    MESSAGE E000 WITH 'You do not have Authority!!'.
*  ENDIF.



ENDFORM.                    " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  GET_WINDOW_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_window_file.

  DATA: global_filemask_all(80).
  DATA: global_filemask_mask(20), global_filemask_text(20).
  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask LIKE global_filemask_all.
  DATA: fieldln TYPE I.
  FIELD-SYMBOLS: <tmp_sym>.

  IF  global_filemask_mask IS INITIAL.
    tmp_mask = ',*.*,*.*.'.
  ELSE.
    tmp_mask = ','.
    WRITE global_filemask_text TO tmp_mask+1.
    WRITE ',' TO tmp_mask+21.
    WRITE global_filemask_mask TO tmp_mask+22.
    WRITE '.' TO tmp_mask+42.
    CONDENSE tmp_mask NO-GAPS.
  ENDIF.
  IF NOT global_filemask_all IS INITIAL.
    tmp_mask = global_filemask_all.
  ENDIF.
  IF path NE ' '.
    fieldln = STRLEN( path ) - 1.
    ASSIGN path+fieldln(1) TO <tmp_sym>.
    IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
      CLEAR <tmp_sym>.
    ENDIF.
  ENDIF.


  CALL FUNCTION 'WS_FILENAME_GET'
  EXPORTING
    def_filename     = rlgrap-filename
    def_path         = path
*     MASK             = ',*.*,*.*.'
    MASK             = tmp_mask
    MODE             = 'O'
*/          MODE             = 'S'
*     TITLE            = ' '
  IMPORTING
    filename         = tmp_filename
*     RC               =
  EXCEPTIONS
    inv_winsys       = 01
    no_batch         = 02
    selection_cancel = 03
    selection_error  = 04.


  IF sy-subrc = 0.
    path = tmp_filename.
  ELSE.
  ENDIF.

ENDFORM.                    " GET_WINDOW_FILE

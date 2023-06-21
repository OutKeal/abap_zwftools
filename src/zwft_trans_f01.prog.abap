

*&---------------------------------------------------------------------*
*&  Include           ZLJW_TRANS_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  FORM  SELECT_PROGLIST
*----------------------------------------------------------------------*
*  TEXT : 선택된 프로그램명을 확인하고 목록에 담는다.
*----------------------------------------------------------------------*
FORM select_objlist.

* 프로그램
  IF so_prog[] IS NOT INITIAL OR g_only_devcls = 'X'.
    PERFORM sel_prog.
  ENDIF.

* FUNCTION-GROUP
  IF so_area[] IS NOT INITIAL OR g_only_devcls = 'X'.
    PERFORM sel_area.
  ENDIF.

* DATA-ELEMENT
  IF so_dtel[] IS NOT INITIAL OR g_only_devcls = 'X'.

    SELECT dd04l~rollname AS object
    APPENDING CORRESPONDING FIELDS OF TABLE gt_lxe
    FROM dd04l JOIN tadir ON tadir~pgmid    = 'R3TR'   AND
    tadir~object   = 'DTEL'   AND
    tadir~obj_name = dd04l~rollname
    WHERE dd04l~rollname IN so_dtel
    AND tadir~devclass IN so_class.

    LOOP AT gt_lxe WHERE objtype = ''.
      gt_lxe-objtype = 'DTEL'.
      MODIFY gt_lxe.
    ENDLOOP.

  ENDIF.

* DOMAIN
  IF so_doma[] IS NOT INITIAL OR g_only_devcls = 'X'.

    SELECT dd01l~domname AS object
    APPENDING CORRESPONDING FIELDS OF TABLE gt_lxe
    FROM dd01l JOIN tadir ON tadir~pgmid    = 'R3TR'   AND
    tadir~object   = 'DOMA'   AND
    tadir~obj_name = dd01l~domname
    WHERE dd01l~domname  IN so_doma
    AND dd01l~valexi    = 'X'         "고정값 존재
    AND tadir~devclass IN so_class.

    LOOP AT gt_lxe WHERE objtype = ''.
      gt_lxe-objtype = 'VALU'.
      MODIFY gt_lxe.
    ENDLOOP.

  ENDIF.

* MESSAGE-CLASS
  IF so_msag[] IS NOT INITIAL OR g_only_devcls = 'X'.

    SELECT t100~arbgb  t100~msgnr
    APPENDING CORRESPONDING FIELDS OF TABLE gt_mess
    FROM t100 JOIN tadir ON tadir~pgmid    = 'R3TR' AND
    tadir~object   = 'MSAG' AND
    tadir~obj_name = t100~arbgb
    WHERE t100~sprsl      = pm_slang
    AND t100~arbgb     IN so_msag
    AND tadir~devclass IN so_class.

    LOOP AT gt_mess.
      CLEAR gt_lxe.
      gt_lxe-objtype = 'MESS'.
      gt_lxe-object+0(20)  = gt_mess-arbgb.
      gt_lxe-object+20(20) = gt_mess-msgnr.
      APPEND gt_lxe.
    ENDLOOP.

  ENDIF.

ENDFORM. "SELECT_PROGLIST

*----------------------------------------------------------------------*
*  FORM  READ_TEXT
*----------------------------------------------------------------------*
*  TEXT : ABAP Text 읽기
*----------------------------------------------------------------------*
FORM read_text .

  CLEAR: gt_m, gt_m[].

  DATA: BEGIN OF lt_type OCCURS 0,
    obj_type LIKE lxe_attob-obj_type,
  END OF lt_type.

* 프로그램
  SELECT obj_type                           "CAD4(GUI 제목)
  INTO TABLE lt_type                                      "SCT4(뭔지모름)
  FROM lxe_attob                                          "SRH4(화면설명)
  WHERE obj_type IN ('CA4',  'RPT4' ,'CA1' ).                      "SRT4(화면필드)

  LOOP AT gt_lxe WHERE objtype = 'PROG'.
    LOOP AT lt_type.
      PERFORM read_process USING lt_type-obj_type
            gt_lxe-object.
    ENDLOOP.
  ENDLOOP.

* T-CODE
  LOOP AT gt_lxe WHERE objtype = 'TRAN'.
    PERFORM read_process USING gt_lxe-objtype
          gt_lxe-object.
  ENDLOOP.


* FUNCTION-GROUP
  LOOP AT gt_lxe WHERE objtype = 'RPT1'.
    PERFORM read_process USING gt_lxe-objtype
          gt_lxe-object.
  ENDLOOP.
* FUNCTION-GROUP INTERFACE
  LOOP AT gt_lxe WHERE objtype = 'CA1'.
    PERFORM read_process USING gt_lxe-objtype
          gt_lxe-object.
  ENDLOOP.

* DATA ELEMENT
  LOOP AT gt_lxe WHERE objtype = 'DTEL'.
    PERFORM read_process USING gt_lxe-objtype
          gt_lxe-object.
  ENDLOOP.

* DOMAIN
  LOOP AT gt_lxe WHERE objtype = 'VALU'.
    PERFORM read_process USING gt_lxe-objtype
          gt_lxe-object.
  ENDLOOP.

*  REFRESH LT_TYPE.
*  SELECT OBJ_TYPE
*    INTO TABLE LT_TYPE
*    FROM LXE_ATTOB
*   WHERE OBJ_TYPE IN ('TABL', 'BEZD', 'TABT').
*
*  LOOP AT GT_LXE INTO WA_ITAB WHERE OBJTYPE = 'TABL'.
*    OBJNAME  = WA_ITAB-OBJECT.
*    LOOP AT LT_TYPE.
*      OBJTYPE = LT_TYPE-OBJ_TYPE.
*      PERFORM READ_PROCESS.
*    ENDLOOP.
*  ENDLOOP.

* MESSAGE CLASS
  LOOP AT gt_lxe WHERE objtype = 'MESS'.
    PERFORM read_process USING gt_lxe-objtype
          gt_lxe-object.
  ENDLOOP.

  SORT gt_m BY objtype objname textkey.

ENDFORM. "READ_TEXT

*----------------------------------------------------------------------*
*  FORM  CHECK_KOREAN
*----------------------------------------------------------------------*
*  TEXT : 한글 포함여부 체크
*----------------------------------------------------------------------*
FORM check_korean USING p_text.

  DATA: l_len TYPE I,
        l_pos TYPE I,
        l_chk.

  DESCRIBE FIELD p_text LENGTH l_len IN CHARACTER MODE .
  CLEAR l_pos.
  DO l_len TIMES.
    IF p_text+l_pos(1) >= '가' AND p_text+l_pos(1) <= '힣'.
      l_chk = 'X'.
      EXIT.
    ENDIF.
    l_pos = l_pos + 1.
  ENDDO.

ENDFORM. "CHECK_KOREAN

*----------------------------------------------------------------------*
*  FORM  UPLOAD_EXCEL
*----------------------------------------------------------------------*
*  TEXT : Excel File에 변경 Text를 정리하여 Target 언어에 반영한다.
*----------------------------------------------------------------------*
FORM upload_excel.

  DATA: l_filename LIKE rlgrap-filename,
        xls_data1  TYPE TABLE OF alsmex_tabline WITH HEADER LINE.

  PERFORM file_name USING 'O' CHANGING l_filename.

  "Excel에서 변환 데이터 읽어오기
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  EXPORTING
    filename                = l_filename
    i_begin_col             = 2
    i_begin_row             = 3
    i_end_col               = 20
    i_end_row               = 10000
  TABLES
    intern                  = xls_data1
  EXCEPTIONS
    inconsistent_parameters = 1
    upload_ole              = 2
    OTHERS                  = 3.

  LOOP AT xls_data1.
    CASE xls_data1-col.
    WHEN '001'.
      gt_m-objtype = xls_data1-VALUE.
    WHEN '002'.
      gt_m-objname = xls_data1-VALUE.
    WHEN '003'.
      gt_m-textkey = xls_data1-VALUE.
    WHEN '004'.
      gt_m-s_text  = xls_data1-VALUE.
    WHEN '005'.
      gt_m-t_text  = xls_data1-VALUE.
    WHEN '006'.
      gt_m-unitmlt = xls_data1-VALUE.
    WHEN '007'.
      gt_m-uppcase = xls_data1-VALUE.
    ENDCASE.

    AT END OF row.
      CHECK gt_m-textkey NE space.
      APPEND gt_m.
    ENDAT.

  ENDLOOP.

  SORT gt_m BY objtype objname.

ENDFORM. "UPLOAD_EXCEL

*----------------------------------------------------------------------*
*  FORM  FILE_NAME
*----------------------------------------------------------------------*
*  TEXT : Open or Save File Name
*----------------------------------------------------------------------*
FORM file_name USING p_mode CHANGING p_file LIKE rlgrap-filename.
  DATA : l_file_table TYPE filetable,
        l_rc         TYPE I,
        l_user_actio TYPE I,
        file_name    TYPE string.

  DATA : lf_sel_file    TYPE string,
        lf_sel_path    TYPE string,
        lf_full_path   TYPE string,
        lf_user_action TYPE I.

  IF p_mode = 'O'.
    "Open File Name
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'File Open'
      default_filename        = space
      file_filter             = '*.xls'
      initial_directory       = 'C:\'
      multiselection          = space
    CHANGING
      file_table              = l_file_table
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 5.

    READ TABLE l_file_table INTO file_name INDEX 1.
  ELSE.
    "Save File Name
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Save File'
      default_file_name = space
      file_filter       = '*.xls'
      initial_directory = 'C:\'
    CHANGING
      filename          = lf_sel_file
      path              = lf_sel_path
      fullpath          = lf_full_path
      user_action       = lf_user_action
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 4.

    CONCATENATE lf_sel_path lf_sel_file INTO file_name.
  ENDIF.

  p_file = file_name.

ENDFORM. "FILE_NAME

*&---------------------------------------------------------------------*
*&      Form  LXE_T002_CHECK_LANGUAGE
*&---------------------------------------------------------------------*
FORM lxe_t002_check_language .

  "ISO 언어 코드로 변환
  CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
  EXPORTING
    r3_lang    = pm_tlang
  IMPORTING
    o_language = g_tlang.

  "ISO 언어 코드로 변환
  CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
  EXPORTING
    r3_lang    = pm_slang
  IMPORTING
    o_language = g_slang.

ENDFORM. " LXE_T002_CHECK_LANGUAGE

*----------------------------------------------------------------------*
*  FORM  READ_PROCESS
*----------------------------------------------------------------------*
*  TEXT :
*----------------------------------------------------------------------*
FORM read_process USING i_objtype
      i_objname.

  DATA: lt_pcx_s1 TYPE TABLE OF lxe_pcx_s1 WITH HEADER LINE.

  PERFORM lxe_obj_text_pair_read TABLES lt_pcx_s1
  USING  i_objtype  i_objname  'X'.

  LOOP AT lt_pcx_s1.

    CLEAR gt_m.
    MOVE-CORRESPONDING lt_pcx_s1 TO gt_m.

    gt_m-objtype = i_objtype.
    gt_m-objname = i_objname.

    COLLECT gt_m.

  ENDLOOP.

ENDFORM. " read_process

*&---------------------------------------------------------------------*
*&      Form  ALV_TOOLBAR
*&---------------------------------------------------------------------*
*  ALV 툴바에 버튼 추가하고자 할 경우
*----------------------------------------------------------------------*
FORM alv_toolbar USING p_grid_name
      e_object TYPE REF TO cl_alv_event_toolbar_set
      e_interactive.

  CASE p_grid_name.
  WHEN 'GO_GRID'.
    PERFORM alv_toolbar_0 USING e_object.
  WHEN 'GO_GRID1'.
    PERFORM alv_toolbar_1 USING e_object.
  ENDCASE.

ENDFORM. " ALV_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  ALV_TOOLBAR_1
*&---------------------------------------------------------------------*
FORM alv_toolbar_1 USING e_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: ls_toolbar TYPE stb_button.

  PERFORM alv_toolbar_set_cnt USING e_object
        go_grid1
        gt_110[].

* 버튼 삽입
  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = '&&SEP90'.
  ls_toolbar-butn_type = '3'.  "분리자
  INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = 'DELE'.
  ls_toolbar-ICON      = icon_delete_row.
  ls_toolbar-TEXT      = 'Delete'.
  ls_toolbar-quickinfo = 'Delete'.
  INSERT ls_toolbar INTO e_object->mt_toolbar INDEX 1.

ENDFORM. " ALV_TOOLBAR_1

*&---------------------------------------------------------------------*
*&      Form  ALV_TOOLBAR_0
*&---------------------------------------------------------------------*
FORM alv_toolbar_0 USING e_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: ls_toolbar    TYPE stb_button,
        l_yes_cnt(10) VALUE '0',
        l_no_cnt(10)  VALUE '0'.

  PERFORM alv_toolbar_set_cnt USING e_object
        go_grid
        gt_m[].

  LOOP AT gt_m.
    IF gt_m-t_text IS INITIAL.
      l_no_cnt  = l_no_cnt + 1.
    ELSE.
      l_yes_cnt = l_yes_cnt + 1.
    ENDIF.
  ENDLOOP.

  CONDENSE: l_no_cnt  NO-GAPS,
  l_yes_cnt NO-GAPS.

* 버튼 삽입
  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = '&&SEP90'.
  ls_toolbar-butn_type = '3'.  "분리자
  APPEND ls_toolbar TO e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = 'TR_YES'.
  ls_toolbar-ICON      = icon_led_green.
  CONCATENATE l_yes_cnt '件 已翻译' INTO ls_toolbar-TEXT.
  ls_toolbar-quickinfo = '已翻译'.
  APPEND ls_toolbar TO e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = 'TR_NO'.
  ls_toolbar-ICON      = icon_led_red.
  CONCATENATE l_no_cnt '件 未翻译' INTO ls_toolbar-TEXT.
  ls_toolbar-quickinfo = '未翻译'.
  APPEND ls_toolbar TO e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = '&&SEP90'.
  ls_toolbar-butn_type = '3'.  "분리자
  APPEND ls_toolbar TO e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-FUNCTION  = 'SEL_ALL'.
  ls_toolbar-ICON      = icon_execute_object.
  ls_toolbar-TEXT      = '批量更改'.
  ls_toolbar-quickinfo = '批量更改'.
  APPEND ls_toolbar TO e_object->mt_toolbar.

ENDFORM. " ALV_TOOLBAR_0

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
FORM alv_user_command USING p_grid_name
      e_ucomm LIKE sy-ucomm.

  CASE p_grid_name.
  WHEN 'GO_GRID'.
    PERFORM alv_user_cmd_0 USING e_ucomm.
  WHEN 'GO_GRID1'.
    PERFORM alv_user_cmd_1 USING e_ucomm.
  ENDCASE.

ENDFORM. " ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_CMD_1
*&---------------------------------------------------------------------*
FORM alv_user_cmd_1 USING e_ucomm LIKE sy-ucomm.

  CASE e_ucomm.
  WHEN 'DELE'.
    PERFORM alv_user_cmd_1_dele USING go_grid1 gt_fcat1[].
  WHEN 'TOT_CNT'.
    PERFORM alv_user_cmd_filter USING go_grid1 gt_fcat1[]  ''  ''.
  ENDCASE.

ENDFORM. " ALV_USER_CMD_1

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_CMD_1_DELE
*&---------------------------------------------------------------------*
FORM alv_user_cmd_1_dele USING po_grid TYPE REF TO cl_gui_alv_grid
      pt_fcat TYPE lvc_t_fcat.

* 선택건
  PERFORM alv_set_mark USING po_grid gt_110[].
  IF sy-tabix = 0.
    MESSAGE s000 WITH '请选择要删除的行'.
    EXIT.
  ENDIF.

* 확인
  DATA: l_answer.
  PERFORM popup_conf USING    '선택건을 삭제 하시겠습니까?'
  CHANGING l_answer.
  IF l_answer <> '1'. EXIT. ENDIF.

  LOOP AT gt_110 WHERE mark = 'X'.

    DELETE FROM ZWFT_CAG WHERE erdat = gt_110-erdat
    AND erzet = gt_110-erzet
    AND seqno = gt_110-seqno.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e000 WITH 'Delete Error [ZWFT_CAG]'.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.

* REFRESH
  DELETE gt_110 WHERE mark = 'X'.

  PERFORM alv_refresh USING po_grid  pt_fcat[]  ''.

  MESSAGE s000 WITH 'Success'.

ENDFORM. " ALV_USER_CMD_1_DELE

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_CMD_0
*&---------------------------------------------------------------------*
FORM alv_user_cmd_0 USING e_ucomm LIKE sy-ucomm.

  CASE e_ucomm.
  WHEN 'SEL_ALL'.   "일괄 적용
    PERFORM alv_user_cmd_sel_all USING go_grid gt_fcat[].
  WHEN 'TR_YES'.
    PERFORM alv_user_cmd_filter USING go_grid gt_fcat[]  'T_TEXT'
          'NE_INITIAL'.
  WHEN 'TR_NO'.
    PERFORM alv_user_cmd_filter USING go_grid gt_fcat[]  'T_TEXT'
          'EQ_INITIAL'.
  WHEN 'TOT_CNT'.
    PERFORM alv_user_cmd_filter USING go_grid gt_fcat[]  ''  ''.
  ENDCASE.

ENDFORM. " ALV_USER_CMD_0

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_CMD_SEL_ALL
*&---------------------------------------------------------------------*
FORM alv_user_cmd_sel_all USING po_grid TYPE REF TO cl_gui_alv_grid
      pt_fcat TYPE lvc_t_fcat.

  LOOP AT gt_m.

    IF gt_m-t_text IS NOT INITIAL. CONTINUE. ENDIF.

    PERFORM sel_ZWFT_CAG USING    gt_m-s_text
    CHANGING gt_m-t_text
      gt_m-ico_sel.

    IF gt_m-t_text IS INITIAL. CONTINUE. ENDIF.

    MODIFY gt_m.

  ENDLOOP.

  PERFORM alv_refresh USING po_grid  pt_fcat[]  'F'.

ENDFORM. " ALV_USER_CMD_SEL_ALL

*&---------------------------------------------------------------------*
*&      Form  ALV_CLEAR_0100
*&---------------------------------------------------------------------*
FORM alv_clear_0100 .

  IF go_grid IS NOT INITIAL. CALL METHOD go_grid->free. ENDIF.
  IF go_cont IS NOT INITIAL. CALL METHOD go_cont->free. ENDIF.

  CLEAR: go_cont, go_grid.

ENDFORM. " ALV_CLEAR_0100

*&---------------------------------------------------------------------*
*&      Form  ALV_CLEAR_0110
*&---------------------------------------------------------------------*
FORM alv_clear_0110 .

  IF go_grid1 IS NOT INITIAL. CALL METHOD go_grid1->free. ENDIF.
  IF go_cont1 IS NOT INITIAL. CALL METHOD go_cont1->free. ENDIF.

  CLEAR: go_cont1, go_grid1.

ENDFORM. " ALV_CLEAR_0110

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM alv_fieldcat_0100 TABLES pt_fcat STRUCTURE lvc_s_fcat.

  LOOP AT pt_fcat.

    pt_fcat-col_opt = 'X'.
    pt_fcat-no_out  = ' '.

    CASE pt_fcat-fieldname.
    WHEN 'OBJTYPE'.
      pt_fcat-col_pos    = '10'.
      pt_fcat-coltext    = 'Type'.
    WHEN 'OBJNAME'.
      pt_fcat-col_pos    = '20'.
    WHEN 'TEXTKEY'.
      pt_fcat-col_pos    = '30'.
    WHEN 'S_TEXT'.
      pt_fcat-col_pos    = '40'.
      pt_fcat-coltext    = 'Source Text'.
      pt_fcat-emphasize  = 'C700'.
    WHEN 'ICO_SEL'.
      pt_fcat-col_pos    = '45'.
      pt_fcat-coltext    = '조회'.
      pt_fcat-just       = 'C'.
      pt_fcat-HOTSPOT    = 'X'.
      pt_fcat-tooltip    = '번역풀 조회'.
    WHEN 'T_TEXT'.
      pt_fcat-col_pos    = '50'.
      pt_fcat-coltext    = 'Target Text'.
      pt_fcat-EDIT       = 'X'.
    WHEN 'UNITMLT'.
      pt_fcat-col_pos    = '60'.
    WHEN 'UPPCASE'.
      pt_fcat-col_pos    = '70'.
    WHEN OTHERS.
      pt_fcat-no_out     = 'X'.
    ENDCASE.

    IF pt_fcat-coltext IS NOT INITIAL.
      pt_fcat-reptext   = pt_fcat-coltext.
      pt_fcat-scrtext_l = pt_fcat-coltext.
      pt_fcat-scrtext_m = pt_fcat-coltext.
      pt_fcat-scrtext_s = pt_fcat-coltext.
    ENDIF.

    MODIFY pt_fcat.

  ENDLOOP.

ENDFORM. " ALV_FIELDCAT_0100

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_0110
*&---------------------------------------------------------------------*
FORM alv_fieldcat_0110 TABLES pt_fcat STRUCTURE lvc_s_fcat.

  LOOP AT pt_fcat.

    pt_fcat-col_opt = 'X'.
    pt_fcat-no_out  = ' '.

    CASE pt_fcat-fieldname.
    WHEN 'ICO_SEL'.
      pt_fcat-col_pos    = '1'.
      pt_fcat-coltext    = '적용'.
      pt_fcat-just       = 'C'.
      pt_fcat-HOTSPOT    = 'X'.
      pt_fcat-tooltip    = '값 적용'.
    WHEN 'TXT_KO'.
      pt_fcat-col_pos    = '10'.
      pt_fcat-coltext    = '한글'.
      IF pm_slang NE '3' AND pm_tlang NE '3'. "korean
        pt_fcat-no_out  = 'X'.
      ENDIF.
    WHEN 'TXT_EN'.
      pt_fcat-col_pos    = '20'.
      pt_fcat-coltext    = '영어'.
      pt_fcat-EDIT       = 'X'.
      IF pm_slang NE 'E' AND pm_tlang NE 'E'. "English
        pt_fcat-no_out  = 'X'.
      ENDIF.
    WHEN 'TXT_ZH'.
      pt_fcat-col_pos    = '30'.
      pt_fcat-coltext    = '중문'.
      pt_fcat-EDIT       = 'X'.
      IF pm_slang NE '1' AND pm_tlang NE '1'. "chinese
        pt_fcat-no_out  = 'X'.
      ENDIF.
    WHEN 'OBJTYPE'.
      pt_fcat-col_pos    = '40'.
    WHEN 'OBJNAME'.
      pt_fcat-col_pos    = '50'.
    WHEN 'TEXTKEY'.
      pt_fcat-col_pos    = '60'.
    WHEN 'ERDAT'.
      pt_fcat-col_pos    = '70'.
    WHEN 'ERZET'.
      pt_fcat-col_pos    = '80'.
    WHEN 'SEQNO'.
      pt_fcat-col_pos    = '90'.
    WHEN OTHERS.
      pt_fcat-no_out     = 'X'.
    ENDCASE.

    IF pt_fcat-coltext IS NOT INITIAL.
      pt_fcat-reptext   = pt_fcat-coltext.
      pt_fcat-scrtext_l = pt_fcat-coltext.
      pt_fcat-scrtext_m = pt_fcat-coltext.
      pt_fcat-scrtext_s = pt_fcat-coltext.
    ENDIF.

    MODIFY pt_fcat.

  ENDLOOP.

ENDFORM. " ALV_FIELDCAT_0110

*&---------------------------------------------------------------------*
*&      Form  ALV_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM alv_data_changed USING p_grid_name
      er_data_changed TYPE REF TO
      cl_alv_changed_data_protocol
      e_onf4
      e_onf4_before
      e_onf4_after
      e_ucomm.

  IF e_onf4 = 'X' AND e_onf4_before = 'X' AND e_onf4_after = ''.
    EXIT.
  ENDIF.

  CASE p_grid_name.
  WHEN 'GO_GRID1'.
    PERFORM alv_data_chg_1 USING er_data_changed.
  ENDCASE.

ENDFORM. " ALV_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  ALV_DATA_CHG_1
*&---------------------------------------------------------------------*
FORM alv_data_chg_1 USING er_data_changed TYPE REF TO
      cl_alv_changed_data_protocol.

  DATA: ls_modi     TYPE lvc_s_modi,
        l_fname(30).

  FIELD-SYMBOLS: <value>.

  LOOP AT er_data_changed->mt_mod_cells INTO ls_modi.

    READ TABLE gt_110 INDEX ls_modi-row_id.
    IF sy-subrc <> 0. EXIT. ENDIF.

*    CONCATENATE 'GT_M-' LS_MODI-FIELDNAME INTO L_FNAME.
*    ASSIGN (L_FNAME) TO <VALUE>.
*
*    CALL METHOD ER_DATA_CHANGED->GET_CELL_VALUE
*      EXPORTING
*        I_ROW_ID    = LS_MODI-ROW_ID
*        I_FIELDNAME = LS_MODI-FIELDNAME
*      IMPORTING
*        E_VALUE     = <VALUE>.            "LS_MODI-VALUE.
*
*    CASE LS_MODI-FIELDNAME.
*
*      WHEN 'LQPOS'.
*        PERFORM ALV_DATA_CHG_0_LQPOS USING    ER_DATA_CHANGED
*                                              LS_MODI
*                                     CHANGING GT_M.
*
*    ENDCASE.

    gt_110-edit_gb = 'X'.

    MODIFY gt_110 INDEX ls_modi-row_id.

  ENDLOOP .

ENDFORM. " ALV_DATA_CHG_1

*&---------------------------------------------------------------------*
*&      Form  ALV_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM alv_double_click USING p_grid_name
      ps_row TYPE lvc_s_row
      ps_col TYPE lvc_s_col.

  CHECK ps_row-rowtype IS INITIAL.

  CASE p_grid_name.
  WHEN 'GO_GRID'.    "MAIN 화면
    PERFORM alv_dbl_clk_0 USING ps_row-INDEX ps_col-fieldname.
  WHEN 'GO_GRID1'.   "번역풀 화면
    PERFORM alv_dbl_clk_1 USING ps_row-INDEX ps_col-fieldname.
  ENDCASE.

ENDFORM. " ALV_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  ALV_DBL_CLK_1
*&---------------------------------------------------------------------*
FORM alv_dbl_clk_1 USING p_row
      p_col.

  IF g_clk_tabix IS INITIAL. EXIT. ENDIF.

  READ TABLE gt_110 INDEX p_row.
  IF sy-subrc <> 0. EXIT. ENDIF.

  CASE p_col.

  WHEN 'ICO_SEL'.
    IF pm_tlang = 'E'."englisgh
      gt_m-t_text = gt_110-txt_en.
  ELSEIF pm_tlang  = '1'."chinese
      gt_m-t_text = gt_110-txt_zh.
  ELSEIF pm_tlang = '3'. "korean
      gt_m-t_text = gt_110-txt_ko.
    ENDIF.
    MODIFY gt_m INDEX g_clk_tabix TRANSPORTING t_text.
    PERFORM alv_refresh USING go_grid  gt_fcat[]  'F'.
    PERFORM alv_set_new_ok_code USING 'DBL_CLK_1_ICO_SEL'.

  ENDCASE.

ENDFORM. " ALV_DBL_CLK_1

*&---------------------------------------------------------------------*
*&      Form  ALV_DBL_CLK_0
*&---------------------------------------------------------------------*
FORM alv_dbl_clk_0 USING p_row
      p_col.

  READ TABLE gt_m INDEX p_row.
  IF sy-subrc <> 0. EXIT. ENDIF.

  CASE p_col.

  WHEN 'S_TEXT'.  "소스텍스트
    IF gt_m-s_text IS INITIAL. EXIT. ENDIF.
    gt_m-t_text = gt_m-s_text.
    MODIFY gt_m INDEX p_row.
    PERFORM alv_refresh USING go_grid  gt_fcat[]  ''.

  WHEN 'ICO_SEL'.
    PERFORM alv_dbl_clk_0_ico_sel USING    p_row
    CHANGING gt_m.

  ENDCASE.

ENDFORM. " ALV_DBL_CLK_0

*&---------------------------------------------------------------------*
*&      Form  ALV_DBL_CLK_0_ICO_SEL
*&---------------------------------------------------------------------*
FORM alv_dbl_clk_0_ico_sel USING p_row
CHANGING ps_m STRUCTURE gt_m.

  DATA: l_t_text  TYPE lxe_pcx_s1-t_text,
        l_ico_sel LIKE ICON-ID.
  DATA: lv_condi  TYPE char255,
        lv_condi1 TYPE char10.

  PERFORM sel_ZWFT_CAG USING    ps_m-s_text
  CHANGING l_t_text
    l_ico_sel.

  IF l_t_text IS NOT INITIAL.
    ps_m-t_text  = l_t_text.
    ps_m-ico_sel = l_ico_sel.
    MODIFY gt_m FROM ps_m INDEX p_row.
    PERFORM alv_refresh USING go_grid  gt_fcat[]  'F'.
    EXIT.
  ENDIF.

  DATA: l_text(132).

  l_text = ps_m-s_text.

*  REPLACE ALL OCCURRENCES OF ' ' IN L_TEXT WITH '*'.
  REPLACE ALL OCCURRENCES OF '.' IN l_text WITH '*'.
  REPLACE ALL OCCURRENCES OF '&' IN l_text WITH '*'.

  CONCATENATE '*' l_text '*' INTO l_text.

  CLEAR: so_text, so_text[].
  so_text-SIGN   = 'I'.
  so_text-option = 'CP'.
  so_text-low    = l_text.
  APPEND so_text.

  CLEAR: gt_110, gt_110[].

  IF pm_slang = '3'. "korean
    lv_condi = 'TXT_KO IN SO_TEXT'.
    lv_condi1 = 'TXT_KO'.
ELSEIF pm_slang = 'E'."englisgh
    lv_condi = 'TXT_EN IN SO_TEXT'.
    lv_condi1 = 'TXT_EN'.
ELSEIF pm_slang = '1'."chinese
    lv_condi = 'TXT_ZH IN SO_TEXT'.
    lv_condi1 = 'TXT_ZH'.
  ENDIF.

  IF pm_tlang = 'E'."englisgh
    CONCATENATE lv_condi
    'AND'
    `TXT_EN NE ''`
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang  = '1'."chinese
    CONCATENATE lv_condi
    'AND'
    `TXT_ZH NE ''`
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang = '3'. "korean
    CONCATENATE lv_condi
    'AND'
    `TXT_KO NE ''`
    INTO lv_condi SEPARATED BY space.
  ENDIF.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE gt_110
  FROM ZWFT_CAG
  WHERE (lv_condi).

  IF gt_110[] IS INITIAL.
    MESSAGE s000 WITH '번역풀이 없습니다.'.
    EXIT.
  ENDIF.

  gt_110-ico_sel = icon_execute_object.
  MODIFY gt_110 TRANSPORTING ico_sel WHERE ico_sel = ''.

  SORT gt_110 BY (lv_condi1).

  g_clk_tabix = p_row.

  CALL SCREEN 0110 STARTING AT 10 2 ENDING AT 120 20.

  CLEAR g_clk_tabix.

ENDFORM. " ALV_DBL_CLK_0_ICO_SEL

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCR
*&---------------------------------------------------------------------*
FORM at_sel_scr .

  CLEAR g_error.

  IF so_prog[]  IS INITIAL AND
  so_area[]  IS INITIAL AND
  so_msag[]  IS INITIAL AND
  so_dtel[]  IS INITIAL AND
  so_doma[]  IS INITIAL.
    IF so_class[] IS INITIAL.
      MESSAGE s000(zfi) WITH '개별 오브젝트 혹은 개발클래스를 입력하세요'(m01)
      DISPLAY LIKE 'E'.
      g_error = 'X'.
      EXIT.
    ELSE.
      g_only_devcls = 'X'.
    ENDIF.
  ENDIF.

ENDFORM. " AT_SEL_SCR

*&---------------------------------------------------------------------*
*&      Form  SEL_PROG
*&---------------------------------------------------------------------*
FORM sel_prog .

  DATA: lt_prog LIKE gt_lxe OCCURS 0 WITH HEADER LINE,
        lt_tcod LIKE tstc   OCCURS 0 WITH HEADER LINE,
        lt_scrn LIKE d020s  OCCURS 0 WITH HEADER LINE.

  SELECT trdir~name AS object
  INTO CORRESPONDING FIELDS OF TABLE lt_prog
  FROM trdir JOIN tadir ON tadir~pgmid    = 'R3TR' AND
  tadir~object   = 'PROG' AND
  tadir~obj_name = trdir~name
  WHERE trdir~name     IN so_prog
  AND tadir~devclass IN so_class.

  LOOP AT lt_prog.

*   프로그램
    CLEAR gt_lxe.
    gt_lxe-object  = lt_prog-object.
    gt_lxe-objtype = 'PROG'.
    APPEND gt_lxe.

*   T-CODE
    CLEAR: lt_tcod, lt_tcod[].
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_tcod
    FROM tstc
    WHERE pgmna = lt_prog-object.

    LOOP AT lt_tcod.
      CLEAR gt_lxe.
      gt_lxe-object  = lt_tcod-tcode.
      gt_lxe-objtype = 'TRAN'.
      APPEND gt_lxe.
    ENDLOOP.

*   SCREEN FIELD
    CLEAR: lt_scrn, lt_scrn[].
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_scrn
    FROM d020s
    WHERE prog  = lt_prog-object
    AND TYPE <> 'S'.

    LOOP AT lt_scrn.

*      화면 제목이므로 빼도 상관 없음
*      CLEAR GT_LXE.
*      GT_LXE-OBJECT    = LT_SCRN-PROG.
*      GT_LXE-OBJECT+40 = LT_SCRN-FNUM.
*      GT_LXE-OBJTYPE   = 'SRH4'.
*      APPEND GT_LXE.

      CLEAR gt_lxe.
      gt_lxe-object    = lt_scrn-prog.
      gt_lxe-object+40 = lt_scrn-dnum.
*      GT_LXE-OBJECT+40 = LT_SCRN-FNUM.
      gt_lxe-objtype   = 'SRT4'.
      APPEND gt_lxe.

    ENDLOOP.

  ENDLOOP.

ENDFORM. " SEL_PROG

*&---------------------------------------------------------------------*
*&      Form  0100_SAVE
*&---------------------------------------------------------------------*
FORM 0100_save .

* 변경사항 IT에 반영
  CALL METHOD go_grid->check_changed_data.

* 행선택
  PERFORM alv_set_mark USING go_grid  gt_m[].
  IF sy-tabix = 0.
    MESSAGE s106(1h) DISPLAY LIKE 'E'.  "라인을 선택하십시오.
    LEAVE SCREEN.
  ENDIF.

* 확인
  DATA: l_answer.
  PERFORM popup_conf USING    '要保存数据吗 ？'
  CHANGING l_answer.
  IF l_answer <> '1'. LEAVE SCREEN. ENDIF.

* 번역풀에 저장 작업 먼저 한다.
  PERFORM save_ZWFT_CAG.

* 선택된 건
  DATA: lt_m LIKE gt_m OCCURS 0 WITH HEADER LINE.

  LOOP AT gt_m WHERE mark = 'X'.
    CLEAR lt_m.
    MOVE-CORRESPONDING gt_m TO lt_m.
    APPEND lt_m.
  ENDLOOP.

* 번역
  DATA: lt_pcx_s1_r TYPE lxe_pcx_s1 OCCURS 0 WITH HEADER LINE,
        lt_pcx_s1_m TYPE lxe_pcx_s1 OCCURS 0 WITH HEADER LINE,
        lt_pcx_s1_w TYPE lxe_pcx_s1 OCCURS 0 WITH HEADER LINE,
        l_new,
        l_end.

  SORT lt_m.

  LOOP AT lt_m.

    AT NEW    objname. l_new = 'X'. ENDAT.
    AT END OF objname. l_end = 'X'. ENDAT.

    IF l_new = 'X'.
      CLEAR: lt_pcx_s1_r, lt_pcx_s1_r[],
      lt_pcx_s1_m, lt_pcx_s1_m[],
      lt_pcx_s1_w, lt_pcx_s1_w[],
      l_new.
    ENDIF.

    CLEAR lt_pcx_s1_m.
    MOVE-CORRESPONDING lt_m TO lt_pcx_s1_m.
    APPEND lt_pcx_s1_m.

    IF l_end = ''. CONTINUE. ENDIF.

    SORT lt_pcx_s1_m BY textkey.

    CLEAR l_end.
    PERFORM lxe_obj_text_pair_read TABLES lt_pcx_s1_r
    USING  lt_m-objtype  lt_m-objname  ''
          .

    LOOP AT lt_pcx_s1_r.

      CLEAR lt_pcx_s1_w.
      MOVE-CORRESPONDING lt_pcx_s1_r TO lt_pcx_s1_w.

      READ TABLE lt_pcx_s1_m WITH KEY textkey = lt_pcx_s1_r-textkey
      BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING lt_pcx_s1_m TO lt_pcx_s1_w.
      ELSE.
        READ TABLE gt_m WITH KEY objtype = lt_m-objtype
        objname = lt_m-objname
        textkey = lt_pcx_s1_r-textkey.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING gt_m TO lt_pcx_s1_w.
        ENDIF.
      ENDIF.

*      IF LT_PCX_S1_W-T_TEXT IS INITIAL.
*        LT_PCX_S1_W-T_TEXT = LT_PCX_S1_W-S_TEXT.
*      ENDIF.

      APPEND lt_pcx_s1_w.

    ENDLOOP.

    PERFORM lxe_obj_text_pair_write TABLES lt_pcx_s1_w
    USING  lt_m.

  ENDLOOP.

  COMMIT WORK.
  MESSAGE s000 WITH '转换成功.'.

ENDFORM. " 0100_SAVE

*&---------------------------------------------------------------------*
*&      Form  LXE_OBJ_TEXT_PAIR_READ
*&---------------------------------------------------------------------*
FORM lxe_obj_text_pair_read TABLES et_pcx_s1 STRUCTURE lxe_pcx_s1
USING  iv_objtype
      iv_objname
      iv_read_only.

  CLEAR: et_pcx_s1, et_pcx_s1[].

  CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
  EXPORTING
    t_lang    = g_tlang
    s_lang    = g_slang
    custmnr   = c_custmnr
    objtype   = iv_objtype
    objname   = iv_objname
    read_only = iv_read_only     "'X'
*   IMPORTING
*     COLLTYP   =
*     COLLNAM   =
*     DOMATYP   =
*     DOMANAM   =
*     PSTATUS   =
*     O_LANG    =
  TABLES
    lt_pcx_s1 = et_pcx_s1
*     LT_CONDITIONS       =
*     LT_WHERECLAUS       =
    .

ENDFORM. " LXE_OBJ_TEXT_PAIR_READ

*&---------------------------------------------------------------------*
*&      Form  LXE_OBJ_TEXT_PAIR_WRITE
*&---------------------------------------------------------------------*
FORM lxe_obj_text_pair_write TABLES it_pcx_s1 STRUCTURE lxe_pcx_s1
USING  is_m      STRUCTURE gt_m.

  DATA: l_pstatus TYPE  lxestatprc.

  CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
  EXPORTING
    t_lang    = g_tlang
    s_lang    = g_slang
    custmnr   = c_custmnr
    objtype   = is_m-objtype
    objname   = is_m-objname
  IMPORTING
    pstatus   = l_pstatus
  TABLES
    lt_pcx_s1 = it_pcx_s1.

  IF l_pstatus = 'F'.
    MESSAGE i000 WITH is_m-objname g_slang g_tlang '번역오류'.
    LEAVE SCREEN.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.
*    IMPORTING
*      RETURN        =
    .

  ENDIF.

ENDFORM. " LXE_OBJ_TEXT_PAIR_WRITE

*&---------------------------------------------------------------------*
*&      Form  POPUP_CONF
*&---------------------------------------------------------------------*
*  저장여부 등 팝업 질문창을 띄운다.   '1' = YES
*----------------------------------------------------------------------*
FORM popup_conf USING p_text
CHANGING p_answer.

  CLEAR p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    TITLEBAR       = 'Confirm'   "' '
*     DIAGNOSE_OBJECT             = ' '
    text_question  = p_text
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
  IMPORTING
    answer         = p_answer
*   TABLES
*     PARAMETER      =
  EXCEPTIONS
    text_not_found = 1
    OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. " POPUP_CONF

*&---------------------------------------------------------------------*
*&      Form  SAVE_ZWFT_CAG
*&---------------------------------------------------------------------*
FORM save_ZWFT_CAG .

  DATA: lt_9000 LIKE ZWFT_CAG OCCURS 0 WITH HEADER LINE,
        l_seqno TYPE ZWFT_CAG-seqno.

  DATA: lv_condi TYPE char50.

  IF pm_slang = '3'. "korean
    lv_condi = 'TXT_KO = GT_M-S_TEXT'.
ELSEIF pm_slang = 'E'."englisgh
    lv_condi = 'TXT_EN = GT_M-S_TEXT'.
ELSEIF pm_slang = '1'."chinese
    lv_condi = 'TXT_ZH = GT_M-S_TEXT'.
  ENDIF.

  IF pm_tlang = 'E'."englisgh
    CONCATENATE lv_condi
    'AND'
    'TXT_EN = GT_M-T_TEXT'
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang  = '1'."chinese
    CONCATENATE lv_condi
    'AND'
    'TXT_ZH = GT_M-T_TEXT'
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang = '3'. "korean
    CONCATENATE lv_condi
    'AND'
    'TXT_KO = GT_M-T_TEXT'
    INTO lv_condi SEPARATED BY space.
  ENDIF.

  LOOP AT gt_m WHERE mark = 'X'
  AND t_text IS NOT INITIAL.

    CONDENSE: gt_m-s_text,
    gt_m-t_text.
    MODIFY gt_m.

    CLEAR ZWFT_CAG.

    SELECT SINGLE * FROM ZWFT_CAG
    WHERE (lv_condi).

    IF sy-subrc = 0.
      IF ZWFT_CAG-objname IS INITIAL.
        UPDATE ZWFT_CAG SET objtype = gt_m-objtype
        objname = gt_m-objname
        textkey = gt_m-textkey
        WHERE erdat   = ZWFT_CAG-erdat
        AND erzet   = ZWFT_CAG-erzet
        AND seqno   = ZWFT_CAG-seqno.
      ENDIF.
      CONTINUE.
    ENDIF.

    IF pm_slang = '3'. "korean
      IF pm_tlang = 'E'."englisgh
        READ TABLE lt_9000 WITH KEY txt_ko = gt_m-s_text
        txt_en = gt_m-t_text.
    ELSEIF pm_tlang  = '1'."chinese
        READ TABLE lt_9000 WITH KEY txt_ko = gt_m-s_text
        txt_zh = gt_m-t_text.
      ENDIF.
  ELSEIF pm_slang = 'E'."englisgh
      IF pm_tlang  = '1'."chinese
        READ TABLE lt_9000 WITH KEY txt_en = gt_m-s_text
        txt_zh = gt_m-t_text.
    ELSEIF pm_tlang = '3'. "korean
        READ TABLE lt_9000 WITH KEY txt_en = gt_m-s_text
        txt_ko = gt_m-t_text.
      ENDIF.
  ELSEIF pm_slang = '1'."chinese
      IF pm_tlang = 'E'."englisgh
        READ TABLE lt_9000 WITH KEY txt_zh = gt_m-s_text
        txt_en = gt_m-t_text.
    ELSEIF pm_tlang = '3'. "korean
        READ TABLE lt_9000 WITH KEY txt_zh = gt_m-s_text
        txt_ko = gt_m-t_text.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    CLEAR lt_9000.
    lt_9000-erdat   = sy-datum.
    lt_9000-erzet   = sy-uzeit.
    lt_9000-seqno   = l_seqno = l_seqno + 1.
    IF pm_slang = '3'. "korean
      lt_9000-txt_ko  = gt_m-s_text.
  ELSEIF pm_slang = 'E'."englisgh
      lt_9000-txt_en  = gt_m-s_text.
  ELSEIF pm_slang = '1'."chinese
      lt_9000-txt_zh  = gt_m-s_text.
    ENDIF.

    IF pm_tlang = '3'. "korean
      lt_9000-txt_ko  = gt_m-t_text.
  ELSEIF pm_tlang = 'E'."englisgh
      lt_9000-txt_en  = gt_m-t_text.
  ELSEIF pm_tlang = '1'."chinese
      lt_9000-txt_zh  = gt_m-t_text.
    ENDIF.
    lt_9000-objtype = gt_m-objtype.
    lt_9000-objname = gt_m-objname.
    lt_9000-textkey = gt_m-textkey.
    APPEND lt_9000.

  ENDLOOP.

  IF lt_9000[] IS INITIAL. EXIT. ENDIF.

  MODIFY ZWFT_CAG FROM TABLE lt_9000.

  IF sy-subrc <> 0.
    ROLLBACK WORK.
    MESSAGE e000 WITH 'Save Error [ZWFT_CAG]'.
  ENDIF.

  COMMIT WORK.
  MESSAGE s000 WITH 'Success'.

ENDFORM. " SAVE_ZWFT_CAG

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  g_pgm_title = sy-TITLE.

ENDFORM. " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  0100_XSEL
*&---------------------------------------------------------------------*
FORM 0100_xsel .

  CLEAR: gt_110, gt_110[].

  CALL SCREEN 0110.   " STARTING AT 10 2 ENDING AT 120 20.

ENDFORM. " 0100_XSEL

*&---------------------------------------------------------------------*
*&      Form  0110_OKAY
*&---------------------------------------------------------------------*
FORM 0110_okay .

  CLEAR: gt_110, gt_110[].

  DATA: lv_condi  TYPE char255,
        lv_condi1 TYPE char10.

  IF pm_slang = '3'. "korean
    lv_condi = 'TXT_KO IN SO_TEXT'.
    lv_condi1 = 'TXT_KO'.
ELSEIF pm_slang = 'E'."englisgh
    lv_condi = 'TXT_EN IN SO_TEXT'.
    lv_condi1 = 'TXT_EN'.
ELSEIF pm_slang = '1'."chinese
    lv_condi = 'TXT_ZH IN SO_TEXT'.
    lv_condi1 = 'TXT_ZH'.
  ENDIF.

  IF pm_tlang = 'E'."englisgh
    CONCATENATE lv_condi
    'AND'
    `TXT_EN NE ''`
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang  = '1'."chinese
    CONCATENATE lv_condi
    'AND'
    `TXT_ZH NE ''`
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang = '3'. "korean
    CONCATENATE lv_condi
    'AND'
    `TXT_KO NE ''`
    INTO lv_condi SEPARATED BY space.
  ENDIF.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE gt_110
  FROM ZWFT_CAG
  WHERE (lv_condi).

  gt_110-ico_sel = icon_execute_object.
  MODIFY gt_110 TRANSPORTING ico_sel WHERE ico_sel = ''.

  SORT gt_110 BY (lv_condi1).

  PERFORM alv_refresh USING go_grid1  gt_fcat1[]  'F'.

ENDFORM. " 0110_OKAY

*&---------------------------------------------------------------------*
*&      Form  SEL_ZWFT_CAG
*&---------------------------------------------------------------------*
FORM sel_ZWFT_CAG USING p_s_text
CHANGING p_t_text
  e_icon.

  e_icon = icon_create_text.

  DATA: lt_9000 LIKE ZWFT_CAG OCCURS 0 WITH HEADER LINE,
        l_s_txt TYPE text132.
  DATA: lv_condi TYPE char255.

  l_s_txt  = p_s_text.
  CONDENSE l_s_txt .

  IF pm_slang = '3'. "korean
    lv_condi = 'TXT_KO = L_S_TXT'.
ELSEIF pm_slang = 'E'."englisgh
    lv_condi = 'TXT_EN = L_S_TXT'.
ELSEIF pm_slang = '1'."chinese
    lv_condi = 'TXT_ZH = L_S_TXT'.
  ENDIF.

  IF pm_tlang = 'E'."englisgh
    CONCATENATE lv_condi
    'AND'
    `TXT_EN NE ''`
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang  = '1'."chinese
    CONCATENATE lv_condi
    'AND'
    `TXT_ZH NE ''`
    INTO lv_condi SEPARATED BY space.
ELSEIF pm_tlang = '3'. "korean
    CONCATENATE lv_condi
    'AND'
    `TXT_KO NE ''`
    INTO lv_condi SEPARATED BY space.
  ENDIF.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE lt_9000
  FROM ZWFT_CAG
  WHERE (lv_condi).


  IF lt_9000[] IS INITIAL.
    EXIT.
  ENDIF.

  e_icon   = icon_change_text.

  IF LINES( lt_9000 ) = 1.
    READ TABLE lt_9000 INDEX 1.

    IF pm_tlang = 'E'."englisgh
      p_t_text = lt_9000-txt_en.
  ELSEIF pm_tlang  = '1'."chinese
      p_t_text = lt_9000-txt_zh.
  ELSEIF pm_tlang = '3'. "korean
      p_t_text = lt_9000-txt_ko.
    ENDIF.

    EXIT.
  ENDIF.

ENDFORM. " SEL_ZWFT_CAG

*&---------------------------------------------------------------------*
*&      Form  0110_SAVE
*&---------------------------------------------------------------------*
FORM 0110_save .

* 변경사항 IT에 반영
  CALL METHOD go_grid1->check_changed_data.

* 수정된 건
  DATA: lt_9000 LIKE ZWFT_CAG OCCURS 0 WITH HEADER LINE.

  LOOP AT gt_110 WHERE edit_gb = 'X'.
    CLEAR lt_9000.
    MOVE-CORRESPONDING gt_110 TO lt_9000.
    APPEND lt_9000.
  ENDLOOP.

  IF lt_9000[] IS INITIAL.
    MESSAGE s000 WITH '수정건이 없습니다'.
    EXIT.
  ENDIF.

* 확인
  DATA: l_answer.
  PERFORM popup_conf USING    '수정건을 저장하시겠습니까?'
  CHANGING l_answer.
  IF l_answer <> '1'. LEAVE SCREEN. ENDIF.

* 저장
  MODIFY ZWFT_CAG FROM TABLE lt_9000.

  IF sy-subrc <> 0.
    ROLLBACK WORK.
    MESSAGE e000 WITH 'Save Error [ZWFT_CAG]'.
  ENDIF.

  COMMIT WORK.
  MESSAGE s000 WITH 'Success'.

ENDFORM. " 0110_SAVE

*&---------------------------------------------------------------------*
*&      Form  0110_DBL_CLK_1_ICO_SEL
*&---------------------------------------------------------------------*
FORM 0110_dbl_clk_1_ico_sel .

  PERFORM alv_clear_0110.

  LEAVE TO SCREEN 0.

ENDFORM. " 0110_DBL_CLK_1_ICO_SEL

*&---------------------------------------------------------------------*
*&      Form  MODI_GT_M
*&---------------------------------------------------------------------*
FORM modi_gt_m .

  DATA: l_t_text TYPE lxe_pcx_s1-t_text.

  LOOP AT gt_m.

    PERFORM sel_ZWFT_CAG USING    gt_m-s_text
    CHANGING l_t_text
      gt_m-ico_sel.

    MODIFY gt_m.

  ENDLOOP.

*  GT_M-ICO_SEL = ICON_EXECUTE_OBJECT.
*  MODIFY GT_M TRANSPORTING ICO_SEL WHERE ICO_SEL = ''.

ENDFORM. " MODI_GT_M

*&---------------------------------------------------------------------*
*&      Form  SEL_AREA
*&---------------------------------------------------------------------*
FORM sel_area .

  DATA: lt_fg   LIKE gt_lxe OCCURS 0 WITH HEADER LINE,
        lt_tcod LIKE tstc   OCCURS 0 WITH HEADER LINE,
        lt_scrn LIKE d020s  OCCURS 0 WITH HEADER LINE.
  DATA:ls_prog TYPE char30.
  SELECT DISTINCT tlibg~area AS object
  INTO CORRESPONDING FIELDS OF TABLE lt_fg
  FROM tlibg JOIN tadir ON tadir~pgmid    = 'R3TR' AND
  tadir~object   = 'FUGR' AND
  tadir~obj_name = tlibg~area
  WHERE tlibg~area     IN so_area
  AND tadir~devclass IN so_class.

  LOOP AT lt_fg.

    CLEAR gt_lxe.
    gt_lxe-object  = lt_fg-object.
    gt_lxe-objtype = 'RPT1'.
    APPEND gt_lxe.

    ls_prog = 'SAPL' && lt_fg-object.

    CLEAR gt_lxe.
    gt_lxe-object  = lt_fg-object.
    gt_lxe-objtype = 'CA1'.
    APPEND gt_lxe.

    CLEAR: lt_scrn, lt_scrn[].
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_scrn
    FROM d020s
    WHERE prog  = ls_prog
    AND TYPE <> 'S'.

    LOOP AT lt_scrn.

*      화면 제목이므로 빼도 상관 없음
*      CLEAR GT_LXE.
*      GT_LXE-OBJECT    = LT_SCRN-PROG.
*      GT_LXE-OBJECT+40 = LT_SCRN-FNUM.
*      GT_LXE-OBJTYPE   = 'SRH4'.
*      APPEND GT_LXE.

      CLEAR gt_lxe.
      gt_lxe-object    = lt_scrn-prog.
      gt_lxe-object+40 = lt_scrn-dnum.
*      GT_LXE-OBJECT+40 = LT_SCRN-FNUM.
      gt_lxe-objtype   = 'SRT4'.
      APPEND gt_lxe.

    ENDLOOP.
  ENDLOOP.

ENDFORM. " SEL_AREA

*&---------------------------------------------------------------------*
*& 包含               ZWFT_WHERE
*&---------------------------------------------------------------------*

TYPE-POOLS abap.
TYPE-POOLS: rsds.
DATA: is_x030l  TYPE x030l,
      it_x031l  TYPE TABLE OF x031l,
      is_x031l  TYPE x031l,
      w_selid   TYPE rsdynsel-selid,
      it_tables TYPE TABLE OF rsdstabs,
      is_tables TYPE rsdstabs,
      it_fields TYPE TABLE OF rsdsfields,
      it_expr   TYPE rsds_texpr,
      it_ranges TYPE rsds_trange,
      w_active  TYPE I,
      w_repid   TYPE sy-repid,
      w_dynnr   TYPE sy-dynnr,
      wt_dynp   TYPE TABLE OF dynpread,
      ws_dynp   TYPE dynpread,
      w_fdkey   TYPE X VALUE '01'.

"!"// 2022-03-13 厦门 新增全局变量申明
DATA gt_twhere TYPE  rsds_twhere.
DATA gs_where TYPE rsds_where .

FORM f_ss_popup USING iv_tabname TYPE tabname.
  PERFORM f_init_table USING iv_tabname.
  IF w_selid IS INITIAL.
    PERFORM f_init_selections.
  ENDIF.
  PERFORM f_selection_dialog.
ENDFORM.

FORM f_table_def USING in_tabname.
  CALL FUNCTION 'DDIF_NAMETAB_GET'
  EXPORTING
    tabname   = in_tabname
  IMPORTING
    x030l_wa  = is_x030l
  TABLES
    x031l_tab = it_x031l
  EXCEPTIONS
    OTHERS    = 1.
  IF is_x030l IS INITIAL.
    MESSAGE e398(00) WITH 'Table' in_tabname
    'does not exist or is not active'.
ELSEIF is_x030l-tabtype NE 'T'.
    MESSAGE e398(00) WITH 'Table' in_tabname  'is not selectable'.
  ENDIF.
ENDFORM.

FORM f_init_table USING iv_tabname TYPE tabname.
* Prepare free selection on table
*  PERFORM f_table_def USING p_table.
  PERFORM f_table_def USING iv_tabname.
  REFRESH it_tables.
*  is_tables-prim_tab = p_table.
  is_tables-prim_tab = iv_tabname.
  APPEND is_tables TO it_tables.
  CLEAR: w_selid.
ENDFORM.

FORM f_init_selections .
* Init free selection dialog
  CALL FUNCTION 'FREE_SELECTIONS_INIT'
  EXPORTING
    expressions  = it_expr
  IMPORTING
    selection_id = w_selid
    expressions  = it_expr
  TABLES
    tables_tab   = it_tables
    fields_tab   = it_fields
  EXCEPTIONS
    OTHERS       = 1.
ENDFORM.

FORM f_selection_dialog .

  CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
  EXPORTING
    selection_id            = w_selid
    TITLE                   = '请输入选择条件'
    status                  = 1
    as_window               = 'X'
  IMPORTING
    expressions             = it_expr
    field_ranges            = it_ranges
    number_of_active_fields = w_active
  TABLES
    fields_tab              = it_fields
  EXCEPTIONS
    OTHERS                  = 1.

  "// 只要it_ranges不为空，就重新生成gt_twhere和gs_where 2022-03-13 厦门
  IF LINES( it_ranges ) > 0.
    CLEAR: gt_twhere, gs_where.
    TRY.
      CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING
*           field_ranges  = lt_ranges
        field_ranges  = it_ranges
      IMPORTING
        where_clauses = gt_twhere.
    CATCH cx_root INTO DATA(lx_fm_error).
    ENDTRY.
    READ TABLE gt_twhere INTO gs_where INDEX 1.
  ENDIF.

ENDFORM.

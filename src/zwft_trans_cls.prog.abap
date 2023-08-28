*&---------------------------------------------------------------------*
*&  Include           ZLJW_ALV
*&---------------------------------------------------------------------*

DATA: gv_repid LIKE sy-cprog.

*----------------------------------------------------------------------*
* LOCAL CLASSES: Definition
*----------------------------------------------------------------------*
CLASS lcl_event_rec_grid DEFINITION.

  PUBLIC SECTION.

    DATA: g_object_text TYPE char30.

    METHODS: constructor
      IMPORTING e_object_text TYPE c.

    METHODS: handle_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm.

    METHODS: handle_data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified
                et_good_cells.

    METHODS: handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column.

    METHODS: handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id.

    METHODS: print_top_of_page
        FOR EVENT print_top_of_page OF cl_gui_alv_grid.

    METHODS: handle_on_f4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING sender
                e_fieldname
                e_fieldvalue
                es_row_no
                er_event_data
                et_bad_cells
                e_display.

    METHODS: handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive.

    METHODS: handle_before_user_command
      FOR EVENT before_user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS: handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS: handle_after_user_command
      FOR EVENT after_user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm
                e_saved
                e_not_processed.

    METHODS: handle_button_click
      FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id
                es_row_no.

    METHODS: handle_context_menu
      FOR EVENT context_menu_request OF cl_gui_alv_grid
      IMPORTING e_object.

ENDCLASS.  "(LCL_EVENT_REC_GRID DEFINITION)

*----------------------------------------------------------------------*
* LOCAL CLASSES: Implementation
*----------------------------------------------------------------------*
CLASS lcl_event_rec_grid IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor.
    g_object_text = e_object_text.
  ENDMETHOD.                    "constructor

  METHOD handle_data_changed.
    PERFORM alv_data_changed IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm.
  ENDMETHOD.                    "handle_data_changed

  METHOD handle_data_changed_finished.
    PERFORM alv_data_changed_finished IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_modified
          et_good_cells.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED

  METHOD handle_double_click.
    PERFORM alv_double_click IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_row
          e_column.
  ENDMETHOD.    "HANDLE_DOUBLE_CLICK

  METHOD handle_hotspot_click.
    PERFORM alv_double_click IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_row_id
          e_column_id.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD print_top_of_page.
    PERFORM alv_top_of_page IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text.
  ENDMETHOD.                    "print_top_of_page

  METHOD handle_on_f4.
    PERFORM alv_on_f4 IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          sender
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display.
  ENDMETHOD.                                                "on_f4

  METHOD handle_toolbar.
    PERFORM alv_toolbar IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_object
          e_interactive.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_before_user_command.
    PERFORM alv_before_user_command IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_ucomm.
  ENDMETHOD.                    "HANDLE_BEFORE_USER_COMMAND

  METHOD handle_user_command.
    PERFORM alv_user_command IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_ucomm.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD handle_after_user_command.
    PERFORM alv_after_user_command IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_ucomm
          e_saved
          e_not_processed.
  ENDMETHOD.                    "HANDLE_AFTER_USER_COMMAND

  METHOD handle_button_click.
    PERFORM alv_button_click IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          es_col_id
          es_row_no.
  ENDMETHOD.    "HANDLE_BUTTON_CLICK

  METHOD handle_context_menu.
    PERFORM alv_context_menu IN PROGRAM (gv_repid) IF FOUND
    USING g_object_text
          e_object.
  ENDMETHOD.    "HANDLE_CONTEXT_MENU

ENDCLASS. "LCL_EVENT_REC_GRID IMPLEMENTATION

*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: go_gui_cont TYPE REF TO cl_gui_container,
      go_cont     TYPE REF TO cl_gui_custom_container,
      go_cont1    TYPE REF TO cl_gui_custom_container,
      go_cont2    TYPE REF TO cl_gui_custom_container,
      go_dock     TYPE REF TO cl_gui_docking_container,
      go_splt     TYPE REF TO cl_gui_splitter_container,
      go_splt1    TYPE REF TO cl_gui_splitter_container,
      go_spcont   TYPE REF TO cl_gui_container,
      go_spcont0  TYPE REF TO cl_gui_container,
      go_spcont1  TYPE REF TO cl_gui_container,
      go_spcont2  TYPE REF TO cl_gui_container,
      go_document TYPE REF TO cl_dd_document,
      go_grid     TYPE REF TO cl_gui_alv_grid,
      go_grid1    TYPE REF TO cl_gui_alv_grid,
      go_grid2    TYPE REF TO cl_gui_alv_grid,
      go_editor   TYPE REF TO cl_gui_textedit,
      go_evt_grid TYPE REF TO lcl_event_rec_grid,
      gs_layout   TYPE lvc_s_layo,
      gs_layout1  TYPE lvc_s_layo,
      gs_layout2  TYPE lvc_s_layo,
      gt_fcat     TYPE lvc_t_fcat WITH HEADER LINE,
      gt_fcat1    TYPE lvc_t_fcat WITH HEADER LINE,
      gt_fcat2    TYPE lvc_t_fcat WITH HEADER LINE,
      gt_sort     TYPE lvc_t_sort WITH HEADER LINE,
      gt_sort1    TYPE lvc_t_sort WITH HEADER LINE,
      gt_sort2    TYPE lvc_t_sort WITH HEADER LINE,
      gt_alv_f4   TYPE lvc_t_f4   WITH HEADER LINE,
      gs_stable   TYPE lvc_s_stbl,
      gs_variant  TYPE disvariant,
      gt_exclude  TYPE ui_functions WITH HEADER LINE,
      gt_ex_ucomm TYPE TABLE OF sy-ucomm WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  ALV_EX_TOOLBAR
*&---------------------------------------------------------------------*
*  ###### ##ư## ##Ÿ#### ### ##ư# ####Ѵ#.
*----------------------------------------------------------------------*
FORM alv_ex_toolbar USING p_tb_name.

  FIELD-SYMBOLS: <table> TYPE ui_functions.

  DATA: l_tb_name  LIKE feld-name.

  gv_repid = sy-cprog.

  CONCATENATE p_tb_name '[]' INTO  l_tb_name.
  ASSIGN     (l_tb_name)    TO <table>.

  REFRESH: <table>.

  PERFORM alv_exclude_tb_1
  TABLES <table>
*   USING: CL_GUI_ALV_GRID=>mc_fc_excl_all.      "#### #######
  USING:
*      CL_GUI_ALV_GRID=>mc_fc_detail,            "#######
        cl_gui_alv_grid=>mc_fc_refresh,           "Refresh
        cl_gui_alv_grid=>mc_fc_check,

        cl_gui_alv_grid=>mc_fc_loc_cut,           "####Ÿ #߶󳻱#
*      CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          "####Ÿ ####
*      CL_GUI_ALV_GRID=>MC_MB_PASTE
*      CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         "####Ÿ #ٿ##ֱ#
        cl_gui_alv_grid=>mc_fc_loc_paste_new_row, "Paste new Row
        cl_gui_alv_grid=>mc_fc_loc_undo,          "########

*      CL_GUI_ALV_GRID=>MC_FG_EDIT,              "edit###### ##ư# #### ###
*      CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    "## ####
*      CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    "## ####
*      CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    "## ###
*      CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      "## ī##

        cl_gui_alv_grid=>mc_fc_graph,             "#׷###
        cl_gui_alv_grid=>mc_fc_info.              "Info

*      CL_GUI_ALV_GRID=>MC_MB_VIEW,              "
*      CL_GUI_ALV_GRID=>MC_FC_VIEWS,             "####Ʈ####    '&VIEW'
*      CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,      "ũ####Ż####
*      CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,        "########
*      CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,         "Grid####      '&VGRID'
*      CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS.        "

*      cl_gui_alv_grid=>mc_fc_sort,
*      cl_gui_alv_grid=>mc_fc_sort_asc,          "Sort ASC
*      cl_gui_alv_grid=>mc_fc_sort_dsc,          "Sort DESC
*      cl_gui_alv_grid=>mc_fc_find,              "Find
*      cl_gui_alv_grid=>mc_fc_find_more,         "Find Next

*      cl_gui_alv_grid=>MC_MB_FILTER,
*      cl_gui_alv_grid=>mc_fc_filter,            "Set ####
*      cl_gui_alv_grid=>mc_fc_delete_filter,     "Del ####

*      cl_gui_alv_grid=>mc_fc_sum,               "Sum
*      cl_gui_alv_grid=>mc_fc_subtot,            "Sub Sum
*      cl_gui_alv_grid=>mc_fc_minimum,           "Min
*      cl_gui_alv_grid=>mc_fc_maximum,           "Max
*      cl_gui_alv_grid=>MC_FC_AVERAGE,           "####
*      cl_gui_alv_grid=>mc_fc_auf,               "#Ұ#Ȯ##&AUF

*      cl_gui_alv_grid=>mc_fc_print,             "###Ʈ
*      cl_gui_alv_grid=>mc_fc_print_back,
*      cl_gui_alv_grid=>mc_fc_print_prev,

*      cl_gui_alv_grid=>MC_MB_EXPORT,            "
*      cl_gui_alv_grid=>mc_fc_data_save,         "Export
*      cl_gui_alv_grid=>mc_fc_word_processor,    "####
*      cl_gui_alv_grid=>mc_fc_pc_file,           "########
*      cl_gui_alv_grid=>mc_fc_send,              "Send
*      cl_gui_alv_grid=>mc_fc_to_office,         "Office
*      cl_gui_alv_grid=>mc_fc_call_abc,          "ABC#м#
*      cl_gui_alv_grid=>mc_fc_html,              "HTML

*      cl_gui_alv_grid=>mc_fc_load_variant,      "#ҷ####
*      cl_gui_alv_grid=>mc_fc_current_variant,   "####
*      cl_gui_alv_grid=>mc_fc_save_variant,      "####
*      cl_gui_alv_grid=>mc_fc_maintain_variant,  "Vari####

*      cl_gui_alv_grid=>mc_fc_col_optimize,      "Optimize
*      cl_gui_alv_grid=>mc_fc_separator,         "#и###
*      cl_gui_alv_grid=>mc_fc_select_all,        "##ü####
*      cl_gui_alv_grid=>mc_fc_deselect_all,      "##ü###
*      cl_gui_alv_grid=>mc_fc_col_invisible,     "Į########
*      cl_gui_alv_grid=>mc_fc_fix_columns,       "Į#####
*      cl_gui_alv_grid=>mc_fc_unfix_columns,     "Į#####Ǯ##
*      cl_gui_alv_grid=>mc_fc_average,           "Average         '&AVERAGE'

*      cl_gui_alv_grid=>mc_fc_f4,
*      cl_gui_alv_grid=>mc_fc_help,
*      CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,          "               '&ABC'
*      cl_gui_alv_grid=>mc_fc_call_chain,
*      cl_gui_alv_grid=>mc_fc_back_classic,
*      cl_gui_alv_grid=>mc_fc_call_crbatch,
*      cl_gui_alv_grid=>mc_fc_call_crweb,
*      cl_gui_alv_grid=>mc_fc_call_lineitems,
*      cl_gui_alv_grid=>mc_fc_call_master_data,
*      cl_gui_alv_grid=>mc_fc_call_more,
*      CL_GUI_ALV_GRID=>MC_FC_CALL_REPORT,
*      cl_gui_alv_grid=>mc_fc_call_xint,
*      cl_gui_alv_grid=>mc_fc_call_xxl,
*      cl_gui_alv_grid=>mc_fc_expcrdata,
*      cl_gui_alv_grid=>mc_fc_expcrdesig,
*      cl_gui_alv_grid=>mc_fc_expcrtempl,
*      cl_gui_alv_grid=>mc_fc_expmdb,
*      cl_gui_alv_grid=>mc_fc_extend,
*      cl_gui_alv_grid=>mc_fc_loc_move_row,

*      cl_gui_alv_grid=>mc_fc_reprep,
*      CL_GUI_ALV_GRID=>MC_FC_TO_REP_TREE.

ENDFORM.                    " ALV_EX_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  ALV_EX_TOOLBAR_NO
*&---------------------------------------------------------------------*
*  ###### ##ư## ##Ÿ#### ### ##ư# ####Ѵ#.
*----------------------------------------------------------------------*
FORM alv_ex_toolbar_no USING p_tb_name.

  FIELD-SYMBOLS: <table> TYPE ui_functions.

  DATA: l_tb_name  LIKE feld-name.

  gv_repid = sy-cprog.

  CONCATENATE p_tb_name '[]' INTO  l_tb_name.
  ASSIGN     (l_tb_name)    TO <table>.

  REFRESH: <table>.

  PERFORM alv_exclude_tb_1
  TABLES <table>
  USING  cl_gui_alv_grid=>mc_fc_excl_all.      "#### #######

ENDFORM.                    " ALV_EX_TOOLBAR_NO

*&---------------------------------------------------------------------*
*&      Form  ALV_EXCLUDE_TB_1
*&---------------------------------------------------------------------*
FORM alv_exclude_tb_1  TABLES   pt_ex TYPE ui_functions
USING    pv_value.

  DATA: ls_exclude TYPE ui_func.

  ls_exclude = pv_value.
  APPEND ls_exclude TO pt_ex.

ENDFORM.                    " ALV_EXCLUDE_TB_1

*&---------------------------------------------------------------------*
*&      Form  ALV_EXEC_TOOLBAR_FCODE
*&---------------------------------------------------------------------*
*  #### #޴### ######Ų##
*&---------------------------------------------------------------------*
FORM alv_exec_toolbar_fcode USING po_grid TYPE REF TO cl_gui_alv_grid
      p_lvc_fcode.

  DATA: l_slis_fcode LIKE sy-ucomm.

*  P_LVC_FCODE = '&PRINT_BACK_PREVIEW'.   " '&RNT'  '%SC' '&PRINT_BACK_PREVIEW' '&VIEW'

  CALL METHOD cl_gui_alv_grid=>transfer_fcode_lvc_to_slis
    EXPORTING
*     IT_FCODES_LVC  =
      i_fcode_lvc    = p_lvc_fcode
    IMPORTING
*     ET_FCODES_SLIS =
      e_fcode_slis   = l_slis_fcode
    EXCEPTIONS
      no_match_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD po_grid->set_function_code
    CHANGING
      c_ucomm = l_slis_fcode.


ENDFORM.                    " ALV_EXEC_TOOLBAR_FCODE

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_MERGE
*&---------------------------------------------------------------------*
FORM alv_fieldcat_merge TABLES pt_m     TYPE table
  pt_fcat  TYPE lvc_t_fcat
USING  p_it_name.

  CLEAR: pt_fcat, pt_fcat[].

  DATA: lt_fcat     TYPE slis_t_fieldcat_alv WITH HEADER LINE.
  DATA: lv_it_name  TYPE slis_tabname.

  gv_repid   = sy-cprog.
  lv_it_name = p_it_name.

* IT ##### ###### ###
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = gv_repid
      i_internal_tabname = lv_it_name
      i_inclname         = gv_repid
*     I_BYPASSING_BUFFER = ' '
    CHANGING
      ct_fieldcat        = lt_fcat[].

* ALV Control##### ##ȯ
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      it_fieldcat_alv = lt_fcat[]
    IMPORTING
      et_fieldcat_lvc = pt_fcat[]
    TABLES
      it_data         = pt_m.

ENDFORM.                    " ALV_FIELDCAT_MERGE

*&---------------------------------------------------------------------*
*&      Form  ALV_FCAT_MERGE_LVC
*&---------------------------------------------------------------------*
FORM alv_fcat_merge_lvc TABLES pt_fieldcat TYPE lvc_t_fcat
USING  p_str_name.

  gv_repid = sy-cprog.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = p_str_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     I_INTERNAL_TABNAME     =
    CHANGING
      ct_fieldcat            = pt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno DISPLAY LIKE sy-msgty
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_FCAT_MERGE_LVC

*&---------------------------------------------------------------------*
*&      Form  ALV_GET_CURSOR
*&---------------------------------------------------------------------*
FORM alv_get_cursor USING    po_grid TYPE REF TO cl_gui_alv_grid
CHANGING p_row_id    "LVC_S_ROW-INDEX
  p_col_nm.   "LVC_S_COL-FIELDNAME

  DATA: l_row        TYPE i,
        l_value(255),
        l_col        TYPE i,
        ls_row       TYPE lvc_s_row,
        ls_col       TYPE lvc_s_col,
        ls_row_no    TYPE lvc_s_roid.

  CLEAR: p_row_id, p_col_nm.

  CALL METHOD po_grid->get_current_cell
    IMPORTING
      e_row     = l_row
      e_value   = l_value
      e_col     = l_col
      es_row_id = ls_row
      es_col_id = ls_col
      es_row_no = ls_row_no.

  IF ls_row-rowtype IS INITIAL.
    p_row_id = ls_row-index.
  ENDIF.

  p_col_nm = ls_col-fieldname.

ENDFORM.                    " ALV_GET_CURSOR

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT_INIT
*&---------------------------------------------------------------------*
FORM alv_layout_init USING    p_edit    "####### ###뿩##
      p_color   "######## ###뿩##
CHANGING ps_layout TYPE lvc_s_layo.

  CLEAR ps_layout.

*  ps_layout-detailinit = 'X'.        "Detail#### NULL#̶### ######
  ps_layout-sel_mode   = 'D'.        "CELL #### ####
*  ps_layout-no_rowins  = 'X'.
*  ps_layout-no_rowmove = 'X'.
*  ps_layout-smalltitle = 'X'.
*  ps_layout-frontend   = 'X'.        "ALV ###: #### #Ǵ# ũ###### #Ǵ# ALV

  IF p_edit = 'X'.
    ps_layout-stylefname = 'CELLTAB'.  "Input/Output ####
  ENDIF.

  IF p_color = 'X'.
    ps_layout-ctab_fname = 'CELLCOL'.  "Color ####
  ENDIF.

  ps_layout-info_fname = 'ROW_COLOR'.  "###### ####

  gs_variant-report    = sy-cprog.     "Default Variant Set

*  PS_LAYOUT-EDIT       = 'X'.         "##ü ########ϰ#
*  PS_LAYOUT-ZEBRA      = 'X'.         "#Ѷ##ξ# ###ϰ#,###### ####, SORT#ϸ# #### #ʴ°# ###.
*  PS_LAYOUT-NO_TOOLBAR = 'X'.
*  PS_LAYOUT-GRID_TITLE = 'GRID ###'.
*  PS_LAYOUT-NO_ROWMARK = 'X'.        "MARK ####(########̸# #۵#####)
*  PS_LAYOUT-SEL_MODE   = 'A'.        "CELL #### ####(########̸# #۵#####)
*  PS_LAYOUT-SEL_MODE   = 'B'.        "LINE #### ####(########̸# #۵#####, NO_ROWMARK = ''#̾#####)
*  PS_LAYOUT-SEL_MODE   = 'C'.        "LINE #### ####(Multi #### ###ð###)
*  PS_LAYOUT-SEL_MODE   = 'D'.        "CELL #### ####

ENDFORM.                    " ALV_LAYOUT_INIT

*&---------------------------------------------------------------------*
*&      Form  ALV_REFRESH
*&---------------------------------------------------------------------*
*  P_GB = ' ' : REFRESH ## ####
*         'F' : Fieldcat ###̰# ###濡 #### ####
*         'S' : Sort, SUMMARY ## #### REFRESH ####
*         'A' : Fieldcat #### + Sort, SUMMARY ####
*----------------------------------------------------------------------*
FORM alv_refresh USING po_grid TYPE REF TO cl_gui_alv_grid
      pt_fcat TYPE lvc_t_fcat
      p_gb.

  IF po_grid IS INITIAL. EXIT. ENDIF.

  DATA: ls_stbl        TYPE lvc_s_stbl,
        l_soft_refresh.

* Fieldcat ###̰# #####ǰų# #Ͽ## ####
  CASE p_gb.
    WHEN 'F' OR 'A'.
      CALL METHOD po_grid->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = pt_fcat[].
  ENDCASE.

* Sort, SUMMARY#### REFRESH
  CASE p_gb.
    WHEN 'S' OR 'A'.
      l_soft_refresh = ''.
    WHEN OTHERS.
      l_soft_refresh = 'X'.
  ENDCASE.

  ls_stbl-row = 'X'.
  ls_stbl-col = 'X'.

  CALL METHOD po_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stbl
      i_soft_refresh = l_soft_refresh.

ENDFORM.                    " ALV_REFRESH

*&---------------------------------------------------------------------*
*&      Form  ALV_MODI_CELL
*&---------------------------------------------------------------------*
FORM alv_modi_cell USING pr_data_changed  TYPE REF TO cl_alv_changed_data_protocol
      p_row
      p_fieldname
      p_value.

  CALL METHOD pr_data_changed->modify_cell
    EXPORTING
      i_row_id    = p_row
      i_fieldname = p_fieldname
      i_value     = p_value.

ENDFORM.                    " ALV_MODI_CELL

*&---------------------------------------------------------------------*
*&      Form  ALV_RELOAD
*&---------------------------------------------------------------------*
FORM alv_reload.

  IF go_grid    IS NOT INITIAL. CALL METHOD go_grid->free.    ENDIF.
*  IF GO_GRID1   IS NOT INITIAL. CALL METHOD GO_GRID1->FREE.   ENDIF.
*  IF GO_GRID2   IS NOT INITIAL. CALL METHOD GO_GRID2->FREE.   ENDIF.

*  IF GO_DOCUMENT IS NOT INITIAL. CALL METHOD GO_DOCUMENT->INITIALIZE_DOCUMENT. ENDIF.

  IF go_spcont  IS NOT INITIAL. CALL METHOD go_spcont->free.  ENDIF.
*  IF GO_SPCONT1 IS NOT INITIAL. CALL METHOD GO_SPCONT1->FREE. ENDIF.
*  IF GO_SPCONT2 IS NOT INITIAL. CALL METHOD GO_SPCONT2->FREE. ENDIF.
*  IF GO_SPLT    IS NOT INITIAL. CALL METHOD GO_SPLT->FREE.    ENDIF.
*  IF GO_SPLT1   IS NOT INITIAL. CALL METHOD GO_SPLT1->FREE.   ENDIF.

  IF go_dock    IS NOT INITIAL. CALL METHOD go_dock->free.    ENDIF.
  IF go_cont    IS NOT INITIAL. CALL METHOD go_cont->free.    ENDIF.
*  IF GO_CONT1   IS NOT INITIAL. CALL METHOD GO_CONT1->FREE.   ENDIF.
*  IF GO_CONT2   IS NOT INITIAL. CALL METHOD GO_CONT2->FREE.   ENDIF.

  CLEAR: go_dock,
  go_cont,    "GO_CONT1,   GO_CONT2,   GO_CONT3,   GO_CONT4,
  go_splt,                                           "GO_SPLT1,
  go_spcont,  "GO_SPCONT1, GO_SPCONT2, GO_SPCONT3, GO_SPCONT4,
  "GO_SPCONT5, GO_SPCONT6, GO_SPCONT7, GO_SPCONT8,
  go_grid.    "GO_GRID1,   GO_GRID2,   GO_GRID3,   GO_GRID4,
  "GO_GRID5,   GO_GRID6,   GO_GRID7,   GO_GRID8,
  "GO_DOCUMENT.

* CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " ALV_RELOAD

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_CURSOR
*&---------------------------------------------------------------------*
FORM alv_set_cursor USING po_grid TYPE REF TO cl_gui_alv_grid
      p_rowid
      p_colnm.

  DATA: ls_row TYPE lvc_s_row,
        ls_col TYPE lvc_s_col.

  ls_row-index     = p_rowid.
  ls_col-fieldname = p_colnm.

*  CALL METHOD PO_GRID->SET_SCROLL_INFO_VIA_ID
*    EXPORTING
*      IS_ROW_INFO = LS_ROW
*      IS_COL_INFO = LS_COL.

  CALL METHOD po_grid->set_current_cell_via_id
    EXPORTING
      is_row_id    = ls_row
      is_column_id = ls_col.

  CALL METHOD cl_gui_alv_grid=>set_focus
    EXPORTING
      control = po_grid.

ENDFORM.                    " ALV_SET_CURSOR

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_DDLB
*&---------------------------------------------------------------------*
FORM alv_set_ddlb USING po_grid TYPE REF TO cl_gui_alv_grid
*                        PT_DROP TYPE        LVC_T_DROP.
      pt_dral TYPE        lvc_t_dral.

  CALL METHOD po_grid->set_drop_down_table
    EXPORTING
*     IT_DROP_DOWN       = PT_DROP[].
      it_drop_down_alias = pt_dral[].

ENDFORM.                    " ALV_SET_DDLB

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_MARK
*&---------------------------------------------------------------------*
*  ###õ# ###### MARK #ʵ带 'X' ó###Ѵ#. SY-TABIX## ###õ# #### #Ǽ#
*----------------------------------------------------------------------*
FORM alv_set_mark USING po_grid TYPE REF TO cl_gui_alv_grid
      pt_itab TYPE STANDARD TABLE.

  FIELD-SYMBOLS: <fs_wa> TYPE any,
                 <fs>.

  DATA: lt_row  TYPE lvc_t_row WITH HEADER LINE,
        l_tabix TYPE sy-tabix.

  CALL METHOD po_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_row[].

  LOOP AT pt_itab ASSIGNING <fs_wa>.
    ASSIGN COMPONENT 'MARK' OF STRUCTURE <fs_wa> TO <fs>.
    IF <fs> = ' '. CONTINUE. ENDIF.
    <fs> = ' '.
    MODIFY pt_itab FROM <fs_wa>.
  ENDLOOP.

  LOOP AT lt_row WHERE rowtype = ' '.
    READ TABLE pt_itab ASSIGNING <fs_wa> INDEX lt_row-index.
    IF sy-subrc <> 0. CONTINUE. ENDIF.
    ASSIGN COMPONENT 'MARK' OF STRUCTURE <fs_wa> TO <fs>.
    <fs> = 'X'.
    MODIFY pt_itab FROM <fs_wa> INDEX lt_row-index.
    l_tabix = l_tabix + 1.
  ENDLOOP.

  sy-tabix = l_tabix.

ENDFORM.                    " ALV_SET_MARK

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_ROW_COLOR
*&---------------------------------------------------------------------*
*  ####Ŭ#### ### ROW_COLOR = 'C500'. ó###Ѵ#. & REFRESH
*----------------------------------------------------------------------*
FORM alv_set_row_color USING    po_grid TYPE REF TO cl_gui_alv_grid
      pt_fcat TYPE        lvc_t_fcat
      p_tabix
CHANGING ct_m    TYPE STANDARD TABLE.

  DATA: l_fname(30).

  FIELD-SYMBOLS: <lfs_wa> TYPE any,
                 <lfs>.

  l_fname = 'ROW_COLOR'.

  READ TABLE ct_m ASSIGNING <lfs_wa> WITH KEY (l_fname) = 'C500'.
  IF sy-subrc = 0.
    ASSIGN COMPONENT l_fname OF STRUCTURE <lfs_wa> TO <lfs>.
    <lfs> = ''.
    MODIFY ct_m FROM <lfs_wa> INDEX sy-tabix TRANSPORTING (l_fname).
  ENDIF.

  READ TABLE ct_m ASSIGNING <lfs_wa> INDEX p_tabix.
  IF sy-subrc = 0.
    ASSIGN COMPONENT l_fname OF STRUCTURE <lfs_wa> TO <lfs>.
    <lfs> = 'C500'.
    MODIFY ct_m FROM <lfs_wa> INDEX sy-tabix TRANSPORTING (l_fname).
  ENDIF.

  PERFORM alv_refresh USING po_grid pt_fcat ''.

ENDFORM.                    " ALV_SET_ROW_COLOR

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_NEW_OK_CODE
*&---------------------------------------------------------------------*
*  ALV #̺#Ʈ #Ŀ# ȭ## PAI ## Ÿ## #Ѵ#.
*----------------------------------------------------------------------*
FORM alv_set_new_ok_code USING p_new_okcd.

  CALL METHOD cl_gui_cfw=>set_new_ok_code
    EXPORTING
      new_code = p_new_okcd.

ENDFORM.                    " ALV_SET_NEW_OK_CODE

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_SEL_ROW
*&---------------------------------------------------------------------*
FORM alv_set_sel_row USING po_grid TYPE REF TO cl_gui_alv_grid
      p_row.

  DATA: lt_row TYPE lvc_t_row WITH HEADER LINE.

  lt_row-index = p_row.
  APPEND lt_row.

  CALL METHOD po_grid->set_selected_rows
    EXPORTING
      it_index_rows = lt_row[].
*      IT_ROW_NO                =
*      IS_KEEP_OTHER_SELECTIONS = 'X'.

ENDFORM.                    " ALV_SET_SEL_ROW

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_SEL_ROW_ALL
*&---------------------------------------------------------------------*
FORM alv_set_sel_row_all USING po_grid TYPE REF TO cl_gui_alv_grid
      pt_m    TYPE STANDARD TABLE
      p_sel_all.

  DATA: lt_row TYPE lvc_t_row WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_wa> TYPE any.

  IF p_sel_all = 'X'.
    LOOP AT pt_m ASSIGNING <fs_wa>.
      lt_row-index = sy-tabix.
      APPEND lt_row.
    ENDLOOP.
  ENDIF.

  CALL METHOD po_grid->set_selected_rows
    EXPORTING
      it_index_rows = lt_row[].
*      IT_ROW_NO                =
*      IS_KEEP_OTHER_SELECTIONS = 'X'.

ENDFORM.                    " ALV_SET_SEL_ROW_ALL

*&---------------------------------------------------------------------*
*&      Form  ALV_TOOLBAR_SET_CNT
*&---------------------------------------------------------------------*
FORM alv_toolbar_set_cnt USING e_object TYPE REF TO cl_alv_event_toolbar_set
      po_grid  TYPE REF TO cl_gui_alv_grid
      pt_it    TYPE STANDARD TABLE.

  DATA: ls_toolbar    TYPE stb_button,
        lt_fidx       TYPE lvc_t_fidx,
        l_tot_cnt(10) VALUE '0',
        l_suc_cnt(10) VALUE '0',
        l_wrn_cnt(10) VALUE '0',
        l_err_cnt(10) VALUE '0',
        l_fil_cnt(10) VALUE '0'.

* #Ǽ#
  DESCRIBE TABLE pt_it LINES l_tot_cnt.  "#ѰǼ#

* ###͸# #Ǽ#
  CALL METHOD po_grid->get_filtered_entries
    IMPORTING
      et_filtered_entries = lt_fidx.
  DESCRIBE TABLE lt_fidx LINES l_fil_cnt.

* #### #Ǽ# = #ѰǼ# - ###͸# #Ǽ#
  l_fil_cnt = l_tot_cnt - l_fil_cnt.

  CONDENSE: l_tot_cnt NO-GAPS,
  l_suc_cnt NO-GAPS,
  l_wrn_cnt NO-GAPS,
  l_err_cnt NO-GAPS,
  l_fil_cnt NO-GAPS.

* ##ư #߰#
  CLEAR ls_toolbar.
  ls_toolbar-function  = '&&SEP90'.
  ls_toolbar-butn_type = '3'.  "#и###
  APPEND ls_toolbar TO e_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = 'TOT_CNT'.
  CONCATENATE 'TOTAL:' l_tot_cnt INTO ls_toolbar-text SEPARATED BY ''.
  APPEND ls_toolbar TO e_object->mt_toolbar.

  IF lt_fidx[] IS NOT INITIAL.
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'CUR_CNT'.
    CONCATENATE 'CURRENT:' l_fil_cnt INTO ls_toolbar-text SEPARATED BY ''.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDIF.

ENDFORM.                    " ALV_TOOLBAR_SET_CNT

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_CMD_FILTER
*&---------------------------------------------------------------------*
FORM alv_user_cmd_filter USING po_grid TYPE REF TO cl_gui_alv_grid
      pt_fcat TYPE lvc_t_fcat
      p_fname
      p_val.         "EQ_INITIAL, NE_INITIAL

  DATA: lt_filter TYPE lvc_t_filt WITH HEADER LINE.

  CALL METHOD po_grid->get_filter_criteria
    IMPORTING
      et_filter = lt_filter[].

  IF p_fname IS INITIAL.
    CLEAR lt_filter[].
  ELSE.
    DELETE lt_filter WHERE fieldname = p_fname.
  ENDIF.

  CASE p_val.
    WHEN 'EQ_INITIAL'.
      CLEAR: lt_filter.
      lt_filter-fieldname = p_fname.
      lt_filter-sign      = 'I'.
      lt_filter-option    = 'EQ'.
      APPEND lt_filter.
    WHEN 'NE_INITIAL'.
      CLEAR: lt_filter.
      lt_filter-fieldname = p_fname.
      lt_filter-sign      = 'I'.
      lt_filter-option    = 'NE'.
      APPEND lt_filter.
    WHEN OTHERS.
      CLEAR: lt_filter.
      lt_filter-fieldname = p_fname.
      lt_filter-sign      = 'I'.
      lt_filter-option    = 'EQ'.
      lt_filter-low       = p_val.
      APPEND lt_filter.
  ENDCASE.

  CALL METHOD po_grid->set_filter_criteria
    EXPORTING
      it_filter                 = lt_filter[]
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM alv_refresh USING po_grid pt_fcat[] 'A'.

ENDFORM.                    " ALV_USER_CMD_FILTER


*----------------------------------------------------------------------*
* #Ʒ### FORM ### #### #ش# ##α׷### #####ؼ# ####
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_0100
*&---------------------------------------------------------------------*
*FORM ALV_FIELDCAT_0100 TABLES PT_FCAT STRUCTURE LVC_S_FCAT.
*
*  LOOP AT PT_FCAT.
*
*    PT_FCAT-COL_OPT = 'X'.
*    PT_FCAT-KEY     = ' '.
*    PT_FCAT-NO_OUT  = ' '.
*
*    CASE PT_FCAT-FIELDNAME.
*      WHEN 'RCOMP'.
*        PT_FCAT-COL_POS     = '10'.
*        PT_FCAT-COLTEXT     = 'S'.
*      WHEN OTHERS.
*        PT_FCAT-NO_OUT      = 'X'.
*    ENDCASE.
*
*    MODIFY PT_FCAT.
*
*  ENDLOOP.
*
*ENDFORM.                    " ALV_FIELDCAT_0100

*&---------------------------------------------------------------------*
*&      Form  ALV_DATA_CHANGED
*&---------------------------------------------------------------------*
*FORM ALV_DATA_CHANGED USING P_GRID_NM
*                            ER_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
*                            E_ONF4
*                            E_ONF4_BEFORE
*                            E_ONF4_AFTER
*                            E_UCOMM.
*
*  IF E_ONF4 = 'X' AND E_ONF4_BEFORE = 'X' AND E_ONF4_AFTER = ''. EXIT. ENDIF.
*
*ENDFORM.                    " ALV_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  ALV_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*FORM ALV_DATA_CHANGED_FINISHED USING P_GRID_NAME
*                                     E_MODIFIED    TYPE CHAR01
*                                     ET_GOOD_CELLS TYPE LVC_T_MODI.
*
*  DATA: LS_MODI TYPE LVC_S_MODI.
*
*  IF E_MODIFIED = ''. EXIT. ENDIF.
*
*  LOOP AT ET_GOOD_CELLS INTO LS_MODI.
*
*    READ TABLE GT_A INDEX LS_MODI-ROW_ID.
*    IF SY-SUBRC <> 0.
*      MESSAGE E899(MM) WITH 'IT READ ERROR'.
*    ENDIF.
*
*    CASE LS_MODI-FIELDNAME.
*
*      WHEN 'RCOMP'.
*        PERFORM SEL_T880_TXT USING    GT_A-RCOMP
*                             CHANGING GT_A-RCOMP_NM
*                                      GT_A-HWAER.
*
*    ENDCASE.
*
*    MODIFY GT_A INDEX LS_MODI-ROW_ID.
*
*  ENDLOOP.
*
*  G_MODI_GB = 'X'.
*
*  PERFORM ALV_REFRESH USING GO_GRID GT_FIELDCAT[] 'F'.
*
*ENDFORM.                    " ALV_DATA_CHANGED_FINISHED

*&---------------------------------------------------------------------*
*&      Form  ALV_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*FORM ALV_DOUBLE_CLICK USING P_GRID_NM
*                            P_ROW TYPE LVC_S_ROW
*                            P_COL TYPE LVC_S_COL.
*
** ȭ## PAI ## Ÿ#### ## ####
*  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
*    EXPORTING
*      NEW_CODE = 'ALV_DBL_CLK'.

*ENDFORM.                    " ALV_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  ALV_F4
*&---------------------------------------------------------------------*
*FORM ALV_F4  USING P_GRID_NM
*                   PO_sender      TYPE REF TO cl_gui_alv_grid
*                   P_fieldname    TYPE lvc_fname
*                   P_fieldvalue   TYPE lvc_value
*                   PS_row_no      TYPE lvc_s_roid
*                   PO_event_data  TYPE REF TO cl_alv_event_data
*                   PT_bad_cells   TYPE lvc_t_modi
*                   P_display      TYPE char01.
*
*ENDFORM.                    " ALV_F4

*&---------------------------------------------------------------------*
*&      Form  ALV_TOOLBAR
*&---------------------------------------------------------------------*
*FORM ALV_TOOLBAR USING P_GRID_NM
*                       E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
*                       E_INTERACTIVE.
*
*ENDFORM.                    " ALV_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  ALV_BEFORE_USER_COMMAND
*&---------------------------------------------------------------------*
*FORM ALV_BEFORE_USER_COMMAND USING P_GRID_NM
*                                   E_UCOMM LIKE SY-UCOMM.
*
*ENDFORM.                    " ALV_BEFORE_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*FORM ALV_USER_COMMAND USING P_GRID_NM
*                            E_UCOMM LIKE SY-UCOMM.
*
*ENDFORM.                    " ALV_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  ALV_AFTER_USER_COMMAND
*&---------------------------------------------------------------------*
*FORM ALV_AFTER_USER_COMMAND USING P_GRID_NM
*                                  E_UCOMM LIKE sy-ucomm
*                                  E_SAVED
*                                  E_NOT_PROCESSED.
*
*ENDFORM.                    " ALV_AFTER_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  ALV_BUTTON_CLICK
*&---------------------------------------------------------------------*
*FORM ALV_BUTTON_CLICK USING P_GRID_NAME
*                            PS_COL_ID TYPE LVC_S_COL
*                            PS_ROW_NO TYPE LVC_S_ROID.
*
*ENDFORM.                    " ALV_BUTTON_CLICK

*&---------------------------------------------------------------------*
*&      Form  ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*FORM ALV_TOP_OF_PAGE USING P_GRID_NAME.
*
*  WRITE: /,
*         / SY-TITLE CENTERED,
*         /.
*
*ENDFORM.                    " ALV_TOP_OF_PAGE

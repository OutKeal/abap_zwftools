*&---------------------------------------------------------------------*
*&  Include           ZLJW_ALV
*&---------------------------------------------------------------------*

DATA: GV_REPID LIKE SY-CPROG.

*----------------------------------------------------------------------*
* LOCAL CLASSES: Definition
*----------------------------------------------------------------------*
CLASS LCL_EVENT_REC_GRID DEFINITION.

  PUBLIC SECTION.

  DATA: G_OBJECT_TEXT TYPE CHAR30.

  METHODS: CONSTRUCTOR
  IMPORTING E_OBJECT_TEXT TYPE C.

  METHODS: HANDLE_DATA_CHANGED
  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
  IMPORTING ER_DATA_CHANGED
    E_ONF4
    E_ONF4_BEFORE
    E_ONF4_AFTER
    E_UCOMM.

  METHODS: HANDLE_DATA_CHANGED_FINISHED
  FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
  IMPORTING E_MODIFIED
    ET_GOOD_CELLS.

  METHODS: HANDLE_DOUBLE_CLICK
  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
  IMPORTING E_ROW
    E_COLUMN.

  METHODS: HANDLE_HOTSPOT_CLICK
  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
  IMPORTING E_ROW_ID
    E_COLUMN_ID.

  METHODS: PRINT_TOP_OF_PAGE
  FOR EVENT PRINT_TOP_OF_PAGE OF CL_GUI_ALV_GRID.

  METHODS: HANDLE_ON_F4
  FOR EVENT ONF4 OF CL_GUI_ALV_GRID
  IMPORTING SENDER
    E_FIELDNAME
    E_FIELDVALUE
    ES_ROW_NO
    ER_EVENT_DATA
    ET_BAD_CELLS
    E_DISPLAY.

  METHODS: HANDLE_TOOLBAR
  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
  IMPORTING E_OBJECT
    E_INTERACTIVE.

  METHODS: HANDLE_BEFORE_USER_COMMAND
  FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
  IMPORTING E_UCOMM.

  METHODS: HANDLE_USER_COMMAND
  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
  IMPORTING E_UCOMM.

  METHODS: HANDLE_AFTER_USER_COMMAND
  FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
  IMPORTING E_UCOMM
    E_SAVED
    E_NOT_PROCESSED.

  METHODS: HANDLE_BUTTON_CLICK
  FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
  IMPORTING ES_COL_ID
    ES_ROW_NO.

  METHODS: HANDLE_CONTEXT_MENU
  FOR EVENT CONTEXT_MENU_REQUEST OF CL_GUI_ALV_GRID
  IMPORTING E_OBJECT.

ENDCLASS.  "(LCL_EVENT_REC_GRID DEFINITION)

*----------------------------------------------------------------------*
* LOCAL CLASSES: Implementation
*----------------------------------------------------------------------*
CLASS LCL_EVENT_REC_GRID IMPLEMENTATION.

  METHOD CONSTRUCTOR.
    CALL METHOD SUPER->CONSTRUCTOR.
    G_OBJECT_TEXT = E_OBJECT_TEXT.
  ENDMETHOD.                    "constructor

  METHOD HANDLE_DATA_CHANGED.
    PERFORM ALV_DATA_CHANGED IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          ER_DATA_CHANGED
          E_ONF4
          E_ONF4_BEFORE
          E_ONF4_AFTER
          E_UCOMM.
  ENDMETHOD.                    "handle_data_changed

  METHOD HANDLE_DATA_CHANGED_FINISHED.
    PERFORM ALV_DATA_CHANGED_FINISHED IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_MODIFIED
          ET_GOOD_CELLS.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED

  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM ALV_DOUBLE_CLICK IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_ROW
          E_COLUMN.
  ENDMETHOD.    "HANDLE_DOUBLE_CLICK

  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM ALV_DOUBLE_CLICK IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_ROW_ID
          E_COLUMN_ID.
  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

  METHOD PRINT_TOP_OF_PAGE.
    PERFORM ALV_TOP_OF_PAGE IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT.
  ENDMETHOD.                    "print_top_of_page

  METHOD HANDLE_ON_F4.
    PERFORM ALV_ON_F4 IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          SENDER
          E_FIELDNAME
          E_FIELDVALUE
          ES_ROW_NO
          ER_EVENT_DATA
          ET_BAD_CELLS
          E_DISPLAY.
  ENDMETHOD.                                                "on_f4

  METHOD HANDLE_TOOLBAR.
    PERFORM ALV_TOOLBAR IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_OBJECT
          E_INTERACTIVE.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_BEFORE_USER_COMMAND.
    PERFORM ALV_BEFORE_USER_COMMAND IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_UCOMM.
  ENDMETHOD.                    "HANDLE_BEFORE_USER_COMMAND

  METHOD HANDLE_USER_COMMAND.
    PERFORM ALV_USER_COMMAND IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_UCOMM.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD HANDLE_AFTER_USER_COMMAND.
    PERFORM ALV_AFTER_USER_COMMAND IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_UCOMM
          E_SAVED
          E_NOT_PROCESSED.
  ENDMETHOD.                    "HANDLE_AFTER_USER_COMMAND

  METHOD HANDLE_BUTTON_CLICK.
    PERFORM ALV_BUTTON_CLICK IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          ES_COL_ID
          ES_ROW_NO.
  ENDMETHOD.    "HANDLE_BUTTON_CLICK

  METHOD HANDLE_CONTEXT_MENU.
    PERFORM ALV_CONTEXT_MENU IN PROGRAM (GV_REPID) IF FOUND
    USING G_OBJECT_TEXT
          E_OBJECT.
  ENDMETHOD.    "HANDLE_CONTEXT_MENU

ENDCLASS. "LCL_EVENT_REC_GRID IMPLEMENTATION

*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: GO_GUI_CONT      TYPE REF TO CL_GUI_CONTAINER,
      GO_CONT          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GO_CONT1         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GO_CONT2         TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GO_DOCK          TYPE REF TO CL_GUI_DOCKING_CONTAINER,
      GO_SPLT          TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GO_SPLT1         TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      GO_SPCONT        TYPE REF TO CL_GUI_CONTAINER,
      GO_SPCONT0       TYPE REF TO CL_GUI_CONTAINER,
      GO_SPCONT1       TYPE REF TO CL_GUI_CONTAINER,
      GO_SPCONT2       TYPE REF TO CL_GUI_CONTAINER,
      GO_DOCUMENT      TYPE REF TO CL_DD_DOCUMENT,
      GO_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GO_GRID1         TYPE REF TO CL_GUI_ALV_GRID,
      GO_GRID2         TYPE REF TO CL_GUI_ALV_GRID,
      GO_EDITOR        TYPE REF TO CL_GUI_TEXTEDIT,
      GO_EVT_GRID      TYPE REF TO LCL_EVENT_REC_GRID,
      GS_LAYOUT        TYPE LVC_S_LAYO,
      GS_LAYOUT1       TYPE LVC_S_LAYO,
      GS_LAYOUT2       TYPE LVC_S_LAYO,
      GT_FCAT          TYPE LVC_T_FCAT WITH HEADER LINE,
      GT_FCAT1         TYPE LVC_T_FCAT WITH HEADER LINE,
      GT_FCAT2         TYPE LVC_T_FCAT WITH HEADER LINE,
      GT_SORT          TYPE LVC_T_SORT WITH HEADER LINE,
      GT_SORT1         TYPE LVC_T_SORT WITH HEADER LINE,
      GT_SORT2         TYPE LVC_T_SORT WITH HEADER LINE,
      GT_ALV_F4        TYPE LVC_T_F4   WITH HEADER LINE,
      GS_STABLE        TYPE LVC_S_STBL,
      GS_VARIANT       TYPE DISVARIANT,
      GT_EXCLUDE       TYPE UI_FUNCTIONS WITH HEADER LINE,
      GT_EX_UCOMM      TYPE TABLE OF SY-UCOMM WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  ALV_EX_TOOLBAR
*&---------------------------------------------------------------------*
*  ###### ##ư## ##Ÿ#### ### ##ư# ####Ѵ#.
*----------------------------------------------------------------------*
FORM ALV_EX_TOOLBAR USING P_TB_NAME.

  FIELD-SYMBOLS: <TABLE> TYPE UI_FUNCTIONS.

  DATA: L_TB_NAME  LIKE FELD-NAME.

  GV_REPID = SY-CPROG.

  CONCATENATE P_TB_NAME '[]' INTO  L_TB_NAME.
  ASSIGN     (L_TB_NAME)    TO <TABLE>.

  REFRESH: <TABLE>.

  PERFORM ALV_EXCLUDE_TB_1
  TABLES <TABLE>
*   USING: CL_GUI_ALV_GRID=>mc_fc_excl_all.      "#### #######
  USING:
*      CL_GUI_ALV_GRID=>mc_fc_detail,            "#######
        CL_GUI_ALV_GRID=>MC_FC_REFRESH,           "Refresh
        CL_GUI_ALV_GRID=>MC_FC_CHECK,

        CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,           "####Ÿ #߶󳻱#
*      CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,          "####Ÿ ####
*      CL_GUI_ALV_GRID=>MC_MB_PASTE
*      CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,         "####Ÿ #ٿ##ֱ#
        CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW, "Paste new Row
        CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,          "########

*      CL_GUI_ALV_GRID=>MC_FG_EDIT,              "edit###### ##ư# #### ###
*      CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,    "## ####
*      CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,    "## ####
*      CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,    "## ###
*      CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,      "## ī##

        CL_GUI_ALV_GRID=>MC_FC_GRAPH,             "#׷###
        CL_GUI_ALV_GRID=>MC_FC_INFO.              "Info

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
FORM ALV_EX_TOOLBAR_NO USING P_TB_NAME.

  FIELD-SYMBOLS: <TABLE> TYPE UI_FUNCTIONS.

  DATA: L_TB_NAME  LIKE FELD-NAME.

  GV_REPID = SY-CPROG.

  CONCATENATE P_TB_NAME '[]' INTO  L_TB_NAME.
  ASSIGN     (L_TB_NAME)    TO <TABLE>.

  REFRESH: <TABLE>.

  PERFORM ALV_EXCLUDE_TB_1
  TABLES <TABLE>
  USING  CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.      "#### #######

ENDFORM.                    " ALV_EX_TOOLBAR_NO

*&---------------------------------------------------------------------*
*&      Form  ALV_EXCLUDE_TB_1
*&---------------------------------------------------------------------*
FORM ALV_EXCLUDE_TB_1  TABLES   PT_EX TYPE UI_FUNCTIONS
USING    PV_VALUE.

  DATA: LS_EXCLUDE TYPE UI_FUNC.

  LS_EXCLUDE = PV_VALUE.
  APPEND LS_EXCLUDE TO PT_EX.

ENDFORM.                    " ALV_EXCLUDE_TB_1

*&---------------------------------------------------------------------*
*&      Form  ALV_EXEC_TOOLBAR_FCODE
*&---------------------------------------------------------------------*
*  #### #޴### ######Ų##
*&---------------------------------------------------------------------*
FORM ALV_EXEC_TOOLBAR_FCODE USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      P_LVC_FCODE.

  DATA: L_SLIS_FCODE LIKE SY-UCOMM.

*  P_LVC_FCODE = '&PRINT_BACK_PREVIEW'.   " '&RNT'  '%SC' '&PRINT_BACK_PREVIEW' '&VIEW'

  CALL METHOD CL_GUI_ALV_GRID=>TRANSFER_FCODE_LVC_TO_SLIS
  EXPORTING
*      IT_FCODES_LVC  =
    I_FCODE_LVC    = P_LVC_FCODE
  IMPORTING
*      ET_FCODES_SLIS =
    E_FCODE_SLIS   = L_SLIS_FCODE
  EXCEPTIONS
    NO_MATCH_FOUND = 1
    OTHERS         = 2          .

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD PO_GRID->SET_FUNCTION_CODE
  CHANGING
    C_UCOMM = L_SLIS_FCODE.


ENDFORM.                    " ALV_EXEC_TOOLBAR_FCODE

*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_MERGE
*&---------------------------------------------------------------------*
FORM ALV_FIELDCAT_MERGE TABLES PT_M     TYPE TABLE
  PT_FCAT  TYPE LVC_T_FCAT
USING  P_IT_NAME.

  CLEAR: PT_FCAT, PT_FCAT[].

  DATA: LT_FCAT     TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.
  DATA: LV_IT_NAME  TYPE SLIS_TABNAME.

  GV_REPID   = SY-CPROG.
  LV_IT_NAME = P_IT_NAME.

* IT ##### ###### ###
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    I_PROGRAM_NAME     = GV_REPID
    I_INTERNAL_TABNAME = LV_IT_NAME
    I_INCLNAME         = GV_REPID
*     I_BYPASSING_BUFFER = ' '
  CHANGING
    CT_FIELDCAT        = LT_FCAT[].

* ALV Control##### ##ȯ
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
  EXPORTING
    IT_FIELDCAT_ALV = LT_FCAT[]
  IMPORTING
    ET_FIELDCAT_LVC = PT_FCAT[]
  TABLES
    IT_DATA         = PT_M.

ENDFORM.                    " ALV_FIELDCAT_MERGE

*&---------------------------------------------------------------------*
*&      Form  ALV_FCAT_MERGE_LVC
*&---------------------------------------------------------------------*
FORM ALV_FCAT_MERGE_LVC TABLES PT_FIELDCAT TYPE LVC_T_FCAT
USING  P_STR_NAME.

  GV_REPID = SY-CPROG.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
  EXPORTING
*     I_BUFFER_ACTIVE              =
    I_STRUCTURE_NAME             = P_STR_NAME
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
*     I_INTERNAL_TABNAME           =
  CHANGING
    CT_FIELDCAT                  = PT_FIELDCAT[]
  EXCEPTIONS
    INCONSISTENT_INTERFACE       = 1
    PROGRAM_ERROR                = 2
    OTHERS                       = 3            .

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO DISPLAY LIKE SY-MSGTY
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ALV_FCAT_MERGE_LVC

*&---------------------------------------------------------------------*
*&      Form  ALV_GET_CURSOR
*&---------------------------------------------------------------------*
FORM ALV_GET_CURSOR USING    PO_GRID TYPE REF TO CL_GUI_ALV_GRID
CHANGING P_ROW_ID    "LVC_S_ROW-INDEX
  P_COL_NM.   "LVC_S_COL-FIELDNAME

  DATA: L_ROW TYPE I,
        L_VALUE(255),
        L_COL TYPE I,
        LS_ROW TYPE LVC_S_ROW,
        LS_COL TYPE LVC_S_COL,
        LS_ROW_NO TYPE LVC_S_ROID.

  CLEAR: P_ROW_ID, P_COL_NM.

  CALL METHOD PO_GRID->GET_CURRENT_CELL
  IMPORTING
    E_ROW     = L_ROW
    E_VALUE   = L_VALUE
    E_COL     = L_COL
    ES_ROW_ID = LS_ROW
    ES_COL_ID = LS_COL
    ES_ROW_NO = LS_ROW_NO.

  IF LS_ROW-ROWTYPE IS INITIAL.
    P_ROW_ID = LS_ROW-INDEX.
  ENDIF.

  P_COL_NM = LS_COL-FIELDNAME.

ENDFORM.                    " ALV_GET_CURSOR

*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT_INIT
*&---------------------------------------------------------------------*
FORM ALV_LAYOUT_INIT USING    P_EDIT    "####### ###뿩##
      P_COLOR   "######## ###뿩##
CHANGING PS_LAYOUT TYPE LVC_S_LAYO.

  CLEAR PS_LAYOUT.

  PS_LAYOUT-DETAILINIT = 'X'.        "Detail#### NULL#̶### ######
  PS_LAYOUT-SEL_MODE   = 'D'.        "CELL #### ####
  PS_LAYOUT-NO_ROWINS  = 'X'.
  PS_LAYOUT-NO_ROWMOVE = 'X'.
  PS_LAYOUT-SMALLTITLE = 'X'.
  PS_LAYOUT-FRONTEND   = 'X'.        "ALV ###: #### #Ǵ# ũ###### #Ǵ# ALV

  IF P_EDIT = 'X'.
    PS_LAYOUT-STYLEFNAME = 'CELLTAB'.  "Input/Output ####
  ENDIF.

  IF P_COLOR = 'X'.
    PS_LAYOUT-CTAB_FNAME = 'CELLCOL'.  "Color ####
  ENDIF.

  PS_LAYOUT-INFO_FNAME = 'ROW_COLOR'.  "###### ####

  GS_VARIANT-REPORT    = SY-CPROG.     "Default Variant Set

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
FORM ALV_REFRESH USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      PT_FCAT TYPE LVC_T_FCAT
      P_GB.

  IF PO_GRID IS INITIAL. EXIT. ENDIF.

  DATA: LS_STBL TYPE LVC_S_STBL,
        L_SOFT_REFRESH.

* Fieldcat ###̰# #####ǰų# #Ͽ## ####
  CASE P_GB.
  WHEN 'F' OR 'A'.
    CALL METHOD PO_GRID->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = PT_FCAT[].
  ENDCASE.

* Sort, SUMMARY#### REFRESH
  CASE P_GB.
  WHEN 'S' OR 'A'.
    L_SOFT_REFRESH = ''.
  WHEN OTHERS.
    L_SOFT_REFRESH = 'X'.
  ENDCASE.

  LS_STBL-ROW = 'X'.
  LS_STBL-COL = 'X'.

  CALL METHOD PO_GRID->REFRESH_TABLE_DISPLAY
  EXPORTING
    IS_STABLE      = LS_STBL
    I_SOFT_REFRESH = L_SOFT_REFRESH.

ENDFORM.                    " ALV_REFRESH

*&---------------------------------------------------------------------*
*&      Form  ALV_MODI_CELL
*&---------------------------------------------------------------------*
FORM ALV_MODI_CELL USING PR_DATA_CHANGED  TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
      P_ROW
      P_FIELDNAME
      P_VALUE.

  CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
  EXPORTING
    I_ROW_ID    = P_ROW
    I_FIELDNAME = P_FIELDNAME
    I_VALUE     = P_VALUE.

ENDFORM.                    " ALV_MODI_CELL

*&---------------------------------------------------------------------*
*&      Form  ALV_RELOAD
*&---------------------------------------------------------------------*
FORM ALV_RELOAD.

  IF GO_GRID    IS NOT INITIAL. CALL METHOD GO_GRID->FREE.    ENDIF.
*  IF GO_GRID1   IS NOT INITIAL. CALL METHOD GO_GRID1->FREE.   ENDIF.
*  IF GO_GRID2   IS NOT INITIAL. CALL METHOD GO_GRID2->FREE.   ENDIF.

*  IF GO_DOCUMENT IS NOT INITIAL. CALL METHOD GO_DOCUMENT->INITIALIZE_DOCUMENT. ENDIF.

  IF GO_SPCONT  IS NOT INITIAL. CALL METHOD GO_SPCONT->FREE.  ENDIF.
*  IF GO_SPCONT1 IS NOT INITIAL. CALL METHOD GO_SPCONT1->FREE. ENDIF.
*  IF GO_SPCONT2 IS NOT INITIAL. CALL METHOD GO_SPCONT2->FREE. ENDIF.
*  IF GO_SPLT    IS NOT INITIAL. CALL METHOD GO_SPLT->FREE.    ENDIF.
*  IF GO_SPLT1   IS NOT INITIAL. CALL METHOD GO_SPLT1->FREE.   ENDIF.

  IF GO_DOCK    IS NOT INITIAL. CALL METHOD GO_DOCK->FREE.    ENDIF.
  IF GO_CONT    IS NOT INITIAL. CALL METHOD GO_CONT->FREE.    ENDIF.
*  IF GO_CONT1   IS NOT INITIAL. CALL METHOD GO_CONT1->FREE.   ENDIF.
*  IF GO_CONT2   IS NOT INITIAL. CALL METHOD GO_CONT2->FREE.   ENDIF.

  CLEAR: GO_DOCK,
  GO_CONT,    "GO_CONT1,   GO_CONT2,   GO_CONT3,   GO_CONT4,
  GO_SPLT,                                           "GO_SPLT1,
  GO_SPCONT,  "GO_SPCONT1, GO_SPCONT2, GO_SPCONT3, GO_SPCONT4,
  "GO_SPCONT5, GO_SPCONT6, GO_SPCONT7, GO_SPCONT8,
  GO_GRID.    "GO_GRID1,   GO_GRID2,   GO_GRID3,   GO_GRID4,
  "GO_GRID5,   GO_GRID6,   GO_GRID7,   GO_GRID8,
  "GO_DOCUMENT.

* CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " ALV_RELOAD

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_CURSOR
*&---------------------------------------------------------------------*
FORM ALV_SET_CURSOR USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      P_ROWID
      P_COLNM.

  DATA: LS_ROW TYPE LVC_S_ROW,
        LS_COL TYPE LVC_S_COL.

  LS_ROW-INDEX     = P_ROWID.
  LS_COL-FIELDNAME = P_COLNM.

*  CALL METHOD PO_GRID->SET_SCROLL_INFO_VIA_ID
*    EXPORTING
*      IS_ROW_INFO = LS_ROW
*      IS_COL_INFO = LS_COL.

  CALL METHOD PO_GRID->SET_CURRENT_CELL_VIA_ID
  EXPORTING
    IS_ROW_ID    = LS_ROW
    IS_COLUMN_ID = LS_COL.

  CALL METHOD CL_GUI_ALV_GRID=>SET_FOCUS
  EXPORTING
    CONTROL = PO_GRID.

ENDFORM.                    " ALV_SET_CURSOR

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_DDLB
*&---------------------------------------------------------------------*
FORM ALV_SET_DDLB USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
*                        PT_DROP TYPE        LVC_T_DROP.
      PT_DRAL TYPE        LVC_T_DRAL.

  CALL METHOD PO_GRID->SET_DROP_DOWN_TABLE
  EXPORTING
*      IT_DROP_DOWN =       PT_DROP[].
    IT_DROP_DOWN_ALIAS = PT_DRAL[].

ENDFORM.                    " ALV_SET_DDLB

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_MARK
*&---------------------------------------------------------------------*
*  ###õ# ###### MARK #ʵ带 'X' ó###Ѵ#. SY-TABIX## ###õ# #### #Ǽ#
*----------------------------------------------------------------------*
FORM ALV_SET_MARK USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      PT_ITAB TYPE STANDARD TABLE.

  FIELD-SYMBOLS: <FS_WA> TYPE ANY,
  <FS>.

  DATA: LT_ROW  TYPE LVC_T_ROW WITH HEADER LINE,
        L_TABIX TYPE SY-TABIX.

  CALL METHOD PO_GRID->GET_SELECTED_ROWS
  IMPORTING
    ET_INDEX_ROWS = LT_ROW[].

  LOOP AT PT_ITAB ASSIGNING <FS_WA>.
    ASSIGN COMPONENT 'MARK' OF STRUCTURE <FS_WA> TO <FS>.
    IF <FS> = ' '. CONTINUE. ENDIF.
    <FS> = ' '.
    MODIFY PT_ITAB FROM <FS_WA>.
  ENDLOOP.

  LOOP AT LT_ROW WHERE ROWTYPE = ' '.
    READ TABLE PT_ITAB ASSIGNING <FS_WA> INDEX LT_ROW-INDEX.
    IF SY-SUBRC <> 0. CONTINUE. ENDIF.
    ASSIGN COMPONENT 'MARK' OF STRUCTURE <FS_WA> TO <FS>.
    <FS> = 'X'.
    MODIFY PT_ITAB FROM <FS_WA> INDEX LT_ROW-INDEX.
    L_TABIX = L_TABIX + 1.
  ENDLOOP.

  SY-TABIX = L_TABIX.

ENDFORM.                    " ALV_SET_MARK

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_ROW_COLOR
*&---------------------------------------------------------------------*
*  ####Ŭ#### ### ROW_COLOR = 'C500'. ó###Ѵ#. & REFRESH
*----------------------------------------------------------------------*
FORM ALV_SET_ROW_COLOR USING    PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      PT_FCAT TYPE        LVC_T_FCAT
      P_TABIX
CHANGING CT_M    TYPE STANDARD TABLE.

  DATA: L_FNAME(30).

  FIELD-SYMBOLS: <LFS_WA> TYPE ANY,
  <LFS>.

  L_FNAME = 'ROW_COLOR'.

  READ TABLE CT_M ASSIGNING <LFS_WA> WITH KEY (L_FNAME) = 'C500'.
  IF SY-SUBRC = 0.
    ASSIGN COMPONENT L_FNAME OF STRUCTURE <LFS_WA> TO <LFS>.
    <LFS> = ''.
    MODIFY CT_M FROM <LFS_WA> INDEX SY-TABIX TRANSPORTING (L_FNAME).
  ENDIF.

  READ TABLE CT_M ASSIGNING <LFS_WA> INDEX P_TABIX.
  IF SY-SUBRC = 0.
    ASSIGN COMPONENT L_FNAME OF STRUCTURE <LFS_WA> TO <LFS>.
    <LFS> = 'C500'.
    MODIFY CT_M FROM <LFS_WA> INDEX SY-TABIX TRANSPORTING (L_FNAME).
  ENDIF.

  PERFORM ALV_REFRESH USING PO_GRID PT_FCAT ''.

ENDFORM.                    " ALV_SET_ROW_COLOR

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_NEW_OK_CODE
*&---------------------------------------------------------------------*
*  ALV #̺#Ʈ #Ŀ# ȭ## PAI ## Ÿ## #Ѵ#.
*----------------------------------------------------------------------*
FORM ALV_SET_NEW_OK_CODE USING P_NEW_OKCD.

  CALL METHOD CL_GUI_CFW=>SET_NEW_OK_CODE
  EXPORTING
    NEW_CODE = P_NEW_OKCD.

ENDFORM.                    " ALV_SET_NEW_OK_CODE

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_SEL_ROW
*&---------------------------------------------------------------------*
FORM ALV_SET_SEL_ROW USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      P_ROW.

  DATA: LT_ROW TYPE LVC_T_ROW WITH HEADER LINE.

  LT_ROW-INDEX = P_ROW.
  APPEND LT_ROW.

  CALL METHOD PO_GRID->SET_SELECTED_ROWS
  EXPORTING
    IT_INDEX_ROWS = LT_ROW[].
*      IT_ROW_NO                =
*      IS_KEEP_OTHER_SELECTIONS = 'X'.

ENDFORM.                    " ALV_SET_SEL_ROW

*&---------------------------------------------------------------------*
*&      Form  ALV_SET_SEL_ROW_ALL
*&---------------------------------------------------------------------*
FORM ALV_SET_SEL_ROW_ALL USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      PT_M    TYPE STANDARD TABLE
      P_SEL_ALL.

  DATA: LT_ROW TYPE LVC_T_ROW WITH HEADER LINE.

  FIELD-SYMBOLS: <FS_WA> TYPE ANY.

  IF P_SEL_ALL = 'X'.
    LOOP AT PT_M ASSIGNING <FS_WA>.
      LT_ROW-INDEX = SY-TABIX.
      APPEND LT_ROW.
    ENDLOOP.
  ENDIF.

  CALL METHOD PO_GRID->SET_SELECTED_ROWS
  EXPORTING
    IT_INDEX_ROWS = LT_ROW[].
*      IT_ROW_NO                =
*      IS_KEEP_OTHER_SELECTIONS = 'X'.

ENDFORM.                    " ALV_SET_SEL_ROW_ALL

*&---------------------------------------------------------------------*
*&      Form  ALV_TOOLBAR_SET_CNT
*&---------------------------------------------------------------------*
FORM ALV_TOOLBAR_SET_CNT USING E_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
      PO_GRID  TYPE REF TO CL_GUI_ALV_GRID
      PT_IT    TYPE STANDARD TABLE.

  DATA: LS_TOOLBAR TYPE STB_BUTTON,
        LT_FIDX    TYPE LVC_T_FIDX,
        L_TOT_CNT(10) VALUE '0',
        L_SUC_CNT(10) VALUE '0',
        L_WRN_CNT(10) VALUE '0',
        L_ERR_CNT(10) VALUE '0',
        L_FIL_CNT(10) VALUE '0'.

* #Ǽ#
  DESCRIBE TABLE PT_IT LINES L_TOT_CNT.  "#ѰǼ#

* ###͸# #Ǽ#
  CALL METHOD PO_GRID->GET_FILTERED_ENTRIES
  IMPORTING
    ET_FILTERED_ENTRIES = LT_FIDX.
  DESCRIBE TABLE LT_FIDX LINES L_FIL_CNT.

* #### #Ǽ# = #ѰǼ# - ###͸# #Ǽ#
  L_FIL_CNT = L_TOT_CNT - L_FIL_CNT.

  CONDENSE: L_TOT_CNT NO-GAPS,
  L_SUC_CNT NO-GAPS,
  L_WRN_CNT NO-GAPS,
  L_ERR_CNT NO-GAPS,
  L_FIL_CNT NO-GAPS.

* ##ư #߰#
  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-FUNCTION  = '&&SEP90'.
  LS_TOOLBAR-BUTN_TYPE = '3'.  "#и###
  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  CLEAR LS_TOOLBAR.
  LS_TOOLBAR-FUNCTION  = 'TOT_CNT'.
  CONCATENATE 'TOTAL:' L_TOT_CNT INTO LS_TOOLBAR-TEXT SEPARATED BY ''.
  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  IF LT_FIDX[] IS NOT INITIAL.
    CLEAR LS_TOOLBAR.
    LS_TOOLBAR-FUNCTION  = 'CUR_CNT'.
    CONCATENATE 'CURRENT:' L_FIL_CNT INTO LS_TOOLBAR-TEXT SEPARATED BY ''.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
  ENDIF.

ENDFORM.                    " ALV_TOOLBAR_SET_CNT

*&---------------------------------------------------------------------*
*&      Form  ALV_USER_CMD_FILTER
*&---------------------------------------------------------------------*
FORM ALV_USER_CMD_FILTER USING PO_GRID TYPE REF TO CL_GUI_ALV_GRID
      PT_FCAT TYPE LVC_T_FCAT
      P_FNAME
      P_VAL.         "EQ_INITIAL, NE_INITIAL

  DATA: LT_FILTER TYPE LVC_T_FILT WITH HEADER LINE.

  CALL METHOD PO_GRID->GET_FILTER_CRITERIA
  IMPORTING
    ET_FILTER = LT_FILTER[].

  IF P_FNAME IS INITIAL.
    CLEAR LT_FILTER[].
  ELSE.
    DELETE LT_FILTER WHERE FIELDNAME = P_FNAME.
  ENDIF.

  CASE P_VAL.
  WHEN 'EQ_INITIAL'.
    CLEAR: LT_FILTER.
    LT_FILTER-FIELDNAME = P_FNAME.
    LT_FILTER-SIGN      = 'I'.
    LT_FILTER-OPTION    = 'EQ'.
    APPEND LT_FILTER.
  WHEN 'NE_INITIAL'.
    CLEAR: LT_FILTER.
    LT_FILTER-FIELDNAME = P_FNAME.
    LT_FILTER-SIGN      = 'I'.
    LT_FILTER-OPTION    = 'NE'.
    APPEND LT_FILTER.
  WHEN OTHERS.
    CLEAR: LT_FILTER.
    LT_FILTER-FIELDNAME = P_FNAME.
    LT_FILTER-SIGN      = 'I'.
    LT_FILTER-OPTION    = 'EQ'.
    LT_FILTER-LOW       = P_VAL.
    APPEND LT_FILTER.
  ENDCASE.

  CALL METHOD PO_GRID->SET_FILTER_CRITERIA
  EXPORTING
    IT_FILTER                 = LT_FILTER[]
  EXCEPTIONS
    NO_FIELDCATALOG_AVAILABLE = 1
    OTHERS                    = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM ALV_REFRESH USING PO_GRID PT_FCAT[] 'A'.

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

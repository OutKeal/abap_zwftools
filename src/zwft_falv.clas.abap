class ZWFT_FALV definition
  public
  inheriting from CL_GUI_ALV_GRID
  create public

  global friends ZWFT_FALV_LAYOUT .

public section.

  interfaces IF_ALV_RM_GRID_FRIEND .

  types:
    BEGIN OF t_subcl_call,
    progname TYPE progname,
    LINE     TYPE I,
    column   TYPE I,
    CLASS    TYPE string,
  END OF t_subcl_call .
  types:
    tt_subcl_call TYPE SORTED TABLE OF t_subcl_call WITH UNIQUE KEY progname LINE column .
  types:
    BEGIN OF t_email,
    smtp_addr  TYPE ad_smtpadr,
    express    TYPE os_boolean,
    COPY       TYPE os_boolean,
    blind_copy TYPE os_boolean,
  END OF t_email .
  types:
    tt_email TYPE TABLE OF t_email .
  types T_COLUMN type ref to ZWFT_FALV_COLUMN .
  types:
    t_columns TYPE SORTED TABLE OF t_column WITH UNIQUE KEY table_line .

  constants:
    BEGIN OF COLOR,
    blue                         TYPE char4 VALUE 'C100',
    blue_intensified             TYPE char4 VALUE 'C110',
    blue_intensified_inversed    TYPE char4 VALUE 'C111',
    blue_inversed                TYPE char4 VALUE 'C101',
    gray                         TYPE char4 VALUE 'C200',
    gray_itensified              TYPE char4 VALUE 'C210',
    gray_intesified_invers       TYPE char4 VALUE 'C211',
    gray_inversed                TYPE char4 VALUE 'C201',
    yellow                       TYPE char4 VALUE 'C300',
    yellow_intensified           TYPE char4 VALUE 'C310',
    yellow_intensified_inversed  TYPE char4 VALUE 'C311',
    yellow_inversed              TYPE char4 VALUE 'C301',
    light_blue                   TYPE char4 VALUE 'C400',
    light_blue_itensified        TYPE char4 VALUE 'C410',
    light_blue_intesified_invers TYPE char4 VALUE 'C411',
    light_blue_inversed          TYPE char4 VALUE 'C401',
    green                        TYPE char4 VALUE 'C500',
    green_intensified            TYPE char4 VALUE 'C510',
    green_intensified_inversed   TYPE char4 VALUE 'C511',
    green_inversed               TYPE char4 VALUE 'C501',
    red                          TYPE char4 VALUE 'C600',
    red_intensified              TYPE char4 VALUE 'C610',
    red_intensified_inversed     TYPE char4 VALUE 'C611',
    red_inversed                 TYPE char4 VALUE 'C601',
    orange                       TYPE char4 VALUE 'C700',
    orange_intensified           TYPE char4 VALUE 'C710',
    orange_intensified_inversed  TYPE char4 VALUE 'C711',
    orange_inversed              TYPE char4 VALUE 'C701',
  END OF COLOR .
  constants VERSION type STRING value '740.1.0.19' ##NO_TEXT.
  constants CC_NAME type CHAR30 value 'CC_GRID' ##NO_TEXT.
  constants C_SCREEN_POPUP type SY-DYNNR value '0200' ##NO_TEXT.
  constants C_SCREEN_FULL type SY-DYNNR value '0100' ##NO_TEXT.
  constants C_FSCR_REPID type SY-REPID value 'SAPLZWFT_FALV' ##NO_TEXT.
  data RESULT type I read-only .
  data SPLITTER_ROW_1_HEIGHT type I read-only .
  data SPLITTER_ROW_2_HEIGHT type I read-only .
  data SPLITTER_ROW_3_HEIGHT type I read-only .
  constants FC_BACK type SY-UCOMM value 'BACK' ##NO_TEXT.
  constants FC_UP type SY-UCOMM value 'UP' ##NO_TEXT.
  constants FC_EXIT type SY-UCOMM value 'EXIT' ##NO_TEXT.
  constants FC_CANCEL type SY-UCOMM value 'CANCEL' ##NO_TEXT.
  constants FC_MASS_REPLACE type SY-UCOMM value 'MASS_REPL' ##NO_TEXT.
  constants FC_SAVE type SY-UCOMM value '&DATA_SAVE' ##NO_TEXT.
  constants FC_PRINT type SY-UCOMM value 'PRINT' ##NO_TEXT.
  constants FC_FIND type SY-UCOMM value 'FIND' ##NO_TEXT.
  constants FC_FIND_NEXT type SY-UCOMM value 'FINDNEXT' ##NO_TEXT.
  constants FC_FIRST_PAGE type SY-UCOMM value 'PGHOME' ##NO_TEXT.
  constants FC_LAST_PAGE type SY-UCOMM value 'PGEND' ##NO_TEXT.
  constants FC_PREVIOUS_PAGE type SY-UCOMM value 'PGUP' ##NO_TEXT.
  constants FC_NEXT_PAGE type SY-UCOMM value 'PGDOWN' ##NO_TEXT.
  constants BUTTON_NORMAL type TB_BTYPE value '0' ##NO_TEXT.
  constants BUTTON_MENU_DEFAULT type TB_BTYPE value '1' ##NO_TEXT.
  constants BUTTON_MENU type TB_BTYPE value '2' ##NO_TEXT.
  constants BUTTON_SEPARATOR type TB_BTYPE value '3' ##NO_TEXT.
  constants BUTTON_RADIOBUTTON type TB_BTYPE value '4' ##NO_TEXT.
  constants BUTTON_CHECKBOX type TB_BTYPE value '5' ##NO_TEXT.
  constants BUTTON_MENU_ENTRY type TB_BTYPE value '6' ##NO_TEXT.
  constants:
    BEGIN OF SYMBOL,
    empty_space        TYPE char01 VALUE ' ',
    plus_box           TYPE char01 VALUE '!',
    minus_box          TYPE char01 VALUE '"',
    plus_circle        TYPE char01 VALUE '#',
    minus_circle       TYPE char01 VALUE '$',
    filled_square      TYPE char01 VALUE '%',
    half_filled_square TYPE char01 VALUE '&',
    square             TYPE char01 VALUE `'`,
    filled_circle      TYPE char01 VALUE '(',
    half_filled_circle TYPE char01 VALUE ')',
    circle             TYPE char01 VALUE '*',
    filled_diamond     TYPE char01 VALUE '+',
    diamond            TYPE char01 VALUE ',',
    bold_x             TYPE char01 VALUE '.',
    note               TYPE char01 VALUE '/',
    document           TYPE char01 VALUE '0',
    checked_document   TYPE char01 VALUE '1',
    documents          TYPE char01 VALUE '2',
    folder             TYPE char01 VALUE '3',
    plus_folder        TYPE char01 VALUE '4',
    minus_folder       TYPE char01 VALUE '5',
    open_folder        TYPE char01 VALUE '6',
    bold_minus         TYPE char01 VALUE '7',
    bold_plus          TYPE char01 VALUE '8',
    CHECKBOX           TYPE char01 VALUE '9',
    RADIOBUTTON        TYPE char01 VALUE  ':',
    left_triangle      TYPE char01 VALUE  ';',
    right_triangle     TYPE char01 VALUE  '<',
    up_triangle        TYPE char01 VALUE  '=',
    down_triangle      TYPE char01 VALUE  '>',
    left_hand          TYPE char01 VALUE  '?',
    left_arrow         TYPE char01 VALUE  'A',
    right_arrow        TYPE char01 VALUE  'B',
    up_arrow           TYPE char01 VALUE  'C',
    down_arrow         TYPE char01 VALUE  'D',
    check_mark         TYPE char01 VALUE  'E',
    pencil             TYPE char01 VALUE  'F',
    glasses            TYPE char01 VALUE  'G',
    locked             TYPE char01 VALUE  'H',
    unlocked           TYPE char01 VALUE  'I',
    phone              TYPE char01 VALUE  'J',
    printer            TYPE char01 VALUE  'K',
    fax                TYPE char01 VALUE  'L',
    asterisk           TYPE char01 VALUE  'M',
    right_hand         TYPE char01 VALUE  'N',
    sorted_up          TYPE char01 VALUE  'O',
    sorted_down        TYPE char01 VALUE  'P',
    cumulated          TYPE char01 VALUE  'Q',
    DELETE             TYPE char01 VALUE  'R',
    executable         TYPE char01 VALUE  'S',
    workflow_item      TYPE char01 VALUE  'T',
    caution            TYPE char01 VALUE  'U',
    flash              TYPE char01 VALUE  'V',
    large_square       TYPE char01 VALUE  'W',
    ellipsis           TYPE char01 VALUE  'X',
  END OF SYMBOL .
  data MAIN_CONTAINER type ref to CL_GUI_CONTAINER read-only .
  data SPLIT_CONTAINER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MAIN_SPLIT_CONTAINER type ref to CL_GUI_SPLITTER_CONTAINER .
  data TOP_OF_PAGE_CONTAINER type ref to CL_GUI_CONTAINER .
  data VARIANT type DISVARIANT .
  data LAYOUT_SAVE type CHAR01 .
  data EXCLUDE_FUNCTIONS type UI_FUNCTIONS .
  data FCAT type LVC_T_FCAT .
  data SORT type LVC_T_SORT .
  data FILTER type LVC_T_FILT .
  data LVC_LAYOUT type LVC_S_LAYO .
  data LAYOUT type ref to ZWFT_FALV_LAYOUT .
  data GUI_STATUS type ref to ZWFT_FALV_DYNAMIC_STATUS .
  data SCREEN type SY-DYNNR read-only .
  data OUTTAB type ref to DATA .
  data TITLE_V1 type STRING .
  data TITLE_V2 type STRING .
  data TITLE_V3 type STRING .
  data TITLE_V4 type STRING .
  data TITLE_V5 type STRING .
  data DELAY_MOVE_CURRENT_CELL type I read-only value 20 ##NO_TEXT.
  data DELAY_CHANGE_SELECTION type I read-only value 20 ##NO_TEXT.
  data TOP_OF_PAGE_HEIGHT type I value 150 ##NO_TEXT.
  data ERROR_LOG_HEIGHT type I value 100 ##NO_TEXT.
  data GRID type ref to CL_GUI_ALV_GRID .
  data BUILT_IN_SCREEN type ABAP_BOOL read-only .
  data BUFFERING_ACTIVE type ABAP_BOOL value ABAP_TRUE ##NO_TEXT.
  data BYPASSING_BUFFER type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants:
* add by yu.xiaosan 20230525 events callback form begin
    BEGIN OF EVENTS,
    after_refresh              TYPE slis_formname VALUE 'AFTER_REFRESH',
    after_user_command         TYPE slis_formname VALUE 'AFTER_USER_COMMAND',
    before_ucommand_internal   TYPE slis_formname VALUE 'BEFORE_UCOMMAND_INTERNAL',
    before_user_command        TYPE slis_formname VALUE 'BEFORE_USER_COMMAND',
    btn_click                  TYPE slis_formname VALUE 'BTN_CLICK',
    data_changed               TYPE slis_formname VALUE 'DATA_CHANGED',
    data_changed_internal      TYPE slis_formname VALUE 'DATA_CHANGED_INTERNAL',
    data_changed_finished      TYPE slis_formname VALUE 'DATA_CHANGED_FINISHED',
    double_click               TYPE slis_formname VALUE 'DOUBLE_CLICK',
    hotspot_click              TYPE slis_formname VALUE 'HOTSPOT_CLICK',
    menu_button                TYPE slis_formname VALUE 'MENU_BUTTON',
    onf1                       TYPE slis_formname VALUE 'ONF1',
    onf4                       TYPE slis_formname VALUE 'ONF4',
    subtotal_text              TYPE slis_formname VALUE 'SUBTOTAL_TEXT',
    toolbar_internal           TYPE slis_formname VALUE 'TOOLBAR_INTERNAL',
    toolbar                    TYPE slis_formname VALUE 'TOOLBAR',
    user_command               TYPE slis_formname VALUE 'USER_COMMAND',
    at_set_pf_status           TYPE slis_formname VALUE 'AT_SET_PF_STATUS',
    at_set_title               TYPE slis_formname VALUE 'AT_SET_TITLE',
    top_of_page                TYPE slis_formname VALUE 'TOP_OF_PAGE',
    delayed_callback           TYPE slis_formname VALUE 'DELAYED_CALLBACK',
    delayed_changed_sel_call   TYPE slis_formname VALUE 'DELAYED_CHANGED_SEL_CALL',
    ondrag                     TYPE slis_formname VALUE 'ONDRAG',
    ondrop                     TYPE slis_formname VALUE 'ONDROP',
    ondropcomplete             TYPE slis_formname VALUE 'ONDROPCOMPLETE',
    ondropgetflavor            TYPE slis_formname VALUE 'ONDROPGETFLAVOR',
    drop_external_file         TYPE slis_formname VALUE 'DROP_EXTERNAL_FILE',
    toolbar_menubutton_click   TYPE slis_formname VALUE 'TOOLBAR_MENUBUTTON_CLICK',
    click_col_header           TYPE slis_formname VALUE 'CLICK_COL_HEADER',
    delayed_move_current_cell  TYPE slis_formname VALUE 'DELAYED_MOVE_CURRENT_CELL',
    f1                         TYPE slis_formname VALUE 'F1',
    dblclick_row_col           TYPE slis_formname VALUE 'DBLCLICK_ROW_COL',
    click_row_col              TYPE slis_formname VALUE 'CLICK_ROW_COL',
    toolbar_button_click       TYPE slis_formname VALUE 'TOOLBAR_BUTTON_CLICK',
    double_click_col_separator TYPE slis_formname VALUE 'DOUBLE_CLICK_COL_SEPARATOR',
    delayed_change_selection   TYPE slis_formname VALUE 'DELAYED_CHANGE_SELECTION',
    context_menu               TYPE slis_formname VALUE 'CONTEXT_MENU',
    total_click_row_col        TYPE slis_formname VALUE 'TOTAL_CLICK_ROW_COL',
    context_menu_selected      TYPE slis_formname VALUE 'CONTEXT_MENU_SELECTED',
    context_menu_request       TYPE slis_formname VALUE 'CONTEXT_MENU_REQUEST',
    toolbar_menu_selected      TYPE slis_formname VALUE 'TOOLBAR_MENU_SELECTED',
    request_data               TYPE slis_formname VALUE 'REQUEST_DATA',
  END OF EVENTS .
  data CB_AFTER_REFRESH type SLIS_FORMNAME value 'FRM_AFTER_REFRESH' ##NO_TEXT.
  data CB_AFTER_USER_COMMAND type SLIS_FORMNAME value 'FRM_AFTER_USER_COMMAND' ##NO_TEXT.
  data CB_BEFORE_USER_COMMAND type SLIS_FORMNAME value 'FRM_BEFORE_USER_COMMAND' ##NO_TEXT.
  data CB_BTN_CLICK type SLIS_FORMNAME value 'FRM_BTN_CLICK' ##NO_TEXT.
  data CB_DATA_CHANGED type SLIS_FORMNAME value 'FRM_DATA_CHANGED' ##NO_TEXT.
  data CB_DATA_CHANGED_INTERNAL type SLIS_FORMNAME value 'FRM_DATA_CHANGED_INTERNAL' ##NO_TEXT.
  data CB_DATA_CHANGED_FINISHED type SLIS_FORMNAME value 'FRM_DATA_CHANGED_FINISHED' ##NO_TEXT.
  data CB_DOUBLE_CLICK type SLIS_FORMNAME value 'FRM_DOUBLE_CLICK' ##NO_TEXT.
  data CB_HOTSPOT_CLICK type SLIS_FORMNAME value 'FRM_HOTSPOT_CLICK' ##NO_TEXT.
  data CB_MENU_BUTTON type SLIS_FORMNAME value 'FRM_MENU_BUTTON' ##NO_TEXT.
  data CB_ONF1 type SLIS_FORMNAME value 'FRM_ONF1' ##NO_TEXT.
  data CB_ONF4 type SLIS_FORMNAME value 'FRM_ONF4' ##NO_TEXT.
  data CB_SUBTOTAL_TEXT type SLIS_FORMNAME value 'FRM_SUBTOTAL_TEXT' ##NO_TEXT.
  data CB_TOOLBAR type SLIS_FORMNAME value 'FRM_TOOLBAR' ##NO_TEXT.
  data CB_USER_COMMAND type SLIS_FORMNAME value 'FRM_USER_COMMAND' ##NO_TEXT.
  data CB_AT_SET_PF_STATUS type SLIS_FORMNAME value 'FRM_AT_SET_PF_STATUS' ##NO_TEXT.
  data CB_AT_SET_TITLE type SLIS_FORMNAME value 'FRM_AT_SET_TITLE' ##NO_TEXT.
  data CB_TOP_OF_PAGE type SLIS_FORMNAME value 'FRM_TOP_OF_PAGE' ##NO_TEXT.
  data CB_DELAYED_CALLBACK type SLIS_FORMNAME value 'FRM_DELAYED_CALLBACK' ##NO_TEXT.
  data CB_DELAYED_CHANGED_SEL_CALL type SLIS_FORMNAME value 'FRM_DELAYED_CHANGED_SEL_CALL' ##NO_TEXT.
  data CB_ONDRAG type SLIS_FORMNAME value 'FRM_ONDRAG' ##NO_TEXT.
  data CB_ONDROP type SLIS_FORMNAME value 'FRM_ONDROP' ##NO_TEXT.
  data CB_ONDROPCOMPLETE type SLIS_FORMNAME value 'FRM_ONDROPCOMPLETE' ##NO_TEXT.
  data CB_ONDROPGETFLAVOR type SLIS_FORMNAME value 'FRM_ONDROPGETFLAVOR' ##NO_TEXT.
  data CB_DROP_EXTERNAL_FILE type SLIS_FORMNAME value 'FRM_DROP_EXTERNAL_FILE' ##NO_TEXT.
  data CB_TOOLBAR_MENUBUTTON_CLICK type SLIS_FORMNAME value 'FRM_TOOLBAR_MENUBUTTON_CLICK' ##NO_TEXT.
  data CB_CLICK_COL_HEADER type SLIS_FORMNAME value 'FRM_CLICK_COL_HEADER' ##NO_TEXT.
  data CB_DELAYED_MOVE_CURRENT_CELL type SLIS_FORMNAME value 'FRM_DELAYED_MOVE_CURRENT_CELL' ##NO_TEXT.
  data CB_F1 type SLIS_FORMNAME value 'FRM_F1' ##NO_TEXT.
  data CB_DBLCLICK_ROW_COL type SLIS_FORMNAME value 'FRM_DBLCLICK_ROW_COL' ##NO_TEXT.
  data CB_CLICK_ROW_COL type SLIS_FORMNAME value 'FRM_CLICK_ROW_COL' ##NO_TEXT.
  data CB_TOOLBAR_BUTTON_CLICK type SLIS_FORMNAME value 'FRM_TOOLBAR_BUTTON_CLICK' ##NO_TEXT.
  data CB_DOUBLE_CLICK_COL_SEPARATOR type SLIS_FORMNAME value 'FRM_DOUBLE_CLICK_COL_SEPARATOR' ##NO_TEXT.
  data CB_DELAYED_CHANGE_SELECTION type SLIS_FORMNAME value 'FRM_DELAYED_CHANGE_SELECTION' ##NO_TEXT.
  data CB_CONTEXT_MENU type SLIS_FORMNAME value 'FRM_CONTEXT_MENU' ##NO_TEXT.
  data CB_TOTAL_CLICK_ROW_COL type SLIS_FORMNAME value 'FRM_TOTAL_CLICK_ROW_COL' ##NO_TEXT.
  data CB_CONTEXT_MENU_SELECTED type SLIS_FORMNAME value 'FRM_CONTEXT_MENU_SELECTED' ##NO_TEXT.
  data CB_CONTEXT_MENU_REQUEST type SLIS_FORMNAME value 'FRM_CONTEXT_MENU_REQUEST' ##NO_TEXT.
  data CB_TOOLBAR_MENU_SELECTED type SLIS_FORMNAME value 'FRM_TOOLBAR_MENU_SELECTED' ##NO_TEXT.
  data CB_REQUEST_DATA type SLIS_FORMNAME value 'FRM_REQUEST_DATA' ##NO_TEXT.

* add by yu.xiaosan 20230525 events callback form end
  class-methods CREATE
    importing
      value(I_PARENT) type ref to CL_GUI_CONTAINER optional
      value(I_APPLOGPARENT) type ref to CL_GUI_CONTAINER optional
      value(I_POPUP) type ABAP_BOOL default ABAP_FALSE
      value(I_APPLOG_EMBEDDED) type ABAP_BOOL default ABAP_FALSE
      value(I_SUBCLASS) type ref to CL_ABAP_TYPEDESCR optional
      value(I_REPID) type SY-REPID optional
      value(I_HANDLE) type SLIS_HANDL optional
      value(I_SUFFIX) type FINB_GN_SUFFIX optional
      value(IT_EVENTS) type SLIS_T_EVENT optional
    changing
      !CT_TABLE type STANDARD TABLE
    returning
      value(RV_FALV) type ref to ZWFT_FALV .
  methods CREATE_BY_COPY
    importing
      value(I_PARENT) type ref to CL_GUI_CONTAINER optional
      value(I_APPLOGPARENT) type ref to CL_GUI_CONTAINER optional
      value(I_POPUP) type ABAP_BOOL default ABAP_FALSE
    returning
      value(RV_FALV) type ref to ZWFT_FALV .
  class-methods CREATE_BY_TYPE
    importing
      value(I_PARENT) type ref to CL_GUI_CONTAINER optional
      value(I_APPLOGPARENT) type ref to CL_GUI_CONTAINER optional
      value(I_POPUP) type ABAP_BOOL default ABAP_FALSE
      value(I_APPLOG_EMBEDDED) type ABAP_BOOL default ABAP_FALSE
      value(I_SUBCLASS) type ref to CL_ABAP_TYPEDESCR optional
      !I_TYPE type ref to CL_ABAP_TYPEDESCR
      value(I_REPID) type SY-REPID optional
      value(I_HANDLE) type SLIS_HANDL optional
      value(IT_EVENTS) type SLIS_T_EVENT optional
    returning
      value(RV_FALV) type ref to ZWFT_FALV .
  class-methods LVC_FCAT_FROM_ITAB
    importing
      !IT_TABLE type STANDARD TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods CONSTRUCTOR
    importing
      value(I_SHELLSTYLE) type I default 0
      value(I_LIFETIME) type I optional
      value(I_PARENT) type ref to CL_GUI_CONTAINER optional
      value(I_APPL_EVENTS) type CHAR01 default SPACE
      !I_PARENTDBG type ref to CL_GUI_CONTAINER optional
      !I_APPLOGPARENT type ref to CL_GUI_CONTAINER optional
      !I_GRAPHICSPARENT type ref to CL_GUI_CONTAINER optional
      value(I_NAME) type STRING optional
      !I_FCAT_COMPLETE type SAP_BOOL default SPACE
    exceptions
      ERROR_CNTL_CREATE
      ERROR_CNTL_INIT
      ERROR_CNTL_LINK
      ERROR_DP_CREATE
      OBJECT_CREATED_MANUALLY .
  methods PBO
    importing
      value(IV_DYNNR) type SY-DYNNR default SY-DYNNR .
  methods PAI
    importing
      value(IV_DYNNR) type SY-DYNNR default SY-DYNNR
    changing
      !C_UCOMM type SY-UCOMM default SY-UCOMM .
  methods DISPLAY
  final
    importing
      value(IV_FORCE_GRID) type ABAP_BOOL default SPACE
      value(IV_START_ROW) type I optional
      value(IV_START_COLUMN) type I optional
      value(IV_END_ROW) type I optional
      value(IV_END_COLUMN) type I optional
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods EXCLUDE_FUNCTION
    importing
      value(IV_UCOMM) type SY-UCOMM
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods COLUMN
    importing
      value(IV_FIELDNAME) type LVC_S_FCAT-FIELDNAME
    returning
      value(RV_COLUMN) type ref to ZWFT_FALV_COLUMN .
  methods SOFT_REFRESH
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_MARK_FIELD
    importing
      value(IV_FIELDNAME) type LVC_S_FCAT-FIELDNAME
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_EDITABLE
    importing
      value(IV_MODIFY) type ABAP_BOOL default ABAP_FALSE
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_READONLY
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods ADD_BUTTON
    importing
      value(IV_FUNCTION) type UI_FUNC
      value(IV_ICON) type ICON_D optional
      value(IV_QUICKINFO) type ICONQUICK optional
      value(IV_BUTN_TYPE) type TB_BTYPE optional
      value(IV_DISABLED) type ABAP_BOOL optional
      value(IV_TEXT) type TEXT40 optional
      value(IV_CHECKED) type ABAP_BOOL optional
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  "! Don't call it from Toolbar event handler
  "! as it will cause endless loop
  methods DISABLE_BUTTON
    importing
      value(IV_FUNCTION) type UI_FUNC
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  "! Don't call it from Toolbar event handler
  "! as it will cause endless loop
  methods ENABLE_BUTTON
    importing
      value(IV_FUNCTION) type UI_FUNC
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  "! Don't call it from Toolbar event handler
  "! as it will cause endless loop
  methods DELETE_BUTTON
    importing
      value(IV_FUNCTION) type UI_FUNC
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  "! Don't call it from Toolbar event handler
  "! as it will cause endless loop
  methods DELETE_ALL_BUTTONS
    importing
      value(IV_EXCEPTIONS) type TTB_BUTTON optional
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_CELL_DISABLED
    importing
      value(IV_FIELDNAME) type FIELDNAME
      value(IV_ROW) type LVC_S_ROID-ROW_ID
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_CELL_ENABLED
    importing
      value(IV_FIELDNAME) type FIELDNAME
      value(IV_ROW) type LVC_S_ROID-ROW_ID
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_CELL_BUTTON
    importing
      value(IV_FIELDNAME) type FIELDNAME
      value(IV_ROW) type LVC_S_ROID-ROW_ID
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_CELL_HOTSPOT
    importing
      value(IV_FIELDNAME) type FIELDNAME
      value(IV_ROW) type LVC_S_ROID-ROW_ID
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_ROW_COLOR
    importing
      value(IV_COLOR) type CHAR04
      value(IV_ROW) type LVC_S_ROID-ROW_ID
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_CELL_COLOR
    importing
      value(IV_FIELDNAME) type FIELDNAME
      value(IV_COLOR) type LVC_S_COLO
      value(IV_ROW) type LVC_S_ROID-ROW_ID
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods MASS_REPLACE .
  methods EXPORT_TO_EXCEL
    returning
      value(RV_XSTRING) type XSTRING .
  methods SAVE_EXCEL_LOCALY
    importing
      value(IV_PATH) type STRING optional
    returning
      value(RV_SAVED) type ABAP_BOOL .
  methods HIDE_TOP_OF_PAGE
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SHOW_TOP_OF_PAGE
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_LIST_VIEW
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods ENCODE_PICTURE_BASE64
    importing
      value(IV_XSTRING) type XSTRING
      value(IV_MIME_TYPE) type CSEQUENCE
    returning
      value(RV_IMAGE) type STRING .
  methods GET_FILE_FROM_MIME
    importing
      value(IV_PATH) type STRING
    exporting
      value(EV_XSTRING) type XSTRING
      value(EV_MIME_TYPE) type STRING .
  methods GET_PICTURE_FROM_SE78
    importing
      value(IV_NAME) type STXBITMAPS-TDNAME
      value(IV_TYPE) type STXBITMAPS-TDBTYPE default 'BCOL'
      value(IV_ID) type STXBITMAPS-TDID default 'BMAP'
    returning
      value(RV_XSTRING) type XSTRING .
  methods SEND
    importing
      value(IV_SUBJECT) type CSEQUENCE optional
      value(IV_SENDER) type AD_SMTPADR optional
      value(IV_SENDER_NAME) type AD_SMTPADR optional
      value(IV_FILENAME) type CSEQUENCE optional
      value(IT_RECIPIENTS) type TT_EMAIL
      value(IV_BODY) type STRING optional
      value(IV_IMPORTANCE) type BCS_DOCIMP optional
      value(IV_SENSITIVITY) type SO_OBJ_SNS optional
      value(IV_IMMEDIATELY) type ABAP_BOOL optional
      value(IV_COMMIT) type ABAP_BOOL default ABAP_TRUE
    returning
      value(R_FALV) type ref to ZWFT_FALV
    exceptions
      CREATE_REQUEST_ERROR
      CREATE_DOCUMENT_ERROR
      ADD_ATTACHMENT_ERROR
      ADD_DOCUMENT_ERROR
      ADD_RECIPIENT_ERROR
      ADD_SENDER_ERROR
      SEND_ERROR
      SEND_IMMEDIATELY_ERROR .
  methods HIDE_APPLOG .
  methods SHOW_APPLOG
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  "! Cell can be editable by layout, by field-catalog or by cell styles
  methods GET_CELL_ENABLED
    importing
      value(I_ROW) type I
      value(I_FIELD) type LVC_FNAME
    returning
      value(R_ENABLED) type ABAP_BOOL .
  methods REFRESH_TOOLBAR
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods SET_DUMMY_FUNCTION_CODE
    returning
      value(R_FALV) type ref to ZWFT_FALV .
  methods GET_COLUMNS
    returning
      value(RT_COLUMNS) type T_COLUMNS .
  methods SET_OUTPUT_TABLE
    changing
      !CT_TABLE type STANDARD TABLE .

  methods SET_FRONTEND_FIELDCATALOG
    redefinition .
  methods SET_FRONTEND_LAYOUT
    redefinition .
  PROTECTED SECTION.


  DATA: toolbar_added      TYPE ttb_button,
        toolbar_deleted    TYPE ttb_button,
        toolbar_disabled   TYPE ttb_button,
        toolbar_exceptions TYPE ttb_button.
  DATA:
        columns TYPE SORTED TABLE OF t_column WITH UNIQUE KEY table_line .

  DATA application_log_embedded TYPE abap_bool .
  DATA subclass_type TYPE REF TO cl_abap_typedescr .

  EVENTS at_set_pf_status .
  EVENTS at_set_title .

  METHODS evf_btn_click
  FOR EVENT button_click OF cl_gui_alv_grid
  IMPORTING
    !es_col_id
    !es_row_no ##NEEDED.
  METHODS evf_user_command
  FOR EVENT user_command OF cl_gui_alv_grid
  IMPORTING
    !e_ucomm ##NEEDED.
  METHODS evf_hotspot_click
  FOR EVENT hotspot_click OF cl_gui_alv_grid
  IMPORTING
    !e_row_id
    !e_column_id
    !es_row_no ##NEEDED.
  METHODS evf_data_changed
  FOR EVENT data_changed OF cl_gui_alv_grid
  IMPORTING
    !er_data_changed
    !e_onf4
    !e_onf4_before
    !e_onf4_after
    !e_ucomm ##NEEDED.
  METHODS evf_data_changed_finished
  FOR EVENT data_changed_finished OF cl_gui_alv_grid
  IMPORTING
    !e_modified
    !et_good_cells ##NEEDED.
  METHODS evf_double_click
  FOR EVENT double_click OF cl_gui_alv_grid
  IMPORTING
    !e_row
    !e_column
    !es_row_no ##NEEDED.
  METHODS evf_onf1
  FOR EVENT onf1 OF cl_gui_alv_grid
  IMPORTING
    !e_fieldname
    !es_row_no
    !er_event_data ##NEEDED.
  METHODS evf_onf4
  FOR EVENT onf4 OF cl_gui_alv_grid
  IMPORTING
    !e_fieldname
    !e_fieldvalue
    !es_row_no
    !er_event_data
    !et_bad_cells
    !e_display ##NEEDED.
  METHODS evf_subtotal_text
  FOR EVENT subtotal_text OF cl_gui_alv_grid
  IMPORTING
    !es_subtottxt_info
    !ep_subtot_line
    !e_event_data ##NEEDED.
  METHODS evf_before_user_command
  FOR EVENT before_user_command OF cl_gui_alv_grid
  IMPORTING
    !e_ucomm ##NEEDED.
  METHODS evf_after_user_command
  FOR EVENT after_user_command OF cl_gui_alv_grid
  IMPORTING
    !e_ucomm
    !e_saved
    !e_not_processed ##NEEDED.
  METHODS evf_menu_button
  FOR EVENT menu_button OF cl_gui_alv_grid
  IMPORTING
    !e_object
    !e_ucomm ##NEEDED.
  METHODS evf_toolbar
  FOR EVENT toolbar OF cl_gui_alv_grid
  IMPORTING
    !e_object
    !e_interactive ##NEEDED.
  METHODS evf_after_refresh
  FOR EVENT after_refresh OF cl_gui_alv_grid.
  METHODS evf_top_of_page
  FOR EVENT top_of_page OF cl_gui_alv_grid
  IMPORTING
    !e_dyndoc_id
    !table_index ##NEEDED.
  METHODS evf_delayed_callback
  FOR EVENT delayed_callback OF cl_gui_alv_grid.
  METHODS evf_delayed_changed_sel_call
  FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.
  METHODS evf_ondropgetflavor
  FOR EVENT ondropgetflavor OF cl_gui_alv_grid
  IMPORTING
    !es_row_no
    !e_column
    !e_dragdropobj
    !e_flavors
    !e_row ##NEEDED.
  METHODS evf_ondrag
  FOR EVENT ondrag OF cl_gui_alv_grid
  IMPORTING
    !es_row_no
    !e_column
    !e_dragdropobj
    !e_row ##NEEDED.
  METHODS evf_ondrop
  FOR EVENT ondrop OF cl_gui_alv_grid
  IMPORTING
    !es_row_no
    !e_column
    !e_dragdropobj
    !e_row ##NEEDED.
  METHODS evf_ondropcomplete
  FOR EVENT ondropcomplete OF cl_gui_alv_grid
  IMPORTING
    !es_row_no
    !e_column
    !e_dragdropobj
    !e_row ##NEEDED.
  METHODS evf_drop_external_file
  FOR EVENT drop_external_files OF cl_gui_alv_grid
  IMPORTING
    !files ##NEEDED.
  METHODS evf_toolbar_menubutton_click
  FOR EVENT toolbar_menubutton_click OF cl_gui_alv_grid
  IMPORTING
    !fcode
    !menu_pos_x
    !menu_pos_y ##NEEDED.
  METHODS evf_click_col_header
  FOR EVENT click_col_header OF cl_gui_alv_grid
  IMPORTING
    !col_id ##NEEDED.
  METHODS evf_delayed_move_current_cell
  FOR EVENT delayed_move_current_cell OF cl_gui_alv_grid.
  METHODS evf_f1
  FOR EVENT f1 OF cl_gui_alv_grid.
  METHODS evf_dblclick_row_col
  FOR EVENT dblclick_row_col OF cl_gui_alv_grid
  IMPORTING
    !col_id
    !row_id ##NEEDED.
  METHODS evf_click_row_col
  FOR EVENT click_row_col OF cl_gui_alv_grid
  IMPORTING
    !col_id
    !row_id ##NEEDED.
  METHODS evf_toolbar_button_click
  FOR EVENT toolbar_button_click OF cl_gui_alv_grid
  IMPORTING
    !fcode ##NEEDED.
  METHODS evf_double_click_col_separator
  FOR EVENT double_click_col_separator OF cl_gui_alv_grid
  IMPORTING
    !col_id ##NEEDED.
  METHODS evf_delayed_change_selection
  FOR EVENT delayed_change_selection OF cl_gui_alv_grid.
  METHODS evf_context_menu
  FOR EVENT context_menu OF cl_gui_alv_grid.
  METHODS evf_context_menu_request
  FOR EVENT context_menu_request OF cl_gui_alv_grid
  IMPORTING
    !e_object.
  METHODS evf_total_click_row_col
  FOR EVENT total_click_row_col OF cl_gui_alv_grid
  IMPORTING
    !col_id
    !row_id ##NEEDED.
  METHODS evf_context_menu_selected
  FOR EVENT context_menu_selected OF cl_gui_alv_grid
  IMPORTING
    !fcode ##NEEDED.
  METHODS evf_toolbar_menu_selected
  FOR EVENT toolbar_menu_selected OF cl_gui_alv_grid
  IMPORTING
    !fcode ##NEEDED.
  METHODS evf_request_data
  FOR EVENT _request_data OF cl_gui_alv_grid
  IMPORTING
    !fragments ##NEEDED.
  METHODS evf_at_set_pf_status
  FOR EVENT at_set_pf_status OF ZWFT_FALV.
  METHODS evf_at_set_title
  FOR EVENT at_set_title OF ZWFT_FALV.
  PRIVATE SECTION.

  CLASS-DATA CREATED_FROM_FACTORY TYPE ABAP_BOOL .
  DATA TOP_OF_PAGE_DOC TYPE REF TO CL_DD_DOCUMENT .
  DATA TOP_OF_PAGE_VISIBLE_AT_START TYPE ABAP_BOOL .
  DATA REPID TYPE SY-REPID .
  DATA MT_EVENTS TYPE SLIS_T_EVENT .
  DATA SUFFIX TYPE FINB_GN_SUFFIX .

  CLASS-METHODS CHECK_IF_CALLED_FROM_SUBCLASS
  returning
  VALUE(RO_SUBCLASS) TYPE REF TO OBJECT .
  CLASS-METHODS CREATE_CONTAINTERS
  IMPORTING
    !I_PARENT TYPE REF TO CL_GUI_CONTAINER
    !I_APPLOGPARENT TYPE REF TO CL_GUI_CONTAINER
    !I_POPUP TYPE ABAP_BOOL
    !I_APPLOG_EMBEDDED TYPE ABAP_BOOL
  EXPORTING
    !E_BUILT_IN_SCREEN TYPE ABAP_BOOL
    !E_PARENT TYPE REF TO CL_GUI_CONTAINER
    !E_APPLOG TYPE REF TO CL_GUI_CONTAINER
    !E_TOP_OF_PAGE_PARENT TYPE REF TO CL_GUI_CONTAINER
    !E_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CONTAINER
    !E_MAIN_SPLIT_CONTAINER TYPE REF TO CL_GUI_SPLITTER_CONTAINER
    !E_SPLIT_CONTAINER TYPE REF TO CL_GUI_SPLITTER_CONTAINER .
  CLASS-METHODS CREATE_FALV_OBJECT
  IMPORTING
    !I_SUBCLASS TYPE REF TO CL_ABAP_TYPEDESCR
    !I_PARENT TYPE REF TO CL_GUI_CONTAINER
    !I_APPLOG TYPE REF TO CL_GUI_CONTAINER
    returning
    VALUE(RV_FALV) TYPE REF TO ZWFT_FALV .
  CLASS-METHODS LINK_CONTAINERS
  IMPORTING
    !IV_FALV TYPE REF TO ZWFT_FALV
    !I_TOP_OF_PAGE_PARENT TYPE REF TO CL_GUI_CONTAINER
    !I_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CONTAINER
    !I_MAIN_SPLIT_CONTAINER TYPE REF TO CL_GUI_SPLITTER_CONTAINER
    !I_SPLIT_CONTAINER TYPE REF TO CL_GUI_SPLITTER_CONTAINER .
  CLASS-METHODS CREATE_MAIN_SPLIT_COTAINER
  IMPORTING
    !I_POPUP TYPE ABAP_BOOL
    !I_APPLOG_EMBEDDED TYPE ABAP_BOOL
    !I_MAIN_PARENT TYPE REF TO CL_GUI_CONTAINER
    returning
    VALUE(R_MAIN_SPLIT_CONTAINER) TYPE REF TO CL_GUI_SPLITTER_CONTAINER .
  CLASS-METHODS CREATE_MAIN_CONT_FOR_FULL_SCR
  IMPORTING
    !I_POPUP TYPE ABAP_BOOL
    returning
    VALUE(R_CUSTOM_CONTAINER) TYPE REF TO CL_GUI_CONTAINER .
  CLASS-METHODS CRATE_MAIN_SPLITTER
  IMPORTING
    !I_MAIN_SPLIT_CONTAINER TYPE REF TO CL_GUI_SPLITTER_CONTAINER
    returning
    VALUE(R_SPLIT_CONTAINER) TYPE REF TO CL_GUI_SPLITTER_CONTAINER .
  METHODS EVF_BEFORE_UCOMMAND_INTERNAL
  FOR EVENT BEFORE_USER_COMMAND OF CL_GUI_ALV_GRID
  IMPORTING
    !E_UCOMM  ##NEEDED.
  METHODS EVF_TOOLBAR_INTERNAL
  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
  IMPORTING
    !E_OBJECT
    !E_INTERACTIVE  ##NEEDED.
  METHODS EVF_DATA_CHANGED_INTERNAL
  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
  IMPORTING
    !ER_DATA_CHANGED
    !E_ONF4
    !E_ONF4_BEFORE
    !E_ONF4_AFTER
    !E_UCOMM  ##NEEDED.
  METHODS SET_PARENT
  IMPORTING
    !IO_PARENT TYPE REF TO OBJECT
    returning
    VALUE(R_FALV) TYPE REF TO ZWFT_FALV .
  METHODS BUILD_COLUMNS .
  METHODS RAISE_TOP_OF_PAGE .
  METHODS SET_HANDLERS
  IMPORTING
    !IV_FALV TYPE REF TO ZWFT_FALV .
  METHODS SET_EVENTS_SUFFIX
  IMPORTING
    VALUE(IV_FALV) TYPE REF TO ZWFT_FALV .
  METHODS SET_EVENTS
  IMPORTING
    !IV_FALV TYPE REF TO ZWFT_FALV .
  METHODS COPY_ATTRIBUTES
  IMPORTING
    !I_FALV TYPE REF TO ZWFT_FALV .
  METHODS CREATE_EX_RESULT_FALV
  returning
  VALUE(ER_RESULT_TABLE) TYPE REF TO CL_SALV_EX_RESULT_DATA_TABLE .
ENDCLASS.



CLASS ZWFT_FALV IMPLEMENTATION.


  METHOD add_button.
    IF NOT line_exists( toolbar_added[ FUNCTION = iv_function ] ).
      INSERT VALUE #( FUNCTION = iv_function
      ICON = CONV #( iv_icon )
      quickinfo = iv_quickinfo
      butn_type = iv_butn_type
      disabled = iv_disabled
      TEXT = iv_text
      checked = iv_checked
      ) INTO TABLE toolbar_added .

      DELETE toolbar_deleted WHERE FUNCTION = iv_function.
      me->refresh_toolbar( ).
    ENDIF.
    r_falv = me.
  ENDMETHOD.


  METHOD build_columns.
    LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fcat>).
      DATA(column) = NEW ZWFT_FALV_column( iv_fieldname = <fcat>-fieldname io_falv = me ).
      INSERT column INTO TABLE columns.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_if_called_from_subclass.
    DATA: callstack TYPE abap_callstack,
          src       TYPE TABLE OF string.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
    IMPORTING
      callstack = callstack.

    ASSIGN callstack[ 3 ] TO FIELD-SYMBOL(<stack>).
    CHECK sy-subrc EQ 0.
    DATA(compiler) = cl_abap_compiler=>create(
          p_name             = <stack>-mainprogram
          p_include          = <stack>-INCLUDE
          p_no_package_check = abap_true ).

    compiler->get_single_ref(
    EXPORTING
      p_full_name = |\\TY:ZWFT_FALV\\ME:{ callstack[ 2 ]-blockname CASE = UPPER }|
      p_grade     = 1   " Grade of Use
    IMPORTING
    p_result    = DATA(falv_references) " Where-Used List
    EXCEPTIONS
      OTHERS      = 5 ).
    IF sy-subrc EQ 0.
      READ REPORT <stack>-INCLUDE INTO src.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

      ASSIGN src[ <stack>-LINE ] TO FIELD-SYMBOL(<line>).
      IF <line> IS ASSIGNED.
        ASSIGN falv_references[ LINE = <stack>-LINE ] TO FIELD-SYMBOL(<reference>).
        IF sy-subrc EQ 0.
          DATA: subclass_name TYPE string.
          DO.
            DATA(offset) = <reference>-column - sy-INDEX - 2. "-2 because of =>
            IF offset LT 0 OR <line>+offset(1) EQ ` `.
              EXIT.
            ENDIF.
            subclass_name =  <line>+offset(1) && subclass_name.
          ENDDO.
          IF subclass_name IS INITIAL OR to_upper( subclass_name ) EQ 'ZWFT_FALV'.
            RETURN.
          ENDIF.

          "global class
          cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = to_upper( subclass_name )
            RECEIVING  p_descr_ref    = ro_subclass
          EXCEPTIONS type_not_found = 1 ).
          IF sy-subrc EQ 0.
            RETURN.
          ELSE.
            "local class
            subclass_name = |\\PROGRAM={ <stack>-mainprogram }\\CLASS={ subclass_name }|.
            cl_abap_classdescr=>describe_by_name( EXPORTING  p_name         = to_upper( subclass_name )
              RECEIVING  p_descr_ref    = ro_subclass
            EXCEPTIONS type_not_found = 1 ).
            IF sy-subrc EQ 0.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD column.
    TRY.
      rv_column = columns[  table_line->fieldname = iv_fieldname ].
    CATCH cx_sy_itab_line_not_found.

    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    IF created_from_factory EQ abap_false.
      RAISE object_created_manually.
    ENDIF.

    super->constructor(
    EXPORTING
      i_shellstyle      = i_shellstyle    " Control Style
      i_lifetime        = i_lifetime    " Lifetime
      i_parent          = i_parent    " Parent Container
      i_appl_events     = i_appl_events    " Register Events as Application Events
      i_parentdbg       = i_parentdbg    " Internal, Do not Use
      i_applogparent    = i_applogparent    " Container for Application Log
      i_graphicsparent  = i_graphicsparent    " Container for Graphics
      i_name            = i_name    " Name
      i_fcat_complete   = i_fcat_complete  " Boolean Variable (X=True, Space=False)
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
      WHEN 1.
        RAISE error_cntl_create.
      WHEN 2.
        RAISE error_cntl_init.
      WHEN 3.
        RAISE error_cntl_link.
      WHEN 4.
        RAISE error_dp_create.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD copy_attributes.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE.
    ASSIGN me->outtab->* TO <outtab>.
    i_falv->set_output_table( CHANGING ct_table = <outtab> ).
    i_falv->fcat = i_falv->lvc_fcat_from_itab( it_table = <outtab> ).
    i_falv->sort = me->sort.
    i_falv->filter = me->filter.
    i_falv->set_frontend_fieldcatalog( it_fieldcatalog = me->fcat ).
    i_falv->application_log_embedded = application_log_embedded.
    i_falv->built_in_screen = built_in_screen.
    i_falv->build_columns( ).
    i_falv->layout = NEW ZWFT_FALV_layout( io_falv = i_falv ).
    i_falv->gui_status ?= me->gui_status->if_os_clone~clone( ). "clone object
    i_falv->lvc_layout = me->lvc_layout.
    i_falv->variant = me->variant.
    i_falv->top_of_page_height = me->top_of_page_height.
    i_falv->top_of_page_visible_at_start = me->top_of_page_visible_at_start.
    i_falv->title_v1 = me->title_v1.
    i_falv->title_v2 = me->title_v2.
    i_falv->title_v3 = me->title_v3.
    i_falv->title_v4 = me->title_v4.
    i_falv->exclude_functions = me->exclude_functions.
    i_falv->toolbar_added = me->toolbar_added.
    i_falv->toolbar_deleted = me->toolbar_deleted.
    i_falv->toolbar_disabled = me->toolbar_disabled.
    i_falv->m_batch_mode = me->m_batch_mode.
    i_falv->layout->delete_all_buttons = me->layout->delete_all_buttons.
    i_falv->layout->mark_field = me->layout->mark_field.
    i_falv->register_f4_for_fields( it_f4 = me->grid->mt_f4 ).
    i_falv->repid = me->repid.
    i_falv->mt_events = me->mt_events.
  ENDMETHOD.


  METHOD crate_main_splitter.
    DATA(parent) = i_main_split_container->get_container( row = 1 column = 1 ).
    IF parent->children[] IS NOT INITIAL.
      r_split_container ?= parent->children[ 1 ].
      RETURN.
    ENDIF.
    r_split_container = NEW cl_gui_splitter_container(
    parent  = parent
    ROWS    = 2
    columns = 1 ).

  ENDMETHOD.


  METHOD create.
    DATA: callstack TYPE abap_callstack.
    IF i_subclass IS INITIAL.
      i_subclass ?= check_if_called_from_subclass( ).
    ENDIF.

    create_containters(
    EXPORTING
      i_parent               = i_parent
      i_applogparent         = i_applogparent
      i_popup                = i_popup
      i_applog_embedded      = i_applog_embedded
    IMPORTING
    e_built_in_screen      = DATA(built_in_screen)
    e_parent               = DATA(parent)
    e_applog               = DATA(applog)
    e_top_of_page_parent   = DATA(top_of_page_parent)
    e_custom_container     = DATA(custom_container)
    e_main_split_container = DATA(main_split_container)
    e_split_container      = DATA(split_container) ).


    rv_falv = create_falv_object(
    i_subclass = i_subclass
    i_parent   = parent
    i_applog   = applog ).

    rv_falv->set_handlers( rv_falv ).
    rv_falv->suffix = i_suffix.
    rv_falv->set_events_suffix( rv_falv ).
    rv_falv->mt_events = it_events.
    rv_falv->set_events( rv_falv ).

    rv_falv->set_output_table( CHANGING ct_table = ct_table ).
    rv_falv->fcat = rv_falv->lvc_fcat_from_itab( it_table = ct_table ).
    rv_falv->set_frontend_fieldcatalog( it_fieldcatalog = rv_falv->fcat ).
    rv_falv->application_log_embedded = i_applog_embedded.
    rv_falv->built_in_screen = built_in_screen.
    rv_falv->build_columns( ).
    rv_falv->layout = NEW zwft_falv_layout( rv_falv ).

    rv_falv->gui_status = NEW zwft_falv_dynamic_status( ).
    rv_falv->screen = SWITCH #( i_popup WHEN abap_true THEN c_screen_popup
    WHEN abap_false THEN c_screen_full ).

    IF built_in_screen EQ abap_true AND rv_falv->screen EQ c_screen_full.
      "default in full screen
      rv_falv->layout->set_no_toolbar( abap_true ).
      CALL FUNCTION 'ZWFT_FALV_ADD_FALV_TO_STACK'
        EXPORTING
          io_falv = rv_falv.
    ENDIF.


    link_containers(
    iv_falv                = rv_falv
    i_top_of_page_parent   = top_of_page_parent
    i_custom_container     = custom_container
    i_main_split_container = main_split_container
    i_split_container      = split_container ).

    rv_falv->layout_save = 'A'.
    rv_falv->lvc_layout-sel_mode   = 'A'.
    rv_falv->lvc_layout-cwidth_opt = 'X'.
    rv_falv->lvc_layout-zebra      = 'X'.

    rv_falv->grid = CAST #(  rv_falv ).

    IF i_repid IS INITIAL.
      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          callstack = callstack.

      ASSIGN callstack[ 2 ] TO FIELD-SYMBOL(<stack>).
      IF sy-subrc EQ 0.
        rv_falv->repid = <stack>-mainprogram.
      ENDIF.
    ELSE.
      rv_falv->repid = i_repid.
    ENDIF.
    IF i_handle IS NOT INITIAL.
      rv_falv->variant-handle   = i_handle.
    ENDIF.
    rv_falv->variant-report = i_repid.
    rv_falv->variant-username = sy-uname.
  ENDMETHOD.


  METHOD create_by_copy.

    create_containters(
    EXPORTING
      i_parent               = i_parent
      i_applogparent         = i_applogparent
      i_popup                = i_popup
      i_applog_embedded      = application_log_embedded
    IMPORTING
*       e_built_in_screen      = data(built_in_screen)
    e_parent               = DATA(parent)
    e_applog               = DATA(applog)
    e_top_of_page_parent   = DATA(top_of_page_parent)
    e_custom_container     = DATA(custom_container)
    e_main_split_container = DATA(main_split_container)
    e_split_container      = DATA(split_container) ).


    rv_falv = create_falv_object(
    i_subclass = subclass_type
    i_parent   = parent
    i_applog   = applog ).

    copy_attributes( rv_falv ).
    set_handlers( rv_falv ).
    set_events( iv_falv = rv_falv ).


    rv_falv->screen = SWITCH #( i_popup WHEN abap_true THEN c_screen_popup
    WHEN abap_false THEN c_screen_full ).
    link_containers(
    iv_falv                = rv_falv
    i_top_of_page_parent   = top_of_page_parent
    i_custom_container     = custom_container
    i_main_split_container = main_split_container
    i_split_container      = split_container ).

    IF rv_falv->built_in_screen EQ abap_true.
      CALL FUNCTION 'ZWFT_FALV_ADD_FALV_TO_STACK'
      EXPORTING
        io_falv = rv_falv.
    ENDIF.
    rv_falv->grid = CAST #( rv_falv ).
  ENDMETHOD.


  METHOD create_by_type.
    DATA: callstack TYPE abap_callstack.
    DATA: lv_repid  LIKE sy-repid.
    DATA:
    lr_output TYPE REF TO DATA.

    FIELD-SYMBOLS:
    <table> TYPE ANY TABLE.

    DATA(lv_type_name) = i_type->absolute_name.

    IF i_type->kind <> cl_abap_typedescr=>kind_table.
      FREE: rv_falv.
      RETURN.
    ENDIF.

    CREATE DATA lr_output TYPE (lv_type_name).
    ASSIGN lr_output->* TO <table>.

    IF i_subclass IS INITIAL.
      i_subclass ?= check_if_called_from_subclass( ).
    ENDIF.

    IF i_repid IS INITIAL.
      CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

      ASSIGN callstack[ 2 ] TO FIELD-SYMBOL(<stack>).
      IF sy-subrc EQ 0.
        lv_repid = <stack>-mainprogram.
      ENDIF.
    ELSE.
      lv_repid = i_repid.
    ENDIF.

    rv_falv = ZWFT_FALV=>create(
    EXPORTING
      i_parent          = i_parent
      i_applogparent    = i_applogparent
      i_popup           = i_popup
      i_applog_embedded = i_applog_embedded
      i_subclass        = i_subclass
      i_repid           = lv_repid
      i_handle          = i_handle
      it_events         = it_events
    CHANGING
      ct_table          = <table>
      ).
  ENDMETHOD.


  METHOD create_containters.

    DATA main_parent TYPE REF TO cl_gui_container.
    DATA docking_parent TYPE REF TO cl_gui_docking_container.

    IF cl_gui_alv_grid=>offline( ) IS NOT INITIAL.
      main_parent ?= docking_parent.
    ENDIF.
    "We need to call full screen ALV as container was not passed
    IF i_parent IS INITIAL.
      e_built_in_screen = abap_true.

      IF cl_gui_alv_grid=>offline( ) IS INITIAL.
        e_custom_container = create_main_cont_for_full_scr( i_popup ).
        main_parent ?= e_custom_container.
        e_main_split_container = create_main_split_cotainer( i_popup           = i_popup
        i_applog_embedded = i_applog_embedded
        i_main_parent     = main_parent ).
        e_split_container = crate_main_splitter( e_main_split_container ).
        e_parent ?= e_split_container->get_container( row = 2 column = 1 ).
        e_applog ?= e_main_split_container->get_container( row = 2 column = 1 ).
        e_top_of_page_parent ?= e_split_container->get_container( row = 1 column = 1 ).
      ELSE.

        e_parent ?= main_parent.
        e_custom_container ?= e_parent.

      ENDIF.

    ELSE.
      IF cl_gui_alv_grid=>offline( ) IS NOT INITIAL.

        e_parent ?= main_parent.
        e_custom_container ?= e_parent.

      ELSE.

        e_applog ?= i_applogparent.
        e_custom_container ?= i_parent.

        e_main_split_container = create_main_split_cotainer( i_popup           = i_popup
        i_applog_embedded = i_applog_embedded
        i_main_parent     = e_custom_container ).

        e_split_container = crate_main_splitter( e_main_split_container ).

        IF e_applog IS INITIAL.
          e_parent ?= e_split_container->get_container( row = 2 column = 1 ).
          e_applog ?= e_main_split_container->get_container( row = 2 column = 1 ).
          e_top_of_page_parent ?= e_split_container->get_container( row = 1 column = 1 ).
        ELSE.
          e_parent ?= e_split_container->get_container( row = 2 column = 1 ).
          e_top_of_page_parent ?= e_split_container->get_container( row = 1 column = 1 ).
          e_custom_container ?= i_parent.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD create_ex_result_falv.

    DATA:
          lt_lvc_row TYPE lvc_t_row.

    CLEAR:
    lt_lvc_row.
    me->get_selected_rows(
    IMPORTING
      et_index_rows = lt_lvc_row ).

    DATA: lt_sel_cols  TYPE lvc_t_col,
          lt_sel_cells TYPE lvc_t_cell.

    me->get_selected_columns(
    IMPORTING
      et_index_columns = lt_sel_cols ).

    me->get_selected_cells(
    IMPORTING
      et_cell = lt_sel_cells ).

    DATA:
          ls_lvc_col  TYPE lvc_s_col,
          ls_lvc_row  TYPE lvc_s_row,
          ls_cur_cell TYPE lvc_s_cell.

    CLEAR:
    ls_lvc_row,
    ls_lvc_col.
    me->get_current_cell(
    IMPORTING
      es_row_id = ls_lvc_row
      es_col_id = ls_lvc_col ).
    ls_cur_cell-col_id-fieldname = ls_lvc_col-fieldname.
    ls_cur_cell-row_id-INDEX = ls_lvc_row-INDEX.

    DATA: ls_hyper_entry    TYPE string,
          ls_dropdown_entry TYPE string,
          lt_drdn           TYPE lvc_t_drop.

    IF grid->r_salv_adapter IS BOUND.
      DATA:
            lr_display TYPE REF TO if_salv_display_adapter.

      lr_display ?= grid->r_salv_adapter.

      DATA:
            lr_columns TYPE REF TO cl_salv_columns_list.

      lr_columns ?= lr_display->get_columns( ).

      ls_hyper_entry = lr_columns->get_hyperlink_entry_column( ).
      ls_dropdown_entry = lr_columns->get_dropdown_entry_column( ).

      DATA:
            lr_om TYPE REF TO cl_salv_table.

      lr_om ?= grid->r_salv_adapter->r_controller->r_model.

      DATA:
            lr_functional_settings TYPE REF TO cl_salv_functional_settings.

      lr_functional_settings = lr_om->get_functional_settings( ).

      DATA:
            lr_dropdowns TYPE REF TO cl_salv_dropdowns.

***<<<Y7AK057779
      TRY.
        lr_dropdowns = lr_functional_settings->get_dropdowns( ).

        lt_drdn = cl_salv_controller_metadata=>get_dropdowns( lr_dropdowns ).
      CATCH cx_salv_method_not_supported.
        CLEAR sy-subrc.
      ENDTRY.
***>>>Y7AK057779

*>>> Y7AK058143
      DATA:
            lr_tol TYPE REF TO cl_salv_form_element,
            lr_eol TYPE REF TO cl_salv_form_element.
*<<< Y7AK058143

      lr_tol = lr_om->get_top_of_list( ).
      lr_eol = lr_om->get_end_of_list( ).
    ENDIF.

*>>> Y7AK058143
    DATA:
          lr_top_of_list TYPE REF TO cl_salv_form,
          lr_end_of_list TYPE REF TO cl_salv_form.

    CREATE OBJECT lr_top_of_list
    EXPORTING
      r_content = lr_tol.

    CREATE OBJECT lr_end_of_list
    EXPORTING
      r_content = lr_eol.
*<<< Y7AK058143

    er_result_table = cl_salv_ex_util=>factory_result_data_table(
    t_selected_rows        = lt_lvc_row
    t_selected_columns     = lt_sel_cols
    t_selected_cells       = lt_sel_cells
    r_data                 = grid->mt_outtab
    s_layout               = grid->m_cl_variant->ms_layout
    t_fieldcatalog         = grid->m_cl_variant->mt_fieldcatalog
    t_sort                 = grid->m_cl_variant->mt_sort
    t_filter               = grid->m_cl_variant->mt_filter
    t_hyperlinks           = grid->mt_hyperlinks
    s_current_cell         = ls_cur_cell
    hyperlink_entry_column = ls_hyper_entry
    dropdown_entry_column  = ls_dropdown_entry
    r_top_of_list          = lr_top_of_list
    r_end_of_list          = lr_end_of_list
    t_dropdown_values      = lt_drdn ).

  ENDMETHOD.


  METHOD create_falv_object.

    created_from_factory = abap_true.

    IF i_subclass IS NOT INITIAL.
      DATA: subclass TYPE REF TO object.
      DATA(sublcass_abs_name) = i_subclass->absolute_name.
      CREATE OBJECT subclass TYPE (sublcass_abs_name)
      EXPORTING
        i_parent       = i_parent
        i_applogparent = i_applog.
      rv_falv ?= subclass.
      rv_falv->subclass_type = i_subclass.

    ELSE.
      CREATE OBJECT rv_falv
      EXPORTING
        i_parent       = i_parent
        i_applogparent = i_applog.

    ENDIF.


  ENDMETHOD.


  METHOD create_main_cont_for_full_scr.

    IF i_popup EQ abap_true.

      CALL FUNCTION 'ZWFT_FALV_CREATE_MAIN_CONTAINE'
      IMPORTING
        main_container = r_custom_container.
    ELSE.
      r_custom_container  = CAST cl_gui_container( NEW cl_gui_custom_container(
      container_name = cc_name
      dynnr          = c_screen_full
      repid          = c_fscr_repid
      no_autodef_progid_dynnr =  abap_true  ) ).
    ENDIF.


  ENDMETHOD.


  METHOD create_main_split_cotainer.

    " Create split container, log at bottom, grid at top.
    "Log hidden as default, will appear when error will be thrown.
    IF i_main_parent->children[] IS NOT INITIAL.
      r_main_split_container ?= i_main_parent->children[ 1 ].
      RETURN.
    ENDIF.
    r_main_split_container = NEW cl_gui_splitter_container(
    link_dynnr = SWITCH #( i_popup WHEN abap_true THEN c_screen_popup
    WHEN abap_false THEN c_screen_full )
    link_repid = c_fscr_repid
    parent     = i_main_parent
    ROWS       = COND #( WHEN  i_applog_embedded EQ abap_true THEN 2
    ELSE 1 )
    columns    = 1 ).

  ENDMETHOD.


  METHOD delete_all_buttons.
    layout->delete_all_buttons = abap_true.
    IF iv_exceptions IS INITIAL.
      APPEND LINES OF mt_toolbar TO toolbar_deleted.
      REFRESH toolbar_added.
    ELSE.
      toolbar_exceptions = iv_exceptions.
      LOOP AT mt_toolbar ASSIGNING FIELD-SYMBOL(<tlb>).
        IF NOT line_exists( iv_exceptions[ FUNCTION = <tlb>-FUNCTION ] ).
          APPEND <tlb> TO toolbar_deleted.
          DELETE toolbar_added WHERE FUNCTION EQ <tlb>-FUNCTION.
        ENDIF.
      ENDLOOP.
    ENDIF.
    APPEND LINES OF toolbar_deleted TO exclude_functions.
    me->refresh_toolbar( ).
    r_falv = me.
  ENDMETHOD.


  METHOD delete_button.
    DELETE toolbar_added WHERE FUNCTION = iv_function.
    IF sy-subrc NE 0.
      INSERT VALUE #( FUNCTION = iv_function ) INTO TABLE toolbar_deleted.
    ENDIF.
    me->refresh_toolbar( ).
    r_falv = me.
  ENDMETHOD.


  METHOD disable_button.
    TRY.
      toolbar_added[ FUNCTION = iv_function ]-disabled = abap_true.
    CATCH cx_sy_itab_line_not_found.
      CLEAR sy-subrc.
    ENDTRY.
    INSERT VALUE #( FUNCTION = iv_function ) INTO TABLE toolbar_disabled.
    me->refresh_toolbar( ).
    r_falv = me.
  ENDMETHOD.


  METHOD display.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE.
    r_falv = me.
    IF me->title_v1 IS INITIAL.
      me->title_v1 = sy-title. " for lazy people who wants to have alv title to be equal one from report.
    ENDIF.
    IF built_in_screen EQ abap_true AND iv_force_grid EQ abap_false.
      IF screen EQ c_screen_popup AND iv_start_row IS INITIAL
      AND iv_start_column IS INITIAL
      AND iv_end_row IS INITIAL
      AND iv_end_column IS INITIAL.
        iv_start_row = 1.
        iv_start_column = 1.
        iv_end_row = 20.
        iv_end_column = 150.
      ENDIF.
      CALL FUNCTION 'ZWFT_FALV_DISPLAY'
        EXPORTING
          io_falv         = me
          iv_start_row    = iv_start_row
          iv_start_column = iv_start_column
          iv_end_row      = iv_end_row
          iv_end_column   = iv_end_column.
      .
    ELSE.
      ASSIGN outtab->* TO <outtab>.
      me->set_table_for_first_display(
      EXPORTING
        i_buffer_active               = buffering_active   " Buffering Active
        i_bypassing_buffer            = bypassing_buffer   " Switch Off Buffer
*         i_consistency_check           =     " Starting Consistency Check for Interface Error Recognition
*         i_structure_name              =     " Internal Output Table Structure Name
        is_variant                    = variant
        i_save                        = layout_save
*         i_default                     = 'X'    " Default Display Variant
        is_layout                     = lvc_layout
*         is_print                      =     " Print Control
*         it_special_groups             =     " Field Groups
        it_toolbar_excluding          = exclude_functions
*         it_hyperlink                  =     " Hyperlinks
*         it_alv_graphics               =     " Table of Structure DTC_S_TC
*         it_except_qinfo               =     " Table for Exception Quickinfo
*         ir_salv_adapter               =     " Interface ALV Adapter
      CHANGING
        it_outtab                     = <outtab> " Output Table
        it_fieldcatalog               = fcat
        it_sort                       = sort " Sort Criteria
        it_filter                     = filter " Filter Criteria
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).
      IF sy-subrc EQ 0.
        IF layout->delete_all_buttons EQ abap_true.
          delete_all_buttons( toolbar_exceptions ).
        ENDIF.
        IF split_container IS NOT INITIAL.
          split_container->set_focus(
          EXPORTING
            control           = me
          EXCEPTIONS
            cntl_error        = 0
            cntl_system_error = 0
            OTHERS            = 0 ).
          me->hide_applog( ).
        ELSE.
          me->parent->set_focus(
          EXPORTING
            control           = me
          EXCEPTIONS
            cntl_error        = 0
            cntl_system_error = 0
            OTHERS            = 0 ).
        ENDIF.
        cl_gui_cfw=>flush(
        EXCEPTIONS
          cntl_system_error = 0
          cntl_error        = 0
          OTHERS            = 0 ).
        me->set_visible( abap_true ).
        me->parent->set_visible( abap_true ).
        me->main_container->set_visible( abap_true ).
        IF me->split_container IS NOT INITIAL.
          me->split_container->set_visible( abap_true ).
        ENDIF.
        me->raise_top_of_page( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD enable_button.
    TRY.
      toolbar_added[ FUNCTION = iv_function ]-disabled = abap_false.
    CATCH cx_sy_itab_line_not_found.
      CLEAR sy-subrc.
    ENDTRY.
    DELETE toolbar_disabled WHERE FUNCTION = iv_function.
    me->refresh_toolbar( ).
    r_falv = me.
  ENDMETHOD.


  METHOD encode_picture_base64.

  rv_image = |<img src="data:{ iv_mime_type };base64,{ cl_http_utility=>encode_x_base64( unencoded = iv_xstring ) }">|.

  ENDMETHOD.


  METHOD evf_after_refresh.
    PERFORM (cb_after_refresh) IN PROGRAM (repid) IF FOUND USING me.       "ZCL_SALV
  ENDMETHOD.


  METHOD evf_after_user_command.
    PERFORM (cb_after_user_command) IN PROGRAM (repid) IF FOUND USING me       "ZCL_SALV
          e_ucomm. "SY-UCOMM
  ENDMETHOD.


  METHOD evf_at_set_pf_status.
    PERFORM (cb_at_set_pf_status) IN PROGRAM (repid) IF FOUND USING me.       "ZCL_SALV
  ENDMETHOD.


  METHOD evf_at_set_title.
    PERFORM (cb_at_set_pf_status) IN PROGRAM (repid) IF FOUND USING me.       "ZCL_SALV
  ENDMETHOD.


  METHOD evf_before_ucommand_internal.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE.
    CASE e_ucomm.
    WHEN me->mc_fc_select_all.
      IF layout->mark_field IS NOT INITIAL AND
      line_exists( fcat[ fieldname = layout->mark_field CHECKBOX = abap_true ] ).
        ASSIGN outtab->* TO <outtab>.
        LOOP AT <outtab> ASSIGNING FIELD-SYMBOL(<line>).
          ASSIGN COMPONENT layout->mark_field OF STRUCTURE <line> TO FIELD-SYMBOL(<mark>).
          IF sy-subrc EQ 0 AND me->get_cell_enabled( i_row = sy-tabix i_field = layout->mark_field ) EQ abap_true.
            <mark> = abap_true.
          ENDIF.
        ENDLOOP.
        me->soft_refresh( ).
        set_user_command( i_ucomm = space ).
      ENDIF.
    WHEN me->mc_fc_deselect_all.
      IF layout->mark_field IS NOT INITIAL AND
      line_exists( fcat[ fieldname = layout->mark_field CHECKBOX = abap_true ] ).
        ASSIGN outtab->* TO <outtab>.
        LOOP AT <outtab> ASSIGNING <line>.
          ASSIGN COMPONENT layout->mark_field OF STRUCTURE <line> TO <mark>.
          IF sy-subrc EQ 0 AND me->get_cell_enabled( i_row = sy-tabix i_field = layout->mark_field ) EQ abap_true.
            <mark> = abap_false.
          ENDIF.
        ENDLOOP.
        me->soft_refresh( ).
        set_user_command( i_ucomm = space ).
      ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD evf_before_user_command.
    PERFORM (cb_before_user_command) IN PROGRAM (repid) IF FOUND USING me       "ZCL_SALV
          e_ucomm. "SY-UCOMM
  ENDMETHOD.


  METHOD evf_btn_click.
    PERFORM (cb_btn_click) IN PROGRAM (repid) IF FOUND USING me        "ZCL_SALV
          es_col_id "LVC_S_COL
          es_row_no."LVC_S_ROID
  ENDMETHOD.


  METHOD evf_click_col_header.
    PERFORM (cb_click_col_header) IN PROGRAM (repid) IF FOUND USING me     "ZCL_SALV
          col_id."C
  ENDMETHOD.


  METHOD evf_click_row_col.
    PERFORM (cb_click_row_col) IN PROGRAM (repid) IF FOUND USING me     "ZCL_SALV
          col_id "C
          row_id."C
  ENDMETHOD.


  METHOD evf_context_menu.
    PERFORM (cb_context_menu) IN PROGRAM (repid) IF FOUND USING me.     "ZCL_SALV
  ENDMETHOD.


  METHOD evf_context_menu_request.
    PERFORM (cb_context_menu_request) IN PROGRAM (repid) IF FOUND USING me       "ZCL_SALV
          e_object."CL_CTMENU
  ENDMETHOD.


  METHOD evf_context_menu_selected.
    PERFORM (cb_context_menu_selected) IN PROGRAM (repid) IF FOUND USING me    "ZCL_SALV
          fcode."C
  ENDMETHOD.


  METHOD evf_data_changed.
    PERFORM (cb_data_changed) IN PROGRAM (repid) IF FOUND USING me              "ZCL_SALV
          er_data_changed "CL_ALV_CHANGED_DATA_PROTOCOL
          e_onf4          "CHAR01
          e_onf4_before   "CHAR01
          e_onf4_after    "CHAR01
          e_ucomm.        "SY-UCOMM
  ENDMETHOD.


  METHOD evf_data_changed_finished.
    PERFORM (cb_data_changed_finished) IN PROGRAM (repid) IF FOUND USING me            "ZCL_SALV
          e_modified    "CHAR01
          et_good_cells."LVC_T_MODI
  ENDMETHOD.


  METHOD evf_data_changed_internal.

    IF er_data_changed->mt_protocol IS NOT INITIAL.
      show_applog( ).
    ELSE.
      hide_applog( ).
    ENDIF.

    "This lines makes that focus stays on grid
    "while embedded error log appears on the screen.
    "This trick will not work if someone will use own handler
    "for data_changed instead of evf_data_changed
    me->m_display_protocol = abap_true.

    er_data_changed->display_protocol( ).

    cl_gui_control=>set_focus(
    EXPORTING
      CONTROL           = CAST #( me )
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3 ).
    IF sy-subrc EQ 0.
      me->m_display_protocol = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD evf_dblclick_row_col.
    PERFORM (cb_dblclick_row_col) IN PROGRAM (repid) IF FOUND USING me     "ZCL_SALV
          col_id "C
          row_id."C
  ENDMETHOD.


  METHOD evf_delayed_callback.
    PERFORM (cb_delayed_callback) IN PROGRAM (repid) IF FOUND USING me."ZCL_SALV
  ENDMETHOD.


  METHOD evf_delayed_changed_sel_call.
    PERFORM (cb_delayed_changed_sel_call) IN PROGRAM (repid) IF FOUND USING me."ZCL_SALV
  ENDMETHOD.


  METHOD evf_delayed_change_selection.
    PERFORM (cb_delayed_change_selection) IN PROGRAM (repid) IF FOUND USING me."ZCL_SALV
  ENDMETHOD.


  METHOD evf_delayed_move_current_cell.
    PERFORM (cb_delayed_move_current_cell) IN PROGRAM (repid) IF FOUND USING me."ZCL_SALV
  ENDMETHOD.


  METHOD evf_double_click.
    PERFORM (cb_double_click) IN PROGRAM (repid) IF FOUND USING me         "ZWFT_FALV
          e_row      "LVC_S_ROW
          e_column   "LVC_S_COL
          es_row_no. "LVC_S_ROID
  ENDMETHOD.


  METHOD evf_double_click_col_separator.
    PERFORM (cb_double_click_col_separator) IN PROGRAM (repid) IF FOUND USING me     "ZWFT_FALV
          col_id."C
  ENDMETHOD.


  METHOD evf_drop_external_file.
    PERFORM (cb_drop_external_file) IN PROGRAM (repid) IF FOUND USING me    "ZWFT_FALV
          files."String
  ENDMETHOD.


  METHOD evf_f1.
    PERFORM (cb_f1) IN PROGRAM (repid) IF FOUND USING me.    "ZWFT_FALV
  ENDMETHOD.


  METHOD evf_hotspot_click.
    PERFORM (cb_hotspot_click) IN PROGRAM (repid) IF FOUND USING me          "ZWFT_FALV
          e_row_id    "LVC_S_ROW
          e_column_id "LVC_S_COL
          es_row_no.  "LVC_S_ROID
  ENDMETHOD.


  METHOD evf_menu_button.
    PERFORM (cb_menu_button) IN PROGRAM (repid) IF FOUND USING me       "ZWFT_FALV
          e_object "CL_CTMENU
          e_ucomm. "SY-UCOMM
  ENDMETHOD.


  METHOD evf_ondrag.
    PERFORM (cb_ondrag) IN PROGRAM (repid) IF FOUND USING me             "ZWFT_FALV
          es_row_no      "LVC_S_ROID
          e_column       "LVC_S_COL
          e_dragdropobj  "CL_DRAGDROPOBJECT
          e_row.         "LVC_S_ROW
  ENDMETHOD.


  METHOD evf_ondrop.
    PERFORM (cb_ondrop) IN PROGRAM (repid) IF FOUND USING me             "ZWFT_FALV
          es_row_no      "LVC_S_ROID
          e_column       "LVC_S_COL
          e_dragdropobj  "CL_DRAGDROPOBJECT
          e_row.         "LVC_S_ROW
  ENDMETHOD.


  METHOD evf_ondropcomplete.
    PERFORM (cb_ondropcomplete) IN PROGRAM (repid) IF FOUND USING me             "ZWFT_FALV
          es_row_no      "LVC_S_ROID
          e_column       "LVC_S_COL
          e_dragdropobj  "CL_DRAGDROPOBJECT
          e_row.         "LVC_S_ROW
  ENDMETHOD.


  METHOD evf_ondropgetflavor.
    PERFORM (cb_ondropgetflavor) IN PROGRAM (repid) IF FOUND USING me             "ZWFT_FALV
          es_row_no      "LVC_S_ROID
          e_column       "LVC_S_COL
          e_dragdropobj  "CL_DRAGDROPOBJECT
          e_flavors      "CNDD_FLAVORS
          e_row.         "LVC_S_ROW
  ENDMETHOD.


  METHOD evf_onf1.
    PERFORM (cb_onf1) IN PROGRAM (repid) IF FOUND USING me              "ZWFT_FALV
          e_fieldname     "LVC_FNAME
          es_row_no       "LVC_S_ROID
          er_event_data.  "CL_ALV_EVENT_DATA
  ENDMETHOD.


  METHOD evf_onf4.
    PERFORM (cb_onf4) IN PROGRAM (repid) IF FOUND USING me              "ZWFT_FALV
          e_fieldname     "LVC_FNAME
          e_fieldvalue    "LVC_VALUE
          es_row_no       "LVC_S_ROID
          er_event_data   "CL_ALV_EVENT_DATA
          et_bad_cells    "LVC_T_MODI
          e_display.      "CHAR01
  ENDMETHOD.


  METHOD evf_request_data.
    PERFORM (cb_request_data) IN PROGRAM (repid) IF FOUND USING me         "ZWFT_FALV
          fragments. "String
  ENDMETHOD.


  METHOD evf_subtotal_text.
    PERFORM (cb_subtotal_text) IN PROGRAM (repid) IF FOUND USING me                "ZWFT_FALV
          es_subtottxt_info "LVC_S_STXT
          ep_subtot_line    "DATA
          e_event_data.     "CL_ALV_EVENT_DATA
  ENDMETHOD.


  METHOD evf_toolbar.
    PERFORM (cb_toolbar) IN PROGRAM (repid) IF FOUND USING me            "ZWFT_FALV
          e_object      "CL_ALV_EVENT_TOOLBAR_SET
          e_interactive."CHAR01
  ENDMETHOD.


  METHOD evf_toolbar_button_click.
    PERFORM (cb_toolbar_button_click) IN PROGRAM (repid) IF FOUND USING me     "ZWFT_FALV
          fcode ."C
  ENDMETHOD.


  METHOD evf_toolbar_internal.
    LOOP AT toolbar_added ASSIGNING FIELD-SYMBOL(<tb>).
      INSERT <tb> INTO TABLE e_object->mt_toolbar[].
    ENDLOOP.
    LOOP AT toolbar_disabled ASSIGNING <tb>.
      TRY.
        e_object->mt_toolbar[ FUNCTION = <tb>-FUNCTION ]-disabled = abap_true.
      CATCH cx_sy_itab_line_not_found.
        CLEAR sy-subrc.
      ENDTRY.
    ENDLOOP.
    LOOP AT toolbar_deleted ASSIGNING <tb>.
      DELETE e_object->mt_toolbar WHERE FUNCTION = <tb>-FUNCTION.
    ENDLOOP.

  ENDMETHOD.


  METHOD evf_toolbar_menubutton_click.
    PERFORM (cb_toolbar_menubutton_click) IN PROGRAM (repid) IF FOUND USING me         "ZWFT_FALV
          fcode      "C
          menu_pos_x "I
          menu_pos_y."I
  ENDMETHOD.


  METHOD evf_toolbar_menu_selected.
    PERFORM (cb_toolbar_menu_selected) IN PROGRAM (repid) IF FOUND USING me     "ZWFT_FALV
          fcode ."C
  ENDMETHOD.


  METHOD evf_top_of_page.
    PERFORM (cb_top_of_page) IN PROGRAM (repid) IF FOUND USING me          "ZWFT_FALV
          e_dyndoc_id "CL_DD_DOCUMENT
          table_index."SYINDEX
  ENDMETHOD.


  METHOD evf_total_click_row_col.
    PERFORM (cb_total_click_row_col) IN PROGRAM (repid) IF FOUND USING me     "ZWFT_FALV
          col_id "C
          row_id."C
  ENDMETHOD.


  METHOD evf_user_command.
    DATA: callstack TYPE abap_callstack.
    CASE e_ucomm.
    WHEN fc_back.
      LEAVE TO SCREEN 0.
    WHEN fc_exit.
      LEAVE TO SCREEN 0.
    WHEN fc_up.
      LEAVE TO SCREEN 0.
    WHEN fc_cancel.
      LEAVE TO SCREEN 0.
    WHEN fc_mass_replace.
      mass_replace( ).
    WHEN fc_find.
      e_ucomm = '%SC'.
      set_function_code( CHANGING c_ucomm = e_ucomm ).
    WHEN fc_find_next.
      e_ucomm = '%SC+'.
      set_function_code( CHANGING c_ucomm = e_ucomm ).
    WHEN OTHERS.
      PERFORM (cb_user_command) IN PROGRAM (repid) IF FOUND USING me       "ZCL_SALV
            e_ucomm. "SY-UCOMM
    ENDCASE.
  ENDMETHOD.


  METHOD exclude_function.
    IF NOT line_exists( exclude_functions[ table_line = iv_ucomm ] ).
      INSERT iv_ucomm INTO TABLE exclude_functions.
    ENDIF.
  ENDMETHOD.


  METHOD export_to_excel.


    DATA: version TYPE string.
    DATA: result_data TYPE REF TO cl_salv_ex_result_data_table.
    DATA: columns TYPE REF TO cl_salv_columns_table.
    DATA: aggreg TYPE REF TO cl_salv_aggregations.
    DATA: salv_intf_descr TYPE REF TO cl_abap_objectdescr.
    DATA: salv_table TYPE REF TO cl_salv_table.
    DATA: file_type TYPE salv_bs_constant.
    FIELD-SYMBOLS <tab> TYPE ANY TABLE.


    IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
    cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

      result_data = create_ex_result_falv( ).

      CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        version = if_salv_bs_xml=>version_26.
      ENDCASE.

      "if XLSX is possible then we create it,  if not then MHTML excel file
      salv_intf_descr ?= cl_abap_intfdescr=>describe_by_name( EXPORTING p_name = 'IF_SALV_BS_XML' ).
      IF salv_intf_descr IS NOT INITIAL AND line_exists( salv_intf_descr->attributes[ name = 'C_TYPE_XLSX'  ] ).
        file_type = 10.
      ELSE.
        file_type = 02.
      ENDIF.

      "transformation of data to excel
      CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = file_type
        xml_version   = version
        r_result_data = result_data
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = rv_xstring.
    ENDIF.


  ENDMETHOD.


  METHOD get_cell_enabled.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <styles> TYPE lvc_t_styl,
    <field>  TYPE ANY.
    CHECK i_row IS NOT INITIAL AND i_field IS NOT INITIAL.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).

    " if sy-subrc eq 0. " The value of SY-SUBRC is always set to 0 by CALL METHOD GET_FRONTEND_LAYOUT.
    IF lvc_layout-stylefname IS NOT INITIAL.
      ASSIGN outtab->* TO <outtab>.
      ASSIGN <outtab>[ i_row ] TO FIELD-SYMBOL(<line>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-stylefname OF STRUCTURE <line> TO <styles>.
        IF sy-subrc EQ 0.
          ASSIGN <styles>[ fieldname = i_field ] TO FIELD-SYMBOL(<style>).
          IF sy-subrc EQ 0.
            IF <style>-style = mc_style_enabled.
              r_enabled = abap_true.
              RETURN.
          ELSEIF <style>-style = mc_style_disabled.
              r_enabled = abap_false.
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "all cells editable
    IF lvc_layout-EDIT EQ abap_true.
      r_enabled = abap_true.
    ELSE.
      get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = fcat ).
      ASSIGN fcat[ fieldname = i_field ] TO FIELD-SYMBOL(<fcat>).
      IF <fcat>-EDIT EQ abap_true.
        r_enabled = abap_true.
      ENDIF.
    ENDIF.
    " endif.
  ENDMETHOD.


  METHOD get_columns.
    rt_columns = me->columns.
  ENDMETHOD.


  METHOD get_file_from_mime.
    cl_mime_repository_api=>get_api( )->get(
    EXPORTING i_url = iv_path
    IMPORTING e_content = ev_xstring
      e_mime_type = ev_mime_type
    EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc EQ 0.
  ENDMETHOD.


  METHOD get_picture_from_se78.
    cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp(
    EXPORTING
      p_object       = 'GRAPHICS'    " SAPscript Graphics Management: Application object
      p_name         = iv_name   " Name
      p_id           = iv_id   " SAPscript Graphics Management: ID
      p_btype        = iv_type   " SAPscript: Type of graphic
      RECEIVING
      p_bmp          = rv_xstring   " Graphic Data
    EXCEPTIONS
      not_found      = 0
      internal_error = 0
      OTHERS         = 0 ).
  ENDMETHOD.


  METHOD hide_applog.

    IF main_split_container IS NOT INITIAL.
      main_split_container->set_row_sash(
      EXPORTING
        ID                = 1   " Row Splitter Bar ID
        TYPE              = split_container->type_sashvisible   " Attribute
        VALUE             = 0   " Value
      EXCEPTIONS
        cntl_error        = 0
        cntl_system_error = 0
        OTHERS            = 0 ).
      main_split_container->set_row_height(
      EXPORTING
        ID                = 2 " Row ID
        height            = 0  " Height
      EXCEPTIONS
        cntl_error        = 0
        cntl_system_error = 0
        OTHERS            = 0 ).
      CLEAR splitter_row_3_height.
    ENDIF.
  ENDMETHOD.


  METHOD hide_top_of_page.
    IF split_container IS NOT INITIAL.

      split_container->set_row_sash(
      EXPORTING
        ID                = 1   " Row Splitter Bar ID
        TYPE              = split_container->type_sashvisible   " Attribute
        VALUE             = 0   " Value
      EXCEPTIONS
        cntl_error        = 0
        cntl_system_error = 0
        OTHERS            = 0 ).

      split_container->set_row_height(
      EXPORTING
        ID                = 1 " Row ID
        height            = 0  " Height
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
      IF sy-subrc EQ 0.
        splitter_row_1_height = 0.
      ENDIF.
    ENDIF.
    r_falv = me.
  ENDMETHOD.


  METHOD link_containers.

    iv_falv->main_container ?= i_custom_container.
    iv_falv->split_container = i_split_container.
    iv_falv->main_split_container = i_main_split_container.
    iv_falv->top_of_page_container = i_top_of_page_parent.
    IF i_main_split_container IS NOT INITIAL.
      i_main_split_container->set_row_mode(
      EXPORTING
        MODE              = i_split_container->mode_absolute
      EXCEPTIONS
        cntl_error        = 0
        cntl_system_error = 0
        OTHERS            = 0 ).
      iv_falv->hide_applog( ).
    ENDIF.
    IF i_split_container IS NOT INITIAL.
      i_split_container->set_row_mode(
      EXPORTING
        MODE              = i_split_container->mode_absolute
      EXCEPTIONS
        cntl_error        = 0
        cntl_system_error = 0
        OTHERS            = 0 ).
      iv_falv->hide_top_of_page( ).
    ENDIF.

  ENDMETHOD.


  METHOD lvc_fcat_from_itab.
    DATA: TABLE TYPE REF TO DATA.
    CREATE DATA TABLE LIKE it_table.
    ASSIGN TABLE->* TO FIELD-SYMBOL(<table>).
    TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv_table)
      CHANGING  t_table      = <table> ).
      rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      r_columns      = salv_table->get_columns( ) " ALV Filter
      r_aggregations = salv_table->get_aggregations( ) )." ALV Aggregations
    CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD mass_replace.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'ZWFT_FALV_MASS_REPLACE'
      EXPORTING
        io_grid   = CAST cl_gui_alv_grid( me )
      CHANGING
        ct_outtab = <outtab>.
    ENDIF.
  ENDMETHOD.


  METHOD pai.
    DATA: ucomm TYPE sy-ucomm.
    ucomm = c_ucomm.
    CLEAR c_ucomm.
    RAISE EVENT user_command EXPORTING e_ucomm = ucomm.

    me->set_function_code(
    CHANGING
      c_ucomm = ucomm ).
  ENDMETHOD.


  METHOD pbo.

    gui_status->show_gui_status( ).
    RAISE EVENT at_set_pf_status .

    gui_status->show_title(
    EXPORTING
      iv_text1 = title_v1
      iv_text2 = title_v2
      iv_text3 = title_v3
      iv_text4 = title_v4
      iv_text5 = title_v5 ).
    RAISE EVENT at_set_title.

  ENDMETHOD.


  METHOD raise_top_of_page.

    IF top_of_page_visible_at_start EQ abap_true AND
    top_of_page_container IS NOT INITIAL.

      IF me->top_of_page_doc IS INITIAL.
        top_of_page_doc = NEW cl_dd_document( ).
      ENDIF.

      EXPORT alv_form_html FROM abap_true
      TO MEMORY ID 'ALV_FORM_HTML'.


      CALL METHOD me->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = me->top_of_page_doc.


      EXPORT alv_form_html FROM abap_false
      TO MEMORY ID 'ALV_FORM_HTML'.

      top_of_page_doc->display_document(
      EXPORTING
        reuse_control      = 'X'
        parent             = top_of_page_container
      EXCEPTIONS
        html_display_error = 0
        OTHERS             = 0 ).
      show_top_of_page( ).
    ENDIF.
  ENDMETHOD.


  METHOD refresh_toolbar.
    CHECK cl_gui_alv_grid=>offline( ) IS INITIAL.
    CHECK grid->m_init_toolbar EQ space.
    TRY.
      me->set_toolbar_interactive(  ).
    CATCH cx_root.
      "in case method is called before the display of grid
      "no need to do anything with that
      CLEAR sy-subrc.
    ENDTRY.
    r_falv = me.
  ENDMETHOD.


  METHOD save_excel_localy.
    DATA: path     TYPE string,
          filename TYPE string.

    IF iv_path IS INITIAL.
      cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
*         window_title         = window_title
        default_extension    = 'XLSX'
*         default_file_name    = default_file_name
*         with_encoding        = with_encoding
*         file_filter          = file_filter
*         initial_directory    = initial_directory
*         prompt_on_overwrite  = 'X'
      CHANGING
        filename             = filename
        path                 = path
        fullpath             = iv_path
*         user_action          = user_action
*         file_encoding        = file_encoding
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    IF iv_path IS NOT INITIAL.

      DATA(xstring) = me->export_to_excel( ).
      DATA(xstrsize) = XSTRLEN( xstring ).
      DATA(solix) = cl_bcs_convert=>xstring_to_solix( xstring  ).

      cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize            = xstrsize
        filename                = iv_path
        filetype                = 'BIN'
      CHANGING
        data_tab                = solix
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24
        ).
      IF sy-subrc EQ 0.
        rv_saved = abap_true.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD send.
    r_falv = me.
    TRY.
      DATA(request) = cl_bcs=>create_persistent( ).
    CATCH cx_send_req_bcs.
      RAISE create_request_error.
    ENDTRY.

    TRY.
      DATA(document) = cl_document_bcs=>create_document(
            i_type = 'HTM'
            i_text = cl_document_bcs=>string_to_soli( iv_body )
            i_subject = CONV #( iv_subject )
            i_importance = iv_importance
            i_sensitivity = iv_sensitivity ).


    CATCH cx_document_bcs.
      RAISE create_document_error.
    ENDTRY.

    DATA(excel) = export_to_excel( ).

    TRY.
      document->add_attachment(
      i_attachment_type    = 'EXT'
      i_att_content_hex    = cl_document_bcs=>xstring_to_solix( excel )
      i_attachment_size    = CONV #( XSTRLEN( excel ) )
      i_attachment_subject = COND #( WHEN iv_filename IS NOT INITIAL THEN iv_filename
      ELSE CONV #( |{ sy-datum }_{ sy-uzeit }.xlsx | )
      ) ).
    CATCH cx_document_bcs .
      RAISE add_attachment_error.
    ENDTRY.


    TRY.

      request->set_document( document ).
    CATCH cx_send_req_bcs.
      RAISE add_document_error.
    ENDTRY.

    IF it_recipients IS NOT INITIAL.
      LOOP AT it_recipients ASSIGNING FIELD-SYMBOL(<addr>).
        TRY.
          DATA(smtp_addr) = cl_cam_address_bcs=>create_internet_address( <addr>-smtp_addr ).
        CATCH cx_address_bcs.
          RAISE add_recipient_error.
        ENDTRY.

        TRY.
          request->add_recipient( i_recipient  = smtp_addr
          i_express    = <addr>-express
          i_copy       = <addr>-COPY
          i_blind_copy = <addr>-blind_copy
          ).

        CATCH cx_send_req_bcs.
          RAISE add_recipient_error.
        ENDTRY.
      ENDLOOP.

      IF iv_sender IS NOT INITIAL.
        TRY.
          DATA(sender) = cl_cam_address_bcs=>create_internet_address( i_address_string = iv_sender
                i_address_name   = iv_sender_name ).
        CATCH cx_address_bcs.
          RAISE add_sender_error.
        ENDTRY.

        TRY.
          request->set_sender( i_sender = sender ).
        CATCH cx_send_req_bcs.
          RAISE add_sender_error.
        ENDTRY.
      ENDIF.

      TRY.
        request->set_message_subject( ip_subject = iv_subject ).
      CATCH cx_root.
      ENDTRY.


      IF iv_immediately EQ abap_true.
        TRY .
*     set send immediately
          request->set_send_immediately( abap_true ).
        CATCH cx_send_req_bcs.
          RAISE send_immediately_error.
        ENDTRY.

      ENDIF.
      TRY.
        DATA(result) = request->send(  ).
        IF iv_commit EQ abap_true.
          COMMIT WORK.
        ENDIF.
      CATCH cx_send_req_bcs.
        RAISE send_error.
      ENDTRY.
    ELSE.
      RAISE add_recipient_error.
    ENDIF.
  ENDMETHOD.


  METHOD set_cell_button.
    r_falv = me.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <styles> TYPE lvc_t_styl.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).
    CHECK lvc_layout-stylefname IS NOT INITIAL.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      ASSIGN <outtab>[ iv_row ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-stylefname OF STRUCTURE <row> TO <styles>.
        IF sy-subrc EQ 0.
          TRY.
            <styles>[ fieldname = iv_fieldname ]-style = mc_style_button.
          CATCH cx_sy_itab_line_not_found.
            INSERT VALUE #( fieldname = iv_fieldname style = mc_style_button ) INTO TABLE <styles>.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_cell_color.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <colors> TYPE lvc_t_scol.
    r_falv = me.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).
    CHECK lvc_layout-ctab_fname IS NOT INITIAL.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      ASSIGN <outtab>[ iv_row ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-ctab_fname OF STRUCTURE <row> TO <colors>.
        IF sy-subrc EQ 0.
          TRY.
            <colors>[ fname = iv_fieldname ]-COLOR = iv_color.
          CATCH cx_sy_itab_line_not_found.
            INSERT VALUE #( fname = iv_fieldname COLOR = iv_color ) INTO TABLE <colors>.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_cell_disabled.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <styles> TYPE lvc_t_styl.
    r_falv = me.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).
    CHECK lvc_layout-stylefname IS NOT INITIAL.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      ASSIGN <outtab>[ iv_row ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-stylefname OF STRUCTURE <row> TO <styles>.
        IF sy-subrc EQ 0.
          TRY.
            <styles>[ fieldname = iv_fieldname ]-style = mc_style_disabled.
          CATCH cx_sy_itab_line_not_found.
            INSERT VALUE #( fieldname = iv_fieldname style = mc_style_disabled ) INTO TABLE <styles>.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_cell_enabled.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <styles> TYPE lvc_t_styl.
    r_falv = me.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).
    CHECK lvc_layout-stylefname IS NOT INITIAL.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      ASSIGN <outtab>[ iv_row ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-stylefname OF STRUCTURE <row> TO <styles>.
        IF sy-subrc EQ 0.
          TRY.
            <styles>[ fieldname = iv_fieldname ]-style = mc_style_enabled.
          CATCH cx_sy_itab_line_not_found.
            INSERT VALUE #( fieldname = iv_fieldname style = mc_style_enabled ) INTO TABLE <styles>.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_cell_hotspot.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <styles> TYPE lvc_t_styl.
    r_falv = me.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).
    CHECK lvc_layout-stylefname IS NOT INITIAL.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      ASSIGN <outtab>[ iv_row ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-stylefname OF STRUCTURE <row> TO <styles>.
        IF sy-subrc EQ 0.
          TRY.
            <styles>[ fieldname = iv_fieldname ]-style = mc_style_hotspot.
          CATCH cx_sy_itab_line_not_found.
            INSERT VALUE #( fieldname = iv_fieldname style = mc_style_hotspot ) INTO TABLE <styles>.
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_dummy_function_code.
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = 'DUMMY'
    EXCEPTIONS
      function_not_supported = 0
      OTHERS                 = 0.
    r_falv = me.
  ENDMETHOD.


  METHOD set_editable.
    r_falv = me.
    CHECK cl_gui_alv_grid=>offline( ) IS INITIAL.
    me->set_ready_for_input( i_ready_for_input = 1 ).
    IF iv_modify EQ abap_true.
      me->register_edit_event(
      EXPORTING
        i_event_id = me->mc_evt_modified
      EXCEPTIONS
        error      = 0
        OTHERS     = 0 ).
    ELSE.
      me->register_edit_event(
      EXPORTING
        i_event_id = me->mc_evt_enter
      EXCEPTIONS
        error      = 0
        OTHERS     = 0 ).
    ENDIF.
  ENDMETHOD.


  METHOD set_events.
    DATA: ls_events TYPE slis_alv_event.
    LOOP AT iv_falv->mt_events INTO ls_events WHERE NOT FORM IS INITIAL.
      CASE ls_events-name.
      WHEN EVENTS-after_refresh.
        iv_falv->cb_after_refresh  = ls_events-FORM.
      WHEN EVENTS-after_user_command.
        iv_falv->cb_after_user_command = ls_events-FORM.
      WHEN EVENTS-before_user_command.
        iv_falv->cb_before_user_command = ls_events-FORM.
      WHEN EVENTS-btn_click.
        iv_falv->cb_btn_click   = ls_events-FORM.
      WHEN EVENTS-data_changed.
        iv_falv->cb_data_changed = ls_events-FORM.
      WHEN EVENTS-data_changed_internal.
        iv_falv->cb_data_changed_internal = ls_events-FORM.
      WHEN EVENTS-data_changed_finished.
        iv_falv->cb_data_changed_finished = ls_events-FORM.
      WHEN EVENTS-double_click.
        iv_falv->cb_double_click = ls_events-FORM.
      WHEN EVENTS-hotspot_click.
        iv_falv->cb_hotspot_click = ls_events-FORM.
      WHEN EVENTS-menu_button.
        iv_falv->cb_menu_button = ls_events-FORM.
      WHEN EVENTS-onf1.
        iv_falv->cb_onf1 = ls_events-FORM.
      WHEN EVENTS-onf4.
        iv_falv->cb_onf4 = ls_events-FORM.
      WHEN EVENTS-subtotal_text.
        iv_falv->cb_subtotal_text = ls_events-FORM.
      WHEN EVENTS-toolbar.
        iv_falv->cb_toolbar = ls_events-FORM.
      WHEN EVENTS-user_command.
        iv_falv->cb_user_command = ls_events-FORM.
      WHEN EVENTS-at_set_pf_status.
        iv_falv->cb_at_set_pf_status = ls_events-FORM.
      WHEN EVENTS-at_set_title.
        iv_falv->cb_at_set_title = ls_events-FORM.
      WHEN EVENTS-top_of_page.
        iv_falv->cb_top_of_page = ls_events-FORM.
      WHEN EVENTS-delayed_callback.
        iv_falv->cb_delayed_callback = ls_events-FORM.
      WHEN EVENTS-delayed_changed_sel_call.
        iv_falv->cb_delayed_changed_sel_call = ls_events-FORM.
      WHEN EVENTS-ondrag.
        iv_falv->cb_ondrag = ls_events-FORM.
      WHEN EVENTS-ondrop.
        iv_falv->cb_ondrop = ls_events-FORM.
      WHEN EVENTS-ondropcomplete.
        iv_falv->cb_ondropcomplete = ls_events-FORM.
      WHEN EVENTS-ondropgetflavor.
        cb_ondropgetflavor = ls_events-FORM.
      WHEN EVENTS-drop_external_file.
        iv_falv->cb_drop_external_file = ls_events-FORM.
      WHEN EVENTS-toolbar_menubutton_click.
        iv_falv->cb_toolbar_menubutton_click = ls_events-FORM.
      WHEN EVENTS-click_col_header.
        iv_falv->cb_click_col_header = ls_events-FORM.
      WHEN EVENTS-delayed_move_current_cell.
        iv_falv->cb_delayed_move_current_cell = ls_events-FORM.
      WHEN EVENTS-f1.
        iv_falv->cb_f1 = ls_events-FORM.
      WHEN EVENTS-dblclick_row_col.
        iv_falv->cb_dblclick_row_col = ls_events-FORM.
      WHEN EVENTS-click_row_col.
        iv_falv->cb_click_row_col = ls_events-FORM.
      WHEN EVENTS-toolbar_button_click.
        iv_falv->cb_toolbar_button_click = ls_events-FORM.
      WHEN EVENTS-double_click_col_separator.
        iv_falv->cb_double_click_col_separator = ls_events-FORM.
      WHEN EVENTS-delayed_change_selection.
        iv_falv->cb_delayed_change_selection = ls_events-FORM.
      WHEN EVENTS-context_menu.
        iv_falv->cb_context_menu = ls_events-FORM.
      WHEN EVENTS-total_click_row_col.
        iv_falv->cb_total_click_row_col = ls_events-FORM.
      WHEN EVENTS-context_menu_selected.
        iv_falv->cb_context_menu_selected = ls_events-FORM.
      WHEN EVENTS-context_menu_request.
        iv_falv->cb_context_menu_request = ls_events-FORM.
      WHEN EVENTS-toolbar_menu_selected.
        iv_falv->cb_toolbar_menu_selected = ls_events-FORM.
      WHEN EVENTS-request_data.
        iv_falv->cb_request_data = ls_events-FORM.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_events_suffix.
    CHECK  iv_falv->suffix IS NOT INITIAL.
    iv_falv->cb_after_refresh = iv_falv->cb_after_refresh && iv_falv->suffix.
    iv_falv->cb_after_user_command = iv_falv->cb_after_user_command && iv_falv->suffix.
    iv_falv->cb_before_user_command = iv_falv->cb_before_user_command && iv_falv->suffix.
    iv_falv->cb_btn_click = iv_falv->cb_btn_click && iv_falv->suffix.
    iv_falv->cb_data_changed = iv_falv->cb_data_changed && iv_falv->suffix.
    iv_falv->cb_data_changed_internal = iv_falv->cb_data_changed_internal && iv_falv->suffix.
    iv_falv->cb_data_changed_finished = iv_falv->cb_data_changed_finished && iv_falv->suffix.
    iv_falv->cb_double_click = iv_falv->cb_double_click && iv_falv->suffix.
    iv_falv->cb_hotspot_click = iv_falv->cb_hotspot_click && iv_falv->suffix.
    iv_falv->cb_menu_button = iv_falv->cb_menu_button && iv_falv->suffix.
    iv_falv->cb_onf1 = iv_falv->cb_onf1 && iv_falv->suffix.
    iv_falv->cb_onf4 = iv_falv->cb_onf4 && iv_falv->suffix.
    iv_falv->cb_subtotal_text = iv_falv->cb_subtotal_text && iv_falv->suffix.
    iv_falv->cb_toolbar = iv_falv->cb_toolbar && iv_falv->suffix.
    iv_falv->cb_user_command = iv_falv->cb_user_command && iv_falv->suffix.
    iv_falv->cb_at_set_pf_status = iv_falv->cb_at_set_pf_status && iv_falv->suffix.
    iv_falv->cb_at_set_title = iv_falv->cb_at_set_title && iv_falv->suffix.
    iv_falv->cb_top_of_page = iv_falv->cb_top_of_page && iv_falv->suffix.
    iv_falv->cb_delayed_callback = iv_falv->cb_delayed_callback && iv_falv->suffix.
    iv_falv->cb_delayed_changed_sel_call = iv_falv->cb_delayed_changed_sel_call && iv_falv->suffix.
    iv_falv->cb_ondrag = iv_falv->cb_ondrag && iv_falv->suffix.
    iv_falv->cb_ondrop = iv_falv->cb_ondrop && iv_falv->suffix.
    iv_falv->cb_ondropcomplete = iv_falv->cb_ondropcomplete && iv_falv->suffix.
    iv_falv->cb_ondropgetflavor = iv_falv->cb_ondropgetflavor && iv_falv->suffix.
    iv_falv->cb_drop_external_file = iv_falv->cb_drop_external_file && iv_falv->suffix.
    iv_falv->cb_toolbar_menubutton_click = iv_falv->cb_toolbar_menubutton_click && iv_falv->suffix.
    iv_falv->cb_click_col_header = iv_falv->cb_click_col_header && iv_falv->suffix.
    iv_falv->cb_delayed_move_current_cell = iv_falv->cb_delayed_move_current_cell && iv_falv->suffix.
    iv_falv->cb_f1 = iv_falv->cb_f1 && iv_falv->suffix.
    iv_falv->cb_dblclick_row_col = iv_falv->cb_dblclick_row_col && iv_falv->suffix.
    iv_falv->cb_click_row_col = iv_falv->cb_click_row_col && iv_falv->suffix.
    iv_falv->cb_toolbar_button_click = iv_falv->cb_toolbar_button_click && iv_falv->suffix.
    iv_falv->cb_double_click_col_separator = iv_falv->cb_double_click_col_separator && iv_falv->suffix.
    iv_falv->cb_delayed_change_selection = iv_falv->cb_delayed_change_selection && iv_falv->suffix.
    iv_falv->cb_context_menu = iv_falv->cb_context_menu && iv_falv->suffix.
    iv_falv->cb_total_click_row_col = iv_falv->cb_total_click_row_col && iv_falv->suffix.
    iv_falv->cb_context_menu_selected = iv_falv->cb_context_menu_selected && iv_falv->suffix.
    iv_falv->cb_context_menu_request = iv_falv->cb_context_menu_request && iv_falv->suffix.
    iv_falv->cb_toolbar_menu_selected = iv_falv->cb_toolbar_menu_selected && iv_falv->suffix.
    iv_falv->cb_request_data = iv_falv->cb_request_data && iv_falv->suffix.

  ENDMETHOD.


  METHOD set_frontend_fieldcatalog.
    fcat = it_fieldcatalog.
    super->set_frontend_fieldcatalog( fcat ).
  ENDMETHOD.


  METHOD set_frontend_layout.
    lvc_layout = is_layout.
    super->set_frontend_layout( is_layout ).
  ENDMETHOD.


  METHOD set_handlers.

    SET HANDLER iv_falv->evf_after_refresh FOR iv_falv.
    SET HANDLER iv_falv->evf_after_user_command FOR iv_falv.
    SET HANDLER iv_falv->evf_before_ucommand_internal FOR iv_falv.
    SET HANDLER iv_falv->evf_before_user_command FOR iv_falv.
    SET HANDLER iv_falv->evf_btn_click FOR iv_falv.
    SET HANDLER iv_falv->evf_data_changed FOR iv_falv.
    SET HANDLER iv_falv->evf_data_changed_internal FOR iv_falv.
    SET HANDLER iv_falv->evf_data_changed_finished FOR iv_falv.
    SET HANDLER iv_falv->evf_double_click FOR iv_falv.
    SET HANDLER iv_falv->evf_hotspot_click FOR iv_falv.
    SET HANDLER iv_falv->evf_menu_button FOR iv_falv.
    SET HANDLER iv_falv->evf_onf1 FOR iv_falv.
    SET HANDLER iv_falv->evf_onf4 FOR iv_falv.
    SET HANDLER iv_falv->evf_subtotal_text FOR iv_falv.
    SET HANDLER iv_falv->evf_toolbar_internal FOR iv_falv.
    SET HANDLER iv_falv->evf_toolbar FOR iv_falv.
    SET HANDLER iv_falv->evf_user_command FOR iv_falv.
    SET HANDLER iv_falv->evf_at_set_pf_status FOR iv_falv.
    SET HANDLER iv_falv->evf_at_set_title FOR iv_falv.
    SET HANDLER iv_falv->evf_top_of_page FOR iv_falv.
    SET HANDLER iv_falv->evf_delayed_callback FOR iv_falv.
    SET HANDLER iv_falv->evf_delayed_changed_sel_call FOR iv_falv.
    SET HANDLER iv_falv->evf_ondrag FOR iv_falv.
    SET HANDLER iv_falv->evf_ondrop FOR iv_falv.
    SET HANDLER iv_falv->evf_ondropcomplete FOR iv_falv.
    SET HANDLER iv_falv->evf_ondropgetflavor FOR iv_falv.
    SET HANDLER iv_falv->evf_drop_external_file FOR iv_falv.
    SET HANDLER iv_falv->evf_toolbar_menubutton_click FOR iv_falv.
    SET HANDLER iv_falv->evf_click_col_header FOR iv_falv.
    SET HANDLER iv_falv->evf_delayed_move_current_cell FOR iv_falv.
    SET HANDLER iv_falv->evf_f1 FOR iv_falv.
    SET HANDLER iv_falv->evf_dblclick_row_col FOR iv_falv.
    SET HANDLER iv_falv->evf_click_row_col FOR iv_falv.
    SET HANDLER iv_falv->evf_toolbar_button_click FOR iv_falv.
    SET HANDLER iv_falv->evf_double_click_col_separator FOR iv_falv.
    SET HANDLER iv_falv->evf_delayed_change_selection FOR iv_falv.
    SET HANDLER iv_falv->evf_context_menu FOR iv_falv.
    SET HANDLER iv_falv->evf_total_click_row_col FOR iv_falv.
    SET HANDLER iv_falv->evf_context_menu_selected FOR iv_falv.
    SET HANDLER iv_falv->evf_context_menu_request FOR iv_falv.
    SET HANDLER iv_falv->evf_toolbar_menu_selected FOR iv_falv.
    SET HANDLER iv_falv->evf_request_data FOR iv_falv.

    iv_falv->set_delay_change_selection(
    EXPORTING
      TIME   = iv_falv->delay_change_selection
    EXCEPTIONS
      error  = 0
      OTHERS = 0 ).

    iv_falv->set_delay_move_current_cell(
    EXPORTING
      TIME   = iv_falv->delay_move_current_cell
    EXCEPTIONS
      error  = 0
      OTHERS = 0 ).

  ENDMETHOD.


  METHOD set_list_view.
    m_batch_mode = abap_true.
    r_falv = me.
  ENDMETHOD.


  METHOD set_mark_field.
    IF line_exists( fcat[ fieldname = iv_fieldname ] ).
      layout->mark_field = iv_fieldname.
      CHECK iv_fieldname IS NOT INITIAL.
      column( iv_fieldname )->set_checkbox( abap_true ).
    ENDIF.
  ENDMETHOD.


  METHOD set_output_table.
    GET REFERENCE OF ct_table INTO outtab.
  ENDMETHOD.


  METHOD set_parent.
    me->parent ?= io_parent.
    r_falv = me.
  ENDMETHOD.


  METHOD set_readonly.
    me->set_ready_for_input( i_ready_for_input = 0 ).
    r_falv = me.
  ENDMETHOD.


  METHOD set_row_color.
    FIELD-SYMBOLS: <outtab> TYPE STANDARD TABLE,
    <color>  TYPE char04.
    r_falv = me.
    get_frontend_layout( IMPORTING es_layout = lvc_layout ).
    CHECK lvc_layout-info_fname IS NOT INITIAL.
    ASSIGN outtab->* TO <outtab>.
    IF sy-subrc EQ 0.
      ASSIGN <outtab>[ iv_row ] TO FIELD-SYMBOL(<row>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT lvc_layout-info_fname OF STRUCTURE <row> TO <color>.
        IF sy-subrc EQ 0.
          <color> = iv_color.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD show_applog.

    r_falv = me.
    IF main_split_container IS NOT INITIAL.
      main_split_container->get_row_height(
      EXPORTING
        ID                = 2
      IMPORTING
        result            = splitter_row_2_height
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
      IF sy-subrc EQ 0 AND splitter_row_2_height EQ 0.
        main_split_container->set_row_sash(
        EXPORTING
          ID                = 1   " Row Splitter Bar ID
          TYPE              = split_container->type_sashvisible   " Attribute
          VALUE             = 1   " Value
        EXCEPTIONS
          cntl_error        = 0
          cntl_system_error = 0
          OTHERS            = 0 ).
        main_split_container->get_row_height(
        EXPORTING
          ID                = 1   " ALV
        IMPORTING
          result            = splitter_row_2_height
        EXCEPTIONS
          cntl_error        = 0
          cntl_system_error = 0
          OTHERS            = 0 ).

        main_split_container->get_row_height(
        EXPORTING
          ID                = 2   " Row ID
        IMPORTING
          result            = splitter_row_3_height   " Result Code
        EXCEPTIONS
          cntl_error        = 0
          cntl_system_error = 0
          OTHERS            = 0 ).

        main_split_container->set_row_height(
        EXPORTING
          ID                = 2 "error log
          height            = error_log_height
        IMPORTING
          result            = result
        EXCEPTIONS
          cntl_error        = 0
          cntl_system_error = 0
          OTHERS            = 0 ).

        TRY.
          main_split_container->set_row_height(
          EXPORTING
            ID                = 1 " ALV
            height            = CONV #( splitter_row_2_height - error_log_height )
          IMPORTING
            result            = result
          EXCEPTIONS
            cntl_error        = 0
            cntl_system_error = 0
            OTHERS            = 0 ).
        CATCH cx_root.
        ENDTRY.
      ENDIF.
      CLEAR splitter_row_2_height.
      CLEAR splitter_row_3_height.
    ENDIF.
  ENDMETHOD.


  METHOD show_top_of_page.
    r_falv = me.
    IF split_container IS NOT INITIAL.
      split_container->set_row_sash(
      EXPORTING
        ID                = 1   " Row Splitter Bar ID
        TYPE              = split_container->type_sashvisible   " Attribute
        VALUE             = 1   " Value
      EXCEPTIONS
        cntl_error        = 0
        cntl_system_error = 0
        OTHERS            = 0 ).

      split_container->get_row_height(
      EXPORTING
        ID                = 1   " Row ID
      IMPORTING
        result            = splitter_row_1_height   " Result Code
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).
      IF sy-subrc EQ 0 AND splitter_row_1_height EQ 0.
        split_container->set_row_height(
        EXPORTING
          ID                = 1 " Row ID
          height            = top_of_page_height
        IMPORTING
          result            = result
        EXCEPTIONS
          cntl_error        = 0
          cntl_system_error = 0
          OTHERS            = 0 ).
      ENDIF.
      top_of_page_visible_at_start = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD soft_refresh.
    me->refresh_table_display(
    EXPORTING
      is_stable      = CONV #( 'XX' ) " With Stable Rows/Columns
      i_soft_refresh = abap_true " Without Sort, Filter, etc.
    EXCEPTIONS
      finished       = 0
      OTHERS         = 0 ).
    r_falv = me.
  ENDMETHOD.
ENDCLASS.

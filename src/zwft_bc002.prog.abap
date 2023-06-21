*&---------------------------------------------------------------------*
*& 程序名称/Program Name         : ZBCC002
*& 程序描述/Program Des.         : 用户角色批量导入
*& 所属模块/Module Belongs       : BC
*& 开发单位/Development Company  : PBIT
*& 作者/Author                   : P149643 / 程成
*& 开发日期/Develop Date         : 2022.08.15
*&---------------------------------------------------------------------*
*&摘要：
*&   一. 业务背景
*&      1).根据模板在系统中批量分配用户角色；
*&   二. 使用场景
*&       1).发布导入模板;
*&       2).使用程序批量创建用户；
*&   三. 程序使用：
*&      1).下载主数据导入模板；
*&         SMWO 模板上载首先设置文件类型：
*&         1>.smwo --> WebRFC应用程序的二进制数据；
*&         2>.设置 --> 定义MIME类型;type：excel；
*&              description：*.xls,*.xlsx
*&         3>.上传excle(.xls)文件ZHAND_STD_SD_001
*&      2).上载主数据EXCEL文件，ALV显示；
*&         注意：此处可以检查导入文件和SAP系统数据是否一致；
*&               数据量较少时可以直接执行导入按钮前台导入，导入过程进度
*&               条显示（进度扇面显示，当前条目/总条目，预计剩余时间）；
*&      3).使用前提；
*&         先在系统中创建有相应的用户
*&---------------------------------------------------------------------*
*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2022-08-15  P149643 / 程成      S4DK900060  初始开发
*&---------------------------------------------------------------------*
REPORT zwft_bc002.

************************************************************************
* Type Declaration
************************************************************************
TYPES:BEGIN OF ty_data,
        username    TYPE bapibname-bapibname,
        agr_name    TYPE bapiagr-agr_name,
        from_dat(8),  "   TYPE bapiagr-from_dat,
        to_dat(8),    "   TYPE bapiagr-to_dat,
        result(80),
      END OF ty_data.

************************************************************************
* Internal Table & WorkArea
************************************************************************

DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data.

DATA: gt_return TYPE TABLE OF bapiret2,
      gs_return TYPE bapiret2.

DATA: gt_bapiagr TYPE TABLE OF bapiagr.

DATA: gs_bapiagr  TYPE bapiagr,
      gs_bapiname TYPE bapibname.

FIELD-SYMBOLS: <fs_data> TYPE ty_data.

************************************************************************
* Global Variant
************************************************************************

*&---------------------------------------------------------------------*
*&      ALV Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.

DATA: gt_fieldcat      TYPE lvc_t_fcat,
      gs_fieldcat      LIKE LINE OF gt_fieldcat,
      gt_sort          TYPE lvc_t_sort,
      gs_sort          TYPE lvc_s_sort,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      gs_grid_settings TYPE lvc_s_glay,
      gt_events        TYPE slis_t_event, "保存AVL事件
      gs_events        LIKE LINE OF gt_events.

DATA: gv_repid LIKE sy-repid.  "SY-REPID 指 当前的主程序
DATA: gr_alvgrid TYPE REF TO cl_gui_alv_grid.

DATA: gt_exclude TYPE slis_t_extab,
      gs_exclude TYPE slis_extab.

DATA: gv_grid_title TYPE lvc_title.

************************************************************************
* Constant
************************************************************************
CONSTANTS: gc_txtu      TYPE string VALUE 'TXT',
           gc_txtl      TYPE string VALUE 'txt',
           gc_xlsxu     TYPE string VALUE 'XLSX',
           gc_xlsxl     TYPE string VALUE 'xlsx',
           gc_separator TYPE c VALUE '\',
           gc_a         TYPE c VALUE 'A',
           gc_objid     TYPE wwwdatatab-objid VALUE 'ZBCC002',
           gc_relid     TYPE w3_relid VALUE 'MI'.


************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_fname TYPE string." OBLIGATORY.

  " 上传下载选择
  PARAMETERS: rb_up   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND comm,
              rb_down RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK blk1.

*&---------------------------------------------------------------------*
*&  Include           ZUSER_CREATEI01
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  " 根据动作不同，选择不同函数
  IF rb_up = abap_true.
    PERFORM frm_get_filename CHANGING p_fname.
  ELSEIF rb_down = abap_true.
    PERFORM frm_set_save_path CHANGING p_fname.
  ENDIF.


START-OF-SELECTION.
  IF p_fname = ''.
    " 定位光标到路径输入框
    SET CURSOR FIELD 'P_FNAME'.
    MESSAGE TEXT-t01 TYPE 'S' DISPLAY LIKE 'W'.
    STOP.
  ENDIF.

  IF rb_up = abap_true.
    " 文件类型判断(lw_str) {
    DATA: lt_str TYPE TABLE OF string,
          l_type TYPE string,
          l_line TYPE i.
    SPLIT p_fname AT '.' INTO TABLE lt_str.
    IF sy-subrc <> 0.
      MESSAGE TEXT-t02 TYPE 'S' DISPLAY LIKE 'W'.
      STOP.
    ENDIF.

    l_line = lines( lt_str ).
    READ TABLE lt_str INTO l_type INDEX l_line.

    " }
    CASE l_type.
      WHEN gc_txtu OR gc_txtl.
        PERFORM frm_txt_upload TABLES gt_data
                               USING p_fname.
      WHEN gc_xlsxu OR gc_xlsxl.
        PERFORM frm_excel_upload TABLES gt_data
                                 USING p_fname.
      WHEN OTHERS.
        MESSAGE TEXT-t03 TYPE 'S' DISPLAY LIKE 'W'.
        STOP.
    ENDCASE.

    " 删除表头行
    "DELETE gt_data INDEX 1.

    PERFORM frm_assignrole.
    PERFORM frm_display.
  ELSEIF rb_down = abap_true.
    PERFORM frm_download_template.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  frm_get_filename
*&---------------------------------------------------------------------*
*       获取文件名称
*   除静态函数（cl_gui_frontend_services=>file_open_dialog）外
*   常用还有四种function module：F4_FILENAME
*                                KD_GET_FILENAME_ON_F4
*                                TB_LIMIT_WS_FILENAME_GET
*                                WS_FILENAME_GET
*   不过这四种函数有不少是封装上述静态函数
*   最为重要的是，静态函数可操作空间巨大，因此这里使用静态函数
*----------------------------------------------------------------------*
*      -->P_FNAME    文件名称存储参数
*----------------------------------------------------------------------*
FORM frm_get_filename CHANGING pv_fname TYPE string.
  DATA: lv_rc        TYPE i,
        lv_title     TYPE string,
        lv_filter    TYPE string,
        lt_filetable TYPE filetable,
        ls_filetable TYPE file_table.

  " 标题
  lv_title = TEXT-t04.

* 打开txt文件
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_title
*     file_filter             = lv_filter
      multiselection          = space
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0 AND lv_rc = 1.
    READ TABLE lt_filetable INTO ls_filetable INDEX 1.
    IF sy-subrc = 0.
      " 这里提取出来，只提供接口，不与全局变量关联
      pv_fname = ls_filetable-filename.
    ENDIF.
  ENDIF.
ENDFORM.                    "frm_get_filename
*&---------------------------------------------------------------------*
*&      Form  frm_txt_upload
*&---------------------------------------------------------------------*
*       获取文本文件数据
*
*----------------------------------------------------------------------*
*      -->PT_DATA    text
*      -->PV_FILE    text
*----------------------------------------------------------------------*
FORM frm_txt_upload TABLES pt_data
                    USING pv_fname TYPE string.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = pv_fname
      has_field_separator     = cl_abap_char_utilities=>horizontal_tab  "以制表符为间隔，每个间隔为一个单元格
    TABLES
      data_tab                = pt_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF lines( pt_data ) = 0.
    " 具体提示哪个文本无数据
    DATA: lt_path TYPE TABLE OF string,
          lw_path TYPE string,
          l_line  TYPE i.
    " 不取具体路径，仅提示文本名称，可随情况修改
    SPLIT pv_fname AT gc_separator INTO TABLE lt_path.
    l_line = lines( lt_path ).
    READ TABLE lt_path INTO lw_path INDEX l_line.
    IF sy-subrc = 0.
      DATA: l_msg TYPE string.
      CONCATENATE lw_path TEXT-t05 INTO l_msg.
      MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    "frm_txt_upload
*&---------------------------------------------------------------------*
*&      Form  frm_excel_upload
*&---------------------------------------------------------------------*
*
*       获取Excel文件数据
*
*----------------------------------------------------------------------*
*      -->PT_EXCEL_DATA  Excel文件数据记录表
*      -->PV_FNAME  文件名
*----------------------------------------------------------------------*
FORM frm_excel_upload TABLES pt_excel_data
                       USING pv_fname.
  DATA: lt_raw TYPE truxs_t_text_data.
  DATA: lv_fn TYPE rlgrap-filename.
  DATA: lt_path TYPE TABLE OF string,
        ls_path TYPE string,
        lv_line TYPE i.
  DATA: lt_return TYPE bapiret2_t.

  lv_fn = pv_fname.

  CALL FUNCTION 'ZFM_ALSM_EXCEL_TO_INTERNAL_TAB'
    EXPORTING
      filename                = lv_fn
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 30
      i_end_row               = 10000
    TABLES
      intern                  = pt_excel_data
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  IF lines( pt_excel_data ) = 0.
    " 具体提示哪个文本无数据
    " 不取具体路径，仅提示文本名称，可随情况修改
    SPLIT pv_fname AT gc_separator INTO TABLE lt_path.
    lv_line = lines( lt_path ).
    READ TABLE lt_path INTO ls_path INDEX lv_line.
    IF sy-subrc = 0.
      DATA: l_msg TYPE string.
      CONCATENATE ls_path TEXT-t05 INTO l_msg.
      MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    " frm_excel_upload
*&---------------------------------------------------------------------*
*&      Form  frm_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_display .
  PERFORM frm_layout.
  PERFORM frm_fieldcat.
  PERFORM frm_output TABLES gt_fieldcat              "输出
                            gt_sort
                            gt_data
                     USING '' " 'ALV_PF_STATUS'
                           '' " 'ALV_USER_COMMAND'
                           gs_layout
                           gs_variant
                           gs_grid_settings.


ENDFORM.                    " frm_display
*&---------------------------------------------------------------------*
*&      Form  frm_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_layout .
  gs_layout-zebra = abap_true.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-sel_mode = gc_a.
ENDFORM.                    " frm_layout
*&---------------------------------------------------------------------*
*&      Form  FRM_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_fieldcat .
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'USERNAME'.
  gs_fieldcat-coltext = TEXT-f01.
  gs_fieldcat-key       =  abap_true.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'AGR_NAME'.
  gs_fieldcat-coltext = TEXT-f02.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'FROM_DAT'.
  gs_fieldcat-coltext = TEXT-f03.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'TO_DAT'.
  gs_fieldcat-coltext = TEXT-f04.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RESULT'.
  gs_fieldcat-coltext = TEXT-f05.
  APPEND gs_fieldcat TO gt_fieldcat.                 "END OF UPDATE.

ENDFORM.                    " FRM_FIELDS
FORM frm_output TABLES pt_lvc TYPE lvc_t_fcat
                       pt_sort TYPE lvc_t_sort
                       pt_data
                USING pv_status
                      pv_ucomm
                      ps_layout TYPE lvc_s_layo
                      ps_variant TYPE disvariant
                      ps_grid_settings TYPE lvc_s_glay.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = pv_status
      i_callback_user_command  = pv_ucomm
      i_grid_title             = gv_grid_title
      i_grid_settings          = ps_grid_settings
      is_layout_lvc            = ps_layout
      it_fieldcat_lvc          = pt_lvc[]
      it_excluding             = gt_exclude
      it_sort_lvc              = pt_sort[]
      i_save                   = 'A'
      is_variant               = ps_variant
      it_events                = gt_events
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_download_template .
  DATA: lv_objdata     LIKE wwwdatatab,
        lv_mime        LIKE w3mime,
        lv_destination LIKE rlgrap-filename,
        lv_objnam      TYPE string,
        lv_rc          LIKE sy-subrc,
        lv_errtxt      TYPE string.

  DATA: lv_filename TYPE string,
        lv_result,
        lv_subrc    TYPE sy-subrc.

  DATA: lv_objid TYPE wwwdatatab-objid .


  lv_objid = gc_objid.  "上传的模版名称

  "查找文件是否存在。
  SELECT SINGLE relid objid
    FROM wwwdata
    INTO CORRESPONDING FIELDS OF lv_objdata
    WHERE srtf2    = 0
    AND   relid    = gc_relid
    AND   objid    = lv_objid.

  "判断模版不存在则报错
  IF sy-subrc NE 0 OR lv_objdata-objid EQ space.
    CONCATENATE TEXT-t06 lv_objid TEXT-t07
    INTO lv_errtxt.
    MESSAGE e000(su) WITH lv_errtxt.
  ENDIF.

  lv_filename = p_fname.

  "判断本地地址是否已经存在此文件。
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_filename
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.
  IF lv_result EQ abap_true.  "如果存在则删除原始文件，重新覆盖
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = lv_filename
      CHANGING
        rc                   = lv_subrc
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF lv_subrc <> 0. "如果删除失败，则报错。
      lv_errtxt = TEXT-t08.
      MESSAGE e000(su) WITH lv_errtxt.
    ENDIF.
  ENDIF.

  lv_destination   = p_fname.

  "下载模版。
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = lv_objdata
      destination = lv_destination
    IMPORTING
      rc          = lv_rc.
  IF lv_rc NE 0.
    lv_errtxt = TEXT-t09.
    MESSAGE e000(su) WITH lv_errtxt.
  ENDIF.
ENDFORM.                    " FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  frm_set_save_path
*&---------------------------------------------------------------------*
*
*       设置文件保存路径
*
*----------------------------------------------------------------------*
*      <--P_FNAME  文件路径
*----------------------------------------------------------------------*
FORM frm_set_save_path  CHANGING pv_fullpath TYPE string.

  DATA: lv_init_path  TYPE string,
        lv_init_fname TYPE string,
        lv_path       TYPE string,
        lv_filename   TYPE string,
        lv_fullpath   TYPE string.

* 初始名称(输出的文件名称)
  lv_init_fname = TEXT-t10.

* 获取桌面路径
  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory    = lv_init_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* 用户选择名称、路径
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_file_name    = lv_init_fname
      initial_directory    = lv_init_path
      prompt_on_overwrite  = abap_true
    CHANGING
      filename             = lv_filename
      path                 = lv_path
      fullpath             = lv_fullpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0.
    pv_fullpath = lv_fullpath.
  ENDIF.
ENDFORM.                    " frm_set_save_path
*&---------------------------------------------------------------------*
*&      Form  FRM_ASSIGNROLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_assignrole .
  SORT gt_data BY username agr_name.

  DATA lv_lines TYPE i.
  lv_lines = lines( gt_data ).

  LOOP AT gt_data INTO gs_data.
    " 进度条
    PERFORM frm_indicator USING sy-tabix lv_lines.

    AT NEW username.
      READ TABLE gt_data ASSIGNING <fs_data> INDEX sy-tabix.

      REFRESH: gt_bapiagr, gt_return.

      gs_bapiname-bapibname = gs_data-username.

    ENDAT.

    CLEAR gs_bapiagr.
    gs_bapiagr-agr_name = gs_data-agr_name.  "角色名称
    gs_bapiagr-from_dat = gs_data-from_dat.
    gs_bapiagr-to_dat = gs_data-to_dat.
    APPEND gs_bapiagr TO gt_bapiagr.

    AT END OF username.
      CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
        EXPORTING
          username       = gs_bapiname-bapibname
        TABLES
          activitygroups = gt_bapiagr
          return         = gt_return.

      READ TABLE gt_return INTO gs_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        MESSAGE ID gs_return-id
                TYPE gs_return-type
                NUMBER gs_return-number
                WITH gs_return-message_v1
                     gs_return-message_v2
                     gs_return-message_v3
                     gs_return-message_v4
                INTO <fs_data>-result.
      ELSE.
        READ TABLE gt_return INTO gs_return WITH KEY type = 'S'.
        IF sy-subrc = 0.
          MESSAGE ID gs_return-id
                  TYPE gs_return-type
                  NUMBER gs_return-number
                  WITH gs_return-message_v1
                       gs_return-message_v2
                       gs_return-message_v3
                       gs_return-message_v4
                  INTO <fs_data>-result.
        ENDIF.
      ENDIF.

    ENDAT.
  ENDLOOP.
ENDFORM.                    " FRM_ASSIGNROLE
*&---------------------------------------------------------------------*
*&      Form  FRM_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_indicator USING pv_index pv_lines.
*&---data declaration/进度条变量
  CONSTANTS lc_step_width TYPE i VALUE 1." 5.    "Width of each step
  DATA:lv_step       TYPE i VALUE lc_step_width, "current step
       lv_lines      TYPE i,                     "total records to be processed
       lv_linesc     TYPE c LENGTH 20,           "total records to be processed of string
       lv_index      TYPE i,                     "current records being processed
       lv_percentage TYPE f,                     "percentage of processed records
       lv_remaining  TYPE i,                     "remaining records to be processed
       lv_text       TYPE c LENGTH 80,           "info displayed at progress indicator
       lv_start      TYPE timestamp.             "time stamp of process started

*&---process logic
  lv_index = pv_index.
  lv_lines = pv_lines.

*&---当前占比%
  lv_percentage = lv_index * 100 / lv_lines.
*&---剩余条目数
  lv_remaining = lv_lines - lv_index.
*&---数字文本强制转换
  WRITE lv_lines     TO lv_linesc LEFT-JUSTIFIED.
  WRITE lv_remaining TO lv_text   LEFT-JUSTIFIED.
*&---进度条（记录）
  CONCATENATE TEXT-904 "'Processing remaining'(005)
  lv_text
  '/'
  lv_linesc
  TEXT-t13
  INTO lv_text SEPARATED BY space.

*&---SAP GUI 进度条
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_percentage
      text       = lv_text.
ENDFORM.

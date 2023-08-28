*&---------------------------------------------------------------------*
*& Report ZWFTEST_EXCEL04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_config_down.
TYPES:BEGIN OF ty_data,
        tabname TYPE tabname,
        ddtext  TYPE as4text,
        data    TYPE REF TO data,
      END OF ty_data.
DATA: gt_data TYPE TABLE OF ty_data.
TABLES dd02l.
SELECT-OPTIONS s_tab FOR dd02l-tabname.

START-OF-SELECTION.

  SELECT dd02l~tabname,ddtext FROM dd02l
    LEFT JOIN dd02t ON dd02l~tabname = dd02t~tabname AND ddlanguage = @sy-langu
    WHERE ( tabclass = 'TRANSP' OR tabclass = 'VIEW' )
    AND dd02l~tabname IN @s_tab
    INTO CORRESPONDING FIELDS OF TABLE @gt_data.

  CHECK sy-subrc EQ 0.

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    CREATE DATA <fs_data>-data TYPE TABLE OF (<fs_data>-tabname).
    SELECT * FROM (<fs_data>-tabname)
      INTO TABLE @<fs_data>-data->*.
  ENDLOOP.

  PERFORM frm_tab_to_excel USING '表数据下载' gt_data .

FORM frm_tab_to_excel USING name data LIKE gt_data .
  DATA: lo_excel     TYPE REF TO zcl_excel,
        lo_worksheet TYPE REF TO zcl_excel_worksheet,
        lo_hyperlink TYPE REF TO zcl_excel_hyperlink,
        lo_writer    TYPE REF TO zif_excel_writer,
        l_settings   TYPE  zexcel_s_table_settings,
        filename     TYPE string,
        path         TYPE string,
        fullpath     TYPE string.
  CREATE OBJECT lo_excel."创建EXCEL对象
  CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007."写入对象

  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_title( |TABLIST| ).
  l_settings-table_style       = zcl_excel_table=>builtinstyle_medium2.
  l_settings-show_row_stripes = abap_true.
  l_settings-nofilters         = abap_false.
  lo_hyperlink = zcl_excel_hyperlink=>create_internal_link( iv_location = |TABLIST!A1| ).
  LOOP AT gt_data INTO DATA(ls_data).
    l_settings-table_name = ls_data-tabname.
    l_settings-top_left_row = 2.
    lo_worksheet = lo_excel->add_new_worksheet( ).
    lo_worksheet->set_cell( ip_column = 'A' ip_row = 1 ip_value = 'RETURN TABLIST' ip_hyperlink = lo_hyperlink ).
    lo_worksheet->set_title( |{ ls_data-tabname }| ).
    lo_worksheet->bind_table( EXPORTING ip_table = ls_data-data->* is_table_settings = l_settings )."将内表赋到表格
  ENDLOOP.
  lo_excel->set_active_sheet_index( 1 ).
  lo_worksheet = lo_excel->get_active_worksheet( ).
  l_settings-top_left_row = 1.
  l_settings-table_name = 'TABLIST'.
  lo_worksheet->bind_table( EXPORTING ip_table = gt_data is_table_settings = l_settings )."将内表赋到表格
  LOOP AT gt_data INTO ls_data.
    DATA(row) = sy-tabix + 1.
    lo_hyperlink = zcl_excel_hyperlink=>create_internal_link( iv_location = | { ls_data-tabname }!A1| ).
    lo_worksheet->set_cell( ip_column = 'A' ip_row = row ip_value = ls_data-tabname ip_hyperlink = lo_hyperlink ).
  ENDLOOP.

  DATA(xstring) = lo_writer->write_file( lo_excel )."EXCEL转为xstring
  DATA(solix_tab) = cl_bcs_convert=>xstring_to_solix( iv_xstring  =  xstring  )."xstring转为solix
  DATA(bytecount) = xstrlen(  xstring )."获取长度
  filename =  name && '.xlsx'.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog "获取保存路径
    EXPORTING
      default_extension = 'xlsx'
      default_file_name = filename
    CHANGING
      filename          = filename
      path              = path
      fullpath          = fullpath.
  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount "下载文件
    filename     = fullpath
    filetype     = 'BIN'
  CHANGING data_tab     = solix_tab ).
ENDFORM.

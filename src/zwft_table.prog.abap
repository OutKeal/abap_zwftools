*&---------------------------------------------------------------------*
*& Report ZWFT_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_table.

TABLES:dd03l.
DATA:message TYPE REF TO zwft_message.
TABLES:  sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK blk01 WITH FRAME TITLE t01.
  PARAMETERS p_table TYPE dd03l-tabname OBLIGATORY.
  SELECTION-SCREEN ULINE.
  SELECTION-SCREEN PUSHBUTTON /33(30) bttn01 USER-COMMAND cmd1.
  PARAMETERS: rb_alv   RADIOBUTTON GROUP g1  DEFAULT 'X',
              rb_csv   RADIOBUTTON GROUP g1,
              rb_excel RADIOBUTTON GROUP g1.

  PARAMETERS:rb_down RADIOBUTTON GROUP g1,
             rb_up   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK blk01.

DATA gtd_output TYPE REF TO data.
FIELD-SYMBOLS <gtd_output> TYPE STANDARD TABLE.


INCLUDE zwft_where.

INITIALIZATION.
  t01 = '选择'.
  %_p_table_%_app_%-text = '表名'.
  bttn01 = '限定条件'.
  "//删除标记字段，用于删除记录时进行标记
  CREATE OBJECT message.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'CMD1'.
      PERFORM f_ss_popup USING p_table.

  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  IF p_table IS NOT INITIAL.
    IF lines( zwft_common=>get_table_fields( p_table ) ) = 0.
      MESSAGE |{ p_table } 不是有效的数据库表| TYPE 'S' DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  IF lines( zwft_common=>get_table_fields( p_table ) ) = 0.
    MESSAGE |给定的表名 { p_table } 不是一个有效的透明表的名称，无法继续| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CREATE DATA gtd_output TYPE TABLE OF (p_table).
  ASSIGN gtd_output->* TO <gtd_output>.

  CASE 'X'.
    WHEN rb_alv.
      PERFORM f10_read_data.
      PERFORM f20_display_data.
    WHEN rb_csv.
      PERFORM f10_read_data.
      zwft_common=>file_download_to_csv( <gtd_output> ).
    WHEN rb_excel.
      PERFORM f10_read_data.
      zwft_common=>file_download_to_excel( <gtd_output> ).
    WHEN rb_down.
      PERFORM f10_read_data_up_10.
      zwft_common=>file_download_to_excel( <gtd_output> ).
    WHEN rb_up.
      zwft_common=>file_upload_from_excel( CHANGING data = <gtd_output> ).
      PERFORM f20_display_data.
  ENDCASE.

FORM f10_read_data.


  SELECT * FROM (p_table) WHERE (gs_where-where_tab) INTO CORRESPONDING FIELDS OF TABLE @<gtd_output>.

ENDFORM.

FORM f10_read_data_up_10.

  SELECT * FROM (p_table) WHERE (gs_where-where_tab) INTO CORRESPONDING FIELDS OF TABLE @<gtd_output>
    UP TO 10 ROWS.

ENDFORM.

FORM f20_display_data.
  DATA(falv) = zwft_falv=>create( CHANGING ct_table = <gtd_output> ).

  falv->lvc_layout-edit = 'X'.
  falv->set_editable( abap_true ) .
  READ TABLE falv->fcat ASSIGNING FIELD-SYMBOL(<fcat>) WITH KEY fieldname = 'MANDT'.
  IF sy-subrc EQ 0.
    <fcat>-tech = 'X'.
  ENDIF.

  falv->display( ).
ENDFORM.

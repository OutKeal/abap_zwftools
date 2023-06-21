FUNCTION ZWFT_GET_TABLE_DATA .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TABNAME) TYPE  TABNAME
*"  EXPORTING
*"     VALUE(MSG_TYPE) TYPE  CHAR1
*"  TABLES
*"      ITAB STRUCTURE  ZWFT_BC_LINE
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any.
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  DATA:ls_string TYPE string.

*创建动态表结构
  CREATE DATA dyn_table TYPE TABLE OF (tabname).
*创建动态内表
  ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
  ASSIGN dyn_wa->* TO <dyn_wa>.

*获取配置表信息
  SELECT  * FROM (tabname) INTO TABLE  <dyn_table>.
  IF sy-subrc EQ 0.
    CLEAR itab[].
    LOOP AT <dyn_table> ASSIGNING <dyn_wa>.
      CALL METHOD cl_abap_container_utilities=>fill_container_c
        EXPORTING
          im_value               = <dyn_wa>
        IMPORTING
          ex_container           = ls_string
        EXCEPTIONS
          illegal_parameter_type = 1.
      MOVE ls_string TO itab.
      APPEND itab.
    ENDLOOP.
    msg_type = 'S'.
  ELSE.
    msg_type = 'E'.
  ENDIF.


ENDFUNCTION.

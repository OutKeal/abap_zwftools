*&---------------------------------------------------------------------*
*& Report ZBATCH_TABLE_DOWN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_table_down_mass.

TABLES dd03l.
FIELD-SYMBOLS: <dyn_table> TYPE TABLE,
<dyn_wa>    TYPE ANY,
<txt_field> TYPE ANY,
<val_field> TYPE ANY.
DATA: dyn_table TYPE REF TO DATA.
DATA: dyn_wa TYPE REF TO DATA.
*声明变量
DATA:  lv_path TYPE string.

*获取表名和路径
SELECT-OPTIONS: s_tab FOR dd03l-tabname .
PARAMETERS:p_path TYPE string.

*将输入的表名存入内表
SELECT tabname
INTO TABLE @DATA(lt_tabname)
      FROM dd02l
      WHERE tabname IN @s_tab.

CHECK sy-subrc EQ 0.
*循环处理每个表
LOOP AT lt_tabname INTO DATA(lv_tabname).


*创建动态表结构
  CREATE DATA dyn_table TYPE TABLE OF (lv_tabname-tabname).
*创建动态内表
  ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
  ASSIGN dyn_wa->* TO <dyn_wa>.

  SELECT * FROM (lv_tabname-tabname)
  INTO TABLE <dyn_table>.
*将数据写入文件 l
  lv_path = p_path && '/' && lv_tabname-tabname && '.csv'.
  cl_gui_frontend_services=>gui_download( EXPORTING filename = lv_path filetype = 'ASC'  WRITE_FIELD_SEPARATOR = '#' CHANGING data_tab = <dyn_table> ).

ENDLOOP.

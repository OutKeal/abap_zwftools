*&---------------------------------------------------------------------*
*& Report ZBC_TRANS_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_table_tran.


TABLES wccr.
TABLES dd02l.
DATA:msg_type TYPE char1.
DATA:bc_line TYPE TABLE OF zbc_line WITH HEADER LINE .
DATA:gt_dd02l TYPE TABLE OF dd02l WITH HEADER LINE.
FIELD-SYMBOLS: <dyn_table> TYPE TABLE,
<dyn_wa>    TYPE ANY,
<fs_value>  TYPE ANY.
DATA: dyn_table TYPE REF TO DATA.
DATA: dyn_wa TYPE REF TO DATA.
DATA: l_typedescr TYPE REF TO cl_abap_typedescr.

PARAMETERS: rfcdest TYPE  wccr-rfcdest OBLIGATORY.
SELECT-OPTIONS: s_tab FOR dd02l-tabname.
PARAMETERS:l_delete TYPE char1 AS CHECKBOX.

START-OF-SELECTION.

IF s_tab[] IS  INITIAL.
  MESSAGE '必须选定表' TYPE 'S' DISPLAY LIKE 'E'.
  STOP.
ENDIF.

SELECT * FROM dd02l INTO TABLE gt_dd02l
WHERE tabname IN s_tab.
IF sy-subrc NE 0.
  MESSAGE '表不存在' TYPE 'S' DISPLAY LIKE 'E'.
  STOP.
ENDIF.

LOOP AT gt_dd02l.
  IF gt_dd02l-tabname+0(1) <> 'Z'.
    MESSAGE '只能更新Z开头表' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT SINGLE tabname INTO gt_dd02l-tabname
  FROM tvdir
  WHERE tabname = gt_dd02l-tabname.
  IF sy-subrc NE 0.
    WRITE: gt_dd02l-tabname && '不存在维护视图，不允许更新', / .
    STOP.
  ENDIF.
ENDLOOP.

SORT gt_dd02l BY tabname.

LOOP AT gt_dd02l.
  CLEAR bc_line[].
  CALL FUNCTION 'ZWFT_GET_TABLE_DATA' DESTINATION rfcdest
  EXPORTING
    tabname  = gt_dd02l-tabname
  IMPORTING
    msg_type = msg_type
  TABLES
    bc_line  = bc_line[].
  IF msg_type = 'S'.
    WRITE: gt_dd02l-tabname && '表获取成功', / .
ELSEIF msg_type = 'E'.
    WRITE: gt_dd02l-tabname && '表获取失败', / .
  ENDIF.

*创建动态表结构
  CREATE DATA dyn_table TYPE TABLE OF (gt_dd02l-tabname).
*创建动态内表
  ASSIGN dyn_table->* TO <dyn_table>.
*创建动态工作区结构
  CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
*创建动态工作区
  ASSIGN dyn_wa->* TO <dyn_wa>.

  LOOP AT bc_line.
*      MOVE bc_line TO <dyn_wa>.
    CALL METHOD cl_abap_container_utilities=>read_container_c
    EXPORTING
    im_container           = bc_line-DATA
    IMPORTING
      ex_value               = <dyn_wa>
    EXCEPTIONS
      illegal_parameter_type = 1
      OTHERS                 = 2.
    IF sy-subrc <> 0.
*       Implement suitable error handling here
    ENDIF.

    ASSIGN COMPONENT 'MANDT' OF STRUCTURE <dyn_wa> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = sy-mandt.
    ENDIF.
    APPEND <dyn_wa> TO <dyn_table>.
  ENDLOOP.

  IF sy-subrc EQ 0.
    IF l_delete = 'X'.
      DELETE  FROM (gt_dd02l-tabname).
    ENDIF.
    MODIFY (gt_dd02l-tabname) FROM TABLE <dyn_table>.
    COMMIT  WORK AND WAIT.
  ENDIF.

  FREE dyn_table.
  FREE <dyn_table>.
  FREE dyn_wa.
  FREE <dyn_wa>.

ENDLOOP.

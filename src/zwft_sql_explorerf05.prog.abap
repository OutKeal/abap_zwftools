************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/28
*^ Include Name    : ZSQLEXPLORERF05
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Create A Dynamic Program
*
*^ Other           :
************************************************************************






******************************************************
*&      Form  Get_Data_In_Dynamic_Program
******************************************************
FORM get_data_in_dynamic_prog USING  p_sql      TYPE  tt_text
                                     p_type     TYPE  string
                                     p_element  TYPE  tt_element.

  DATA : it_code   TYPE tt_code     ,
         prog      TYPE c LENGTH 8  ,
         msg       TYPE c LENGTH 120,
         lin       TYPE c LENGTH 3  ,
         wrd       TYPE c LENGTH 10 ,
         off       TYPE c LENGTH 3  ,
         l_msg     TYPE string      ,
         p_stcurt  TYPE REF TO data .


  PERFORM  create_dynamic_program_code USING p_sql
                                             p_element
                                             p_type
                                    CHANGING it_code   .

  GENERATE SUBROUTINE POOL it_code NAME prog
                                MESSAGE msg
                                   LINE lin
                                   WORD wrd
                                 OFFSET off.

  IF sy-subrc <> 0.
    l_msg  =  msg .
    PERFORM append_error_message USING 4 l_msg .
    PERFORM download_table_to_local TABLES it_code[]
                                   USING     'c:\11.txt'.
  ELSE .

*& Get The Dynamic Structure
    PERFORM get_structure  IN PROGRAM (prog) CHANGING p_stcurt .
    ASSIGN p_stcurt->* TO <table> .


*& Get Data With SQL
    PERFORM get_data       IN PROGRAM (prog) USING  <table> .


  ENDIF .

ENDFORM .                    "Get_Data_In_Dynamic_Prog









******************************************************
*&      Form  Create_Dynamic_Program_Code
******************************************************
FORM  create_dynamic_program_code USING  p_sql     TYPE  tt_text
                                         p_element TYPE  tt_element
                                         p_type    TYPE  string
                               CHANGING  p_code    TYPE  tt_code    .
  DATA : it_temp  TYPE  tt_code .

  CLEAR : p_code[] .


*& Create The Start Of The Dynamic Program
  PERFORM  define_program_01     USING p_element
                              CHANGING it_temp  .
  APPEND LINES OF it_temp TO p_code .



*& Create The First Function
  PERFORM  define_program_02  CHANGING it_temp  .
  APPEND LINES OF it_temp TO p_code .



*& Create The Get Data Function
  PERFORM  define_program_03     USING p_sql
                                       p_type
                              CHANGING it_temp  .
  APPEND LINES OF it_temp TO p_code .


ENDFORM .                    "Create_Dynamic_Program






******************************************************
*&      Form  Define_Program_01
******************************************************
FORM  define_program_01 USING  p_element TYPE  tt_element
                     CHANGING  p_code    TYPE  tt_code    .
  DATA : wa_element  TYPE  st_element ,
         wa_line     TYPE  LINE OF tt_code,
         l_text      TYPE  string .

  CLEAR : p_code[] .

  APPEND 'PROGRAM SUBPOOL.                                      ' TO p_code .
  APPEND 'TYPES:Begin OF ST_DATA,                               ' TO p_code .

  LOOP AT p_element INTO wa_element .
    IF wa_element-display CS c_count .
      CONCATENATE wa_element-alias ' ' 'TYPE' ' '
                   'i,'
             INTO wa_line
        SEPARATED BY space .
    ELSE .
      CONCATENATE wa_element-source '-' wa_element-name
            INTO l_text .
      CONCATENATE wa_element-alias ' ' 'LIKE' ' '
                  l_text ','
             INTO wa_line
        SEPARATED BY space .
    ENDIF .
    APPEND wa_line TO p_code .
  ENDLOOP .

  APPEND '      End   OF ST_DATA.                               ' TO p_code .
  APPEND 'TYPES: TT_DATA  TYPE STANDARD TABLE OF ST_DATA.       ' TO p_code .



ENDFORM .                    "Define_Program_01







******************************************************
*&      Form  Define_Program_02
******************************************************
FORM  define_program_02  CHANGING  p_code  TYPE  tt_code    .
  DATA : wa_element  TYPE  st_element ,
         wa_line     TYPE  LINE OF tt_code .

  CLEAR : p_code[] .

  APPEND 'FORM get_structure Changing p_struct TYPE REF TO data . ' TO p_code .
  APPEND '                                                        ' TO p_code .
  APPEND '  CREATE DATA p_struct TYPE TT_DATA.                    ' TO p_code .
  APPEND '                                                        ' TO p_code .
  APPEND 'ENDFORM .                                               ' TO p_code .


ENDFORM .                    "Define_Program_02







******************************************************
*&      Form  Define_Program_03
******************************************************
FORM  define_program_03     USING  p_sql   TYPE  tt_text
                                   p_type  TYPE  string
                         CHANGING  p_code  TYPE  tt_code    .
  DATA : wa_text TYPE st_text    ,
         l_first TYPE i  VALUE 0 .

  CLEAR : p_code[] .


  APPEND 'FORM GET_DATA USING P_DATA     TYPE ANY TABLE . ' TO p_code .
  APPEND '  DATA : IT_DATA TYPE TT_DATA .                 ' TO p_code .
  APPEND '  DATA : WA_DATA TYPE ST_DATA .                 ' TO p_code .
  APPEND '                                                ' TO p_code .

  LOOP AT p_sql INTO wa_text .
    IF wa_text = 'FROM' AND l_first = 0 .
      IF p_type =  c_single .
        wa_text = 'INTO       WA_DATA FROM' .
      ELSE .
        wa_text = 'INTO CORRESPONDING FIELDS OF TABLE IT_DATA FROM' .
      ENDIF .
      l_first  =  1 .
    ENDIF .
    SHIFT wa_text-line BY 1 PLACES RIGHT .
    APPEND wa_text TO p_code .
  ENDLOOP .

  APPEND '  .                                             ' TO p_code .
  IF p_type =  c_single .
    APPEND '  APPEND WA_DATA TO IT_DATA.                    ' TO p_code .
  ENDIF .
  APPEND '  P_DATA = IT_DATA[].                           ' TO p_code .
  APPEND '                                                ' TO p_code .
  APPEND 'ENDFORM.                                        ' TO p_code .


ENDFORM .                    "Define_Program_03







*& End

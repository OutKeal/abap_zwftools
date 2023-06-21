************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/27
*^ Include Name    : ZSQLEXPLORERF04
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Process SQL
*
*^ Other           :
************************************************************************






******************************************************
*&      Form  Process_Comment2
******************************************************
FORM process_comment2   CHANGING p_text TYPE st_text .
  DATA : l_temp   TYPE string   ,
         l_amount TYPE i VALUE 0,
         l_mod    TYPE i VALUE 0,
         l_index  TYPE i        ,
         l_length TYPE i        .

  l_amount =  0 .

  l_length = STRLEN( p_text-line ) .

  DO l_length TIMES .
    l_index  = sy-index  - 1 .
    CASE p_text-line+l_index(1).
      WHEN c_comment2 .
        l_mod = l_amount MOD 2 .
        IF l_mod = 0 .
          IF l_index = 0 .
            p_text = space .
          ELSE .
            p_text = p_text(l_index).
          ENDIF .
          EXIT .
        ENDIF .
      WHEN c_quotes .
        ADD 1 TO l_amount .
    ENDCASE .
  ENDDO .

  l_mod = l_amount MOD 2 .

  IF l_mod <> 0 .
*& Exception "You maybe miss a single quoted symbol"
    PERFORM append_error_message USING 1 p_text-line.
  ENDIF .


ENDFORM.                    "Process_Comment2





******************************************************
*&      Form  Move_Comments_In_SQL
******************************************************
FORM move_comments_in_sql  CHANGING p_text TYPE tt_text .


  DATA : wa_text  TYPE st_text  .


  LOOP AT p_text INTO wa_text .

    IF wa_text-line(1) = c_comment1.
      DELETE p_text .
      CONTINUE .
    ENDIF .

    IF wa_text-line CS c_comment2 .
      PERFORM process_comment2   CHANGING wa_text .
      MODIFY p_text FROM wa_text .
    ENDIF .

    CONDENSE : wa_text-line .
    CHECK wa_text-line = space.
    DELETE p_text .

  ENDLOOP .

ENDFORM .                    "Move_Comments_In_SQL







******************************************************
*&      Form  Separated_Quoted
******************************************************
FORM separated_quoted    USING p_line   TYPE st_text
                      CHANGING p_table  TYPE tt_text .
  DATA : wa_text   TYPE  st_text ,
         l_flag    TYPE  i  VALUE 1 ,
         l_length  TYPE  i          ,
         l_index   TYPE  i          ,
         l_start   TYPE  i  VALUE 0 ,
         l_end     TYPE  i  VALUE 0 ,
         l_mod     TYPE  i  VALUE 0 ,
         l_amount  TYPE  i  VALUE 0 ,
         l_str     TYPE  string  .

  l_length = STRLEN( p_line-line ) .


  DO l_length TIMES .
    l_index  =  sy-index - 1 .
    CASE p_line+l_index(1).
      WHEN c_quotes .
        IF l_amount = 0 .
          IF sy-index > 1 .
            l_end = l_index - l_start .
            l_str = p_line-line+l_start(l_end) .
            APPEND l_str TO p_table .
            l_start = l_index .
          ENDIF .
        ENDIF .
        ADD 1 TO l_amount .

      WHEN space.
        IF l_amount = 0 .
          l_end = l_index - l_start .
          IF l_end =  0 .
            l_end  =  1 .
          ENDIF .
          l_str = p_line-line+l_start(l_end) .
          APPEND l_str TO p_table .
          l_start = l_index .
        ENDIF .

        l_mod = l_amount MOD 2 .

        CHECK l_amount <> 0 AND l_mod = 0.

        l_end = l_index - l_start .
        l_str = p_line-line+l_start(l_end) .
        APPEND l_str TO p_table .
        l_start = l_index .
        l_amount = 0 .

      WHEN OTHERS .
        l_mod = l_amount MOD 2 .
        CHECK l_amount <> 0 AND l_mod = 0.
        l_end = l_index - l_start .
        l_str = p_line-line+l_start(l_end) .
        APPEND l_str TO p_table .
        l_start = l_index .
        l_amount = 0 .
    ENDCASE .

  ENDDO .

  l_str = p_line-line+l_start.
  APPEND l_str TO p_table .
  l_start = l_index .


  l_mod = l_amount MOD 2 .

  IF l_mod <> 0 .
*& Exception "You maybe miss a single quoted symbol"
    PERFORM append_error_message USING 1 p_line-line.
  ENDIF .


ENDFORM .                    "Process_Quoted





******************************************************
*&      Form  Separate_SQL_From_Word
******************************************************
FORM  separate_from_word  CHANGING p_text TYPE tt_text .
  DATA : wa_text   TYPE  st_text ,
         it_temp   TYPE  tt_text ,
         it_split  TYPE  tt_text ,
         l_tab     TYPE  c       .

  CLEAR : it_temp.

  LOOP AT p_text INTO wa_text .

    IF wa_text CS c_quotes .
      CLEAR it_split .
      PERFORM separated_quoted USING wa_text
                            CHANGING it_split .
      APPEND LINES OF it_split TO it_temp .

    ELSE .
      CLEAR it_split .
      SPLIT wa_text AT space INTO TABLE it_split .
      APPEND LINES OF it_split TO it_temp .
    ENDIF .
  ENDLOOP .



*& Delete All Blank Line
  LOOP AT it_temp INTO wa_text .
    CHECK NOT wa_text CS c_quotes .
    CONDENSE wa_text-line  .
    MODIFY it_temp FROM wa_text .
  ENDLOOP .


  PERFORM get_asc_code    USING  c_hex
                       CHANGING  l_tab.

  DELETE it_temp WHERE LINE = space OR
                       LINE = l_tab.

  p_text[] = it_temp[] .

ENDFORM.                    "Separate_SQL_From_Word






******************************************************
*&      Form  Case_To_Upper
******************************************************
FORM case_to_upper CHANGING p_text .

  TRANSLATE p_text TO UPPER CASE.

ENDFORM.                    "Case_To_Upper







******************************************************
*&      Form  Case_To_Lower
******************************************************
FORM case_to_lower CHANGING p_text .

  TRANSLATE p_text TO LOWER CASE.

ENDFORM.                    "Case_To_Upper






******************************************************
*&      Form  transfer_Case
******************************************************
FORM transfer_case    USING p_type  TYPE  i
                   CHANGING p_text  TYPE  tt_text  .
  DATA : wa_text  TYPE  st_text .

  LOOP AT p_text INTO wa_text .

    CASE p_type .
      WHEN '1'.
        PERFORM case_to_lower CHANGING wa_text-line .
      WHEN '2'.
        PERFORM case_to_upper CHANGING wa_text-line .
    ENDCASE .

    MODIFY p_text FROM wa_text .
  ENDLOOP .

ENDFORM .                    "transfer_Case






******************************************************
*&      Form  Check_SQL_Key_Word
******************************************************
FORM check_sql_key_word  USING p_word
                      CHANGING p_return TYPE i .

  DATA : l_word       TYPE string .

  l_word = p_word .
  PERFORM case_to_upper CHANGING l_word .

  CONCATENATE c_separ l_word c_separ
         INTO l_word .

  p_return = 1.

  CHECK  c_sql_key_word_01 CS l_word OR
         c_sql_key_word_02 CS l_word OR
         c_sql_key_word_03 CS l_word OR
         c_sql_key_word_04 CS l_word  .

  p_return = 0.


ENDFORM .                    "Check_SQL_Key_Word





******************************************************
*&      Form  Upper_All_Word_SQL Exception Quotes Value
******************************************************
FORM upper_all_word_sql CHANGING p_text TYPE tt_text .
  DATA : wa_text   TYPE  st_text ,
         l_return  TYPE  i       .

  LOOP AT p_text INTO wa_text .

    CHECK NOT wa_text-line CS c_quotes .
    PERFORM check_sql_key_word USING wa_text
                            CHANGING l_return .
    CASE l_return.
      WHEN 0.
        PERFORM case_to_upper CHANGING wa_text-line .
      WHEN 1 .
        PERFORM case_to_lower CHANGING wa_text-line .
    ENDCASE .

    MODIFY p_text FROM wa_text .

  ENDLOOP .

ENDFORM .                    "Upper_All_Word_SQL








******************************************************
*&      Form  Process_funct_word
******************************************************
FORM process_funct_word    CHANGING p_text  TYPE tt_text.
  DATA : wa_text   TYPE  st_text ,
         wa_temp   TYPE  st_text ,
         l_str_01  TYPE  string  ,
         l_str_02  TYPE  string  ,
         l_str_03  TYPE  string  ,
         l_index   TYPE  i       ,
         l_temp    TYPE  st_text-line  ,
         l_step    TYPE  i       VALUE 0.

*&           SUM  (  *  )  AS  TT
*&  l_Step    1   2  3  4  5   6

  LOOP AT p_text INTO wa_text .

    CASE l_step .
      WHEN 2 .
        IF wa_text-line CS c_left .
          CONCATENATE wa_temp-line c_left
                 INTO wa_temp-line.
          DELETE p_text.
          l_step = 3 .
        ELSE .
          PERFORM append_error_message USING 2 wa_text-line.
        ENDIF .
      WHEN 3 .
*        CHECK NOT wa_text-line CS c_left .
        IF wa_text-line  CS c_right .
          SPLIT wa_text-line AT c_right INTO l_str_01 l_str_02  .
          IF l_str_01 IS NOT INITIAL .
            CONCATENATE wa_temp-line l_str_01 c_right
                   INTO wa_temp-line
              SEPARATED BY space.
          ELSE .
            CONCATENATE wa_temp-line c_right
                   INTO wa_temp-line
              SEPARATED BY space.
          ENDIF .
          l_step = 4 .
          IF l_str_02 = c_as.
            CONCATENATE wa_temp-line l_str_02
                   INTO wa_temp-line
              SEPARATED BY space.
            l_step = 5 .
          ENDIF .
        ELSE .
          CONCATENATE wa_temp-line wa_text-line
                 INTO wa_temp-line
            SEPARATED BY space.
          l_step = 3 .
        ENDIF .

        DELETE p_text.

      WHEN 4 .
        CHECK NOT wa_text-line CS c_right.
        IF wa_text-line  CS  c_as .
          CONCATENATE wa_temp-line wa_text-line
                 INTO wa_temp-line
            SEPARATED BY space.
          DELETE p_text.
          l_step = 5 .
        ELSE .
*& Assign A Alias To The Field
          INSERT wa_temp INTO p_text INDEX l_index .
          l_step = 0 .
        ENDIF .

      WHEN 5 .
        CONCATENATE c_separ wa_text-line c_separ
               INTO l_str_01 .
        IF c_sql_key_word_01 CS l_str_01  .
** Exception : Need A Field Alias
          PERFORM append_error_message USING 3 wa_text-line .
*          INSERT wa_temp INTO p_text INDEX l_index .
        ELSE .
          CONCATENATE wa_temp-line wa_text-line
                 INTO wa_temp-line
            SEPARATED BY space.
          MODIFY p_text FROM wa_temp .
        ENDIF .

        l_step = 0 .
    ENDCASE .

    l_temp  =   wa_text-line .
    PERFORM case_to_upper CHANGING  l_temp.

    IF l_temp = c_sum   OR  l_temp(4) = 'SUM(' OR
       l_temp = c_min   OR  l_temp(4) = 'MIN(' OR
       l_temp = c_max   OR  l_temp(4) = 'MAX(' OR
       l_temp = c_avg   OR  l_temp(4) = 'AVG(' OR
       l_temp = c_count OR  l_temp(6) = c_count1 .

      wa_text-line  =  l_temp.

      l_index  =  sy-tabix .  l_step   =  2 .

      IF wa_text-line CS c_left.
        SPLIT wa_text-line AT  c_left   INTO  l_str_01  l_str_02  .
        CONCATENATE l_str_01   c_left   INTO  wa_temp-line .
        IF l_str_02 IS INITIAL.
          l_step = 3.
        ELSE .
          IF l_str_02 CS c_right.
            SPLIT l_str_02  AT  c_right  INTO  l_str_01  l_str_03 .
            IF l_str_03 = c_as .
              CONCATENATE wa_temp-line l_str_01 c_right l_str_03
                     INTO wa_temp-line
                SEPARATED BY space.
              l_step = 5 .
            ELSE.
              CONCATENATE wa_temp-line l_str_01 c_right
                     INTO wa_temp-line
                SEPARATED BY space.
              l_step = 4 .
            ENDIF .
          ELSE .
            CONCATENATE wa_temp-line l_str_02
                    INTO wa_temp-line
               SEPARATED BY space.
            l_step = 3 .
          ENDIF .
        ENDIF .
      ELSE .
        wa_temp-line  =  l_str_01 .
        l_step = 2.
      ENDIF .

      DELETE p_text .
      CONTINUE .
    ENDIF .

  ENDLOOP .

ENDFORM .                    "Process_funct_word








******************************************************
*&      Form  get_all_Element_By_Key
******************************************************
FORM get_all_element_by_key    USING p_text    TYPE tt_text
                                     p_all     TYPE i
                                     p_from    TYPE string
                            CHANGING p_element TYPE tt_text .
  DATA : l_key_word  TYPE string  .

  CONCATENATE c_sql_key_word_01
              c_sql_key_word_02
              c_sql_key_word_03
         INTO l_key_word .

  PERFORM get_all_element_by_range  USING p_text
                                          p_all
                                          p_from
                                          l_key_word
                                 CHANGING p_element.

ENDFORM .                    "Get_All_Element_By_Range





******************************************************
*&      Form  get_all_Element_By_range
******************************************************
FORM get_all_element_by_range  USING p_text    TYPE tt_text
                                     p_all     TYPE i
                                     p_from    TYPE string
                                     p_to      TYPE string
                            CHANGING p_element TYPE tt_text .

  DATA : wa_text   TYPE st_text     ,
         l_flag    TYPE i  VALUE  1 ,
         l_text    TYPE string      ,
         l_to      TYPE string      .

  CLEAR : p_element .

  CONCATENATE c_separ p_to c_separ
         INTO l_to .


  LOOP AT p_text INTO wa_text .

    CASE wa_text-line .

      WHEN  p_from.
        l_flag = 0 .
        CONTINUE .

      WHEN  OTHERS.
        CHECK l_flag = 0 .

        CONCATENATE c_separ wa_text-line  c_separ
               INTO l_text .
        PERFORM case_to_upper CHANGING l_text .
        IF l_to CS l_text .
          IF p_all = 1 .
            EXIT .
          ELSE .
            l_flag = 1 .
          ENDIF .

        ENDIF .
    ENDCASE .

    CHECK l_flag = 0 .
    APPEND wa_text TO p_element .

  ENDLOOP .

ENDFORM .                    "Get_All_Element_By_Range






******************************************************
*&      Form  Get_All_Fields_Name
******************************************************
FORM get_all_fields_name USING p_text     TYPE  tt_text
                      CHANGING p_element  TYPE  tt_element .
  DATA : l_all   TYPE i VALUE 1 ,
         it_temp TYPE tt_text   .

  CLEAR : it_temp .
  PERFORM get_all_element_by_key   USING  p_text    l_all
                                          c_select
                                CHANGING  it_temp .

  PERFORM get_element_alias        USING  it_temp
                                CHANGING  p_element .

ENDFORM .                    "Get_All_Fields_Name







******************************************************
*&      Form  Get_All_Tables_Name
******************************************************
FORM get_all_tables_name USING p_text    TYPE  tt_text
                      CHANGING p_element TYPE  tt_element .
  DATA : l_all      TYPE i VALUE 1 ,
         it_temp    TYPE tt_text   ,
         it_element TYPE tt_element,
         wa_element TYPE st_element.


  CLEAR : p_element .

  l_all = 1 .
  PERFORM get_all_element_by_key   USING  p_text    l_all
                                          c_from
                                CHANGING  it_temp .

  PERFORM get_element_alias        USING  it_temp
                                CHANGING  it_element .

  APPEND LINES OF it_element TO p_element .

  l_all = 0 .
  PERFORM get_all_element_by_key   USING  p_text    l_all
                                          c_join
                                CHANGING  it_temp .

  PERFORM get_element_alias        USING  it_temp
                                CHANGING  it_element .

  APPEND LINES OF it_element TO p_element .

*& Process Table Alias Name
  LOOP AT p_element INTO wa_element .

    IF wa_element-alias IS INITIAL  .
      wa_element-alias = wa_element-name .
    ENDIF .

    CONCATENATE wa_element-name c_as
                wa_element-alias
           INTO wa_element-display
        SEPARATED BY space .

    MODIFY p_element FROM wa_element    .

  ENDLOOP .

ENDFORM.                    "Get_All_Tables_Name







******************************************************
*&      Form  Get_Element_Alias
******************************************************
FORM get_element_alias USING p_text    TYPE tt_text
                    CHANGING p_element TYPE tt_element .
  DATA : it_temp    TYPE tt_text,
         wa_text    TYPE st_text,
         wa_element TYPE st_element,
         l_flag     TYPE i VALUE 1 ,
         l_text     TYPE string    .

  it_temp = p_text .
  CLEAR : p_element,wa_element.

  DELETE it_temp  WHERE LINE = c_single   OR
                        LINE = c_distinct .

  LOOP AT it_temp INTO wa_text .
    l_text  =  wa_text-line .
    PERFORM case_to_upper CHANGING l_text .
    CASE l_text .
      WHEN c_as.
        l_flag = 0 .

      WHEN OTHERS .
        IF l_flag = 0 .
          IF wa_text-line CS c_right AND
             wa_text-line CS c_left .
** Excpetion Nedd A Alias
            PERFORM append_error_message USING 3 wa_text-line .
          ELSE.
            wa_element-alias = wa_text-line .
            APPEND wa_element TO p_element .
          ENDIF .
          CLEAR : wa_element .
          l_flag = 1 .

        ELSE .
          IF wa_element IS NOT INITIAL .
            APPEND wa_element TO p_element .
          ENDIF .
          CLEAR : wa_element .
          wa_element-name = wa_text-line .
        ENDIF .

    ENDCASE .

    AT LAST.
      IF l_flag = 0.
** Excpetion Nedd A Alias
        PERFORM append_error_message USING 3 wa_text-line .
      ENDIF .

      IF wa_element IS NOT INITIAL .
        APPEND wa_element TO p_element .
      ENDIF .
    ENDAT .
  ENDLOOP .

ENDFORM .                    "Get_Element_Alias






******************************************************
*&      Form  Get_All_Fields_Infor
******************************************************
FORM get_all_fields_infor USING p_table  TYPE tt_element
                       CHANGING p_fields TYPE tt_element .

*& Process Normal Fields
  PERFORM get_normal_fields_infor USING  p_table
                               CHANGING  p_fields .

*& Get Aggregate Function Fields
  PERFORM get_aggra_fields_infor  USING  p_table
                               CHANGING  p_fields .

*& Process Symbol * For All Fields
  PERFORM get_symbol_fields_infor USING  p_table
                               CHANGING  p_fields .

  PERFORM process_duplic_alias CHANGING  p_fields .


ENDFORM .                    "Get_All_Fields_Infor







******************************************************
*&      Form  Get_Normal_Fields_Infor
******************************************************
FORM get_normal_fields_infor  USING p_tables  TYPE  tt_element
                           CHANGING p_fields  TYPE  tt_element .
  DATA : wa_table        TYPE  st_element ,
         wa_field        TYPE  st_element ,
         it_fieldcat     TYPE  slis_t_fieldcat_alv WITH HEADER LINE,
         l_table_alias   TYPE  dfies-fieldname,
         l_field_alias   TYPE  dfies-fieldname,
         l_field_name    TYPE  dfies-fieldname,
         l_field_name01  TYPE  dfies-fieldname.

  LOOP AT p_tables INTO wa_table .

    CLEAR : it_fieldcat, it_fieldcat[] .
    PERFORM get_table_infor USING  wa_table-name
                         CHANGING  it_fieldcat[] .
*& Check Whether Table Exist
    IF it_fieldcat[] IS INITIAL .
      PERFORM append_error_message USING 5 wa_table-name .
      CONTINUE.
    ENDIF .

    LOOP AT p_fields INTO wa_field WHERE label IS INITIAL.
*& Skip Symbol *
      CHECK NOT wa_field-name CS c_all_fields .

      IF wa_field-name CS c_ss .
        SPLIT wa_field-name AT c_ss INTO l_table_alias l_field_name .
        CHECK l_table_alias = wa_table-alias .
      ELSE .
        l_field_name  = wa_field-name .
      ENDIF .

      l_field_name01  = l_field_name .
      PERFORM case_to_upper CHANGING l_field_name01 .

      READ TABLE it_fieldcat WITH KEY fieldname = l_field_name01 .

      CHECK sy-subrc  = 0 .

      wa_field-name   = l_field_name   .
      wa_field-source = wa_table-name  .
      wa_field-link   = wa_table-alias .

      wa_field-label  = it_fieldcat-seltext_l .
      IF wa_field-alias IS INITIAL.
        wa_field-alias  = wa_field-name .
      ENDIF .

      CONCATENATE wa_field-link c_ss wa_field-name
             INTO wa_field-display .

      MODIFY p_fields FROM wa_field .
    ENDLOOP .

  ENDLOOP .



ENDFORM .                    "Get_Normal_Fields_infor









******************************************************
*&      Form  Get_Aggra_Fields_Infor
******************************************************
FORM get_aggra_fields_infor  USING  p_tables TYPE tt_element
                          CHANGING  p_fields TYPE tt_element .
  DATA : wa_field  TYPE  st_element ,
         wa_temp   TYPE  st_element ,
         it_text   TYPE  tt_text    ,
         wa_text   TYPE  st_text    ,
         l_step    TYPE  i VALUE 0  ,
         l_index   TYPE  i          ,
         l_str_01  TYPE  string     ,
         l_str_02  TYPE  string     .


  LOOP AT p_fields INTO wa_field WHERE label IS INITIAL.

    IF wa_field-name CS c_sum OR  wa_field-name CS c_min OR
       wa_field-name CS c_max OR  wa_field-name CS c_avg OR
       wa_field-name CS c_count .

      CLEAR : it_text .

      SPLIT wa_field-name AT space INTO TABLE it_text.
      DELETE it_text WHERE LINE = space .

*& Get Field Name
      READ TABLE it_text INTO wa_text WITH KEY line = c_right .
      l_index  =  sy-tabix - 1 .
      CLEAR : wa_text .
      READ TABLE it_text INTO wa_text  INDEX l_index .
      IF wa_text = c_all_fields.
        wa_field-label  = c_funct .
      ELSE.
        PERFORM get_field_infor_by_name  USING p_tables
                                               wa_text-line
                                      CHANGING wa_field .
        CONCATENATE wa_field-link c_ss wa_field-name
               INTO wa_text .
      ENDIF .
      MODIFY it_text FROM wa_text INDEX l_index .

*& Get Field Alias
      CLEAR : wa_text .
      READ TABLE it_text INTO wa_text WITH KEY line = c_as .
      IF sy-subrc = 0 .
        l_index  =  sy-tabix + 1 .
        CLEAR : wa_text .
        READ TABLE it_text INTO wa_text INDEX l_index .
        IF sy-subrc = 0 .
          wa_field-alias  =  wa_text-line .
        ELSE .
          wa_field-alias  =  c_funct.
        ENDIF .
      ELSE .
        wa_field-alias  =  c_funct.
      ENDIF .

*& Set Display
      CLEAR : wa_field-display .
      LOOP AT it_text INTO wa_text .
        IF wa_text-line = c_as .
          EXIT .
        ENDIF .
        CONCATENATE wa_field-display wa_text-line
               INTO wa_field-display
          SEPARATED BY space .
      ENDLOOP .


      CONDENSE : wa_field-display .


      MODIFY p_fields FROM wa_field .

    ENDIF .

  ENDLOOP .


ENDFORM.                    "Get_Aggra_Fields_infor










******************************************************
*&      Form  Get_Symbol_Fields_Infor
******************************************************
FORM get_symbol_fields_infor USING p_tables TYPE tt_element
                          CHANGING p_fields TYPE tt_element .
  DATA : wa_table        TYPE  st_element ,
         wa_field        TYPE  st_element ,
         it_fieldcat     TYPE  slis_t_fieldcat_alv WITH HEADER LINE,
         l_table_alias   TYPE  dfies-fieldname,
         l_field_name    TYPE  dfies-fieldname,
         it_temp         TYPE  tt_element     ,
         wa_temp         TYPE  st_element     .

  LOOP AT p_fields INTO wa_field WHERE label IS INITIAL.
    CLEAR : l_table_alias, l_field_name .

    IF wa_field-name CS c_ss .
      SPLIT wa_field-name AT c_ss INTO l_table_alias l_field_name .
    ELSE .
      l_field_name = wa_field-name .
    ENDIF .

    CHECK l_field_name  =  c_all_fields .

    CLEAR : it_temp[] .

    LOOP AT p_tables INTO wa_table .
      CLEAR : it_fieldcat, it_fieldcat[] .

      CHECK l_table_alias = wa_table-alias OR
            l_table_alias IS INITIAL .

      PERFORM get_table_infor USING  wa_table-name
                           CHANGING  it_fieldcat[] .
      LOOP AT it_fieldcat  .
        CLEAR : wa_temp .
        wa_temp-name    =  it_fieldcat-fieldname .
        wa_temp-alias   =  it_fieldcat-fieldname .
        wa_temp-source  =  wa_table-name         .
        wa_temp-link    =  wa_table-alias        .
        wa_temp-label   =  it_fieldcat-seltext_l .

        CONCATENATE wa_temp-link c_ss wa_temp-name
               INTO wa_temp-display .
        APPEND wa_temp TO it_temp .
      ENDLOOP .
    ENDLOOP .

*& Deletle The Sybmol *
    DELETE p_fields INDEX sy-tabix .

    INSERT LINES OF it_temp INTO p_fields  INDEX sy-tabix .

  ENDLOOP .


ENDFORM .                    "Get_Symbol_Fields_infor







******************************************************
*&      Form  Process_Duplic_Alias
******************************************************
FORM process_duplic_alias CHANGING  p_fields TYPE tt_element.
  DATA : wa_prior  TYPE  st_element ,
         wa_field  TYPE  st_element ,
         l_i       TYPE  n LENGTH 2 .

  LOOP AT p_fields INTO wa_field .

    wa_field-index =  sy-tabix .

    MODIFY p_fields FROM wa_field .

    IF wa_field-display IS INITIAL .
      PERFORM append_error_message USING 6 wa_field-name .
    ENDIF .
  ENDLOOP .

  SORT p_fields BY alias index .

  LOOP AT p_fields INTO wa_field.

    IF wa_prior-alias  = wa_field-alias .
      ADD 1 TO l_i .
      CONCATENATE wa_field-alias '_' l_i
             INTO wa_field-alias .
    ELSE .
      l_i = 1 .
      wa_prior = wa_field .
    ENDIF .

    CONCATENATE wa_field-display  c_as  wa_field-alias
           INTO wa_field-display
      SEPARATED BY space .

    MODIFY p_fields FROM wa_field .

  ENDLOOP .

  SORT p_fields BY index .

ENDFORM .                    "Process_Duplic_Alias







******************************************************
*&      Form  Get_Field_Infor_By_Name
******************************************************
FORM get_field_infor_by_name  USING p_tables  TYPE tt_element
                                    p_name
                           CHANGING p_field   TYPE st_element.
  DATA: wa_table      TYPE st_element ,
        l_table_alias TYPE dfies-fieldname,
        l_field_name  TYPE dfies-fieldname.


  IF p_name CS c_ss .
    SPLIT p_name AT c_ss INTO l_table_alias l_field_name .
    READ TABLE p_tables INTO wa_table WITH KEY alias = l_table_alias .
    CHECK sy-subrc = 0  .
    PERFORM get_field_infor_in_table  USING wa_table-name
                                            l_field_name
                                   CHANGING p_field      .
    p_field-link = wa_table-alias .
    p_field-name = l_field_name   .
  ELSE .

    LOOP AT p_tables INTO wa_table .
      PERFORM get_field_infor_in_table  USING wa_table-name
                                              p_name
                                     CHANGING p_field      .

      IF p_field-alias IS NOT INITIAL .
        p_field-link = wa_table-alias .
        p_field-name = p_name         .
        EXIT .
      ENDIF .
    ENDLOOP .

  ENDIF .

ENDFORM .                    "Get_Field_Infor_By_Name







******************************************************
*&      Form  Get_Field_Infor_In_Table
******************************************************
FORM get_field_infor_in_table  USING p_table_name
                                     p_name
                            CHANGING p_field       TYPE st_element.
  DATA : l_field_name01  TYPE  dfies-fieldname,
         it_fieldcat     TYPE slis_t_fieldcat_alv WITH HEADER LINE .

  CLEAR : it_fieldcat, it_fieldcat[] .

  PERFORM get_table_infor USING  p_table_name
                       CHANGING  it_fieldcat[] .

  l_field_name01  = p_name.
  PERFORM case_to_upper CHANGING l_field_name01 .

  READ TABLE it_fieldcat WITH KEY fieldname = l_field_name01 .

  CHECK sy-subrc  = 0 .
  p_field-name   = p_name   .
  p_field-source = p_table_name  .
  p_field-label  = it_fieldcat-seltext_l .

  IF p_field-alias IS INITIAL.
    p_field-alias  = p_field-name .
  ENDIF .

ENDFORM .                    "get_field_infor_in_table









******************************************************
*&      Form  Get_Table_Infor
******************************************************
FORM get_table_infor USING  p_table
                  CHANGING  p_fieldcat TYPE slis_t_fieldcat_alv .
  DATA : l_table  TYPE  dd02l-tabname .

  l_table  =  p_table .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = l_table
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = p_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM .                    "Get_Table_Infor





******************************************************
*&   Form Get_asc_code
******************************************************
FORM get_asc_code      USING  p_hex
                    CHANGING  p_char .
  DATA: l_string(1024).

  CALL FUNCTION 'STPU1_HEX_TO_CHAR'
    EXPORTING
      hex_string  = p_hex
    IMPORTING
      char_string = l_string.

  p_char = l_string.

ENDFORM.                    " get_asc_code









******************************************************
*&      Form  Get_Fields_And_Catalog
******************************************************
FORM get_sql_fields          USING     p_text     TYPE tt_text
                                       p_tables   TYPE tt_element
                             CHANGING  p_fields   TYPE tt_element.

  PERFORM get_all_fields_name      USING p_text
                                CHANGING p_fields.


  PERFORM get_all_fields_infor     USING p_tables
                                CHANGING p_fields.


ENDFORM .                    "Get_Fields_And_Catalog







******************************************************
*&      Form  Get_On_Tables
******************************************************
FORM get_on_tables           USING  p_text    TYPE tt_text
                                    p_tables  TYPE tt_element
                           CHANGING p_on      TYPE tt_text.

  DATA : wa_text  TYPE  st_text ,
         l_str    TYPE  string  ,
         wa_table TYPE  st_element,
         l_step   TYPE  i  VALUE 0,
         l_index  TYPE  i  VALUE 0,
         l_join   TYPE  i  VALUE 0,
         l_on     TYPE  i  VALUE 0,
         l_mod    TYPE  i  VALUE 0.

  CLEAR: p_on .


  LOOP AT p_text INTO wa_text .

    CASE wa_text-line .
      WHEN c_from  .
        l_step = 1 .  CONTINUE .

      WHEN c_inner .
        l_join  = 0 .  CHECK l_on > 0 .
        l_on    = 0 .  APPEND l_str TO p_on .
        l_step = 0 .

      WHEN c_left_j .
        l_join  = 1 .  CHECK l_on > 0 .
        l_on    = 0 .  APPEND l_str TO p_on .
        l_step = 0 .

      WHEN c_join.
        l_step  = 2 .  CHECK l_on > 0 .
        l_on    = 0 .  APPEND l_str TO p_on .

      WHEN c_on .
        l_step  = 3 .  CHECK l_on > 0 .
        l_on    = 0 . APPEND l_str TO p_on .

      WHEN c_where OR c_order OR c_group OR c_have.
        CHECK l_on > 0 .   l_on    = 0 .
        APPEND l_str TO p_on .

        EXIT .
    ENDCASE .


    CASE l_step .
      WHEN 1 .
        ADD 1 TO l_index .
        READ TABLE p_tables INTO wa_table INDEX l_index .
        CONCATENATE c_from wa_table-display
               INTO l_str
          SEPARATED BY space .
        SHIFT l_str BY 2 PLACES RIGHT.
        APPEND l_str TO p_on .
        l_step = 0 .
      WHEN 2 .
        ADD 1 TO l_index .
        READ TABLE p_tables INTO wa_table INDEX l_index .
        IF l_join = 0 .
          CONCATENATE c_inner c_join wa_table-display
                 INTO l_str
            SEPARATED BY space .
          SHIFT l_str BY 1 PLACES RIGHT.
          APPEND l_str TO p_on .
        ELSE .
          CONCATENATE c_left_j c_join wa_table-display
                 INTO l_str
            SEPARATED BY space .
          SHIFT l_str BY 2 PLACES RIGHT.
          APPEND l_str TO p_on .
        ENDIF .
        l_step = 0 .
        l_join = 0 .
      WHEN 3 .
        ADD 1 TO l_on .
        IF l_on = 1   .
          CONCATENATE c_on wa_text-line
                 INTO l_str
            SEPARATED BY space .
          SHIFT l_str BY 4 PLACES RIGHT.
        ELSE .
          l_mod = l_on MOD 8 .
          IF l_mod = 0 .
            APPEND l_str TO p_on .
            CLEAR : l_str .
            l_str =  wa_text-line .
            CASE wa_text-line .
              WHEN c_or .
                SHIFT l_str BY 4 PLACES RIGHT.
              WHEN c_and.
                SHIFT l_str BY 3 PLACES RIGHT.
              WHEN OTHERS.
                SHIFT l_str BY 7 PLACES RIGHT.
            ENDCASE .
          ELSE .
            CONCATENATE l_str wa_text-line
                   INTO l_str
              SEPARATED BY space .
          ENDIF .
        ENDIF .

      WHEN 4 .

    ENDCASE .

    AT LAST .
      IF l_on > 0 .
        l_on = 0 .
        APPEND l_str TO p_on .
        l_step = 0 .
      ENDIF .
    ENDAT .

  ENDLOOP .




ENDFORM .                    "Get_On_Tables






******************************************************
*&      Form  Get_Where
******************************************************
FORM get_where               USING  p_text  TYPE  tt_text
                          CHANGING p_where  TYPE  tt_text .
  DATA : wa_text  TYPE  st_text ,
         l_where  TYPE  i  VALUE 0 ,
         l_flag   TYPE  i  VALUE 1 ,
         l_mod    TYPE  i  VALUE 0 ,
         l_str    TYPE  string     .


  LOOP AT p_text INTO wa_text .
    CASE wa_text-line .
      WHEN c_where .
        l_flag  =  0 .
        l_where =  -1 .
      WHEN c_group OR c_have OR c_order .
        l_flag  =  1 .
    ENDCASE .

    IF l_flag = 0 .
      ADD 1 TO l_where .

      IF l_where = 0 .
        l_str  =  wa_text-line .
        SHIFT l_str BY 1 PLACES RIGHT.
      ELSE .
        l_mod  =  l_where MOD 8 .
        IF l_mod = 0 .
          APPEND l_str TO p_where .
          CLEAR : l_str .
          l_str =  wa_text-line .
          CASE wa_text-line .
            WHEN c_or .
              SHIFT l_str BY 4 PLACES RIGHT.
            WHEN c_and.
              SHIFT l_str BY 3 PLACES RIGHT.
            WHEN OTHERS.
              SHIFT l_str BY 7 PLACES RIGHT.
          ENDCASE .
        ELSE .
          CONCATENATE l_str wa_text-line
                 INTO l_str
            SEPARATED BY space .
        ENDIF .
      ENDIF .
    ENDIF .

    AT LAST.
      CHECK  l_str IS NOT INITIAL .
      APPEND l_str TO p_where .
    ENDAT .
  ENDLOOP .


ENDFORM .                    "Get_Where





******************************************************
*&      Form  Get_Group
******************************************************
FORM get_group               USING p_text   TYPE  tt_text
                          CHANGING p_group  TYPE  tt_text .
  DATA : wa_text  TYPE  st_text ,
         l_group  TYPE  i  VALUE 0 ,
         l_flag   TYPE  i  VALUE 1 ,
         l_mod    TYPE  i  VALUE 0 ,
         l_str    TYPE  string     .


  LOOP AT p_text INTO wa_text .
    CASE wa_text-line .
      WHEN c_group   .
        l_flag  =  0 .
        l_group =  0 .
      WHEN c_have OR c_order .
        l_flag  =  1 .
    ENDCASE .

    IF l_flag = 0 .
      ADD 1 TO l_group .

      IF l_group = 1 .
        l_str  =  wa_text-line .
        SHIFT l_str BY 1 PLACES RIGHT.
      ELSE .
        l_mod  =  l_group MOD 8 .
        IF l_mod = 0 .
          APPEND l_str TO p_group .
          CLEAR : l_str .
          l_str =  wa_text-line .
          SHIFT l_str BY 7 PLACES RIGHT.
        ELSE .
          CONCATENATE l_str wa_text-line
                 INTO l_str
            SEPARATED BY space .
        ENDIF .
      ENDIF .
    ENDIF .

    AT LAST.
      CHECK  l_str IS NOT INITIAL .
      APPEND l_str TO p_group .
    ENDAT .
  ENDLOOP .

ENDFORM .                    "Get_Group






******************************************************
*&      Form  Get_Have
******************************************************
FORM get_have                USING p_text   TYPE  tt_text
                          CHANGING p_have   TYPE  tt_text .
  DATA : wa_text  TYPE  st_text ,
         l_have   TYPE  i  VALUE 0 ,
         l_flag   TYPE  i  VALUE 1 ,
         l_mod    TYPE  i  VALUE 0 ,
         l_str    TYPE  string     .


  LOOP AT p_text INTO wa_text .
    CASE wa_text-line .
      WHEN c_have .
        l_flag  =  0 .
        l_have  =  0 .
      WHEN c_order .
        l_flag  =  1 .
    ENDCASE .

    IF l_flag = 0 .
      ADD 1 TO l_have .

      IF l_have = 1 .
        l_str  =  wa_text-line .
      ELSE .
        l_mod  =  l_have MOD 8 .
        IF l_mod = 0 .
          APPEND l_str TO p_have .
          CLEAR : l_str .
          l_str =  wa_text-line .
          SHIFT l_str BY 7 PLACES RIGHT.
        ELSE .
          CONCATENATE l_str wa_text-line
                 INTO l_str
            SEPARATED BY space .
        ENDIF .
      ENDIF .
    ENDIF .

    AT LAST.
      CHECK  l_str IS NOT INITIAL .
      APPEND l_str TO p_have .
    ENDAT .
  ENDLOOP .

ENDFORM .                    "Get_Have





******************************************************
*&      Form  Get_Order
******************************************************
FORM get_order               USING p_text   TYPE  tt_text
                          CHANGING p_order  TYPE  tt_text .
  DATA : wa_text  TYPE  st_text ,
         l_order  TYPE  i  VALUE 0 ,
         l_flag   TYPE  i  VALUE 1 ,
         l_mod    TYPE  i  VALUE 0 ,
         l_str    TYPE  string     .


  LOOP AT p_text INTO wa_text .
    CASE wa_text-line .
      WHEN c_order .
        l_flag   =  0 .
        l_order  =  0 .
      WHEN c_select OR c_from  OR c_where OR
           c_have   OR c_inner OR c_join .
        CHECK l_flag = 0 .
        PERFORM append_error_message USING 9 wa_text-line.
        EXIT .
    ENDCASE .

    IF l_flag = 0 .
      ADD 1 TO l_order .

      IF l_order = 1 .
        l_str  =  wa_text-line .
        SHIFT l_str BY 1 PLACES RIGHT.
      ELSE .
        l_mod  =  l_order MOD 8 .
        IF l_mod = 0 .
          APPEND l_str TO p_order .
          CLEAR : l_str .
          l_str =  wa_text-line .
          SHIFT l_str BY 7 PLACES RIGHT.
        ELSE .
          CONCATENATE l_str wa_text-line
                 INTO l_str
            SEPARATED BY space .
        ENDIF .
      ENDIF .
    ENDIF .

    AT LAST.
      CHECK  l_str IS NOT INITIAL .
      APPEND l_str TO p_order .
    ENDAT .
  ENDLOOP .


ENDFORM .                    "Get_Order






******************************************************
*&      Form  perpare_SQL_Element
******************************************************
FORM perpare_sql_element   CHANGING p_text   TYPE  tt_text
                                    p_fields TYPE  tt_element
                                    p_tables TYPE  tt_element
                                    p_on     TYPE  tt_text
                                    p_where  TYPE  tt_text
                                    p_group  TYPE  tt_text
                                    p_have   TYPE  tt_text
                                    p_order  TYPE  tt_text .

  PERFORM move_comments_in_sql  CHANGING p_text .

  IF p_text IS INITIAL .
    PERFORM append_error_message USING 8 '' .
    EXIT .
  ENDIF .

  PERFORM separate_from_word    CHANGING p_text .
  PERFORM upper_all_word_sql    CHANGING p_text .
  PERFORM process_funct_word    CHANGING p_text .

  PERFORM get_all_tables_name      USING p_text
                                CHANGING p_tables.

  IF p_tables IS INITIAL .
    PERFORM append_error_message USING 10 '' .
    EXIT .
  ENDIF .

  PERFORM get_sql_fields           USING p_text
                                         p_tables
                                CHANGING p_fields.

  IF p_fields IS INITIAL .
    PERFORM append_error_message USING 11 '' .
    EXIT .
  ENDIF .

  PERFORM get_on_tables            USING p_text
                                         p_tables
                                CHANGING p_on .

  PERFORM get_where                USING p_text
                                CHANGING p_where .

  PERFORM get_group                USING p_text
                                CHANGING p_group .

  PERFORM get_have                 USING p_text
                                CHANGING p_have .

  PERFORM get_order                USING p_text
                                CHANGING p_order .


ENDFORM .                    "Perpare_SQL_Element





******************************************************
*&      Form  Get_Ref_Table
******************************************************
FORM get_ref_table  USING  p_table_alias  TYPE ddobjname
                           p_tables       TYPE tt_element
                 CHANGING  p_table_name   TYPE ddobjname  .
  DATA : wa_element  TYPE  st_element .

  READ TABLE p_tables INTO wa_element
                      WITH KEY alias = p_table_alias .

  p_table_name  =  wa_element-name .


ENDFORM .                    "Get_Ref_Table







******************************************************
*&      Form  Check_SQL_With_Sub_Query
******************************************************
FORM check_sql_with_sub_query      USING p_text   TYPE tt_text
                                CHANGING p_return TYPE i.
  DATA : wa_text TYPE st_text .

  LOOP AT p_text INTO  wa_text .

    CHECK wa_text-line CP c_from .

    ADD 1 TO p_return .

  ENDLOOP.


ENDFORM .                    "Check_SQL_With_Sub_Query








******************************************************
*&      Form  Execute_SQL
******************************************************
FORM execute_sql          USING  p_text   TYPE tt_text
                       CHANGING  l_number TYPE i       .
  DATA: it_fields   TYPE  tt_element  ,
        it_tables   TYPE  tt_element  ,
        it_select   TYPE  tt_text     ,
        it_on       TYPE  tt_text     ,
        it_where    TYPE  tt_text     ,
        it_group    TYPE  tt_text     ,
        it_have     TYPE  tt_text     ,
        it_order    TYPE  tt_text     ,
        it_fieldcat TYPE  lvc_t_fcat  ,
        it_temp     TYPE  tt_text     ,
        l_type      TYPE  string      ,
        l_act       TYPE  i VALUE 1   ,
        l_return    TYPE  i   VALUE 0 .


  it_temp[] =  p_text[] .


  PERFORM format_sql_adapter      USING  l_act
                               CHANGING it_temp      l_type
                                        it_fields    it_tables
                                        it_select    it_on
                                        it_where     it_group
                                        it_have      it_order
                                        l_return .


*  PERFORM check_sql_with_sub_query USING it_temp[]
*                                CHANGING l_return .

  IF <table> IS ASSIGNED .
    CLEAR : <table> .
    UNASSIGN <table> .
  ENDIF .

  IF g_exception[] IS INITIAL .
    CASE l_return.
*& SQL With Error
      WHEN 0 .


*& Without Sub Query
      WHEN 1 .

        PERFORM get_data_in_dynamic_sql USING it_temp[]    l_type
                                              it_fields    it_select
                                              it_on        it_where
                                              it_group     it_have
                                              it_order .

*& With Sub Query
      WHEN OTHERS .
        PERFORM get_data_in_dynamic_prog USING it_temp[]
                                               l_type
                                               it_fields.
    ENDCASE .
  ENDIF .

  IF g_exception IS INITIAL .
*& Show Data
    PERFORM prepare_alv_field_cat    USING it_fields
                                  CHANGING it_fieldcat .
    l_number =  sy-dbcnt .
    PERFORM show_data_in_alv         USING g_grid  <table>
                                           it_fieldcat[] .
  ELSE .
*& Show Exception
    PERFORM prepare_alv_error_field_cat CHANGING it_fieldcat .
    PERFORM show_data_in_alv         USING g_grid   g_exception
                                           it_fieldcat[] .
  ENDIF .

  g_postion = 50 .
  PERFORM set_splitter_postion     USING g_splitter
                                         g_postion .

ENDFORM .                    "Execute_SQL





******************************************************
*&      Form  Get_Stucture_Define
******************************************************
FORM get_stucture_define        USING p_fields  TYPE  tt_element
                             CHANGING p_text    TYPE  tt_text   .
  DATA : wa_text   TYPE  st_text ,
         wa_field  TYPE  st_element .

  CLEAR : p_text[] .
  wa_text  =  'TYPES : BEGIN OF ST_DATA,'.
  APPEND wa_text TO p_text .

  LOOP AT p_fields INTO wa_field .
    CLEAR : wa_text .
    IF  wa_field-display(6) = c_count1 .
      CONCATENATE wa_field-alias 'TYPE i ,'
             INTO wa_text-line
        SEPARATED BY space .
    ELSE .
      CONCATENATE wa_field-source '-' wa_field-name
             INTO wa_text-line .
      CONCATENATE wa_field-alias 'TYPE' wa_text-line ','
             INTO wa_text-line
        SEPARATED BY space .
    ENDIF .
    SHIFT wa_text-line BY 10 PLACES RIGHT .
    APPEND wa_text TO p_text .

  ENDLOOP .

  wa_text  =  '        END   OF ST_DATA.'.
  APPEND wa_text TO p_text .

ENDFORM.                    "Get_Stucture_Define






******************************************************
*&      Form  format_sql_adapter
******************************************************
FORM format_sql_adapter      USING  p_act     TYPE  i
                          CHANGING  p_text    TYPE  tt_text
                                    p_type    TYPE  string
                                    p_fields  TYPE  tt_element
                                    p_tables  TYPE  tt_element
                                    p_select  TYPE  tt_text
                                    p_on      TYPE  tt_text
                                    p_where   TYPE  tt_text
                                    p_group   TYPE  tt_text
                                    p_have    TYPE  tt_text
                                    p_order   TYPE  tt_text
                                    p_return  type  i.
  Data : it_temp     TYPE  tt_text .

  PERFORM perpare_sql_element  CHANGING p_text     p_fields
                                        p_tables   p_on
                                        p_where    p_group
                                        p_have     p_order.


  PERFORM check_sql_with_sub_query  USING p_text
                                 CHANGING p_return .

  CASE p_return.
*& SQL With Error
    WHEN 0 .

*& Without Sub Query
    WHEN 1 .

*& Format Select Fields
      PERFORM format_fields           USING  p_fields
                                             p_act
                                   CHANGING  p_select
                                             p_text
                                             p_type    .

*& Format Table Name
      PERFORM format_tables           USING  p_on
                                   CHANGING  p_text  .

*& Format Where Condition
      PERFORM format_where            USING  p_where
                                   CHANGING  p_text   .

*& Format Group By
      PERFORM format_group            USING  p_group
                                   CHANGING  p_text   .

*& Format Having
      PERFORM format_have             USING  p_have
                                   CHANGING  p_text   .
*& Format Order By
      PERFORM format_order            USING  p_order
                                   CHANGING  p_text   .

*& Trnasfer Case
      PERFORM transfer_case           USING  g_case
                                   CHANGING  p_text   .

    WHEN OTHERS .
*& Format Select Fields
      it_temp[]  =  p_text .
      PERFORM format_fields           USING  p_fields
                                             p_act
                                   CHANGING  p_select
                                             it_temp
                                             p_type    .

      IF p_act <> 1 .
        PERFORM append_error_message USING 7 ''.
      ENDIF .
  ENDCASE .

ENDFORM .                    "format_sql





******************************************************
*&      Form  format_sql
******************************************************
FORM format_sql   CHANGING  p_text    TYPE  tt_text .
  DATA:  it_fields   TYPE  tt_element ,
         it_tables   TYPE  tt_element ,
         it_fieldcat TYPE  lvc_t_fcat ,
         it_select   TYPE  tt_text    ,
         it_on       TYPE  tt_text    ,
         it_where    TYPE  tt_text    ,
         it_group    TYPE  tt_text    ,
         it_have     TYPE  tt_text    ,
         it_order    TYPE  tt_text    ,
         l_act       TYPE  i VALUE 0  ,
         l_type      TYPE  string     ,
         l_return    type  i          .

  PERFORM format_sql_adapter      USING  l_act
                               CHANGING  p_text     l_type
                                         it_fields  it_tables
                                         it_select  it_on
                                         it_where   it_group
                                         it_have    it_order
                                         l_return.


*& Show Exception
  IF g_exception IS INITIAL AND p_text[] IS NOT INITIAL.
    PERFORM load_text_from_table     USING g_editor
                                  CHANGING p_text .
    g_postion = 100 .

  ELSE .
    PERFORM prepare_alv_error_field_cat CHANGING it_fieldcat .

    PERFORM show_data_in_alv         USING g_grid
                                           g_exception
                                           it_fieldcat[] .
    g_postion = 50 .
  ENDIF .

  PERFORM set_splitter_postion       USING g_splitter
                                           g_postion .

ENDFORM .                    "format_sql





******************************************************
*&      Form  bud_sturcture
******************************************************
FORM bud_sturcture            USING p_text  TYPE  tt_text
                           CHANGING p_temp  TYPE  tt_text  .

  DATA:  it_fields   TYPE  tt_element ,
         it_tables   TYPE  tt_element ,
         it_fieldcat TYPE  lvc_t_fcat ,
         it_select   TYPE  tt_text    ,
         it_on       TYPE  tt_text    ,
         it_where    TYPE  tt_text    ,
         it_group    TYPE  tt_text    ,
         it_have     TYPE  tt_text    ,
         it_order    TYPE  tt_text    ,
         l_act       TYPE  i VALUE 0  ,
         l_type      TYPE  string     ,
         l_temp      TYPE  tt_text    ,
         l_return    type  i          .

  CLEAR : p_temp .
  l_temp  =  p_text .

  PERFORM format_sql_adapter      USING  l_act
                               CHANGING  l_temp     l_type
                                         it_fields  it_tables
                                         it_select  it_on
                                         it_where   it_group
                                         it_have    it_order
                                         l_return.


*& Show Exception
  IF g_exception IS INITIAL AND p_text[] IS NOT INITIAL.
    PERFORM get_stucture_define        USING it_fields
                                    CHANGING p_temp     .
  ELSE .
    PERFORM prepare_alv_error_field_cat CHANGING it_fieldcat .

    PERFORM show_data_in_alv           USING g_grid
                                             g_exception
                                             it_fieldcat[] .
    g_postion = 50 .
    PERFORM set_splitter_postion       USING g_splitter
                                             g_postion .

  ENDIF .


  g_postion = 70 .
  PERFORM set_splitter_postion       USING g_splitter1
                                           g_postion .

ENDFORM .                    "bud_sturcture

*& End

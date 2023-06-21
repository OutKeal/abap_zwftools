************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/28
*^ Include Name    : ZSQLEXPLORERF06
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Create A Dynamic SQL
*
*^ Other           :
************************************************************************








******************************************************
*&      Form  Get_Data_In_Dynamic_SQL
******************************************************
FORM get_data_in_dynamic_sql USING p_text    TYPE  tt_text
                                   p_type    TYPE  string
                                   p_fields  TYPE  tt_element
                                   p_select  TYPE  tt_text
                                   p_on      TYPE  tt_text
                                   p_where   TYPE  tt_text
                                   p_group   TYPE  tt_text
                                   p_have    TYPE  tt_text
                                   p_order   TYPE  tt_text .

  DATA : p_all     TYPE  i  VALUE 1 ,
         wa_text   TYPE  st_text    ,
         p_table   TYPE  REF TO data,
         p_line    TYPE  REF TO data,
         l_to      TYPE  string     ,
         l_length  TYPE  i          .


*& Get Field Statement
  l_length = 6 .
  PERFORM set_blank USING l_length
                 CHANGING p_select .

*& Get Table Statement
  l_length = 6 .
  PERFORM set_blank USING l_length
                 CHANGING p_on .


*& Get Where Statement
  l_length = 6 .
  PERFORM set_blank USING l_length
                 CHANGING p_where .

*& Get Group Statement
  l_length = 9 .
  PERFORM set_blank USING l_length
                 CHANGING p_group.

*& Get Have  Statement
  l_length = 6 .
  PERFORM set_blank USING l_length
                 CHANGING p_have.


*& Get Order Statement
  l_length = 9 .
  PERFORM set_blank USING l_length
                 CHANGING p_order.


*& Get Dynamic Structure
  PERFORM get_structure USING p_fields
                     CHANGING p_table .

  CHECK g_exception[] IS INITIAL .

  ASSIGN p_table->* TO <table> .

  CREATE DATA p_line LIKE LINE OF <table>.
  ASSIGN p_line->* TO <line> .

*& Check SQL Type
  PERFORM get_data USING  p_select  p_on
                          p_where   p_group
                          p_have    p_order
                          p_type
                CHANGING  <table>   <line>.

ENDFORM .                    "Get_Data_in_dynamic_SQL





******************************************************
*&      Form  Get_Structure
******************************************************
FORM get_structure USING  p_fields  TYPE tt_element
                CHANGING  p_ref     TYPE REF TO data .
  DATA: wa_component  TYPE  abap_componentdescr,
        it_component  TYPE  abap_component_tab,
        wa_strucdescr TYPE  REF TO cl_abap_structdescr,
        it_tabledescr TYPE  REF TO cl_abap_tabledescr,
        wa_field      TYPE  st_element,
        l_oref        TYPE  REF TO cx_root ,
        l_str         TYPE  string,
        l_msg         TYPE  string.

  LOOP AT p_fields INTO wa_field .
    CLEAR :  wa_component .
    wa_component-name  =  wa_field-alias .

    IF  wa_field-display(6) = c_count1 .
      CONCATENATE 'LVC_S_FCAT' '-'  'ROW_POS'
         INTO l_str .
    ELSE .
      CONCATENATE wa_field-source '-' wa_field-name
             INTO l_str .
    ENDIF .

    PERFORM case_to_upper CHANGING l_str .
    PERFORM case_to_upper CHANGING wa_component-name .

    TRY.

        wa_component-type ?= cl_abap_typedescr=>describe_by_name( l_str ).
        INSERT wa_component INTO TABLE it_component.

      CATCH cx_root INTO l_oref.
        l_msg  = l_oref->get_text( ).
        PERFORM append_error_message USING 4 l_msg .
      CLEANUP.
        CLEAR l_oref.
    ENDTRY .

  ENDLOOP .

  CHECK g_exception[] IS INITIAL .

  TRY.
      IF it_component IS NOT INITIAL.
        wa_strucdescr = cl_abap_structdescr=>create( it_component ).
        it_tabledescr = cl_abap_tabledescr=>create( p_line_type = wa_strucdescr ).
      ENDIF.
      CREATE DATA p_ref TYPE HANDLE it_tabledescr.
    CATCH cx_root INTO l_oref.
      l_msg  = l_oref->get_text( ).
      PERFORM append_error_message USING 4 l_msg .
    CLEANUP.
      CLEAR l_oref.
  ENDTRY .


ENDFORM .                    "get_structure





******************************************************
*&      Form  Get_Data
******************************************************
FORM get_data USING  p_field  TYPE  tt_text
                     p_table  TYPE  tt_text
                     p_where  TYPE  tt_text
                     p_group  TYPE  tt_text
                     p_have   TYPE  tt_text
                     p_order  TYPE  tt_text
                     p_type   TYPE  string
           CHANGING  p_data   TYPE  STANDARD TABLE
                     p_line   TYPE  any  .
  DATA : l_oref   TYPE REF TO cx_root ,
         wa_text  TYPE st_text        ,
         l_msg    TYPE string         .

  TRY.
      CASE p_type  .
        WHEN c_single  .
          SELECT SINGLE (p_field)
            INTO p_line
            FROM (p_table)
           WHERE (p_where)
           GROUP BY (p_group)
          HAVING (p_have).
          APPEND p_line TO p_data .

        WHEN c_distinct  .
          SELECT DISTINCT (p_field)
            INTO TABLE p_data
            FROM (p_table)
           WHERE (p_where)
           GROUP BY (p_group)
          HAVING (p_have)
           ORDER BY (p_order) .

        WHEN OTHERS .
          SELECT (p_field)
            INTO TABLE p_data
            FROM (p_table)
           WHERE (p_where)
           GROUP BY (p_group)
          HAVING (p_have)
           ORDER BY (p_order) .


      ENDCASE .

    CATCH cx_root INTO l_oref.
      l_msg  = l_oref->get_text( ).
      PERFORM append_error_message USING 4 l_msg .

    CLEANUP.
      CLEAR l_oref.

  ENDTRY .

ENDFORM .                    "Get_Data






******************************************************
*&      Form  Get_Field_Statement
******************************************************
FORM get_field_statement     USING p_element  TYPE tt_element
                          CHANGING p_field    TYPE tt_text   .
  DATA : wa_element TYPE  st_element ,
         wa_field   TYPE  st_text    ,
         l_temp     TYPE  string     .


  LOOP AT p_element INTO wa_element .

    wa_field-line  = wa_element-display .

    APPEND wa_field TO p_field  .

  ENDLOOP .


ENDFORM .                    "Get_Field_Statement





******************************************************
*&      Form  Set_Blank
******************************************************
FORM set_blank      USING l_length  TYPE  i
                 CHANGING p_text    TYPE  tt_text.
  DATA : wa_text  TYPE  st_text .

  CHECK p_text IS NOT INITIAL .

  READ TABLE p_text INTO wa_text INDEX 1 .
  wa_text-line(l_length) = '' .
  SHIFT wa_text-line BY l_length PLACES RIGHT .
  MODIFY p_text FROM wa_text INDEX 1 .


ENDFORM .                    "Set_Blank







*& End .

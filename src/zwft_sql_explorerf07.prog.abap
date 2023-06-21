************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/27
*^ Include Name    : ZSQLEXPLORERF07
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Format SQL
*
*^ Other           :
************************************************************************







******************************************************
*&      Form  Format_fields
******************************************************
FORM format_fields   USING  value(p_fields)  TYPE  tt_element
                            p_act            TYPE  i
                  CHANGING  p_select         TYPE  tt_text
                            p_text           TYPE  tt_text
                            p_type           TYPE  string  .
  DATA : wa_text   TYPE  st_text    ,
         l_line    TYPE  i VALUE 1  ,
         l_mod     TYPE  i VALUE 0  ,
         l_index   TYPE  i VALUE 0  ,
         l_string  TYPE  c LENGTH 120,
         wa_field  TYPE  st_element ,
         l_flag    TYPE  i VALUE 1  .

*& Display Key Word "Single" And "Distinct"
  LOOP AT p_text INTO wa_text .
    CASE wa_text-line .
      WHEN c_single OR c_distinct .
        p_type = wa_text-line .

      WHEN c_from OR c_where OR c_order .
        EXIT .
    ENDCASE .

    DELETE p_text .
  ENDLOOP .


  LOOP AT p_fields INTO wa_field .

    IF sy-tabix = 1 .
      IF p_type <> '' AND p_act <> 1 .
        CONCATENATE c_select p_type wa_field-display
               INTO l_string
           SEPARATED BY space .
        ADD 2 TO l_index .
      ELSE .
        CONCATENATE c_select wa_field-display
               INTO l_string
           SEPARATED BY space .
        ADD 1 TO l_index .
      ENDIF .
    ELSE .
      SHIFT wa_field-display BY c_fields_blank PLACES RIGHT .
      CONCATENATE l_string   wa_field-display
             INTO l_string
         SEPARATED BY space .
      ADD 1 TO l_index .
    ENDIF .

    l_mod = l_index MOD c_line_fields .

    IF l_mod = 0 .

      IF l_line <> 1 .
        SHIFT l_string BY 5 PLACES RIGHT.
      ENDIF .

      INSERT l_string INTO p_text INDEX l_line .
      APPEND l_string TO p_select .
      ADD 1 TO l_line  .
      CLEAR : l_string .
    ENDIF .

    AT LAST.
      CHECK l_string IS NOT INITIAL .
      IF l_line <> 1 .
        SHIFT l_string BY 5 PLACES RIGHT.
      ENDIF .
      INSERT l_string INTO p_text INDEX l_line .
      APPEND l_string TO p_select .
      ADD 1 TO l_line  .
    ENDAT .

  ENDLOOP .


ENDFORM .                    "Format_Fields








******************************************************
*&      Form  format_tables
******************************************************
FORM format_tables           USING  p_on      TYPE  tt_text
                          CHANGING  p_text    TYPE  tt_text.
  DATA : l_from_index TYPE  i       ,
         l_to         TYPE  string  ,
         wa_text      TYPE  st_text ,
         l_str        TYPE  string  ,
         l_delete     TYPE  i  VALUE 1 .

  CHECK p_on IS NOT INITIAL .

  CONCATENATE c_separ c_where  c_separ c_group
              c_separ c_have   c_separ c_order  c_separ
         INTO l_to .

  LOOP AT p_text INTO wa_text .
    IF wa_text-line = c_from .
      l_from_index  = sy-tabix .
      l_delete      = 0 .
    ENDIF .

    CONCATENATE c_separ wa_text-line c_separ
           INTO wa_text-line .
    IF l_to CS wa_text-line .
      EXIT .
    ENDIF .

    IF l_delete = 0 .
      DELETE p_text .
    ENDIF .

  ENDLOOP .

  CHECK l_from_index > 0 .
  INSERT LINES OF p_on INTO p_text INDEX l_from_index .


ENDFORM .                    "format_fields







******************************************************
*&      Form  Format_Where
******************************************************
FORM format_where            USING  p_where  TYPE  tt_text
                          CHANGING  p_text   TYPE  tt_text .
  DATA :  l_index      TYPE  i          ,
          l_to         TYPE  string     ,
          wa_text      TYPE  st_text    ,
          l_delete     TYPE  i  VALUE 1 .

  CHECK p_where IS NOT INITIAL .

  CONCATENATE c_separ c_group  c_separ c_have
              c_separ c_order  c_separ
         INTO l_to .

  LOOP AT p_text INTO wa_text .

    IF wa_text-line = c_where .
      l_index       = sy-tabix .
      l_delete      = 0 .
    ENDIF .

    CONCATENATE c_separ wa_text-line c_separ
           INTO wa_text-line .
    IF l_to CS wa_text-line .
      EXIT .
    ENDIF .

    IF l_delete = 0 .
      DELETE p_text .
    ENDIF .

  ENDLOOP .

  CHECK l_index > 0 .
  INSERT LINES OF p_where INTO p_text INDEX l_index .


ENDFORM .                    "Format_Where






******************************************************
*&      Form  Format_Group
******************************************************
FORM format_group            USING  p_group  TYPE  tt_text
                          CHANGING  p_text   TYPE  tt_text .
  DATA :  l_index      TYPE  i          ,
          l_to         TYPE  string     ,
          wa_text      TYPE  st_text    ,
          l_delete     TYPE  i  VALUE 1 .

  CHECK p_group  IS NOT INITIAL .

  CONCATENATE c_separ c_have
              c_separ c_order  c_separ
         INTO l_to .

  LOOP AT p_text INTO wa_text .

    IF wa_text-line = c_group .
      l_index       = sy-tabix .
      l_delete      = 0 .
    ENDIF .

    CONCATENATE c_separ wa_text-line c_separ
           INTO wa_text-line .

    IF l_to CS wa_text-line .
      EXIT .
    ENDIF .

    IF l_delete = 0 .
      DELETE p_text .
    ENDIF .

  ENDLOOP .

  CHECK l_index > 0 .
  INSERT LINES OF p_group INTO p_text INDEX l_index .

ENDFORM .                    "Format_Group




******************************************************
*&      Form  Format_have
******************************************************
FORM format_have             USING  p_have  TYPE  tt_text
                          CHANGING  p_text  TYPE  tt_text  .
  DATA :  l_index      TYPE  i          ,
          l_to         TYPE  string     ,
          wa_text      TYPE  st_text    ,
          l_delete     TYPE  i  VALUE 1 .

  CHECK p_have  IS NOT INITIAL .

  CONCATENATE c_separ c_order  c_separ
         INTO l_to .

  LOOP AT p_text INTO wa_text .

    IF wa_text-line = c_have  .
      l_index       = sy-tabix .
      l_delete      = 0 .
    ENDIF .

    CONCATENATE c_separ wa_text-line c_separ
           INTO wa_text-line .

    IF l_to CS wa_text-line .
      EXIT .
    ENDIF .

    IF l_delete = 0 .
      DELETE p_text .
    ENDIF .

  ENDLOOP .

  CHECK l_index > 0 .
  INSERT LINES OF p_have INTO p_text INDEX l_index .



ENDFORM .                    "Format_have





******************************************************
*&      Form  format_order
******************************************************
FORM format_order            USING  p_order  TYPE  tt_text
                          CHANGING  p_text    TYPE  tt_text .
  DATA :    l_index      TYPE  i          ,
            l_to         TYPE  string     ,
            wa_text      TYPE  st_text    ,
            l_delete     TYPE  i  VALUE 1 .

  CHECK p_order IS NOT INITIAL .

  CONCATENATE c_separ c_order  c_separ
         INTO l_to .

  LOOP AT p_text INTO wa_text .

    IF wa_text-line = c_order .
      l_index       = sy-tabix .
      l_delete      = 0 .
    ENDIF .

    CONCATENATE c_separ wa_text-line c_separ
           INTO wa_text-line .

    IF l_delete = 0 .
      DELETE p_text .
    ENDIF .

  ENDLOOP .

  CHECK l_index > 0 .
  INSERT LINES OF p_order INTO p_text INDEX l_index .


ENDFORM .                    "format_order



*& End

FUNCTION zwft_check_value.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(LINE) OPTIONAL
*"  TABLES
*"      TAB OPTIONAL
*"      RULE STRUCTURE  ZWFT_CHECK_RULE
*"      RET STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------


  FIELD-SYMBOLS: <fs_value> TYPE any.
  FIELD-SYMBOLS: <fs_tab> TYPE  STANDARD TABLE.
  DATA tabix TYPE sy-tabix.
  DATA msg TYPE char40.
  DATA: lt_doma_list LIKE TABLE OF zwft_doma_list WITH HEADER LINE.
  DATA: lt_doma_value LIKE TABLE OF zwft_doma_value WITH HEADER LINE.
  IF line IS INITIAL AND tab IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT rule.
    IF rule-rollname IS INITIAL.
      CONTINUE.
    ENDIF.
    lt_doma_list-rollname = rule-rollname.
    COLLECT lt_doma_list.
    CLEAR lt_doma_list.
  ENDLOOP.


  IF lt_doma_list[] IS NOT INITIAL.
    CALL FUNCTION 'ZWFT_DOMA_GET'
      TABLES
        doma_list  = lt_doma_list
        doma_value = lt_doma_value.
  ENDIF.

  SORT lt_doma_value BY rollname domval.

  IF line IS NOT INITIAL.
    LOOP AT rule.
      ASSIGN COMPONENT rule-fieldname OF STRUCTURE line TO <fs_value>.
      CHECK sy-subrc EQ 0.

      IF <fs_value> IS INITIAL AND rule-notnull = 'X'.
        PERFORM add_msg TABLES ret USING 'E' 002 rule-ddtext '' . "字段&1不能为空
        CONTINUE.
      ENDIF.

      IF rule-rollname IS NOT INITIAL.
        READ TABLE lt_doma_value WITH KEY rollname = rule-rollname
        domval = <fs_value> BINARY SEARCH.
        IF sy-subrc NE 0.
          PERFORM add_msg TABLES ret USING 'E' 003  rule-ddtext <fs_value> ."&1字段值&2不正确
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF tab IS NOT INITIAL.
    LOOP AT tab .
      tabix = sy-tabix.
      LOOP AT rule.
        ASSIGN COMPONENT rule-fieldname OF STRUCTURE tab TO <fs_value>.
        CHECK sy-subrc EQ 0.

        IF <fs_value> IS INITIAL AND rule-notnull = 'X'.
          PERFORM add_msg TABLES ret USING 'E' 002 tabix rule-ddtext .
          CONTINUE.
        ENDIF.

        IF rule-rollname IS NOT INITIAL.
          READ TABLE lt_doma_value WITH KEY rollname = rule-rollname
          domval = <fs_value> BINARY SEARCH.
          IF sy-subrc NE 0.
            PERFORM add_msg TABLES ret USING 'E' 003 rule-fieldname <fs_value>.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.

FORM add_msg TABLES ret STRUCTURE bapiret2
USING type number fieldname value .

  ret-type = type.
  ret-number = number.
  ret-id = 'ZWFT'.
  ret-field = fieldname.
  ret-message_v1 = fieldname.
  ret-message_v2 = value.
  CONDENSE ret-message_v1 NO-GAPS.
  MESSAGE ID ret-id  TYPE ret-type  NUMBER ret-number
  INTO ret-message
  WITH ret-message_v1 ret-message_v2.
  APPEND ret.
  CLEAR ret.

ENDFORM.

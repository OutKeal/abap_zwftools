FUNCTION zwft_doma_value_text_get.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_DOMA) TYPE  DOMNAME
*"     REFERENCE(I_VALUE)
*"     VALUE(I_LANGU) TYPE  SYLANGU DEFAULT SY-LANGU
*"  EXPORTING
*"     REFERENCE(E_TEXT)
*"     REFERENCE(E_DOMA) TYPE  DD01V
*"  EXCEPTIONS
*"      NOT_FOUND
*"      ERROR
*"----------------------------------------------------------------------

  TYPES:
    BEGIN OF s_doma_buf,
      doma      TYPE domname,
      value(20) TYPE c,
      langu     TYPE sylangu,
      s_doma    TYPE dd01v,
      text(100) TYPE c,
    END OF s_doma_buf,
    ts_doma_buf TYPE SORTED TABLE OF s_doma_buf WITH UNIQUE KEY doma value langu.

  DATA: l_dd01v  TYPE dd01v,
        l_dd07v  TYPE dd07v,
        lt_dd07v TYPE TABLE OF dd07v.

  DATA: l_tname  TYPE tabname,
        l_index  TYPE i,
        l_dfies  TYPE dfies,
        l_where  TYPE rsdswhere,
        lt_dfies TYPE TABLE OF dfies,
        lt_txtfi TYPE TABLE OF fieldname,
        lt_where TYPE TABLE OF rsdswhere.

  STATICS: lt_doma_buf TYPE ts_doma_buf.
  DATA: ls_doma_buf TYPE s_doma_buf,
        l_use_buf   TYPE abap_bool.
  FIELD-SYMBOLS: <ls_doma_buf> TYPE s_doma_buf.

  l_use_buf = abap_true.

  IF l_use_buf EQ abap_true.
    ls_doma_buf-doma  = i_doma.
    ls_doma_buf-value = i_value.
    ls_doma_buf-langu = i_langu.
    READ TABLE lt_doma_buf FROM ls_doma_buf
                         ASSIGNING <ls_doma_buf>.
    IF sy-subrc IS INITIAL.
      e_text = <ls_doma_buf>-text.
      IF e_doma IS REQUESTED.
        e_doma = <ls_doma_buf>-s_doma.
      ENDIF.
      EXIT.
    ENDIF.
  ENDIF.


*--- Find out if enity-table or value-table exists
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = i_doma
      langu         = i_langu
    IMPORTING
      dd01v_wa      = l_dd01v
    TABLES
      dd07v_tab     = lt_dd07v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING error.
  ENDIF.

  ls_doma_buf-s_doma = l_dd01v.
  IF e_doma IS REQUESTED.
    e_doma = l_dd01v.
  ENDIF.

  IF l_dd01v-valexi IS INITIAL.
*--- Look for text in foreign-texttable
* find text table
    CALL FUNCTION 'DDUT_TEXTTABLE_GET'
      EXPORTING
        tabname   = l_dd01v-entitytab
      IMPORTING
        texttable = l_tname.

* get fields of text table
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = l_tname
      TABLES
        dfies_tab = lt_dfies
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

* check field-structure of text-table (SPRAS on 2nd position)

    READ TABLE lt_dfies TRANSPORTING NO FIELDS
                        WITH KEY datatype = 'LANG'.

    IF NOT ( sy-subrc = 0 AND sy-tabix = 2 ).
      RAISE not_found.
    ENDIF.


* find selection criteria and bulid where-clause

    READ TABLE lt_dfies INTO l_dfies WITH KEY
                             checktable = l_dd01v-entitytab.
    l_index = sy-tabix.
    CONCATENATE l_dfies-fieldname ' = ' '''' i_value ''''
                INTO l_where.
    APPEND l_where TO lt_where.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

* find text-field as next field and build select projection

    l_index = l_index + 1.
    LOOP AT lt_dfies INTO l_dfies FROM l_index WHERE keyflag = ''.
      EXIT.
    ENDLOOP.
    IF NOT ( sy-subrc = 0 AND l_dfies-datatype = 'CHAR' ).
      RAISE not_found.
    ENDIF.

    APPEND l_dfies-fieldname TO lt_txtfi.

* dynamically select required text-data from text-table

    SELECT SINGLE (lt_txtfi)   FROM (l_tname)
                               INTO e_text
                               WHERE spras = i_langu
                               AND  (lt_where).

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

  ELSE.
*--- Find text in value-table
    READ TABLE lt_dd07v INTO l_dd07v WITH KEY domvalue_l = i_value.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

    e_text = l_dd07v-ddtext.

  ENDIF.

  IF l_use_buf EQ abap_true.
    ls_doma_buf-text = e_text.
    INSERT ls_doma_buf INTO TABLE lt_doma_buf.
  ENDIF.

ENDFUNCTION.

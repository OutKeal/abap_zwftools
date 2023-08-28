FUNCTION zwft_doma_get.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(SPRAS) TYPE  SPRAS DEFAULT '1'
*"  TABLES
*"      DOMA_LIST STRUCTURE  ZWFT_DOMA_LIST
*"      DD04T STRUCTURE  DD04T OPTIONAL
*"      DOMA_VALUE STRUCTURE  ZWFT_DOMA_VALUE
*"      REF_DATA TYPE  TABLE OPTIONAL
*"----------------------------------------------------------------------
  CHECK doma_list[] IS NOT INITIAL.

  DATA: dd04vv TYPE TABLE OF dd04vv WITH HEADER LINE.
  DATA: dd04vv_def TYPE TABLE OF dd04vv WITH HEADER LINE.
  DATA: dd04vv_tab TYPE TABLE OF dd04vv WITH HEADER LINE.
  DATA: dd08l TYPE TABLE OF dd08l WITH HEADER LINE.

  DATA: it_dd03l TYPE TABLE OF dd03l WITH HEADER LINE.
  DATA: dd07t TYPE TABLE OF dd07t WITH HEADER LINE.
  DATA: tabname TYPE tabname.

  DATA: keyfname TYPE fieldname.
  DATA: textfname TYPE fieldname.
  DATA: sprasname TYPE fieldname.

  DATA: l_field(100) TYPE c,
        l_where      TYPE rsdswhere,
        lt_where     TYPE TABLE OF rsdswhere.

  FIELD-SYMBOLS: <dyn_table> TYPE table,
                 <dyn_wa>    TYPE any.
  DATA: dyn_table TYPE REF TO data.
  DATA: dyn_wa TYPE REF TO data.
  DATA: val_field TYPE char40.
  DATA: txt_field TYPE char40.

  CHECK doma_list[] IS NOT INITIAL.

  SELECT * INTO TABLE dd04vv FROM dd04vv
                  FOR ALL ENTRIES IN doma_list
                  WHERE rollname = doma_list-rollname ."AND  dtelmaster = spras.

  CHECK sy-subrc EQ 0.
  SELECT * INTO TABLE dd04t FROM dd04t
                  FOR ALL ENTRIES IN dd04vv
                  WHERE rollname = dd04vv-rollname .


  LOOP AT dd04vv.                                   "带域的数据元素
    IF dd04vv-valexi = 'X'.                         "存在固定值
      dd04vv_def = dd04vv.
      APPEND dd04vv_def.
      CLEAR dd04vv_def.
    ELSEIF dd04vv-entitytab IS NOT INITIAL.         "值表不为空
      dd04vv_tab = dd04vv.
      APPEND dd04vv_tab.
      CLEAR dd04vv_tab.
    ENDIF.
  ENDLOOP.

  IF dd04vv_def[] IS NOT INITIAL.                   "有固定域值的情况下
    SELECT * FROM dd07t INTO TABLE dd07t            "表： 用于域固定值的文本（语言相关），根据域名取数-对应文本
                  FOR ALL ENTRIES IN dd04vv_def
                  WHERE domname = dd04vv_def-domname
                  AND ddlanguage = spras.
    IF sy-subrc EQ 0.
      LOOP AT dd07t.
        READ TABLE dd04vv_def WITH KEY domname = dd07t-domname.
        IF sy-subrc EQ 0.
          APPEND VALUE #( rollname = dd04vv_def-rollname
                                        domval = dd07t-domvalue_l
                                        ddtext = dd07t-ddtext
                                        ) TO doma_value.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF dd04vv_tab[] IS NOT INITIAL.
    SELECT * FROM dd08l INTO TABLE dd08l
                FOR ALL ENTRIES IN dd04vv_tab
                WHERE checktable = dd04vv_tab-entitytab           "值表
                AND frkart = 'TEXT'                               "外文建语义
                AND as4local = 'A'.                               "激活的
    IF dd08l[] IS NOT INITIAL.
      SELECT * FROM dd03l INTO TABLE it_dd03l "获取文本表的表结构
                FOR ALL ENTRIES IN dd08l
                WHERE tabname = dd08l-tabname.
    ENDIF.
    SELECT * FROM dd03l APPENDING TABLE it_dd03l "获取值表的表结构
                FOR ALL ENTRIES IN dd04vv_tab
                WHERE tabname = dd04vv_tab-entitytab.

    DELETE  it_dd03l WHERE fieldname = 'MANDT' ."删除表结构里的客户端/语言字段

    SORT it_dd03l BY tabname position.

    LOOP AT dd04vv_tab.
      CLEAR lt_where[].
      CLEAR l_where.

      READ TABLE dd08l WITH KEY checktable = dd04vv_tab-entitytab.
      IF sy-subrc NE 0.
        tabname = dd04vv_tab-entitytab.
      ELSE.
        tabname = dd08l-tabname.
      ENDIF.

      LOOP AT it_dd03l WHERE tabname = tabname
                                        AND keyflag = ' '
                                        AND fieldname <> 'SPRAS'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        textfname = it_dd03l-fieldname.
      ELSE.
        CONTINUE."值表的结构'
      ENDIF.

      READ TABLE it_dd03l WITH KEY tabname = tabname
                                                         domname = dd04vv_tab-domname.
      IF sy-subrc NE 0.
        CONTINUE.
      ELSE.
        keyfname = it_dd03l-fieldname.
      ENDIF.

      PERFORM frm_textfname USING dd04vv_tab-rollname CHANGING textfname.

      CREATE DATA dyn_table TYPE TABLE OF (tabname).
      ASSIGN dyn_table->* TO <dyn_table>.
      CHECK sy-subrc EQ 0.

      CREATE DATA dyn_wa LIKE LINE OF <dyn_table>.
      ASSIGN dyn_wa->* TO <dyn_wa>.


      READ TABLE doma_list WITH KEY rollname = dd04vv_tab-rollname.
      IF sy-subrc EQ 0
      AND doma_list-ref_fieldname IS NOT INITIAL
      AND ref_data[] IS NOT INITIAL.
        LOOP AT ref_data ASSIGNING FIELD-SYMBOL(<ref_line>).
          ASSIGN COMPONENT doma_list-ref_fieldname OF STRUCTURE <ref_line> TO FIELD-SYMBOL(<fs_value>).
          IF sy-subrc EQ 0.
            IF <fs_value> IS NOT INITIAL.
              l_where-line = |{ keyfname } = '{ <fs_value> }'|.
              APPEND l_where TO lt_where.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lt_where IS NOT INITIAL.
          SORT lt_where.
          DELETE ADJACENT DUPLICATES FROM lt_where.
          LOOP AT lt_where INTO l_where.
            IF sy-tabix = 1.
              l_where-line = |(  { l_where-line }|.
            ELSE.
              l_where-line = |OR { l_where-line }|.
            ENDIF.

            IF sy-tabix =  lines( lt_where ) .
              l_where-line = |{ l_where-line } )|.
            ENDIF.
            MODIFY lt_where FROM l_where.
          ENDLOOP.
        ENDIF.
      ENDIF.

      READ TABLE dd08l WITH KEY tabname = tabname.
      IF sy-subrc EQ 0.
        READ TABLE it_dd03l INTO DATA(ls_dd03l) WITH KEY tabname = tabname rollname = 'SPRAS'.
        IF sy-subrc EQ 0.
          sprasname = ls_dd03l-fieldname.
          IF lt_where IS NOT INITIAL.
            l_where-line =  |AND { sprasname } = '{ spras }'|.
          ELSE.
            l_where-line = |{ sprasname } = '{ spras }'|.
          ENDIF.
          APPEND l_where TO lt_where.
        ENDIF.
      ENDIF.

      SELECT  * FROM (tabname) INTO TABLE  <dyn_table>
        WHERE (lt_where).

      IF sy-subrc EQ 0.
        LOOP AT <dyn_table> INTO <dyn_wa>.
          CLEAR txt_field.
          ASSIGN COMPONENT keyfname OF STRUCTURE <dyn_wa> TO FIELD-SYMBOL(<key_value>).
          CHECK sy-subrc = 0.
          ASSIGN COMPONENT textfname OF STRUCTURE <dyn_wa> TO FIELD-SYMBOL(<text_value>).
          CHECK sy-subrc = 0.
          APPEND VALUE #(  rollname = dd04vv_tab-rollname
                                        domval = <key_value>
                                        ddtext = <text_value>
                                     ) TO doma_value.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM doma_value COMPARING rollname domval.

ENDFUNCTION.

FORM frm_textfname USING rollname textname.
  CASE rollname.
    WHEN 'MEINS' . textname = 'MSEHT'.

  ENDCASE.


ENDFORM.

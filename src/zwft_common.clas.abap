class ZWFT_COMMON definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_fcat,
    col_pos   TYPE col_pos,
    fieldname TYPE fieldname,
    EDIT      TYPE lvc_edit,
    emphasize TYPE lvc_emphsz,
    HOTSPOT   TYPE lvc_hotspt,
    coltext   TYPE lvc_txtcol,
    tech      TYPE lvc_tech,
  END OF ty_fcat .
  types:
    BEGIN OF ty_doma_list,
    rollname      TYPE rollname,
    def_fieldname TYPE fieldname,
  END OF ty_doma_list .
  types:
    BEGIN OF ty_doma_value,
    rollname TYPE rollname,
    domval   TYPE char50,
    ddtext   TYPE val_text,
  END OF ty_doma_value .

  data:
    doma_list TYPE TABLE OF ty_doma_list .
  data:
    doma_value TYPE TABLE OF ty_doma_value .

  class-methods GET_GUID32
    returning
      value(RV_GUID32) type SYSUUID_C32 .
  class-methods GET_GUID16
    returning
      value(RV_GUID16) type SYSUUID_X16 .
  class-methods GET_LOGSYS
    returning
      value(RV_LOGSYS) type LOGSYS .
  class-methods GET_ENCODING
    importing
      value(I_EXTERNAL_NAME) type CSEQUENCE
    returning
      value(R_ENCODING) type ABAP_ENCODING .
  class-methods GET_NR_NUMBER
    importing
      !IV_NR type NRNR
      !IV_OBJECT type NROBJ
      !IV_YEAR type INRI-TOYEAR optional
      !IV_NO_BUFFER type ABAP_BOOL optional
    returning
      value(RV_NUMBER) type ref to DATA .
  class-methods BUILD_MESSAGE_TEXT
    changing
      !CS_RETURN type BAPIRET2 optional
      !CT_RETURN type BAPIRET2_TAB optional .
  class-methods SEARCH_VENDOR
    changing
      !LIFNR type ANY .
  class-methods SEARCH_CUSTOMER
    changing
      !KUNNR type ANY .
  class-methods ADD_GOS_RELATIONSHIP
    importing
      value(OBJKEY1) type ANY optional
      value(OBJTYPE1) type SWO_OBJTYP optional
      value(OBJKEY2) type ANY optional
      value(OBJTYPE2) type SWO_OBJTYP optional
      value(RELATION) type OBLRELTYPE default 'OB' .
  class-methods CONFIRM
    importing
      value(IV_TEXT) type CLIKE optional
    returning
      value(EV_RESULT) type ABAP_BOOL .
  class-methods GET_USER_NAME
    importing
      value(IV_USER_ID) type CHAR12 optional
    returning
      value(EV_USER_FULL_NAME) type STRING .
  class-methods NUMBER_CHECK
    importing
      !IV_STRING type STRING
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods NUMBER_INPUT
    changing
      !VALUE type ANY optional
      !NUMBER type ANY optional
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods DATE_OUTPUT
    importing
      value(DATE) type DATUM
      value(SPLITER) type CHAR1 optional
    returning
      value(RV_DATE) type CHAR10 .
  class-methods DATE_INPUT
    importing
      !VALUE type ANY
    changing
      value(DATE) type DATUM
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods CALL_TRANSATION
    importing
      value(TYPE) type CHAR10
      value(KEY1) type ANY
      value(KEY2) type ANY optional
      value(KEY3) type ANY optional .
  class-methods FILE_DOWNLOAD_TO_CSV
    importing
      !DATA type TABLE .
  class-methods FILE_DOWNLOAD_TO_EXCEL
    importing
      value(DATA) type TABLE .
  class-methods FILE_GET_PATH
    importing
      value(EXTNAME) type STRING default 'XLSX'
    returning
      value(RV_FILE) type RLGRAP-FILENAME .
  class-methods FILE_DOWNLOAD_TEMPLATE
    importing
      !IV_OBJID type W3OBJID
      !IV_FILENAME type RLGRAP-FILENAME .
  class-methods PROGRESSBAR_SHOW
    importing
      !IV_CURRENT type I
      !IV_TOTAL type I
      !IV_MSG type STRING optional .
  class-methods DOMA_VALUE_CHECK
    importing
      value(I_DOMA) type DOMNAME
      value(I_VALUE) type ANY
      value(I_LANGU) type SYLANGU default SY-LANGU
    exporting
      value(E_TEXT) type ANY
      value(E_DOMA) type DD01V
    returning
      value(RV_OK) type ABAP_BOOL
    exceptions
      ERROR .
  class-methods DOMA_VALUE_GET_MULTIPLE
    importing
      value(DOMA_LIST) like DOMA_LIST
      value(REF_DATA) type TABLE optional
    returning
      value(DOMA_VALUE) like DOMA_VALUE .
  class-methods DOMA_VALUE_GET_SINGLE
    importing
      value(ROLLNAME) type ROLLNAME
    returning
      value(DOMA_VALUE) like DOMA_VALUE .
  class-methods SET_DEFAULT_VALUE_LINE
    importing
      value(IT_CONFIG) type ref to DATA
    changing
      value(IS_DATA) type ref to DATA .
  class-methods SET_DEFAULT_VALUE_TABLE
    importing
      value(IT_CONFIG) type ref to DATA
    changing
      value(IT_DATA) type ref to DATA .
  class-methods SET_ADMIN_VALUE_CREATE
    changing
      value(IS_DATA) type ref to DATA .
  class-methods SET_ADMIN_VALUE_MODIFY
    changing
      value(IS_DATA) type ref to DATA .
  class-methods SET_INIT_SDATE
    importing
      value(DAYS) type INT2 default 15
    changing
      value(SDATE) type ref to DATA .
  class-methods SET_INIT_ICON
    importing
      value(STATUS) type CHAR1
    returning
      value(ICON) type ICON_D .
  class-methods FCAT_SET_VALUE
    importing
      value(CONFIG) type DATA
    changing
      value(FCAT) type LVC_T_FCAT .
  class-methods FCAT_FROM_ITAB
    importing
      !IT_TABLE type STANDARD TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZWFT_COMMON IMPLEMENTATION.


  METHOD add_gos_relationship.
    DATA: borident1 TYPE borident.
    DATA: borident2 TYPE borident.

    borident1-objkey = objkey1.
    borident1-objtype = objtype1.
    borident2-objkey = objkey2.
    borident2-objtype = objtype2.

    CALL FUNCTION 'BINARY_RELATION_CREATE'
    EXPORTING
      obj_rolea      = objkey1
      obj_roleb      = borident2
      relationtype   = relation
    EXCEPTIONS
      no_model       = 1
      internal_error = 2
      unknown        = 3
      OTHERS         = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD build_message_text.
    IF cs_return IS SUPPLIED.
      IF cs_return IS INITIAL.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = cs_return-ID
          msgnr               = cs_return-NUMBER
          msgv1               = cs_return-message_v1
          msgv2               = cs_return-message_v2
          msgv3               = cs_return-message_v3
          msgv4               = cs_return-message_v4
        IMPORTING
          message_text_output = cs_return-MESSAGE.
      ENDIF.
    ENDIF.
    IF ct_return IS SUPPLIED.
      LOOP AT ct_return ASSIGNING FIELD-SYMBOL(<ls_return>).
        IF <ls_return>-MESSAGE IS INITIAL.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = <ls_return>-ID
            msgnr               = <ls_return>-NUMBER
            msgv1               = <ls_return>-message_v1
            msgv2               = <ls_return>-message_v2
            msgv3               = <ls_return>-message_v3
            msgv4               = <ls_return>-message_v4
          IMPORTING
            message_text_output = <ls_return>-MESSAGE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD call_transation.
    CASE TYPE.
    WHEN 'EBAN' OR 'PR' OR 'BUS2015'."采购申请
      SET PARAMETER ID 'BAN' FIELD key1.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    WHEN 'EKKO' OR 'PO' OR 'STO' OR 'BUS2012'.  "采购订单
      SET PARAMETER ID 'BES' FIELD key1.
      CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.

    WHEN 'ASN' . "内向交货单"
      SET PARAMETER ID 'VL' FIELD key1.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.

    WHEN 'VBAK' OR 'SO' OR 'BUS2032'. "销售订单"
      SET PARAMETER ID 'AUN' FIELD key1.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'LIKP' OR 'DN' OR 'VL'. "交货单"
      SET PARAMETER ID 'VL' FIELD key1.
      CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

    WHEN 'VBRK' OR 'VF' OR 'RV' OR 'BUS2037'. "发票"
      SET PARAMETER ID 'VF' FIELD key1.
      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

    WHEN 'RESB' OR 'RS' OR 'BUS2093'. "预留"
      SET PARAMETER ID 'RES' FIELD key1.
      CALL TRANSACTION  'MB23' AND SKIP FIRST SCREEN.

    WHEN 'MKPF' OR 'MB' OR 'BUS2017'. "商品凭证"
      CALL FUNCTION 'MIGO_DIALOG'
      EXPORTING
        i_action = 'A04'
        i_refdoc = 'R02'
        i_mblnr  = key1
        i_mjahr  = key2.

    WHEN 'RBKP' OR 'MR' OR 'BUS2081'.            "发票校验"
      SET PARAMETER ID 'RBN' FIELD key1.
      SET PARAMETER ID 'GJR' FIELD key2.
      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    WHEN 'BKPF' OR 'FB' .  "会计凭证
      SET PARAMETER ID 'BLN' FIELD key1.
      SET PARAMETER ID 'GJR' FIELD key2.
      SET PARAMETER ID 'BUK' FIELD key3.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

    WHEN 'BUS1001' . "商品"
      SET PARAMETER ID 'MAT' FIELD key1.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'MARA' OR 'MATNR' OR 'BUS1001001' . "商品"
      SET PARAMETER ID 'MAT' FIELD key1.
      CALL TRANSACTION 'MM43' AND SKIP FIRST SCREEN.

    WHEN 'KNA1' OR 'LFA1' OR 'KUNNR' OR 'LIFNR' OR 'BUS1006'. "客户/供应商"
      SET PARAMETER ID 'BPA' FIELD key1.
      SUBMIT r_ftr_display_bp WITH p_bp = key1 AND RETURN.

    WHEN 'AUFK' OR 'BUS2075'.  "内部订单"
      SET PARAMETER ID 'ANR' FIELD key1.
      CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.

    WHEN 'SKA1' OR 'HKONT' OR ' BUS3006'. "科目"
      SET PARAMETER ID 'SAK' FIELD key1.
      SET PARAMETER ID 'BUK' FIELD key2.
      CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN.

    WHEN 'MAST' OR 'BOM' OR 'BUS3006'. "BOM"
      SET PARAMETER ID 'MAT' FIELD key1.
      SET PARAMETER ID 'WRK' FIELD key2.
      SET PARAMETER ID 'CSV' FIELD key3.
      CALL TRANSACTION 'CS03' AND SKIP FIRST SCREEN.

    WHEN 'ANLA' OR 'ANLN1' OR 'BUS1022'.   "固定资产
      SET PARAMETER ID 'AN1' FIELD key1.
      SET PARAMETER ID 'BUK' FIELD key2.
      CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.

    WHEN 'T001W' OR 'WERKS' OR 'BUS1069'. "地点
      SET PARAMETER ID 'WRK' FIELD key1.
      CALL TRANSACTION 'WB03' AND SKIP FIRST SCREEN.

    WHEN 'AFKO' OR 'AUFNR' OR 'BUS2005'."生产订单
      SET PARAMETER ID 'ANR' FIELD key1.
      CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.

    WHEN 'EDIDC' OR 'IDOC'."IDOC
      SUBMIT idoc_tree_control WITH docnum = key1 AND RETURN.

    WHEN 'XML' OR 'PROXY'.
      SUBMIT rsxmb_display_msg_vers_new WITH msgguid = key1
      AND RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD confirm.

    DATA lv_msg TYPE char100.
    DATA lv_ans TYPE char1.
    CLEAR lv_ans.

    ev_result = abap_false.

    lv_msg = COND #( WHEN iv_text IS INITIAL THEN '确定么?' ELSE iv_text ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = lv_msg
      text_button_1         = '确定'
      icon_button_1         = '@01@' "icon_checked
      text_button_2         = '取消'
      icon_button_2         = '@02@' "icon_incomplete
      default_button        = '2'
      display_cancel_button = ''
    IMPORTING
      answer                = lv_ans
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
    IF sy-subrc EQ 0 AND lv_ans = '1'.
      ev_result = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD date_input.
    DATA:flag TYPE char1.
    DATA(len) = STRLEN( VALUE ).
    DATA:year TYPE char4.
    DATA:month TYPE char2.
    DATA:day TYPE char2.

    rv_ok = abap_false.
    IF len < 8 OR len > 10.
      CLEAR DATE.
      RETURN.
    ENDIF.

    FIND '.' IN VALUE.
    IF sy-subrc EQ 0.
      flag = '.'.
    ENDIF.

    FIND '/' IN VALUE.
    IF sy-subrc EQ 0.
      flag = '/'.
    ENDIF.

    FIND '-' IN VALUE.
    IF sy-subrc EQ 0.
      flag = '-'.
    ENDIF.

    CASE flag.
    WHEN ''.
      IF len = 8.
        DATE = VALUE.
      ELSE.
        CLEAR DATE.
        RETURN.
      ENDIF.
    WHEN OTHERS.

      SPLIT VALUE AT flag INTO year month day.
      DO 4 - STRLEN( year ) TIMES.
        year = '0' && year.
      ENDDO.

      DO 2 - STRLEN( month ) TIMES.
        month = '0' && month.
      ENDDO.

      DO 2 - STRLEN( day ) TIMES.
        day = '0' && day.
      ENDDO.

      DATE = year && month && day.

      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = DATE
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
      IF sy-subrc <> 0.
        CLEAR DATE.
        RETURN.
      ELSE.
        rv_ok = abap_true.
      ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD date_output.
    IF spliter IS INITIAL.
      rv_date = |{ DATE DATE = ENVIRONMENT }|.
    ELSE.
      rv_date = |{ DATE+0(4) }{ spliter }{ DATE+4(2) }{ spliter }{ DATE+6(2) }|.
    ENDIF.
  ENDMETHOD.


  METHOD doma_value_check.
    TYPES:
    BEGIN OF s_doma_buf,
      doma      TYPE domname,
      VALUE(20) TYPE C,
      langu     TYPE sylangu,
      s_doma    TYPE dd01v,
      TEXT(100) TYPE C,
    END OF s_doma_buf,
    ts_doma_buf TYPE SORTED TABLE OF s_doma_buf WITH UNIQUE KEY doma VALUE langu.

    DATA: l_dd01v  TYPE dd01v,
          l_dd07v  TYPE dd07v,
          lt_dd07v TYPE TABLE OF dd07v.

    DATA: l_tname  TYPE tabname,
          l_where  TYPE rsdswhere,
          lt_txtfi TYPE TABLE OF fieldname,
          lt_where TYPE TABLE OF rsdswhere.

    STATICS: lt_doma_buf TYPE ts_doma_buf.
    DATA: ls_doma_buf TYPE s_doma_buf,
          l_use_buf   TYPE abap_bool.
    FIELD-SYMBOLS: <ls_doma_buf> TYPE s_doma_buf.


    rv_ok = abap_false.
    l_use_buf = abap_true.

    IF l_use_buf EQ abap_true.
      ls_doma_buf-doma  = i_doma.
      ls_doma_buf-VALUE = i_value.
      ls_doma_buf-langu = i_langu.
      READ TABLE lt_doma_buf FROM ls_doma_buf
      ASSIGNING <ls_doma_buf>.
      IF sy-subrc IS INITIAL.
        e_text = <ls_doma_buf>-TEXT.
        IF e_doma IS REQUESTED.
          e_doma = <ls_doma_buf>-s_doma.
        ENDIF.
        rv_ok = abap_true.
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

      IF l_tname IS INITIAL.
        l_tname = l_dd01v-entitytab.
      ENDIF.

* get fields of text table
      SELECT * FROM dd03l
      WHERE tabname = @l_tname
      INTO TABLE @DATA(lt_dd03l).
      IF sy-subrc <> 0.
        RAISE error.
      ENDIF.

      SORT lt_dd03l BY POSITION.
* find selection criteria and bulid where-clause

      READ TABLE lt_dd03l INTO DATA(ls_dd03l) WITH KEY
            domname = i_doma
            keyflag = 'X'.
      l_where-LINE = |{ ls_dd03l-fieldname } = '{ i_value }'|.
      APPEND l_where TO lt_where.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.


* check field-structure of text-table (SPRAS on 2nd position)

      READ TABLE lt_dd03l INTO ls_dd03l
      WITH KEY datatype = 'LANG'.
      IF  sy-subrc = 0 .
        l_where-LINE = |AND { ls_dd03l-fieldname } = '{ i_langu }'|.
        APPEND l_where TO lt_where.
      ENDIF.

* find text-field as next field and build select projection


      LOOP AT lt_dd03l INTO ls_dd03l WHERE keyflag = ''
      AND datatype = 'CHAR'
      AND fieldname <> 'LAND1'."KNA1的异常
        EXIT.
      ENDLOOP.
      IF NOT ( sy-subrc = 0 ).
        RETURN.
      ENDIF.

      APPEND ls_dd03l-fieldname TO lt_txtfi.

* dynamically select required text-data from text-table

      SELECT SINGLE (lt_txtfi)   FROM (l_tname)
      INTO e_text
      WHERE (lt_where).

      IF sy-subrc <> 0.
        RETURN.
      ELSE.
        rv_ok = abap_true.
      ENDIF.

      ELSE.
*--- Find text in value-table
      READ TABLE lt_dd07v INTO l_dd07v WITH KEY domvalue_l = i_value.
      IF sy-subrc EQ 0.
        rv_ok = abap_true.
        e_text = l_dd07v-ddtext.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    IF l_use_buf EQ abap_true.
      ls_doma_buf-TEXT = e_text.
      INSERT ls_doma_buf INTO TABLE lt_doma_buf.
    ENDIF.


  ENDMETHOD.


  METHOD doma_value_get_multiple.
    CALL FUNCTION 'ZWFT_DOMA_GET'
*     EXPORTING
*       SPRAS            = '1'
    TABLES
      doma_list  = doma_list
*       DD04T      =
      doma_value = doma_value
      ref_data   = ref_data.

  ENDMETHOD.


  METHOD doma_value_get_single.
    DATA:lt_doma_list TYPE TABLE OF ty_doma_list.
    APPEND VALUE #( rollname = rollname ) TO lt_doma_list.
    CALL FUNCTION 'ZWFT_DOMA_GET'
*    EXPORTING
*      SPRAS            = '1'
    TABLES
      doma_list  = lt_doma_list
*       DD04T      =
      doma_value = doma_value
*       ref_data   =
      .


  ENDMETHOD.


  METHOD fcat_from_itab.
    DATA: TABLE TYPE REF TO DATA.
    CREATE DATA TABLE LIKE it_table.
    ASSIGN TABLE->* TO FIELD-SYMBOL(<table>).
    TRY.
      cl_salv_table=>factory( IMPORTING
      r_salv_table   = DATA(salv_table)
      CHANGING
        t_table        = <table>  ).
      rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      r_columns      = salv_table->get_columns( ) " ALV Filter
      r_aggregations = salv_table->get_aggregations( ) )." ALV Aggregations
    CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD FCAT_SET_VALUE.

    FIELD-SYMBOLS: <ft_config> TYPE TABLE.
    FIELD-SYMBOLS: <fs_config> TYPE ANY.

    DATA:ls_fcat TYPE ty_fcat.
    DATA:lt_fcat TYPE TABLE OF ty_fcat .

    ASSIGN config->* TO <ft_config>.
    LOOP AT <ft_config> ASSIGNING <fs_config>.
      MOVE-CORRESPONDING <fs_config> TO ls_fcat.
      APPEND ls_fcat TO lt_fcat.
    ENDLOOP.

    LOOP AT fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname = <fs_fcat>-fieldname.
      IF sy-subrc EQ 0.
        <fs_fcat> = VALUE #( BASE <fs_fcat>
        col_pos = ls_fcat-col_pos
        EDIT = ls_fcat-EDIT
        emphasize = ls_fcat-emphasize
        HOTSPOT = ls_fcat-HOTSPOT
        coltext = ls_fcat-coltext
        scrtext_l = ls_fcat-coltext
        scrtext_m = ls_fcat-coltext
        scrtext_s = ls_fcat-coltext
        tech = ls_fcat-tech
        ).
      ELSE.
        <fs_fcat>-tech = 'X'.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD file_download_template.
    DATA: ls_key      TYPE wwwdatatab,
          lv_filename TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.

* 判断模版是否存在
    SELECT SINGLE *
    FROM wwwdata
    WHERE relid EQ 'MI'
    AND   objid EQ @iv_objid
    INTO CORRESPONDING FIELDS OF @ls_key.
    IF sy-subrc NE 0.
      MESSAGE s015(zsd001) WITH iv_objid.
      RETURN.
    ENDIF.

    lv_filename = iv_filename.
    cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_extension         = cl_gui_frontend_services=>filetype_excel
      default_file_name         = lv_filename
    CHANGING
      filename                  = lv_filename
      path                      = lv_path
      fullpath                  = lv_fullpath
    EXCEPTIONS
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      OTHERS                    = 5 ).
    IF sy-subrc NE 0.
    ENDIF.
    CHECK lv_fullpath NE ''.

* 下载SMW0模版
    CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = ls_key
      destination = CONV localfile( lv_fullpath ).
  ENDMETHOD.


  METHOD file_download_to_csv.
    TYPES:BEGIN OF fieldnames,
      name TYPE char30,
    END OF fieldnames.
    DATA:fieldnames TYPE TABLE OF fieldnames.
    DATA filename TYPE string.

    CHECK DATA IS NOT INITIAL.
    DATA(fcat) = fcat_from_itab( DATA ).
    filename = file_get_path( 'CSV' ).
    CHECK filename IS NOT INITIAL.

    fieldnames = CORRESPONDING #( fcat MAPPING name = reptext ).
    READ TABLE fieldnames TRANSPORTING NO FIELDS WITH KEY name = ''.
    IF sy-subrc EQ 0.
      CLEAR fieldnames.
      fieldnames = CORRESPONDING #( fcat MAPPING name = fieldname ).
    ENDIF.

    CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*       BIN_FILESIZE            =
      filename                = filename
      filetype                = 'ASC'
      write_field_separator   = 'X'
      HEADER                  = '00'
      trunc_trailing_blanks   = 'X'
      codepage                = get_encoding( 'UTF-8' )
    TABLES
    data_tab                = DATA
          fieldnames              = fieldnames
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


  ENDMETHOD.


  METHOD file_download_to_excel.
    FIELD-SYMBOLS: <cdata> TYPE STANDARD TABLE.
    DATA filename TYPE string.
    DATA: salv  TYPE REF TO cl_salv_table.
    CHECK DATA IS NOT INITIAL.

    filename = file_get_path( 'xlsx' ).
    CHECK filename IS NOT INITIAL.
    ASSIGN DATA TO <cdata>.
    cl_salv_table=>factory(  IMPORTING  r_salv_table = salv
    CHANGING t_table = <cdata> ).

    DATA(xstring) = salv->to_xml( '10' ).

    cl_salv_data_services=>download_xml_to_file(
    filename = filename
    xcontent = xstring ).

  ENDMETHOD.


  METHOD file_get_path.

    DATA:filename TYPE string.
    DATA:path TYPE string.
    DATA:fullpath TYPE string.
    cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = path ).
    cl_gui_cfw=>flush( ).
    filename = |{ sy-TITLE }_{ sy-datum  }_{ sy-uzeit }|.

    cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_file_name       = filename
      default_extension       = extname
      file_filter             = extname
      initial_directory = path
    CHANGING
      path            = path
      filename            = filename
      fullpath            = fullpath
    EXCEPTIONS
      cntl_error          = 1
      error_no_gui        = 2
      OTHERS              = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      rv_file = fullpath.
    ENDIF.
  ENDMETHOD.


  METHOD get_encoding.
    DATA :  l_codepage TYPE cpcodepage .
    CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
    EXPORTING
      external_name = i_external_name
    IMPORTING
      sap_codepage  = l_codepage.
    r_encoding = l_codepage.
  ENDMETHOD.


  METHOD get_guid16.
    rv_guid16 = cl_system_uuid=>create_uuid_x16_static( ).
  ENDMETHOD.


  METHOD get_guid32.
    rv_guid32 = cl_system_uuid=>create_uuid_c32_static( ).
  ENDMETHOD.


  METHOD get_logsys.
    CLEAR rv_logsys.
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = rv_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 99.
  ENDMETHOD.


  METHOD get_nr_number.
    DATA lv_nr TYPE nrnr .
    lv_nr = |{ iv_nr CASE = UPPER }|.
    DATA lv_object TYPE nrobj .
    lv_object = |{ iv_object CASE = UPPER }|.

    SELECT SINGLE tnro~domlen,dd01l~datatype, dd01l~leng
    FROM tnro INNER JOIN dd01l ON tnro~domlen = dd01l~domname AND dd01l~as4local = 'A'
    WHERE object = @lv_object INTO @DATA(ls_datainfo).
    IF sy-subrc <> 0.
      CREATE DATA rv_number TYPE char30. "//返回一个空值，初始化！
      RETURN.
    ENDIF.

    SELECT SINGLE @abap_true
    FROM nriv WHERE object = @lv_object AND nrrangenr = @lv_nr
    INTO @DATA(lv_result).
    IF sy-subrc <> 0.
      CREATE DATA rv_number TYPE char30. "//返回一个空值，初始化！
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <lv_number> TYPE ANY.
    IF ls_datainfo-datatype = 'NUMC' OR ls_datainfo-datatype = 'CHAR'.
      CREATE DATA rv_number TYPE char100.
    ELSE.
      CREATE DATA rv_number TYPE char100.
    ENDIF.

    ASSIGN rv_number->* TO <lv_number>.

    DATA lv_number_temp TYPE C LENGTH 100.

    CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = lv_nr
      object                  = lv_object
      toyear                  = COND #( WHEN iv_year IS NOT INITIAL THEN iv_year ELSE '0000' )
      ignore_buffer           = COND #( WHEN iv_no_buffer IS INITIAL THEN abap_false ELSE abap_true )
    IMPORTING
      NUMBER                  = lv_number_temp
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
    IF sy-subrc <> 0.
      CLEAR <lv_number>.
    ELSE.
      <lv_number> = lv_number_temp.
    ENDIF.

  ENDMETHOD.


  METHOD get_user_name.
    DATA lv_user_full_name TYPE addr3_val-name_text.
    CLEAR lv_user_full_name.
    IF iv_user_id IS INITIAL.
      CALL FUNCTION 'USER_NAME_GET' IMPORTING full_user_name = lv_user_full_name.
      ev_user_full_name = lv_user_full_name.
    ELSE.
      SELECT SINGLE FROM usr21
      LEFT OUTER JOIN adrp ON usr21~persnumber = adrp~persnumber
*      FIELDS concat( name_first,name_last ) AS name
*      FIELDS concat_with_space( name_first,name_last,1 ) AS name
      FIELDS concat( name_last,name_first ) AS name "//中文名
      WHERE bname = @iv_user_id
      INTO @lv_user_full_name.
      ev_user_full_name = lv_user_full_name.
    ENDIF.
  ENDMETHOD.


  METHOD number_check.
    DATA lv_char100 TYPE C LENGTH 100.
    lv_char100 = iv_string.
    IF cl_abap_matcher=>matches(
    pattern = '^[-+]?(([0-9]+)([.]([0-9]+))?|([.]([0-9]+))?)$'
    TEXT = lv_char100 ) = abap_true.
      rv_ok = abap_true.
    ELSE.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD number_input.
    REPLACE ALL OCCURRENCES OF REGEX ',' IN VALUE WITH ''.
    rv_ok = abap_true.
    TRY .
      NUMBER = VALUE.
    CATCH cx_root INTO DATA(lr_message).
      rv_ok = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD PROGRESSBAR_SHOW.
    DATA: lv_msg TYPE string.
    IF iv_msg IS INITIAL.
      lv_msg = |{ TEXT-t01 }........ { iv_current }/{ iv_total }|.
    ELSE.
      lv_msg = iv_msg.
    ENDIF.
    cl_progress_indicator=>progress_indicate(
    EXPORTING
      i_text               = lv_msg
      i_processed          = iv_current
      i_total              = iv_total
      i_output_immediately = abap_true ).
  ENDMETHOD.


  METHOD search_customer.
    DATA lt_customer_found TYPE TABLE OF customer_found .

    DATA l_customer TYPE customer_found.
    DATA:l_kunnr TYPE kunnr.

    CHECK kunnr IS NOT INITIAL .

    l_kunnr = |{ kunnr }ALPHA = IN|.
    SELECT SINGLE kunnr INTO l_kunnr
    FROM kna1
    WHERE kunnr = l_kunnr
    AND name1 = l_kunnr.
    IF sy-subrc EQ 0.
      kunnr = l_kunnr.
      RETURN.
    ENDIF.

    kunnr = |%{ kunnr }%|.

    SELECT
    DISTINCT
    kunnr
    name1 AS name
    sortl AS sort1
    INTO CORRESPONDING FIELDS OF TABLE lt_customer_found
    FROM kna1
    WHERE kunnr LIKE kunnr
    OR name1 LIKE kunnr
    OR sortl LIKE kunnr.

    IF LINES( lt_customer_found ) EQ 0.
      CLEAR kunnr.
  ELSEIF LINES( lt_customer_found ) EQ 1.
      kunnr = lt_customer_found[ 1 ]-kunnr.
    ELSE.
      CALL FUNCTION 'MM_CUSTOMER_SHOW_HITS'
      IMPORTING
        e_customer_return = l_customer
      TABLES
        t_customer_value  = lt_customer_found.
      kunnr = l_customer-kunnr.
    ENDIF.

  ENDMETHOD.


  METHOD search_vendor.
    DATA:  lt_vendors      TYPE TABLE OF  vendor_found .
    DATA:  l_vendor        TYPE vendor_found.
    DATA:  l_lifnr TYPE lifnr.

    CHECK lifnr IS NOT INITIAL .

    l_lifnr = |{ lifnr }ALPHA = IN|.

    SELECT
    SINGLE lifnr
    INTO l_lifnr
    FROM lfa1
    WHERE lifnr = l_lifnr
    OR name1 = l_lifnr.
    IF sy-subrc EQ 0.
      lifnr = l_lifnr.
      RETURN.
    ENDIF.

    lifnr = |%{ lifnr }%|.

    SELECT
    lifnr
    name1 AS name
    sortl AS sort1
    INTO CORRESPONDING FIELDS OF TABLE lt_vendors
    FROM lfa1
    WHERE lifnr LIKE lifnr
    OR name1 LIKE lifnr
    OR sortl LIKE lifnr .


    IF LINES( lt_vendors ) EQ 0.
      CLEAR lifnr.
  ELSEIF LINES( lt_vendors ) EQ 1.
      lifnr = lt_vendors[ 1 ]-lifnr.
    ELSE.
      CALL FUNCTION 'MM_VENDOR_SHOW_HITS'
      IMPORTING
        e_vendor_return = l_vendor
      TABLES
        t_vendor_value  = lt_vendors.
      lifnr = l_vendor-lifnr.
    ENDIF.


  ENDMETHOD.


  METHOD set_admin_value_create.

    FIELD-SYMBOLS: <fs_data> TYPE ANY.
    ASSIGN is_data->* TO <fs_data>.

    ASSIGN COMPONENT 'ERDAT' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc EQ 0.
      <fs_value> = sy-datum.
    ENDIF.

    ASSIGN COMPONENT 'ERZET' OF STRUCTURE <fs_data> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = sy-uzeit.
    ENDIF.

    ASSIGN COMPONENT 'ERNAM' OF STRUCTURE <fs_data> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = sy-uname.
    ENDIF.

    ASSIGN COMPONENT 'STATUS' OF STRUCTURE <fs_data> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = 'A'.
    ENDIF.

  ENDMETHOD.


  METHOD set_admin_value_modify.

    FIELD-SYMBOLS: <fs_data> TYPE ANY.
    ASSIGN is_data->* TO <fs_data>.

    ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_value>).
    IF sy-subrc EQ 0.
      <fs_value> = sy-datum.
    ENDIF.

    ASSIGN COMPONENT 'AETIM' OF STRUCTURE <fs_data> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = sy-uzeit.
    ENDIF.

    ASSIGN COMPONENT 'AENAM' OF STRUCTURE <fs_data> TO <fs_value>.
    IF sy-subrc EQ 0.
      <fs_value> = sy-uname.
    ENDIF.

  ENDMETHOD.


  METHOD set_default_value_line.

    FIELD-SYMBOLS: <fs_data> TYPE ANY.
    FIELD-SYMBOLS: <ft_config> TYPE TABLE.

    ASSIGN is_data->* TO <fs_data>.
    ASSIGN it_config->* TO <ft_config>.


    LOOP AT <ft_config> ASSIGNING FIELD-SYMBOL(<fs_config>).
      ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fs_config> TO FIELD-SYMBOL(<fs_name>).
      CHECK sy-subrc EQ 0.
      ASSIGN COMPONENT 'DEFAULT_VALUE' OF STRUCTURE <fs_config> TO FIELD-SYMBOL(<fs_value>).
      CHECK sy-subrc EQ 0.
      ASSIGN COMPONENT <fs_name> OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_to_value>).
      CHECK sy-subrc EQ 0.
      IF <fs_to_value> IS INITIAL AND <fs_value> IS NOT INITIAL.
        <fs_to_value> = <fs_value>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_default_value_table.
    FIELD-SYMBOLS: <ft_data> TYPE TABLE.
    FIELD-SYMBOLS: <ft_config> TYPE TABLE.

    ASSIGN it_data->* TO <ft_data>.
    ASSIGN it_config->* TO <ft_config>.


    LOOP AT <ft_data> ASSIGNING FIELD-SYMBOL(<fs_data>).
      LOOP AT <ft_config> ASSIGNING FIELD-SYMBOL(<fs_config>).
        ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <fs_config> TO FIELD-SYMBOL(<fs_name>).
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT 'DEFAULT_VALUE' OF STRUCTURE <fs_config> TO FIELD-SYMBOL(<fs_value>).
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT <fs_name> OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_to_value>).
        CHECK sy-subrc EQ 0.
        IF <fs_to_value> IS INITIAL AND <fs_value> IS NOT INITIAL.
          <fs_to_value> = <fs_value>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_init_icon.
    CASE status.
    WHEN ''."无关
      ICON = icon_light_out.
    WHEN 'A'."待处理
      ICON = icon_yellow_light.
    WHEN 'B'."执行中
      ICON = icon_generate.
    WHEN 'C'."完成
      ICON = icon_green_light.
    WHEN 'D'."作废
      ICON = icon_delete.
    WHEN 'E'."错误
      ICON = icon_red_light.
    WHEN 'F'."错误2
      ICON = icon_incomplete.
    WHEN 'P'."完成/释放
      ICON = icon_complete.
    WHEN 'R'."记账/释放
      ICON = icon_release.
    WHEN 'L'."锁定
      ICON = icon_locked.
    ENDCASE.
  ENDMETHOD.


  METHOD set_init_sdate.
    FIELD-SYMBOLS <ft_value> TYPE TABLE.
    FIELD-SYMBOLS <fs_value> TYPE ANY.

    ASSIGN sdate->* TO  <ft_value>.

    CHECK <ft_value> IS INITIAL.
    APPEND INITIAL LINE TO <ft_value> ASSIGNING FIELD-SYMBOL(<fs_line>).

    ASSIGN COMPONENT 'SIGN' OF STRUCTURE <fs_line> TO <fs_value>.
    CHECK sy-subrc EQ 0.
    <fs_value> = 'I'.

    ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_line> TO <fs_value>.
    CHECK sy-subrc EQ 0.
    <fs_value> = 'BT'.

    ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_line> TO <fs_value>.
    CHECK sy-subrc EQ 0.
    <fs_value> = sy-datum - days.

    ASSIGN COMPONENT 'HIGH' OF STRUCTURE <fs_line> TO <fs_value>.
    CHECK sy-subrc EQ 0.
    <fs_value> = sy-datum.

  ENDMETHOD.
ENDCLASS.

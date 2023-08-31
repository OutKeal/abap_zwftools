class ZWFT_COMMON definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF s_type_metadata,
        is_hierseq   TYPE abap_bool,
        tabname      TYPE string,
        tabname_line TYPE string,
        s_keyinfo    TYPE kkblo_keyinfo,
        s_layout     TYPE lvc_s_layo,
        t_fcat       TYPE lvc_t_fcat,
        t_filter     TYPE lvc_t_filt,
        t_sort       TYPE lvc_t_sort,
      END OF s_type_metadata .
  types:
    BEGIN OF ty_fcat,
        col_pos   TYPE col_pos,
        fieldname TYPE fieldname,
        edit      TYPE lvc_edit,
        emphasize TYPE lvc_emphsz,
        hotspot   TYPE lvc_hotspt,
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

  class-methods CLASS_CONSTRUCTOR .
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
      value(IV_STRING) type ANY
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
  class-methods CALL_TRANSATION_BY_LINE
    importing
      value(LINE) type ANY
      value(FIELDNAME) type FIELDNAME .
  class-methods FILE_DOWNLOAD_TO_CSV
    importing
      !DATA type TABLE .
  class-methods FILE_DOWNLOAD_TO_EXCEL
    importing
      value(DATA) type TABLE .
  class-methods FILE_UPLOAD_FROM_EXCEL
    importing
      value(BEGIN_COL) type I default 1
      value(BEGIN_ROW) type I default 1
      value(END_COL) type I default 100
      value(END_ROW) type I default 9999
    changing
      value(DATA) type TABLE .
  class-methods FILE_GET_READ_PATH
    importing
      value(EXTNAME) type STRING default 'XLSX'
    returning
      value(RV_FILE) type RLGRAP-FILENAME .
  class-methods FILE_GET_SAVE_PATH
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
  class-methods FCAT_FROM_DATA
    importing
      !IT_TABLE type DATA
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  class-methods FCAT_FROM_NAME
    importing
      !IV_TABNAME type CHAR30
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  class-methods GET_TABLE_FIELDS
    importing
      value(IV_TABNAME) type TABNAME
    returning
      value(RT_X031L) type DDX031LTAB .
  class-methods GET_FIELDS
    importing
      value(IV_TABNAME) type TABNAME
    returning
      value(RT_X031L) type DDX031LTAB .
  class-methods GET_FIELDS_DFIES
    importing
      value(IV_TABNAME) type TABNAME
    returning
      value(RT_DFIES) type DFIES_TAB .
  class-methods CREATE_TABLE_DFIES
    importing
      !IT_DFIES type DFIES_TAB
    changing
      !CT_DATA type ref to DATA
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods CREATE_TABLE_COMPO
    importing
      !IT_COMPO type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    changing
      !CT_DATA type ref to DATA
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods CREATE_TABLE_FCAT
    importing
      !IT_FCAT type LVC_T_FCAT
    changing
      !CT_DATA type ref to DATA
    returning
      value(RV_OK) type ABAP_BOOL .
  class-methods GET_USER_PARAMETER
    importing
      value(PARID) type MEMORYID
      value(UNAME) type SY-UNAME default SY-UNAME
    returning
      value(R_PARAMETER_VALUE) type XUVALUE .
  class-methods GET_WHERE_FROM_RANGES
    importing
      !TRANGE type RSDS_TRANGE
    returning
      value(TWHERE) type RSDS_TWHERE .
  class-methods GET_REPORT_ALV_DATA
    importing
      !TCODE type TCODE
      !VARIANT type VARIANT optional
    returning
      value(DATA) type ref to DATA .
  class-methods GET_REPORT_ALV_MATE
    importing
      !TCODE type TCODE
      !VARIANT type VARIANT
    exporting
      !DATA type ref to DATA
    returning
      value(METADATA) type S_TYPE_METADATA .
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
            msgid               = cs_return-id
            msgnr               = cs_return-number
            msgv1               = cs_return-message_v1
            msgv2               = cs_return-message_v2
            msgv3               = cs_return-message_v3
            msgv4               = cs_return-message_v4
          IMPORTING
            message_text_output = cs_return-message.
      ENDIF.
    ENDIF.
    IF ct_return IS SUPPLIED.
      LOOP AT ct_return ASSIGNING FIELD-SYMBOL(<ls_return>).
        IF <ls_return>-message IS INITIAL.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = <ls_return>-id
              msgnr               = <ls_return>-number
              msgv1               = <ls_return>-message_v1
              msgv2               = <ls_return>-message_v2
              msgv3               = <ls_return>-message_v3
              msgv4               = <ls_return>-message_v4
            IMPORTING
              message_text_output = <ls_return>-message.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD call_transation.
    CASE type.
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
    DATA(len) = strlen( value ).
    DATA:year TYPE char4.
    DATA:month TYPE char2.
    DATA:day TYPE char2.

    rv_ok = abap_false.
    IF len < 8 OR len > 10.
      CLEAR date.
      RETURN.
    ENDIF.

    FIND '.' IN value.
    IF sy-subrc EQ 0.
      flag = '.'.
    ENDIF.

    FIND '/' IN value.
    IF sy-subrc EQ 0.
      flag = '/'.
    ENDIF.

    FIND '-' IN value.
    IF sy-subrc EQ 0.
      flag = '-'.
    ENDIF.

    CASE flag.
      WHEN ''.
        IF len = 8.
          date = value.
        ELSE.
          CLEAR date.
          RETURN.
        ENDIF.
      WHEN OTHERS.

        SPLIT value AT flag INTO year month day.
        DO 4 - strlen( year ) TIMES.
          year = '0' && year.
        ENDDO.

        DO 2 - strlen( month ) TIMES.
          month = '0' && month.
        ENDDO.

        DO 2 - strlen( day ) TIMES.
          day = '0' && day.
        ENDDO.

        date = year && month && day.

        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
            date                      = date
          EXCEPTIONS
            plausibility_check_failed = 1
            OTHERS                    = 2.
        IF sy-subrc <> 0.
          CLEAR date.
          RETURN.
        ELSE.
          rv_ok = abap_true.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD date_output.
    IF spliter IS INITIAL.
      rv_date = |{ date DATE = ENVIRONMENT }|.
    ELSE.
      rv_date = |{ date+0(4) }{ spliter }{ date+4(2) }{ spliter }{ date+6(2) }|.
    ENDIF.
  ENDMETHOD.


  METHOD doma_value_check.
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
      ls_doma_buf-value = i_value.
      ls_doma_buf-langu = i_langu.
      READ TABLE lt_doma_buf FROM ls_doma_buf
      ASSIGNING <ls_doma_buf>.
      IF sy-subrc IS INITIAL.
        e_text = <ls_doma_buf>-text.
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

      SORT lt_dd03l BY position.
* find selection criteria and bulid where-clause

      READ TABLE lt_dd03l INTO DATA(ls_dd03l) WITH KEY
            domname = i_doma
            keyflag = 'X'.
      l_where-line = |{ ls_dd03l-fieldname } = '{ i_value }'|.
      APPEND l_where TO lt_where.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.


* check field-structure of text-table (SPRAS on 2nd position)

      READ TABLE lt_dd03l INTO ls_dd03l
      WITH KEY datatype = 'LANG'.
      IF  sy-subrc = 0 .
        l_where-line = |AND { ls_dd03l-fieldname } = '{ i_langu }'|.
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
      ls_doma_buf-text = e_text.
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


  METHOD fcat_set_value.

    FIELD-SYMBOLS: <ft_config> TYPE table.
    FIELD-SYMBOLS: <fs_config> TYPE any.

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
        edit = ls_fcat-edit
        emphasize = ls_fcat-emphasize
        hotspot = ls_fcat-hotspot
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
      MESSAGE s899(mm) WITH |{ iv_objid }不存在| .
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
        key         = ls_key
        destination = CONV localfile( lv_fullpath ).
  ENDMETHOD.


  METHOD file_download_to_csv.
    TYPES:BEGIN OF fieldnames,
            name TYPE char30,
          END OF fieldnames.
    DATA:fieldnames TYPE TABLE OF fieldnames.
    DATA filename TYPE string.

    CHECK data IS NOT INITIAL.
    DATA(fcat) = fcat_from_data( data ).
    filename = file_get_save_path( 'CSV' ).
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
        header                  = '00'
        trunc_trailing_blanks   = 'X'
        codepage                = get_encoding( 'UTF-8' )
      TABLES
        data_tab                = data
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
    CHECK data IS NOT INITIAL.

    filename = file_get_save_path( 'xlsx' ).
    CHECK filename IS NOT INITIAL.
    ASSIGN data TO <cdata>.
    cl_salv_table=>factory(  IMPORTING  r_salv_table = salv
    CHANGING t_table = <cdata> ).

*    DATA(fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
*          r_columns      = salv->get_columns( )
*          r_aggregations = salv->get_aggregations( ) ).
*    LOOP AT fcat INTO DATA(ls_fcat).
*      DATA(lr_column) = salv->get_columns( )->get_column( ls_fcat-fieldname ).
*      lr_column->set_short_text( |{ ls_fcat-reptext }| ) .
*      lr_column->set_medium_text( |{ ls_fcat-reptext }| ) .
*      lr_column->set_long_text( |{ ls_fcat-reptext }| ) .
*    ENDLOOP.
    cl_salv_data_services=>download_xml_to_file(
    filename = filename
    xcontent = salv->to_xml( '10' ) ).
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

    FIELD-SYMBOLS <lv_number> TYPE any.
    IF ls_datainfo-datatype = 'NUMC' OR ls_datainfo-datatype = 'CHAR'.
      CREATE DATA rv_number TYPE char100.
    ELSE.
      CREATE DATA rv_number TYPE char100.
    ENDIF.

    ASSIGN rv_number->* TO <lv_number>.

    DATA lv_number_temp TYPE c LENGTH 100.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lv_nr
        object                  = lv_object
        toyear                  = COND #( WHEN iv_year IS NOT INITIAL THEN iv_year ELSE '0000' )
        ignore_buffer           = COND #( WHEN iv_no_buffer IS INITIAL THEN abap_false ELSE abap_true )
      IMPORTING
        number                  = lv_number_temp
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
    DATA lv_char100 TYPE c LENGTH 100.
    lv_char100 = iv_string.
    IF cl_abap_matcher=>matches(
    pattern = '^[-+]?(([0-9]+)([.]([0-9]+))?|([.]([0-9]+))?)$'
    text = lv_char100 ) = abap_true.
      rv_ok = abap_true.
    ELSE.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD number_input.
    REPLACE ALL OCCURRENCES OF REGEX ',' IN value WITH ''.
    rv_ok = abap_true.
    TRY .
        number = value.
      CATCH cx_root INTO DATA(lr_message).
        rv_ok = abap_false.
    ENDTRY.
  ENDMETHOD.


  METHOD progressbar_show.
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

    IF lines( lt_customer_found ) EQ 0.
      CLEAR kunnr.
    ELSEIF lines( lt_customer_found ) EQ 1.
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


    IF lines( lt_vendors ) EQ 0.
      CLEAR lifnr.
    ELSEIF lines( lt_vendors ) EQ 1.
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

    FIELD-SYMBOLS: <fs_data> TYPE any.
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

    FIELD-SYMBOLS: <fs_data> TYPE any.
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

    FIELD-SYMBOLS: <fs_data> TYPE any.
    FIELD-SYMBOLS: <ft_config> TYPE table.

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
    FIELD-SYMBOLS: <ft_data> TYPE table.
    FIELD-SYMBOLS: <ft_config> TYPE table.

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
        icon = icon_light_out.
      WHEN 'A'."待处理
        icon = icon_yellow_light.
      WHEN 'B'."执行中
        icon = icon_generate.
      WHEN 'C'."完成
        icon = icon_green_light.
      WHEN 'D'."作废
        icon = icon_delete.
      WHEN 'E'."错误
        icon = icon_red_light.
      WHEN 'F'."错误2
        icon = icon_incomplete.
      WHEN 'P'."完成/释放
        icon = icon_complete.
      WHEN 'R'."记账/释放
        icon = icon_release.
      WHEN 'L'."锁定
        icon = icon_locked.
    ENDCASE.
  ENDMETHOD.


  METHOD set_init_sdate.
    FIELD-SYMBOLS <ft_value> TYPE table.
    FIELD-SYMBOLS <fs_value> TYPE any.

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


  METHOD get_table_fields.
    DATA lv_tabname TYPE tabname.
    lv_tabname = |{ iv_tabname CASE = UPPER }|.
    DATA ls_x030l TYPE x030l .
    DATA lt_x031l TYPE TABLE OF x031l.
    CLEAR: ls_x030l, lt_x031l.
    CLEAR rt_x031l.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = lv_tabname
      IMPORTING
        x030l_wa  = ls_x030l
      TABLES
        x031l_tab = lt_x031l
      EXCEPTIONS
        OTHERS    = 1.
    IF ls_x030l-tabtype = 'T'.
      rt_x031l = lt_x031l.
    ENDIF.
    FREE: ls_x030l, lt_x031l, lv_tabname.
  ENDMETHOD.


  METHOD create_table_fcat.
    DATA(lt_fcat) = it_fcat.
    DELETE lt_fcat WHERE tabname IS INITIAL OR fieldname IS INITIAL.
    "// 检查表名和字段名是否正确？
    rv_ok = abap_false.
    "//-------------------------------------------------------
    DATA lt_compo TYPE cl_abap_structdescr=>component_table.
    CLEAR lt_compo.
    DATA lo_data TYPE REF TO cl_abap_datadescr.
    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      APPEND INITIAL LINE TO lt_compo ASSIGNING FIELD-SYMBOL(<ls_compo>).
      <ls_compo>-name = <ls_fcat>-fieldname.
      lo_data = CAST cl_abap_datadescr(
      cl_abap_datadescr=>describe_by_name( |{ <ls_fcat>-tabname }-{ <ls_fcat>-fieldname }| ) ).
      <ls_compo>-type = lo_data.
    ENDLOOP.
    rv_ok = zwft_common=>create_table_compo( EXPORTING it_compo = lt_compo CHANGING ct_data = ct_data ).
  ENDMETHOD.


  METHOD create_table_dfies.
    DATA(lt_dfies) = it_dfies.
    DELETE lt_dfies WHERE tabname IS INITIAL OR fieldname IS INITIAL.
    "// 检查表名和字段名是否正确？
    rv_ok = abap_false.
    "//-------------------------------------------------------
    DATA lt_compo TYPE cl_abap_structdescr=>component_table.
    CLEAR lt_compo.
    DATA lo_data TYPE REF TO cl_abap_datadescr.
    LOOP AT it_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
      APPEND INITIAL LINE TO lt_compo ASSIGNING FIELD-SYMBOL(<ls_compo>).
      <ls_compo>-name = <ls_dfies>-fieldname.
      lo_data = CAST cl_abap_datadescr(
      cl_abap_datadescr=>describe_by_name( |{ <ls_dfies>-tabname }-{ <ls_dfies>-fieldname }| ) ).
      <ls_compo>-type = lo_data.
    ENDLOOP.
    rv_ok = zwft_common=>create_table_compo( EXPORTING it_compo = lt_compo CHANGING ct_data = ct_data ).
  ENDMETHOD.


  METHOD create_table_compo.
    rv_ok = abap_false.
    DATA(lo_table) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( it_compo ) ).
    CREATE DATA ct_data TYPE HANDLE lo_table.
    rv_ok = abap_true.
  ENDMETHOD.


  METHOD get_fields_dfies.
    DATA lv_tabname TYPE tabname.
    lv_tabname = |{ iv_tabname CASE = UPPER }|.
    CONDENSE lv_tabname NO-GAPS.
    CLEAR rt_dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = iv_tabname
        langu     = sy-langu
      TABLES
        dfies_tab = rt_dfies.
  ENDMETHOD.


  METHOD get_fields.
    DATA lv_tabname TYPE tabname.
    lv_tabname = |{ iv_tabname CASE = UPPER }|.
    CONDENSE lv_tabname NO-GAPS.
    DATA ls_x030l TYPE x030l .
    DATA lt_x031l TYPE TABLE OF x031l.
    CLEAR: ls_x030l, lt_x031l.
    CLEAR rt_x031l.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = lv_tabname
      IMPORTING
        x030l_wa  = ls_x030l
      TABLES
        x031l_tab = lt_x031l
      EXCEPTIONS
        OTHERS    = 1.
    rt_x031l = lt_x031l.
    FREE: ls_x030l, lt_x031l, lv_tabname.
  ENDMETHOD.


  METHOD call_transation_by_line.

    ASSIGN COMPONENT fieldname OF STRUCTURE line TO FIELD-SYMBOL(<value>).
    CHECK sy-subrc EQ 0.

    CASE fieldname.
      WHEN 'BANFN' ."采购申请
        SET PARAMETER ID 'BAN' FIELD <value>.
        CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

      WHEN 'EBELN' .  "采购订单
        SET PARAMETER ID 'BES' FIELD <value>.
        CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.

      WHEN 'VBELN_ASN' . "内向交货单"
        SET PARAMETER ID 'VL' FIELD <value>.
        CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.

      WHEN 'VBELN_VA'. "销售订单"
        SET PARAMETER ID 'AUN' FIELD <value>.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      WHEN 'VBELN_VL'. "交货单"
        SET PARAMETER ID 'VL' FIELD <value>.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

      WHEN 'VBELN_VF'. "发票"
        SET PARAMETER ID 'VF' FIELD <value>.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

      WHEN 'RSNUM'. "预留"
        SET PARAMETER ID 'RES' FIELD <value>.
        CALL TRANSACTION  'MB23' AND SKIP FIRST SCREEN.

      WHEN 'MBLNR'. "商品凭证"
        ASSIGN COMPONENT 'MJAHR' OF STRUCTURE line TO FIELD-SYMBOL(<mjahr>).
        CHECK sy-subrc EQ 0.
        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action = 'A04'
            i_refdoc = 'R02'
            i_mblnr  = <value>
            i_mjahr  = <mjahr>.

      WHEN 'BELNR_R' .            "发票校验"
        ASSIGN COMPONENT 'GJAHR_R' OF STRUCTURE line TO FIELD-SYMBOL(<gjahr_r>).
        CHECK sy-subrc EQ 0.
        SET PARAMETER ID 'RBN' FIELD <value>.
        SET PARAMETER ID 'GJR' FIELD <gjahr_r>.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

      WHEN 'BELNR' .  "会计凭证
        ASSIGN COMPONENT 'GJAHR' OF STRUCTURE line TO FIELD-SYMBOL(<gjahr>).
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE line TO FIELD-SYMBOL(<bukrs>).
        CHECK sy-subrc EQ 0.
        SET PARAMETER ID 'BLN' FIELD <value>.
        SET PARAMETER ID 'GJR' FIELD <gjahr>.
        SET PARAMETER ID 'BUK' FIELD <bukrs>.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      WHEN 'MATNR' OR 'STANR'. "商品"
        SET PARAMETER ID 'MAT' FIELD <value>.
        CALL TRANSACTION 'MM43' AND SKIP FIRST SCREEN.


      WHEN 'LIFNR' OR 'KUNNR' . "客户/供应商"
        SET PARAMETER ID 'BPA' FIELD <value>.
        SUBMIT r_ftr_display_bp WITH p_bp = <value> AND RETURN.

      WHEN 'AUFNR_U' .  "内部订单"
        SET PARAMETER ID 'ANR' FIELD <value>.
        CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.

      WHEN 'SAKNR' OR 'HKONT' . "科目"
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE line TO FIELD-SYMBOL(<bukrs_skb1>).
        CHECK sy-subrc EQ 0.
        SET PARAMETER ID 'SAK' FIELD <value>.
        SET PARAMETER ID 'BUK' FIELD <bukrs_skb1>.
        CALL TRANSACTION 'FS00' AND SKIP FIRST SCREEN.


      WHEN 'ANLA' OR 'ANLN1' OR 'BUS1022'.   "固定资产
        ASSIGN COMPONENT 'BUKRS' OF STRUCTURE line TO FIELD-SYMBOL(<bukrs_as03>).
        CHECK sy-subrc EQ 0.
        SET PARAMETER ID 'AN1' FIELD <value>.
        SET PARAMETER ID 'BUK' FIELD <bukrs_as03>.
        CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.

      WHEN 'WERKS'  OR 'UMWRK'. "地点
        SET PARAMETER ID 'WRK' FIELD <value>.
        CALL TRANSACTION 'WB03' AND SKIP FIRST SCREEN.

      WHEN 'AUFNR' ."生产订单
        SET PARAMETER ID 'ANR' FIELD <value>.
        CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.

      WHEN 'KOSTL'.
        SET PARAMETER ID 'KOS' FIELD <value>.
        CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM' ."IDOC
        SUBMIT idoc_tree_control WITH docnum = <value> AND RETURN.

      WHEN 'XML' OR 'PROXY'.
        SUBMIT rsxmb_display_msg_vers_new WITH msgguid = <value>
        AND RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD file_upload_from_excel.
    DATA: msg TYPE REF TO zwft_message.
    DATA:lt_excel TYPE TABLE OF alsmex_tabline.
    DATA(pv_path) = file_get_read_path( 'xlsx' ).

    msg = NEW zwft_message( ).
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = pv_path
        i_begin_col             = begin_col
        i_begin_row             = begin_row
        i_end_col               = end_col
        i_end_row               = end_row
      TABLES
        intern                  = lt_excel
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      msg->add_single( msgty = 'E' msgid = 'MG' msgno = '899' msgv1 = '读取导入文件失败' ).
    ENDIF.

    CLEAR data.
    DATA(fcat) = fcat_from_data( data ).


    LOOP AT lt_excel INTO DATA(ls_excel).
      IF ls_excel-row = 1.
        READ TABLE fcat INTO DATA(ls_fcat) WITH KEY col_pos = ls_excel-col.
        IF sy-subrc NE 0 OR ls_excel-value <> ls_fcat-reptext .
          msg->add_single( msgty = 'E' msgid = 'MG' msgno = '899' msgv1 = |列{ ls_fcat-reptext }模板与数据不一致| ).
          msg->pop_msg( ).
          RETURN.
        ENDIF.
        CONTINUE.
      ENDIF.




      AT NEW row.
        APPEND INITIAL LINE TO data ASSIGNING FIELD-SYMBOL(<line>).
      ENDAT.
      READ TABLE fcat INTO ls_fcat WITH KEY col_pos = ls_excel-col.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
        IF sy-subrc EQ 0.
          CASE ls_fcat-inttype.
            WHEN 'I' OR 'P'.
              IF number_check( ls_excel-value ).
                <value> = ls_excel-value.
              ELSE.
                msg->add_single( msgty = 'E' msgid = 'MG' msgno = '899' msgv1 = |行{ ls_excel-row }列{ ls_fcat-fieldname }不是数值,导入失败| ).
              ENDIF.
            WHEN 'D'.
              IF NOT date_input( EXPORTING value = ls_excel-value CHANGING date = <value> ).
                msg->add_single( msgty = 'E' msgid = 'MG' msgno = '899' msgv1 = |行{ ls_excel-row }列{ ls_fcat-fieldname }不是日期,导入失败|  ).
              ENDIF.
            WHEN OTHERS.
              <value> = ls_excel-value.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF msg->get_error( ).
      msg->pop_msg( ).
    ENDIF.

  ENDMETHOD.


  METHOD file_get_save_path.

    DATA:filename TYPE string.
    DATA:path TYPE string.
    DATA:fullpath TYPE string.
    cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = path
                                                                                    EXCEPTIONS cntl_error = 1
                                                                                                          error_no_gui = 2
                                                                                                          not_supported_by_gui = 3 ).
    cl_gui_cfw=>flush( ).
    filename = |{ sy-title }_{ sy-datum  }_{ sy-uzeit }|.

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


  METHOD file_get_read_path.

    DATA:filename TYPE string.
    DATA:path TYPE string.
    DATA:fullpath TYPE string.

    DATA: lt_filetab TYPE filetable,
          lv_rc      TYPE i.
    cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = path
                                                                                    EXCEPTIONS cntl_error = 1
                                                                                                          error_no_gui = 2
                                                                                                          not_supported_by_gui = 3 ).
    cl_gui_cfw=>flush( ).
    filename = |{ sy-title }_{ sy-datum  }_{ sy-uzeit }|.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        default_extension       = extname
        default_filename        = filename
        file_filter             = extname
        initial_directory       = path
      CHANGING
        file_table              = lt_filetab
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      READ TABLE lt_filetab INTO DATA(ls_filetab) INDEX 1.
      IF sy-subrc EQ 0.
        rv_file = ls_filetab-filename.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_user_parameter.
    DATA:parameter TYPE TABLE OF bapiparam.
    DATA:return TYPE TABLE OF bapiret2 .
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username  = uname
      TABLES
        parameter = parameter
        return    = return.
    READ TABLE parameter INTO DATA(l_parameter) WITH KEY parid = parid.
    IF sy-subrc EQ 0.
      r_parameter_value = l_parameter-parva.
    ENDIF.

  ENDMETHOD.


  METHOD fcat_from_name.
    CLEAR rt_fcat.
    DATA lv_tabname TYPE tabname.
    lv_tabname = |{ iv_tabname CASE = UPPER }|.
    CONDENSE lv_tabname NO-GAPS.
    DATA(lt_dfies) = get_fields_dfies( lv_tabname ).
    IF lt_dfies IS INITIAL.
      MESSAGE '给定的DDIC表名或者结构名不存在' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA lds_structure TYPE REF TO data.
    CREATE DATA lds_structure TYPE (lv_tabname).
    ASSIGN lds_structure->* TO FIELD-SYMBOL(<lds_structure>).
    rt_fcat = fcat_from_data( <lds_structure> ).
  ENDMETHOD.


  METHOD fcat_from_data.
    CLEAR rt_fcat.
    DATA lo_type TYPE REF TO cl_abap_typedescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( it_table ).
    DATA t_data TYPE REF TO data.
    FIELD-SYMBOLS <t_data> TYPE any .
    CASE lo_type->type_kind.
      WHEN cl_abap_typedescr=>typekind_table. "内表
        CREATE DATA t_data LIKE it_table.
        ASSIGN t_data->* TO <t_data>.
      WHEN cl_abap_typedescr=>typekind_struct1."结构
        CREATE DATA t_data LIKE TABLE OF it_table.
        ASSIGN t_data->* TO <t_data>.
      WHEN cl_abap_typedescr=>typekind_struct2."结构
        CREATE DATA t_data LIKE TABLE OF it_table.
        ASSIGN t_data->* TO <t_data>.
      WHEN cl_abap_typedescr=>typekind_dref. "type ref to data
        ASSIGN it_table->* TO <t_data> .
        fcat_from_data( <t_data> ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.
    "//---------------------------------------------------------------
    FIELD-SYMBOLS <t_table> TYPE ANY TABLE.
    ASSIGN <t_data> TO <t_table>.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv_table)
        CHANGING  t_table      = <t_table> ).
        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = salv_table->get_columns( )
        r_aggregations = salv_table->get_aggregations( )
        ).
      CATCH cx_root.
        RETURN.
    ENDTRY.
    "//---------------------------------------------------------------
    LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<fcat>) .
      <fcat>-tooltip = <fcat>-fieldname.
      IF <fcat>-scrtext_s IS INITIAL .
        <fcat>-scrtext_s  = <fcat>-fieldname.
      ELSEIF <fcat>-scrtext_m IS INITIAL .
        <fcat>-scrtext_m  = <fcat>-fieldname.
      ELSEIF <fcat>-scrtext_l IS INITIAL.
        <fcat>-scrtext_l  = <fcat>-fieldname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_where_from_ranges.
    CHECK lines( trange ) > 0.
    TRY.
        CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
          EXPORTING
            field_ranges  = trange
          IMPORTING
            where_clauses = twhere.
      CATCH cx_root INTO DATA(lx_fm_error).
    ENDTRY.
  ENDMETHOD.


  METHOD get_report_alv_data.
    SELECT SINGLE * FROM tstc
                    WHERE tcode = @tcode
                    INTO @DATA(l_tstc).

    CHECK sy-subrc EQ 0.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
      metadata = abap_false
    data     = abap_true ).

    IF variant IS INITIAL.
      SUBMIT (l_tstc-pgmna)
      EXPORTING LIST TO MEMORY AND RETURN.
    ELSE.
      SUBMIT (l_tstc-pgmna)
      USING SELECTION-SET variant
      EXPORTING LIST TO MEMORY AND RETURN.
    ENDIF.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = data ).
*      ASSIGN gr_data->* TO <fs_table>.
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE '获取数据错误' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    cl_salv_bs_runtime_info=>clear_all( ).
  ENDMETHOD.


  METHOD get_report_alv_mate.
    SELECT SINGLE * FROM tstc
      WHERE tcode = @tcode
      INTO @DATA(l_tstc).

    CHECK sy-subrc EQ 0.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
      metadata = abap_true
      data     = abap_true ).

    IF variant IS INITIAL.
      SUBMIT (l_tstc-pgmna)
      EXPORTING LIST TO MEMORY AND RETURN.
    ELSE.
      SUBMIT (l_tstc-pgmna)
      USING SELECTION-SET variant
      EXPORTING LIST TO MEMORY AND RETURN.
    ENDIF.

    TRY.
        metadata = cl_salv_bs_runtime_info=>get_metadata( ).
*      ASSIGN gr_data->* TO <fs_table>.
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE '获取数据错误' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = data ).
*      ASSIGN gr_data->* TO <fs_table>.
      CATCH cx_salv_bs_sc_runtime_info.
        MESSAGE '获取数据错误' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.


    cl_salv_bs_runtime_info=>clear_all( ).
  ENDMETHOD.


  METHOD class_constructor.
  ENDMETHOD.
ENDCLASS.

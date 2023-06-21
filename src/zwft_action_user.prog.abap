*&---------------------------------------------------------------------*
*& Report ZEBC005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_action_user.
TYPE-POOLS: slis.
TABLES: v_usr_name,  tstcp, trdirt, tstct,
        tadir, tdevc, df14l, tstc, trdir, tdevct ,usr02,user_addrp.

*----------------------------------------------------------------------*
*                     变量声明
*----------------------------------------------------------------------*
* Global data
*DATA: t_output TYPE TABLE OF SWNCAGGUSERTCODE, "SWNCHITLIST,  ""Final output table

TYPES: BEGIN OF zsworkload,
         mandt       TYPE c LENGTH 3,          "集团
         period      TYPE n LENGTH 6,         "分析期间
         zzdatum     TYPE d,                  "日期
         account     TYPE usr21-bname,      "用户名称
         nmaccount   TYPE c LENGTH 80,     "完整用户名称
         department  TYPE adcp-department,  "部门
         function    TYPE adcp-function,    "职能
         ztcode      TYPE c   LENGTH 20,      "TCODE
         ddtext      TYPE c LENGTH 80,        "报告标题
         typelem     TYPE c LENGTH 1,        "报告标题
         zlncht      TYPE c LENGTH 10,        "类别
         zrepnm      TYPE c LENGTH 40,        "自定义字段
         zdesrepnm   TYPE c LENGTH 70,        "程序名称
         devclass    TYPE c LENGTH 30,        "报告标题
         desclass    TYPE c LENGTH 60,        "包
         compid      TYPE c   LENGTH 24,     "应用程序组件标识
         cnam        TYPE c LENGTH 12,       "作者
         cdat        TYPE d,                  "创建日期
         cnamod      TYPE c LENGTH 12,       "最后修改人
         cdatmod     TYPE d,                "最后修改日期
         destasktype TYPE c LENGTH 6,        "自定义字段
         zcounts     TYPE int4,             "使用人数
         zsteps      TYPE int4,             "交互数
       END OF zsworkload.
TYPES:BEGIN OF ty_user,
        name_last  TYPE adrp-name_last,        "姓
        name_first TYPE adrp-name_first,       "名
        name       TYPE adrp-name_first,       "姓名
        bname      TYPE usr21-bname,          "登录名
        department TYPE adcp-department,      "部门
        function   TYPE adcp-function,        "职能
      END OF ty_user.
DATA: wa_user TYPE ty_user,
      it_user TYPE ty_user OCCURS 0 WITH HEADER LINE.
DATA: t_work    TYPE TABLE OF swncaggusertcode "SWNCHITLIST
      WITH HEADER LINE,            "Tabella temporanea
      t_dirmoni TYPE TABLE OF swncmonikey
      WITH HEADER LINE.          "Workload component
DATA: t_output TYPE TABLE OF zsworkload WITH HEADER LINE.
DATA: itsktp TYPE swnctasktyperaw.
DATA: BEGIN OF t_elcod OCCURS 0,
        ztcode TYPE zsworkload-ztcode,
      END OF t_elcod.
DATA: BEGIN OF it_tstcp OCCURS 0,
        tcode     TYPE tstcp-tcode,
        zrepnm    TYPE zsworkload-zrepnm,
        zdesrepnm TYPE zsworkload-zdesrepnm,
      END OF it_tstcp.
CONSTANTS: znamestruc TYPE dd02l-tabname VALUE 'ZSWORKLOAD'.
DATA: gt_lvc           TYPE lvc_t_fcat,
      gt_sort          TYPE lvc_t_sort,
      gs_layout        TYPE lvc_s_layo,   "alv的格式
      gs_variant       TYPE disvariant,
      gs_grid_settings TYPE lvc_s_glay,
      gs_lvc           TYPE lvc_s_fcat,
      gs_sort          TYPE lvc_s_sort,
      gv_repid         LIKE sy-repid, "SY-REPID 指 当前的主程序
      gt_events        TYPE TABLE OF slis_alv_event WITH HEADER LINE, "保存AVL事件
      gs_events        LIKE LINE OF gt_events.
DATA: gt_exclude TYPE slis_t_extab,
      gs_exclude TYPE slis_extab.

*-----------------------------------------------------------------------
* ALV specific Declarations...........................................
*-----------------------------------------------------------------------
* ALV specific Internal table declarations.............................
DATA: i_field_cat TYPE lvc_t_fcat, " Field catalogue
      i_alv_sort  TYPE lvc_t_sort. " Sort table
* ALV variables........................................................
DATA: w_alv_layout  TYPE lvc_s_layo, " ALV Layout
      w_alv_save    TYPE c, " ALV save
      w_alv_variant TYPE disvariant. " ALV Variant

*----------------------------------------------------------------------*
*                     选择屏幕定义
*----------------------------------------------------------------------*
* Selection parameters
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
  SELECT-OPTIONS: s_period FOR tdevc-created_on,
  s_tcode FOR tstc-tcode,
  s_user FOR user_addrp-name_text.
*              S_REPTR FOR C.

  PARAMETERS: rd1 RADIOBUTTON GROUP rgp DEFAULT 'X',
              rd2 RADIOBUTTON GROUP rgp,
              rd3 RADIOBUTTON GROUP rgp.



SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*                     INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_user-low.

  PERFORM frm_get_kostl.

AT SELECTION-SCREEN.

START-OF-SELECTION.
* Carica dati di output
  PERFORM get_data_workload.
  DELETE t_output WHERE ddtext = ''.
  IF NOT t_output[] IS INITIAL.
    PERFORM list_output_to_alv. " Perform ALV Output operations
  ENDIF.


*----------------------------------------------------------------------*
*                    END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* FORM LIST_OUTPUT_TO_ALV *
*----------------------------------------------------------------------*
* This subroutine is used to call the screen for ALV Output. *
*----------------------------------------------------------------------*
* There are no interface parameters to be passed to this subroutine. *
*----------------------------------------------------------------------*
FORM list_output_to_alv.
  PERFORM init_layout.             "设置输出格式
  PERFORM init_sort.               "设置排序、合计
  PERFORM init_variant.            "设置变式控制
  PERFORM frm_init_lvc.
  PERFORM frm_exclude.
  PERFORM frm_build_event.
  PERFORM frm_output TABLES gt_lvc        "输出
    gt_sort
    t_output[]
  USING 'ALV_PF_STATUS'
        'ALV_USER_COMMAND'
        gs_layout
        gs_variant
        gs_grid_settings.
*
ENDFORM. " LIST_OUTPUT_TO_ALV

FORM alv_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING pt_extab.
ENDFORM.
*FORM ALV_USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
*                            RS_SELFIELD TYPE SLIS_SELFIELD.
*
*  DATA: L_SUBRC TYPE SYSUBRC.
*
*  CASE R_UCOMM.
*    when 'ONLI'.
*    WHEN OTHERS.
*  ENDCASE.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_data_workload
*&---------------------------------------------------------------------*
*      text
*----------------------------------------------------------------------*
FORM get_data_workload.
  DATA: i_valido TYPE c.
  DATA: i_entry(40),
        i_account TYPE zsworkload-account.
  TYPES: BEGIN OF t_steps,
           period       TYPE zsworkload-period,
           zzdatum      TYPE datum,                "add at 2017.11.08, by wuxiguang
           account      TYPE zsworkload-account,
           entry_id(40),
           count        TYPE zsworkload-zsteps,
         END OF t_steps.
  DATA: it_steps TYPE HASHED TABLE OF t_steps
        WITH UNIQUE KEY period zzdatum account entry_id.
  DATA: h_steps TYPE t_steps.

  DATA:
    l_tabix      TYPE i,
    lw_output    TYPE zsworkload,
    lw_account   TYPE t_steps,
    lw_steps_sum TYPE t_steps,
    lt_output    TYPE STANDARD TABLE OF zsworkload,
    lt_account   TYPE STANDARD TABLE OF t_steps,        "人数
    lt_steps_sum TYPE STANDARD TABLE OF t_steps.        "次数


  REFRESH: t_dirmoni, t_output, it_steps, it_tstcp, t_elcod.
* Lettura carichi di lavoro
  CALL FUNCTION 'SWNC_COLLECTOR_GET_DIRECTORY'
    EXPORTING
      get_dir_from_cluster = ' '
      exclude_summary      = ' '
    TABLES
      directory_keys       = t_dirmoni
    EXCEPTIONS
      no_data_found        = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
  ENDIF.
  " Application servers WORKLOAD
* with month registration period
  IF rd1 = 'X'.
    LOOP AT t_dirmoni WHERE component NE 'TOTAL' OR
    periodtype NE 'D'.
      DELETE t_dirmoni.
    ENDLOOP.
  ELSE.
    LOOP AT t_dirmoni WHERE component NE 'TOTAL' OR
    periodtype NE 'M'.
      DELETE t_dirmoni.
    ENDLOOP.
  ENDIF.
*  DELETE t_dirmoni WHERE assigndsys <> sy-sysid.  "只处理登陆系统的
  LOOP AT t_dirmoni.
    REFRESH t_work.
    " Read aggregated activity
    CHECK t_dirmoni-periodstrt IN s_period.
    CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
      EXPORTING
        component     = t_dirmoni-component
        periodtype    = t_dirmoni-periodtype
        periodstrt    = t_dirmoni-periodstrt
*       SUMMARY_ONLY  = 'X'
      TABLES
        usertcode     = t_work
*       HITLIST_DATABASE = t_work
      EXCEPTIONS
        no_data_found = 1
        OTHERS        = 2.
    SELECT   name_last
    name_first
    usr21~bname
    INTO TABLE it_user
    FROM usr21
    INNER JOIN adrp ON adrp~persnumber = usr21~persnumber.
    SORT t_work BY account entry_id.
    CLEAR: i_entry, i_account.
    LOOP AT t_work.
* Step Counter for each report/transaction
      CLEAR h_steps.
      MOVE t_work-account TO h_steps-account.
      MOVE t_work-entry_id(40) TO h_steps-entry_id.
      MOVE t_dirmoni-periodstrt(6) TO h_steps-period.
      IF rd1 = 'X'.
        MOVE t_dirmoni-periodstrt TO h_steps-zzdatum.
      ENDIF.
      MOVE t_work-count TO h_steps-count. "numero di steps
      IF rd1 = 'X'.
        READ TABLE it_steps WITH KEY account  = h_steps-account
        entry_id = h_steps-entry_id
        zzdatum = h_steps-zzdatum
        period  = h_steps-period
        TRANSPORTING NO FIELDS.
      ELSE.
        READ TABLE it_steps WITH KEY account  = h_steps-account
        entry_id = h_steps-entry_id
        period  = h_steps-period
        TRANSPORTING NO FIELDS.
      ENDIF.
      IF sy-subrc EQ 0. "If already exists
        " Collect data steps
        COLLECT h_steps INTO it_steps.
        DELETE t_work.
        CONTINUE.
      ELSE.
        " Collectdata steps
        COLLECT h_steps INTO it_steps.
      ENDIF.
      "Filter to verify selection conditions
      CLEAR t_output.
      MOVE t_work-account TO t_output-account.
      MOVE t_work-entry_id+72(1) TO t_output-typelem.
      MOVE t_work-entry_id(40) TO t_output-ztcode. "Save transaction code or report
      CONDENSE t_output-ztcode NO-GAPS.
      IF NOT t_work-entry_id+40(32) IS INITIAL. "If name defined = JOB
        MOVE 'B' TO t_output-zlncht.
      ENDIF.
      IF rd1 = 'X'.
        MOVE t_dirmoni-periodstrt TO t_output-zzdatum.  "add at 2017.11.08, by wuxiguang
      ENDIF.
      MOVE t_dirmoni-periodstrt(6) TO t_output-period.
      MOVE t_work-tasktype TO t_output-destasktype. "To convert
      PERFORM verifica_selezioni USING t_output
      CHANGING i_valido.
      IF i_valido EQ 'X'.
        PERFORM add_info CHANGING t_output.
        APPEND t_output.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  IF t_output[] IS INITIAL.
    MESSAGE i000(fb) WITH TEXT-e01.
  ENDIF.


  SELECT  tcode AS ztcode INTO TABLE t_elcod FROM tstct FOR ALL ENTRIES IN t_output WHERE tcode = t_output-ztcode.
  SELECT  tcode AS ztcode  tcode AS zdesrepnm  ttext AS zrepnm  INTO TABLE it_tstcp FROM tstct FOR ALL ENTRIES IN t_output WHERE tcode = t_output-ztcode.
  SELECT   adrp~name_last
  adrp~name_first
  usr21~bname
  adcp~department
  adcp~function

  INTO CORRESPONDING FIELDS OF TABLE it_user
  FROM usr21
  INNER JOIN adrp ON adrp~persnumber = usr21~persnumber
  INNER JOIN adcp ON adrp~persnumber = adcp~persnumber .


  SORT t_output BY period zzdatum account ztcode zrepnm.
  DELETE ADJACENT DUPLICATES FROM t_output COMPARING period zzdatum account ztcode zrepnm.
  LOOP AT t_output.
    CLEAR: h_steps.
    IF rd1 = 'X'.
      READ TABLE it_steps INTO h_steps WITH KEY entry_id = t_output-ztcode
      period = t_output-period
      zzdatum = t_output-zzdatum   "add at 2017.11.08, by wuxiguang
      account = t_output-account.
    ELSE.
      READ TABLE it_steps INTO h_steps WITH KEY entry_id = t_output-ztcode
      period = t_output-period
      account = t_output-account.
    ENDIF.
    IF sy-subrc EQ 0.
      "Assign step value calculated
      t_output-zsteps = h_steps-count.
    ENDIF.
    READ TABLE it_user INTO  wa_user WITH  KEY bname = t_output-account.
    IF sy-subrc EQ 0.
      t_output-department = wa_user-department.
      t_output-function = wa_user-function.
    ENDIF.



    MODIFY t_output.

  ENDLOOP.

  "do nothing.
  DELETE t_output WHERE nmaccount = ''.
  IF s_user IS INITIAL.
    DELETE t_output WHERE account EQ 'ANSS' OR account EQ  'TMSADM' OR account EQ 'HD-USER00' OR  account EQ 'ZTEST'.
  ENDIF.

  IF rd1 = 'X'.
    "do nothing
  ELSEIF rd2 = 'X'.
    "按月份、程序汇总，得到程序的使用人数、次数
    CLEAR: lt_account, lt_steps_sum, lt_output.
    LOOP AT t_output WHERE account IS NOT INITIAL.
      CLEAR lw_account.
      lw_account-period = t_output-period.
      lw_account-entry_id = t_output-ztcode.
      lw_account-account = t_output-account.
      COLLECT lw_account INTO lt_account.
    ENDLOOP.

    LOOP AT t_output WHERE ztcode IS NOT INITIAL.
      CLEAR lw_steps_sum.
      lw_steps_sum-period = t_output-period.
      lw_steps_sum-entry_id = t_output-ztcode.
      lw_steps_sum-count = t_output-zsteps.
      COLLECT lw_steps_sum INTO lt_steps_sum.
    ENDLOOP.

    LOOP AT t_output.
      CLEAR lw_output.
      lw_output-period = t_output-period.
      lw_output-ztcode = t_output-ztcode.
      lw_output-ddtext = t_output-ddtext.
      lw_output-zdesrepnm = t_output-zdesrepnm.
      lw_output-devclass = t_output-devclass.
      lw_output-desclass = t_output-desclass.
      lw_output-desclass = t_output-desclass.
      lw_output-compid = t_output-compid.
      lw_output-compid = t_output-compid.
      lw_output-cnam = t_output-cnam.
      lw_output-cdat = t_output-cdat.
      lw_output-cnamod = t_output-cnamod.
      lw_output-cdatmod = t_output-cdatmod.
      lw_output-destasktype = t_output-destasktype.
      COLLECT lw_output INTO lt_output.
    ENDLOOP.
    LOOP AT lt_output INTO lw_output.
      CLEAR: l_tabix.
      LOOP AT lt_account INTO lw_account WHERE period = lw_output-period
      AND entry_id = lw_output-ztcode.
        l_tabix = l_tabix + 1.
      ENDLOOP.
      lw_output-zcounts = l_tabix.

      CLEAR lw_steps_sum.
      READ TABLE lt_steps_sum INTO lw_steps_sum WITH KEY period = lw_output-period
      entry_id = lw_output-ztcode.
      lw_output-zsteps = lw_steps_sum-count.
      MODIFY lt_output FROM lw_output TRANSPORTING zcounts zsteps.
    ENDLOOP.
    t_output[] = lt_output.

  ELSEIF rd3 = 'X'.
    lt_output = t_output[].
    SORT lt_output BY  account ztcode.
    CLEAR t_output[].                   "清空原有数据

    "按照程序名和用户汇总数据
*    LOOP AT LT_OUTPUT INTO LW_OUTPUT
*     GROUP BY  ( ZTCODE = LW_OUTPUT-ZTCODE  ACCOUNT = LW_OUTPUT-ACCOUNT )
*      ASCENDING
*      ASSIGNING FIELD-SYMBOL(<FS>).
*
*      LW_OUTPUT-PERIOD = ''.
*  LOOP AT GROUP <FS> ASSIGNING FIELD-SYMBOL(<ty>).
*    "这里可以进行求和啥的
*    LW_OUTPUT-ZSTEPS = LW_OUTPUT-ZSTEPS + <ty>-ZSTEPS.
*  ENDLOOP.
*
*ENDLOOP.

    DATA: v_tabix    TYPE i,
          wa_output  TYPE zsworkload,
          wa1_output TYPE zsworkload.
    LOOP AT lt_output INTO wa1_output.
      READ TABLE lt_output INTO lw_output WITH  KEY account = wa1_output-account  ztcode = wa1_output-ztcode.
      IF sy-subrc = 0.
        v_tabix = sy-tabix.
        LOOP AT lt_output INTO wa_output FROM v_tabix.
          IF wa_output-account NE lw_output-account OR wa_output-ztcode NE lw_output-ztcode. EXIT.ENDIF.
          lw_output-period = ''.
          lw_output-zsteps = lw_output-zsteps + wa_output-zsteps.
        ENDLOOP.
        APPEND lw_output TO t_output[].
      ENDIF.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM t_output[].
  ENDIF.
  "add end at 2017.11.09

ENDFORM.                    "get_data_workload
*&---------------------------------------------------------------------*
*&      Form  add_info
*&---------------------------------------------------------------------*
*      Info details
*----------------------------------------------------------------------*
*      -->T_OUTPUT  output table
*----------------------------------------------------------------------*
FORM add_info CHANGING t_output TYPE zsworkload.
  DATA: ipgmna   TYPE tstc-pgmna,
        ifctr_id TYPE tdevc-component.
  DATA: iconta TYPE i.
  DATA: search_trsn TYPE string.
  DATA: l_name_first TYPE string.
  DATA: l_name_last TYPE string.
  " Conversion task type description
  CLEAR itsktp.
  MOVE t_output-destasktype TO itsktp.
  CLEAR t_output-destasktype.
  CALL METHOD cl_swnc_collector_info=>translate_tasktype
    EXPORTING
      tasktyperaw = itsktp
    RECEIVING
      tasktype    = t_output-destasktype.
  " Set extend name account
  CLEAR t_output-nmaccount.

  SELECT SINGLE name_first name_last FROM v_usr_name INTO (l_name_first,l_name_last)
  WHERE bname EQ t_output-account.
  CONCATENATE l_name_last l_name_first  INTO t_output-nmaccount.
  IF t_output-typelem EQ 'T'.
****************** DATA on transaction*****************************
    CLEAR t_output-ddtext.
    SELECT SINGLE ttext FROM tstct INTO t_output-ddtext
    WHERE tcode EQ t_output-ztcode AND
    sprsl EQ sy-langu.
* Development class
*  Component
* Creator
* Creation Data
* Modification author
* Modification date
    CLEAR: t_output-devclass.
    SELECT SINGLE devclass
    FROM tadir INTO t_output-devclass
    WHERE obj_name EQ t_output-ztcode AND
    object EQ 'TRAN' AND
    pgmid EQ 'R3TR'.
    " Search application component
    CLEAR: t_output-compid, ifctr_id.
    IF NOT t_output-devclass IS INITIAL.
      SELECT SINGLE component FROM tdevc INTO ifctr_id
      WHERE devclass EQ t_output-devclass.
      IF sy-subrc EQ 0.
        SELECT SINGLE ps_posid FROM df14l INTO t_output-compid
        WHERE fctr_id EQ ifctr_id.
      ENDIF.
    ENDIF.
    CLEAR: ipgmna.
    SELECT SINGLE pgmna FROM tstc INTO ipgmna
    WHERE tcode EQ t_output-ztcode.
    CLEAR: t_output-cnam, t_output-cdat, t_output-cnamod, t_output-cdatmod.
    SELECT SINGLE cnam cdat unam udat
    FROM trdir INTO (t_output-cnam,t_output-cdat,t_output-cnamod,t_output-cdatmod)
    WHERE name EQ ipgmna.
* Development class description
    CLEAR t_output-desclass.
    SELECT SINGLE ctext FROM tdevct INTO t_output-desclass
    WHERE devclass EQ t_output-devclass AND
    spras EQ sy-langu.
  ELSEIF t_output-typelem EQ 'R'.
****************** DATA on REPORT *****************************
* Object description
    CLEAR t_output-ddtext.
    SELECT SINGLE text FROM trdirt INTO t_output-ddtext
    WHERE name EQ t_output-ztcode AND
    sprsl EQ sy-langu.
* Development Class
* Component
* Creator
* Creation data
* Modification author
* Modification date
    CLEAR: t_output-devclass.
    SELECT SINGLE devclass
    FROM tadir INTO t_output-devclass
    WHERE obj_name EQ t_output-ztcode AND
    object EQ 'PROG' AND
    pgmid EQ 'R3TR'.
    " Search application component
    CLEAR: t_output-compid, ifctr_id.
    IF NOT t_output-devclass IS INITIAL.
      SELECT SINGLE component FROM tdevc INTO ifctr_id
      WHERE devclass EQ t_output-devclass.
      IF sy-subrc EQ 0.
        SELECT SINGLE ps_posid FROM df14l INTO t_output-compid
        WHERE fctr_id EQ ifctr_id.
      ENDIF.
    ENDIF.
    CLEAR: t_output-cnam, t_output-cdat, t_output-cnamod, t_output-cdatmod.
    SELECT SINGLE cnam cdat unam udat
    FROM trdir INTO (t_output-cnam,t_output-cdat,t_output-cnamod,t_output-cdatmod)
    WHERE name EQ t_output-ztcode.
* Development class description
    CLEAR t_output-desclass.
    SELECT SINGLE ctext FROM tdevct INTO t_output-desclass
    WHERE devclass EQ t_output-devclass AND
    spras EQ sy-langu.
* For report search transactions code related
    CLEAR: iconta, search_trsn. ", it_tstcp,.
    CONCATENATE '%D_SREPOVARI-REPORT=' t_output-ztcode '%' INTO search_trsn.
    CONDENSE search_trsn NO-GAPS.
*    CLEAR: TSTCP, T_OUTPUT-ZREPNM, T_OUTPUT-ZDESREPNM, IT_TSTCP.
    CLEAR: tstcp, t_output-zrepnm, t_output-zdesrepnm.
  ENDIF.
ENDFORM.                    "add_info
*&---------------------------------------------------------------------*
*&      Form  verifica_selezioni
*&---------------------------------------------------------------------*
*      text
*----------------------------------------------------------------------*
*      -->F_LINE    text
*      -->F_VALIDO  text
*----------------------------------------------------------------------*
FORM verifica_selezioni USING f_line TYPE zsworkload "SWNCHITLIST
CHANGING f_valido.
  CLEAR f_valido.
  CHECK NOT f_line-ztcode IS INITIAL AND f_line-ztcode IN s_tcode.
  CHECK NOT f_line-account IS INITIAL AND f_line-account IN s_user.
  f_valido = 'X'.
ENDFORM.                    "verifica_selezioni

*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT
*&      布局控制
*&      ZEBRA 可选行颜色,如果设置了，出现了间隔色带 SPACE, ‘X’
*&      CWIDTH_OPT 最优化宽度 SPACE, ‘X’
*&      SEL_MODE 选择模式 SPACE, ‘A’, ‘B’, ‘C’, ‘D’
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM init_layout .
  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-sel_mode   = 'A'.
*  GS_LAYOUT-BOX_FNAME = 'CHECK'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_SORT
*&      排序
*&---------------------------------------------------------------------*

FORM init_sort .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_variant .
  CLEAR: gs_variant.
*  GS_VARIANT-REPORT = SY-REPID.
*  GS_VARIANT-HANDLE = '0001'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_INIT_LVC
*&     设置ALV字段
*& 这里的字段就是ALV报表即将显示的字段,注意这里的字段名都必须大写否则报错
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
DEFINE init_fieldcat.      "  ALV Fieldcat Setting
  gs_lvc-fieldname  = &1.
  gs_lvc-coltext    = &2.
  gs_lvc-scrtext_l  = &2.
  gs_lvc-scrtext_m  = &2.
  gs_lvc-scrtext_s  = &2.
  gs_lvc-reptext    = &2.
  gs_lvc-outputlen  = &3.
  gs_lvc-cfieldname = &4.
  gs_lvc-qfieldname = &5.
  gs_lvc-checkbox   = &6.
  gs_lvc-fix_column = &7.
  gs_lvc-ref_table  = &8.
  gs_lvc-ref_field  = &9.
  APPEND gs_lvc TO gt_lvc.
  CLEAR gs_lvc.
END-OF-DEFINITION.
FORM frm_init_lvc .
  IF rd1 = 'X' OR rd3 = 'X'.
*    INIT_FIELDCAT  'MANDT'  '集团'         '' '' '' '' '' '' ''.
    init_fieldcat  'PERIOD'  '分析期间'   '' '' '' '' '' '' ''.
    init_fieldcat  'ZZDATUM'  '日期'        '' '' '' '' '' '' ''.
    init_fieldcat  'ACCOUNT'  '用户名称'        '' '' '' '' '' 'usr02' 'BNAME'.
    init_fieldcat  'NMACCOUNT'  '完整用户名称'  '' '' '' '' '' '' ''.
    init_fieldcat  'ZTCODE'  'TCODE'  '' '' '' '' '' '' ''.
    init_fieldcat  'ZSTEPS'  '交互数'   '' '' '' '' '' '' ''.
    init_fieldcat  'ZCOUNTS'  '使用人数'         '' '' '' '' '' '' ''.
*  INIT_FIELDCAT  'NMACCOUNT'  'SQL：跟踪评估的事务'     '' '' '' '' '' '' ''.
    init_fieldcat  'DEPARTMENT'  '部门'     '' '' '' '' '' '' ''.
    init_fieldcat  ' FUNCTION'  '职能'     '' '' '' '' '' '' ''.
    init_fieldcat  'ZREPNM'  '自定义字段'     '' '' '' '' '' '' ''.
    init_fieldcat  'DDTEXT'  '程序名称'     '' '' '' '' '' '' ''.
    init_fieldcat  'DEVCLASS'  '报告标题'     '' '' '' '' '' '' ''.
    init_fieldcat  'DESCLASS'  '包'     '' '' '' '' '' '' ''.
    init_fieldcat  'DESCLASS'  '资源'     '' '' '' '' '' '' ''.
    init_fieldcat  'COMPID'  '应用程序组件标识'     '' '' '' '' '' '' ''.
    init_fieldcat  'CNAM'  '作者'     '' '' '' '' '' '' ''.
    init_fieldcat  'CDAT'  '创建日期'     '' '' '' '' '' '' ''.
    init_fieldcat  'CNAMOD'  '最后修改人'     '' '' '' '' '' ' ' ' '.
    init_fieldcat  'CDATMOD'  '最后修改时间'     '' '' '' '' '' '' ''.
    init_fieldcat  'DESTASKTYPE'  '自定义字段'     '' '' '' '' '' '' ''.
  ELSE.
*    INIT_FIELDCAT  'MANDT'  '集团'         '' '' '' '' '' '' ''.
    init_fieldcat  'PERIOD'  '分析期间'   '' '' '' '' '' '' ''.
    init_fieldcat  'ZTCODE'  'TCODE'  '' '' '' '' '' '' ''.
    init_fieldcat  'ZSTEPS'  '交互数'   '' '' '' '' '' '' ''.
    init_fieldcat  'ZCOUNTS'  '使用人数'         '' '' '' '' '' '' ''.
    init_fieldcat  'DDTEXT'  '程序名称'     '' '' '' '' '' '' ''.
*    INIT_FIELDCAT  'DEVCLASS'  '报告标题'     '' '' '' '' '' '' ''.
*    INIT_FIELDCAT  'DESCLASS'  '包'     '' '' '' '' '' '' ''.
*    INIT_FIELDCAT  'DESCLASS'  '资源'     '' '' '' '' '' '' ''.
*    INIT_FIELDCAT  'COMPID'  '应用程序组件标识'     '' '' '' '' '' '' ''.
    init_fieldcat  'CNAM'  '作者'     '' '' '' '' '' '' ''.
    init_fieldcat  'CDAT'  '创建日期'     '' '' '' '' '' '' ''.
    init_fieldcat  'CNAMOD'  '最后修改人'     '' '' '' '' '' ' ' ' '.
    init_fieldcat  'CDATMOD'  '最后修改时间'     '' '' '' '' '' '' ''.
    init_fieldcat  'DESTASKTYPE'  '自定义字段'     '' '' '' '' '' '' ''.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_EXCLUDE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_exclude .
  REFRESH gt_exclude.
  CLEAR gs_exclude.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_BUILD_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_build_event .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LVC  text
*      -->P_GT_SORT  text
*      -->P_GT_DATA  text
*      -->P_0186   text
*      -->P_0187   text
*      -->P_GS_LAYOUT  text
*      -->P_GS_VARIANT  text
*      -->P_GS_GRID_SETTINGS  text
*----------------------------------------------------------------------*
FORM frm_output  TABLES pt_lvc  TYPE lvc_t_fcat
  pt_sort TYPE lvc_t_sort
  pt_data
USING  pv_status
      pv_ucomm
      pv_layout  TYPE lvc_s_layo
      pv_variant TYPE disvariant
      pv_grid_settings TYPE lvc_s_glay.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = sy-repid
      i_callback_pf_status_set = pv_status
*     I_CALLBACK_USER_COMMAND  = PV_UCOMM
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         = ''
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
      i_grid_settings          = pv_grid_settings
      is_layout_lvc            = pv_layout
      it_fieldcat_lvc          = pt_lvc[]
      it_excluding             = gt_exclude
*     IT_SPECIAL_GROUPS_LVC    =
      it_sort_lvc              = pt_sort[]
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
      is_variant               = pv_variant
      it_events                = gt_events[]
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = pt_data
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_GET_KOSTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_get_kostl .
  SELECT   name_last
  name_first
  usr21~bname
  INTO TABLE it_user
  FROM usr21
  INNER JOIN adrp ON adrp~persnumber = usr21~persnumber.

  LOOP AT it_user INTO wa_user.
    CONCATENATE wa_user-name_last  wa_user-name_first INTO wa_user-name.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'NAME'  "搜索帮助内表要输出的帮助字段名
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_USER-LOW'
      value_org   = 'S'
    TABLES
      value_tab   = it_user. "存储搜索帮助内容的内表
  IF sy-subrc <> 0.
    MESSAGE '没有相关搜索帮助' TYPE 'I'.
  ENDIF.
ENDFORM.

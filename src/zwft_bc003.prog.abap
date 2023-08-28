*&---------------------------------------------------------------------*
*&程序名称/Program Name         : ZBCC003
*&程序描述/Program Des.         : 角色批量导入(通用角色派生)
*& 所属模块/Module Belongs      : BC
*& 开发单位/Development Company : PBIT
*&作者/Author                   : P149643
*&完成日期/Completion Date      : 2022-10-19
*&---------------------------------------------------------------------*
*&摘要：
*&   一. 业务背景
*&      1).期初上线时，客户存在大量的公司需要创建本地角色，通用角色可以
*&         手工创建，但是本地角色数据巨大，通过批导产生，提高工作效率；
*&      2).减小上线时角色工作的压力和工作量，降低上线时角色创建带来的风险；
*&   二. 使用场景
*&       1).发布导入模板;
*&       2).客户按照导入模板格式及各个字段意义正确填写对应的数据，
*&          并保证数据真实有效;
*&       3).客户收集对应的模板数据，并检查是否符合规范;
*&       4).客户使用导入程序导入模板数据;
*&       5).客户前台检查导入数据是否正确
*&   三. 程序使用：
*&      1).下载主数据导入模板；
*&         SMWO 模板上载首先设置文件类型：
*&         1>.smwo --> WebRFC应用程序的二进制数据；
*&         2>.设置 --> 定义MIME类型;type：excel；
*&              description：*.xls,*.xlsx
*&         3>.上传excle(.xls)文件ZCOMB_806
*&      2).上载主数据EXCEL文件，ALV显示；
*&         注意：此处展示的是导入的结果的ALV；
*&               数据量较大时建议分批导入，实际使用时100个本地角色导入
*&               没有问题
*&---------------------------------------------------------------------*
*&变更记录：                                                           *
*&Date         Developer           ReqNo       Descriptions            *
*& ==========  ==================  ==========  ========================*
*& 2022-10-19  P149643 / 程成      S4DK900060  初始开发
*&---------------------------------------------------------------------*
REPORT zwft_bc003.

*----------------------------------------------------------------------*
* 常量声明
*----------------------------------------------------------------------*
CONSTANTS: gc_objid TYPE wwwdatatab-objid VALUE 'ZBCC003',
           gc_relid TYPE w3_relid         VALUE 'MI'.

CONSTANTS: gc_object_p     TYPE agr_1251-object VALUE 'K_PCA',
           gc_object_k     TYPE agr_1251-object VALUE 'K_CCA',
           gc_object_field TYPE agr_1251-field  VALUE 'RESPAREA'.

CONSTANTS: gc_type_c TYPE c VALUE 'C',    "字母开头
           gc_type_1 TYPE c VALUE '1'.    "数字开头

CONSTANTS: gc_resparea_p       TYPE char6 VALUE 'PCPB00',
           gc_resparea_p_group TYPE char6 VALUE 'PHPB00',
           gc_resparea_k       TYPE char6 VALUE 'KSPB00',
           gc_resparea_k_group TYPE char6 VALUE 'KNPB00'.

CONSTANTS: gc_presparea TYPE agr_1251-field VALUE 'PRESPAREA',
           gc_kresparea TYPE agr_1251-field VALUE 'KRESPAREA'.

*----------------------------------------------------------------------*
* 数据库表声明
*----------------------------------------------------------------------*
TABLES: agr_texts,sscrfields.

*----------------------------------------------------------------------*
* 声明类型
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tab,
         st01 TYPE agr_name,
         st02 TYPE agr_name,
         st03 TYPE agr_title,
         st04 TYPE xuobject,      "权限对象
         st05 TYPE string,
         st06 TYPE string,
         st07 TYPE string,
         st08 TYPE string,
         st09 TYPE string,
         st10 TYPE string,
         st11 TYPE string,
         st12 TYPE string,
         st13 TYPE string,
         st14 TYPE string,
         st15 TYPE string,
         st16 TYPE string,
         st17 TYPE string,
         st18 TYPE string,
         st19 TYPE string,
         st20 TYPE string,
         st21 TYPE string,
         st22 TYPE string,
         st23 TYPE string,
         st24 TYPE string,
         st25 TYPE string,
         st26 TYPE string,
         st27 TYPE string,
         st28 TYPE string,
         st29 TYPE string,
         st30 TYPE string,
         st31 TYPE string,
         st32 TYPE string,
         st33 TYPE string,
         st34 TYPE string,
         st35 TYPE string,
         st36 TYPE string,
         st37 TYPE string,
         st38 TYPE string,
         st39 TYPE string,
         st40 TYPE string,
         st41 TYPE string,
         st42 TYPE string,
         st43 TYPE string,
         st44 TYPE string,
         st45 TYPE string,
         st46 TYPE string,
         st47 TYPE string,
         st48 TYPE string,
         st49 TYPE string,
         st50 TYPE string,
         st51 TYPE string,
         st52 TYPE string,
         st53 TYPE string,
         st54 TYPE string,
         st55 TYPE string,
         st56 TYPE string,
         st57 TYPE string,
         st58 TYPE string,
         st59 TYPE string,
         st60 TYPE string,
         st61 TYPE string,
         st62 TYPE string,
         st63 TYPE string,
         st64 TYPE string,
         st65 TYPE string,
         st66 TYPE string,
         st67 TYPE string,
         st68 TYPE string,
         st69 TYPE string,
         st70 TYPE string,
         st71 TYPE string,
         st72 TYPE string,
         st73 TYPE string,
         st74 TYPE string,
         st75 TYPE string,
         st76 TYPE string,
         st77 TYPE string,
         st78 TYPE string,
         st79 TYPE string,
         st80 TYPE string,
         st81 TYPE string,
         st82 TYPE string,
         st83 TYPE string,
         st84 TYPE string,
         st85 TYPE string,
         st86 TYPE string,
         st87 TYPE string,
         st88 TYPE string,
         st89 TYPE string,
         st90 TYPE string,
         st91 TYPE string,
         st92 TYPE string,
         st93 TYPE string,
         st94 TYPE string,
         st95 TYPE string,
         st96 TYPE string,
         st97 TYPE string,
         st98 TYPE string,
         text TYPE string,
         led  TYPE int4,
       END OF ty_tab.

TYPES: BEGIN OF ty_mtab,
         m_rol TYPE agr_name,
       END OF ty_mtab.

*----------------------------------------------------------------------*
* 内表/工作区定义
*----------------------------------------------------------------------*
DATA: gt_tab   TYPE TABLE OF ty_tab WITH HEADER LINE.
DATA: it_tab   TYPE TABLE OF ty_tab.
DATA: gt_mtab  TYPE TABLE OF ty_mtab.
DATA: gs_mtab  TYPE ty_mtab.

*&---------------------------------------------------------------------*
*& GLOBLE/全局常量声明
*&---------------------------------------------------------------------*
DATA: gt_bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
      gt_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: gs_tab LIKE LINE OF gt_tab,
      gw_tab LIKE LINE OF gt_tab,
      gx_tab LIKE LINE OF gt_tab.

DATA: gt_252      LIKE pt1252  OCCURS 100 WITH HEADER LINE,
      gt_252_temp LIKE pt1252  OCCURS 100 WITH HEADER LINE,
      gt_old      LIKE pt1251  OCCURS 100 WITH HEADER LINE,
      gt_old_temp LIKE pt1251  OCCURS 100 WITH HEADER LINE.

DATA: ls_low      LIKE gt_old-low .
DATA: ls_high     LIKE gt_old-low .
DATA: it_fs       TYPE TABLE OF string WITH HEADER LINE.
FIELD-SYMBOLS: <fs1> TYPE any,
               <fs2> TYPE any,
               <fs3> TYPE any,
               <fs4> TYPE any.
DATA: gv_all_col   TYPE i VALUE 98, "总列数
      gv_b_col     TYPE i VALUE 4,  "数据开始列
      gv_b_row     TYPE i VALUE 3,  "数据开始行
      gv_col(2)    TYPE n,
      gv_text1(11) TYPE c VALUE 'GT_TAB-ST01',
      gv_text2(11) TYPE c VALUE 'GS_TAB-ST01',
      gv_text3(11) TYPE c VALUE 'GW_TAB-ST01',
      gv_text4(11) TYPE c VALUE 'GX_TAB-ST01'.
DATA: functxt TYPE smp_dyntxt.  "功能代码文本

*&---------------------------------------------------------------------*
*& SELECTION SCREEN/选择屏幕
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-t01.

  PARAMETERS p_path  LIKE rlgrap-filename MEMORY ID me1.
  PARAMETERS p_ps    RADIOBUTTON GROUP grp2 DEFAULT 'X' USER-COMMAND uc2.
  PARAMETERS p_only1 RADIOBUTTON GROUP grp2 .        "仅修改权限

SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-t02.

  " 上传下载选择
  PARAMETERS: rb_up   RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND uc2,
              rb_down RADIOBUTTON GROUP grp1.

SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-t06.

  " 利润中心组与成本中心组的特殊处理
  PARAMETERS: p_ccjg AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blk3.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN/搜索帮助                                        *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  " 根据动作不同，选择不同函数
  IF rb_up = abap_true.
    PERFORM frm_path_help CHANGING p_path.
  ELSEIF rb_down = abap_true.
    PERFORM frm_set_save_path CHANGING p_path.
  ENDIF.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION/开始选择屏幕                                     *
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_path IS INITIAL.
    "请选择文件路径！
    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF rb_up = abap_true.
    "excel导入到内表
    PERFORM frm_upload_excel TABLES gt_tab
                              USING p_path gv_text1 gv_all_col 9 2.
* 数据检查
    PERFORM frm_datacheck.

    IF p_only1 <> 'X'."仅修改权限
      "BDC创建角色
      PERFORM frm_create_role_bdc.
    ENDIF.

    "更改角色权限
    PERFORM frm_change_role_pro.

    "显示日志
    PERFORM frm_show_logmsg TABLES it_tab
                             USING gv_text1 gv_all_col gv_b_col 9 2 7 4.

  ELSEIF rb_down = abap_true.

    PERFORM frm_download_template.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SUB_DATCHECK
*&---------------------------------------------------------------------*
*       text  数据检查
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_datacheck .
  FIELD-SYMBOLS: <fs_tab>   TYPE ty_tab.
* 清除状态栏及日志文本
  LOOP AT gt_tab ASSIGNING <fs_tab>.
    CLEAR: <fs_tab>-led,<fs_tab>-text.
  ENDLOOP.
  it_tab = gt_tab[].
  SORT it_tab BY st01 st02.
  DO gv_b_row TIMES.
    DELETE it_tab INDEX 1.
  ENDDO.
  DELETE ADJACENT DUPLICATES FROM it_tab COMPARING st01 st02.
ENDFORM.                    " SUB_DATCHECK

*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_ROLE_BDC
*&---------------------------------------------------------------------*
*       text  BDC 创建角色
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_create_role_bdc .
  DATA: lv_mode  TYPE c VALUE 'N'.
  DATA: ls_opt  TYPE ctu_params.
  FIELD-SYMBOLS: <fs_tab>   TYPE ty_tab.

  ls_opt-dismode  = 'N'."CALL TRANSACTION USING... 的处理模式
  ls_opt-updmode  = 'S'."CALL TRANSACTION USING...的更新模式
  ls_opt-cattmode	= ''."用于 CALL TRANSACTION USING... 的 CATT 模式
  ls_opt-defsize  = 'X'."CALL TRANSACTION USING 的缺省屏幕大小...
  ls_opt-racommit	= 'X'."CALL TRANSACTION USING... 按照 COMMIT 是不完全的
  ls_opt-nobinpt  = 'X'."CALL TRANSACTION USING 的 SY-BINPT=SPACE ...
  ls_opt-nobiend  = ''."CALL TRANSACTION USING... 数据结束后的 SY-BINPT=SPACE

  LOOP AT it_tab ASSIGNING <fs_tab>.
    "非数据行
*    IF sy-tabix <= gv_b_row.
*      CONTINUE.
*    ENDIF.
    "检查角色字段
    IF <fs_tab>-st01 = '' OR <fs_tab>-st02 = ''.
      <fs_tab>-led = 1.
      <fs_tab>-text = TEXT-003.
      CONTINUE.
    ENDIF.

    "检查通用角色是否存在
    SELECT SINGLE * FROM agr_texts WHERE agr_name = <fs_tab>-st01.
    IF sy-subrc <> 0.
      <fs_tab>-led = 1.
      <fs_tab>-text = TEXT-004.
      CONTINUE.
    ENDIF.

    "检查本地角色是否存在
    SELECT SINGLE * FROM agr_texts WHERE agr_name = <fs_tab>-st02.
    IF sy-subrc = 0.
      "更改角色描述
      PERFORM frm_create_role_bdc_text USING <fs_tab>.

      <fs_tab>-led = 3.
      <fs_tab>-text = TEXT-005.
      CONTINUE.
    ENDIF.

    "BDC创建角色
    CLEAR: gt_bdcdata,gt_bdcdata[],gt_messtab,gt_messtab[].

    PERFORM frm_bdc_dynpro  USING 'SAPLPRGN_TREE' '0121'.
    PERFORM frm_bdc_field   USING 'BDC_CURSOR'    'AGR_NAME_NEU'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE'    '=ANLE'.
    PERFORM frm_bdc_field   USING 'AGR_NAME_NEU'
                                  <fs_tab>-st02.
    PERFORM frm_bdc_dynpro  USING 'SAPLPRGN_TREE' '0300'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE'    '/00'.
    PERFORM frm_bdc_field   USING 'S_AGR_TEXTS-TEXT'
                                  <fs_tab>-st03.
    PERFORM frm_bdc_field   USING 'BDC_CURSOR' 'S_AGR_DEFINE-PARENT_AGR'.
    PERFORM frm_bdc_field   USING 'S_AGR_DEFINE-PARENT_AGR'
                                   <fs_tab>-st01.
    PERFORM frm_bdc_dynpro  USING 'SAPLSPO1'   '0500'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE' '=OPT1'.
    PERFORM frm_bdc_dynpro  USING 'SAPLSPO1'   '0500'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE' '=OPT1'.
    PERFORM frm_bdc_dynpro  USING 'SAPLPRGN_TREE' '0300'.
    PERFORM frm_bdc_field   USING 'BDC_CURSOR'    'S_AGR_TEXTS-TEXT'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE'    '=TAB5'.
    PERFORM frm_bdc_dynpro  USING 'SAPLPRGN_TREE' '0300'.
    PERFORM frm_bdc_field   USING 'BDC_CURSOR'    'S_AGR_TEXTS-TEXT'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE'    '=PSUG'.
    PERFORM frm_bdc_dynpro  USING 'SAPLPRGN_TREE' '0300'.
    PERFORM frm_bdc_field   USING 'BDC_CURSOR'    'S_AGR_TEXTS-TEXT'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE'    '=BACK'.
    PERFORM frm_bdc_dynpro  USING 'SAPLSPO1'      '0500'.
    PERFORM frm_bdc_field   USING 'BDC_OKCODE'    '=OPT1'.

    "CALL TRASACTION
    CALL TRANSACTION 'PFCG'
                 USING gt_bdcdata
*                 MODE  lv_mode"'A'
*                 UPDATE 'S'
                 OPTIONS FROM ls_opt
                 MESSAGES INTO gt_messtab.
    "判断是否创建成功！注：不同消息号可能不同，录屏方式不同消息不同
    READ TABLE gt_messtab WITH KEY msgid = 'S#' msgnr = '405'.
    IF sy-subrc <> 0.
      LOOP AT gt_messtab  WHERE msgtyp = 'A' OR msgtyp = 'E'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = gt_messtab-msgid
            msgnr               = gt_messtab-msgnr
            msgv1               = gt_messtab-msgv1
            msgv2               = gt_messtab-msgv2
            msgv3               = gt_messtab-msgv3
            msgv4               = gt_messtab-msgv4
          IMPORTING
            message_text_output = <fs_tab>-text.
      ENDLOOP.
      IF <fs_tab>-text = ''.
        LOOP AT gt_messtab .
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = gt_messtab-msgid
              msgnr               = gt_messtab-msgnr
              msgv1               = gt_messtab-msgv1
              msgv2               = gt_messtab-msgv2
              msgv3               = gt_messtab-msgv3
              msgv4               = gt_messtab-msgv4
            IMPORTING
              message_text_output = <fs_tab>-text.
        ENDLOOP.
      ENDIF.

      "如果失败则状态为1
      <fs_tab>-led = 1.
    ELSE.
      CLEAR: <fs_tab>-led.
      <fs_tab>-text = TEXT-017.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FRM_CREATE_ROLE_BDC

*&---------------------------------------------------------------------*
*&      Form  FRM_CHANGE_ROLE_PRO
*&---------------------------------------------------------------------*
*       text  修改角色参数
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_change_role_pro .

  DATA: ls_tab    TYPE ty_tab.
  DATA: lv_line TYPE i.
  DATA: object LIKE gt_old-object.
  DATA: lv_tmp TYPE string.
  DATA: lv_str    TYPE string,
        lt_roles  TYPE STANDARD TABLE OF  agr_st_name WITH HEADER LINE,
        lt_return TYPE STANDARD TABLE OF  bapiret2 WITH HEADER LINE.
  DATA: lv_agr_name TYPE agr_name.
  FIELD-SYMBOLS: <fs_mtab>    TYPE ty_mtab.
  FIELD-SYMBOLS: <fs_tab>     TYPE ty_tab.
  DATA: lv_request     TYPE trkorr,
        lt_return_role TYPE TABLE OF bapiret2,
        lt_son_role    TYPE zwft_tagr_name.

* 取得EXCEL非数据的抬头信息
  "取得权限字段名
  CLEAR: gs_tab,gw_tab.
  LOOP AT gt_tab INTO gs_tab FROM gv_b_row.
    IF sy-tabix = gv_b_row.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF gs_tab-st04 = ''.
    EXIT.
  ENDIF.

* 取得权限字段类型
  READ TABLE gt_tab INTO gw_tab INDEX 1.
* 取得权限对象说明
  READ TABLE gt_tab INTO gx_tab INDEX 2.

  SORT it_tab BY st01 st02.
  LOOP AT it_tab ASSIGNING <fs_tab>.

    "取得组织权限对象
    CLEAR:gt_252[],gt_old[].
    CLEAR : gt_old_temp,gt_old_temp[].
    CLEAR : gt_252_temp,gt_252_temp[].

    "判断是否要对通用角色派生
    IF p_ps = 'X' .

      CLEAR:gs_mtab.

      "取得表中的通用角色到表GT_MTAB
      gs_mtab-m_rol = <fs_tab>-st01.

      "对每一个批导的派生角色做权限传递
      CLEAR: lv_request.
      REFRESH: lt_return_role,lt_son_role.
      APPEND <fs_tab>-st02 TO lt_son_role.

      CALL FUNCTION 'ZFM_SUPRN_TRANSFER_AUTH_DATA'
        EXPORTING
          top_activity_group   = gs_mtab-m_rol
          son_activity_group_t = lt_son_role
        IMPORTING
          return               = lt_return_role
        CHANGING
          request              = lv_request.

      lv_agr_name = <fs_tab>-st02.
    ELSE.
      lv_agr_name = <fs_tab>-st01.
    ENDIF.

    "取得组织级别的权限对象
    CALL FUNCTION 'PRGN_1252_READ_ORG_LEVELS'
      EXPORTING
        activity_group    = lv_agr_name
      TABLES
        org_levels        = gt_252
      EXCEPTIONS
        no_data_available = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      gt_tab-led = 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = <fs_tab>-text.
      <fs_tab>-led = 1.
      CONTINUE.
    ENDIF.

    "取得普通字段的权限对象
    CALL FUNCTION 'PRGN_1251_READ_FIELD_VALUES'
      EXPORTING
        activity_group    = lv_agr_name
      TABLES
        field_values      = gt_old
      EXCEPTIONS
        no_data_available = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      gt_tab-led = 1.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = <fs_tab>-text.
      <fs_tab>-led = 1.
      CONTINUE.
    ENDIF.

    "按权限对象及字段排序
    SORT gt_old[] BY object field.
    DELETE gt_old WHERE deleted = 'X'.

*----------------------------------------------------------------------*

    "传递权限后按照权限对象名称对权限字段值进行更改

    LOOP AT gt_tab FROM gv_b_row WHERE st01 = <fs_tab>-st01
                                   AND st02 = <fs_tab>-st02
                                   AND led <> 1  .
      IF sy-tabix <= gv_b_row.
        CONTINUE.
      ENDIF.

      CONDENSE: gt_tab-st04 NO-GAPS.

      gv_col = gv_b_col.

      "循环取当前行的所有字段
      WHILE gv_col <= gv_all_col.

        gv_col = gv_col + 1.

        "取权限字段的值
        gv_text1+9(2) = gv_col.
        ASSIGN (gv_text1) TO <fs1>.
        "取权限字段名
        gv_text2+9(2) = gv_col.
        ASSIGN (gv_text2) TO <fs2>.
        "取权限对象说明
        gv_text4+9(2) = gv_col.
        ASSIGN (gv_text4) TO <fs4>.

        "如果权限字段名为空则不在查找后面字段,退出循环
        IF <fs2> = ''.
          EXIT.
        ENDIF.

        IF p_ccjg = abap_true..
          CASE gt_tab-st04.       "权限对象名
            WHEN gc_object_p.     "利润中心组
              IF <fs2> = gc_presparea.
                <fs2> = <fs2>+1(8).
              ELSEIF <fs2> = gc_kresparea.
                CONTINUE.
              ENDIF.
            WHEN gc_object_k.     "成本中心组
              IF <fs2> = gc_presparea.
                CONTINUE.
              ELSEIF <fs2> = gc_kresparea.
                <fs2> = <fs2>+1(8).
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        "如果权限字段的值不为空
        IF <fs1> <> ''.

          "取权限字段的类型是普通字段还是组织级别
          gv_text3+9(2) = gv_col.
          ASSIGN (gv_text3) TO <fs3>.
          "判断字段是组织级别还是普通
          IF <fs3> = TEXT-006.
            CLEAR: lv_line.
            "更改权限对象内表的值
            LOOP AT gt_old WHERE field = <fs2> AND object = gt_tab-st04.
              "同一权限对象同一字段只执行一次,且保留一行,其他加到临时表GT_OLD_TEMP中
              IF gt_old-object <> object.
                CLEAR: lv_line.
                object = gt_old-object.
              ENDIF.
              lv_line = lv_line + 1.
              DELETE gt_old.
              CLEAR:gt_old-high,ls_low,ls_high.
              "判断导入字段的格式

              IF <fs1> CA ',' . " 多值,或区间值与多值混合

                CLEAR :  it_fs[].
                SPLIT <fs1> AT ',' INTO TABLE it_fs .
                LOOP AT  it_fs .
                  CLEAR:gt_old-low,gt_old-high.
                  CONDENSE  it_fs NO-GAPS.
                  IF it_fs CA '-' .  " 区间给值
                    SPLIT it_fs AT '-' INTO gt_old-low gt_old-high.
                  ELSE.
                    gt_old-low = it_fs.
                  ENDIF.

                  IF p_ccjg = abap_true.
                    PERFORM frm_ccjg_special_logic USING <fs2>
                                                         gt_tab-st04
                                                CHANGING gt_old-low.
                  ENDIF.

                  gt_old-modified = 'M'.

                  APPEND gt_old TO gt_old_temp.

                ENDLOOP .

                IF p_ccjg = abap_true.
                  CASE gt_tab-st04.       "权限对象名
                    WHEN gc_object_p.     "利润中心组
                      <fs2> = gc_presparea.
                    WHEN gc_object_k.     "成本中心组
                      <fs2> = gc_kresparea.
                    WHEN OTHERS.
                  ENDCASE.
                ENDIF.

              ELSEIF <fs1> CA '-' .  " 区间给值

                SPLIT <fs1> AT '-' INTO ls_low ls_high.
                CONDENSE ls_high NO-GAPS.
                CONDENSE ls_low NO-GAPS.
                gt_old-high = ls_high.
                gt_old-low = ls_low.

                IF p_ccjg = abap_true.
                  PERFORM frm_ccjg_special_logic USING <fs2>
                                                       gt_tab-st04
                                              CHANGING gt_old-low.
                ENDIF.

                gt_old-modified = 'M'.
                COLLECT gt_old INTO gt_old_temp[].

                IF p_ccjg = abap_true.
                  CASE gt_tab-st04.       "权限对象名
                    WHEN gc_object_p.     "利润中心组
                      <fs2> = gc_presparea.
                    WHEN gc_object_k.     "成本中心组
                      <fs2> = gc_kresparea.
                    WHEN OTHERS.
                  ENDCASE.
                ENDIF.

              ELSE. " 单值

                gt_old-low = <fs1>.

                IF p_ccjg = abap_true.
                  PERFORM frm_ccjg_special_logic USING <fs2>
                                                       gt_tab-st04
                                              CHANGING gt_old-low.
                ENDIF.

                gt_old-modified = 'M'.
                COLLECT gt_old INTO gt_old_temp[].

                IF p_ccjg = abap_true.
                  CASE gt_tab-st04.       "权限对象名
                    WHEN gc_object_p.     "利润中心组
                      <fs2> = gc_presparea.
                    WHEN gc_object_k.     "成本中心组
                      <fs2> = gc_kresparea.
                    WHEN OTHERS.
                  ENDCASE.
                ENDIF.

              ENDIF.

            ENDLOOP.

          ELSEIF <fs3> = TEXT-007.

            CLEAR lv_tmp.
            "如果是组织级别,处理逻辑同上,权限对象内表不同GT_252
            CONCATENATE '$' <fs2> INTO lv_tmp.
            CLEAR lv_line.
            LOOP AT gt_252 WHERE varbl = lv_tmp.
              lv_line = lv_line + 1.
              IF lv_line > 1.
                DELETE gt_252.
                CONTINUE.
              ENDIF.
              CLEAR:gt_252-high,ls_low,ls_high.
              IF <fs1> CA ',' . " 多值,或区间值与多值混合
                CLEAR :  it_fs[].
                SPLIT <fs1> AT ',' INTO TABLE it_fs .
                LOOP AT it_fs .
                  CLEAR:gt_252-low,gt_252-high.
                  CONDENSE  it_fs NO-GAPS.
                  IF it_fs CA '-' .  " 区间给值
                    SPLIT it_fs AT '-' INTO gt_252-low gt_252-high.
                  ELSE.
                    gt_252-low = it_fs.
                  ENDIF.
                  IF sy-tabix = 1.
                    MODIFY gt_252.
                  ELSE.
                    APPEND gt_252 TO gt_252_temp.
                  ENDIF.
                ENDLOOP .
              ELSEIF <fs1> CA '-' .  " 区间给值
                SPLIT <fs1> AT '-' INTO ls_low ls_high.
                CONDENSE ls_high NO-GAPS.
                CONDENSE ls_low NO-GAPS.
                gt_252-high = ls_high.
                gt_252-low = ls_low.
                MODIFY gt_252.
              ELSE. " 单值
                gt_252-low = <fs1>.
                MODIFY gt_252.
              ENDIF.
            ENDLOOP.

          ENDIF.
        ENDIF.

      ENDWHILE.

    ENDLOOP.
*----------------------------------------------------------------------*

    "多值的情况将临时表中数据追加进主表

    LOOP AT gt_old_temp .
      MOVE-CORRESPONDING gt_old_temp TO gt_old.
      COLLECT gt_old.
      CLEAR:gt_old,gt_old_temp.
    ENDLOOP .
    "删除无法分配的数据
    DELETE gt_old WHERE low = '' AND high = ''.

    LOOP AT gt_252_temp .
      APPEND  gt_252_temp TO gt_252.
    ENDLOOP .

    "从新分配权限对象值
    CALL FUNCTION 'PRGN_1251_SAVE_FIELD_VALUES'
      EXPORTING
        activity_group = <fs_tab>-st02
      TABLES
        field_values   = gt_old.
    CALL FUNCTION 'PRGN_1252_SAVE_ORG_LEVELS'
      EXPORTING
        activity_group = <fs_tab>-st02
      TABLES
        org_levels     = gt_252.

    "更新生成
    CALL FUNCTION 'PRGN_UPDATE_DATABASE'
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc = 0.
      <fs_tab>-led = 3.
      IF gt_tab-text <> ''.
        <fs_tab>-text = TEXT-008.
      ELSE.
        <fs_tab>-text = TEXT-009.
      ENDIF.

      REFRESH:lt_roles,lt_return.
      lt_roles-agr_name = <fs_tab>-st02.
      APPEND lt_roles.
      "生成角色
      CALL FUNCTION 'PRGN_GEN_PROFILES_FOR_ROLES'
        TABLES
          it_roles  = lt_roles
          et_return = lt_return.
      CLEAR:lv_str.
      LOOP AT lt_return WHERE type = 'E' OR type = 'A'.
        CONCATENATE lv_str lt_return-message INTO lv_str.
      ENDLOOP.
      IF sy-subrc = 0.
        CONCATENATE <fs_tab>-text TEXT-010  lv_str
               INTO <fs_tab>-text.
      ENDIF.
    ELSE.
      <fs_tab>-led = 1.
      <fs_tab>-text = TEXT-011.
    ENDIF.

    COMMIT WORK AND WAIT.
    WAIT UP TO '0.1' SECONDS.

    "清空缓存
    CALL FUNCTION 'PRGN_CLEAR_BUFFER'
      EXCEPTIONS
        OTHERS = 1.

  ENDLOOP.

ENDFORM.                    " FRM_CHANGE_ROLE_PRO
*&---------------------------------------------------------------------*
*& Form frm_ccjg_special_logic
*&---------------------------------------------------------------------*
*& 利润中心组和成本中心组的特殊处理
*&---------------------------------------------------------------------*
FORM frm_ccjg_special_logic USING pv_field  TYPE string
                                  pv_object TYPE xuobject
                         CHANGING pv_value  TYPE pt1251-low.

  "正则表达式匹配用
  DATA: lt_results TYPE match_result_tab,
        ls_results TYPE match_result.

  "字母首位
  DATA: lv_first_character TYPE c,
        lv_type            TYPE c.

*&----------------------------------------------------&*

  IF strlen( pv_value ) >= 1.

    CLEAR: lv_first_character,lv_type.
    lv_first_character = pv_value(1).
    IF lv_first_character CA '1234567890'.
      lv_type = gc_type_1.
    ELSE.
      lv_type = gc_type_c.
    ENDIF.
    "字母开头
*    FIND PCRE '[A-Z]+' IN lv_first_character.
*    IF sy-subrc = 0.

*    ELSE.

    "数字开头
*      FIND PCRE '[0-9]+' IN lv_first_character.
*      IF sy-subrc = 0.

*      ENDIF.


    IF pv_field = gc_object_field.      "层次结构字段
      IF pv_object = gc_object_p.

        IF lv_type = gc_type_c.         "利润中心
          pv_value = |{ gc_resparea_p }{ pv_value }|.
        ELSEIF lv_type = gc_type_1.     "组
          pv_value = |{ gc_resparea_p_group }{ pv_value }|.
        ENDIF.

      ELSEIF pv_object = gc_object_k.

        IF lv_type = gc_type_c.         "成本中心
          pv_value = |{ gc_resparea_k }{ pv_value }|.
        ELSEIF lv_type = gc_type_1.     "组
          pv_value = |{ gc_resparea_k_group }{ pv_value }|.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_CREATE_ROLE_BDC_TEXT
*&---------------------------------------------------------------------*
*       text  修改本地角色描述
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_create_role_bdc_text USING ps_tab TYPE ty_tab .
  "更改角色描述
  CLEAR:gt_bdcdata,gt_bdcdata[],gt_messtab,gt_messtab[].
  PERFORM frm_bdc_dynpro   USING 'SAPLPRGN_TREE'            '0121'.
  PERFORM frm_bdc_field    USING 'BDC_OKCODE'               '=AEND'.
  PERFORM frm_bdc_field    USING 'AGR_NAME_NEU'             ps_tab-st02.

  PERFORM frm_bdc_dynpro   USING 'SAPLPRGN_TREE'            '0300'.
  PERFORM frm_bdc_field    USING 'BDC_OKCODE'               '=SAVE'.
  PERFORM frm_bdc_field    USING 'S_AGR_TEXTS-TEXT'         ps_tab-st03.

  PERFORM frm_bdc_dynpro   USING 'SAPLPRGN_TREE'            '0300'.
  PERFORM frm_bdc_field    USING 'BDC_OKCODE'               '=BACK'.

  PERFORM frm_bdc_dynpro   USING 'SAPLPRGN_TREE'            '0121'.
  PERFORM frm_bdc_field    USING 'BDC_OKCODE'               '=BACK'.
  CALL TRANSACTION 'PFCG'
               USING gt_bdcdata
               MODE  'N'
               UPDATE 'S'
               MESSAGES INTO gt_messtab.
ENDFORM.                    " FRM_CREATE_ROLE_BDC_TEXT

*&---------------------------------------------------------------------*
*&      Form  FRM_PATH_HELP
*&---------------------------------------------------------------------*
*       text  文件路径搜索帮助
*----------------------------------------------------------------------*
*      <--P_P_PATH  text
*----------------------------------------------------------------------*
FORM frm_path_help  CHANGING pv_path.
  DATA: pv_file LIKE draw-filep.

  CALL FUNCTION 'CV120_DOC_GET_FILE'
    EXPORTING
      pf_mask2   = '*.XLS;*.XLSX'
      pf_mode    = 'O'
    IMPORTING
      pfx_file   = pv_file
    EXCEPTIONS
      wrong_appl = 1
      error      = 2
      OTHERS     = 3.
  IF sy-subrc <> 0 .
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.
  IF pv_file NE space.
    pv_path = pv_file.
  ENDIF.
ENDFORM.                    " FRM_PATH_HELP
*&---------------------------------------------------------------------*
*&      Form  frm_set_save_path
*&---------------------------------------------------------------------*
*
*       设置文件保存路径
*
*----------------------------------------------------------------------*
*      <--P_FNAME  文件路径
*----------------------------------------------------------------------*
FORM frm_set_save_path CHANGING pv_fullpath TYPE rlgrap-filename.

  DATA: lv_init_path  TYPE string,
        lv_init_fname TYPE string,
        lv_path       TYPE string,
        lv_filename   TYPE string,
        lv_fullpath   TYPE string.

* 初始名称(输出的文件名称)
  lv_init_fname = TEXT-t03.

* 获取桌面路径
  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory    = lv_init_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* 用户选择名称、路径
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_file_name    = lv_init_fname
      initial_directory    = lv_init_path
      prompt_on_overwrite  = abap_true
    CHANGING
      filename             = lv_filename
      path                 = lv_path
      fullpath             = lv_fullpath
*     USER_ACTION          =
*     FILE_ENCODING        =
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0.
    pv_fullpath = lv_fullpath.
  ENDIF.
ENDFORM.                    " frm_set_save_path
*&---------------------------------------------------------------------*
*&      Form  FRM_UPLOAD_EXCEL
*&---------------------------------------------------------------------*
*       text  上载Excel数据
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_upload_excel TABLES gt_tab
                       USING pv_path pv_text1 pv_all_col p1 p2.
  DATA: lv_num(2)      TYPE n.
  DATA: lv_t_raw       TYPE truxs_t_text_data.
  FIELD-SYMBOLS: <fs1> TYPE any.
  TYPE-POOLS:truxs.

  IF pv_path IS INITIAL.
    MESSAGE s001(00) DISPLAY LIKE 'E' WITH TEXT-001.
    STOP.
  ENDIF.
  REFRESH gt_tab[].

  "上载
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = ''
      i_tab_raw_data       = lv_t_raw
      i_filename           = pv_path
    TABLES
      i_tab_converted_data = gt_tab[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S'  NUMBER sy-msgno DISPLAY LIKE 'E'
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

  IF gt_tab[] IS INITIAL.
    MESSAGE s001(00) DISPLAY LIKE 'E' WITH TEXT-002.
    STOP.
  ENDIF.

  LOOP AT gt_tab.
    CLEAR: lv_num.

    WHILE lv_num <= pv_all_col.
      lv_num = lv_num + 1.
      pv_text1+p1(p2) = lv_num.
      ASSIGN (gv_text1) TO <fs1>.
      IF <fs1> <> ''.
        TRANSLATE <fs1> TO UPPER CASE.
        CONDENSE <fs1> NO-GAPS.
      ELSE.
        "避免循环太多次浪费时间
        IF lv_num >= 20.
          EXIT.
        ENDIF.
      ENDIF.
    ENDWHILE.
    MODIFY gt_tab.
  ENDLOOP.
ENDFORM.                    " FRM_UPLOAD_EXCEL

*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text  BDC 屏幕
*----------------------------------------------------------------------*
*      -->P_0601   text
*      -->P_0602   text
*----------------------------------------------------------------------*
FORM frm_bdc_dynpro USING program dynpro.
  CLEAR gt_bdcdata.
  gt_bdcdata-program  = program.
  gt_bdcdata-dynpro   = dynpro.
  gt_bdcdata-dynbegin = 'X'.
  APPEND gt_bdcdata.
ENDFORM.                    "FRM_BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  FRM_BDC_FIELD
*&---------------------------------------------------------------------*
*       text 填充BDC字段
*----------------------------------------------------------------------*
*      -->P_0606   text
*      -->P_0607   text
*----------------------------------------------------------------------*
FORM frm_bdc_field USING fnam fval.

  CLEAR gt_bdcdata.
  gt_bdcdata-fnam = fnam.
  gt_bdcdata-fval = fval.
  APPEND gt_bdcdata.

ENDFORM.                    "FRM_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  FRM_SHOW_LOGMSG
*&---------------------------------------------------------------------*
*       text  展示导入日志
*----------------------------------------------------------------------*
*      -->P_GT_TAB  text
*      -->P_L_TEXT1  text
*      -->P_L_I  text
*----------------------------------------------------------------------*
FORM frm_show_logmsg  TABLES pt_tab
                       USING pv_text1 pv_all_col pv_b_col p1 p2 p3 p4.
  FIELD-SYMBOLS: <fs1>  TYPE any.
  DATA: lv_num(2)       TYPE n.
  DATA: ls_layout_lvc   TYPE lvc_s_layo,
        lt_fieldcat_lvc LIKE TABLE OF lvc_s_fcat WITH HEADER LINE.
  FREE: lt_fieldcat_lvc[].
  CLEAR: ls_layout_lvc,lt_fieldcat_lvc,lv_num.

  "展示结构
  lt_fieldcat_lvc-fieldname = 'TEXT'.
  lt_fieldcat_lvc-coltext = TEXT-012.
  APPEND lt_fieldcat_lvc.

  READ TABLE gt_tab INDEX 3.
  WHILE lv_num <= pv_all_col.
    lv_num = lv_num + 1.
    pv_text1+p1(p2) = lv_num.
    ASSIGN (pv_text1) TO <fs1>.
    IF <fs1> = '' AND lv_num > pv_b_col.
      EXIT.
    ENDIF.
    CLEAR lt_fieldcat_lvc.
    lt_fieldcat_lvc-fieldname = pv_text1+p3(p4).
    lt_fieldcat_lvc-coltext = <fs1>.
    APPEND lt_fieldcat_lvc.
  ENDWHILE.

  ls_layout_lvc-cwidth_opt = 'X'.
  ls_layout_lvc-zebra = 'X'.
  ls_layout_lvc-excp_fname = 'LED'.
  ls_layout_lvc-excp_led = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat_lvc    = lt_fieldcat_lvc[]
      is_layout_lvc      = ls_layout_lvc
    TABLES
      t_outtab           = pt_tab[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FRM_SHOW_LOGMSG

*&---------------------------------------------------------------------*
*&      Form  FRM_DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*       text  下载模板
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM frm_download_template .

  DATA: lv_objdata     LIKE wwwdatatab,
        lv_mime        LIKE w3mime,
        lv_destination LIKE rlgrap-filename,
        lv_objnam      TYPE string,
        lv_rc          LIKE sy-subrc,
        lv_errtxt      TYPE string.

  DATA: lv_filename TYPE string,
        lv_result,
        lv_subrc    TYPE sy-subrc.

  DATA: lv_objid TYPE wwwdatatab-objid .

  lv_objid = gc_objid.  "上传的模版名称

  "查找文件是否存在。
  SELECT SINGLE relid objid
    FROM wwwdata
    INTO CORRESPONDING FIELDS OF lv_objdata
   WHERE srtf2 = 0
     AND relid = gc_relid
     AND objid = lv_objid.

  "判断模版不存在则报错
  IF sy-subrc NE 0 OR lv_objdata-objid EQ space.
    CONCATENATE TEXT-t04 lv_objid TEXT-t05
    INTO lv_errtxt.
    MESSAGE e000(su) WITH lv_errtxt.
  ENDIF.

  lv_filename = p_path.

  "判断本地地址是否已经存在此文件。
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_filename
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.
  IF lv_result EQ abap_true.  "如果存在则删除原始文件，重新覆盖
    CALL METHOD cl_gui_frontend_services=>file_delete
      EXPORTING
        filename             = lv_filename
      CHANGING
        rc                   = lv_subrc
      EXCEPTIONS
        file_delete_failed   = 1
        cntl_error           = 2
        error_no_gui         = 3
        file_not_found       = 4
        access_denied        = 5
        unknown_error        = 6
        not_supported_by_gui = 7
        wrong_parameter      = 8
        OTHERS               = 9.
    IF sy-subrc <> 0 OR lv_subrc <> 0. "如果删除失败，则报错。
      lv_errtxt = TEXT-t08.
      MESSAGE e000(su) WITH lv_errtxt.
    ENDIF.
  ENDIF.

  lv_destination   = p_path.

  "下载模版。
  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = lv_objdata
      destination = lv_destination
    IMPORTING
      rc          = lv_rc.
  IF lv_rc NE 0.
    "模板文件下载失败！
    lv_errtxt = TEXT-e02.
    MESSAGE e000(su) WITH lv_errtxt.
  ENDIF.

ENDFORM.                    "frm_download_template

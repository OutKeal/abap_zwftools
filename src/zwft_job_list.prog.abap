*&---------------------------------------------------------------------*
*& Report  ZRBC_004
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwft_job_list NO STANDARD PAGE HEADING LINE-SIZE 210.

TABLES: tbtcs, tbtcp, tbtco, trdirt.


DATA: BEGIN OF htab OCCURS 0.
        INCLUDE STRUCTURE tbtcs.
DATA: END OF htab.

DATA: BEGIN OF dtab OCCURS 0.
        INCLUDE STRUCTURE htab.
DATA:   stepcount LIKE tbtcp-stepcount,
        progname  LIKE tbtcp-progname,
        variant   LIKE tbtcp-variant,
      END OF dtab.

*DATA: BEGIN OF temp_tab OCCURS 0,
*        name LIKE trdirt-name,
*        text LIKE trdirt-text,
*      END OF temp_tab.

* 岿喊 角青 扒荐 笼拌甫 困茄 函荐
* 林扁啊 "盒"老 版快  : * (1440/林扁(盒)) * 30
* 林扁啊 "矫埃"老 版快: * (24/林扁(矫埃)) * 30
* 林扁啊 "老"老 版快  : * 30
* 林扁啊 "林"老 版快  : * 4
* 林扁啊 "岿"老 版快  : * 1
DATA: mm_cnt1 TYPE i,    " '盒' 扒荐
      mm_cnt2 TYPE i,    " '盒' 岿 角青扒荐
      hh_cnt1 TYPE i,    " '矫埃' 扒荐
      hh_cnt2 TYPE i,    " '矫埃' 岿 角青扒荐
      dd_cnt1 TYPE i,    " '老' 扒荐
      dd_cnt2 TYPE i,    " '老' 岿 角青扒荐
      wk_cnt1 TYPE i,    " '林' 扒荐
      wk_cnt2 TYPE i,    " '林' 岿 角青扒荐
      mt_cnt1 TYPE i,    " '岿' 扒荐
      mt_cnt2 TYPE i.    " '岿' 岿 角青扒荐

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS: zclient  FOR tbtcs-authckman,
                  zjobname FOR tbtcs-jobname  ,
                  zserver  FOR tbtcs-btcsystem,
                  zuser    FOR tbtcs-sdluname,
                  zprgname FOR tbtcp-progname,
                  zclass   FOR tbtcs-jobclass ,
                  periodic FOR tbtcs-periodic,
                  zcdate   FOR sy-datum NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.



TOP-OF-PAGE.
  SET LEFT SCROLL-BOUNDARY COLUMN 40.
  PERFORM header_display.

START-OF-SELECTION.
  PERFORM select_data.
  PERFORM modify_data.
  DATA(falv) = zcl_falv=>create( CHANGING ct_table = dtab[] ).
  falv->layout->set_cwidth_opt( abap_true ).
  falv->layout->set_zebra( abap_true ).
  falv->layout->set_sel_mode( 'A' ).
  falv->layout->set_no_toolbar( 'X' ).
  falv->display( ).
*  PERFORM write_data.



*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data.
  REFRESH: htab, dtab. CLEAR: htab, dtab.

  CLEAR tbtcs.
  SELECT * INTO TABLE htab FROM tbtcs
                          WHERE jobname   IN zjobname
                            AND btcsystem IN zserver
                            AND sdluname  IN zuser
                            AND jobclass  IN zclass
                            AND authckman IN zclient
                            AND periodic  IN periodic.

  LOOP AT htab.
    MOVE-CORRESPONDING htab TO dtab.
    CLEAR tbtcp.
    SELECT * FROM tbtcp WHERE jobname  = htab-jobname
                          AND jobcount = htab-jobcount.
      CHECK tbtcp-progname IN zprgname.
      dtab-stepcount = tbtcp-stepcount.
      dtab-progname  = tbtcp-progname .
      dtab-sdluname  = tbtcp-sdluname .
      dtab-variant   = tbtcp-variant  .
      dtab-authcknam = tbtcp-authcknam .
      APPEND dtab.
    ENDSELECT.
  ENDLOOP.
ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data.
  SORT dtab BY jobname stepcount.
ENDFORM.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  WRITE_DATA
*&---------------------------------------------------------------------*
FORM write_data.
  DATA: t_jobname   LIKE dtab-jobname,
        t_jobcount  LIKE dtab-jobcount,
        t_btcsystem LIKE dtab-btcsystem,
        t_sdlstrtdt LIKE dtab-sdlstrtdt,
        t_sdlstrttm LIKE dtab-sdlstrttm,
        t_sdluname  LIKE dtab-sdluname,
        t_jobclass  LIKE dtab-jobclass,
        t_prdmins   LIKE dtab-prdmins,
        t_prdhours  LIKE dtab-prdhours,
        t_prddays   LIKE dtab-prddays,
        t_prdweeks  LIKE dtab-prdweeks,
        t_prdmonths LIKE dtab-prdmonths,
        t_periodic  LIKE dtab-periodic.
  DATA: at_new(1).
  DATA: l_cnt TYPE i.

  LOOP AT dtab.
    t_jobname   = dtab-jobname  .
    t_btcsystem = dtab-btcsystem.
    t_sdlstrtdt = dtab-sdlstrtdt.
    t_sdlstrttm = dtab-sdlstrttm.
    t_sdluname  = dtab-sdluname .
    t_jobclass  = dtab-jobclass .
    t_prdmins   = dtab-prdmins  .
    t_prdhours  = dtab-prdhours .
    t_prddays   = dtab-prddays  .
    t_prdweeks  = dtab-prdweeks .
    t_prdmonths = dtab-prdmonths.
    t_periodic  = dtab-periodic .

* Change 包访 单捞磐
    CLEAR tbtco.
    SELECT SINGLE lastchdate lastchtime lastchname
                  INTO CORRESPONDING FIELDS OF tbtco
                  FROM tbtco
                 WHERE jobname  = dtab-jobname
                   AND jobcount = dtab-jobcount.
    IF NOT zcdate-low IS INITIAL.
      CHECK tbtco-lastchdate >= zcdate-low.
    ENDIF.

    AT NEW jobname.
      ADD 1 TO l_cnt.
      WRITE: /                                 sy-vline NO-GAP,
              (04) l_cnt       NO-SIGN NO-GAP, sy-vline NO-GAP,
                   t_jobname           NO-GAP, sy-vline NO-GAP,
              (10) t_btcsystem         NO-GAP, sy-vline NO-GAP,
                   t_sdlstrtdt         NO-GAP, sy-vline NO-GAP,
                   t_sdlstrttm         NO-GAP, sy-vline NO-GAP,
                   t_sdluname          NO-GAP, sy-vline NO-GAP,
                   tbtco-lastchdate    NO-GAP, sy-vline NO-GAP,
                   tbtco-lastchtime    NO-GAP, sy-vline NO-GAP,
                   tbtco-lastchname    NO-GAP, sy-vline NO-GAP,
                   t_jobclass          NO-GAP, sy-vline NO-GAP,
                   t_prdmins           NO-GAP, sy-vline NO-GAP,
                   t_prdhours          NO-GAP, sy-vline NO-GAP,
                   t_prddays           NO-GAP, sy-vline NO-GAP,
                   t_prdweeks          NO-GAP, sy-vline NO-GAP,
                   t_prdmonths         NO-GAP, sy-vline NO-GAP,
                   t_periodic          NO-GAP, sy-vline NO-GAP.
* 岿喊 角青 扒荐 拌魂阑 困茄 肺流 - Start
      DATA: temp_cnt TYPE i.
      IF t_periodic = 'X'.
        IF     t_prdmins <> '00' AND t_prdhours = '00' AND
               t_prddays = '000' AND t_prdweeks = '00' AND
               t_prdmonths = '00'.
          ADD 1 TO mm_cnt1.
          temp_cnt = ( 1440 / t_prdmins ) * 30.
          mm_cnt2 = mm_cnt2 + temp_cnt.
          CLEAR temp_cnt.
        ELSEIF t_prdmins = '00'  AND t_prdhours <> '00' AND
               t_prddays = '000' AND t_prdweeks = '00'  AND
               t_prdmonths = '00'.
          ADD 1 TO hh_cnt1.
          temp_cnt = ( 24 / t_prdhours ) * 30.
          hh_cnt2 = hh_cnt2 + temp_cnt.
          CLEAR temp_cnt.
        ELSEIF t_prdmins = '00'   AND t_prdhours = '00' AND
               t_prddays <> '000' AND t_prdweeks = '00' AND
               t_prdmonths = '00'.
          ADD 1 TO dd_cnt1.
          temp_cnt = 30.
          dd_cnt2 = dd_cnt2 + temp_cnt.
          CLEAR temp_cnt.
        ELSEIF t_prdmins = '00'  AND t_prdhours = '00'  AND
               t_prddays = '000' AND t_prdweeks <> '00' AND
               t_prdmonths = '00'.
          ADD 1 TO wk_cnt1.
          temp_cnt = 4.
          wk_cnt2 = wk_cnt2 + temp_cnt.
          CLEAR temp_cnt.
        ELSEIF t_prdmins = '00'  AND t_prdhours = '00' AND
               t_prddays = '000' AND t_prdweeks = '00' AND
               t_prdmonths <> '00'.
          ADD 1 TO mt_cnt1.
          temp_cnt = 1.
          mt_cnt2 = mt_cnt2 + temp_cnt.
        ENDIF.
      ENDIF.
* 岿喊 角青 扒荐 拌魂阑 困茄 肺流 - End
      at_new = 'X'.
    ENDAT.
    IF at_new <> 'X'.
      WRITE:                  sy-vline NO-GAP,
              (04) '' NO-GAP, sy-vline NO-GAP,
              (32) '' NO-GAP, sy-vline NO-GAP,
              (10) '' NO-GAP, sy-vline NO-GAP,
              (10) '' NO-GAP, sy-vline NO-GAP,
              (08) '' NO-GAP, sy-vline NO-GAP,
              (12) '' NO-GAP, sy-vline NO-GAP,
              (10) '' NO-GAP, sy-vline NO-GAP,
              (08) '' NO-GAP, sy-vline NO-GAP,
              (12) '' NO-GAP, sy-vline NO-GAP,
              (01) '' NO-GAP, sy-vline NO-GAP,
              (02) '' NO-GAP, sy-vline NO-GAP,
              (02) '' NO-GAP, sy-vline NO-GAP,
              (03) '' NO-GAP, sy-vline NO-GAP,
              (02) '' NO-GAP, sy-vline NO-GAP,
              (02) '' NO-GAP, sy-vline NO-GAP,
              (01) '' NO-GAP, sy-vline NO-GAP.
    ENDIF.
    WRITE: 137 dtab-stepcount NO-GAP, sy-vline NO-GAP,
          (30) dtab-progname  NO-GAP, sy-vline NO-GAP,
               dtab-variant   NO-GAP, sy-vline NO-GAP,
               dtab-authcknam NO-GAP, sy-vline NO-GAP.
    NEW-LINE.
    AT END OF jobname.
      WRITE: /(206) sy-uline.
    ENDAT.
    CLEAR at_new.

*    temp_tab-name = dtab-progname.
*    COLLECT temp_tab. CLEAR temp_tab.
  ENDLOOP.

*  LOOP AT temp_tab.
*    CHECK temp_tab-name+0(1) = 'Z'.
*    CLEAR trdirt.
*    SELECT SINGLE text INTO trdirt-text FROM trdirt
*                                       WHERE sprsl = 'E'
*                                         AND name = temp_tab-name.
*    temp_tab-text = trdirt-text.
*    WRITE: / temp_tab-name, '|', trdirt-text.
*  ENDLOOP.

* 岿喊 角青扒荐 Write
*  SKIP 1.
*  WRITE: /3 '泅犁 Release惑怕牢 巴父 焊咯林骨肺 炼雀矫痢俊 蝶扼 扒荐啊',
*            ' 促甫 荐 乐澜. 溜 Active 惑怕俊 乐绰 巴甸篮 力寇凳.'.
  SKIP 1.
  WRITE: /3 'Type', 10 'Job Number', 22 'Monthly Count'.
  WRITE: /3(30) sy-uline.
  WRITE: /3 'Minutes'  , 10 mm_cnt1, mm_cnt2.
  WRITE: /3 'Hours', 10 hh_cnt1, hh_cnt2.
  WRITE: /3 'Days'  , 10 dd_cnt1, dd_cnt2.
  WRITE: /3 'Weeks'  , 10 wk_cnt1, wk_cnt2.
  WRITE: /3 'Months'  , 10 mt_cnt1, mt_cnt2.
  DATA: sum_cnt1 TYPE i,
        sum_cnt2 TYPE i.
  sum_cnt1 = mm_cnt1 + hh_cnt1 + dd_cnt1 + wk_cnt1 + mt_cnt1.
  sum_cnt2 = mm_cnt2 + hh_cnt2 + dd_cnt2 + wk_cnt2 + mt_cnt2.
  WRITE: /3(30) sy-uline.
  WRITE: /3 'Total', 10 sum_cnt1, sum_cnt2.
ENDFORM.                    " WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  HEADER_DISPLAY
*&---------------------------------------------------------------------*
FORM header_display.
  WRITE: /117 'C : Job Class'                    .
  WRITE: /117 'MM: Duration period in minutes'   .
  WRITE: /117 'HH: Duration period in hours'     .
  WRITE: /117 'DD: Duration in days'             .
  WRITE: /117 'WK: Duration period in weeks'     .
  WRITE: /117 'MT: Duration period in months'    .
  WRITE: /117 'P : Periodic jobs indicator (X)'  .
  WRITE: /(206) sy-uline.
  WRITE: /                                       sy-vline NO-GAP,
              (04) 'No'         CENTERED NO-GAP, sy-vline NO-GAP,
              (32) 'Job Name'   CENTERED NO-GAP, sy-vline NO-GAP,
              (10) 'Server'     CENTERED NO-GAP, sy-vline NO-GAP,
              (10) 'Sch.Date'   CENTERED NO-GAP, sy-vline NO-GAP,
              (08) 'Sch.Time'   CENTERED NO-GAP, sy-vline NO-GAP,
              (12) 'Sch.User'   CENTERED NO-GAP, sy-vline NO-GAP,
              (10) 'Cha.Date'   CENTERED NO-GAP, sy-vline NO-GAP,
              (08) 'Cha.Time'   CENTERED NO-GAP, sy-vline NO-GAP,
              (12) 'Cha.User'   CENTERED NO-GAP, sy-vline NO-GAP,
              (01) 'C'          CENTERED NO-GAP, sy-vline NO-GAP,
              (02) 'MM'         CENTERED NO-GAP, sy-vline NO-GAP,
              (02) 'HH'         CENTERED NO-GAP, sy-vline NO-GAP,
              (03) 'DD'         CENTERED NO-GAP, sy-vline NO-GAP,
              (02) 'WK'         CENTERED NO-GAP, sy-vline NO-GAP,
              (02) 'MT'         CENTERED NO-GAP, sy-vline NO-GAP,
              (01) 'P'          CENTERED NO-GAP, sy-vline NO-GAP,
              (10) 'Step Count' CENTERED NO-GAP, sy-vline NO-GAP,
              (30) 'Program'    CENTERED NO-GAP, sy-vline NO-GAP,
              (14) 'Variant'    CENTERED NO-GAP, sy-vline NO-GAP,
              (12) 'Exec.User'  CENTERED NO-GAP, sy-vline NO-GAP.
  WRITE: /(206) sy-uline.
ENDFORM.                    " HEADER_DISPLAY

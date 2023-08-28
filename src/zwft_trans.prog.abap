*----------------------------------------------------------------------*
* Report            : ZCAG_TRANS
* Module/####Ŭ#### : #### / TEMP
* T_CODE            :
* Description       : ABAP Text Translation
*----------------------------------------------------------------------*
*  #######  | ##### | ####
*----------------------------------------------------------------------*
* 2013.11.08 | ###### | Initial Release
*----------------------------------------------------------------------*

REPORT  zwft_trans MESSAGE-ID zsc001.

INCLUDE zwft_trans_top.
*INCLUDE ZCAG_TRANS_TOP.
INCLUDE zwft_trans_cls.
*INCLUDE ZCAG_TRANS_CLS.
INCLUDE zwft_trans_m01.
*INCLUDE ZCAG_TRANS_M01.
INCLUDE zwft_trans_f01.
*INCLUDE ZCAG_TRANS_F01.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM at_sel_scr.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF g_error = 'X'. EXIT. ENDIF.
  gv_repid = sy-repid.

* ISO #### #ڵ### ##ȯ
  PERFORM lxe_t002_check_language.

*  IF PM_EXCEL = 'X'.
*    PERFORM UPLOAD_EXCEL.
*  ELSE.
  PERFORM select_objlist.
  PERFORM read_text.
*  ENDIF.

  PERFORM modi_gt_m.


  CALL SCREEN 0100.

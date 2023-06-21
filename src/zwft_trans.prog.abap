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

REPORT  ZWFT_TRANS MESSAGE-ID ZSC001.

INCLUDE ZWFT_TRANS_TOP.
*INCLUDE ZCAG_TRANS_TOP.
INCLUDE ZWFT_TRANS_CLS.
*INCLUDE ZCAG_TRANS_CLS.
INCLUDE ZWFT_TRANS_M01.
*INCLUDE ZCAG_TRANS_M01.
INCLUDE ZWFT_TRANS_F01.
*INCLUDE ZCAG_TRANS_F01.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM AT_SEL_SCR.


*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF G_ERROR = 'X'. EXIT. ENDIF.

* ISO #### #ڵ### ##ȯ
  PERFORM LXE_T002_CHECK_LANGUAGE.

*  IF PM_EXCEL = 'X'.
*    PERFORM UPLOAD_EXCEL.
*  ELSE.
    PERFORM SELECT_OBJLIST.
    PERFORM READ_TEXT.
*  ENDIF.

  PERFORM MODI_GT_M.


  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&  Include           ZLJW_TRANS_M01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100' WITH G_PGM_TITLE.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_0100 INPUT.

  CLEAR G_OKCD.
  G_OKCD = G_UCOM.
  CLEAR G_UCOM.

  CASE G_OKCD.

  WHEN 'BACK' OR 'EXIT' OR 'CANC'.
    PERFORM ALV_CLEAR_0100.
    LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " EXIT_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.

  CHECK GO_CONT IS INITIAL.

  CREATE OBJECT GO_CONT
  EXPORTING
    CONTAINER_NAME = 'GO_CONT'.

  CREATE OBJECT GO_GRID
  EXPORTING
    I_PARENT = GO_CONT.

* #̺#Ʈ
  CREATE OBJECT GO_EVT_GRID
  EXPORTING
    E_OBJECT_TEXT = 'GO_GRID'.

  SET HANDLER GO_EVT_GRID->HANDLE_TOOLBAR       FOR GO_GRID.
  SET HANDLER GO_EVT_GRID->HANDLE_USER_COMMAND  FOR GO_GRID.
  SET HANDLER GO_EVT_GRID->HANDLE_DOUBLE_CLICK  FOR GO_GRID.
  SET HANDLER GO_EVT_GRID->HANDLE_HOTSPOT_CLICK FOR GO_GRID.
  SET HANDLER GO_EVT_GRID->HANDLE_BUTTON_CLICK  FOR GO_GRID.
  SET HANDLER GO_EVT_GRID->HANDLE_DATA_CHANGED  FOR GO_GRID.

* #### ##ư ###
  PERFORM ALV_EX_TOOLBAR USING 'GT_EXCLUDE'.

* LAYOUT
  PERFORM ALV_LAYOUT_INIT USING '' '' CHANGING GS_LAYOUT.      "EDIT, COLOR

* FIELDCAT
  PERFORM ALV_FIELDCAT_MERGE TABLES GT_M GT_FCAT
  USING  'GT_M'.
  PERFORM ALV_FIELDCAT_0100  TABLES GT_FCAT.

** SORT
*  PERFORM ALV_SORT_0100 TABLES GT_SORT.

* Edit  #̺#Ʈ ####
  CALL METHOD GO_GRID->REGISTER_EDIT_EVENT
  EXPORTING
    I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.  "#ٷ# ###
  CALL METHOD GO_GRID->SET_READY_FOR_INPUT                  "####Ʈ## 1 #̹Ƿ#
  EXPORTING                               "###ص###
    I_READY_FOR_INPUT = 1.

* First Display
  CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
  EXPORTING
    IS_LAYOUT            = GS_LAYOUT
    IT_TOOLBAR_EXCLUDING = GT_EXCLUDE[]
    I_DEFAULT            = ' '            "###̾ƿ# ######## ########
    I_SAVE               = 'A'            "##ü#######.
    IS_VARIANT           = GS_VARIANT
  CHANGING
    IT_OUTTAB            = GT_M[]
    IT_SORT              = GT_SORT[]
    IT_FIELDCATALOG      = GT_FCAT[].

ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR G_OKCD.
  G_OKCD = G_UCOM.
  CLEAR G_UCOM.

  CASE G_OKCD.

  WHEN 'SAVE'.              "####
    PERFORM 0100_SAVE.

  WHEN 'XSEL'.              "####Ǯ #ȸ
    PERFORM 0100_XSEL.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.

  SET PF-STATUS '0110'.
  SET TITLEBAR  '0100' WITH '####Ǯ #ȸ'.

ENDMODULE.                 " STATUS_0110  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_0110  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT_0110 INPUT.

  CLEAR G_OKCD.
  G_OKCD = G_UCOM.
  CLEAR G_UCOM.

  CASE G_OKCD.
  WHEN 'BACK' OR 'EXIT' OR 'CANC'.
    PERFORM ALV_CLEAR_0110.
    LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.

  CLEAR G_OKCD.
  G_OKCD = G_UCOM.
  CLEAR G_UCOM.

  CASE G_OKCD.

  WHEN 'OKAY'.
    PERFORM 0110_OKAY.

  WHEN 'SAVE'.
    PERFORM 0110_SAVE.

  WHEN 'DBL_CLK_1_ICO_SEL'.
    PERFORM 0110_DBL_CLK_1_ICO_SEL.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0110  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0110 OUTPUT.

  CHECK GO_CONT1 IS INITIAL.

  CREATE OBJECT GO_CONT1
  EXPORTING
    CONTAINER_NAME = 'GO_CONT1'.

  CREATE OBJECT GO_GRID1
  EXPORTING
    I_PARENT = GO_CONT1.

* #̺#Ʈ
  CREATE OBJECT GO_EVT_GRID
  EXPORTING
    E_OBJECT_TEXT = 'GO_GRID1'.

  SET HANDLER GO_EVT_GRID->HANDLE_TOOLBAR       FOR GO_GRID1.
  SET HANDLER GO_EVT_GRID->HANDLE_USER_COMMAND  FOR GO_GRID1.
  SET HANDLER GO_EVT_GRID->HANDLE_DOUBLE_CLICK  FOR GO_GRID1.
  SET HANDLER GO_EVT_GRID->HANDLE_HOTSPOT_CLICK FOR GO_GRID1.
  SET HANDLER GO_EVT_GRID->HANDLE_BUTTON_CLICK  FOR GO_GRID1.
  SET HANDLER GO_EVT_GRID->HANDLE_DATA_CHANGED  FOR GO_GRID1.

* #### ##ư ###
  PERFORM ALV_EX_TOOLBAR USING 'GT_EXCLUDE'.

* LAYOUT
  PERFORM ALV_LAYOUT_INIT USING '' '' CHANGING GS_LAYOUT1.      "EDIT, COLOR

* FIELDCAT
  PERFORM ALV_FIELDCAT_MERGE TABLES GT_110 GT_FCAT1
  USING  'GT_110'.
  PERFORM ALV_FIELDCAT_0110  TABLES GT_FCAT1.

** SORT
*  PERFORM ALV_SORT_0100 TABLES GT_SORT.

* Edit  #̺#Ʈ ####
  CALL METHOD GO_GRID1->REGISTER_EDIT_EVENT
  EXPORTING
    I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.  "#ٷ# ###
  CALL METHOD GO_GRID1->SET_READY_FOR_INPUT                  "####Ʈ## 1 #̹Ƿ#
  EXPORTING                               "###ص###
    I_READY_FOR_INPUT = 1.

* First Display
  CALL METHOD GO_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
  EXPORTING
    IS_LAYOUT            = GS_LAYOUT1
    IT_TOOLBAR_EXCLUDING = GT_EXCLUDE[]
    I_DEFAULT            = ' '            "###̾ƿ# ######## ########
    I_SAVE               = 'A'            "##ü#######.
    IS_VARIANT           = GS_VARIANT
  CHANGING
    IT_OUTTAB            = GT_110[]
    IT_SORT              = GT_SORT1[]
    IT_FIELDCATALOG      = GT_FCAT1[].

ENDMODULE.                 " PBO_0110  OUTPUT

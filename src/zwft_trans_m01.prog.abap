*&---------------------------------------------------------------------*
*&  Include           ZLJW_TRANS_M01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100' WITH g_pgm_title.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
MODULE exit_0100 INPUT.

  CLEAR g_okcd.
  g_okcd = g_ucom.
  CLEAR g_ucom.

  CASE g_okcd.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM alv_clear_0100.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " EXIT_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*MODULE pbo_0100 OUTPUT.
*
*ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR g_okcd.
  g_okcd = g_ucom.
  CLEAR g_ucom.

  CASE g_okcd.

    WHEN 'SAVE'.              "####
      PERFORM 0100_save.

    WHEN 'XSEL'.              "####Ǯ #ȸ
      PERFORM 0100_xsel.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0110 OUTPUT.

  SET PF-STATUS '0110'.
  SET TITLEBAR  '0100' WITH '####Ǯ #ȸ'.

ENDMODULE.                 " STATUS_0110  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_0110  INPUT
*&---------------------------------------------------------------------*
MODULE exit_0110 INPUT.

  CLEAR g_okcd.
  g_okcd = g_ucom.
  CLEAR g_ucom.

  CASE g_okcd.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM alv_clear_0110.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0110 INPUT.

  CLEAR g_okcd.
  g_okcd = g_ucom.
  CLEAR g_ucom.

  CASE g_okcd.

    WHEN 'OKAY'.
      PERFORM 0110_okay.

    WHEN 'SAVE'.
      PERFORM 0110_save.

    WHEN 'DBL_CLK_1_ICO_SEL'.
      PERFORM 0110_dbl_clk_1_ico_sel.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0110  INPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0110  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0110 OUTPUT.

  CHECK go_cont1 IS INITIAL.

  CREATE OBJECT go_cont1
    EXPORTING
      container_name = 'GO_CONT1'.

  CREATE OBJECT go_grid1
    EXPORTING
      i_parent = go_cont1.

* #̺#Ʈ
  CREATE OBJECT go_evt_grid
    EXPORTING
      e_object_text = 'GO_GRID1'.

  SET HANDLER go_evt_grid->handle_toolbar       FOR go_grid1.
  SET HANDLER go_evt_grid->handle_user_command  FOR go_grid1.
  SET HANDLER go_evt_grid->handle_double_click  FOR go_grid1.
  SET HANDLER go_evt_grid->handle_hotspot_click FOR go_grid1.
  SET HANDLER go_evt_grid->handle_button_click  FOR go_grid1.
  SET HANDLER go_evt_grid->handle_data_changed  FOR go_grid1.

* #### ##ư ###
  PERFORM alv_ex_toolbar USING 'GT_EXCLUDE'.

* LAYOUT
  PERFORM alv_layout_init USING '' '' CHANGING gs_layout1.      "EDIT, COLOR

* FIELDCAT
  PERFORM alv_fieldcat_merge TABLES gt_110 gt_fcat1
  USING  'GT_110'.
  PERFORM alv_fieldcat_0110  TABLES gt_fcat1.

** SORT
*  PERFORM ALV_SORT_0100 TABLES GT_SORT.

* Edit  #̺#Ʈ ####
  CALL METHOD go_grid1->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.  "#ٷ# ###
  CALL METHOD go_grid1->set_ready_for_input                  "####Ʈ## 1 #̹Ƿ#
    EXPORTING                               "###ص###
      i_ready_for_input = 1.

* First Display
  CALL METHOD go_grid1->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layout1
      it_toolbar_excluding = gt_exclude[]
      i_default            = ' '            "###̾ƿ# ######## ########
      i_save               = 'A'            "##ü#######.
      is_variant           = gs_variant
    CHANGING
      it_outtab            = gt_110[]
      it_sort              = gt_sort1[]
      it_fieldcatalog      = gt_fcat1[].

ENDMODULE.                 " PBO_0110  OUTPUT
*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  CHECK go_cont IS INITIAL.

  CREATE OBJECT go_cont
    EXPORTING
      container_name = 'GO_CONT'.


  CREATE OBJECT go_grid
    EXPORTING
      i_parent = go_cont.

* #̺#Ʈ
  CREATE OBJECT go_evt_grid
    EXPORTING
      e_object_text = 'GO_GRID'.

  SET HANDLER go_evt_grid->handle_toolbar       FOR go_grid.
  SET HANDLER go_evt_grid->handle_user_command  FOR go_grid.
  SET HANDLER go_evt_grid->handle_double_click  FOR go_grid.
  SET HANDLER go_evt_grid->handle_hotspot_click FOR go_grid.
  SET HANDLER go_evt_grid->handle_button_click  FOR go_grid.
  SET HANDLER go_evt_grid->handle_data_changed  FOR go_grid.

* #### ##ư ###
  PERFORM alv_ex_toolbar USING 'GT_EXCLUDE'.

* LAYOUT
  PERFORM alv_layout_init USING '' '' CHANGING gs_layout.      "EDIT, COLOR

* FIELDCAT
  PERFORM alv_fieldcat_merge TABLES gt_m gt_fcat
  USING  'GT_M'.
  PERFORM alv_fieldcat_0100  TABLES gt_fcat.

** SORT
*  PERFORM ALV_SORT_0100 TABLES GT_SORT.

* Edit  #̺#Ʈ ####
  CALL METHOD go_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.  "#ٷ# ###
  CALL METHOD go_grid->set_ready_for_input                  "####Ʈ## 1 #̹Ƿ#
    EXPORTING                               "###ص###
      i_ready_for_input = 1.

* First Display
  CALL METHOD go_grid->set_table_for_first_display
    EXPORTING
      is_layout            = gs_layout
      it_toolbar_excluding = gt_exclude[]
      i_default            = ' '            "###̾ƿ# ######## ########
      i_save               = 'A'            "##ü#######.
      is_variant           = gs_variant
    CHANGING
      it_outtab            = gt_m[]
      it_sort              = gt_sort[]
      it_fieldcatalog      = gt_fcat[].

ENDMODULE.

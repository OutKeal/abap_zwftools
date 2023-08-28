*&---------------------------------------------------------------------*
*& Report ZEBC_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_report_edit.
TABLES:trdir.
DATA:
      g_editor TYPE REF TO cl_gui_abapedit.
DATA: g_code TYPE string OCCURS 0.
DATA:g_container TYPE REF TO cl_gui_docking_container.
DATA:g_error TYPE abap_bool.

PARAMETERS: p_report LIKE trdir-name.

START-OF-SELECTION.
  READ REPORT p_report INTO g_code.

  IF g_code[] IS INITIAL.
    MESSAGE '名称错误,无法修改' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CALL SCREEN 100.

FORM frm_init_abap_edit.
  IF g_container IS INITIAL.
    g_container = NEW cl_gui_docking_container( extension = '3000' ).
    g_editor = NEW cl_gui_abapedit( parent = g_container ).
    g_editor->set_toolbar_mode( 1 ).
    g_editor->set_statusbar_mode( 1 ).
    g_editor->set_readonly_mode( 0 ).
    g_editor->set_text( g_code ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  PERFORM frm_init_abap_edit.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      g_editor->free( ).
      g_container->free( ).
      FREE: g_editor,g_container.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CHECK'.
      PERFORM frm_abap_check .
    WHEN 'SAVE'.
      PERFORM frm_abap_check.
      CHECK g_error = abap_false.
      PERFORM frm_abap_save.
    WHEN 'FORMAT'.
      PERFORM frm_abap_format.
  ENDCASE.
ENDMODULE.

FORM frm_abap_check  .
  cl_gui_cfw=>flush( ).
  DATA: mess     TYPE string,
        lin      TYPE i ##needed,
        wrd      TYPE string ##needed,
        warnings TYPE  STANDARD TABLE OF rslinlmsg.
  DATA:report TYPE progdir-name.
  DATA:mainprograms   TYPE STANDARD TABLE OF syrepid.
  g_error = abap_false.

  g_editor->get_text( IMPORTING table = g_code ).
  CALL FUNCTION 'RS_GET_MAINPROGRAMS'
    EXPORTING
      dialog       = space
      fulltab      = 'X'
      name         = p_report
    TABLES
      mainprograms = mainprograms
    EXCEPTIONS
      cancelled    = 0.
  IF mainprograms IS INITIAL.
    report = p_report.
  ELSE.
    report = mainprograms[ 1 ].
  ENDIF.

  SYNTAX-CHECK FOR g_code
        MESSAGE mess
        LINE lin
        WORD wrd
        ID 'MSG' TABLE warnings
        PROGRAM report
        REPLACING p_report.

  IF sy-subrc NE 0.
    g_error = abap_true.

    g_editor->select_lines( from_line = lin to_line = lin ).

    MESSAGE mess TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE '代码检查通过' TYPE 'S' .
  ENDIF.

ENDFORM.

FORM frm_abap_save.
  INSERT REPORT p_report FROM g_code.
ENDFORM.

FORM frm_abap_format.
  g_editor->get_text( IMPORTING table = g_code ).
  DATA:
    l_pp       TYPE REF TO cl_sedi_pretty_printer,
    l_exc      TYPE REF TO cx_sedi_pretty_printer,
    l_settings TYPE REF TO if_pretty_printer_settings.
  DATA:buffer TYPE  rswsourcet.
  CREATE OBJECT l_pp.
  TRY.
      buffer = g_code.
      CREATE OBJECT l_settings TYPE cl_pretty_printer_wb_settings.
      l_pp->format_source(
      EXPORTING
        i_include  = zat_go=>abap_template
        i_settings = l_settings
      CHANGING
        c_source   = buffer ).
    CATCH cx_sedi_pretty_printer INTO l_exc.
      MESSAGE l_exc TYPE sy-msgty DISPLAY LIKE 'S'.
      RETURN.
  ENDTRY.

  g_code = buffer.
  g_editor->set_text(  g_code ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_REPORT_F4
*&---------------------------------------------------------------------*

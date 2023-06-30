FUNCTION-POOL zwft_falv.                        "MESSAGE-ID ..
CLASS lcl_output DEFINITION DEFERRED.

TYPES: t_output TYPE REF TO lcl_output.
TYPES: tt_output TYPE SORTED TABLE OF t_output WITH UNIQUE KEY table_line.
DATA: current_guid TYPE char22.
DATA: outputs TYPE tt_output.
DATA: okcode TYPE sy-ucomm.
DATA: call_stack TYPE STANDARD TABLE OF char22.
FIELD-SYMBOLS: <out> TYPE t_output.
INCLUDE lzwft_falvd01.                         " Local class definition

DATA: dummy_container TYPE REF TO cl_gui_container.
DATA: create_dummy_container TYPE abap_bool.

DATA: g_par01_type TYPE char255.
DATA: g_par02_type TYPE char255.
DATA: g_par03_type TYPE char255.
DATA: g_par04_type TYPE char255.
DATA: g_par05_type TYPE char255.
DATA: g_par06_type TYPE char255.
DATA: g_par07_type TYPE char255.
DATA: g_par08_type TYPE char255.
DATA: g_par09_type TYPE char255.
DATA: g_par10_type TYPE char255.
DATA: g_wait TYPE c.
DATA: g_fieldname TYPE string.
DATA: g_okcode TYPE sy-ucomm.
FIELD-SYMBOLS: <fcat> TYPE lvc_s_fcat.
FIELD-SYMBOLS: <any> TYPE any.

DEFINE add_param_descr.
  CONCATENATE 'P_DESC' &1 INTO g_fieldname.
  ASSIGN (g_fieldname) TO <any>.
  IF sy-subrc EQ 0.
    <any> = &2.
  ENDIF.
  CONCATENATE 'P_WITH' &1 INTO g_fieldname.
  ASSIGN (g_fieldname) TO <any>.
  IF sy-subrc EQ 0.
    <any> = TEXT-s02.
  ENDIF.
  CONCATENATE 'P_CLEA' &1 INTO g_fieldname.
  ASSIGN (g_fieldname) TO <any>.
  IF sy-subrc EQ 0.
    <any> = TEXT-s03.
  ENDIF.
END-OF-DEFINITION.

DEFINE change_parametr_type.
  DATA: f_type TYPE string.
  CONCATENATE 'G_PAR' &1 '_TYPE' INTO g_fieldname.
  CONCATENATE &2 '-' &3 INTO f_type.
  ASSIGN (g_fieldname) TO <any>.
  IF sy-subrc EQ 0.
    <any> = f_type.
  ENDIF.
END-OF-DEFINITION.


*--------------------------------------------------------------------*
* begin of selection screens
*--
DEFINE add_ss_line.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(14) p_clea&1.
  PARAMETERS: p_parc&1 AS CHECKBOX USER-COMMAND p_parc&1.
  SELECTION-SCREEN COMMENT 18(10) p_desc&1.
  PARAMETERS: p_parf&1 LIKE (g_par&1_type) LOWER CASE.
  SELECTION-SCREEN COMMENT 77(6) p_with&1.
  PARAMETERS: p_part&1 LIKE (g_par&1_type) LOWER CASE.

  "selection-screen comment 51(10) p_with&1.
  SELECTION-SCREEN END OF LINE.
END-OF-DEFINITION.



"mass replace ss
SELECTION-SCREEN BEGIN OF SCREEN 1001.
  add_ss_line: 01, 02, 03, 04, 05, 06, 07, 08, 09, 10.
SELECTION-SCREEN END OF SCREEN 1001.


AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr EQ 1001.
    lcl_output=>set_ss1001_status( ).
    SET TITLEBAR 'TITLE' WITH TEXT-s01.
    LOOP AT SCREEN.
      IF screen-name CP '*P_*01*' AND g_par01_type IS INITIAL.
        CLEAR: p_parf01, p_part01.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*02*' AND g_par02_type IS INITIAL.
        CLEAR: p_parf02, p_part02.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*03*' AND g_par03_type IS INITIAL.
        CLEAR: p_parf03, p_part03.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*04*' AND g_par04_type IS INITIAL.
        CLEAR: p_parf04, p_part04.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*05*' AND g_par05_type IS INITIAL.
        CLEAR: p_parf05, p_part05.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*06*' AND g_par06_type IS INITIAL.
        CLEAR: p_parf06, p_part06.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*07*' AND g_par07_type IS INITIAL.
        CLEAR: p_parf07, p_part07.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*08*' AND g_par08_type IS INITIAL.
        CLEAR: p_parf08, p_part08.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*09*' AND g_par09_type IS INITIAL.
        CLEAR: p_parf09, p_part09.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CP '*P_*10*' AND g_par10_type IS INITIAL.
        CLEAR: p_parf10, p_part10.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name+0(6) EQ 'P_PARF'.
        CONCATENATE 'P_PARC' screen-name+6(2) INTO g_fieldname.
        ASSIGN (g_fieldname) TO <any>.
        IF sy-subrc EQ 0 AND <any> IS NOT INITIAL.
          ASSIGN (screen-name) TO <any>.
          IF sy-subrc EQ 0.
            CLEAR <any>.
          ENDIF.
          screen-input = 0.
          MODIFY SCREEN.
        ELSEIF sy-subrc EQ 0.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF sy-dynnr EQ 1001.
    g_okcode = sy-ucomm.
    CLEAR sy-ucomm.
    CASE g_okcode.
      WHEN 'ENTER'.
        " return.
      WHEN 'CANCEL'.
        CLEAR: p_parf01, p_parf02, p_parf03, p_parf04, p_parf05, p_parf06, p_parf07, p_parf08, p_parf09, p_parf10,
        p_part01, p_part02, p_part03, p_part04, p_part05, p_part06, p_part07, p_part08, p_part09, p_part10        .
        "return.
      WHEN 'P_PARC01' OR 'P_PARC02' OR 'P_PARC03' OR 'P_PARC04' OR 'P_PARC05' OR 'P_PARC06' OR 'P_PARC07' OR 'P_PARC08' OR
      'P_PARC09' OR 'P_PARC10'.

    ENDCASE.
  ENDIF.

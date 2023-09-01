*&---------------------------------------------------------------------*
*& Report ZWFT_REPORT_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_report_download.


PARAMETERS: tcode   TYPE tcode OBLIGATORY,
            variant TYPE variant.
PARAMETERS:
  p_excel RADIOBUTTON GROUP g1 DEFAULT 'X',
  p_csv   RADIOBUTTON GROUP g1.

START-OF-SELECTION.

  SELECT SINGLE * INTO @DATA(l_tstc) FROM tstc WHERE tcode = @tcode.
  IF sy-subrc NE 0.
    MESSAGE '事务码不存在' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF variant IS NOT INITIAL .
    DATA:rc TYPE sy-subrc.
    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report              = l_tstc-pgmna
        variant             = variant
      IMPORTING
        r_c                 = rc
      EXCEPTIONS
        not_authorized      = 1
        no_report           = 2
        report_not_existent = 3
        report_not_supplied = 4
        OTHERS              = 5.
    IF sy-subrc <> 0 OR rc <> 0.
      MESSAGE '变式不存在' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ELSEIF NOT zwft_common=>confirm( '无变式,是否继续' ).
    RETURN.
  ENDIF.

  DATA lt_data TYPE REF TO data.
  DATA outtab TYPE REF TO data.
  DATA(metadata) = zwft_common=>get_report_alv_mate( EXPORTING tcode = tcode
    variant = variant
  IMPORTING data =  lt_data ).

  CASE 'X'.
    WHEN p_excel.
      zwft_common=>file_download_to_excel( lt_data->*  ).
    WHEN p_csv.
      zwft_common=>file_download_to_csv( lt_data->*  ).
  ENDCASE.

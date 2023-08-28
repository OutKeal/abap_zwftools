*&---------------------------------------------------------------------*
*& Report ZEBC_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZWFT_REPORT_CHANGE.
DATA: code(1072) TYPE c OCCURS 0.
PARAMETERS: p_report LIKE progdir-name.
READ REPORT p_report INTO code.
EDITOR-CALL FOR code.
INSERT REPORT p_report FROM code.

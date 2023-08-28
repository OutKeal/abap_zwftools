*&---------------------------------------------------------------------*
*& Report ZWFT_TEST01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zwft_test01.

SELECT * FROM mara INTO TABLE @DATA(lt_mara) up to 10 rows.

zwft_falv=>create( CHANGING ct_table = lt_mara )->display( ).

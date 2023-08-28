FUNCTION ZWFT_FALV_ADD_FALV_TO_STACK.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IO_FALV) TYPE REF TO  ZWFT_FALV
*"----------------------------------------------------------------------
  insert new lcl_output( io_falv ) into table outputs.


endfunction.

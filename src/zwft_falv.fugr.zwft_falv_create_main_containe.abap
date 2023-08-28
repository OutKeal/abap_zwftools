FUNCTION ZWFT_FALV_CREATE_MAIN_CONTAINE.
*"--------------------------------------------------------------------
*"*"局部接口：
*"  EXPORTING
*"     REFERENCE(MAIN_CONTAINER) TYPE REF TO CL_GUI_CONTAINER
*"--------------------------------------------------------------------
    create_dummy_container = abap_true.
    call screen zwft_falv=>c_screen_popup STARTING AT 1 1.
    main_container = dummy_container.
    create_dummy_container = abap_false.

ENDFUNCTION.

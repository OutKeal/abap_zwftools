class ZWFT_FALV_SPLITTER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF msob_list,
        object type REF TO zwft_falv,
      end of msob_list .
  types:
    mtob_list TYPE TABLE OF msob_list .

  data OB_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data OB_HEAD type ref to ZWFT_FALV .
  data OB_ITEM type ref to ZWFT_FALV .
  data OB_SPLITTER2 type ref to CL_GUI_SPLITTER_CONTAINER .
  data OB_HEAD2 type ref to ZWFT_FALV .
  data OB_ITEM2 type ref to ZWFT_FALV .
  data OB_LIST type MTOB_LIST .

  methods CONSTRUCTOR .
  class-methods CREATE2
    importing
      !I_PARENT type ref to CL_GUI_CONTAINER
      !I_STYLE type INT1 default 1
    changing
      !C_HEAD type ANY TABLE
      !C_ITEM type ANY TABLE
    returning
      value(OB_ME) type ref to ZWFT_FALV_SPLITTER .
  class-methods CREATE3
    importing
      !I_PARENT type ref to CL_GUI_CONTAINER
      !I_STYLE type I default 3
    changing
      !C_HEAD type ANY TABLE
      !C_ITEM type ANY TABLE
      !C_ITEM2 type ANY TABLE
    returning
      value(OB_ME) type ref to ZWFT_FALV_SPLITTER .
  class-methods CREATE4
    importing
      !I_PARENT type ref to CL_GUI_CONTAINER
    changing
      !C_HEAD type ANY TABLE
      !C_ITEM type ANY TABLE
      !C_HEAD2 type ANY TABLE
      !C_ITEM2 type ANY TABLE
    returning
      value(OB_ME) type ref to ZWFT_FALV_SPLITTER .
  methods MASS_DO
    importing
      !METHOD type METHOD_NAME .
protected section.
private section.
ENDCLASS.



CLASS ZWFT_FALV_SPLITTER IMPLEMENTATION.


  method CONSTRUCTOR.
    CLEAR OB_LIST.
  endmethod.


  METHOD create2.
    DATA row TYPE i.
    DATA col TYPE i.

    row = COND #( WHEN i_style = 1 THEN 2
                           WHEN i_style = 2 THEN 1 ).

    col = COND #( WHEN i_style = 1 THEN 1
                       WHEN i_style = 2 THEN 2 ).

    ob_me = NEW zwft_falv_splitter( ).
    ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = row columns = col ).

    ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
                                                  i_repid  = sy-cprog
                                                  i_suffix = '_HEAD'
                                        CHANGING  ct_table = c_head ).
    ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = row column = col )
                                                  i_repid  = sy-cprog
                                                  i_suffix = '_ITEM'
                                        CHANGING  ct_table = c_item ).
    APPEND VALUE #( object = ob_me->ob_head ) TO ob_me->ob_list.
    APPEND VALUE #( object = ob_me->ob_item ) TO ob_me->ob_list.

  ENDMETHOD.


  METHOD create4.
    FIELD-SYMBOLS:<fs_head> TYPE ANY TABLE,
                  <fs_item> TYPE ANY TABLE.

    ob_me = NEW zwft_falv_splitter( ).
    ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 2 columns = 2 ).

    ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
                                                  i_repid  = sy-cprog
                                                  i_suffix = 'HEAD'
                                        CHANGING  ct_table = c_head ).
    ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 2 column = 1 )
                                                  i_repid  = sy-cprog
                                                  i_suffix = 'ITEM'
                                        CHANGING  ct_table = c_item ).

    ob_me->ob_head2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 2 )
                                                                  i_repid  = sy-cprog
                                                                  i_suffix = 'HEAD2'
                                                                CHANGING  ct_table = c_head2 ).
    ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 2 column = 2 )
                                                                  i_repid  = sy-cprog
                                                                  i_suffix = 'ITEM2'
                                                                CHANGING  ct_table = c_item2 ).

    APPEND VALUE #( object = ob_me->ob_head ) TO ob_me->ob_list.
    APPEND VALUE #( object = ob_me->ob_item ) TO ob_me->ob_list.
    APPEND VALUE #( object = ob_me->ob_head2 ) TO ob_me->ob_list.
    APPEND VALUE #( object = ob_me->ob_item2 ) TO ob_me->ob_list.
  ENDMETHOD.


  METHOD create3.

    ob_me = NEW zwft_falv_splitter( ).
    CASE i_style.
      WHEN 1.
        ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 2 columns = 1 ).
        ob_me->ob_splitter2 = NEW cl_gui_splitter_container( parent = ob_me->ob_splitter->get_container( row = 2 column = 1 )
                                                                                              rows = 1 columns = 2 ).
        ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_HEAD'
        CHANGING  ct_table = c_head ).
        ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_ITEM'
        CHANGING  ct_table = c_item ).
        ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 1 column = 2 )
          i_repid = sy-cprog
          i_suffix = '_ITEM2'
        CHANGING  ct_table = c_item2 ).

      WHEN 2.
        ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 2 columns = 1 ).
        ob_me->ob_splitter2 = NEW cl_gui_splitter_container( parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
                                                                                              rows = 1 columns = 2 ).
        ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_HEAD'
        CHANGING  ct_table = c_head ).
        ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 1 column = 2 )
          i_repid  = sy-cprog
          i_suffix = '_ITEM'
        CHANGING  ct_table = c_item ).
        ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 2 column = 1 )
          i_repid = sy-cprog
          i_suffix = '_ITEM2'
        CHANGING  ct_table = c_item2 ).
      WHEN 3.
        ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 3 columns = 1 ).
        ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_HEAD'
        CHANGING  ct_table = c_head ).
        ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 2 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_ITEM'
        CHANGING  ct_table = c_item ).
        ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 3 column = 1 )
          i_repid = sy-cprog
          i_suffix = '_ITEM2'
        CHANGING  ct_table = c_item2 ).
      WHEN 4.
        ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 1 columns = 3 ).
        ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_HEAD'
        CHANGING  ct_table = c_head ).
        ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 2 )
          i_repid  = sy-cprog
          i_suffix = '_ITEM'
        CHANGING  ct_table = c_item ).
        ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 3 )
          i_repid = sy-cprog
          i_suffix = '_ITEM2'
        CHANGING  ct_table = c_item2 ).

      WHEN 5.
        ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 1 columns = 2 ).
        ob_me->ob_splitter2 = NEW cl_gui_splitter_container( parent = ob_me->ob_splitter->get_container( row = 1 column = 2 )
        rows = 2 columns = 1 ).
        ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_HEAD'
        CHANGING  ct_table = c_head ).
        ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_ITEM'
        CHANGING  ct_table = c_item ).
        ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 2 column = 1 )
          i_repid = sy-cprog
          i_suffix = '_ITEM2'
        CHANGING  ct_table = c_item2 ).

      WHEN 6.
        ob_me->ob_splitter = NEW cl_gui_splitter_container( parent = i_parent  rows = 1 columns = 2 ).
        ob_me->ob_splitter2 = NEW cl_gui_splitter_container( parent = ob_me->ob_splitter->get_container( row = 1 column = 1 )
        rows = 2 columns = 1 ).
        ob_me->ob_head = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 1 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_HEAD'
        CHANGING  ct_table = c_head ).
        ob_me->ob_item = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter2->get_container( row = 2 column = 1 )
          i_repid  = sy-cprog
          i_suffix = '_ITEM'
        CHANGING  ct_table = c_item ).
        ob_me->ob_item2 = zwft_falv=>create( EXPORTING i_parent = ob_me->ob_splitter->get_container( row = 1 column = 2 )
          i_repid = sy-cprog
          i_suffix = '_ITEM2'
        CHANGING  ct_table = c_item2 ).
    ENDCASE.

    APPEND VALUE #( object = ob_me->ob_head ) TO ob_me->ob_list.
    APPEND VALUE #( object = ob_me->ob_item ) TO ob_me->ob_list.
    APPEND VALUE #( object = ob_me->ob_item2 ) TO ob_me->ob_list.
  ENDMETHOD.


  METHOD mass_do.
    LOOP AT ob_list INTO DATA(l_list).
      CALL METHOD l_list-object->(method).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

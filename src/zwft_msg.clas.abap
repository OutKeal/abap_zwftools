class ZWFT_MSG definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods ADD_SINGLE
    importing
      value(MSGTY) type MSGTY
      value(MSGID) type MSGID
      value(MSGNO) type MSGNO
      value(MSGV1) type MSGV1 optional
      value(MSGV2) type MSGV2 optional
      value(MSGV3) type MSGV3 optional
      value(MSGV4) type MSGV4 optional .
  methods ADD_LINE
    importing
      value(IS_RETURN) type BAPIRET2 .
  methods ADD_TABLE
    importing
      value(IT_RETURN) type BAPIRET2_T .
  methods GET_RETURN
    returning
      value(ET_RETURN) type BAPIRET2_T .
  methods GET_ERROR
    returning
      value(ERROR) type ABAP_BOOL .
  methods POP_MSG
    importing
      value(REFRESH) type ABAP_BOOL optional .
  methods FLUSH .
protected section.
private section.

  data T_RETURN type BAPIRET2_T .
ENDCLASS.



CLASS ZWFT_MSG IMPLEMENTATION.


  METHOD add_line.
    APPEND is_return TO t_return.
  ENDMETHOD.


  METHOD ADD_SINGLE.
    APPEND VALUE #( type = msgty
                                  id = msgid
                                  number = msgno
                                  message_v1 = msgv1
                                  message_v2 = msgv2
                                  message_v3 = msgv3
                                  message_v4 = msgv4
                                ) TO t_return.
  ENDMETHOD.


  METHOD add_table.
    LOOP AT it_return INTO DATA(is_return).
      APPEND is_return TO me->t_return.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    CLEAR t_return.
  ENDMETHOD.


  METHOD FLUSH.
    CLEAR t_return.
  ENDMETHOD.


  METHOD get_error.
    error = abap_false.
    LOOP AT t_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
      error = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_return.
    et_return = t_return.
  ENDMETHOD.


  METHOD pop_msg.

    DATA:lt_message TYPE TABLE OF esp1_message_wa_type .
    DATA:it_message TYPE TABLE OF esp1_message_wa_type .

    CHECK t_return IS NOT INITIAL.

    LOOP AT t_return INTO DATA(is_return).
      APPEND VALUE #( msgty = is_return-type
      msgid = is_return-id
      msgno = is_return-number
      msgv1 = is_return-message_v1
      msgv2 = is_return-message_v2
      msgv3 = is_return-message_v3
      msgv4 = is_return-message_v4   ) TO lt_message.
    ENDLOOP.

    LOOP AT lt_message INTO DATA(is_message) WHERE msgty CA 'EAX'.
      APPEND is_message TO it_message.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = it_message[].
    ELSE.
      IF lines( lt_message ) = 1.
        DATA(message_line) = lt_message[ 1 ].
        MESSAGE ID message_line-msgid TYPE message_line-msgty
          NUMBER message_line-msgno
          WITH message_line-msgv1 message_line-msgv2 message_line-msgv3 message_line-msgv4.
      ELSE.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_message[].
      ENDIF.
    ENDIF.

    IF refresh IS NOT INITIAL.
      flush( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.

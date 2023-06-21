************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/12
*^ Include Name    : ZSQLEXPLORERO01
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To define parameters and varants
*
*^ Other           :
************************************************************************







******************************************************
*&      Form  Show_Message
******************************************************
FORM show_message USING p_message TYPE string .

  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = g_repid
      txt2  = space
      txt1  = p_message.

ENDFORM.                    "Show_Message




******************************************************
*&      Module  Initial_Screen  OUTPUT
******************************************************
MODULE initial_screen OUTPUT.
  DATA : l_orientation  TYPE  i  ,
         l_mode         TYPE  i  .

  CHECK g_editor IS INITIAL.

  g_repid = sy-repid.

*& Create Control Container

  PERFORM create_container_object USING g_container
                                        c_sql_editor.

  l_orientation  = 1 .
  PERFORM create_splitter_object  USING g_splitter1
                                        g_container
                                        l_orientation .

  PERFORM create_editor_object    USING g_editor1
                                        g_splitter1->bottom_right_container .

  l_orientation  = 0 .
  PERFORM create_splitter_object  USING g_splitter
                                        g_splitter1->top_left_container
                                        l_orientation.


*& Create Text Editor
  PERFORM create_editor_object USING g_editor
                                     g_splitter->top_left_container .

*& Create ALV
  PERFORM create_alv_object    USING g_grid
                                     g_splitter->bottom_right_container.


  IF sy-subrc NE 0.
    PERFORM show_message USING c_msg01 .
  ENDIF.

*& Set SQL Editor Support Highlight Comments
  PERFORM set_comment_mode USING g_editor .
  PERFORM set_comment_mode USING g_editor1.

*& Set Editor Read Only
  l_mode  =  1 .
  PERFORM set_readonly_mode USING g_editor1
                                  l_mode  .

ENDMODULE.                 " Initial_Screen  OUTPUT








******************************************************
*&      Module  status_0100  OUTPUT
******************************************************
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'SCREEN100'.
  SET TITLEBAR  'TITLE100'.

ENDMODULE.                 " status_0100  OUTPUT



*& End

*&---------------------------------------------------------------------*
*&  Include           ZSQLEXPLORERF03
*&---------------------------------------------------------------------*
************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/26
*^ Include Name    : ZSQLEXPLORERF03
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Define Text Editor
*
*^ Other           :
************************************************************************



DATA : g_postion TYPE i .


******************************************************
*&      Form  Create_Editor_Object
******************************************************
FORM create_editor_object USING p_editor    TYPE REF TO cl_gui_textedit
                                p_container TYPE REF TO cl_gui_container.

  CREATE OBJECT p_editor
    EXPORTING
       parent             = p_container
       wordwrap_mode      = cl_gui_textedit=>wordwrap_at_fixed_position
       wordwrap_position  = c_line_length
       wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
        OTHERS = 1.

ENDFORM .                    "Create_Editor_Object





******************************************************
*&      Form  Set_Comment_Mode
******************************************************
FORM set_comment_mode USING p_editor TYPE REF TO cl_gui_textedit .

  CALL METHOD p_editor->set_comments_string.
  CALL METHOD p_editor->set_highlight_comments_mode.

ENDFORM .                    "Set_Comment_Mode






******************************************************
*&      Form  Get_Selection_Index
******************************************************
FORM get_selection_index USING p_editor TYPE REF TO cl_gui_textedit
                               p_return  TYPE i                      .
  DATA : l_from  TYPE  i ,
         l_to    TYPE  i .

  CALL METHOD p_editor->get_selection_indexes
    IMPORTING
      from_index             = l_from
      to_index               = l_to
    EXCEPTIONS
      error_cntl_call_method = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_return = l_to - l_from .


ENDFORM .                    "Get_Selection_Index







******************************************************
*&      Form  Save_Text_To_Table
******************************************************
FORM save_text_to_table  USING p_editor TYPE REF TO cl_gui_textedit
                      CHANGING p_text   TYPE tt_text .
  DATA : l_flag  TYPE  i .

  PERFORM get_selection_index USING g_editor l_flag.

  IF l_flag = 0 .
    CALL METHOD p_editor->get_text_as_r3table
      IMPORTING
        table  = p_text
      EXCEPTIONS
        OTHERS = 1.
  ELSE .
    CALL METHOD p_editor->get_selected_text_as_r3table
      IMPORTING
        table  = p_text
      EXCEPTIONS
        OTHERS = 1.
  ENDIF .

  IF sy-subrc NE 0.
    PERFORM show_message USING c_msg03 .
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc NE 0.
    PERFORM show_message USING c_msg02 .
  ENDIF.


ENDFORM .                    "Save_TEXT_To_TABLE





******************************************************
*&      Form  Save_As_Local_File
******************************************************
FORM save_as_local_file USING p_editor TYPE REF TO cl_gui_textedit
                              p_file .

  CALL METHOD p_editor->save_as_local_file
    EXPORTING
      file_name              = p_file
    EXCEPTIONS
      error_cntl_call_method = 1
      OTHERS                 = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM .                    "Save_As_Local_File




******************************************************
*&      Form  set_text_as_r3table
******************************************************
FORM set_text_as_r3table  USING p_editor TYPE REF TO cl_gui_textedit
                       CHANGING p_text   TYPE tt_text.

  CALL METHOD p_editor->set_text_as_r3table
    EXPORTING
      table  = p_text
    EXCEPTIONS
      OTHERS = 1.


ENDFORM .                    "set_text_as_r3table





******************************************************
*&      Form  set_text_as_r3table
******************************************************
FORM set_selected_text_as_r3table  USING p_editor TYPE REF TO cl_gui_textedit
                                CHANGING p_text   TYPE tt_text.

  CALL METHOD p_editor->set_selected_text_as_r3table
    EXPORTING
      table  = p_text
    EXCEPTIONS
      OTHERS = 1.


ENDFORM .                    "set_text_as_r3table




******************************************************
*&      Form  Load_Text_From_Table
******************************************************
FORM load_text_from_table USING p_editor TYPE REF TO cl_gui_textedit
                       CHANGING p_text   TYPE tt_text.
  DATA : l_flag  TYPE  i .

  PERFORM get_selection_index USING g_editor l_flag.

  IF l_flag = 0 .
    PERFORM set_text_as_r3table  USING p_editor
                              CHANGING p_text   .
  ELSE .
    PERFORM set_selected_text_as_r3table  USING p_editor
                                       CHANGING p_text   .
  ENDIF .

  IF sy-subrc NE 0.
    PERFORM show_message USING c_msg04 .
  ENDIF.

ENDFORM.                    "Load_Text_From





******************************************************
*&      Form  Set_Status_Text
******************************************************
FORM set_status_text    USING p_editor TYPE REF TO cl_gui_textedit
                     CHANGING p_text   .

  CALL METHOD p_editor->set_status_text
    EXPORTING
      status_text            = p_text
    EXCEPTIONS
      error_cntl_call_method = 1
      OTHERS                 = 2.


ENDFORM.                    "Set_Status_Text





******************************************************
*&      Form  Set_Toolbar_Mode
******************************************************
FORM set_toolbar_mode  USING p_editor TYPE REF TO cl_gui_textedit
                             p_status TYPE  i .

  CALL METHOD p_editor->set_toolbar_mode
    EXPORTING
      toolbar_mode           = p_status
    EXCEPTIONS
      error_cntl_call_method = 1
      invalid_parameter      = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "Set_Toolbar_Mode





******************************************************
*&      Form  SET_READONLY_MODE
******************************************************
FORM set_readonly_mode  USING p_editor TYPE REF TO cl_gui_textedit
                              p_mode   TYPE  i      .

  CALL METHOD p_editor->set_readonly_mode
    EXPORTING
      readonly_mode          = p_mode
    EXCEPTIONS
      error_cntl_call_method = 1
      invalid_parameter      = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "set_readonly_mode



******************************************************
*&      Form  Destroy_Editor_Object
******************************************************
FORM destroy_editor_object CHANGING  p_editor TYPE REF TO cl_gui_textedit.

  destroy_control_object p_editor .

ENDFORM .                    "Destroy_Object





******************************************************
*&      Form  Create_Container_Object
******************************************************
FORM create_container_object USING p_container TYPE REF TO cl_gui_custom_container
                                    p_sql_editor.

  CREATE OBJECT p_container
      EXPORTING
          container_name = p_sql_editor
      EXCEPTIONS
          cntl_error                   = 1
          cntl_system_error            = 2
          create_error                 = 3
          lifetime_error               = 4
          lifetime_dynpro_dynpro_link  = 5.

ENDFORM .                    "Create_Container_Object





******************************************************
*&      Form  Destroy_Container_Object
******************************************************
FORM destroy_container_object CHANGING  p_container TYPE REF TO cl_gui_custom_container.

  destroy_control_object p_container .

ENDFORM .                    "Destroy_Object





******************************************************
*&      Form  Get_Splitter_Postion
******************************************************
FORM get_splitter_postion USING p_splitter  TYPE REF TO cl_gui_easy_splitter_container.

  CALL METHOD p_splitter->get_sash_position
    IMPORTING
      sash_position     = g_postion
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM .                    "get_splitter_postion






******************************************************
*&      Form  Set_Splitter_Postion
******************************************************
FORM set_splitter_postion USING p_splitter  TYPE REF TO cl_gui_easy_splitter_container
                                p_postion   TYPE i .

*& Set Splitter Postion
  CALL METHOD p_splitter->set_sash_position
    EXPORTING
      sash_position = p_postion.

ENDFORM .                    "Set_Splitter_Postion




******************************************************
*&      Form  Move_splitter_Postion
******************************************************
FORM move_splitter_postion  USING p_splitter  TYPE REF TO cl_gui_easy_splitter_container .

*  PERFORM get_splitter_postion USING p_splitter .

  CASE g_postion .
    WHEN 0 .
      g_postion = 100  .
    WHEN 50.
      g_postion = 0 .
    WHEN 100 .
      g_postion = 50   .

  ENDCASE .

  PERFORM set_splitter_postion USING p_splitter g_postion .


ENDFORM .                    "move_splitter_postion






******************************************************
*&      Form  Create_Splitter_Object
******************************************************
FORM create_splitter_object   USING p_splitter  TYPE REF TO cl_gui_easy_splitter_container
                                    p_container
                                    p_orientation  TYPE i .
  DATA : l_position  TYPE  i VALUE 100 .

  CREATE OBJECT p_splitter
       EXPORTING
          parent       =  p_container
          orientation  =  p_orientation .


  PERFORM set_splitter_postion USING p_splitter
                                     l_position .


ENDFORM .                    "Create_Splitter_Object






******************************************************
*&      Form  Destroy_Splitter_Object
******************************************************
FORM destroy_splitter_object CHANGING  p_splitter TYPE REF TO cl_gui_easy_splitter_container.

  destroy_control_object  p_splitter .

ENDFORM .                    "Destroy_Object



*& End

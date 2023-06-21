************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/26
*^ Include Name    : ZSQLEXPLORERF02
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Define AVL
*
*^ Other           :
************************************************************************






******************************************************
*&      Form  destroy_Control_object.
******************************************************
DEFINE destroy_control_object.
  check &1 is not initial .

  call method &1->free
    exceptions
      others = 1.

  free : &1 .

END-OF-DEFINITION.




******************************************************
*&      Form  Create_ALV_Object
******************************************************
FORM create_alv_object USING p_grid      TYPE REF TO cl_gui_alv_grid
                             p_container TYPE REF TO cl_gui_container.

  CREATE OBJECT p_grid
         EXPORTING i_parent = p_container.

ENDFORM .                    "Create_ALV_Object





******************************************************
*       Form Show_Data_In_ALV
******************************************************
FORM show_data_in_alv USING p_grid     TYPE REF TO cl_gui_alv_grid
                            p_data     TYPE ANY TABLE
                            p_field    TYPE ANY TABLE .

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      i_buffer_active               = 'X'
    CHANGING
      it_outtab                     = p_data
      it_fieldcatalog               = p_field
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDFORM.                    "Show_Data_In_ALV








******************************************************
*&      Form  Destroy_Grid_Object
******************************************************
FORM destroy_alv_object CHANGING  p_grid TYPE REF TO cl_gui_alv_grid.

  destroy_control_object p_grid .

ENDFORM .                    "Destroy_Object








******************************************************
*&      Form  prepare_alv_field_cat
******************************************************
FORM prepare_alv_field_cat    USING p_fields   TYPE tt_element
                           CHANGING p_fieldcat TYPE lvc_t_fcat .
  DATA : wa_field     TYPE  st_element ,
         wa_fieldcat  TYPE  LINE OF  lvc_t_fcat .

  CLEAR : p_fieldcat .

  LOOP AT p_fields INTO wa_field  .

*& Must Translate Field Name To Upper Case
    TRANSLATE wa_field-alias TO UPPER CASE .

    CASE g_label  .
      WHEN 1 .
        PERFORM change_field_name   USING  wa_field-alias
                                           wa_field-label
                                           ''
                                 CHANGING  p_fieldcat .
      WHEN 2 .
        PERFORM change_field_name   USING  wa_field-alias
                                           wa_field-alias
                                           ''
                                 CHANGING  p_fieldcat .
    ENDCASE .
  ENDLOOP .

ENDFORM .                    "destroy_alv_object






******************************************************
*&      Form  prepare_alv_error_field_Cat
******************************************************
FORM prepare_alv_error_field_cat CHANGING p_fieldcat TYPE lvc_t_fcat .

  CLEAR  p_fieldcat[] .

  PERFORM change_field_name   USING   'ICON' 'Status' 'X'
                           CHANGING   p_fieldcat .

  PERFORM change_field_name   USING   'MSG'  'Message' ''
                           CHANGING   p_fieldcat .


ENDFORM .                    "prepare_alv_error_field_Cat






******************************************************
*&      Form  CHANGE_FIELD_NAME
******************************************************

FORM change_field_name  USING p_field  p_text p_icon
                     CHANGING p_fieldcat TYPE lvc_t_fcat .
  DATA  wa_fieldcat TYPE LINE OF lvc_t_fcat .

  wa_fieldcat-fieldname    =   p_field.

  wa_fieldcat-scrtext_l    =   p_text.
  wa_fieldcat-scrtext_m    =   p_text.
  wa_fieldcat-scrtext_s    =   p_text.
  wa_fieldcat-icon         =   p_icon .

  IF p_field = 'MSG'.
    wa_fieldcat-outputlen  = 100 .
  ENDIF .

  APPEND wa_fieldcat TO p_fieldcat.

ENDFORM.                    " CHANGE_FIELD_NAME





******************************************************
*&      Form  Append_Error_Message
******************************************************
FORM append_error_message USING p_id   TYPE  i
                                p_msg  .
  DATA : wa_exception  TYPE  st_exception ,
         l_msg         TYPE  string       ,
         l_lines       TYPE  i            .

  CASE p_id .
    WHEN 1 .
      CONCATENATE 'Miss a '' at ' p_msg
             INTO l_msg
        SEPARATED BY space .
    WHEN 2 .
      CONCATENATE 'Miss a ( at ' p_msg
             INTO l_msg
        SEPARATED BY space .
    WHEN 3 .
      CONCATENATE 'After ''AS'' , need a alias at ' p_msg
             INTO l_msg
        SEPARATED BY space .
    WHEN 4 .
      l_msg = p_msg .
    WHEN 5 .
      CONCATENATE 'Table' p_msg 'doesn''t exist '
             INTO l_msg
        SEPARATED BY space .
    WHEN 6 .
      CONCATENATE 'Field' p_msg 'doesn''t exist '
             INTO l_msg
        SEPARATED BY space .
    WHEN 7 .
      l_msg = 'SQL with sub SQL can not be formated in this verison .' .

    WHEN 8 .
      l_msg = 'Please input Open SQL !'.

    WHEN 9 .
      CONCATENATE 'Encounter a ' p_msg 'after order by !'
             INTO l_msg
        SEPARATED BY space .

    WHEN 10 .
      l_msg = 'Can not find any field !'.

    WHEN 11 .
      l_msg = 'Can not find any table !'.
  ENDCASE .

  DESCRIBE TABLE g_exception LINES l_lines .

  wa_exception-id   =  l_lines  + 1 .
  wa_exception-icon = '@0A@'        .
  wa_exception-msg  =  l_msg        .

  APPEND wa_exception TO g_exception .


ENDFORM.                    "Append_Error_Message





******************************************************
*&      Form  Download_Table_To_Local
******************************************************
FORM download_table_to_local  TABLES p_table
                               USING p_file  TYPE rlgrap-filename .

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      filename                = p_file
      filetype                = 'ASC'
    TABLES
      data_tab                = p_table
    EXCEPTIONS
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
      invalid_type            = 4
      no_batch                = 5
      unknown_error           = 6
      invalid_table_width     = 7
      gui_refuse_filetransfer = 8
      customer_error          = 9
      no_authority            = 10
      OTHERS                  = 11.


ENDFORM .                    "Download_Table_To_Local




******************************************************
*&      Form  Get_FileName
******************************************************
FORM get_filename CHANGING p_file .


  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
*      mask             = ',*.txt.'
      mode             = 'S'
      title            = 'Save to local'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.                    "Get_FileName

*& End

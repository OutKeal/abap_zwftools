************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/12
*^ Include Name    : ZSQLEXPLORERI01
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Define Screen 100 PAI
*
*^ Other           :
************************************************************************









******************************************************
*&      Module  USER_COMMAND_0100  INPUT
******************************************************
MODULE user_command_0100 INPUT.
  DATA : l_start    TYPE  i ,
         l_end      TYPE  i ,
         it_temp    TYPE  tt_text ,
         l_number   TYPE  i .

*& Initial Serval Global Variant
  CLEAR : g_exception, l_number .

*& Keep Start Time
  GET RUN TIME FIELD l_start.


*& Processing User Command
  CASE g_ucomm.

    WHEN 'EXIT' OR 'BACK' OR 'BREAK'.
      PERFORM exit_program.

    WHEN 'EXEC'.
      PERFORM save_text_to_table       USING g_editor
                                    CHANGING it_text.
      PERFORM execute_sql              USING it_text
                                    CHANGING l_number.

    WHEN 'FORM'.
      PERFORM save_text_to_table       USING g_editor
                                    CHANGING it_text.

      PERFORM format_sql            CHANGING it_text.

    WHEN 'BUDG'.
      PERFORM save_text_to_table       USING g_editor
                                    CHANGING it_text.

      PERFORM bud_sturcture            USING it_text
                                    CHANGING it_temp .

      PERFORM set_text_as_r3table      USING g_editor1
                                    CHANGING it_temp   .

    WHEN 'SAVE'.
      IF g_file IS INITIAL .
        PERFORM get_filename CHANGING g_file .
      ENDIF .

      CHECK g_file IS NOT INITIAL AND sy-subrc = 0 .
      PERFORM save_as_local_file USING g_editor
                                       g_file  .


    WHEN 'MOVE'.
      PERFORM move_splitter_postion    USING g_splitter .

    WHEN 'CONF'.
      CALL SCREEN c_200 STARTING AT 30 5 .

  ENDCASE.

*& Set Runtime
  GET RUN TIME FIELD l_end.

  PERFORM set_runtime_and_lines USING l_start
                                      l_end
                                      l_number.



  CLEAR : g_ucomm .


ENDMODULE.                 " USER_COMMAND_0100  INPUT






******************************************************
*&      Form  EXIT_PROGRAM
******************************************************
FORM exit_program.


*& Free All Memory Located By Objects
  PERFORM destroy_alv_object       CHANGING g_grid      .
  PERFORM destroy_editor_object    CHANGING g_editor    .
  PERFORM destroy_editor_object    CHANGING g_editor1   .
  PERFORM destroy_splitter_object  CHANGING g_splitter  .
  PERFORM destroy_splitter_object  CHANGING g_splitter1 .
  PERFORM destroy_container_object CHANGING g_container .


*& Finally Flush
  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      OTHERS = 1.

  LEAVE PROGRAM.

ENDFORM.                               " EXIT_PROGRAM








******************************************************
*&      Form  Set_Runtime
******************************************************
FORM set_runtime_and_lines USING  p_start  TYPE  i
                                  p_end    TYPE  i
                                  p_number TYPE  i .
  DATA :   l_temp     TYPE string,
           l_status   TYPE c   LENGTH 100,
           l_spend    TYPE p LENGTH 12 DECIMALS 3 .

*& Set Runtime
  l_spend  = ( p_end  - p_start ) / 1000000 .
  l_temp = l_spend .

  CONCATENATE ' Runtime :' l_temp 'Seconds'
         INTO l_status
    SEPARATED BY space .

*& Set The Number Of The Total Records
  l_temp = l_number .

  CONCATENATE l_status ', Total Records :' l_temp
         INTO l_status
    SEPARATED BY space .


  PERFORM set_status_text          USING g_editor
                                         l_status .

ENDFORM .                    "Set_Runtime

*& End

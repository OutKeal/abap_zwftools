************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2005/12/12
*^ Include Name    : ZSQLEXPLORERI02
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Define Screen 200 PAI
*
*^ Other           :
************************************************************************


******************************************************
*&      Module  USER_COMMAND_0200  INPUT
******************************************************
MODULE USER_COMMAND_0200 INPUT.

  CASE G_UCOMM .

    WHEN 'CONF'.
      CASE 'X'.
        WHEN L_CASE_01 .
          G_CASE = 1 .
        WHEN L_CASE_02 .
          G_CASE = 2 .
        WHEN L_CASE_03 .
          G_CASE = 3 .
      ENDCASE .

      CASE 'X'.
        WHEN L_LABEL_01 .
          G_LABEL = 1 .
        WHEN L_LABEL_02 .
          G_LABEL = 2 .
      ENDCASE .

    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.

  ENDCASE .

  CLEAR : G_UCOMM .

ENDMODULE.                 " USER_COMMAND_0200  INPUT

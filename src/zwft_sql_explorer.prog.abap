************************************************************************
*^ Report                 ZSQLEXPLORER
*^ Written By           : Tom Yang
*^ Date Written         : 2006/12/26
*^ Program Purpose      : Provide The Service That User Key Open SQL
*^                        Into The SQL Editor And Execute it , To Get Data What
*^                        You Wants
*^ Run Frequency        : as needed
*^ Transaction Codes    : Z_SQL_EXPLORER
*^ Input File Names     :
*^ Output File Names    :
*^ Reports Generated    :
*^ Related Processes    :
*^ Others               :
************************************************************************
*^ Maintenance History (latest on top)
*
*^ Code Tag :              Date:               Author:
*^ Transport:
*^ Description of Change:
*
*^ Code Tag :              Date:               Author:
*^ Transport:
*^ Description of Change:
*
*
************************************************************************

REPORT   zwft_sql_explorer .

*INCLUDE zsqlexplorerf08.
INCLUDE ZWFT_SQL_EXPLORERF01.
*INCLUDE zsqlexplorerf01.
INCLUDE zsqlexplorertop.
INCLUDE ZWFT_SQL_EXPLORERF02.
*INCLUDE zsqlexplorerf02.
INCLUDE ZWFT_SQL_EXPLORERF03.
*INCLUDE zsqlexplorerf03.
INCLUDE ZWFT_SQL_EXPLORERF07.
*INCLUDE zsqlexplorerf07.
INCLUDE ZWFT_SQL_EXPLORERF04.
*INCLUDE zsqlexplorerf04.
INCLUDE ZWFT_SQL_EXPLORERF05.
*INCLUDE zsqlexplorerf05.
INCLUDE ZWFT_SQL_EXPLORERF06.
*INCLUDE zsqlexplorerf06.

INCLUDE ZWFT_SQL_EXPLORERO01.
*INCLUDE zsqlexplorero01.
INCLUDE ZWFT_SQL_EXPLORERO02.
*INCLUDE zsqlexplorero02.
INCLUDE ZWFT_SQL_EXPLORERI01.
*INCLUDE zsqlexploreri01.
INCLUDE ZWFT_SQL_EXPLORERI02.
*INCLUDE zsqlexploreri02.





START-OF-SELECTION.
  CALL SCREEN c_100.









*& End

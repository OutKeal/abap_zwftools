************************************************************************
*^ Written By      : Tom Yang
*^ Date Written    : 2006/12/12
*^ Include Name    : ZSQLEXPLORERF01
*^ Used in Programs: <Programs referencing this include>
*^ Purpose         : To Define Kinds Of Types
*
*^ Other           :
************************************************************************







CONSTANTS: c_line_length TYPE i VALUE 150.

*& Message
CONSTANTS :
  c_sql_editor  TYPE  c LENGTH 10  VALUE 'SQL_EDITOR' ,

  c_msg01 TYPE string VALUE
               'Error while instantiating ABAP proxy of TextEdit control',
  c_msg02 TYPE string VALUE
               'Error in flush',
  c_msg03 TYPE string VALUE
               'Error while retrieving text form TextEdit control',
  c_msg04 TYPE string VALUE
               'Error while sending text into TextEdit control',
  c_msg05 TYPE string VALUE
               'Error while destroying TextEdit control'.




CONSTANTS :
   c_sql_key_word_01  TYPE string
                      VALUE '-SELECT-FROM-INNER-JOIN-ON-LEFT-OUTER-',

   c_sql_key_word_02  TYPE string
                      VALUE '-CLIENT-SPECIFIED-INTO-APPENDING-AND-OR-IN-BETWEEN-LIKE-',

   c_sql_key_word_03  TYPE string
                      VALUE '-WHERE-ORDER-BY-GROUP-HAVING-ASCENDING-DESCENDING-',

   c_sql_key_word_04  TYPE string
                      VALUE '-AS-SINGLE-DISTINCT-SUM-COUNT-MIN-MAX-AVG-',

   c_select           TYPE string VALUE 'SELECT',
   c_sum              TYPE string VALUE 'SUM',
   c_count            TYPE string VALUE 'COUNT',
   c_count1           type string value 'COUNT(',
   c_min              TYPE string VALUE 'MIN',
   c_max              TYPE string VALUE 'MAX',
   c_avg              TYPE string VALUE 'AVG',
   c_left             TYPE string VALUE '(',
   c_right            TYPE string VALUE ')',
   c_all_fields       TYPE string VALUE '*',
   c_from             TYPE string VALUE 'FROM',
   c_on               TYPE string VALUE 'ON',
   c_inner            TYPE string VALUE 'INNER',
   c_join             TYPE string VALUE 'JOIN',
   c_out              TYPE string VALUE 'OUTER',
   c_left_j           TYPE string VALUE 'LEFT',
   c_as               TYPE string VALUE 'AS'  ,
   c_or               TYPE string VALUE 'OR',
   c_and              TYPE string VALUE 'AND',
   c_single           TYPE string VALUE 'SINGLE',
   c_distinct         TYPE string VALUE 'DISTINCT',
   c_where            TYPE string VALUE 'WHERE',
   c_order            TYPE string VALUE 'ORDER',
   c_group            TYPE string VALUE 'GROUP',
   c_have             TYPE string VALUE 'HAVING',
   c_hex              TYPE c LENGTH 5 VALUE '\X\09',
   c_funct            TYPE string VALUE 'FUNCT',
   c_separ            TYPE string VALUE '-',
   c_ss               TYPE string VALUE '~',
   c_quotes           TYPE string VALUE '''',

   c_line_fields      TYPE i      VALUE  3  ,
   c_fields_blank     TYPE i      VALUE  1  .


CONSTANTS :
      c_comment1  TYPE string VALUE '*',
      c_comment2  TYPE string VALUE '"'.




******************************************************
* To Definde Structure
******************************************************
TYPES: BEGIN OF st_text,
         line TYPE c LENGTH c_line_length ,
       END   OF st_text.

TYPES: tt_text TYPE STANDARD TABLE OF st_text .




TYPES: BEGIN OF st_element,
         alias    TYPE  c LENGTH 40    ,
         name     TYPE  c LENGTH 40    ,
         source   TYPE  ddobjname      ,
         link     TYPE  ddobjname      ,
         label    TYPE  string         ,
         index    TYPE  i              ,
         display  TYPE  string         ,
       END   OF st_element.


TYPES: BEGIN OF st_exception,
         id   TYPE  i,
         icon TYPE  icon_d,
         msg  TYPE  string,
       END   OF st_exception.

TYPES: tt_element TYPE STANDARD TABLE OF st_element .
TYPES: tt_exception  TYPE STANDARD TABLE OF st_exception .


TYPES: tt_code TYPE TABLE OF rssource-line .


*& end

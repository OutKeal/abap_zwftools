*&---------------------------------------------------------------------*
*& Report  ZUT_FIND_USER_EXITS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
 REPORT  zwft_userexit_find_xhl  MESSAGE-ID zut0 NO STANDARD PAGE HEADING.

************************************************************************
************************************************************************
*^ Written By           : Carolyn Fuller
*^ Date Written         : March 15, 2009
*^ Application Area     : Workbench
*^ Program Purpose      : To find as many user exits for a development
*                         class as possible. Using SAP Transaction,
*                         you can find possible SAP user exits available
*                         to the transaction. Using MIT Development
*                         Class you can find most user exits associated
*                         with that development class. Using "Find All
*                         MIT..." you can find the vast majority of
*                         the implemented user exits here at MIT. As
*                         time goes by, we'll reach closer to 100%.
*                         There is a hot spot link associated with the
*                         user exit name that will take you to the
*                         appropriate user exit transaction. In the
*                         case of BAdI definitions, you are asked if
*                         you wish to display the definition or
*                         implement it, in which case you taken to
*                         the appropriate implementation transaction
*                         and given instructions on how to do that.
*^ Run Frequency        : As needed
*^ Run Parameters       : Either Transaction Code or Development Class
*                         or the radio button for all MIT user exits.
*                         If you choose all MIT user exits, you have
*                         the option of choosing the old style exits
*                         that are embedded in SAP code. Since this
*                         search is based on naming convention patterns
*                         it is a very lengthy search. Therefore, the
*                         default behavior is not to include these
*                         objects and to warn you if you choose to
*                         include them, allowing you to change your
*                         mind.
*^ Transaction Codes    : ZUSEREXIT
*^ Major Logical Steps  : 1- Housekeeping (set up variables including
*                            dev class if transaction code is chosen)
*                         2- Find objects associated with dev class or
*                         3- Find MIT user exits - either all or all
*                            except SAP name space ones.
*                         4- Get the title of each of the objects found
*                         5- Write out each other & its title
*                         6- Write the total line
*                         7- Write the ending comments
*                         8- AT LINE-SELECTION -> determine which
*                            transaction code should be invoked
*                            and set parameter IDs or memory.
*                         9- AT LINE-SELECTION -> If classic BAdI
*                            definition is selected give the user the
*                            option to implement it instead of
*                            displaying it.
*^ Input File Names     : None
*^ Output File Names    : None
*^ Input Data Validation: Valid transaction or development class if
*                         running by TCode or Dev Class.
*^ Reports Generated    : Report listing the user exits associated with
*                         development class or MIT user exits.
*^ Related Processes    : None
*^ Limitations          : There are a few types of user exits this prog
*                         will not find. See bottom of report for a list
*                         of those user exits and how they can be found.
*                         Just because a user exit is in the same dev
*                         class as a TCode does not necessarily mean
*                         that user exit is available to that
*                         transaction. In the case of all MIT user exits
*                         we are finding all the special cases that we
*                         know about. As time goes by and we discover
*                         additional special cases, we'll incorporate
*                         into the program logic. These special cases
*                         are documented under Special Cases.
*                         I am going to attempt to do a better job with
*                         Transaction Code search
*^ MES                  : Not relavant
*^ Other Considerations : None
*  Special Cases        : 1- SAP program names that end in ZZ, FZA - FZY
*                            FZ0 - FZ9, US0 - US9, USA - USZ, ZXX or
*                            development class = VMOD and appear in a
*                            MIT transport. These tend to be Sales &
*                            Distribution user exits. These are
*                            documented in IMG under S&D -> System
*                            Modifications -> User Exits. Program names
*                            that end in Z00 - Z09, one of these is a
*                            payroll time management report.
*                         2- SAP program names that begin with RV and
*                            end in 999 and appear in a MIT transport.
*                            These tend to be Sales & Distributions
*                            copy / control configuration
*                         3- SAP program names that begin with PCBURZ
*                            or RPCBURZ and appear in a MIT transport.
*                            We have one of these (PCBURZUS0) that is
*                            labeled HR/PY: Include file with custom
*                            code for Time/Payroll.
*                         4- OPEN_FI business events and publish &
*                            subscribe implemented function modules
*                            can be found in tables, TBE34, TPS34 &
*                            TPS31 (where function name in customer
*                            name space.
*                         5- EHS user exits exceptions can be found in
*                            table TCGUEFU where functin name in
*                            customer name space.
**--------------------------------------------------------------------**
*^ Maintenance History (latest on top)
*
*^ Date: MM/DD/YY    Author: Fuller       Transport: SF2K946134
*^ Description of Change: Original program
*
************************************************************************
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
 TABLES :
*         tstc,    " SAP Transaction Codes
*         tadir,   " Directory of Repository Objects
*         modsapt, " SAP Enhancements - Short Texts
*         modact,  " Modifications
*         trdir,   " System table TRDIR
*         tfdir,   " Function Module
*         enlfdir, " Additional Attributes for Function Modules
*         tdevct,  " Development Class Texts
   rseumod. " Settings for ABAP Workbench

************************************************************************
***                      Types                                       ***
************************************************************************
 TYPES:
   BEGIN OF ty_keywords,
     word(30) TYPE c,
   END OF ty_keywords,

   BEGIN OF ty_sourcecode,
     line(200) TYPE c,
   END OF ty_sourcecode,

   BEGIN OF ty_includes,
     name TYPE trdir-name,
   END OF ty_includes,

   BEGIN OF ty_forms,
     name      TYPE trdir-name,
     form(300) TYPE c,
   END OF ty_forms,

   BEGIN OF ty_function,
     name TYPE tfdir-funcname,
   END OF ty_function,

   BEGIN OF ty_tadir,
     object      TYPE tadir-object,
     obj_name    TYPE tadir-obj_name,
     cproject    TYPE tadir-cproject,
     routine(83) TYPE c,
     number      TYPE i,
   END OF ty_tadir.

************************************************************************
***                       work fields                                ***
************************************************************************
 DATA : gt_tadir          TYPE TABLE OF          ty_tadir        WITH HEADER LINE,
        gt_includes       TYPE TABLE OF          ty_includes     WITH HEADER LINE,
        gt_submits        TYPE TABLE OF          ty_includes     WITH HEADER LINE,
        gt_performs       TYPE TABLE OF          ty_forms        WITH HEADER LINE,
        gt_functions      TYPE TABLE OF          ty_function     WITH HEADER LINE,
        gt_keywords       TYPE TABLE OF          ty_keywords     WITH HEADER LINE,
        gt_frames         TYPE TABLE OF          ty_keywords     WITH HEADER LINE, "#EC NEEDED
        gt_sourcecode     TYPE TABLE OF          ty_sourcecode   WITH HEADER LINE,
        gt_tokens         TYPE STANDARD TABLE OF stokex WITH HEADER LINE,
        gt_statements     TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE, "#EC NEEDED
        field1(30)        TYPE                   c,
        g_tcode_program   TYPE                   tstc-pgmna,
        g_msg(125)        TYPE                   c,
        g_ucomm(4)        TYPE                   c,
        g_title_object    TYPE                   tadir-devclass, " TCode, Dev Class or none
        g_obj_desc(83)    TYPE                   c,
        g_title_desc(60)  TYPE                   c,
        g_answer(1)       TYPE                   c,
        g_overflow(30000) TYPE                   c,
        g_number(3)       TYPE                   c.

************************************************************************
***                 Constants                                        ***
************************************************************************
 CONSTANTS:
   gc_false           TYPE boole_d VALUE space,
   gc_true            TYPE boole_d VALUE 'X',
   gc_pubs            TYPE tadir-object VALUE 'FUPS',
   gc_proc            TYPE tadir-object VALUE 'FUPR',
   gc_enhance_impl    TYPE tadir-object VALUE 'ENHO',
   gc_enhance_spot    TYPE tadir-object VALUE 'ENHS',
   gc_img_switch      TYPE tadir-object VALUE 'ENBC',
   gc_composite_imp   TYPE tadir-object VALUE 'ENHC',
   gc_composite_spot  TYPE tadir-object VALUE 'ENSC',
   gc_fgrp_cus_cus    TYPE tadir-object VALUE 'FUGX',
   gc_fgrp_cus_sap    TYPE tadir-object VALUE 'FUGS',
   gc_cust_proj       TYPE tadir-object VALUE 'CMOD',
   gc_sap_project     TYPE tadir-object VALUE 'SMOD',
   gc_menu_ehance     TYPE tadir-object VALUE 'CUAT',
   gc_badi_imp        TYPE tadir-object VALUE 'SE19',
   gc_img_badi        TYPE tadir-object VALUE 'SXCC',
   gc_badi_impl       TYPE tadir-object VALUE 'SXCI',
   gc_badi_def        TYPE tadir-object VALUE 'SXSD',
   gc_img_badi_def    TYPE tadir-object VALUE 'SXFT',
   gc_program         TYPE tadir-object VALUE 'PROG',
   gc_function_module TYPE tadir-object VALUE 'FUNC',
   gc_sample_ps(16)   TYPE c            VALUE 'SAMPLE_INTERFACE',
   gc_sample_proc(14) TYPE c            VALUE 'SAMPLE_PROCESS'.

************************************************************************
***                 ranges & range definition                        ***
************************************************************************
 DEFINE fill_ranges.
** &1=RANGE-NAME; &2=SIGN; &3=OPTION; &4=LOW; &5=HIGH
   &1-sign   = &2.
   &1-option = &3.
   &1-low    = &4.
   &1-high   = &5.
   APPEND &1.
 end-of-definition.

 RANGES: gr_objects FOR tadir-object,
 gr_s_cmods FOR tadir-object,
 gr_badis FOR tadir-object,
 gr_mit_objects FOR tadir-object,
 gr_names FOR tadir-obj_name.

*----------------------------------------------------------------------*
*  SELECTION SCREEN
*----------------------------------------------------------------------*
*Choose selection method
 SELECTION-SCREEN BEGIN OF BLOCK meth WITH FRAME TITLE TEXT-011.
   PARAMETERS: p_bytcde RADIOBUTTON GROUP grp1 DEFAULT 'X'
   USER-COMMAND xxxx.
   PARAMETERS: p_bydev RADIOBUTTON GROUP grp1.
   PARAMETERS: p_custom RADIOBUTTON GROUP grp1.
 SELECTION-SCREEN END OF BLOCK meth.

*Enter SAP Transaction Code
 SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE TEXT-001.
   SELECTION-SCREEN SKIP.
   PARAMETERS : p_tcode TYPE tstc-tcode.
   SELECTION-SCREEN SKIP.
 SELECTION-SCREEN END OF BLOCK a01.

*Enter Development Class
 SELECTION-SCREEN BEGIN OF BLOCK dev WITH FRAME TITLE TEXT-012.
   SELECTION-SCREEN SKIP.
   PARAMETERS : p_devc TYPE tadir-devclass.
   SELECTION-SCREEN SKIP.
 SELECTION-SCREEN END OF BLOCK dev.

*Choose objects
 SELECTION-SCREEN BEGIN OF BLOCK prj WITH FRAME TITLE TEXT-017.
   SELECTION-SCREEN SKIP.
   PARAMETERS: p_prj RADIOBUTTON GROUP grp2 DEFAULT 'X'. " report by CMODs
   PARAMETERS: p_inc RADIOBUTTON GROUP grp2. " report by ZX* INCLUDES
   PARAMETERS: p_sap AS CHECKBOX. " include SAP name space
   SELECTION-SCREEN SKIP.
 SELECTION-SCREEN END OF BLOCK prj.

************************************************************************
***               at selection-screen processing                     ***
************************************************************************
 AT SELECTION-SCREEN.
   PERFORM at_selection_screen.
*
************************************************************************
***              at selection-screen output processing               ***
************************************************************************
 AT SELECTION-SCREEN OUTPUT.
   PERFORM selection_screen_pbo.

 TOP-OF-PAGE.
   PERFORM display_top_of_page.

 TOP-OF-PAGE DURING LINE-SELECTION.
   PERFORM display_top_of_page.

************************************************************************
***                   start-of-selection                             ***
************************************************************************
 START-OF-SELECTION.

   PERFORM housekeeping.

   CASE 'X'.
     WHEN p_bytcde.
       PERFORM add_smods_in_devclass.
       PERFORM analyze_main_program.
       PERFORM analyze_includes.
       PERFORM analyze_submits.
       PERFORM analyze_function_modules.
       PERFORM analyze_performs.

     WHEN p_bydev.
       PERFORM get_dev_class_objects.

     WHEN p_custom.
       PERFORM get_customer_user_exits.

   ENDCASE.

   SORT gt_tadir BY object obj_name routine.
   DELETE ADJACENT DUPLICATES FROM gt_tadir COMPARING object obj_name routine.

   IF p_custom = gc_false.

     PERFORM find_mit_implementations.

     IF p_bydev = gc_true.
       PERFORM find_cmod_includes USING p_devc.

     ENDIF. " search by development class

   ENDIF. " not custom search

   SORT gt_tadir BY object obj_name routine.
   DELETE ADJACENT DUPLICATES FROM gt_tadir COMPARING object obj_name routine.

   LOOP AT gt_tadir.

     IF gt_tadir-object = gc_program AND gt_tadir-obj_name(2) <> 'ZX'
     AND p_custom = gc_false.
       IF p_tcode(1) = 'Z' OR p_devc(1) = 'Z'.
         DELETE gt_tadir.
         CONTINUE. " I'm only interested in user exit INCLUDES
       ENDIF.
     ENDIF.

     PERFORM get_title USING    gt_tadir-object
           gt_tadir-obj_name
           gt_tadir-routine
     CHANGING g_obj_desc.

     IF g_obj_desc IS INITIAL.
       CONTINUE.
     ENDIF. " does object exist?

     IF gt_tadir-number > 0.
       WRITE gt_tadir-number TO g_number.
       CONCATENATE g_obj_desc g_number 'routines' INTO g_obj_desc
       SEPARATED BY space.
     ENDIF.

     PERFORM write_line.

   ENDLOOP. " loop through objects


   PERFORM write_total_line.

   PERFORM write_comments.

 end-of-selection.

************************************************************************
***                     line-selection                               ***
************************************************************************
 AT LINE-SELECTION.
   GET CURSOR FIELD field1.
   CHECK field1(8) EQ 'GT_TADIR'.

   IF gt_tadir-object = gc_sap_project. " SMOD

     SET PARAMETER ID 'MON' FIELD gt_tadir-obj_name.
     CALL TRANSACTION 'SMOD'.

   ELSEIF gt_tadir-object = gc_cust_proj. " CMOD

     SET PARAMETER ID 'MON_KUN' FIELD gt_tadir-obj_name.
     CALL TRANSACTION 'CMOD'.

   ELSEIF gt_tadir-object = gc_badi_def. " SXSD (BAdI definition)

     SET PARAMETER ID 'EXN' FIELD gt_tadir-obj_name.

     CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
         titlebar      = 'To View or Implement'(025)
*       Do you wish to view this BAdI definition or implement it?
         text_question = TEXT-024
         text_button_1 = 'View'(026)
         text_button_2 = 'Impl'(027)
       IMPORTING
         answer        = g_answer. " allow fm to handle bad return code
     IF g_answer <> 'A'.
       IF g_answer = '1'.
         MESSAGE i008.  " Don't forget to switch to BAdI Name
         CALL TRANSACTION 'SE18'.
       ELSEIF g_answer = '2'.
*       Don't forget to switch to Classic BAdI in
*       "Create Implementation" section
         MESSAGE i007.
         CALL TRANSACTION 'SE19'.
       ENDIF.
     ENDIF. " If the user cancelled don't do anything

   ELSEIF gt_tadir-object = gc_badi_impl. " SXCI (BAdI implementation)

     SET PARAMETER ID 'IMN' FIELD gt_tadir-obj_name.
*   Don't forget to switch to Classic BAdI
     MESSAGE i015.
     CALL TRANSACTION 'SE19'.

   ELSEIF gt_tadir-object(3) = 'FUG'. " Function Group

*   SELECT * necessary because I must export all the columns to memory
     SELECT SINGLE * FROM rseumod " Settings for SE80
     WHERE uname = sy-uname.
     IF sy-subrc = 0.
       rseumod-seltype = 'F'.
       rseumod-fgroup = gt_tadir-obj_name.
       EXPORT rseumod TO MEMORY ID 'RSEUMOD'.
     ENDIF.
     CALL TRANSACTION 'SE80'.

   ELSEIF gt_tadir-object = gc_enhance_impl. " ENHO (Enhancement Implementation)

     SET PARAMETER ID 'IMN_BADI' FIELD gt_tadir-obj_name.   "#EC EXISTS
     CALL TRANSACTION 'SE19'.

   ELSEIF gt_tadir-object = gc_enhance_spot. " ENHS (Ehancement Spot)

     SET PARAMETER ID 'ENHSPOT' FIELD gt_tadir-obj_name.
     CALL TRANSACTION 'SE18'.

   ELSEIF gt_tadir-object = gc_composite_imp. " ENHC (Composite Implementation)

*   Copy name: & and go to composite implementation
     MESSAGE i013 WITH gt_tadir-obj_name.
     CALL TRANSACTION 'SE84'.

   ELSEIF gt_tadir-object = gc_composite_spot. " ENSC (Composite Enhancement Spot)

*   Copy name: & and go to composite enhance spot
     MESSAGE i014 WITH gt_tadir-obj_name.
     CALL TRANSACTION 'SE84'.

   ELSEIF gt_tadir-object = gc_program. " PROG (includes, etc)

     IF NOT ( gt_tadir-routine IS INITIAL ).  " Indicates user exit routines
*     Copy routine: & and search for it in program
       MESSAGE i017 WITH gt_tadir-routine.
     ENDIF.

     SET PARAMETER ID 'RID' FIELD gt_tadir-obj_name.
     CALL TRANSACTION 'SE38'.


   ELSEIF gt_tadir-object = gc_function_module. " FUNC (function module)

     SET PARAMETER ID 'LIB' FIELD gt_tadir-obj_name.
     CALL TRANSACTION 'SE37'.

* OPEN_FI objects
   ELSEIF gt_tadir-object = gc_pubs          " FUPS (publish & subscribe)
   OR gt_tadir-object = gc_proc.            " FUPR (process)

*   Don't forget to copy 8 digit event or process & to paste later
     MESSAGE i012 WITH gt_tadir-obj_name(8).
     CALL TRANSACTION 'FIBF'.

   ENDIF.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM display_top_of_page .
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE:/(20) 'Development Class - '(034),
   21(20) g_title_object,
   45(50) g_title_desc,
   125 space.
   SKIP.

* Legend & instructions
   FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF. " pink
   WRITE: /
   'Ehancements Implementation.'(035).
   WRITE 125 space.

   FORMAT COLOR COL_NEGATIVE INTENSIFIED ON.  " red
   WRITE: /
   'Enhancement Spot.'(036).
   WRITE 125 space.

   FORMAT COLOR COL_TOTAL INTENSIFIED OFF.     " light yellow
   WRITE: / 'Composite Enhancement (Implementation & Spot) SE84.'(037),
   TEXT-038.
   WRITE 125 space.

   FORMAT COLOR COL_GROUP INTENSIFIED OFF.     " light orange
   WRITE: / 'Function groups containing user exits.'(039).
   WRITE 125 space.
   FORMAT COLOR COL_GROUP INTENSIFIED ON.      " dark orange
   WRITE:/ 'SMOD projects or CMOD enhancements'(040).
   WRITE 125 space.
   FORMAT  COLOR COL_POSITIVE INTENSIFIED OFF.  " light green
   WRITE:/
   'Business Add-Ins - Implementations. Switch to Classic BAdI'(041).
   WRITE 125 space.
   FORMAT COLOR COL_POSITIVE INTENSIFIED ON.    " dark green
   WRITE: / 'Business Add-Ins - Definitions. Switch to BAdi Name'(042).
   WRITE 125 space.
   FORMAT COLOR COL_KEY INTENSIFIED OFF.        " lightest blue
   WRITE: / TEXT-043.
   WRITE 125 space.
   FORMAT COLOR COL_HEADING INTENSIFIED OFF.    " yet another blue!
   WRITE: / 'OPEN FI Publish & Subscribe and Process'(063).
   WRITE 125 space.
   FORMAT COLOR COL_KEY INTENSIFIED ON.         " dull blue
   WRITE: / 'User exit INCLUDES'(044).
   WRITE 125 space.
   SKIP.
   WRITE:/(125) sy-uline.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE:/1 sy-vline,
   2 'Exit Name'(045),
   41 sy-vline ,
   42 'Description'(046),
   125 sy-vline.
   WRITE:/(125) sy-uline.
 ENDFORM.                    " DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  at_selection_screen
*&---------------------------------------------------------------------*
*       Capture the user command
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM at_selection_screen.
   g_ucomm = sy-ucomm.
 ENDFORM.                    " at_selection_screen
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo
*&---------------------------------------------------------------------*
*       Processing to be done before selection screen is displayed
*       Depending upon how the end user wants to run this report
*       certain fields will be open or closed for input
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM selection_screen_pbo.

   IF g_ucomm = 'XXXX'.             " just changed output report choice
     CLEAR g_ucomm.
   ENDIF.

   LOOP AT SCREEN.
     IF screen-name = 'P_TCODE' AND p_bytcde NE 'X'. " not by tcode
       CLEAR p_tcode.                                " clear tcode
       screen-input = 0.                             " don't allow mod
       MODIFY SCREEN.
     ENDIF.
     IF screen-name = 'P_DEVC' AND p_bydev NE 'X'.   " not by dev class
       CLEAR p_devc.                                 " clear dev class
       screen-input = 0.                             " don't allow mod
       MODIFY SCREEN.
     ENDIF.
     IF screen-name = 'P_PRJ' AND p_custom NE 'X'.   " not by custom
       p_prj = 'X'.                                  " reset to default
       screen-input = 0.                             " don't allow mod
       MODIFY SCREEN.
     ENDIF.
     IF screen-name = 'P_INC' AND p_custom NE 'X'.   " not by custom
       CLEAR p_inc.                                  " reset to default
       screen-input = 0.                             " don't allow mod
       MODIFY SCREEN.
     ENDIF.
     IF screen-name = 'P_SAP' AND p_custom NE 'X'.   " not by custom
       CLEAR p_sap.                                  " clear include SAP
       screen-input = 0.                             " don't allow mod
       MODIFY SCREEN.
     ENDIF.
   ENDLOOP.

 ENDFORM.                    " selection_screen_pbo
*&---------------------------------------------------------------------*
*&      Form  HOUSEKEEPING
*&---------------------------------------------------------------------*
*       Validate input & fill range tables & if the user has chosen
*       a transaction confirm whether they want to find all the user
*       exits in the transaction's program or all the user exits in the
*       transaction's development class.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM housekeeping .
   DATA: l_include      TYPE c,
         l_param        TYPE tstcp-param,
         l_tcode        TYPE sy-tcode,
         l_devclass     TYPE tadir-devclass,
         l_message(120) TYPE c.

   CLEAR: g_title_object,
   g_tcode_program.

   CASE 'X'.
     WHEN p_bytcde.

       IF p_tcode IS INITIAL.
*       &1: Required input parameters must not be blank.
         MESSAGE i081(zz) WITH 'Transaction Code'(047).
         STOP.
       ENDIF.

*     Validate Transaction Code
       SELECT SINGLE pgmna INTO g_tcode_program
       FROM tstc
       WHERE tcode EQ p_tcode.
       IF sy-subrc <> 0.
*       &1 &2 does not exist
         MESSAGE i009 WITH 'Transaction Code'(047) p_tcode.
         STOP.
       ENDIF.

       g_title_object = p_tcode.

       IF g_tcode_program IS INITIAL.
         SELECT SINGLE param FROM tstcp INTO l_param
         WHERE tcode EQ p_tcode.
         IF sy-subrc = 0.
           CHECK l_param(1)   = '/'.
           CHECK l_param+1(1) = '*'.
           IF l_param CA ' '.                               "#EC NEEDED
           ENDIF.
           SUBTRACT 2 FROM sy-fdpos.
           IF sy-fdpos GT 0.
             l_tcode = l_param+2(sy-fdpos).
           ENDIF.
           SELECT SINGLE pgmna FROM tstc INTO g_tcode_program
           WHERE tcode EQ l_tcode. " check for return code below
         ENDIF. " does have a parameter
       ENDIF. " transaction code has no program

       IF g_tcode_program IS INITIAL.
*       No program found for: &
         MESSAGE i010 WITH p_tcode.
         STOP.
       ENDIF.

       SELECT SINGLE devclass INTO l_devclass
       FROM tadir
       WHERE pgmid = 'R3TR'
       AND object = gc_program
       AND obj_name = g_tcode_program.
       IF sy-subrc = 0 AND NOT l_devclass IS INITIAL.
*       Do you wish to find user exits called by &V1& or user exits
*       within development class &V2&?
         l_message = TEXT-077.
         REPLACE '&V1&' IN l_message WITH g_tcode_program.
         REPLACE '&V2&' IN l_message WITH l_devclass.
         CALL FUNCTION 'POPUP_TO_CONFIRM'
           EXPORTING
             titlebar      = 'By Program or By Dev Class'(079)
             text_question = l_message
             text_button_1 = 'Program'(080)
             text_button_2 = 'Dev Class'(081)
           IMPORTING
             answer        = g_answer. " allow fm to handle bad rc
         IF g_answer = 'A'.
           STOP.
         ELSEIF g_answer = '2'.
           CLEAR: p_bytcde,
           p_tcode.
           p_bydev = gc_true.
           p_devc = l_devclass.
           SELECT SINGLE obj_name INTO g_title_object
           FROM tadir
           WHERE pgmid = 'R3TR'
           AND object = 'DEVC'
           AND obj_name = p_devc.
           IF sy-subrc <> 0.
*           &1 &2 does not exist
             MESSAGE i009 WITH 'Development Class'(048) p_devc.
             STOP.
           ENDIF. " dev class exist?
         ENDIF. " by program or dev class?
       ENDIF. " there is a development class

       IF p_bydev = gc_false. " wants to analyze the program
*       the source code key words of interest
         gt_keywords-word = 'CALL'.
         APPEND gt_keywords.
         gt_keywords-word = 'FORM'.
         APPEND gt_keywords.
         gt_keywords-word = 'PERFORM'.
         APPEND gt_keywords.
         gt_keywords-word = 'SUBMIT'.
         APPEND gt_keywords.
         gt_keywords-word = 'INCLUDE'.
         APPEND gt_keywords.
         gt_keywords-word = 'ENHANCEMENT-POINT'.
         APPEND gt_keywords.
         gt_keywords-word = 'ENHANCEMENT-SECTION'.
         APPEND gt_keywords.
       ENDIF. " scanning the program associated with transaction

     WHEN p_bydev.
       IF p_devc IS INITIAL.
*       &1: Required input parameters must not be blank.
         MESSAGE i081(zz) WITH 'Development Class'(048).
         STOP.
       ENDIF.

       SELECT SINGLE obj_name INTO g_title_object
       FROM tadir
       WHERE pgmid = 'R3TR'
       AND object = 'DEVC'
       AND obj_name = p_devc.
       IF sy-subrc <> 0.
*       &1 &2 does not exist
         MESSAGE i009 WITH 'Development Class'(048) p_devc.
         STOP.
       ENDIF.

     WHEN p_custom.

       IF p_sap = gc_true.  " include SAP name space

         PERFORM check_include_sap
         CHANGING l_include.
         IF l_include <> 1.
           CLEAR p_sap. " do not inclue SAP name space
         ENDIF.

       ENDIF. " give them a chance to change their minds

       g_title_object = 'none'(082).
       IF p_sap = gc_true.  " include SAP name space
         fill_ranges gr_names 'I' 'CP' '*ZZ'   space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ0'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ1'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ2'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ3'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ4'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ5'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ6'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ7'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ8'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZ9'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZA'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZB'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZC'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZD'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZE'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZF'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZG'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZH'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZI'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZJ'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZK'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZL'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZM'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZN'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZO'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZP'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZQ'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZR'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZS'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZT'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZU'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZV'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZW'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZX'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*FZY'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US0'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US1'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US2'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US3'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US4'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US5'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US6'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US7'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US8'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*US9'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USA'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USB'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USC'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USD'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USE'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USF'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USG'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USH'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USI'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USJ'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USK'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USL'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USM'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USN'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USO'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USP'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USQ'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USS'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*UST'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USU'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USV'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USW'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USX'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USY'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*USZ'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*ZXX'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z00'  space.   " PY user exits
         fill_ranges gr_names 'I' 'CP' '*Z01'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z03'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z04'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z05'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z06'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z07'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z08'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*Z09'  space.   " SD user exits
         fill_ranges gr_names 'I' 'CP' '*999'  space.   " SD copy
         fill_ranges gr_names 'I' 'CP' 'PCBURZ*' space. " PY user exits
         fill_ranges gr_names 'I' 'CP' 'RPCBURZ*' space. " PY user exits
         fill_ranges gr_names 'E' 'CP' 'Z*'    space.   " not custom
         fill_ranges gr_names 'E' 'CP' 'Y*'    space.   " not custom
         fill_ranges gr_names 'E' 'CP' '/1BCWDY/*' space. " not auto generated
       ENDIF. " include SAP Name space

   ENDCASE.

   IF g_title_object IS INITIAL.
*   &1 &2 does not exist.
     MESSAGE i009 WITH 'Development Class'(048) p_devc.
     STOP.
   ENDIF.

   IF p_bytcde = gc_false.
*   gr_objects used to find user exit objects when searching via
*   development class. It is also used to populate gr_mit_objects
*   which is used to finding all MIT user exits
     fill_ranges gr_objects 'I' 'EQ' gc_sap_project    space. " SMOD
     fill_ranges gr_objects 'I' 'EQ' gc_enhance_impl   space. " ENHO
     fill_ranges gr_objects 'I' 'EQ' gc_enhance_spot   space. " ENHS
     fill_ranges gr_objects 'I' 'EQ' gc_img_switch     space. " ENBC
     fill_ranges gr_objects 'I' 'EQ' gc_composite_imp  space. " ENHC
     fill_ranges gr_objects 'I' 'EQ' gc_composite_spot space. " ENSC
     fill_ranges gr_objects 'I' 'EQ' gc_fgrp_cus_sap   space. " FUGS
     fill_ranges gr_objects 'I' 'EQ' gc_fgrp_cus_cus   space. " FUGX
     fill_ranges gr_objects 'I' 'EQ' gc_menu_ehance    space. " CUAT
     fill_ranges gr_objects 'I' 'EQ' gc_badi_imp       space. " SE19
     fill_ranges gr_objects 'I' 'EQ' gc_img_badi       space. " SXCC
     fill_ranges gr_objects 'I' 'EQ' gc_badi_impl      space. " SXCI
     fill_ranges gr_objects 'I' 'EQ' gc_badi_def       space. " SXSD
     fill_ranges gr_objects 'I' 'EQ' gc_img_badi_def   space. " SXFT

*   gr_mit_objects used to get all the user exits in Z name space
*   dev class except for CMODS which are sometimes in devclass %TMP!
     gr_mit_objects[] = gr_objects[].

     fill_ranges gr_objects 'I' 'EQ' gc_cust_proj      space. " CMOD

     IF g_title_object(1) = 'Z' OR p_custom = gc_true.
       fill_ranges gr_objects 'I' 'EQ' gc_program space. " to grab CMOD include
     ENDIF.

   ELSE.

*   gr_s_cmods & gr_badis used analyzing program code. This is
*   done when user chooses to search by transaction code program
     fill_ranges gr_s_cmods 'I' 'EQ' gc_sap_project space.
     fill_ranges gr_s_cmods 'I' 'EQ' gc_cust_proj space.

     fill_ranges gr_badis 'I' 'EQ' gc_badi_impl space.
     fill_ranges gr_badis 'I' 'EQ' gc_badi_def space.

   ENDIF. " Development Class/MIT Custom or Transaction Code

   CASE 'X'.
     WHEN p_bydev.
       SELECT SINGLE ctext INTO g_title_desc
       FROM tdevct
       WHERE spras EQ sy-langu
       AND devclass EQ p_devc. " don't care what sy-subrc is
     WHEN p_bytcde.
       SELECT SINGLE ttext INTO g_title_desc
       FROM tstct
       WHERE sprsl EQ sy-langu
       AND tcode = p_tcode. " don't care what sy-subrc is
     WHEN p_custom.
*     Dev Class not considered for MIT user exit search
       g_title_desc = TEXT-010.
   ENDCASE.

 ENDFORM.                    " HOUSEKEEPING
*&---------------------------------------------------------------------*
*&      Form  GET_TITLE
*&---------------------------------------------------------------------*
*       Find the object's description
*----------------------------------------------------------------------*
*      -->pv_object  Object
*      -->pv_name    Object's name
*      -->pv_routine FORM routine in embedded SAP user exit code
*      <--pv_desc    Description
*----------------------------------------------------------------------*
 FORM get_title  USING    pv_object TYPE tadir-object
       pv_name TYPE tadir-obj_name
       pv_routine TYPE c
 CHANGING pv_desc TYPE c.

   DATA: l_desc(80)  TYPE c,
         l_name      TYPE tadir-obj_name,
         l_not_found TYPE boole_d,
         l_subrc     TYPE sy-subrc.

   CLEAR: pv_desc,
   l_not_found,
   l_name,
   l_desc.

* SMOD
   IF pv_object = gc_sap_project.
     SELECT SINGLE modtext INTO pv_desc
     FROM modsapt
     WHERE sprsl = sy-langu
     AND name = pv_name.
     IF sy-subrc <> 0.
       SELECT COUNT(*)
       FROM modsap
       WHERE name = pv_name.
       IF sy-subrc = 0.
         pv_desc = 'SMOD Title not in MODSAPT'(072).
       ENDIF. " but SMOD was found
     ENDIF. " title text not found

* CMOD
   ELSEIF pv_object = gc_cust_proj.
     SELECT SINGLE modtext INTO pv_desc
     FROM modtext
     WHERE sprsl = sy-langu
     AND name = pv_name.
     IF sy-subrc <> 0.
       SELECT COUNT(*)
       FROM modact
       WHERE name = pv_name.
       IF sy-subrc = 0.
         pv_desc = 'CMOD Title not in MODTEXT'(073).
       ENDIF. " but CMOD was found
     ENDIF. " title text not found


* SXSD (Classic BAdI definition)
   ELSEIF pv_object = gc_badi_def.
     SELECT SINGLE text INTO pv_desc
     FROM sxs_attrt
     WHERE sprsl = sy-langu
     AND exit_name = pv_name.
     IF sy-subrc <> 0.
       SELECT COUNT(*)
       FROM sxs_attr
       WHERE exit_name = pv_name.
       IF sy-subrc = 0.
         pv_desc = 'Classic BAdI Title not found in SXS_ATTRT'(074).
       ENDIF. " but classic BAdI was found
     ENDIF. " title text not found

* SXCI (Class BAdI implementation
   ELSEIF pv_object = gc_badi_impl.
     SELECT SINGLE text INTO pv_desc
     FROM sxc_attrt
     WHERE sprsl = sy-langu
     AND imp_name = pv_name.
     IF sy-subrc <> 0.
       SELECT COUNT(*)
       FROM sxc_attr
       WHERE imp_name = pv_name.
       IF sy-subrc = 0.
         pv_desc = 'Classic BAdI Title not found in SXC_ATTRT'(075).
       ENDIF. " but classic BAdI was found
     ENDIF. " title text not found

* FUG (Function Groups - FUGS & FUGX user exits SAP & Customer)
   ELSEIF pv_object(3) = 'FUG'.
     SELECT SINGLE areat INTO pv_desc
     FROM tlibt
     WHERE spras = sy-langu
     AND area = pv_name.
     IF sy-subrc <> 0.
       SELECT COUNT(*)
       FROM tlibg
       WHERE area = pv_name.
       IF sy-subrc = 0.
         pv_desc = 'Function Group Title not found in TLIBT'(076).
       ENDIF. " but Function Group was found
     ENDIF. " title text not found

* FUNC
   ELSEIF pv_object = gc_function_module.
     SELECT SINGLE stext INTO pv_desc
     FROM tftit
     WHERE spras = sy-langu
     AND funcname = pv_name.
     IF sy-subrc <> 0.
       SELECT COUNT(*)
       FROM tfdir
       WHERE funcname = pv_name.
       IF sy-subrc = 0.
         IF pv_name CS gc_sample_ps.
           pv_desc =
           'Sample Function Module for P&S Event XXX'(060).
           REPLACE 'XXX' IN pv_desc WITH pv_name+17.
         ELSEIF pv_name CS gc_sample_proc.
           pv_desc = 'Sample Function Module for Process XXX'(061).
           REPLACE 'XXX' IN pv_desc WITH pv_name+17.
         ELSE.
           pv_desc = 'There is no English Text for Function Module'(062).
         ENDIF.
       ENDIF.
     ENDIF.

* PUBS (Open FI Publish & Subscribe)
   ELSEIF pv_object = gc_pubs.
     SELECT SINGLE text1 INTO pv_desc
     FROM tbe01t
     WHERE spras = sy-langu
     AND event = pv_name(8).
     IF sy-subrc <> 0.
       pv_desc = 'P&S description not found'(069).
     ENDIF.

* PROC (Open FI Process)
   ELSEIF pv_object = gc_proc.
     SELECT SINGLE text1 INTO pv_desc
     FROM tps01t
     WHERE spras = sy-langu
     AND procs = pv_name(8).
     IF sy-subrc <> 0.
       pv_desc = 'Process description not found'(070).
     ENDIF.

* PROG (INCLUDES)
   ELSEIF pv_object = gc_program.
     CLEAR l_subrc.
     IF pv_name(1) = 'Z'.   " custom
       SELECT SINGLE text INTO pv_desc
       FROM trdirt
       WHERE sprsl = sy-langu
       AND name = pv_name.
       l_subrc = sy-subrc.
     ELSEIF NOT pv_routine IS INITIAL.
       pv_desc = pv_routine.
     ELSE.
       pv_desc = 'Old Style Embedded User Exit Within SAP INCLUDE'(049).
     ENDIF.
     IF l_subrc = 0.
       IF pv_desc IS INITIAL.
         pv_desc = 'no title'.
       ENDIF.
     ELSE.
       CLEAR l_subrc.
       SELECT COUNT(*)
       FROM reposrc
       WHERE progname = pv_name
       AND r3state = 'A'.
       IF sy-subrc = 0.
         pv_desc = 'Report Title not in TRDIRT'(050).
       ENDIF. " Is it an active program?
     ENDIF. " object found in text table

* ENHO (Enhancement Implementations)
   ELSEIF pv_object = gc_enhance_impl.
     SELECT SINGLE sotr_text~text INTO pv_desc
     FROM enhheader
     JOIN sotr_text
     ON  sotr_text~concept = enhheader~shorttext_id
     WHERE enhheader~enhname = pv_name
     AND enhheader~version = 'A'
     AND sotr_text~object = space
     AND sotr_text~langu = sy-langu.                        "#EC *
     IF sy-subrc <> 0.
       SELECT SINGLE sotr_text~text INTO pv_desc
       FROM badiimpl_enh
       JOIN sotr_text
       ON sotr_text~concept = badiimpl_enh~shorttext_obj
       WHERE badiimpl_enh~enhname = pv_name
       AND sotr_text~langu = sy-langu
       AND sotr_text~object = space.                        "#EC *
     ENDIF.
     IF sy-subrc <> 0.
       SELECT SINGLE ddtext INTO pv_desc
       FROM objt
       WHERE language = sy-langu
       AND objectname = pv_object
       AND objecttype = 'L'. " don't care what sy-subrc is
     ENDIF.

* ENHS (Enhancement Spots)
   ELSEIF pv_object = gc_enhance_spot. " ENHS
     l_name = pv_name+3.
     TRANSLATE l_name TO UPPER CASE.                     "#EC TRANSLANG
     SELECT SINGLE text INTO l_desc
     FROM trdirt
     WHERE sprsl = sy-langu
     AND name = l_name.
     IF sy-subrc <> 0.
       CLEAR l_desc.
     ENDIF.

     SELECT SINGLE sotr_text~text INTO pv_desc
     FROM enhspotheader
     JOIN sotr_text
     ON  sotr_text~concept = enhspotheader~shorttextid
     WHERE enhspotheader~enhspot = pv_name
     AND enhspotheader~version = 'A'
     AND sotr_text~object = space
     AND sotr_text~langu = sy-langu.                        "#EC *
     IF sy-subrc <> 0.
       SELECT SINGLE ddtext INTO pv_desc
       FROM objt
       WHERE language = sy-langu
       AND objectname = pv_object
       AND objecttype = 'L'. " don't care what sy-subrc is
     ENDIF.
     IF NOT pv_desc IS INITIAL AND NOT l_desc IS INITIAL.
       CONCATENATE pv_desc '-' l_desc INTO pv_desc SEPARATED BY space.
     ENDIF.

* Anything else
   ELSE.
     SELECT SINGLE ddtext INTO pv_desc
     FROM objt
     WHERE language = sy-langu
     AND objectname = pv_object
     AND objecttype = 'L'.       " Logical transport object
   ENDIF. " get description (don't care what sy-subrc is)

   IF pv_desc IS INITIAL.
     l_not_found = gc_true.
     CONCATENATE '** This'(051) pv_object 'Object not found. **'(052)
     INTO  pv_desc SEPARATED BY space.
   ENDIF.

   IF g_title_object(1) = 'Z' OR p_custom = gc_true.

     IF l_not_found <> gc_true.
       IF gt_tadir-cproject CS 'L' AND gt_tadir-cproject NS 'S'.

         CONCATENATE '** TADIR identifies as LOCAL OBJECT **'(053) pv_desc
         INTO  pv_desc SEPARATED BY space.

       ENDIF. " local (not transported) object
     ENDIF. " object found?

   ENDIF. " MIT custom

 ENDFORM.                    " GET_TITLE
*&---------------------------------------------------------------------*
*&      Form  WRITE_LINE
*&---------------------------------------------------------------------*
*       Write out a report line
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM write_line .
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   IF gt_tadir-object = gc_sap_project OR gt_tadir-object = gc_cust_proj. " SMOD/CMOD
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_GROUP INTENSIFIED ON, " dark orange
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_badi_def. " SXSD
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_POSITIVE INTENSIFIED ON, " dark green
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_badi_impl. " SXCI
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_POSITIVE INTENSIFIED OFF, " light green
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object(3) = 'FUG'.
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_GROUP INTENSIFIED OFF, " light orange
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_enhance_impl. " ENHO
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_NEGATIVE INTENSIFIED OFF, " pink
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_enhance_spot. " ENHS
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_NEGATIVE INTENSIFIED ON, " red
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_program AND gt_tadir-obj_name(1) = 'Z'.
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_KEY INTENSIFIED ON, " dull blue
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_program AND gt_tadir-obj_name(1) <> 'Z'.
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_KEY INTENSIFIED ON, " dull blue
     41 sy-vline ,
     42 g_obj_desc COLOR COL_BACKGROUND INTENSIFIED ON, " lettering blue
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_function_module.
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_KEY INTENSIFIED OFF, " ligtest blue
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_pubs OR gt_tadir-object = gc_proc.
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_HEADING INTENSIFIED OFF, " another blue
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_composite_imp. " ENHC
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_TOTAL INTENSIFIED OFF,
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSEIF gt_tadir-object = gc_composite_spot. " ENSC
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_TOTAL INTENSIFIED OFF,
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ELSE.
     WRITE:/1 sy-vline,
     2 gt_tadir-obj_name HOTSPOT ON COLOR COL_TOTAL INTENSIFIED ON,
     41 sy-vline ,
     42 g_obj_desc,
     125 sy-vline.
   ENDIF.
   HIDE gt_tadir-object.
   HIDE gt_tadir-obj_name.
   HIDE gt_tadir-routine.
 ENDFORM.                    " WRITE_LINE
*&---------------------------------------------------------------------*
*&      Form  WRITE_TOTAL_LINE
*&---------------------------------------------------------------------*
*       Write the total line
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM write_total_line .
   WRITE:/(125) sy-uline.
   DESCRIBE TABLE gt_tadir.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE:/1 sy-vline,
   2 'No of Exits:'(054) , sy-tfill,
   41 sy-vline ,
   125 sy-vline.
   WRITE:/(125) sy-uline.
   SKIP.
 ENDFORM.                    " WRITE_TOTAL_LINE
*&---------------------------------------------------------------------*
*&      Form  WRITE_COMMENTS
*&---------------------------------------------------------------------*
*       Write the comments about which user exits aren't found
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM write_comments .
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.
   IF p_bytcde = gc_true.
*Keep in mind this does not include all the user exits available to this transaction, &.
*(1) It doesn't find user exits associated with subsequent transaction actions such as SAVE.
*(3) It doesn't find & the Business Transaction Events available under the OPEN_FI function modules.
*For those, use transaction FIBF and follow instructions on The SAP Consultant or SAP web sites:
*http://www.thesapconsultant.com/2006/10/sap-business-transaction-events.html
*http://help.sap.com/saphelp_47x200/helpdata/en/3b/7f3e8be57c11d1951f0000e82dec10/frameset.htm
*(4) It doesn't find the EHS user exits in table TCGUEFU, managed through IMG.
     g_msg = TEXT-002.
     REPLACE '&' IN g_msg WITH p_tcode.
     WRITE:/ g_msg.
     WRITE:/ TEXT-078.
     g_msg = TEXT-006.
     REPLACE '&' IN g_msg WITH 'all'(071).
     WRITE:/ g_msg.
     WRITE:/ TEXT-007.
     WRITE:/ TEXT-008.
     WRITE:/ TEXT-015.
     WRITE:/ TEXT-016.
   ELSEIF p_bydev = gc_true.
*Keep in mind this does not include all the user exits within development class &.
*(2) It doesn't find the old style embedded user exit subroutines within SAP INCLUDES such as RV60AFZZ.
*For those you must go to the IMG under the module of interest -> System Modifications-> User Exits or re-run for all MIT.
*(3) It doesn't find & the Business Transaction Events available under the OPEN_FI function modules.
     g_msg = TEXT-013.
     REPLACE '&' IN g_msg WITH p_devc.
     WRITE:/ g_msg.
     WRITE:/ TEXT-004.
     WRITE:/ TEXT-005.
     WRITE:/ TEXT-006.
     IF g_title_object(1) = 'Z'.
*For those see table TPS34 and TBE34. Also check table TPS31 for FUNCT = Z* or re-run for all MIT.
*(4) It doesn't find the EHS user exits in table TCGUEFU, managed through IMG.
       WRITE:/ TEXT-014.
       WRITE:/ TEXT-016.
     ELSE.
*For those, use transaction FIBF and follow instructions on The SAP Consultant or SAP web sites:
*http://www.thesapconsultant.com/2006/10/sap-business-transaction-events.html
*http://help.sap.com/saphelp_47x200/helpdata/en/3b/7f3e8be57c11d1951f0000e82dec10/frameset.htm
*(4) It doesn't find the EHS user exits in table TCGUEFU, managed through IMG.
       WRITE:/ TEXT-007.
       WRITE:/ TEXT-008.
       WRITE:/ TEXT-015.
       WRITE:/ TEXT-016.
     ENDIF.
   ELSEIF p_sap = gc_false. " doesn't include SAP name space user exits
*Keep in mind this does not include all the MIT user exits.
*(2) It doesn't find the old style embedded user exit subroutines within SAP INCLUDES such as RV60AFZZ.
*For those you must re-run for all MIT and include SAP Name Space Objects.
     WRITE:/ TEXT-018.
     WRITE:/ TEXT-004.
     WRITE:/ TEXT-019.
   ENDIF.
   SKIP.
*To find documentation on implementing Implicit Enhancement Spots:
*http://fuller.mit.edu/user_exits/enhancement_builder.html#implicit
*To find documentation on implementing Explicit Enhancement Spots:
*http://fuller.mit.edu/user_exits/enhancement_builder.html#explicit
*To find documentation on implementing Classic BAdIs
*http://fuller.mit.edu/user_exits/implementing_badi.html
   WRITE: / TEXT-028.
   WRITE: / TEXT-029.
   WRITE: / TEXT-030.
   WRITE: / TEXT-031.
   WRITE: / TEXT-032.
   WRITE: / TEXT-033.
 ENDFORM.                    " WRITE_COMMENTS
*&---------------------------------------------------------------------*
*&      Form  GET_DEV_CLASS_OBJECTS
*&---------------------------------------------------------------------*
*       Find the user exit objects within specific development class
*       The range table gr_objects is filled in housekeeping subroutine
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM get_dev_class_objects .

   SELECT object obj_name cproject FROM tadir
   INTO TABLE gt_tadir
   WHERE pgmid = 'R3TR'
   AND object IN gr_objects
   AND devclass = p_devc
   AND delflag = space.
* don't care what sy-subrc is

 ENDFORM.                    " GET_DEV_CLASS_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  get_customer_user_exits
*&---------------------------------------------------------------------*
*       Find all the MIT implemented user exits.
*       It first goes after all the user exit objects except for CMODS
*       that are in a dev class in the customer name space (Z). CMODS
*       are excluded because they are often in devclass %TMP.
*       If reporting by CMOD find all the active custom projects.
*       If the user has chosen to include the SAP Name Space programs,
*       this routine will find programming involved with SAP programs
*       that end in ZZ or begin with RV and end in 999 (copy control
*       user exits).
*       It will also find all of our implemented OPEN_FI function
*       modules.
*       If reporting by INCLUDE it goes after all programs that
*       begin with 'ZX'.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM get_customer_user_exits .

   DATA: BEGIN OF lt_tfdir OCCURS 0,
           funcname TYPE tfdir-funcname,
         END OF lt_tfdir.

   DATA: BEGIN OF lt_includes OCCURS 0,
           name TYPE tadir-obj_name,
         END OF lt_includes.

   DATA: BEGIN OF lt_events OCCURS 0,
           funct TYPE tbe34-funct,
         END OF lt_events.

   DATA: BEGIN OF lt_names OCCURS 0,
           name TYPE tadir-obj_name,
         END OF lt_names.

   RANGES: lr_names FOR tadir-obj_name.

* get all the user exits in Z name space dev class except for CMODS
* which are sometimes in devclass %TMP!
   SELECT object obj_name cproject FROM tadir
   INTO TABLE gt_tadir
   WHERE pgmid = 'R3TR'
   AND object IN gr_mit_objects
   AND devclass LIKE 'Z%'
   AND delflag = space. " don't care what sy-subrc is

   IF p_prj = gc_true.  " report by CMODs
*   get all active CMODS - By definition these are all custom projects
     SELECT name INTO TABLE lt_names
     FROM modattr
     WHERE status = 'A'.
     IF sy-subrc = 0.
       SELECT object obj_name cproject FROM tadir
       APPENDING TABLE gt_tadir
       FOR ALL ENTRIES IN lt_names
       WHERE obj_name = lt_names-name
       AND pgmid   = 'R3TR'
       AND object  = gc_cust_proj
       AND delflag = space. " don't care what sy-subrc is
     ENDIF. " found active CMODs
   ENDIF. " reporting by project

   REFRESH: lt_names,
   lt_includes.

   IF p_sap = gc_true. " include SAP name space
*   Now go after some SAP includess that are really user exits
*   First by Development Class
     SELECT trdir~name INTO TABLE lt_includes
     FROM trdir
     JOIN tadir
     ON trdir~name = tadir~obj_name
     WHERE tadir~devclass ='VMOD' " user exit dev class
     AND tadir~pgmid = 'R3TR'
     AND tadir~object = gc_program
     AND trdir~subc = 'I'       " include
     AND trdir~unam <> 'SAP'.   " not last updated by SAP
*   don't care what sy-subrc is
     CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
         text = 'Be patient - Selecting SAP Name Space INCLUDES'(057).

*   Second by name
*   This select takes a long, long time to execute but
*   I don't know of any other way to get to the information
     SELECT progname APPENDING TABLE lt_includes
     FROM reposrc
     WHERE progname IN gr_names   " known user exit name patterns
     AND r3state = 'A'          " active
     AND subc = 'I'             " include
     AND unam <> 'SAP'.         " not last updated by SAP (don't care sy-subrc)
   ENDIF. " If the user has opted to find SAP Objects

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Finishing up'(058).

   IF p_inc = gc_true. " report by ZX* INCLUDES
*   now get the user exit includes
     SELECT progname APPENDING TABLE lt_includes
     FROM reposrc
     WHERE progname LIKE 'ZX%'    " CMOD INCLUDEs
     AND r3state = 'A'          " active
     AND subc = 'I'             " include
     AND unam <> 'SAP'.         " not last updated by SAP (don't care sy-subrc)
   ENDIF. " report by INCLUDEs

* Let's get rid of those that have no code here
   LOOP AT lt_includes.
     PERFORM analyze_statements CHANGING lt_includes-name.
     IF lt_includes-name IS INITIAL.
       DELETE lt_includes.
     ENDIF.
   ENDLOOP.

   SORT lt_includes BY name.
   DELETE ADJACENT DUPLICATES FROM lt_includes.

   LOOP AT lt_includes.
     SELECT object obj_name cproject FROM tadir
     APPENDING TABLE gt_tadir
     WHERE obj_name = lt_includes-name
     AND pgmid = 'R3TR'
     AND object = gc_program.
     IF sy-subrc <> 0.
       gt_tadir-obj_name = lt_includes-name.
       gt_tadir-object = gc_program.
       APPEND gt_tadir.
     ENDIF.
   ENDLOOP.

* Publish&Subscribe BTE: Customer Enhancements
   SELECT funct INTO TABLE lt_events
   FROM tbe34. " don't care what sy-subrc is

* Process BTE: Customer Enhancements
   SELECT funct APPENDING TABLE lt_events
   FROM tps34. " don't care what sy-subrc is

* Process BTE: Alternative Function Modules from SAP
   SELECT funct APPENDING TABLE lt_events
   FROM tps31
   WHERE funct LIKE 'Z%'. " don't care what sy-subrc is

* EHS: User Exits in User Exit Management with FM Assignment
   SELECT fbnam APPENDING TABLE lt_events
   FROM tcguefu
   WHERE fbnam LIKE 'Z%'. " don't care what sy-subrc is

   REFRESH lr_names.
   CLEAR lr_names.

   LOOP AT lt_events.
     fill_ranges lr_names 'I' 'EQ' lt_events-funct   space.
   ENDLOOP.

* LR_NAMES holds FI_OPEN event & EHS function modules
   SELECT funcname INTO TABLE lt_tfdir
   FROM tfdir
   WHERE funcname IN lr_names. " don't care what sy-subrc is

   LOOP AT lt_tfdir.
     gt_tadir-object = gc_function_module.
     gt_tadir-obj_name = lt_tfdir-funcname.
     APPEND gt_tadir.
   ENDLOOP.

   SORT gt_tadir BY object obj_name.
   DELETE ADJACENT DUPLICATES FROM gt_tadir.

 ENDFORM.                    " get_customer_user_exits
*&---------------------------------------------------------------------*
*&      Form  ANALYZE_STATEMENTS
*&---------------------------------------------------------------------*
*       Analyze the statements in the INCLUDE to determine if there is
*       any significant code. If there is no significant coee, the name
*       will be cleared.
*----------------------------------------------------------------------*
*      <--pv_name  Name of Include (Cleared if no significant code.
*----------------------------------------------------------------------*
 FORM analyze_statements  CHANGING pv_name TYPE tadir-obj_name.
   DATA:
     lt_tokens     TYPE STANDARD TABLE OF stokex WITH HEADER LINE,
     lt_statements TYPE STANDARD TABLE OF sstmnt WITH HEADER LINE. "#EC NEEDED

   DATA: BEGIN OF lt_source OCCURS 0,
           text(255) TYPE c,
         END OF lt_source.

   READ REPORT pv_name INTO lt_source.  " don't care what sy-subrc is
   SCAN ABAP-SOURCE lt_source WITH INCLUDES
   TOKENS INTO lt_tokens
   FRAME PROGRAM FROM gt_frames
   STATEMENTS INTO lt_statements WITH ANALYSIS.
   IF sy-subrc <> 0 OR lt_tokens[] IS INITIAL.
     CLEAR pv_name.
     EXIT.
   ENDIF.

 ENDFORM.                    " ANALYZE_STATEMENTS
*---------------------------------------------------------------------*
*       FORM CHECK_INCLUDE_SAP                                        *
*---------------------------------------------------------------------*
*       Make sure the end user really wants to include SAP objects.
*       Warn them that the select statement looking for SAP user exit
*       name patterns takes a long time to execute
*---------------------------------------------------------------------*
*      <--pv_include Indicates whether the end user wishes to include
*                    SAP Name Space programs ("1") or not ("2")
*----------------------------------------------------------------------*
 FORM check_include_sap CHANGING pv_include TYPE c.

* Including SAP name space objects like MV45AFZZ takes a long time to
* search. Do you really want to include these objects?

   CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
       titlebar      = 'To Include or Not Include'(021)
       text_question = TEXT-020
       text_button_1 = 'Yes'(022)
       text_button_2 = 'No'(023)
     IMPORTING
       answer        = pv_include. " allow fm to handle bad return code
   IF pv_include = 'A'. " cancel
     STOP.
   ENDIF.

 ENDFORM.                    "CHECK_INCLUDE_SAP
*&---------------------------------------------------------------------*
*&      Form  analyze_main_program
*&---------------------------------------------------------------------*
*       Analyze the source code associated with the program behind a
*       transaction
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM analyze_main_program .
   DATA: l_later_pass      TYPE boole_d,
         l_calling_program TYPE trdir-name.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Analyzing Main Program'(056).

* import program into source code table
   READ REPORT g_tcode_program INTO gt_sourcecode.
   IF sy-subrc <> 0.
*   Screen & does not exist
     MESSAGE i017(enhancement) WITH g_tcode_program.
     STOP.
   ENDIF.

   SCAN ABAP-SOURCE gt_sourcecode TOKENS        INTO gt_tokens
   STATEMENTS    INTO gt_statements
   KEYWORDS      FROM gt_keywords
   FRAME PROGRAM FROM gt_frames
   OVERFLOW      INTO g_overflow
   WITH INCLUDES
   WITH ANALYSIS.
   IF sy-subrc <> 0.
*   Syntax error in screen. (No flow logic?)
     MESSAGE i130(enhancement).
     STOP.
   ENDIF.

   IF NOT gt_tokens[] IS INITIAL.
     CLEAR: l_later_pass.
     l_calling_program = g_tcode_program.
     PERFORM code_analysis TABLES gt_tokens
       gt_tadir
       gt_includes
     USING l_later_pass
           l_calling_program.
   ENDIF.

 ENDFORM.                    " analyze_main_program
*&---------------------------------------------------------------------*
*&      Form  CODE_ANALYSIS
*&---------------------------------------------------------------------*
*       Analyze the program tokens for user exits
*----------------------------------------------------------------------*
*      -->PT_TOKENS           Source code tokens
*      <--PT_TADIR            Directory of Responsitory Objects
*      <--PT_INCLUDES         Names of all the INCLUDES found
*      -->PV_IS_LATER_PASS    Is this pass a function module, etc pass
*      -->PV_CALLING_PROGRAM  Program from which tokens were found
*----------------------------------------------------------------------*
 FORM code_analysis  TABLES   pt_tokens          STRUCTURE stokex
   pt_tadir           STRUCTURE gt_tadir
   pt_includes        STRUCTURE gt_includes
 USING    pv_is_later_pass   TYPE boole_d
       pv_calling_program TYPE trdir-name.

   DATA: l_next      TYPE sy-tabix,
         l_plus3     TYPE sy-tabix,
         l_plus4     TYPE sy-tabix,
         l_funcname  TYPE tfdir-funcname,
         l_badi      TYPE sxs_attr-exit_name,
         l_master    TYPE d010inc-master,
         l_mod_name  TYPE modsapa-name,
         ls_token    TYPE stokex,
         l_pgm_name  TYPE trdir-name,
         l_form(300) TYPE c,
         l_junk(300) TYPE c.

*  FIELD-SYMBOLS:  LIKE gt_tadir. "谢:注释掉,因为语句不正确.

   LOOP AT pt_tokens.

     l_next = sy-tabix + 1.
     l_plus3 = sy-tabix + 3.
     l_plus4 = sy-tabix + 4.

     CLEAR pt_tadir.

********************************
*   First passes
********************************
     IF pv_is_later_pass = gc_false.

*     INCLUDES (will be analyzed later)
       IF pt_tokens-str EQ 'INCLUDE'.

         READ TABLE pt_tokens INDEX l_next INTO ls_token.
         IF sy-subrc <> 0.
           EXIT.
         ENDIF.
         IF NOT ls_token-str CS 'STRUCTURE'
         AND NOT ls_token-str CS 'SYMBOL'.

           IF ls_token-str(1) <> 'Z' OR p_tcode(1) = 'Z'.
             READ TABLE pt_includes WITH KEY name = ls_token-str.
             IF sy-subrc <> 0.
               pt_includes-name = ls_token-str.
               APPEND pt_includes.
             ENDIF. " not already in table
           ELSE.               " If INCLUDE is custom & TCode not custom
             SELECT SINGLE object obj_name cproject FROM tadir INTO pt_tadir
             WHERE pgmid = 'R3TR'
             AND object = gc_program
             AND obj_name = ls_token-str
             AND delflag = space.
             IF sy-subrc = 0.
               APPEND pt_tadir.
               CLEAR pt_tadir.
             ENDIF. " found INCLUDE in repository
           ENDIF. " custom INCLUDE in SAP program?

         ENDIF. " not a structure or symbol

         CONTINUE.
       ENDIF. " INCLUDE

*     Function modules (SE37)
       IF pt_tokens-str EQ 'FUNCTION'.
         CLEAR gt_functions.
         READ TABLE pt_tokens INDEX l_next INTO ls_token.
         IF sy-subrc <> 0.
           EXIT.
         ENDIF.
         REPLACE FIRST OCCURRENCE OF '''' IN ls_token-str WITH space.
         REPLACE FIRST OCCURRENCE OF '''' IN ls_token-str WITH space.
         IF sy-subrc = 4.   " didn't find 2nd quote (ie name truncated)
           CONCATENATE ls_token-str '%' INTO ls_token-str.
           SELECT SINGLE funcname INTO gt_functions-name
           FROM tfdir
           WHERE funcname LIKE ls_token-str.                "#EC *
           IF sy-subrc <> 0.
             CONTINUE.
           ENDIF.
         ELSE.
           gt_functions-name = ls_token-str.
         ENDIF.
         APPEND gt_functions.

         CONTINUE.
       ENDIF. " Function modules (SE37)

*     Submit programs (SE38)
       IF pt_tokens-str EQ 'SUBMIT'.
         CHECK NOT pt_tokens-str CS '_'.   " ensure not SUBMIT_XXX
         READ TABLE pt_tokens INDEX l_next INTO ls_token.
         IF sy-subrc <> 0.
           EXIT.
         ENDIF.
         CHECK NOT ls_token-str CS '_'.   " ensure not SUBMIT_XXX
         REPLACE ALL OCCURRENCES OF '''' IN ls_token-str WITH space.
         READ TABLE gt_submits WITH KEY name = ls_token-str.
         IF sy-subrc <> 0.
           gt_submits-name = ls_token-str.
           APPEND gt_submits.
         ENDIF.

         CONTINUE.
       ENDIF. " Submit programs (SE38)

*     Perform routines (which reference external programs)
       IF pt_tokens-str EQ 'PERFORM'.
         CLEAR: l_pgm_name,
         l_form.
         READ TABLE pt_tokens INDEX l_next INTO ls_token.
         IF sy-subrc <> 0.
           EXIT.
         ENDIF.
         IF NOT ls_token-ovfl IS INITIAL.
           SPLIT g_overflow AT '(' INTO l_form l_junk.
           SPLIT l_junk AT ')' INTO l_pgm_name l_junk.
         ELSE.
           SPLIT ls_token-str AT '(' INTO l_form l_junk.
           SPLIT l_junk AT ')' INTO l_pgm_name l_junk.
         ENDIF.
         IF NOT l_pgm_name IS INITIAL.
           gt_performs-name = l_pgm_name.
           gt_performs-form = l_form.
           APPEND gt_performs.
           CLEAR gt_performs.
         ENDIF.

         CONTINUE.
       ENDIF. " Perform routines (which reference external programs)

********************************
*   Final passes
********************************
     ELSEIF pt_tokens-str EQ 'INCLUDE'.

       IF ( p_bytcde = gc_true AND p_tcode(1) <> 'Z' )
       OR ( p_bydev = gc_true AND p_devc(1) <> 'Z' ).
         READ TABLE pt_tokens INDEX l_next INTO ls_token.
         IF sy-subrc <> 0.
           EXIT.
         ENDIF.
         IF NOT ls_token-str CS 'STRUCTURE'
         AND NOT ls_token-str CS 'SYMBOL'.
           IF ls_token-str(1) = 'Z'.
             SELECT SINGLE object obj_name cproject FROM tadir INTO pt_tadir
             WHERE pgmid = 'R3TR'
             AND object = gc_program
             AND obj_name = ls_token-str
             AND delflag = space.
             IF sy-subrc = 0.
               APPEND pt_tadir.
               CLEAR pt_tadir.
             ENDIF. " found INCLUDE in repository
           ENDIF. " custom INCLUDE in SAP program?
         ENDIF. " not a structure
       ENDIF. " SAP transaction code

       CONTINUE.
     ELSEIF pt_tokens-str EQ 'FUNCTION'.

       CONTINUE.
     ELSEIF pt_tokens-str EQ 'SUBMIT'.

       CONTINUE.
     ELSEIF pt_tokens-str EQ 'PERFORM'.

       CONTINUE.
     ENDIF. " which pass are we on

************************************
*   Token checking continued with
*   objects not stopped by specific
*   pass testing
************************************

*   Enhancement spots
     IF pt_tokens-str EQ 'ENHANCEMENT-POINT'
     OR pt_tokens-str EQ 'ENHANCEMENT-SECTION'.
       READ TABLE pt_tokens INDEX l_plus3 INTO ls_token.
       IF sy-subrc <> 0.
         EXIT.
       ENDIF.
*      READ TABLE pt_tadir ASSIGNING
*                          WITH KEY obj_name = ls_token-str."这两句被注释掉,改成下面一句.
       READ TABLE pt_tadir  WITH KEY obj_name = ls_token-str.
       IF sy-subrc = 0.
         ADD 1 TO pt_tadir-number.
       ELSE.
         SELECT SINGLE object obj_name cproject FROM tadir INTO pt_tadir
         WHERE pgmid = 'R3TR'
         AND object = gc_enhance_spot
         AND obj_name = ls_token-str
         AND delflag = space.
         IF sy-subrc = 0.
           pt_tadir-number = 1.
           APPEND pt_tadir.
           CLEAR pt_tadir.
         ENDIF. " found it in tadir
       ENDIF. " not already there?

       CONTINUE.
     ENDIF. " Enhancement coding option

*   SMOD (add the function module, project to TADIR table)
     IF pt_tokens-str = 'CUSTOMER-FUNCTION'.
       CLEAR l_funcname.
       READ TABLE pt_tokens INDEX l_next INTO ls_token.
       IF sy-subrc <> 0.
         EXIT.
       ENDIF.
       TRANSLATE ls_token-str USING ''' '. " get rid of quotes '004'
       CONDENSE ls_token-str.              " get rid of spaces
       CONCATENATE 'EXIT' pv_calling_program
       ls_token-str INTO l_funcname
       SEPARATED BY '_'.

*     1st add the function module to TADIR table
       SELECT COUNT(*)
       FROM tfdir
       WHERE funcname = l_funcname.
       IF sy-subrc = 0.
         pt_tadir-object = gc_function_module.
         pt_tadir-obj_name = l_funcname.
         APPEND pt_tadir.
         CLEAR pt_tadir.
       ELSE.
         CLEAR l_master.
         SELECT SINGLE master INTO l_master
         FROM d010inc
         WHERE include = pv_calling_program.                "#EC *
         IF sy-subrc = 0.
           CONCATENATE 'EXIT' l_master ls_token-str INTO l_funcname
           SEPARATED BY '_'.
           SELECT COUNT(*)
           FROM tfdir
           WHERE funcname = l_funcname.
           IF sy-subrc = 0.
             pt_tadir-object = gc_function_module.
             pt_tadir-obj_name = l_funcname.
             APPEND pt_tadir.
             CLEAR pt_tadir.
             gt_functions-name = l_funcname.
             APPEND gt_functions.
             CLEAR gt_functions.
           ENDIF. " found fm in repository
         ENDIF. " found the master program in where-used INCLUDE table
       ENDIF. " found user exit function module

*     2nd add the SAP project to TADIR table
       SELECT SINGLE name INTO l_mod_name
       FROM modsap
       WHERE typ = 'E'
       AND member = l_funcname.                             "#EC *
       IF sy-subrc = 0.
         READ TABLE pt_tadir WITH KEY object = gc_sap_project
         obj_name = l_mod_name.
         IF sy-subrc <> 0.                            " not already there
           SELECT SINGLE object obj_name cproject FROM tadir INTO pt_tadir
           WHERE pgmid = 'R3TR'
           AND object IN gr_s_cmods
           AND obj_name = l_mod_name
           AND delflag = space.                             "#EC *
           IF sy-subrc = 0.
             APPEND pt_tadir.
             CLEAR pt_tadir.
           ELSE.
             pt_tadir-object = gc_sap_project.
             pt_tadir-obj_name = l_mod_name.
             APPEND pt_tadir.
             CLEAR pt_tadir.
           ENDIF. " found SMOD in repository
         ENDIF. " SMOD not already in table
       ENDIF. " found SMOD with this function module as member

       CONTINUE.
     ENDIF. " SMOD user exit found

*   Classic BAdIs (SE18)
     IF pt_tokens-str CS 'cl_exithandler='.
       READ TABLE pt_tokens INDEX l_plus4 INTO ls_token.
       IF sy-subrc <> 0.
         EXIT.
       ENDIF.
       REPLACE ALL OCCURRENCES OF '''' IN ls_token-str WITH space.
       l_badi = ls_token-str.
       SELECT COUNT(*)
       FROM sxs_attr
       WHERE exit_name = l_badi
       AND internal <> gc_true.
       IF sy-subrc = 0.
         SELECT SINGLE object obj_name cproject FROM tadir INTO pt_tadir
         WHERE pgmid = 'R3TR'
         AND object IN gr_badis
         AND obj_name = l_badi
         AND delflag = space.                               "#EC *
         IF sy-subrc = 0.
           APPEND pt_tadir.
           CLEAR pt_tadir.
         ELSE.
           pt_tadir-object = gc_badi_def.
           pt_tadir-obj_name = l_badi.
           APPEND pt_tadir.
           CLEAR pt_tadir.
         ENDIF. " found BAdI in repository
       ENDIF. " found BAdI in table

       CONTINUE.
     ENDIF. " BAdI found

*   Business transaction events (FIBF)
*   add sample function module &
*   add event/process to TADIR &
*   add customer implemented event/process to TADIR
     IF pt_tokens-str CS 'OPEN_FI_PERFORM'.
       ls_token-str = pt_tokens-str.
*     e.g., OPEN_FI_PERFOM_00501014_E or OPEN_FI_PERFOM_00501022_P
       REPLACE ALL OCCURRENCES OF '''' IN ls_token-str WITH space.
       pt_tadir-obj_name = ls_token-str+16(8). " e.g., 00501014
*     last character is either a E (Publish & Subscribe) or P (process)
       CASE ls_token-str+25(1).

*       E = Publish & Subscribe
         WHEN 'E'.
*         1st add event to TADIR
           CONCATENATE pt_tadir-obj_name 'P&S'(065)
           INTO pt_tadir-obj_name SEPARATED BY '/'.
*         e.g., 00501014/P&S
           pt_tadir-object = gc_pubs.  " 'FUPS'
           APPEND pt_tadir.
           CLEAR pt_tadir.
*         2nd add sample function module to TADIR
           SELECT SINGLE interface INTO l_funcname
           FROM tbe01
           WHERE event = ls_token-str+16(8).
           IF sy-subrc = 0.
             SELECT COUNT(*)
             FROM tfdir
             WHERE funcname = l_funcname.
             IF sy-subrc = 0.
               pt_tadir-object = gc_function_module.
               pt_tadir-obj_name = l_funcname.
               APPEND pt_tadir.
               CLEAR pt_tadir.
             ENDIF. " sample function module in repository
           ENDIF. " found a sample function module
*         3rd add custom function module to TADIR
           SELECT SINGLE funct INTO l_funcname
           FROM tbe34
           WHERE event = ls_token-str+16(8).                "#EC *
           IF sy-subrc = 0.
             SELECT COUNT(*)
             FROM tfdir
             WHERE funcname = l_funcname.
             IF sy-subrc = 0.
               pt_tadir-object = gc_function_module.
               pt_tadir-obj_name = l_funcname.
               APPEND pt_tadir.
               CLEAR pt_tadir.
             ENDIF. " custom function module in repository
           ENDIF. " found a custom function module

*       P = Process
         WHEN 'P'.
*         1st add process to TADIR
           CONCATENATE pt_tadir-obj_name 'Process'(064)
           INTO pt_tadir-obj_name SEPARATED BY '/'.
*         e.g., 00501022/Process
           pt_tadir-object = gc_proc.  " 'FUPR'
           APPEND pt_tadir.
           CLEAR pt_tadir.
*         2nd add sample function module to TADIR
           SELECT SINGLE interface INTO l_funcname
           FROM tps01
           WHERE procs = ls_token-str+16(8).
           IF sy-subrc = 0.
             SELECT COUNT(*)
             FROM tfdir
             WHERE funcname = l_funcname.
             IF sy-subrc = 0.
               pt_tadir-object = gc_function_module.
               pt_tadir-obj_name = l_funcname.
               APPEND pt_tadir.
               CLEAR pt_tadir.
             ENDIF. " sample function module in repository
           ENDIF. " found a sample function module
*         3rd add custom function module to TADIR
           SELECT SINGLE funct INTO l_funcname
           FROM tps34
           WHERE procs = ls_token-str+16(8).                "#EC *
           IF sy-subrc = 0.
             SELECT COUNT(*)
             FROM tfdir
             WHERE funcname = l_funcname.
             IF sy-subrc = 0.
               pt_tadir-object = gc_function_module.
               pt_tadir-obj_name = l_funcname.
               APPEND pt_tadir.
               CLEAR pt_tadir.
             ENDIF. " custom function module in repository
           ENDIF. " found a custom function module
       ENDCASE.

       CONTINUE.
     ENDIF. " business transaction events found

*   USEREXIT embedded in SAP code
     IF pt_tokens-str CS 'USEREXIT_'.
       CHECK NOT pt_tokens-str CS '-'.   " ensure not USEREXIT_XX-XXX
       CHECK NOT pt_tokens-str CS '('.   " ensure not SUBMIT_XX(X)
       ls_token-str = pt_tokens-str.
*     e.g., USEREXIT_MOVE_FIELD_TO_KOMKD
       REPLACE ALL OCCURRENCES OF '''' IN ls_token-str WITH space.
       pt_tadir-object = gc_program.
       pt_tadir-obj_name = pv_calling_program.
       pt_tadir-routine = ls_token-str.
       APPEND pt_tadir.
       CLEAR pt_tadir.

       CONTINUE.
     ENDIF. " USEREXIT embedded in SAP code

   ENDLOOP. " loop through the program tokens

 ENDFORM.                    " CODE_ANALYSIS
*&---------------------------------------------------------------------*
*&      Form  ANALYZE_INCLUDES
*&---------------------------------------------------------------------*
*       Analyze the INCLUDES found in the main program and any
*       additional ones found in other INCLUDES
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM analyze_includes .
   DATA: l_later_pass      TYPE boole_d,
         l_calling_program TYPE trdir-name.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Analyzing INCLUDES'(059).

   CLEAR: l_later_pass.

   SORT gt_includes BY name.
   DELETE ADJACENT DUPLICATES FROM gt_includes COMPARING name.

   LOOP AT gt_includes.

*   import program into source code table
     READ REPORT gt_includes-name INTO gt_sourcecode.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     SCAN ABAP-SOURCE gt_sourcecode TOKENS        INTO gt_tokens
     STATEMENTS    INTO gt_statements
     KEYWORDS      FROM gt_keywords
     FRAME PROGRAM FROM gt_frames
     OVERFLOW      INTO g_overflow
     WITH INCLUDES
     WITH ANALYSIS.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     IF NOT gt_tokens[] IS INITIAL.
       l_calling_program = gt_includes-name.
       PERFORM code_analysis TABLES gt_tokens
         gt_tadir
         gt_includes
       USING l_later_pass
             l_calling_program.
     ENDIF.
   ENDLOOP. " Loop through INCLUDES

 ENDFORM.                    " ANALYZE_INCLUDES
*&---------------------------------------------------------------------*
*&      Form  ANALYZE_SUBMITS
*&---------------------------------------------------------------------*
*       Analyze the programs that are being submitted by main program
*       or INCLUDES. This is now a subsequent pass so analysis is not
*       as deep.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM analyze_submits .
   DATA: l_later_pass      TYPE boole_d,
         l_calling_program TYPE trdir-name.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Analyzing Submitted Programs'(066).

   l_later_pass = gc_true.

   SORT gt_submits BY name.
   DELETE ADJACENT DUPLICATES FROM gt_submits COMPARING name.

   LOOP AT gt_submits.

*   import program into source code table
     READ REPORT gt_submits-name INTO gt_sourcecode.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     SCAN ABAP-SOURCE gt_sourcecode TOKENS        INTO gt_tokens
     STATEMENTS    INTO gt_statements
     KEYWORDS      FROM gt_keywords
     FRAME PROGRAM FROM gt_frames
     WITH INCLUDES
     WITH ANALYSIS.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     IF NOT gt_tokens[] IS INITIAL.
       l_calling_program = gt_submits-name.
       PERFORM code_analysis TABLES gt_tokens
         gt_tadir
         gt_includes
       USING l_later_pass
             l_calling_program.
     ENDIF.
   ENDLOOP. " Loop through Submits

 ENDFORM.                    " ANALYZE_SUBMITS
*&---------------------------------------------------------------------*
*&      Form  ANALYZE_FUNCTION_MODULES
*&---------------------------------------------------------------------*
*       Analyze the function modules found in the main program and
*       INCLUDES. This is a subsequent pass so the analysis is not as
*       deep.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM analyze_function_modules .

   DATA: l_later_pass      TYPE boole_d,
         l_calling_program TYPE trdir-name,
         l_group           TYPE rs38l-area.

   DATA: BEGIN OF ls_tfdir,
           pname   TYPE tfdir-pname,
           include TYPE tfdir-include,
         END OF ls_tfdir.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Analyzing Function Modules'(067).

   l_later_pass = gc_true.

   SORT gt_functions BY name.
   DELETE ADJACENT DUPLICATES FROM gt_functions COMPARING name.

   LOOP AT gt_functions.

     SELECT SINGLE pname include INTO ls_tfdir
     FROM tfdir
     WHERE funcname = gt_functions-name.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
       EXPORTING
         program = ls_tfdir-pname
       IMPORTING
         group   = l_group. " let fm handle exceptions

     CONCATENATE 'L' l_group 'U' ls_tfdir-include INTO l_calling_program.

*   import program into source code table
     READ REPORT l_calling_program INTO gt_sourcecode.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     SCAN ABAP-SOURCE gt_sourcecode TOKENS        INTO gt_tokens
     STATEMENTS    INTO gt_statements
     KEYWORDS      FROM gt_keywords
     FRAME PROGRAM FROM gt_frames
     WITH INCLUDES
     WITH ANALYSIS.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     IF NOT gt_tokens[] IS INITIAL.
       PERFORM code_analysis TABLES gt_tokens
         gt_tadir
         gt_includes
       USING l_later_pass
             l_calling_program.
     ENDIF.
   ENDLOOP. " Loop through function modules

 ENDFORM.                    " ANALYZE_FUNCTION_MODULES
*&---------------------------------------------------------------------*
*&      Form  ANALYZE_PERFORMS
*&---------------------------------------------------------------------*
*       Analyze performs of routines in external programs called by
*       main program and INCLUDES. This is a subsequent pass so
*       analysis is not as deep.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM analyze_performs .
   DATA: l_later_pass      TYPE boole_d,
         l_calling_program TYPE trdir-name.

   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Analyzing Performs from External Programs'(068).

   l_later_pass = gc_true.

* SORT gt_performs BY name form.
   SORT gt_performs BY name.
   DELETE ADJACENT DUPLICATES FROM gt_performs COMPARING name.
   DELETE gt_performs WHERE name = g_tcode_program.

   LOOP AT gt_performs.

*   import program into source code table
     READ REPORT gt_performs-name INTO gt_sourcecode.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     SCAN ABAP-SOURCE gt_sourcecode TOKENS        INTO gt_tokens
     STATEMENTS    INTO gt_statements
     KEYWORDS      FROM gt_keywords
     FRAME PROGRAM FROM gt_frames
     WITH INCLUDES
     WITH ANALYSIS.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.

     IF NOT gt_tokens[] IS INITIAL.
       l_calling_program = gt_performs-name.
       PERFORM code_analysis TABLES gt_tokens
         gt_tadir
         gt_includes
       USING l_later_pass
             l_calling_program.
     ENDIF.
   ENDLOOP. " Loop through Performs

 ENDFORM.                    " ANALYZE_PERFORMS
*&---------------------------------------------------------------------*
*&      Form  add_smods_in_devclass
*&---------------------------------------------------------------------*
*       Add the SMODs in the development class associated with the
*       transaction code
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM add_smods_in_devclass .

   DATA: l_devclass TYPE tadir-devclass.
   REFRESH: gt_tadir,
   gt_functions.

   SELECT SINGLE devclass INTO l_devclass
   FROM tadir
   WHERE pgmid = 'R3TR'
   AND object = 'PROG'
   AND obj_name = g_tcode_program.
   IF sy-subrc <> 0.
     EXIT.
   ENDIF.

   SELECT object obj_name cproject FROM tadir
   INTO TABLE gt_tadir
   WHERE pgmid = 'R3TR'
   AND object = gc_sap_project
   AND devclass = l_devclass
   AND delflag = space.  " don't care what sy-subrc is

   LOOP AT gt_tadir.
     SELECT member APPENDING TABLE gt_functions
     FROM modsap
     WHERE typ = 'E'
     AND name = gt_tadir-obj_name.  " don't care what sy-subrc is
   ENDLOOP.

 ENDFORM.                    " add_smods_in_devclass
*&---------------------------------------------------------------------*
*&      Form  FIND_MIT_IMPLEMENTATIONS
*&---------------------------------------------------------------------*
*       Earlier we found Enhancements Spots and BAdI definitions
*       Now find out if they have been implemented by MIT
*       There are SAP enhancment implementations that are in table
*       SXC_EXIT but there don't seem to be any custom enhancement
*       implementations in SXC_EXIT. But table BADIIMPL_ENH seems to
*       hold all the custom enhancement implementations.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
 FORM find_mit_implementations .

   DATA: BEGIN OF lt_impl OCCURS 0,
           imp_name TYPE tadir-obj_name,
         END OF lt_impl.

   RANGES: lr_object FOR tadir-object.

   REFRESH lt_impl.

   fill_ranges lr_object 'I' 'EQ' gc_badi_impl      space. " SXCI
   fill_ranges lr_object 'I' 'EQ' gc_enhance_impl   space. " ENHO

   LOOP AT gt_tadir WHERE object = gc_badi_def.

     SELECT imp_name APPENDING TABLE lt_impl
     FROM sxc_exit
     WHERE exit_name = gt_tadir-obj_name. " don't care what sy-subrc is

   ENDLOOP.

   LOOP AT gt_tadir WHERE object = gc_enhance_spot.

     SELECT enhname APPENDING TABLE lt_impl
     FROM badiimpl_enh
     WHERE spotname = gt_tadir-obj_name.  " don't care what sy-subrc is

   ENDLOOP.

   CLEAR gt_tadir.

   LOOP AT lt_impl.
     SELECT SINGLE object obj_name cproject INTO gt_tadir
     FROM tadir
     WHERE pgmid = 'R3TR'
     AND object IN lr_object
     AND obj_name = lt_impl-imp_name
     AND devclass LIKE 'Z%'.      "#EC *  " don't care what sy-subrc is
     IF sy-subrc = 0.
       APPEND gt_tadir.
       CLEAR gt_tadir.
     ENDIF.
   ENDLOOP.

 ENDFORM.                    " FIND_MIT_IMPLEMENTATIONS
*&---------------------------------------------------------------------*
*&      Form  FIND_CMOD_INCLUDES
*&---------------------------------------------------------------------*
*       Find the CMOD INCLUDES associated with a particular development
*       class by finding the user exit function modules and then
*       finding the existing INCLUDES
*----------------------------------------------------------------------*
*      -->PV_DEVC  development class of interest
*----------------------------------------------------------------------*
 FORM find_cmod_includes USING pv_devc TYPE tadir-devclass.

   DATA: lt_tadir TYPE TABLE OF ty_tadir        WITH HEADER LINE.

   SELECT object obj_name cproject FROM tadir
   INTO TABLE lt_tadir
   WHERE pgmid = 'R3TR'
   AND object = gc_sap_project
   AND devclass = pv_devc
   AND delflag = space.  " don't care what sy-subrc is

   LOOP AT lt_tadir.
     SELECT member APPENDING TABLE gt_functions
     FROM modsap
     WHERE typ = 'E'
     AND name = lt_tadir-obj_name.  " don't care what sy-subrc is
   ENDLOOP.

   PERFORM analyze_function_modules.

 ENDFORM.                    " FIND_CMOD_INCLUDES

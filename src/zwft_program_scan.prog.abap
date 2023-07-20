*&---------------------------------------------------------------------*
*& Report  ZBCPROGRAM_SCAN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwft_program_scan  MESSAGE-ID 00 NO STANDARD PAGE HEADING LINE-SIZE 150.

TABLES: d020s,         " System table D020S (screen sources)
        tadir,         " Directory of R/3 Repository Objects
        trdir,
        tuchk3.        " Information on the CHECK Index

* Variable
TYPES: BEGIN OF result_ln,
         repname    LIKE sy-repid,
         dynnr      LIKE sy-dynnr,
         line_no(6) TYPE n,
         text       TYPE rstxp-tdline,
         sstring,
       END OF result_ln,

       result_tab TYPE result_ln OCCURS 0.

DATA: disptype  VALUE 'A',
      dtab      TYPE result_tab WITH HEADER LINE,
      ftab      TYPE result_tab WITH HEADER LINE,
      hit_count TYPE i,
      tabix     LIKE sy-tabix,
      text_line TYPE rstxp-tdline,
      top_flag.

DATA: BEGIN OF dyn OCCURS 0,
        prog LIKE d020s-prog,
        dnum LIKE d020s-dnum,
      END OF dyn,

      BEGIN OF fm OCCURS 0,
        obj_name LIKE tadir-obj_name,
      END OF fm,

      BEGIN OF irdir OCCURS 50,
        name LIKE trdir-name,
      END OF irdir,

      BEGIN OF itab OCCURS 50,
        repname LIKE sy-repid,
      END OF itab,

      itab_ii LIKE itab OCCURS 50 WITH HEADER LINE,

      BEGIN OF rtab OCCURS 0,
*        text LIKE rpy_dyflow-line,
        text(1000),
      END OF rtab,

      rtab_lcase LIKE LINE OF rtab,
      rtab_tmp   LIKE LINE OF rtab.

*
SELECTION-SCREEN: BEGIN OF BLOCK a10 WITH FRAME TITLE a10.
SELECT-OPTIONS:   repname  FOR trdir-name MEMORY ID zre,
                  dynnr    FOR d020s-dnum,
                  subc     FOR trdir-subc,
                  appl     FOR trdir-appl,
                  cnam     FOR trdir-cnam,
                  unam     FOR trdir-unam,
                  devclass FOR tadir-devclass.
SELECTION-SCREEN: END OF BLOCK a10,
BEGIN OF BLOCK a20 WITH FRAME TITLE a20.
SELECT-OPTIONS:   sstring     FOR tuchk3-text1 NO INTERVALS.
PARAMETERS: plusminu(2) TYPE n DEFAULT 2,
            inclu       AS CHECKBOX DEFAULT 'X',
            modiass     AS CHECKBOX,
            comment     AS CHECKBOX,
            mask        AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK a20,
BEGIN OF BLOCK a30 WITH FRAME TITLE a30.
PARAMETERS: rb_code RADIOBUTTON GROUP r10,
            rb_dyn  RADIOBUTTON GROUP r10,
            rb_all  RADIOBUTTON GROUP r10.
SELECTION-SCREEN: END OF BLOCK a30.

DATA: string LIKE sstring-low.

*
INITIALIZATION.
  a10 = 'Report/Screen selection'.
  a20 = 'Search criteria'.
  a30 = 'Search area'.

*
TOP-OF-PAGE.
  PERFORM top_of_page.

*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

*
AT SELECTION-SCREEN.
  IF sstring[] IS INITIAL AND modiass IS INITIAL.
    MESSAGE e000 WITH 'Enter search string'.
  ENDIF.

*
START-OF-SELECTION.

*  CALL FUNCTION 'Z_CBO_USAGE_LOG'
*       EXPORTING
*            s_time = sy-uzeit.
*
*  PERFORM record_use(zkbc1001) USING sy-cprog.

  IF NOT modiass IS INITIAL.
    sstring-sign    = 'I'.
    sstring-option  = 'EQ'.
    sstring-low     = '{'.
    APPEND sstring.
    sstring-low     = '}'.
    APPEND sstring.
  ENDIF.

  READ TABLE sstring INDEX 1.
  DESCRIBE TABLE sstring LINES sy-tfill.
  IF sy-tfill = 1.
    string = sstring-low.
  ELSE.
    CONCATENATE sstring-low
               '...'
                INTO string SEPARATED BY space.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Read data'.

  SET PF-STATUS 'MAIN'.

  IF NOT devclass[] IS INITIAL.
    SELECT obj_name INTO TABLE irdir
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'PROG'
      AND   devclass IN devclass.

    SELECT obj_name INTO TABLE fm
      FROM tadir
      WHERE pgmid  = 'R3TR'
      AND   object = 'FUGR'
      AND   devclass IN devclass.

    LOOP AT fm.
      CONCATENATE 'SAPL'
                   fm-obj_name INTO irdir.
      APPEND irdir.
    ENDLOOP.
  ENDIF.

  IF NOT repname[] IS INITIAL.
    SELECT name APPENDING TABLE irdir
      FROM trdir
      WHERE name IN repname
      AND   cnam IN cnam
      AND   unam IN unam
      AND   subc IN subc
      AND   appl IN appl.
  ENDIF.

  CHECK NOT irdir[] IS INITIAL.

  IF rb_code IS INITIAL.
*   Also scan dynpro flow logic
    SELECT prog dnum INTO TABLE dyn
      FROM d020s FOR ALL ENTRIES IN irdir
      WHERE prog = irdir-name
      AND   dnum IN dynnr.
  ENDIF.

  IF NOT inclu IS INITIAL.
    LOOP AT irdir.
      REFRESH itab_ii.
      CALL FUNCTION 'GET_INCLUDES'
        EXPORTING
          progname = irdir-name
        TABLES
          incltab  = itab_ii.

      APPEND LINES OF itab_ii TO itab.
    ENDLOOP.
  ENDIF.

  APPEND LINES OF irdir TO itab.
  SORT itab.
  DELETE ADJACENT DUPLICATES FROM itab.

  IF rb_dyn IS INITIAL.
    LOOP AT itab.
      READ REPORT itab-repname INTO rtab.
      CHECK sy-subrc = 0.
      IF NOT comment IS INITIAL.
        DELETE rtab
          WHERE text(1) = '*'.
      ENDIF.

      LOOP AT sstring.
        IF mask IS INITIAL.
          SEARCH rtab FOR sstring-low.
          CHECK sy-subrc = 0.
        ELSE.
          sy-tabix = 1.
        ENDIF.

        LOOP AT rtab FROM sy-tabix.
          tabix      = sy-tabix.
          rtab_lcase = rtab.
          TRANSLATE rtab TO UPPER CASE.

          CASE mask.
            WHEN space.
              IF rtab-text CS sstring-low.
                PERFORM: get_hit_set USING rtab_lcase tabix space.
              ENDIF.

            WHEN OTHERS.
              IF rtab-text CP sstring-low.
                PERFORM: get_hit_set USING rtab_lcase tabix space.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  FREE: itab.

  IF rb_code IS INITIAL.
    LOOP AT dyn.
      PERFORM get_dynpro_flow_logic.

      IF NOT comment IS INITIAL.
        DELETE rtab
          WHERE text(1) = '*'.
      ENDIF.

      LOOP AT sstring.
        IF mask IS INITIAL.
          SEARCH rtab FOR sstring-low.
          CHECK sy-subrc = 0.
        ELSE.
          sy-tabix = 1.
        ENDIF.

        LOOP AT rtab FROM sy-tabix.
          tabix      = sy-tabix.
          rtab_lcase = rtab.
          TRANSLATE rtab TO UPPER CASE.

          CASE mask.
            WHEN space.
              IF rtab-text CS sstring-low.
                PERFORM: get_hit_set USING rtab_lcase tabix 'D'.
              ENDIF.

            WHEN OTHERS.
              IF rtab-text CP sstring-low.
                PERFORM: get_hit_set USING rtab_lcase tabix 'D'.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  FREE: dyn.

END-OF-SELECTION.
  IF ftab[] IS INITIAL AND dtab[] IS INITIAL.
    MESSAGE s000 WITH 'String' string 'not found'.
    EXIT.
  ENDIF.

  SORT: ftab BY repname line_no sstring DESCENDING,
        dtab BY repname dynnr line_no sstring DESCENDING.

  DELETE ADJACENT DUPLICATES FROM: ftab COMPARING repname line_no,
                                   dtab COMPARING repname dynnr line_no.

  MESSAGE s000 WITH hit_count 'Hits for string' string.

  PERFORM: display_ftab_alt,
           display_dtab_alt.

*
AT LINE-SELECTION.
  PERFORM call_editor.

*
AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'TOGL'.
      CASE disptype.
        WHEN 'M'.
          disptype = 'A'.
          PERFORM: display_ftab_alt,
                   display_dtab_alt.
        WHEN 'A'.
          disptype = 'M'.
          PERFORM: display_ftab,
                   display_dtab.
      ENDCASE.
      sy-lsind = sy-lsind - 1.
  ENDCASE.

*---------------------------------------------------------------------*
*       FORM APPEND_FTAB                                              *
*---------------------------------------------------------------------*
FORM append_ftab USING text    LIKE rtab_lcase
                       line_no LIKE sy-tabix
                       sstring TYPE c
                       source  TYPE c.

  DATA: ln TYPE result_ln.

  ln-line_no = line_no.
  ln-text    = text.
  ln-sstring = sstring.

  IF source IS INITIAL.
    ln-repname = itab-repname.
    APPEND ln TO ftab.
  ELSE.
    ln-repname = dyn-prog.
    ln-dynnr   = dyn-dnum.
    APPEND ln TO dtab.
    CLEAR: ln-dynnr.
  ENDIF.

  IF NOT ln-sstring IS INITIAL.
    ADD 1 TO hit_count.
  ENDIF.
ENDFORM.                    "append_ftab

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA: repname(50).

  FORMAT INTENSIFIED OFF.
  ULINE /(150).
  WRITE: /  sy-vline,
           'Scan for string:',
            string+0(40) COLOR COL_GROUP INTENSIFIED,
        66 'Date:',
            sy-datum DD/MM/YYYY,
      150   sy-vline.
  ULINE /(150).
  FORMAT INTENSIFIED ON.
  WRITE: / sy-vline.

  IF top_flag = 'A'.
    repname = ftab-repname.
  ELSE.
    CONCATENATE dtab-repname
               'Screen'
                dtab-dynnr
                INTO repname SEPARATED BY space.
  ENDIF.

  CASE disptype.
    WHEN 'A'.
      CONCATENATE 'Line   Source code ('
                   repname
                   ')' INTO text_line.
      WRITE: text_line COLOR COL_HEADING INTENSIFIED.
    WHEN 'M'.
      IF top_flag = 'A'.
        WRITE: 'Source Code'.
      ELSE.
        WRITE: 'Source Code'.
      ENDIF.
  ENDCASE.
  WRITE: 150 sy-vline.
  ULINE (150).
ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM DISPLAY_FTAB                                             *
*---------------------------------------------------------------------*
*       Display search results
*---------------------------------------------------------------------*
FORM display_ftab.
  CHECK rb_dyn IS INITIAL.

  top_flag = 'A'.

  NEW-PAGE.

  LOOP AT ftab.
*    FORMAT HOTSPOT ON.
    IF ftab-sstring IS INITIAL.
      WRITE: / sy-vline,
               ftab-text INTENSIFIED OFF.
    ELSE.
      WRITE: / sy-vline,
               ftab-text INTENSIFIED OFF COLOR COL_KEY.
    ENDIF.
    WRITE:   sy-vline       NO-GAP,
             ftab-line_no   NO-GAP INTENSIFIED,
             sy-vline       NO-GAP,
             ftab-repname   NO-GAP INTENSIFIED,
        150  sy-vline       NO-GAP.

    HIDE: ftab-repname,
          ftab-line_no.

    AT END OF repname.
*      FORMAT HOTSPOT OFF.
      ULINE /(150).
    ENDAT.
  ENDLOOP.
ENDFORM.                    "display_ftab
*---------------------------------------------------------------------*
*       FORM DISPLAY_DTAB                                             *
*---------------------------------------------------------------------*
*       Display search results (dynpro)
*---------------------------------------------------------------------*
FORM display_dtab.
  CHECK rb_code IS INITIAL.

  top_flag = 'D'.

  NEW-PAGE.

  LOOP AT dtab.
*    FORMAT HOTSPOT ON.
    IF dtab-sstring IS INITIAL.
      WRITE: / sy-vline,
               dtab-text INTENSIFIED OFF.
    ELSE.
      WRITE: / sy-vline,
               dtab-text INTENSIFIED OFF COLOR COL_KEY.
    ENDIF.
    WRITE:   sy-vline       NO-GAP,
             dtab-line_no   NO-GAP INTENSIFIED,
             sy-vline       NO-GAP,
             dtab-repname   NO-GAP INTENSIFIED,
             dtab-dynnr     NO-GAP INTENSIFIED,
        150  sy-vline       NO-GAP.

    HIDE:    dtab-repname,
             dtab-dynnr,
             dtab-line_no.

    AT END OF repname.
*      FORMAT HOTSPOT OFF.
      ULINE /(150).
    ENDAT.
  ENDLOOP.
ENDFORM.                    "display_dtab

*---------------------------------------------------------------------*
*       FORM DISPLAY_FTAB_ALT
*---------------------------------------------------------------------*
*       Display hit list without report name on every line
*---------------------------------------------------------------------*
FORM display_ftab_alt.
  CHECK rb_dyn IS INITIAL.

  top_flag = 'A'.

  NEW-PAGE.

  LOOP AT ftab.

    AT NEW repname.
*      FORMAT HOTSPOT ON.
      IF sy-tabix > 1.
        NEW-PAGE.
      ENDIF.
    ENDAT.

    WRITE: / sy-vline       NO-GAP,
             ftab-line_no   NO-GAP,
             sy-vline       NO-GAP.

    IF ftab-sstring IS INITIAL.
      WRITE: ftab-text      INTENSIFIED OFF.
    ELSE.
      WRITE: ftab-text      INTENSIFIED OFF COLOR COL_KEY.
    ENDIF.

    WRITE: 150 sy-vline.

    HIDE:    ftab-repname,
             ftab-line_no.

    AT END OF repname.
*      FORMAT HOTSPOT OFF.
      ULINE /(150).
    ENDAT.
  ENDLOOP.
ENDFORM.                    "display_ftab_alt
*---------------------------------------------------------------------*
*       FORM DISPLAY_DTAB_ALT
*---------------------------------------------------------------------*
*       Display dynpro hit list without report name on every line
*---------------------------------------------------------------------*
FORM display_dtab_alt.
  CHECK rb_code IS INITIAL.

  top_flag = 'D'.

  NEW-PAGE.

  LOOP AT dtab.

    AT NEW repname.
*      FORMAT HOTSPOT ON.
      IF sy-tabix > 1.
        NEW-PAGE.
      ENDIF.
    ENDAT.

    WRITE: / sy-vline       NO-GAP,
             dtab-line_no   NO-GAP,
             sy-vline       NO-GAP.

    IF dtab-sstring IS INITIAL.
      WRITE: dtab-text      INTENSIFIED OFF.
    ELSE.
      WRITE: dtab-text      INTENSIFIED OFF COLOR COL_KEY.
    ENDIF.

    WRITE: 150 sy-vline.

    HIDE:    dtab-repname,
             dtab-dynnr,
             dtab-line_no.

    AT END OF repname.
*      FORMAT HOTSPOT OFF.
      ULINE /(150).
    ENDAT.
  ENDLOOP.
ENDFORM.                    "display_dtab_alt
*----------------------------------------------------------------------*
*       Form  GET_PLUS_MINUS_X_LINES
*----------------------------------------------------------------------*
*       Get x lines before the found string and x lines after
*----------------------------------------------------------------------*
FORM get_hit_set USING VALUE(ctext)   LIKE rtab_lcase
                       VALUE(line_no) LIKE sy-tabix
                       srce_type      TYPE c.

  DATA: end    TYPE i,
        start  TYPE i,
        xtabix LIKE sy-tabix.

  IF plusminu <= 0.
    PERFORM append_ftab USING ctext line_no 'X' srce_type.
    EXIT.
  ENDIF.

  start = line_no - plusminu .
  end   = line_no + plusminu.

  IF start < 1.
    start = 1.
  ENDIF.

  WHILE start <= end.
    READ TABLE rtab INTO ctext INDEX start.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    xtabix = sy-tabix.
    IF start = line_no.
      PERFORM append_ftab USING ctext xtabix 'X' srce_type.
    ELSE.
      PERFORM append_ftab USING ctext xtabix space srce_type.
    ENDIF.
    ADD 1 TO start.
  ENDWHILE.
ENDFORM.                               " GET_HIT_SET
*----------------------------------------------------------------------*
*       Form  CALL_EDITOR
*----------------------------------------------------------------------*
*       Call ABAP or screen painter editor
*----------------------------------------------------------------------*
FORM call_editor.
  CLEAR: ftab,
         dtab.

  READ LINE sy-lilli
       FIELD VALUE ftab-repname
                   ftab-line_no
                   dtab-repname
                   dtab-dynnr
                   dtab-line_no.

  IF ftab-repname IS INITIAL AND dtab-repname IS INITIAL.
    MESSAGE s000 WITH 'Invalid cursor position'.
    EXIT.
  ENDIF.

  IF dtab-dynnr IS INITIAL.
*   Call ABAP editor
    CALL FUNCTION 'EDITOR_PROGRAM'
      EXPORTING
        display = 'X'
        program = ftab-repname
        line    = ftab-line_no
      EXCEPTIONS
        OTHERS  = 1.

    SET PARAMETER ID 'RID' FIELD sy-repid.
  ELSE.
    CALL FUNCTION 'RS_SCRP'
      EXPORTING
        abl_line       = dtab-line_no
        dynnr          = dtab-dynnr
        progname       = dtab-repname
        wanted_mode    = 'SHOW'
      EXCEPTIONS
        already_exists = 1
        not_found      = 2
        not_executed   = 3
        OTHERS         = 4.
  ENDIF.
ENDFORM.                               " CALL_EDITOR
*---------------------------------------------------------------------*
*       Form  GET_DYNPRO_FLOW_LOGIC
*----------------------------------------------------------------------*
*       Get flow logic of the dynpro
*----------------------------------------------------------------------*
FORM get_dynpro_flow_logic.
  DATA: dhead  LIKE d020s,
        dfield LIKE d021s OCCURS 0,
        dflow  LIKE d022s OCCURS 0,
        dmatch LIKE d023s OCCURS 0,

        BEGIN OF dynp_id,
          prog LIKE d020s-prog,
          dnum LIKE d020s-dnum,
        END OF dynp_id.

  dynp_id-prog = dyn-prog.
  dynp_id-dnum = dyn-dnum.

  IMPORT DYNPRO dhead dfield dflow dmatch ID dynp_id.

  rtab[] = dflow[].
ENDFORM.                               " GET_DYNPRO_FLOW_LOGIC

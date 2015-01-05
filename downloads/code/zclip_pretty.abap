*&-------------------------------------------------------------*
*&     Beautify ABAP Code Via Clip                             *
*&-------------------------------------------------------------*

* based on ideas from
* http://sap.ionelburlacu.ro/abap/sap2/Beautify_Source_Code.html

* Author Jayanta Narayan Choudhuri
*         Flat 302
*         395 Jodhpur Park
*         Kolkata 700 068
*       Email sss@cal.vsnl.net.in
*       URL:  http://www.geocities.com/ojnc

*---------------------------------------------------------------
* This program takes ABAP code in ClipBoard, and does the following:
*    - Attempts to move comments to the end of the line
*    - Adds comments (table name) for the tables listed after a TABLES
*      statement if the line has not been commented already.
*    - Adds comments (field name) for data elements, parameters, and
*      select-options that are defined using the LIKE or FOR statement
*    - For ENDLOOP/ENDSELECT adds comment identify the LOOP/SELECT
*      that is being closed
*    - For FORM/ENDFORM adds comment identify the FORM that is being
*      closed
*    - Calls function PRETTY_PRINTER to do the SAP standard pretty print
*      after the custom comments have been created
* Returns Modified Code Via ClipBoard
*-----------------------------------------------------------------------
REPORT  zclip_pretty.          .

TABLES:
e071 ,      " Change & Transport System: Object Entries of Requests/Tasks
tadir ,     " Directory of Repository Objects
trdir ,     " Generated Table for View TRDIR
dd02t .     " R/3 DD: SAP table texts

DATA: BEGIN OF mtab_old_prog OCCURS 0,
line(172) TYPE c,
END OF mtab_old_prog.

DATA: BEGIN OF mtab_new_prog OCCURS 0,
line(172) TYPE c,
END OF mtab_new_prog.

DATA: BEGIN OF mtab_jnc_prog OCCURS 0,
line(172) TYPE c,
END OF mtab_jnc_prog.

DATA:
* Hold an entire statement, even if it spans multiple lines
BEGIN OF mtab_long_line OCCURS 0,
start        TYPE i,
end          TYPE i,
code(9999)   TYPE c, "For type "C", a maximum length specification
"of 65535 is allowed.
END OF mtab_long_line.

DATA: BEGIN OF mtab_tabname OCCURS 0,
tabname LIKE dd02t-tabname,    " Table name
tabdesc LIKE dd02t-ddtext,     " Short text describing ABAP/4 Dictio
END OF mtab_tabname.

* Queue to hold list of internal table names for commenting the ENDLOOP
* line
DATA: BEGIN OF mtab_itab_names OCCURS 0,
tabname(40) TYPE c,
END OF mtab_itab_names.

* Queue to hold list of table names for commenting the ENDSELECT line
DATA: BEGIN OF mtab_tab_names OCCURS 0,
tabname(40) TYPE c,
END OF mtab_tab_names.

DATA: BEGIN OF mtab_form_names OCCURS 0,
tabname(40) TYPE c,
END OF mtab_form_names.

DATA: mylength TYPE i,
myrc     TYPE i.

CONSTANTS: myhats(40) VALUE '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'.

* START of EXECUTION

* Read the program code in ClipBoard into an internal table
CALL METHOD cl_gui_frontend_services=>clipboard_import
IMPORTING
data   = mtab_old_prog[]
length = mylength.

IF sy-subrc NE 0.
WRITE: / `Unable to read ClipBoard`.
WRITE: / `Exiting program`.
ENDIF.

PERFORM create_condensed_table TABLES mtab_old_prog mtab_long_line.

PERFORM format_program.

CALL FUNCTION `PRETTY_PRINTER`
EXPORTING
inctoo             = space
TABLES
ntext              = mtab_jnc_prog
otext              = mtab_new_prog
EXCEPTIONS
enqueue_table_full = 1
include_enqueued   = 2
include_readerror  = 3
include_writeerror = 4
OTHERS             = 5.

* Write the beautiful program code to ClipBoard from internal table
CALL METHOD cl_gui_frontend_services=>clipboard_export
IMPORTING
data = mtab_jnc_prog[]
CHANGING
rc   = myrc.

LOOP AT mtab_jnc_prog.
IF mtab_jnc_prog = space.
SKIP 1.
ENDIF.
WRITE: / mtab_jnc_prog.
ENDLOOP.         " LOOP AT MTAB_JNC_PROG


*---------------------------------------------------------------------*
*       FORM CREATE_CONDENSED_TABLE               *
*---------------------------------------------------------------------*
*       Create a table that has all statements condensed onto 1 line  *
*---------------------------------------------------------------------*
FORM create_condensed_table
TABLES ftab_old_prog STRUCTURE mtab_old_prog
ftab_long_line STRUCTURE mtab_long_line.

DATA:
* Structure to hold program code/comment
BEGIN OF fstr_line,
code(172)    TYPE c,              " Program Code
comment(172) TYPE c,              " Inline comments
END OF fstr_line.

LOOP AT ftab_old_prog.

IF ftab_long_line-start = 0.
ftab_long_line-start = ftab_long_line-end + 1.
CLEAR ftab_long_line-end.
ENDIF.

*   Strip off any inline comments so they do not get in the way
*   If comments are not separated, then words in the comments could
*   look like keywords, and cause problems
SPLIT ftab_old_prog-line AT `"` INTO fstr_line-code fstr_line-comment.

*   Align all statements to be left justified
SHIFT fstr_line-code LEFT DELETING LEADING space.

*   Put all lines that make up a single statement into one field
*   This will make it easier to isolate key words.  For example, if you
*   want to process a TABLES statement, but exclude the TABLES part of a
*   function call, or a subroutine call.
CONCATENATE ftab_long_line-code  fstr_line-code
INTO ftab_long_line-code SEPARATED BY space.

IF fstr_line-code   CA `.`   OR    " Period means end of statement
fstr_line-code(1) = `*` OR      " Comment Line
fstr_line-code   CO space.      " Blank Line
*     Keep track of the table index that the statement ends on
ftab_long_line-end = sy-tabix.
*     Remove delimiter from concatenation of fields
SHIFT ftab_long_line-code LEFT BY 1 PLACES.

APPEND ftab_long_line.

CLEAR: ftab_long_line-code, ftab_long_line-start.

*     Don`t clear out fstr_long_line-end yet.  It is used to calc
*     fstr_long_line-start.
ENDIF.

ENDLOOP.         " LOOP AT FTAB_OLD_PROG
ENDFORM.    " FORM CREATE_CONDENSED_TABLE

*---------------------------------------------------------------------*
*       FORM FORMAT_PROGRAM                                           *
*---------------------------------------------------------------------*
FORM format_program.

DATA: lstr_old_prog LIKE LINE OF mtab_old_prog.

LOOP AT mtab_long_line.

TRANSLATE mtab_long_line-code TO UPPER CASE.

IF mtab_long_line-code(1) EQ `*`.  " Do not modify Comment Lines

LOOP AT mtab_old_prog FROM mtab_long_line-start
TO   mtab_long_line-end.

mtab_new_prog-line = mtab_old_prog-line.

APPEND mtab_new_prog.

ENDLOOP.     " LOOP AT MTAB_OLD_PROG

ELSEIF mtab_long_line-code(6) EQ `TABLES`.

*     Reformat any TABLES statements.  Will only reformat when TABLES
*     is at the start of the statement.  Will not try to get table
*     descriptions for CALL FUNCTIONS or FORM/PERFORMs

*     Get the table names from mstr_long_line.
PERFORM get_table_names_from_statement TABLES mtab_tabname
USING mtab_long_line-code.
*     Find the descriptions for each table
PERFORM get_table_descriptions TABLES mtab_tabname.

*     create the new statement
PERFORM build_new_tables_statement USING mtab_long_line.

ELSE. " All other modifications to the code handled here

LOOP AT mtab_old_prog FROM mtab_long_line-start
TO   mtab_long_line-end.

*       Remove extra spaces from line for comparisons
lstr_old_prog-line = mtab_old_prog-line.
CONDENSE lstr_old_prog-line.
TRANSLATE lstr_old_prog-line TO UPPER CASE.

IF lstr_old_prog-line CS `"`.  " Comments
mtab_new_prog-line = mtab_old_prog-line.
ELSE.
IF lstr_old_prog-line CS ` LIKE ` OR
lstr_old_prog-line CS ` TYPE ` OR
lstr_old_prog-line CS ` FOR `  OR
lstr_old_prog-line CS `~`.            "jnc OpenSQL table~column


PERFORM get_for_like_comment USING mtab_old_prog
CHANGING mtab_new_prog.

ELSEIF lstr_old_prog-line(8) = 'LOOP AT'.

*     save table name into a queue
PERFORM enqueue_itab_name USING mtab_long_line-code.
mtab_new_prog-line = mtab_old_prog-line.

ELSEIF lstr_old_prog-line(7) = `ENDLOOP`.

*     get name off of queue and add it as a comment to the ENDLOOP line
PERFORM add_comment_to_endloop USING mtab_old_prog-line
CHANGING mtab_new_prog-line.

ELSEIF lstr_old_prog-line(7) EQ 'SELECT' AND
lstr_old_prog-line(13) NE 'SELECT SINGLE'.

*     save table name into a queue
PERFORM enqueue_tab_name USING mtab_old_prog-line.
mtab_new_prog-line = mtab_old_prog-line.
ELSEIF lstr_old_prog-line(9) = `ENDSELECT`.

*     get name off of queue and add it as a comment to the ENDSELECT
PERFORM add_comment_to_select USING mtab_old_prog-line
CHANGING mtab_new_prog-line.

ELSEIF lstr_old_prog-line(5) = 'FORM'.

*         save form name into a queue
PERFORM enqueue_form_name USING mtab_old_prog-line.
mtab_new_prog-line = mtab_old_prog-line.

ELSEIF lstr_old_prog-line(7) = `ENDFORM`.

*         get name off of queue and add it as a comment to the ENDFORM
PERFORM add_comment_to_endform USING mtab_old_prog-line
CHANGING mtab_new_prog-line.
ELSE.    " Any other lines
mtab_new_prog-line = mtab_old_prog-line.
ENDIF.
ENDIF.

APPEND mtab_new_prog.

ENDLOOP.     " LOOP AT MTAB_OLD_PROG
ENDIF.
ENDLOOP.         " LOOP AT MTAB_LONG_LINE

ENDFORM.    " FORM FORMAT_PROGRAM


*---------------------------------------------------------------------*
*       FORM GET_TABLE_NAMES_FROM_STATEMENT                           *
*---------------------------------------------------------------------*
FORM get_table_names_from_statement TABLES ftab_tabname
STRUCTURE mtab_tabname
USING  fc_statement.

CLEAR ftab_tabname.
REFRESH ftab_tabname.

REPLACE `TABLES` WITH space INTO fc_statement.
TRANSLATE fc_statement USING `. `.   " Replace periods
TRANSLATE fc_statement USING `, `.   " Replace commas
TRANSLATE fc_statement USING `: `.   " Replace colons

CONDENSE fc_statement.               " Remove all extra spaces

SPLIT fc_statement AT space INTO TABLE ftab_tabname.

ENDFORM.    " FORM GET_TABLE_NAMES_FROM_STATEMENT


*---------------------------------------------------------------------*
*       FORM GET_TABLE_DESCRIPTIONS                                   *
*---------------------------------------------------------------------*
FORM get_table_descriptions TABLES ftab_tabname STRUCTURE mtab_tabname.

LOOP AT ftab_tabname.
SELECT SINGLE * FROM  dd02t
WHERE  tabname     = ftab_tabname-tabname
AND    ddlanguage  = sy-langu.

IF sy-subrc = 0.
ftab_tabname-tabdesc = dd02t-ddtext.
MODIFY ftab_tabname.
ENDIF.

ENDLOOP.         " LOOP AT FTAB_TABNAME
ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM BUILD_NEW_TABLES_STATEMENT                               *
*---------------------------------------------------------------------*
FORM build_new_tables_statement USING fstr_long_line LIKE mtab_long_line.

DATA: lc_sep(1)   TYPE c,
li_rows     TYPE i,
wordlen     TYPE i.

DESCRIBE TABLE mtab_tabname LINES li_rows.

mtab_new_prog-line = `TABLES:`.
APPEND mtab_new_prog.

LOOP AT mtab_tabname.
IF sy-tabix = li_rows.
lc_sep = `.`.
ELSE.
lc_sep = `,`.
ENDIF.

wordlen = STRLEN( mtab_tabname-tabname ).

IF wordlen < 12.
wordlen = 12 - wordlen.
ELSE.
wordlen = 1.
ENDIF.

CONCATENATE `^^` mtab_tabname-tabname lc_sep myhats+0(wordlen) ` " `
mtab_tabname-tabdesc INTO mtab_new_prog.

TRANSLATE mtab_new_prog USING `^ `.

APPEND mtab_new_prog.

ENDLOOP.         " LOOP AT MTAB_TABNAME

ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM GET_FOR/LIKE_COMMENT                                     *
*---------------------------------------------------------------------*
FORM get_for_like_comment USING value(f_old_prog) LIKE mtab_old_prog
CHANGING f_new_prog     LIKE mtab_new_prog
.

DATA:
lc_dummy(1)    TYPE c,
lc_tabname(40) TYPE c,
wordlen        TYPE i,
ltab_nametab   LIKE dntab OCCURS 0 WITH HEADER LINE,
lstr_old_prog  LIKE LINE OF mtab_old_prog,

BEGIN OF lstr_field,
tabname      LIKE dd02t-tabname, " Table name
fldname      LIKE dd02t-tabname, " Table name
END OF lstr_field.

lstr_old_prog-line = f_old_prog.  " SAVE input

TRANSLATE f_old_prog TO UPPER CASE.
CONDENSE f_old_prog.

IF f_old_prog-line   CA `"` OR       " Line already commented
f_old_prog-line(1) = `*`.
f_new_prog = f_old_prog.
RETURN.

ELSEIF f_old_prog CS ` LIKE `.
SPLIT f_old_prog AT ` LIKE ` INTO lc_dummy lc_tabname.

ELSEIF f_old_prog CS ` TYPE `.
SPLIT f_old_prog AT ` TYPE ` INTO lc_dummy lc_tabname.

ELSEIF f_old_prog CS ` FOR `.
SPLIT f_old_prog AT ` FOR ` INTO lc_dummy  lc_tabname.

ELSEIF f_old_prog CS `~`.
MOVE f_old_prog TO lc_tabname.
CONDENSE lc_tabname.
SPLIT lc_tabname AT `~` INTO lstr_field-tabname lstr_field-fldname.

ELSE.
f_new_prog = lstr_old_prog-line.
RETURN.
ENDIF.

* If there is anything following the table-field in a LIKE or FOR clause
* it will be removed so that only the table-field remains
IF NOT f_old_prog CS `~`.
CONDENSE lc_tabname.

TRANSLATE lc_tabname USING `. `.     " Remove periods
TRANSLATE lc_tabname USING `, `.     " Remove commas
CONDENSE lc_tabname.                 " Remove extra white space

SPLIT lc_tabname AT `-` INTO lstr_field-tabname lstr_field-fldname.

* The system variables are actually defined in DDIC structure SYST
IF lstr_field-tabname = `SY`.
lstr_field-tabname = `SYST`.
ENDIF.
ENDIF.

TRANSLATE lstr_field-tabname TO UPPER CASE.
TRANSLATE lstr_field-fldname TO UPPER CASE.

CALL FUNCTION 'NAMETAB_GET'
EXPORTING
tabname             = lstr_field-tabname
TABLES
nametab             = ltab_nametab
EXCEPTIONS
internal_error      = 1
table_has_no_fields = 2
table_not_activ     = 3
no_texts_found      = 4
OTHERS              = 5.

IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

READ TABLE ltab_nametab
WITH KEY tabname   = lstr_field-tabname
fieldname = lstr_field-fldname.

IF sy-subrc = 0.

wordlen = STRLEN( lstr_old_prog-line ).

IF wordlen < 45.
wordlen = 45 - wordlen.
ELSE.
wordlen = 1.
ENDIF.

CONCATENATE lstr_old_prog-line  myhats+0(wordlen) ` " ` ltab_nametab-fieldtext
INTO f_new_prog.

TRANSLATE mtab_new_prog USING `^ `.

ELSE.
f_new_prog = lstr_old_prog-line.
ENDIF.

ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS

*---------------------------------------------------------------------*
*       FORM ENQUEUE_ITAB_NAME                                        *
*---------------------------------------------------------------------*
FORM enqueue_itab_name USING value(f_line) LIKE mtab_long_line-code.

DATA:
lc_dummy(1) TYPE c,
lc_itab(40) TYPE c.

TRANSLATE f_line TO UPPER CASE.

SPLIT f_line AT `LOOP AT ` INTO lc_dummy lc_itab.

SPLIT lc_itab AT space INTO lc_itab lc_dummy.

TRANSLATE lc_itab USING `. `.
CONDENSE lc_itab.

mtab_itab_names = lc_itab.

* Always have the most recent LOOP AT table as the first entry in the
* queue
INSERT mtab_itab_names INDEX 1.

ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM ADD_COMMENT_TO_ENDLOOP                                   *
*---------------------------------------------------------------------*
FORM add_comment_to_endloop USING fstr_long_line LIKE mtab_old_prog-line
CHANGING f_prog_line LIKE mtab_new_prog-line
.

IF mtab_old_prog-line NA `"`.        " No comments
*     Get the internal table from the queue
READ TABLE mtab_itab_names INDEX 1.
CONCATENATE mtab_old_prog-line `    "` `LOOP AT`  mtab_itab_names-tabname
INTO f_prog_line SEPARATED BY space.

*     Dequeue the itab name
DELETE mtab_itab_names INDEX 1.
ELSE.
f_prog_line = mtab_old_prog-line.
ENDIF.
ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM ENQUEUE_TAB_NAME                                         *
*---------------------------------------------------------------------*
FORM enqueue_tab_name USING  f_line LIKE mtab_old_prog-line.

DATA:
lc_dummy(1) TYPE c,
lc_tab(40) TYPE c.

TRANSLATE f_line TO UPPER CASE.

SPLIT f_line AT ` FROM ` INTO lc_dummy lc_tab.

CONDENSE lc_tab. " Remove leading/trailing extra spaces

SPLIT lc_tab AT space INTO lc_tab lc_dummy.

TRANSLATE lc_tab USING `. `.
CONDENSE lc_tab.

mtab_tab_names = lc_tab.

* Always have the most recent LOOP AT table as the first entry in the
* queue
INSERT mtab_tab_names INDEX 1.

ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM ADD_COMMENT_TO_SELECT                                    *
*---------------------------------------------------------------------*
FORM add_comment_to_select USING fstr_long_line LIKE mtab_old_prog-line
CHANGING f_prog_line.

IF mtab_old_prog-line NA `"`.        " No comments
*     Get the table from the queue
READ TABLE mtab_tab_names INDEX 1.
CONCATENATE mtab_old_prog-line  `    "` `SELECT FROM` mtab_tab_names-tabname
INTO f_prog_line SEPARATED BY space.

*     Dequeue the tab name
DELETE mtab_tab_names INDEX 1.
ELSE.
f_prog_line = mtab_old_prog-line.
ENDIF.

ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM ADD_COMMENT_TO_ENDFORM                                   *
*---------------------------------------------------------------------*
FORM add_comment_to_endform USING fstr_long_line LIKE mtab_old_prog-line
CHANGING f_prog_line.

IF mtab_old_prog-line NA `"`.        " No comments
*     Get the table from the queue
READ TABLE mtab_form_names INDEX 1.
CONCATENATE mtab_old_prog-line `   "` `FORM`  mtab_form_names-tabname
INTO f_prog_line SEPARATED BY space.

*     Dequeue the form name
DELETE mtab_form_names INDEX 1.
ELSE.
f_prog_line = mtab_old_prog-line.
ENDIF.

ENDFORM.    " FORM GET_TABLE_DESCRIPTIONS


*---------------------------------------------------------------------*
*       FORM ENQUEUE_FORM_NAME                                        *
*---------------------------------------------------------------------*
FORM enqueue_form_name USING f_line.

DATA:
lc_dummy(1) TYPE c,
lc_tab(40) TYPE c.

TRANSLATE f_line TO UPPER CASE.

SPLIT f_line AT `FORM ` INTO lc_dummy
lc_tab.

CONDENSE lc_tab. " Remove leading/trailing extra spaces

SPLIT lc_tab AT space INTO lc_tab lc_dummy.

TRANSLATE lc_tab USING `. `.
CONDENSE lc_tab.

mtab_form_names = lc_tab.

* Always have the most recent LOOP AT table as the first entry in the
* queue
INSERT mtab_form_names INDEX 1.

ENDFORM.    " FORM ENQUEUE_FORM_NAME

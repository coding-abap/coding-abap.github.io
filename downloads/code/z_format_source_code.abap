*-----------------------------------------------------------------------
* This program takes an existing ABAP report, and does the following:
*    - Attempts to move comments to the end of the line.  Added because
*      pretty printer in 4.6 no longer does this
*    - Adds comments (table name) for the tables listed after a TABLES
*      statement if the line has not been commented already.
*    - Adds comments (field name) for data elements, parameters, and
*      select-options that are defined using the LIKE or FOR statement
*    - For ENDLOOP/ENDSELECT adds comment identify the LOOP/SELECT
*      that is being closed
*    - For FORM/ENDFORM adds comment identify the FORM that is being
*      closed
*    - Checks to ensure that the program being modified is either a
*      Local Private Object, or on a transport that belongs to the
*      person running this program.  This is to help prevent screwups.
*    - Calls function PRETTY_PRINTER to do the SAP standard pretty print
*      after the custom comments have been created
*-----------------------------------------------------------------------
REPORT  z_format_source_code          .

TABLES:
e071 , " Objects of a request or task (E070)
tadir , " Catalog of R/3 Repository objects
trdir ,         " System table TRDIR
dd02t .         " R/3-DD: SAP Table Texts

DATA: BEGIN OF mtab_old_prog OCCURS 0,
line(72) TYPE c,
END OF mtab_old_prog.

DATA: BEGIN OF mtab_new_prog OCCURS 0,
line(72) TYPE c,
END OF mtab_new_prog.

DATA: BEGIN OF mtab_statement OCCURS 0,
line(72) TYPE c,
END OF mtab_statement.

DATA:
* Hold an entire statement, even if it spans multiple lines
BEGIN OF mtab_long_line OCCURS 0,
start       TYPE i,
end         TYPE i,
code(5000)  TYPE c,
END OF mtab_long_line.

DATA: BEGIN OF mtab_tabname OCCURS 0,
tabname LIKE dd02t-tabname,    " Table name
tabdesc LIKE dd02t-ddtext, " Short text describing ABAP/4 Dictio
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

PARAMETERS:
p_report LIKE sy-repid,              " Program: Name of ABAP/4 program
p_test   AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.
PERFORM get_program_code TABLES mtab_old_prog
USING  p_report.

END-OF-SELECTION.
PERFORM create_condensed_table TABLES mtab_old_prog
mtab_long_line.

PERFORM format_program.

CALL FUNCTION 'PRETTY_PRINTER'
EXPORTING
inctoo             = space
TABLES
ntext              = mtab_new_prog
otext              = mtab_new_prog
EXCEPTIONS
enqueue_table_full = 1
include_enqueued   = 2
include_readerror  = 3
include_writeerror = 4
OTHERS             = 5.

IF p_test = space.
PERFORM save_program.
ENDIF.

LOOP AT mtab_new_prog.
IF mtab_new_prog = space.
SKIP 1.
ENDIF.
WRITE: / mtab_new_prog.
ENDLOOP.         " LOOP AT MTAB_NEW_PROG
*---------------------------------------------------------------------*
*       FORM GET_PROGRAM_CODE *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FTAB_OLD_PROG         *
*  -->  F_REPORT              *
*---------------------------------------------------------------------*
FORM get_program_code TABLES ftab_old_prog STRUCTURE mtab_old_prog
USING  f_report.

* Read the program code into an internal table
CALL FUNCTION 'RFC_READ_REPORT'
EXPORTING
program = f_report
TABLES
qtab    = ftab_old_prog
EXCEPTIONS
OTHERS  = 1.

IF sy-subrc NE 0.
WRITE: / 'Unable to read report:', f_report.
WRITE: / 'Exiting program'.
ENDIF.

ENDFORM.           " FORM GET_PROGRAM_CODE
*---------------------------------------------------------------------*
*       FORM CREATE_CONDENSED_TABLE               *
*---------------------------------------------------------------------*
*       Create a table that has all statements condensed onto 1 line  *
*---------------------------------------------------------------------*
*  -->  FTAB_OLD_PROG         *
*  -->  FTAB_long_line        *
*---------------------------------------------------------------------*
FORM create_condensed_table
TABLES ftab_old_prog STRUCTURE mtab_old_prog
ftab_long_line STRUCTURE mtab_long_line.

DATA:
* Structure to hold program code/comment
BEGIN OF fstr_line,
code(72)    TYPE c,              " Program Code
comment(72) TYPE c,              " Inline comments
END OF fstr_line.

LOOP AT ftab_old_prog.

IF ftab_long_line-start = 0.
ftab_long_line-start = ftab_long_line-end + 1.
CLEAR ftab_long_line-end.
ENDIF.

*   Strip off any inline comments so they do not get in the way
*   If comments are not separated, then words in the comments could
*   look like keywords, and cause problems
SPLIT ftab_old_prog-line AT '"' INTO fstr_line-code
fstr_line-comment.

*   Align all statements to be left justified
SHIFT fstr_line-code LEFT DELETING LEADING space.

*   Put all lines that make up a single statement into one field
*   This will make it easier to isolate key words.  For example, if you
*   want to process a TABLES statement, but exclude the TABLES part of a
*   function call, or a subroutine call.
CONCATENATE ftab_long_line-code
fstr_line-code
INTO ftab_long_line-code SEPARATED BY space.

IF fstr_line-code   CA '.'   OR    " Period means end of statement
fstr_line-code(1) = '*' OR      " Comment Line
fstr_line-code   CO space.      " Blank Line
*     Keep track of the table index that the statement ends on
ftab_long_line-end = sy-tabix.
*     Remove delimiter from concatenation of fields
SHIFT ftab_long_line-code LEFT BY 1 PLACES.
APPEND ftab_long_line.
CLEAR: ftab_long_line-code,
ftab_long_line-start.
*     Don't clear out fstr_long_line-end yet.  It is used to calc
*     fstr_long_line-start.
ENDIF.

ENDLOOP.         " LOOP AT FTAB_OLD_PROG
ENDFORM.           " FORM CREATE_CONDENSED_TABLE
*---------------------------------------------------------------------*
*       FORM FORMAT_PROGRAM   *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
FORM format_program.

DATA: lstr_old_prog LIKE mtab_old_prog.

LOOP AT mtab_long_line.
TRANSLATE mtab_long_line-code TO UPPER CASE.

IF mtab_long_line-code(1) EQ '*'.  " Do not modify Comment Lines
LOOP AT mtab_old_prog FROM mtab_long_line-start
TO mtab_long_line-end.

mtab_new_prog-line = mtab_old_prog-line.
APPEND mtab_new_prog.
ENDLOOP.     " LOOP AT MTAB_OLD_PROG
ELSEIF mtab_long_line-code(6) EQ 'TABLES'.
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
lstr_old_prog = mtab_old_prog.
CONDENSE lstr_old_prog.
TRANSLATE lstr_old_prog TO UPPER CASE.

IF lstr_old_prog-line CS '"'.  " Comments
mtab_new_prog-line = mtab_old_prog-line.
ELSE.
IF lstr_old_prog-line CS ' LIKE ' OR
lstr_old_prog-line CS ' FOR '.
PERFORM get_for_like_comment USING mtab_old_prog
CHANGING mtab_new_prog.
ELSEIF lstr_old_prog-line(7) = 'LOOP AT '.
*     save table name into a queue
PERFORM enqueue_itab_name USING mtab_long_line-code.
mtab_new_prog-line = mtab_old_prog-line.
ELSEIF lstr_old_prog-line(7) = 'ENDLOOP'.
*     get name off of queue and add it as a comment to the ENDLOOP line
PERFORM add_comment_to_endloop USING mtab_old_prog-line
CHANGING mtab_new_prog-line.
ELSEIF lstr_old_prog-line(7) EQ 'SELECT ' AND
lstr_old_prog-line(13) NE 'SELECT SINGLE'.
*     save table name into a queue
PERFORM enqueue_tab_name USING mtab_old_prog-line.
mtab_new_prog-line = mtab_old_prog-line.
ELSEIF lstr_old_prog-line(9) = 'ENDSELECT'.
*     get name off of queue and add it as a comment to the ENDSELECT
PERFORM add_comment_to_select USING mtab_old_prog-line
CHANGING mtab_new_prog-line.
ELSEIF lstr_old_prog-line(5) = 'FORM '.
*         save form name into a queue
PERFORM enqueue_form_name USING mtab_old_prog-line.
mtab_new_prog-line = mtab_old_prog-line.
ELSEIF lstr_old_prog-line(7) = 'ENDFORM'.
*         get name off of queue and add it as a comment to the ENDFORM
PERFORM add_comment_to_endform USING mtab_old_prog-line
CHANGING mtab_new_prog-line.
ELSE.    " Any other lines
mtab_new_prog-line = mtab_old_prog-line.
ENDIF.
ENDIF.

PERFORM format_comments CHANGING mtab_new_prog-line.

APPEND mtab_new_prog.
ENDLOOP.     " LOOP AT MTAB_OLD_PROG
ENDIF.
ENDLOOP.         " LOOP AT MTAB_LONG_LINE

ENDFORM.           " FORM CREATE_CONDENSED_TABLE
*---------------------------------------------------------------------*
*       FORM GET_TABLE_NAMES_FROM_STATEMENT       *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FTAB_TABNAME          *
*  -->  FC_STATEMENT          *
*---------------------------------------------------------------------*
FORM get_table_names_from_statement
TABLES ftab_tabname STRUCTURE mtab_tabname
USING  fc_statement.

CLEAR ftab_tabname.
REFRESH ftab_tabname.

REPLACE 'TABLES' WITH space INTO fc_statement.
TRANSLATE fc_statement USING '. '.   " Replace periods
TRANSLATE fc_statement USING ', '.   " Replace commas
TRANSLATE fc_statement USING ': '.   " Replace colons

CONDENSE fc_statement.               " Remove all extra spaces

SPLIT fc_statement AT space INTO TABLE ftab_tabname.

ENDFORM. " FORM GET_TABLE_NAMES_FROM_STATEMENT
*---------------------------------------------------------------------*
*       FORM GET_TABLE_DESCRIPTIONS               *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FTAB_TABNAME          *
*  -->  LOOP                  *
*  -->  AT*
*  -->  FTAB_TABNAME          *
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
ENDFORM.           " FORM GET_TABLE_DESCRIPTIONS
*---------------------------------------------------------------------*
*       FORM BUILD_NEW_TABLES_STATEMENT           *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FSTR_LONG_LINE        *
*---------------------------------------------------------------------*
FORM build_new_tables_statement
USING fstr_long_line LIKE mtab_long_line.

DATA: lc_sep(1)   TYPE c,
li_rows     TYPE i.

DESCRIBE TABLE mtab_tabname LINES li_rows.

mtab_new_prog-line = 'TABLES:'.
APPEND mtab_new_prog.

LOOP AT mtab_tabname.
IF sy-tabix = li_rows.
lc_sep = '.'.
ELSE.
lc_sep = ','.
ENDIF.

CONCATENATE '~~'
mtab_tabname-tabname
lc_sep
'"'
mtab_tabname-tabdesc
INTO mtab_new_prog SEPARATED BY space.

TRANSLATE mtab_new_prog USING '~ '.

APPEND mtab_new_prog.

ENDLOOP.         " LOOP AT MTAB_TABNAME

ENDFORM.           " FORM BUILD_NEW_TABLES_STATEMENT
*---------------------------------------------------------------------*
*       FORM GET_FOR/LIKE_COMMENT                 *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  F_OLD_PROG            *
*  -->  F_NEW_PROG            *
*---------------------------------------------------------------------*
FORM get_for_like_comment USING value(f_old_prog) LIKE mtab_old_prog
CHANGING f_new_prog.

DATA:
lc_dummy(1)    TYPE c,
lc_tabname(40) TYPE c,
ltab_nametab   LIKE dntab OCCURS 0 WITH HEADER LINE,
BEGIN OF lstr_field,
tabname      LIKE dd02t-tabname, " Table name
fldname      LIKE dd02t-tabname, " Table name
END OF lstr_field.

IF f_old_prog-line   CA '"' OR       " Line already commented
f_old_prog-line(1) = '*'.
f_new_prog = f_old_prog.
EXIT.
ELSEIF f_old_prog CS ' LIKE '.
SPLIT f_old_prog AT ' LIKE ' INTO lc_dummy
lc_tabname.
ELSEIF f_old_prog CS ' FOR '.
SPLIT f_old_prog AT ' FOR ' INTO lc_dummy
lc_tabname.
ELSE.
f_new_prog = f_old_prog.
EXIT.
ENDIF.

* If there is anything following the table-field in a LIKE or FOR clause
* it will be removed so that only the table-field remains
SPLIT lc_tabname AT space INTO lc_tabname
lc_dummy.

TRANSLATE lc_tabname USING '. '.     " Remove periods
TRANSLATE lc_tabname USING ', '.     " Remove commas
CONDENSE lc_tabname.                 " Remove extra white space

SPLIT lc_tabname AT '-' INTO lstr_field-tabname
lstr_field-fldname.

* The system variables are actually defined in DDIC structure SYST
IF lstr_field-tabname = 'SY'.
lstr_field-tabname = 'SYST'.
ENDIF.

CALL FUNCTION 'NAMETAB_GET'
EXPORTING
langu   = sy-langu
only    = ' '
tabname = lstr_field-tabname
TABLES
nametab = ltab_nametab
EXCEPTIONS
OTHERS  = 4.

READ TABLE ltab_nametab
WITH KEY tabname   = lstr_field-tabname
fieldname = lstr_field-fldname.

IF sy-subrc = 0.
CONCATENATE f_old_prog
'"'
ltab_nametab-fieldtext
INTO f_new_prog SEPARATED BY space.
ELSE.
f_new_prog = f_old_prog.
ENDIF.

ENDFORM.           " FORM BUILD_NEW_TABLES_STATEMENT
*---------------------------------------------------------------------*
*       FORM SAVE_PROGRAM     *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
FORM save_program.

DATA:
lc_saveok(1) TYPE c.

DATA:
lstr_message LIKE message.

* Check to see if the program is a local private object ($TMP)
SELECT SINGLE * FROM  tadir
WHERE  pgmid    = 'R3TR'
AND    object   = 'PROG'
AND    obj_name = p_report.

IF sy-subrc = 0.
IF tadir-devclass = '$TMP'.        " Local Private Object
lc_saveok = 'X'.
ELSE.
*     Check to see if the person trying to update the source is the same
*     one who last changed the program

SELECT SINGLE * FROM  trdir
WHERE  name = p_report.

IF sy-subrc = 0 AND
trdir-unam <> sy-uname.
lc_saveok = space.
MESSAGE ID 'ZZ' TYPE 'I' NUMBER 000
WITH 'You are not the user who last changed this program.'
'  Program changes not saved.'.
ELSE.
*     Check to see if the program is on a request.  If it is not, do not
*     allow it to be updated.  If the user cannot update the program,
*     then this program won't either.
SELECT SINGLE * FROM  e071
WHERE ( ( pgmid = 'R3TR' AND object = 'PROG' )   OR
( pgmid = 'LIMU' AND object = 'REPS' ) ) AND
obj_name = p_report.
IF sy-subrc = 0 AND e071-lockflag = 'X'.
lc_saveok = 'X'.
ELSE.
lc_saveok = space.
MESSAGE ID 'ZZ' TYPE 'I' NUMBER 000
WITH 'Program must be on a transport, or be a local'
'private object.  Program changes not saved.'.
ENDIF.
ENDIF.
ENDIF.
ELSE.
lc_saveok = space.
MESSAGE ID 'ZZ' TYPE 'I' NUMBER 000
WITH 'Program not saved'.
ENDIF.

CHECK lc_saveok = 'X'.

*-- Check to see if the program is locked
CALL FUNCTION 'ENQUEUE_ESRDIRE'
EXPORTING
mode_trdir     = 'X'  " Exclusive Lock
name           = p_report
EXCEPTIONS
foreign_lock   = 1
system_failure = 2
OTHERS         = 3.

CASE sy-subrc.
WHEN 0.         " OK
INSERT REPORT p_report FROM mtab_new_prog.
IF sy-subrc = 0.
FORMAT COLOR COL_POSITIVE.
WRITE: / 'Program now has fields commented.'.
ELSE.
FORMAT COLOR COL_NEGATIVE.
WRITE: / 'Error saving program'.
ENDIF.

CALL FUNCTION 'DEQUEUE_ESRDIRE'
EXPORTING
mode_trdir = 'E'
name       = p_report.
WHEN 1.        " Locked by another user
CALL FUNCTION 'WRITE_MESSAGE'
EXPORTING
msgid  = 'EU'
msgno  = '510'
msgty  = 'S'
msgv1  = sy-msgv1
msgv2  = sy-msgv2
msgv3  = sy-msgv3
msgv4  = sy-msgv4
msgv5  = space
IMPORTING
messg  = lstr_message
EXCEPTIONS
OTHERS = 1.
IF sy-subrc = 0.
FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
WRITE: / lstr_message-msgtx,
/ 'No changes made to program', p_report.
ENDIF.
WHEN 2.
CALL FUNCTION 'WRITE_MESSAGE'
EXPORTING
msgid  = 'EU'
msgno  = '510'
msgty  = 'S'
msgv1  = sy-msgv1
msgv2  = sy-msgv2
msgv3  = sy-msgv3
msgv4  = sy-msgv4
msgv5  = space
IMPORTING
messg  = lstr_message
EXCEPTIONS
OTHERS = 1.
IF sy-subrc = 0.
FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
WRITE: / lstr_message-msgtx,
/ 'No changes made to program', p_report.
ENDIF.
ENDCASE.

FORMAT COLOR COL_NORMAL.

SKIP 2.

ENDFORM.           " FORM SAVE_PROGRAM
*---------------------------------------------------------------------*
*       FORM ENQUEUE_ITAB_NAME*
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  F_LINE                *
*---------------------------------------------------------------------*
FORM enqueue_itab_name USING value(f_line) LIKE mtab_long_line-code.

DATA:
lc_dummy(1) TYPE c,
lc_itab(40) TYPE c.

TRANSLATE f_line TO UPPER CASE.

SPLIT f_line AT 'LOOP AT ' INTO lc_dummy
lc_itab.

SPLIT lc_itab AT space INTO lc_itab
lc_dummy.

TRANSLATE lc_itab USING '. '.
CONDENSE lc_itab.

mtab_itab_names = lc_itab.

* Always have the most recent LOOP AT table as the first entry in the
* queue
INSERT mtab_itab_names INDEX 1.

ENDFORM.           " ENQUEUE_ITAB_NAME
*---------------------------------------------------------------------*
*       FORM ADD_COMMENT_TO_ENDLOOP               *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FSTR_LONG_LINE        *
*  -->  F_PROG_LINE           *
*---------------------------------------------------------------------*
FORM add_comment_to_endloop USING fstr_long_line LIKE mtab_old_prog-line
CHANGING f_prog_line.

IF mtab_old_prog-line NA '"'.        " No comments
*     Get the internal table from the queue
READ TABLE mtab_itab_names INDEX 1.
CONCATENATE mtab_old_prog-line
'"'
'LOOP AT'
mtab_itab_names-tabname
INTO f_prog_line SEPARATED BY space.
*     Dequeue the itab name
DELETE mtab_itab_names INDEX 1.
ELSE.
f_prog_line = mtab_old_prog-line.
ENDIF.
ENDFORM.           " FORM SAVE_PROGRAM
*---------------------------------------------------------------------*
*       FORM ENQUEUE_TAB_NAME *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  F_LINE                *
*---------------------------------------------------------------------*
FORM enqueue_tab_name USING  f_line LIKE mtab_old_prog-line.

DATA:
lc_dummy(1) TYPE c,
lc_tab(40) TYPE c.

TRANSLATE f_line TO UPPER CASE.

SPLIT f_line AT ' FROM ' INTO lc_dummy
lc_tab.

CONDENSE lc_tab. " Remove leading/trailing extra spaces

SPLIT lc_tab AT space INTO lc_tab
lc_dummy.

TRANSLATE lc_tab USING '. '.
CONDENSE lc_tab.

mtab_tab_names = lc_tab.

* Always have the most recent LOOP AT table as the first entry in the
* queue
INSERT mtab_tab_names INDEX 1.

ENDFORM.           " FORM BUILD_NEW_TABLES_STATEMENT
*---------------------------------------------------------------------*
*       FORM ADD_COMMENT_TO_SELECT                *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FSTR_LONG_LINE        *
*  -->  F_PROG_LINE           *
*---------------------------------------------------------------------*
FORM add_comment_to_select USING fstr_long_line LIKE mtab_old_prog-line
CHANGING f_prog_line.

IF mtab_old_prog-line NA '"'.        " No comments
*     Get the table from the queue
READ TABLE mtab_tab_names INDEX 1.
CONCATENATE mtab_old_prog-line
'"'
'SELECT FROM'
mtab_tab_names-tabname
INTO f_prog_line SEPARATED BY space.
*     Dequeue the tab name
DELETE mtab_tab_names INDEX 1.
ELSE.
f_prog_line = mtab_old_prog-line.
ENDIF.

ENDFORM.           " FORM SAVE_PROGRAM
*---------------------------------------------------------------------*
*       FORM ADD_COMMENT_TO_ENDFORM               *
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  FSTR_LONG_LINE        *
*  -->  F_NEW_PROG            *
*---------------------------------------------------------------------*
FORM add_comment_to_endform USING fstr_long_line LIKE mtab_old_prog-line
CHANGING f_prog_line.

IF mtab_old_prog-line NA '"'.        " No comments
*     Get the table from the queue
READ TABLE mtab_form_names INDEX 1.
CONCATENATE mtab_old_prog-line
'"'
'FORM'
mtab_form_names-tabname
INTO f_prog_line SEPARATED BY space.
*     Dequeue the form name
DELETE mtab_form_names INDEX 1.
ELSE.
f_prog_line = mtab_old_prog-line.
ENDIF.

ENDFORM.           " FORM SAVE_PROGRAM

*---------------------------------------------------------------------*
*       FORM ENQUEUE_FORM_NAME*
*---------------------------------------------------------------------*
*       ........              *
*---------------------------------------------------------------------*
*  -->  F_LINE                *
*---------------------------------------------------------------------*
FORM enqueue_form_name USING f_line.

DATA:
lc_dummy(1) TYPE c,
lc_tab(40) TYPE c.

TRANSLATE f_line TO UPPER CASE.

SPLIT f_line AT 'FORM ' INTO lc_dummy
lc_tab.

CONDENSE lc_tab. " Remove leading/trailing extra spaces

SPLIT lc_tab AT space INTO lc_tab
lc_dummy.

TRANSLATE lc_tab USING '. '.
CONDENSE lc_tab.

mtab_form_names = lc_tab.

* Always have the most recent LOOP AT table as the first entry in the
* queue
INSERT mtab_form_names INDEX 1.

ENDFORM.           " FORM ENQUEUE_FORM_NAME

*---------------------------------------------------------------------*
*       FORM format_comments  *
*---------------------------------------------------------------------*
*       This routine has not been well tested, so there may be        *
*       some unanticipated features present!      *
*---------------------------------------------------------------------*
*  -->  F_PROGLINE            *
*---------------------------------------------------------------------*
FORM format_comments CHANGING f_progline LIKE mtab_new_prog-line.

DATA: li_pos      TYPE i,
li_code_len TYPE i, " Length of the code
li_comm_len TYPE i, " length of the comment
lc_line     LIKE mtab_new_prog-line,
lc_comment  LIKE mtab_new_prog-line.

*-- Make sure there is an inline comment on the line
CHECK f_progline CS '"'.

*-- Separate program code from comments
SPLIT f_progline AT '"' INTO lc_line
lc_comment.

CONCATENATE '"'
lc_comment
INTO lc_comment SEPARATED BY space.

f_progline = lc_line.

li_code_len = strlen( lc_line ).
li_comm_len = strlen( lc_comment ).

li_pos = 70 - li_code_len - li_comm_len.

IF li_pos GE 45 AND
li_pos GT li_code_len.
f_progline+li_pos = lc_comment.
ENDIF.

ENDFORM." format_comments

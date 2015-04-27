---
title: Dynamic Programming
author: diesire
date: 2015-04-23
template: article.jade
tags: [utils, dynamic programming, open SQL, security, release 7.0]
---

Reading [SAP docs](http://help.sap.com/abapdocu_740/en/abennews-710-others.htm#!ABAP_MODIFICATION_9@9@),  I've found `CL_ABAP_DYN_PRG`, an utility class for dynamic programming.

---
# Class `CL_ABAP_DYN_PRG`

Method Name                   | Description
:-----------------            |:--------------
CHECK_INT_VALUE               | Check whether the input is a valid integer (optional sign)
ESCAPE_QUOTES                 | Escape single quotes
ESCAPE_QUOTES_STR             | Escape backquotes
QUOTE                         | Put single quotes around the value and escape single quotes
QUOTE_STR                     | Put backquotes around the value and escape backquotes
CHECK_COLUMN_NAME             | Check whether the input can be a column name
CHECK_VARIABLE_NAME           | Check whether the input can be a variable name
CHECK_TABLE_NAME_STR          | Check whether the input is a DB table name, check package
CHECK_TABLE_NAME_TAB          | Check whether the input is a DB table name, check package
CHECK_TABLE_OR_VIEW_NAME_STR  | Check whether input is DB table or view name, check package
CHECK_WHITELIST_STR           | Check whether the input is in a whitelist (string version)
CHECK_WHITELIST_TAB           | Check whether the input is in a whitelist (table version)
CHECK_TABLE_OR_VIEW_NAME_TAB  | Check whether input is DB table or view name, check package
ESCAPE_XSS_XML_HTML           | Escape XML/HTML string for XSS safety
ESCAPE_XSS_JAVASCRIPT         | Escape Javascript string for XSS safety
ESCAPE_XSS_CSS                | Escape Cascading Style Sheets string for XSS safety
ESCAPE_XSS_URL                | Escape URL string for XSS safety
CHECK_CHAR_LITERAL            | Check whether the input is a valid type c literal
CHECK_STRING_LITERAL          | Check whether the input is a valid type string literal


##Handling of literal values in conditions

`ESCAPE_QUOTES*` and `QUOTE*` methods are useful to sanitize dynamic where clauses

Without proper input sanitization an SQL exception will raise

```abap
FORM sql_error.
  DATA:
    lv_where TYPE string VALUE `name1 = '$CITY'`. "Dynamic condition

  "Dirty input with invalid characters
  REPLACE '$CITY' IN lv_where WITH `O'Reilly`.
  "where clause: name1 = 'O'Reilly'

  TRY .
      SELECT COUNT(*)
      FROM t001w
      WHERE (lv_where).
    CATCH cx_root.
      WRITE: / 'Dirty Query - SQL Error'.
  ENDTRY.
ENDFORM.
```

> `Dirty Query - SQL Error`

Or worse, there is an SQL Injection open door

```abap
FORM sql_injection.
  DATA:
    lv_where TYPE string VALUE `name1 = '$CITY'`. "Dynamic condition

  "Forged query to exploit an SQL Injection
  REPLACE '$CITY' IN lv_where WITH `' OR name1 <> '`.
  "where clause: name1 = ' ' OR name1 <> ''

  TRY .
      SELECT COUNT(*)
      FROM t001w
      WHERE (lv_where).

      WRITE: / 'SQL Injection. Number of entries = ' , sy-dbcnt.
    CATCH cx_root.
      "...
  ENDTRY.
ENDFORM.
```

> `SQL Injection. Number of entries = 127`

This is really dangerous, we could be exposing sensible data. Input sanitization is mandatory


```abap
FORM sql_sanitized.
  DATA:
    lv_where TYPE string VALUE `name1 = '$CITY'`. "Dynamic condition

  "Sanitize input
  REPLACE '$CITY' IN lv_where WITH cl_abap_dyn_prg=>escape_quotes( `O'Reilly` ).
  "where clause: name1 = 'O''Reilly'

  TRY .
      SELECT COUNT(*)
      FROM t001w
      WHERE (lv_where).

      WRITE: / 'Sanitized Query - OK. Number of entries = ' , sy-dbcnt.
    CATCH cx_root.
      "...
  ENDTRY.
ENDFORM.
```

> `Sanitized Query - OK. Number of entries = 0`

As expected, we have no results

##Handling of column and variable names

`CHECK_COLUMN*`, `CHECK_VARIABLE*` and `CHECK_TABLE*` methods are used to check
identifiers generated at runtime.

##Other

`ESCAPE_XSS*` methods are mandatory to avoid **XSS injection** in external resources

`CHECK_INT*`, `CHECK_CHAR*` and `CHECK_STRING*` are general purpose methods useful in some use cases

### More info

* _Class Documentation or_ [Consolut - CL_ABAP_DYN_PRG](https://www.consolut.com/en/s/sap-ides-access/d/s/doc/N-CL_ABAP_DYN_PRG)

 
### Source

* _Further Changes in 7.0, EhP2_ [SAP docs](http://help.sap.com/abapdocu_740/en/abennews-710-others.htm#!ABAP_MODIFICATION_9@9@)
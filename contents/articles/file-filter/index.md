---
title: File Filter
author: diesire
date: 2015-01-14
template: article.jade
tags: [file, gui]
---

# asdasdasd

### sdfsdf


Do you remember the exact string to set a file filter in ABAP file chooser? I don't, but class `cl_gui_frontend_services` has some nice predefined class atributes

---

```javascript
var readFile = Promise.denodeify(require('fs').readFile);
// now `readFile` will return a promise rather than expecting a callback

function readJSON(filename, callback){
  // If a callback is provided, call it with error as the first argument
  // and result as the second argument, then return `undefined`.
  // If no callback is provided, just return the promise.
  return readFile(filename, 'utf8').then(JSON.parse).nodeify(callback);
}
```


```abap
FILETYPE_ALL = 'All Files (*.*)|*.*|'
FILETYPE_EXCEL = 'Microsoft Excel Files (*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|'
FILETYPE_WORD = 'Microsoft Word Files (*.DOC;*.DOCX;*.DOCM)|*.DOC;*.DOCX;*.DOCM|'
FILETYPE_TEXT = 'Text Files (*.TXT)|*.TXT|'
FILETYPE_HTML = 'HTML files (*.HTML, *.HTM)|*.HTML;*.HTM|'
FILETYPE_RTF = 'RTF Files (*.RTF)|*.RTF|'
FILETYPE_XML = 'XML (*.XML)|*.XML|'
FILETYPE_POWERPOINT = 'Microsoft PowerPoint Files (*PPT;*.PPTX;*.PPTM)|*PPT;*.PPTX;*.PPTM|'
```

Usage example

```abap
cl_gui_frontend_services=>file_open_dialog(
  EXPORTING
    file_filter             =  cl_gui_frontend_services=>FILETYPE_EXCEL
    multiselection          =  space
  CHANGING
    file_table              =  lt_files
    rc                      =  lv_subrc
    user_action             =  lv_user_action
  EXCEPTIONS
    file_open_dialog_failed = 1
    cntl_error              = 2
    error_no_gui            = 3
    not_supported_by_gui    = 4
    OTHERS                  = 5 ).
```

```abap
*----------------------------------------------------------------------*
*  Program Title       : Nicelabel Re-Print
*  Author              : Pablo Escalada Gómez
*  Date                : 02/19/2015
*  SAP Change Request #: ECDK937957
*----------------------------------------------------------------------*
*  Description:        : Nicelabel print program
*  Type:               : Report
*  Run Frequency:      : On request
*  Development Class:  : ZDEV
*  Requirement Ref.    : EN2389
*----------------------------------------------------------------------*

REPORT  zfwmr_fglabel MESSAGE-ID zfwm
  NO STANDARD PAGE HEADING
  LINE-SIZE  132
  LINE-COUNT 65.


*--------------------------------------------------------------------*
* INCLUDES
*--------------------------------------------------------------------*
INCLUDE zfwmr_fglabel_top01.
INCLUDE zfwmr_fglabel_sel01.
INCLUDE zfwmr_fglabel_cls01.


*--------------------------------------------------------------------*
* AT SELECTION-SCREEN
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pname.

  zcl_fwm_fglabel=>set_prntname_values(
    EXPORTING
      iv_werks   = p_werks
      iv_lpla    = p_lpla
      iv_plac    = p_plac
      iv_listbox = 'P_PNAME'
  ).


*--------------------------------------------------------------------*
* START-OF-SELECTION.
*--------------------------------------------------------------------*
START-OF-SELECTION.
  TRY .
      CREATE OBJECT go_lbl
        EXPORTING
          iv_werks = p_werks
          iv_lpla  = p_lpla
          iv_plac  = p_plac
          iv_pname = p_pname
          iv_qty   = p_qty
          ir_charg = so_charg[].

      go_lbl->print( ).
      go_lbl->get_messages( ).

    CATCH cx_operation INTO go_ex_operation.
      MESSAGE go_ex_operation TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
```

Extra point for supporting MS Office multiple file extensions

![Excel file filter in action](file_filter.png)

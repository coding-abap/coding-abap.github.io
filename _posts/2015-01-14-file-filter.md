---
layout: post
title: File Filter
category: tips
tags: [tip, file, gui]
---

## Tip

Do you remember the exact string to set a file filter in ABAP file chooser? I don't, but class `cl_gui_frontend_services` has some predefined class atributes

 * **FILETYPE_ALL**  `All Files (*.*)|*.*|`
 * **FILETYPE_EXCEL** `Microsoft Excel Files  (*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|`
 * **FILETYPE_WORD** `Microsoft Word Files (*.DOC;*.DOCX;*.DOCM)|*.DOC;*.DOCX;*.DOCM|`
 * **FILETYPE_TEXT** `Text Files (*.TXT)|*.TXT|`
 * **FILETYPE_HTML** `HTML files (*.HTML, *.HTM)|*.HTML;*.HTM|`
 * **FILETYPE_RTF** `RTF Files (*.RTF)|*.RTF|`
 * **FILETYPE_XML** `XML (*.XML)|*.XML|`
 * **FILETYPE_POWERPOINT** `Microsoft PowerPoint Files (*PPT;*.PPTX;*.PPTM)|*PPT;*.PPTX;*.PPTM|`


## Example

```abap
cl_gui_frontend_services=>file_open_dialog(
  EXPORTING
    file_filter             =  cl_gui_frontend_services=>filetype_excel
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

![Excel file filter in action](/assets/2015-01-14-file-filter/file_filter.png)


## More info
 * Class `cl_gui_frontend_services` at [SAP](https://www.google.com/search?as_q=cl_gui_frontend_services&as_sitesearch=help.sap.com)

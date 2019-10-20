REPORT zr_view_maintenance_no_dialog.

"""
* This program is a little example of use VIEW_MAINTENANCE_NO_DIALOG.
* It adds new line(s) in view without dialog (in my case in background mode).
"""

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS:
    banksrec TYPE banksrec DEFAULT 'AB',
    bankkrec TYPE bankkrec DEFAULT '111111111',
    banknrec TYPE banknrec DEFAULT '2222222222222'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  PARAMETERS:
    waers    TYPE waers DEFAULT 'USD',
    bankssnd TYPE bankssnd DEFAULT 'RU',
    bankksnd TYPE bankksnd DEFAULT '333333333',
    uzawe    TYPE uzawe DEFAULT '5M'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
  PARAMETERS:
*        CHAINNO      type  CHAINNO,
    chnbankt TYPE chainbankt DEFAULT '3',
    chnbanks TYPE chainbanks DEFAULT 'US',
    chnbankk TYPE chainbankk DEFAULT '444444444',
    chnbankn TYPE chainbankn DEFAULT '5555555555',
    iban     TYPE iban DEFAULT ''.
SELECTION-SCREEN END OF BLOCK b3.


DATA:
  lt_corr_keytab  TYPE STANDARD TABLE OF e071k,
  lt_dba_sellist  TYPE STANDARD TABLE OF vimsellist,
  lt_dpl_sellist  TYPE STANDARD TABLE OF vimsellist,
  lt_extract      TYPE STANDARD TABLE OF char255,
  lt_total        TYPE STANDARD TABLE OF char255,
  lt_x_header     TYPE STANDARD TABLE OF vimdesc,
  lt_x_namtab     TYPE STANDARD TABLE OF vimnamtab,
  lt_ix_to_modify TYPE STANDARD TABLE OF vimmodix.

DATA(lv_view_name) = CONV dd02v-tabname( 'V_TBCH41' ).

CALL FUNCTION 'VIEW_GET_DDIC_INFO'
  EXPORTING
    viewname        = lv_view_name
  TABLES
    sellist         = lt_dba_sellist
    x_header        = lt_x_header
    x_namtab        = lt_x_namtab
  EXCEPTIONS
    no_tvdir_entry  = 1
    table_not_found = 2
    OTHERS          = 3.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  RETURN.
ENDIF.

lt_dpl_sellist[] = lt_dba_sellist[].

TYPES BEGIN OF lty_s_view.
        INCLUDE TYPE v_tbch41.
TYPES flag TYPE c.
TYPES END OF lty_s_view.

DATA lt_data TYPE STANDARD TABLE OF lty_s_view WITH EMPTY KEY.

DATA(lt_dba) = lt_dba_sellist[].

LOOP AT lt_dba ASSIGNING FIELD-SYMBOL(<fs_dba>).
  CASE <fs_dba>-viewfield.
    WHEN 'MANDT'.
      <fs_dba>-value = sy-mandt.
    WHEN 'BANKSREC'.
      <fs_dba>-value = banksrec.
    WHEN 'BANKKREC'.
      <fs_dba>-value = bankkrec.
    WHEN 'BANKNREC'.
      <fs_dba>-value = banknrec.
    WHEN 'WAERS'.
      <fs_dba>-value = waers.
    WHEN 'BANKSSND'.
      <fs_dba>-value = bankssnd.
    WHEN 'BANKKSND'.
      <fs_dba>-value = bankksnd.
    WHEN 'UZAWE'.
      <fs_dba>-value = uzawe.
  ENDCASE.
ENDLOOP.

" Get existing items
CALL FUNCTION 'VIEW_GET_DATA'
  EXPORTING
    view_name              = lv_view_name
  TABLES
    dba_sellist            = lt_dba
    data                   = lt_data
    x_header               = lt_x_header
    x_namtab               = lt_x_namtab
  EXCEPTIONS
    no_viewmaint_tool      = 1
    no_authority           = 2
    no_auth_for_sel        = 3
    data_access_restricted = 4
    no_functiongroup       = 5
    OTHERS                 = 6.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

DATA(lv_last_line) = lines( lt_data[] ).

IF lv_last_line > 0.
  READ TABLE lt_data INTO DATA(ls_new_line) INDEX lv_last_line.
  IF sy-subrc = 0.
    " Add next item
    ls_new_line-chainno = ls_new_line-chainno + 1.
    ls_new_line-chainbankt = chnbankt.
    ls_new_line-chainbanks = chnbanks.
    ls_new_line-chainbankk = chnbankk.
    ls_new_line-chainbankn = chnbankn.
    ls_new_line-iban = iban.
    ls_new_line-flag = 'N'.
    APPEND ls_new_line TO lt_data.
  ENDIF.
ELSE.
  " Add first item
  lt_data = VALUE #(
    ( mandt = sy-mandt
      banksrec = bankkrec
      bankkrec = bankkrec
      banknrec = banknrec
      waers = waers
      bankssnd = bankssnd
      bankksnd = bankksnd
      uzawe = uzawe
      chainno = 1
      chainbankt = chnbankt
      chainbanks = chnbanks
      chainbankk = chnbankk
      chainbankn = chnbankn
      iban = iban
      flag = 'N' ) ).
ENDIF.

lt_extract[] = lt_total[] = lt_data[].

CALL FUNCTION 'VIEW_SET_CLUSTER_ACTIVE'
  EXPORTING
    vclname = lv_view_name.

DATA:
  lv_update_req TYPE c,
  ls_results    TYPE vimmodres.

" Insert new lines into our view
CALL FUNCTION 'VIEW_MAINTENANCE_NO_DIALOG'
  EXPORTING
    view_name           = lv_view_name
    action              = 'SAVE' "'EDIT' "
  IMPORTING
    update_required     = lv_update_req
    results             = ls_results
  TABLES
    corr_keytab         = lt_corr_keytab
    dba_sellist         = lt_dba_sellist
    dpl_sellist         = lt_dpl_sellist
    extract             = lt_extract
    total               = lt_total
    x_header            = lt_x_header
    x_namtab            = lt_x_namtab
    ix_to_modify        = lt_ix_to_modify
  EXCEPTIONS
    missing_corr_number = 1
    OTHERS              = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

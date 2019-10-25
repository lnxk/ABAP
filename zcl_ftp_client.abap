CLASS zcl_ftp_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF mty_s_params,
        host         TYPE text50,
        login        TYPE text50,
        passwd       TYPE text50,
        background   TYPE abap_bool,
        passive_mode TYPE abap_bool,
      END OF mty_s_params.

    METHODS:

      constructor
        IMPORTING is_params TYPE mty_s_params
        RAISING   zcx_ftp_client,

      run_command
        IMPORTING iv_command       TYPE text255
        RETURNING VALUE(rt_result) TYPE tttext255
        RAISING   zcx_ftp_client,

      upload_binary_file
        IMPORTING iv_name TYPE text255
                  it_blob TYPE solix_tab
        RAISING   zcx_ftp_client,

      upload_text_file
        IMPORTING iv_name TYPE text255
                  it_text TYPE tttext255
        RAISING   zcx_ftp_client,

      download_binary_file
        IMPORTING iv_name        TYPE text255
        RETURNING VALUE(rt_blob) TYPE solix_tab
        RAISING   zcx_ftp_client,

      download_text_file
        IMPORTING iv_name        TYPE text255
        RETURNING VALUE(rt_text) TYPE tttext255
        RAISING   zcx_ftp_client,

      is_exist_on_ftp
        IMPORTING iv_fullpath     TYPE string
                  iv_directory    TYPE abap_bool DEFAULT abap_false
        RETURNING VALUE(rv_exist) TYPE abap_bool,

      disconnect
        RAISING zcx_ftp_client.

  PRIVATE SECTION.

    CONSTANTS:
      mc_scramble_key TYPE i VALUE 26101957.

    DATA:
      mv_rfc_dest TYPE rfcdes-rfcdest,
      mv_handle   TYPE i.

    METHODS:

      scramble_password
        IMPORTING iv_passwd        TYPE text50
        RETURNING VALUE(rv_passwd) TYPE text50,

      get_table_length
        IMPORTING it_data       TYPE STANDARD TABLE
                  iv_line_size  TYPE i DEFAULT 255
        RETURNING VALUE(rv_len) TYPE i,

      get_path_and_name
        IMPORTING iv_fullpath TYPE string
        EXPORTING ev_path     TYPE string
                  ev_name     TYPE string.

ENDCLASS.



CLASS zcl_ftp_client IMPLEMENTATION.

  METHOD constructor.

    SET EXTENDED CHECK OFF.

    DATA(lv_passwd) = me->scramble_password( is_params-passwd ).

    me->mv_rfc_dest = COND #( WHEN is_params-background = abap_true THEN 'SAPFTPA' ELSE 'SAPFTP' ).

    " You should fill table SAPFTP_SERVERS to be able connect to FTP servers
    " Enter SAPFTP_SERVERS_V in transaction SM30 to do this
    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
        user            = is_params-login
        password        = lv_passwd
        host            = is_params-host
        rfc_destination = mv_rfc_dest
      IMPORTING
        handle          = me->mv_handle
      EXCEPTIONS
        not_connected   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = |FTP_CONNECT error|.
    ENDIF.

    IF is_params-passive_mode = abap_true.
      me->run_command( 'set passive on' ).
    ENDIF.

  ENDMETHOD.


  METHOD scramble_password.

    CLEAR: rv_passwd.

    DATA(lv_passlen) = strlen( iv_passwd ).
    DATA: lv_passwd TYPE text50.

    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        source      = iv_passwd
        sourcelen   = lv_passlen
        key         = me->mc_scramble_key
      IMPORTING
        destination = rv_passwd.

  ENDMETHOD.


  METHOD disconnect.

    CHECK me->mv_handle IS NOT INITIAL.

    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = me->mv_handle.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination          = me->mv_rfc_dest
      EXCEPTIONS
        destination_not_open = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = |RFC_CONNECTION_CLOSE error|.
    ENDIF.
  ENDMETHOD.


  METHOD download_binary_file.

    CLEAR: rt_blob.

    me->run_command( 'binary' ).

    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = me->mv_handle
        fname         = iv_name
      TABLES
        blob          = rt_blob
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = |FTP_SERVER_TO_R3 error, fname = { iv_name }|.
    ENDIF.
  ENDMETHOD.


  METHOD run_command.

    CLEAR: rt_result.

    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = me->mv_handle
        command       = iv_command
      TABLES
        data          = rt_result
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      DATA(lv_last_item) = lines( rt_result ).
      READ TABLE rt_result ASSIGNING FIELD-SYMBOL(<fs_result>) INDEX lv_last_item.
      IF sy-subrc = 0.
        DATA(lv_error) = CONV string( <fs_result> ).
      ELSE.
        lv_error = |FTP_COMMAND error, command = { iv_command }|.
      ENDIF.

      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = lv_error.
    ENDIF.

  ENDMETHOD.


  METHOD download_text_file.

    CLEAR: rt_text.

    me->run_command( 'ascii' ).

    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = me->mv_handle
        fname         = iv_name
      TABLES
        blob          = rt_text
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = |FTP_SERVER_TO_R3 error, fname = { iv_name }|.
    ENDIF.
  ENDMETHOD.


  METHOD upload_binary_file.

    DATA(lv_blob_length) = me->get_table_length( it_blob ).

    me->run_command( 'binary' ).

    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle        = me->mv_handle
        fname         = iv_name
        blob_length   = lv_blob_length
      TABLES
        blob          = it_blob
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = |FTP_R3_TO_SERVER error, fname = { iv_name }|.
    ENDIF.
  ENDMETHOD.


  METHOD upload_text_file.

    DATA(lv_text_length) = me->get_table_length( it_text ).

    me->run_command( 'ascii' ).

    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle         = me->mv_handle
        fname          = iv_name
        character_mode = abap_true
      TABLES
        text           = it_text
      EXCEPTIONS
        tcpip_error    = 1
        command_error  = 2
        data_error     = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_ftp_client
        EXPORTING
          textid     = zcx_ftp_client=>program_error
          desc_error = |FTP_R3_TO_SERVER error, fname = { iv_name }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_table_length.
    CLEAR: rv_len.
    DATA(lines) = lines( it_data ).
    rv_len = lines * iv_line_size.
  ENDMETHOD.


  METHOD is_exist_on_ftp.

    CLEAR: rv_exist.

    me->get_path_and_name(
      EXPORTING
        iv_fullpath = iv_fullpath
      IMPORTING
        ev_path     = DATA(lv_path)
        ev_name     = DATA(lv_name)
    ).

    " IF iv_directory = abap_true search for directory else search for file
    DATA(lv_regex) = COND string( WHEN iv_directory = abap_true THEN |.+<DIR>.+| ELSE |.+\\d+.+| ).
    lv_regex = |{ lv_regex }{ lv_name }|.

    TRY.
        DATA(lt_result) = me->run_command( |ls { lv_path }| ).

        IF cl_abap_matcher=>contains(
             pattern = lv_regex
             table = lt_result
           ).
          rv_exist = abap_true.
        ENDIF.
      CATCH zcx_ftp_client
            cx_sy_regex
            cx_sy_matcher.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD get_path_and_name.

    CLEAR: ev_path, ev_name.

    SPLIT iv_fullpath AT '/' INTO TABLE DATA(lt_split).

    DATA(lv_last_item) = lines( lt_split ).

    LOOP AT lt_split ASSIGNING FIELD-SYMBOL(<fs_split>) FROM 1 TO ( lv_last_item - 1 ).
      IF <fs_split> IS INITIAL.
        ev_path = |{ ev_path }/|.
      ELSE.
        ev_path = |{ ev_path }{ <fs_split> }/|.
      ENDIF.
    ENDLOOP.

    READ TABLE lt_split ASSIGNING <fs_split> INDEX lv_last_item.
    IF sy-subrc = 0.
      ev_name = <fs_split>.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

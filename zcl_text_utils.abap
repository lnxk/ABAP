CLASS zcl_text_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS read_text
      IMPORTING
        !iv_id         TYPE thead-tdid
        !iv_lang       TYPE thead-tdspras
        !iv_name       TYPE thead-tdname
        !iv_object     TYPE thead-tdobject
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_text_utils.

    METHODS read_text_caching
      IMPORTING
        !iv_id         TYPE thead-tdid
        !iv_lang       TYPE thead-tdspras
        !iv_name       TYPE thead-tdname
        !iv_object     TYPE thead-tdobject
      RETURNING
        VALUE(rv_text) TYPE string
      RAISING
        zcx_text_utils.

    METHODS clear_cache.

    CLASS-METHODS trim_extra_commas
      CHANGING
        !cv_str TYPE any.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF mty_s_text_cache,
        id     TYPE thead-tdid,
        lang   TYPE thead-tdspras,
        name   TYPE thead-tdname,
        object TYPE thead-tdobject,
        text   TYPE string,
      END OF mty_s_text_cache.

    DATA:
      mt_text_cache TYPE SORTED TABLE OF mty_s_text_cache WITH UNIQUE KEY id lang name object.

ENDCLASS.



CLASS zcl_text_utils IMPLEMENTATION.

  METHOD clear_cache.
    CLEAR: me->mt_text_cache.
  ENDMETHOD.


  METHOD read_text.

    DATA:
      lt_lines  TYPE tline_t,
      lt_stream TYPE STANDARD TABLE OF char255.

    CLEAR rv_text.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = iv_id
        language                = iv_lang
        name                    = iv_name
        object                  = iv_object
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_message).
      RAISE EXCEPTION TYPE zcx_text_utils
        EXPORTING
          textid     = zcx_text_utils=>zcx_system_error
          desc_error = lv_message.
    ELSE.
      CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
        TABLES
          itf_text    = lt_lines
          text_stream = lt_stream.

      LOOP AT lt_stream ASSIGNING FIELD-SYMBOL(<fs_stream>).
        CONCATENATE rv_text <fs_stream> INTO rv_text.
      ENDLOOP.

      CONDENSE rv_text.

    ENDIF.

  ENDMETHOD.


  METHOD read_text_caching.

    DATA:
      lt_lines  TYPE tline_t,
      lt_stream TYPE STANDARD TABLE OF char255.

    CLEAR rv_text.

    READ TABLE me->mt_text_cache
      ASSIGNING FIELD-SYMBOL(<fs_cache>)
      WITH KEY id = iv_id
               lang = iv_lang
               name = iv_name
               object = iv_object
               BINARY SEARCH.
    IF sy-subrc = 0.
      rv_text = <fs_cache>-text.
    ELSE.
      rv_text = me->read_text(
        iv_id = iv_id
        iv_lang = iv_lang
        iv_name = iv_name
        iv_object = iv_object
      ).

      INSERT VALUE #( id = iv_id
                      lang = iv_lang
                      name = iv_name
                      object = iv_object
                      text = rv_text ) INTO TABLE me->mt_text_cache.
    ENDIF.
  ENDMETHOD.


  METHOD trim_extra_commas.
    DATA(lv_str) = CONV string( cv_str ).
    REPLACE ALL OCCURRENCES OF REGEX '(^[\s,]*|[\s,]*$)' IN lv_str WITH ''.
    REPLACE ALL OCCURRENCES OF REGEX '(\s*,\s*){2,}' IN lv_str WITH `, `.
    cv_str = lv_str.
  ENDMETHOD.
ENDCLASS.


*=============

CLASS zcx_text_utils DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS zcx_system_error TYPE sotr_conc VALUE 'B62A30A31A021ED891F168A8E8358FC2' ##NO_TEXT.
    DATA desc_error TYPE string.

    METHODS constructor
      IMPORTING
        !textid     LIKE textid OPTIONAL
        !previous   LIKE previous OPTIONAL
        !desc_error TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_text_utils IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    me->desc_error = desc_error.
  ENDMETHOD.
ENDCLASS.

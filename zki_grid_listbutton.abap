*&---------------------------------------------------------------------*
*& Report  ZKI_GRID_LISTBUTTON
*&
*&---------------------------------------------------------------------*
*& ALV Grid with listbutton demo
*&---------------------------------------------------------------------*
REPORT zki_grid_listbutton.


CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS:

      handle_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_ucomm
            sender,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
            e_object
            e_interactive
            sender,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
            e_ucomm
            sender.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_sep ) TO e_object->mt_toolbar.

    APPEND VALUE #( function  = 'BUTTON1'
                    icon      = icon_abap
                    quickinfo = 'BUTTON1'
                    butn_type = cntb_btype_button
                    disabled  = abap_false
                    text      = 'BUTTON1' ) TO e_object->mt_toolbar.

    APPEND VALUE #( function  = 'BUTTON2'
                    icon      = icon_abap
                    quickinfo = 'BUTTON2'
                    butn_type = cntb_btype_button
                    disabled  = abap_false
                    text      = 'BUTTON2' ) TO e_object->mt_toolbar.

    APPEND VALUE #( function  = 'BUTTON3'
                    icon      = icon_abap
                    quickinfo = 'BUTTON3'
                    butn_type = cntb_btype_button
                    disabled  = abap_false
                    text      = 'BUTTON3' ) TO e_object->mt_toolbar.

    APPEND VALUE #( function  = 'LISTBUTTON'
                    icon      = icon_abap
                    quickinfo = 'LISTBUTTON'
                    butn_type = cntb_id_dropdown "cntb_btype_dropdown
                    disabled  = abap_false
                    text      = 'LISTBUTTON' ) TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_menu_button.

    CHECK e_object IS BOUND AND e_ucomm = 'LISTBUTTON'.

    e_object->add_function(
      fcode = 'ITEM1'
      text = 'ITEM1'
    ).

    e_object->add_function(
      fcode = 'ITEM2'
      text = 'ITEM2'
    ).

    e_object->add_function(
      fcode = 'ITEM3'
      text = 'ITEM3'
    ).

    e_object->add_function(
      fcode = 'ITEM4'
      text = 'ITEM4'
    ).

  ENDMETHOD.

  METHOD handle_user_command.
    MESSAGE |Pressed button with function: { e_ucomm }| TYPE 'I'.
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA:
    go_grid TYPE REF TO cl_gui_alv_grid,
    gt_tab  TYPE TABLE OF t001.

  SELECTION-SCREEN BEGIN OF SCREEN 1100.
  SELECTION-SCREEN END OF SCREEN 1100.

  SELECT * FROM t001 INTO CORRESPONDING FIELDS OF TABLE @gt_tab.

  go_grid = NEW #( i_parent = cl_gui_container=>screen0 ).

  go_grid->set_table_for_first_display(
    EXPORTING
      i_structure_name = 'T001'
    CHANGING
      it_outtab        = gt_tab ).

  DATA(lo_event_handler) = NEW lcl_event_handler( ).

  SET HANDLER:
    lo_event_handler->handle_menu_button  FOR go_grid,
    lo_event_handler->handle_toolbar      FOR go_grid,
    lo_event_handler->handle_user_command FOR go_grid.

  go_grid->set_toolbar_interactive( ).

  CALL SELECTION-SCREEN 1100.
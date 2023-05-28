CLASS zcl_abapgit_user_exit_client DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_exit_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS zif_abapgit_exit~create_http_client REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_user_exit_client IMPLEMENTATION.


  METHOD zif_abapgit_exit~create_http_client.

    DATA:
      lv_host        TYPE string,
      lv_destination TYPE rfcdest.

    lv_host = zcl_abapgit_url=>host( iv_url ).

*    IF lv_host CS 'gitlab'.
*      lv_destination = |GITLAB|.
    IF lv_host CS 'github'.
      IF iv_url CS 'marcfbe'.
        lv_destination = |GITHUB_MARCFBE|.
      ELSE.
        lv_destination = |GITHUB|.
      ENDIF.
    ENDIF.

    IF lv_destination IS INITIAL.
      RETURN.
    ENDIF.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = lv_destination
      IMPORTING
        client                   = ri_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100(  ).
    ENDIF.

    zcl_abapgit_login_manager=>clear( ).

  ENDMETHOD.
ENDCLASS.

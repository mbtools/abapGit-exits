CLASS zcl_abapgit_user_exit DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_exit.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_USER_EXIT IMPLEMENTATION.


  METHOD zif_abapgit_exit~allow_sap_objects.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_local_host.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_authentication.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_port.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_proxy_url.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_tadir.

  ENDMETHOD.


  METHOD zif_abapgit_exit~create_http_client.

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.
    DATA: lo_source        TYPE REF TO cl_oo_source,
          lv_begin         TYPE string,
          lv_end           TYPE string,
          lv_remove        TYPE abap_bool,
          lv_source        LIKE LINE OF rt_source,
          lv_obj_name      TYPE tadir-obj_name,
          li_package       TYPE REF TO zif_abapgit_sap_package,
          lt_super_package TYPE zif_abapgit_sap_package=>ty_devclass_tt,
          lv_package       TYPE devclass,
          lt_package_range TYPE RANGE OF devclass,
          ls_package_range LIKE LINE OF lt_package_range.

    ls_package_range-sign   = 'I'.
    ls_package_range-option = 'CP'.
    ls_package_range-low    = '$ABAPGIT*'.
    APPEND ls_package_range TO lt_package_range.

    lv_obj_name = is_class_key-clsname.

    lv_package = zcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = 'CLAS'
      iv_obj_name = lv_obj_name
    ).
    IF lv_package IS INITIAL.
      RETURN. " Probably INTF instead of CLAS
    ENDIF.

    li_package = zcl_abapgit_factory=>get_sap_package( lv_package ).
    lt_super_package = li_package->list_superpackages( ).

    LOOP AT lt_super_package TRANSPORTING NO FIELDS WHERE table_line IN lt_package_range.
      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      RETURN. " Normal serialization
    ENDIF.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_class_key
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error from CL_OO_SOURCE. Subrc = { sy-subrc }| ).
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).

    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT rt_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE rt_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

  ENDMETHOD.
ENDCLASS.

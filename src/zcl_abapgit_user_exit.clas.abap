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

    DATA:
       lv_host        TYPE string,
       lv_destination TYPE rfcdest.

    lv_host = zcl_abapgit_url=>host( iv_url ).

    IF lv_host CS 'gitlab'.
      lv_destination = |GITLAB|.
    ELSEIF lv_host CS 'github'.
      lv_destination = |GITHUB|.
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

  ENDMETHOD.


  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    DATA:
      lo_source        TYPE REF TO cl_oo_source,
      lv_begin         TYPE string,
      lv_end           TYPE string,
      lv_publ          TYPE string,
      lv_prot          TYPE string,
      lv_priv          TYPE string,
      lv_cmnt          TYPE string,
      lv_remove        TYPE abap_bool,
      lv_tabix         TYPE sy-tabix,
      lv_eof_def       TYPE sy-tabix,
      lt_source        LIKE rt_source,
      lt_source_temp   LIKE rt_source,
      lv_source        LIKE LINE OF rt_source,
      lt_interfaces    TYPE TABLE OF seoclsname,
      lv_method        TYPE string,
      ls_method        TYPE seocpdkey,
      lt_methods       TYPE seop_methods_w_include,
      lt_includes      TYPE TABLE OF programm,
      lv_obj_name      TYPE tadir-obj_name,
      li_package       TYPE REF TO zif_abapgit_sap_package,
      lt_super_package TYPE zif_abapgit_sap_package=>ty_devclass_tt,
      lv_package       TYPE devclass,
      lt_package_range TYPE RANGE OF devclass,
      ls_package_range LIKE LINE OF lt_package_range.

    FIELD-SYMBOLS:
      <source>    TYPE string,
      <method>    TYPE LINE OF seop_methods_w_include,
      <include>   TYPE programm,
      <interface> TYPE seoclsname.

***   Only for certain objects
**    lv_obj_name = is_class_key-clsname.
**
**    CHECK lv_obj_name = 'ZCL_TEST_METHOD_ORDER'.

*   Only for some packages / super packages
    ls_package_range-sign   = 'I'.
    ls_package_range-option = 'CP'.
    ls_package_range-low    = '$ABAPGIT*'.
    APPEND ls_package_range TO lt_package_range.
    ls_package_range-low    = '$MBT*'.
    APPEND ls_package_range TO lt_package_range.

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

*   Get existing source
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
    lt_source = lo_source->get_old_source( ).

*   Remove signatures and default comments
    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_publ = '*"* public components of class'.
    lv_prot = '*"* protected components of class'.
    lv_priv = '*"* private components of class'.
    lv_cmnt = '*"* do not include other source files here!!!'.

    lv_remove = abap_false.
    LOOP AT lt_source ASSIGNING <source>.
      IF <source> CS lv_publ OR <source> CS lv_prot OR <source> CS lv_priv OR <source> = lv_cmnt.
        CONTINUE.
      ENDIF.
      IF <source> = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_false.
        APPEND <source> TO lt_source_temp.
      ENDIF.
      IF <source> = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

**   Remove empty class sections
*    DO 3 TIMES.
*      CASE sy-index.
*        WHEN 1.
*          lv_source = 'PUBLIC SECTION.'.
*        WHEN 2.
*          lv_source = 'PROTECTED SECTION.'.
*        WHEN 3.
*          lv_source = 'PRIVATE SECTION.'.
*      ENDCASE.
*
*      FIND lv_source IN TABLE lt_source_temp
*        IGNORING CASE MATCH LINE lv_tabix.
*      IF sy-subrc = 0.
*        lv_tabix = lv_tabix + 1.
*      ELSE.
*        CONTINUE.
*      ENDIF.
*
**     If section is empty, then remove section completely
*      lv_remove = abap_true.
*      LOOP AT lt_source_temp ASSIGNING <source> FROM lv_tabix.
*        IF <source> CS 'SECTION.' OR <source> CS 'ENDCLASS.'.
*          EXIT.
*        ELSEIF <source> IS INITIAL.
*          CONTINUE.
*        ELSE.
*          lv_remove = abap_false.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
*      IF lv_remove = abap_true.
*        DELETE lt_source_temp WHERE table_line CS lv_source.
*      ENDIF.
*    ENDDO.

*   Change class identifier to lower case
    CLEAR lv_eof_def.
    LOOP AT lt_source_temp ASSIGNING <source>
      WHERE table_line CP 'CLASS * IMPLEMENTATION.' OR table_line CP 'CLASS * DEFINITION'.

      SPLIT <source> AT space INTO lv_begin <source> lv_end.
      TRANSLATE <source> TO LOWER CASE.
      CONCATENATE lv_begin <source> lv_end INTO <source> SEPARATED BY space.

      IF <source> CP 'CLASS * IMPLEMENTATION.'.
        lv_eof_def = sy-tabix.
      ENDIF.
    ENDLOOP.

*   Re-order method implementations
    CLEAR: rt_source.

    LOOP AT lt_source_temp ASSIGNING <source> TO lv_eof_def.
      INSERT <source> INTO TABLE rt_source.
    ENDLOOP.

    CALL METHOD cl_oo_classname_service=>get_all_method_includes
      EXPORTING
        clsname            = is_class_key-clsname
      RECEIVING
        result             = lt_methods
      EXCEPTIONS
        class_not_existing = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, class { is_class_key-clsname }| ).
    ENDIF.

    SELECT refclsname FROM seometarel INTO TABLE lt_interfaces
      WHERE clsname = is_class_key-clsname
      ORDER BY editorder.

    APPEND is_class_key-clsname TO lt_interfaces.

    LOOP AT lt_interfaces ASSIGNING <interface>.
      SELECT cmpname FROM seocompodf INTO lv_method
        WHERE clsname = <interface> AND editorder > 0 ORDER BY editorder.

        IF <interface> <> is_class_key-clsname.
          CONCATENATE <interface> '~' lv_method INTO lv_method.
        ENDIF.
        ls_method-clsname = is_class_key-clsname.
        ls_method-cpdname = lv_method.

        READ TABLE lt_methods ASSIGNING <method>
          WITH KEY cpdkey = ls_method.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, method { lv_method }| ).
        ENDIF.

        APPEND <method>-incname TO lt_includes.

      ENDSELECT.
    ENDLOOP.
*
    LOOP AT lt_includes ASSIGNING <include>.
      READ REPORT <include> INTO lt_source.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, include { <include> }| ).
      ENDIF.

      CLEAR lv_source.
      INSERT lv_source INTO TABLE rt_source.
      INSERT lv_source INTO TABLE rt_source.
      INSERT LINES OF lt_source INTO TABLE rt_source.
    ENDLOOP.

    lv_source = 'ENDCLASS.'.
    INSERT lv_source INTO TABLE rt_source.

  ENDMETHOD.


  METHOD zif_abapgit_exit~get_ssl_id.

  ENDMETHOD.


  METHOD zif_abapgit_exit~http_client.

  ENDMETHOD.
ENDCLASS.

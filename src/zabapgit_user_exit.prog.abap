CLASS zcl_abapgit_user_exit DEFINITION
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_exit.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_user_exit IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~ALLOW_SAP_OBJECTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ALLOWED                     TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~allow_sap_objects.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_LOCAL_HOST
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_HOSTS                       TYPE        TY_ICM_SINFO2_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_local_host.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_PROXY_AUTHENTICATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [<-->] CV_PROXY_AUTHENTICATION        TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_proxy_authentication.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_PROXY_PORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [<-->] CV_PROXY_PORT                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_proxy_port.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_PROXY_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [<-->] CV_PROXY_URL                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_proxy_url.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_TADIR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [<-->] CT_TADIR                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_tadir.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CREATE_HTTP_CLIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [<-()] RI_CLIENT                      TYPE REF TO IF_HTTP_CLIENT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CUSTOM_SERIALIZE_ABAP_CLIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_CLASS_KEY                   TYPE        SEOCLSKEY
* | [<-()] RT_SOURCE                      TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    CONSTANTS:
      c_version_active TYPE seoversion VALUE '1',
      c_clstype_if     TYPE seoclstype VALUE '1',
      c_clstype_cl     TYPE seoclstype VALUE '0',
      c_reltype_if     TYPE seoreltype VALUE '1',
      c_reltype_cl     TYPE seoreltype VALUE '2',
      c_cmptype_method TYPE seocmptype VALUE '1'.

    TYPES:
      BEGIN OF ty_inheritance,
        editorder TYPE seoorder,
        clsname   TYPE seoclsname,
      END OF ty_inheritance,
      BEGIN OF ty_clif,
        clsname   TYPE seoclsname,
        clstype   TYPE seoclass-clstype,
      END OF ty_clif.

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
      ls_inheritance   TYPE ty_inheritance,
      lt_inheritance   TYPE TABLE OF ty_inheritance,
      ls_clif          TYPE ty_clif,
      lt_clif          TYPE TABLE OF ty_clif,
      lv_method        TYPE string,
      ls_method_incl   TYPE seocpdkey,
      lt_method_incl   TYPE seop_methods_w_include,
      lt_includes      TYPE TABLE OF programm,
      lv_obj_name      TYPE tadir-obj_name,
      li_package       TYPE REF TO zif_abapgit_sap_package,
      lt_super_package TYPE zif_abapgit_sap_package=>ty_devclass_tt,
      lv_package       TYPE devclass,
      lt_package_range TYPE RANGE OF devclass,
      ls_package_range LIKE LINE OF lt_package_range.

    FIELD-SYMBOLS:
      <source>      TYPE string,
      <method_incl> TYPE LINE OF seop_methods_w_include,
      <include>     TYPE programm,
      <clif>        TYPE ty_clif.

    RETURN.  ">>>>>>>>>>>>>>>>>>>

*   Only for certain objects
    lv_obj_name = is_class_key-clsname.

**  CHECK lv_obj_name = 'ZCL_TEST_METHOD_ORDER'.

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

*   Mapping from methods to includes
    CALL METHOD cl_oo_classname_service=>get_all_method_includes
      EXPORTING
        clsname            = is_class_key-clsname
      RECEIVING
        result             = lt_method_incl
      EXCEPTIONS
        class_not_existing = 1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, class { is_class_key-clsname }| ).
    ENDIF.

*   Classes in order of inheritance
    ls_inheritance-editorder = 1.
    ls_inheritance-clsname = is_class_key-clsname.
    DO.
      APPEND ls_inheritance TO lt_inheritance.

      SELECT SINGLE refclsname FROM seometarel INTO ls_inheritance-clsname
        WHERE clsname = ls_inheritance-clsname
          AND version = c_version_active
          AND reltype = c_reltype_cl.
      IF sy-subrc = 0.
        ADD 1 TO ls_inheritance-editorder.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    SORT lt_inheritance BY editorder DESCENDING.

*   Interfaces per class in edit order
    CLEAR lt_clif.
    LOOP AT lt_inheritance INTO ls_inheritance.

      SELECT a~refclsname b~clstype APPENDING TABLE lt_clif
        FROM seometarel AS a JOIN seoclass AS b
        ON a~refclsname = b~clsname
        WHERE a~clsname = ls_inheritance-clsname
          AND a~version = c_version_active
          AND a~reltype = c_reltype_if
        ORDER BY editorder.

    ENDLOOP.

*   Classes in order of inheritance (root first)
    LOOP AT lt_inheritance INTO ls_inheritance.

      ls_clif-clsname = ls_inheritance-clsname.
      ls_clif-clstype = c_clstype_cl.
      APPEND ls_clif TO lt_clif.

    ENDLOOP.

*   Methods per interface in edit order
    CLEAR lt_includes.
    LOOP AT lt_clif ASSIGNING <clif>.

      SELECT a~cmpname INTO lv_method
        FROM seocompo AS a JOIN seocompodf AS b
        ON a~clsname = b~clsname AND a~cmpname = b~cmpname
        WHERE a~clsname = <clif>
          AND a~cmptype = c_cmptype_method
          AND b~alias   = abap_false
        ORDER BY b~editorder.

*       Constructor is not inherited
        IF lv_method = 'CONSTRUCTOR' AND <clif>-clsname <> is_class_key-clsname.
          CONTINUE.
        ENDIF.

*       Inherited class or interface?
        IF <clif>-clstype = c_clstype_if.
          CONCATENATE <clif>-clsname '~' lv_method INTO lv_method.
        ENDIF.

*       Is there an implementation?
        ls_method_incl-clsname = is_class_key-clsname.
        ls_method_incl-cpdname = lv_method.

        READ TABLE lt_method_incl ASSIGNING <method_incl>
          WITH KEY cpdkey = ls_method_incl.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        APPEND <method_incl>-incname TO lt_includes.

      ENDSELECT.

    ENDLOOP.

    IF lines( lt_method_incl ) <> lines( lt_includes ).
      zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, some methods not processed in class { lv_obj_name }| ).
    ENDIF.

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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~GET_SSL_ID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SSL_ID                      TYPE        SSFAPPLSSL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~get_ssl_id.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~HTTP_CLIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [--->] II_CLIENT                      TYPE REF TO IF_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~http_client.

  ENDMETHOD.
ENDCLASS.

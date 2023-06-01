CLASS zcl_abapgit_user_exit DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_exit.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS fix_types_indent
      IMPORTING
        !it_code       TYPE rswsourcet
      RETURNING
        VALUE(rt_code) TYPE rswsourcet.
ENDCLASS.



CLASS zcl_abapgit_user_exit IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_USER_EXIT->FIX_TYPES_INDENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_CODE                        TYPE        RSWSOURCET
* | [<-()] RT_CODE                        TYPE        RSWSOURCET
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fix_types_indent.

    DATA lv_fix_next TYPE abap_bool.
    DATA lv_types_indent TYPE sy-fdpos.

    FIELD-SYMBOLS <ls_code> TYPE any.

    rt_code = it_code.
    lv_fix_next = abap_false.
    LOOP AT rt_code ASSIGNING <ls_code>.
      IF <ls_code> CS 'TYPES:' AND <ls_code>(1) <> '*' AND lv_fix_next = abap_false.
        lv_types_indent = sy-fdpos.
      ENDIF.
      IF ( <ls_code> CS 'INCLUDE TYPE' OR <ls_code> CS 'INCLUDE STRUCTURE' ) AND lv_types_indent > 0.
        lv_fix_next = abap_true.
      ELSEIF lv_fix_next = abap_true.
        SHIFT <ls_code> LEFT DELETING LEADING space.
        SHIFT <ls_code> RIGHT BY lv_types_indent PLACES.
        CLEAR lv_fix_next.
      ELSE.
        CLEAR lv_fix_next.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~ADJUST_DISPLAY_COMMIT_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [--->] IV_REPO_NAME                   TYPE        CSEQUENCE
* | [--->] IV_REPO_KEY                    TYPE        CSEQUENCE
* | [--->] IV_COMMIT_HASH                 TYPE        ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_SHA1
* | [<-->] CV_DISPLAY_URL                 TYPE        CSEQUENCE
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~adjust_display_commit_url.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~ADJUST_DISPLAY_FILENAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILENAME                    TYPE        STRING
* | [<-()] RV_FILENAME                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~adjust_display_filename.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~ALLOW_SAP_OBJECTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ALLOWED                     TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~allow_sap_objects.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_LOCAL_HOST
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_HOSTS                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_local_host.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_PROXY_AUTHENTICATION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [<-->] CV_PROXY_AUTHENTICATION        TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_proxy_authentication.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_PROXY_PORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [<-->] CV_PROXY_PORT                  TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_proxy_port.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_PROXY_URL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REPO_URL                    TYPE        CSEQUENCE
* | [<-->] CV_PROXY_URL                   TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_proxy_url.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_SUPPORTED_DATA_OBJECTS
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_OBJECTS                     TYPE        ZIF_ABAPGIT_DATA_SUPPORTER=>TY_OBJECTS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_supported_data_objects.

    DATA ls_object LIKE LINE OF ct_objects.

    ls_object-type = 'TABU'.
    ls_object-name = 'RSADM*'.
    INSERT ls_object INTO TABLE ct_objects.
    ls_object-type = 'TABU'.
    ls_object-name = '/MBTOOLS/*'.
    INSERT ls_object INTO TABLE ct_objects.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_SUPPORTED_OBJECT_TYPES
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_TYPES                       TYPE        TY_OBJECT_TYPES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_supported_object_types.

    DATA lv_type LIKE LINE OF ct_types.

    " Support for apm
    lv_type = 'ZAPM'.
    INSERT lv_type INTO TABLE ct_types.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CHANGE_TADIR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [<-->] CT_TADIR                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_TADIR_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~change_tadir.
    RETURN.
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


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~CUSTOM_SERIALIZE_ABAP_CLIF
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_CLASS_KEY                   TYPE        SEOCLSKEY
* | [--->] IT_SOURCE                      TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT(optional)
* | [<-()] RT_SOURCE                      TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_STRING_TT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~custom_serialize_abap_clif.

    RETURN. ">>>>>

    DATA:
      ls_settings     TYPE rseumod,
      ls_settings_tmp TYPE rseumod,
      lt_code         TYPE rswsourcet,
      lt_code_pp      TYPE rswsourcet.

    " lowercase setting:
    " space = upper case
    " X     = lower case
    " G     = upper case keyword
    " L     = lower case keyword
    " A     = auto-detect (if supported)
    " indent setting:
    " 0 = no indent
    " 2 = with indent
    IF is_class_key-clsname CP '/MBTOOLS/*' AND is_class_key-clsname NS 'AJSON' AND is_class_key-clsname NS 'STRING_MAP'.
      CALL FUNCTION 'RS_WORKBENCH_CUSTOMIZING'
        EXPORTING
          suppress_dialog = abap_true
        IMPORTING
          setting         = ls_settings.

      ls_settings_tmp = ls_settings.
      ls_settings_tmp-lowercase = 'G'.
      ls_settings_tmp-indent    = 2.

      CALL FUNCTION 'RS_WORKBENCH_CUSTOMIZING'
        EXPORTING
          suppress_dialog = abap_true
          setting_import  = ls_settings_tmp.
    ELSE.
      RETURN.
    ENDIF.

    IF it_source IS NOT INITIAL.
      lt_code[] = it_source.

      CALL FUNCTION 'PRETTY_PRINTER'
        EXPORTING
          inctoo             = abap_false
        TABLES
          ntext              = lt_code_pp
          otext              = lt_code
        EXCEPTIONS
          enqueue_table_full = 1
          include_enqueued   = 2
          include_readerror  = 3
          include_writeerror = 4
          OTHERS             = 5.
      IF sy-subrc = 0.
        rt_source = fix_types_indent( lt_code_pp ).
      ENDIF.

      CALL FUNCTION 'RS_WORKBENCH_CUSTOMIZING'
        EXPORTING
          suppress_dialog = abap_true
          setting_import  = ls_settings.

      RETURN.
    ENDIF.

    " 1) Remove signatures and default comments
    " 2) Remove empty class sections
    " 3) Change class identifier to lower case
    " 4) Re-order method implementations
    CONSTANTS:
      c_options TYPE string VALUE '123'.

    CONSTANTS:
      c_publ           TYPE string VALUE '*"* public components of class',
      c_prot           TYPE string VALUE '*"* protected components of class',
      c_priv           TYPE string VALUE '*"* private components of class',
      c_cmnt           TYPE string VALUE '*"* do not include other source files here!!!',
      c_version_active TYPE seoversion VALUE '1',
      c_clstype_if     TYPE seoclstype VALUE '1',
      c_clstype_cl     TYPE seoclstype VALUE '0',
      c_reltype_if     TYPE seoreltype VALUE '1',
      c_reltype_cl     TYPE seoreltype VALUE '2',
      c_cmptype_method TYPE seocmptype VALUE '1',
      c_exposure_priv  TYPE seoexpose  VALUE '0'.

    TYPES:
      BEGIN OF ty_inheritance,
        editorder TYPE seoorder,
        clsname   TYPE seoclsname,
      END OF ty_inheritance,
      BEGIN OF ty_clif,
        clsname TYPE seoclsname,
        clstype TYPE seoclstype,
      END OF ty_clif,
      BEGIN OF ty_method,
        clsname  TYPE seoclsname,
        cpdname  TYPE seocpdname,
        exposure TYPE seoexpose,
      END OF ty_method.

    DATA:
      lv_clsname       TYPE seoclsname,
      lv_obj_name      TYPE tadir-obj_name,
      li_package       TYPE REF TO zif_abapgit_sap_package,
      lt_super_package TYPE zif_abapgit_sap_package=>ty_devclass_tt,
      lt_package_range TYPE RANGE OF devclass,
      ls_package_range LIKE LINE OF lt_package_range,
      lv_package       TYPE devclass,
      lo_source        TYPE REF TO cl_oo_source,
      lv_begin         TYPE string,
      lv_end           TYPE string,
      lv_remove        TYPE abap_bool,
      lv_tabix         TYPE sy-tabix,
      lv_tabix_section TYPE sy-tabix,
      lv_eof_def       TYPE sy-tabix,
      lt_source        LIKE rt_source,
      lt_source_temp   LIKE rt_source,
      lv_source        LIKE LINE OF rt_source,
      ls_inheritance   TYPE ty_inheritance,
      lt_inheritance   TYPE TABLE OF ty_inheritance,
      ls_clif          TYPE ty_clif,
      lt_clif          TYPE TABLE OF ty_clif,
      ls_alias         TYPE ty_method,
      ls_method        TYPE ty_method,
      lt_method        TYPE TABLE OF ty_method,
      lv_count_incl    TYPE i,
      ls_method_incl   TYPE seocpdkey,
      lt_method_incl   TYPE seop_methods_w_include.

    FIELD-SYMBOLS:
      <source>      TYPE string,
      <clif>        TYPE ty_clif,
      <method>      TYPE ty_method,
      <method_incl> TYPE LINE OF seop_methods_w_include.

    RETURN. ">>>>>

    IF c_options IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = is_class_key-clsname.

    " Only for some packages / super packages
    ls_package_range-sign   = 'I'.
    ls_package_range-option = 'EQ'.
    ls_package_range-low    = '$ABAPGIT'.
    APPEND ls_package_range TO lt_package_range.
    ls_package_range-option = 'CP'.
    ls_package_range-low    = '$ABAPGIT_*'.
    APPEND ls_package_range TO lt_package_range.
    ls_package_range-low    = '/MBTOOLS/*'.
    APPEND ls_package_range TO lt_package_range.

    lv_obj_name = is_class_key-clsname.

    lv_package = zcl_abapgit_factory=>get_tadir( )->get_object_package(
      iv_object   = 'CLAS'
      iv_obj_name = lv_obj_name ).

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

    " Get existing source
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

    " 1) Remove signatures and default comments
    IF c_options CA '1'.
      CONCATENATE '* <SIGNATURE>------------------------------------'
        '---------------------------------------------------+'
        INTO lv_begin.

      CONCATENATE '* +------------------------------------------------'
        '--------------------------------------</SIGNATURE>'
        INTO lv_end.

      lv_remove = abap_false.
      LOOP AT lt_source ASSIGNING <source>.
        IF <source> CS c_publ OR <source> CS c_prot OR <source> CS c_priv OR <source> = c_cmnt.
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

      rt_source = lt_source_temp.
    ENDIF.

    " 2) Remove empty class sections
    IF c_options CA '2'.
      lt_source_temp = rt_source.

      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_source = 'PUBLIC SECTION.'.
          WHEN 2.
            lv_source = 'PROTECTED SECTION.'.
          WHEN 3.
            lv_source = 'PRIVATE SECTION.'.
        ENDCASE.

        FIND lv_source IN TABLE lt_source_temp
          IGNORING CASE MATCH LINE lv_tabix.
        IF sy-subrc = 0.
          lv_tabix = lv_tabix + 1.
        ELSE.
          CONTINUE.
        ENDIF.

        " If section is empty, then remove section completely
        lv_remove = abap_true.
        LOOP AT lt_source_temp ASSIGNING <source> FROM lv_tabix.
          IF <source> CS 'SECTION.' OR <source> CS 'ENDCLASS.'.
            EXIT.
          ELSEIF <source> IS INITIAL.
            CONTINUE.
          ELSE.
            lv_remove = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_remove = abap_true.
          DELETE lt_source_temp WHERE table_line CS lv_source.
        ENDIF.
      ENDDO.

      rt_source = lt_source_temp.
    ENDIF.

    " 3) Change class identifier to lower case
    IF c_options CA '3'.
      lt_source_temp = rt_source.

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

      rt_source = lt_source_temp.
    ENDIF.

    " 4) Re-order method implementations
    IF c_options CA '4'.
      lt_source_temp = rt_source.

      CLEAR: rt_source.

      LOOP AT lt_source_temp ASSIGNING <source> TO lv_eof_def.
        INSERT <source> INTO TABLE rt_source.
      ENDLOOP.

      " Classes in order of inheritance
      ls_inheritance-editorder = 1.
      ls_inheritance-clsname = lv_clsname.
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

      " Interfaces per class in edit order
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

      " Classes in order of inheritance (root first)
      LOOP AT lt_inheritance INTO ls_inheritance.

        ls_clif-clsname = ls_inheritance-clsname.
        ls_clif-clstype = c_clstype_cl.
        APPEND ls_clif TO lt_clif.

      ENDLOOP.

      " Methods per interface in edit order
      CLEAR lt_method.
      LOOP AT lt_clif ASSIGNING <clif>.

        SELECT a~clsname a~cmpname b~exposure INTO ls_method
          FROM seocompo AS a JOIN seocompodf AS b
          ON a~clsname = b~clsname AND a~cmpname = b~cmpname
          WHERE a~clsname = <clif>-clsname
            AND a~cmptype = c_cmptype_method
            AND b~version = c_version_active
            AND b~alias   = abap_false
          ORDER BY b~editorder.

          " Check inheritance
          IF ls_method-clsname <> lv_clsname.
            " Ignore constructors
            IF ls_method-cpdname = 'CONSTRUCTOR' OR ls_method-cpdname = 'CLASS-CONSTRUCTOR'.
              CONTINUE.
            ENDIF.
            " Ignore private methods
            IF ls_method-exposure = c_exposure_priv.
              CONTINUE.
            ENDIF.
          ENDIF.

          " Get alias for method
          SELECT SINGLE clsname cmpname exposure FROM seocompodf INTO ls_alias
            WHERE clsname    = lv_clsname
              AND version    = c_version_active
              AND refclsname = ls_method-clsname
              AND refcmpname = ls_method-cpdname
              AND alias      = abap_true.
          IF sy-subrc = 0.
            APPEND ls_alias TO lt_method.
          ENDIF.

          " Compound method names for interfaces
          IF <clif>-clstype = c_clstype_if.
            CONCATENATE ls_method-clsname '~' ls_method-cpdname INTO ls_method-cpdname.
          ENDIF.

          ls_method-clsname = lv_clsname.
          APPEND ls_method TO lt_method.

        ENDSELECT.

      ENDLOOP.

      " Implementation per method in given order
      CALL METHOD cl_oo_classname_service=>get_all_method_includes
        EXPORTING
          clsname            = lv_clsname
        RECEIVING
          result             = lt_method_incl
        EXCEPTIONS
          class_not_existing = 1.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, class { lv_clsname }| ).
      ENDIF.

      CLEAR lv_count_incl.
      LOOP AT lt_method ASSIGNING <method>.

        " Get include name for method
        READ TABLE lt_method_incl ASSIGNING <method_incl>
          WITH KEY cpdkey-clsname = <method>-clsname cpdkey-cpdname = <method>-cpdname.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        " Get coding for include
        READ REPORT <method_incl>-incname INTO lt_source.
        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, include { <method_incl>-incname }| ).
        ENDIF.

        CLEAR lv_source.
        INSERT lv_source INTO TABLE rt_source.
        INSERT lv_source INTO TABLE rt_source.
        INSERT LINES OF lt_source INTO TABLE rt_source.

        ADD 1 TO lv_count_incl.
      ENDLOOP.

      IF lv_count_incl <> lines( lt_method_incl ).
        BREAK-POINT.
        zcx_abapgit_exception=>raise( |Error in CUSTOM_SERIALIZE_ABAP_CLIF, number of implementations | &&
          |{ lv_count_incl } vs. { lines( lt_method_incl ) }| ).
      ENDIF.

      lv_source = 'ENDCLASS.'.
      INSERT lv_source INTO TABLE rt_source.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~DESERIALIZE_POSTPROCESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_STEP                        TYPE        ZIF_ABAPGIT_OBJECTS=>TY_STEP_DATA
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~deserialize_postprocess.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~DETERMINE_TRANSPORT_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_REPO                        TYPE REF TO ZCL_ABAPGIT_REPO
* | [--->] IV_TRANSPORT_TYPE              TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_TRANSPORT_TYPE
* | [<-->] CV_TRANSPORT_REQUEST           TYPE        TRKORR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~determine_transport_request.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~ENHANCE_REPO_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_MENU                        TYPE REF TO ZCL_ABAPGIT_HTML_TOOLBAR
* | [--->] IV_KEY                         TYPE        ZIF_ABAPGIT_PERSISTENCE=>TY_VALUE
* | [--->] IV_ACT                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~enhance_repo_toolbar.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~GET_CI_TESTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_OBJECT                      TYPE        TADIR-OBJECT
* | [<-->] CT_CI_REPOS                    TYPE        TY_CI_REPOS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~get_ci_tests.

    " Return the name of repos and their clone URL
    " The name is used to define the package "$___<name>" linked to the abapGit repo

    " If you don't want to use the default https://github.com/abapGit-tests/{type} repo, clear the list
*    CLEAR ct_ci_repos.

    " Other Examples:
    CASE iv_object.
      WHEN 'TABL'.
        " Add some other repo from https://github.com/abapGit-tests
*        INSERT VALUE #(
*          name      = 'TABL_foreign_key'
*          clone_url = 'https://github.com/abapGit-tests/TABL_foreign_key' )
*          INTO TABLE ct_ci_repos.

        " Add your own repo works as well
*        INSERT VALUE #(
*          name      = 'MYTEST'
*          clone_url = 'https://github.com/mbtools/abapGit-test-del' )
*          INTO TABLE ct_ci_repos.
    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~GET_SSL_ID
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_SSL_ID                      TYPE        SSFAPPLSSL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~get_ssl_id.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~HTTP_CLIENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_URL                         TYPE        STRING
* | [--->] II_CLIENT                      TYPE REF TO IF_HTTP_CLIENT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~http_client.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~ON_EVENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] II_EVENT                       TYPE REF TO ZIF_ABAPGIT_GUI_EVENT
* | [<-()] RS_HANDLED                     TYPE        ZIF_ABAPGIT_GUI_EVENT_HANDLER=>TY_HANDLING_RESULT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~on_event.

*    IF rs_handled IS INITIAL.
*      rs_handled = zcl_abaplint_abapgit_ext_exit=>get_instance( )->on_event( ii_event ).
*    ENDIF.
*
*    IF rs_handled IS INITIAL.
*      rs_handled = zcl_markdown_abapgit_ext_exit=>get_instance( )->on_event( ii_event ).
*    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~PRE_CALCULATE_REPO_STATUS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REPO_META                   TYPE        ZIF_ABAPGIT_PERSISTENCE=>TY_REPO
* | [<-->] CT_LOCAL                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT
* | [<-->] CT_REMOTE                      TYPE        ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_FILES_TT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~pre_calculate_repo_status.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~SERIALIZE_POSTPROCESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [<-->] CT_FILES                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_FILES_ITEM_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~serialize_postprocess.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~VALIDATE_BEFORE_PUSH
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_COMMENT                     TYPE        ZIF_ABAPGIT_GIT_DEFINITIONS=>TY_COMMENT
* | [--->] IO_STAGE                       TYPE REF TO ZCL_ABAPGIT_STAGE
* | [--->] IO_REPO                        TYPE REF TO ZCL_ABAPGIT_REPO_ONLINE
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~validate_before_push.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~WALL_MESSAGE_LIST
* +-------------------------------------------------------------------------------------------------+
* | [--->] II_HTML                        TYPE REF TO ZIF_ABAPGIT_HTML
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~wall_message_list.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_USER_EXIT->ZIF_ABAPGIT_EXIT~WALL_MESSAGE_REPO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_REPO_META                   TYPE        ZIF_ABAPGIT_PERSISTENCE=>TY_REPO
* | [--->] II_HTML                        TYPE REF TO ZIF_ABAPGIT_HTML
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_exit~wall_message_repo.

*    zcl_abaplint_abapgit_ext_exit=>get_instance( )->wall_message_repo(
*      is_repo_meta = is_repo_meta
*      ii_html      = ii_html ).
*
*    zcl_markdown_abapgit_ext_exit=>get_instance( )->wall_message_repo(
*      is_repo_meta = is_repo_meta
*      ii_html      = ii_html ).

  ENDMETHOD.
ENDCLASS.

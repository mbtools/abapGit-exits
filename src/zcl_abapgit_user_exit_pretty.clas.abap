CLASS zcl_abapgit_user_exit_pretty DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_user_exit_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS zif_abapgit_user_exit~custom_serialize_abap_clif
      REDEFINITION.
    METHODS zif_abapgit_user_exit~pre_calculate_repo_status
      REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS fix_types_indent
      IMPORTING
        !it_code       TYPE rswsourcet
      RETURNING
        value(rt_code) TYPE rswsourcet.

    METHODS fix_source_code_file
      CHANGING
        !cv_data       TYPE xstring
      RAISING
        zcx_abapgit_exception.

    METHODS fix_source_code
      IMPORTING
        !iv_options    TYPE string
        !it_code       TYPE rswsourcet
      RETURNING
        value(rt_code) TYPE rswsourcet
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_USER_EXIT_PRETTY IMPLEMENTATION.


  METHOD fix_source_code.

    " iv_options:
    " 1) Remove signatures and default comments
    " 2) Remove empty class sections
    " 3) Change class identifier to lower case
    " 4) Re-order method implementations
    " 5) Remove comments before class definition
    " 6) Replace case of value()
    " 7) Remove comments after ENDMETHOD and ENDCLASS

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
      lt_source        LIKE rt_code,
      lt_source_temp   LIKE rt_code,
      lv_source        LIKE LINE OF rt_code,
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

    IF iv_options IS INITIAL.
      RETURN.
    ENDIF.

    lt_source = it_code.

    " 1) Remove signatures and default comments
    IF iv_options CA '1'.
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

      rt_code = lt_source_temp.
    ENDIF.

    " 2) Remove empty class sections
    IF iv_options CA '2'.
      lt_source_temp = rt_code.

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

      rt_code = lt_source_temp.
    ENDIF.

    " 3) Change class identifier to lower case
    IF iv_options CA '3'.
      lt_source_temp = rt_code.

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

      rt_code = lt_source_temp.
    ENDIF.

    " 4) Re-order method implementations
    IF iv_options CA '4'.
      lt_source_temp = rt_code.

      CLEAR: rt_code.

      LOOP AT lt_source_temp ASSIGNING <source> TO lv_eof_def.
        INSERT <source> INTO TABLE rt_code.
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
          zcx_abapgit_exception=>raise( |Error in FIX_SOURCE_CODE, include { <method_incl>-incname }| ).
        ENDIF.

        CLEAR lv_source.
        INSERT lv_source INTO TABLE rt_code.
        INSERT lv_source INTO TABLE rt_code.
        INSERT LINES OF lt_source INTO TABLE rt_code.

        ADD 1 TO lv_count_incl.
      ENDLOOP.

      IF lv_count_incl <> lines( lt_method_incl ).
        BREAK-POINT.
        zcx_abapgit_exception=>raise( |Error in FIX_SOURCE_CODE, number of implementations | &&
          |{ lv_count_incl } vs. { lines( lt_method_incl ) }| ).
      ENDIF.

      lv_source = 'ENDCLASS.'.
      INSERT lv_source INTO TABLE rt_code.

    ENDIF.

    " 5) Remove comments before class definition
    IF iv_options CA '5'.
      lt_source_temp = rt_code.

      CLEAR lv_eof_def.
      LOOP AT lt_source_temp ASSIGNING <source>.
        IF <source> CP '"!*' OR ( strlen( <source> ) > 1 AND <source>(1) = '*' ).
          DELETE lt_source_temp INDEX sy-tabix.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.

      rt_code = lt_source_temp.
    ENDIF.

    " 6) Replace case of value()
    IF iv_options CA '6'.
      lt_source_temp = rt_code.

      LOOP AT lt_source_temp ASSIGNING <source>
        WHERE table_line CS 'VALUE('.

        FIND REGEX '^\s*value(.+)' IN <source>.
        IF sy-subrc = 0.
          REPLACE 'value(' IN <source> WITH 'VALUE('.
        ENDIF.
      ENDLOOP.

      rt_code = lt_source_temp.
    ENDIF.

    " 7) Remove comments after ENDMETHOD and ENDCLASS
    IF iv_options CA '7'.
      lt_source_temp = rt_code.

      LOOP AT lt_source_temp ASSIGNING <source>
        WHERE table_line CP 'ENDMETHOD.*' OR table_line CP 'ENDCLASS.*'.

        FIND '"' IN <source>.
        IF sy-subrc = 0.
          <source> = <source>(sy-fdpos).
        ENDIF.
      ENDLOOP.

      rt_code = lt_source_temp.
    ENDIF.


  ENDMETHOD.


  METHOD fix_source_code_file.

    DATA:
      lv_code TYPE string,
      lt_code TYPE rswsourcet,
      lt_new  TYPE rswsourcet.

    lv_code = zcl_abapgit_convert=>xstring_to_string_utf8( cv_data ).

    SPLIT lv_code AT cl_abap_char_utilities=>newline INTO TABLE lt_code.

    lt_new = fix_source_code(
      iv_options = '123567'
      it_code    = lt_code ).

    CONCATENATE LINES OF lt_new INTO lv_code SEPARATED BY cl_abap_char_utilities=>newline.

    cv_data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_code ).

  ENDMETHOD.


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


  METHOD zif_abapgit_user_exit~custom_serialize_abap_clif.

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

  ENDMETHOD.


  METHOD zif_abapgit_user_exit~pre_calculate_repo_status.

*    IF is_repo_meta-url NS 'abapGit/abapGit'.
*      RETURN.
*    ENDIF.

    FIELD-SYMBOLS:
      <ls_local>  LIKE LINE OF ct_local,
      <ls_remote> LIKE LINE OF ct_remote,
      <ls_file>   LIKE <ls_local>-file.

    " Local class files
    LOOP AT ct_local ASSIGNING <ls_local> WHERE item-obj_type = 'CLAS' AND file-filename CP '*.clas.abap'.
      fix_source_code_file( CHANGING cv_data = <ls_local>-file-data ).
      <ls_local>-file-sha1 = zcl_abapgit_hash=>sha1_raw( <ls_local>-file-data ).
    ENDLOOP.

    " Remote class files
    LOOP AT ct_remote ASSIGNING <ls_remote> WHERE filename CP '*.clas.abap'.
      fix_source_code_file( CHANGING cv_data = <ls_remote>-data ).
      <ls_remote>-sha1 = zcl_abapgit_hash=>sha1_raw( <ls_remote>-data ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.

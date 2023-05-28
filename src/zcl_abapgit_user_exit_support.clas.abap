CLASS zcl_abapgit_user_exit_support DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_exit_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      zif_abapgit_exit~change_supported_data_objects REDEFINITION,
      zif_abapgit_exit~change_supported_object_types REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_user_exit_support IMPLEMENTATION.


  METHOD zif_abapgit_exit~change_supported_data_objects.

    DATA ls_object LIKE LINE OF ct_objects.

    ls_object-type = 'TABU'.
    ls_object-name = 'RSADM*'.
    INSERT ls_object INTO TABLE ct_objects.
    ls_object-type = 'TABU'.
    ls_object-name = '/MBTOOLS/*'.
    INSERT ls_object INTO TABLE ct_objects.

  ENDMETHOD.


  METHOD zif_abapgit_exit~change_supported_object_types.

    DATA lv_type LIKE LINE OF ct_types.

    " Support for apm
    lv_type = 'ZAPM'.
    INSERT lv_type INTO TABLE ct_types.

  ENDMETHOD.
ENDCLASS.

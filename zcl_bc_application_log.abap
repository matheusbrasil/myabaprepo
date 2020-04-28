**************************************************************************************
* Important Transactions
* The important transactions codes used for application log are as following:
* ·      SLG0 – Create a new Log Object and sub object
* ·      SLG1 – Display Application Log
* ·      SLG2 – Delete the Application Log
* Creation of object and sub object:
* For generating a custom application log we need to create a new log object and sub object.
* For this we use transaction SLG0.
* Go to transaction SLG0 and click on new entries.
* Then give the name of object and save.
* For any of the object we can create the sub object as well.
* Sub objects are simply further classifications of the
**************************************************************************************
CLASS zcl_bc_application_log DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mt_log_number TYPE bal_t_lgnm .
    METHODS constructor
      IMPORTING
        !im_log_object    TYPE balobj_d
        !im_log_subobject TYPE balsubobj
        !im_repid         TYPE sy-repid.
    METHODS add_message
      IMPORTING
        !im_msg TYPE bal_s_msg .
    METHODS save.
    METHODS display .
    METHODS get_mv_log_handle
      RETURNING
        VALUE(re_output) TYPE balloghndl .
    METHODS add_message_from_sy .
    METHODS add_message_from_bapiret2
      IMPORTING
        !im_bapiret2 TYPE bapiret2 .
    METHODS add_message_from_tab_bapiret2
      IMPORTING
        !it_table TYPE bapiret2_t .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA gv_log_object    TYPE balobj_d .
    DATA gv_log_subobject TYPE balsubobj .
    DATA gv_log_handle    TYPE balloghndl .
    DATA gv_repid         TYPE sy-repid.

    METHODS create.

ENDCLASS.



CLASS zcl_bc_application_log IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->ADD_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_MSG                         TYPE        BAL_S_MSG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg       = im_msg
        i_log_handle  = gv_log_handle
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
    IF sy-subrc <> 0.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->ADD_MESSAGE_FROM_BAPIRET2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_BAPIRET2                    TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message_from_bapiret2.

    DATA(ls_msg) = VALUE bal_s_msg( msgid = im_bapiret2-id
                                    msgno = im_bapiret2-number
                                    msgty = im_bapiret2-type
                                    msgv1 = im_bapiret2-message_v1
                                    msgv2 = im_bapiret2-message_v2
                                    msgv3 = im_bapiret2-message_v3
                                    msgv4 = im_bapiret2-message_v4 ).

    me->add_message( ls_msg ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->ADD_MESSAGE_FROM_SY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message_from_sy.

    DATA(ls_msg) = VALUE bal_s_msg( msgid = sy-msgid
                                    msgno = sy-msgno
                                    msgty = sy-msgty
                                    msgv1 = sy-msgv1
                                    msgv2 = sy-msgv2
                                    msgv3 = sy-msgv3
                                    msgv4 = sy-msgv4 ).

    me->add_message( ls_msg ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->ADD_MESSAGE_FROM_TAB_BAPIRET2
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_TABLE                       TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_message_from_tab_bapiret2.

    LOOP AT it_table INTO DATA(ls_line).

      me->add_message_from_bapiret2( im_bapiret2 = ls_line ).

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_LOG_OBJECT                  TYPE        BALOBJ_D
* | [--->] IV_LOG_SUBOBJECT               TYPE        BALSUBOBJ
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    me->gv_log_object    = im_log_object.
    me->gv_log_subobject = im_log_subobject.
    me->gv_repid         = im_repid.

    me->create( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->CREATE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_LOG_CREATE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = VALUE bal_s_log( object    = me->gv_log_object
                                                   subobject = me->gv_log_subobject
                                                   aluser    = sy-uname
                                                   alprog    = sy-repid )   " Log header data
      IMPORTING
        e_log_handle            = me->gv_log_handle    " Log handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.

*      RAISE EXCEPTION TYPE zcx_log_create_error
*        EXPORTING
*          textid = VALUE scx_t100key( attr1 = sy-msgv1
*                                      attr2 = sy-msgv2
*                                      attr3 = sy-msgv3
*                                      attr4 = sy-msgv4
*                                      msgid = sy-msgid
*                                      msgno = sy-msgno )
**         previous =
*        .

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->DISPLAY
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD display.

    DATA lt_log_handle TYPE bal_t_logh.

    IF gv_log_handle IS NOT INITIAL.

      APPEND gv_log_handle TO lt_log_handle.

      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_t_log_handle       = lt_log_handle     " Restrict display by log handle
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          no_data_available    = 3
          no_authority         = 4
          OTHERS               = 5.
      IF sy-subrc <> 0.
*   message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->GET_MV_LOG_HANDLE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_RESULT                      TYPE        BALLOGHNDL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_mv_log_handle.

    re_output = me->gv_log_handle.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BC_APPLICATION_LOG->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_LOG_SAVE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save.

    DATA lt_log_handle      TYPE bal_t_logh.

    APPEND gv_log_handle TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_t_log_handle   = lt_log_handle
      IMPORTING
        e_new_lognumbers = me->mt_log_number
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.

*      RAISE EXCEPTION TYPE zcx_log_save_error
*        EXPORTING
*          textid = VALUE scx_t100key( attr1 = sy-msgv1
*                                      attr2 = sy-msgv2
*                                      attr3 = sy-msgv3
*                                      attr4 = sy-msgv4
*                                      msgid = sy-msgid
*                                      msgno = sy-msgno )
**         previous =
*        .

    ENDIF.

  ENDMETHOD.
ENDCLASS.

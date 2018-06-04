CLASS zcl_cbo2_prtpay_process_email DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    METHODS run_send_email
      RETURNING
        VALUE(re_return) TYPE tt_bapiret2 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_payrq_h,
        req_guid        TYPE zprt_payrq_h-req_guid,
        req_guid_ref    TYPE zprt_payrq_h-req_guid_ref,
        req_date        TYPE zprt_payrq_h-req_date,
        req_time        TYPE zprt_payrq_h-req_time,
        req_status      TYPE zprt_payrq_h-req_status,
        req_status_date TYPE zprt_payrq_h-req_status_date,
        total_val_req   TYPE zprt_payrq_h-total_val_req,
        total_val_paid  TYPE zprt_payrq_h-total_val_paid,
        pay_typ         TYPE zprt_payrq_h-pay_typ,
        numb_id         TYPE zprt_payrq_h-numb_id,
        req_type        TYPE zprt_payrq_h-req_type,
      END OF ty_payrq_h .
    TYPES:
      tt_payrq_h TYPE STANDARD TABLE OF ty_payrq_h .
    TYPES:
      BEGIN OF ty_payrq_i,
        req_guid         TYPE zprt_payrq_i-req_guid,
        item_type        TYPE zprt_payrq_i-item_type,
        req_item_aggr    TYPE zprt_payrq_i-req_item_aggr,
        req_item         TYPE zprt_payrq_i-req_item,
        source_id        TYPE zprt_payrq_i-source_id,
        tr_id            TYPE zprt_payrq_i-tr_id,
        new_tr_id        TYPE zprt_payrq_i-new_tr_id,
        xblnr            TYPE zprt_payrq_i-xblnr,
        lpn              TYPE zprt_payrq_i-lpn,
        bukrs            TYPE zprt_payrq_i-bukrs,
        action           TYPE zprt_payrq_i-action,
        opbel            TYPE zprt_payrq_i-opbel,
        opupk            TYPE zprt_payrq_i-opupk,
        betrw            TYPE zprt_payrq_i-betrw,
        item_status      TYPE zprt_payrq_i-item_status,
        item_prev_status TYPE zprt_payrq_i-item_prev_status,
        item_status_date TYPE zprt_payrq_i-item_status_date,
        req_guid_ref     TYPE zprt_payrq_i-req_guid_ref,
        doc_anul         TYPE zprt_payrq_i-doc_anul,
        doc_comp         TYPE zprt_payrq_i-doc_comp,
        doc_new          TYPE zprt_payrq_i-doc_new,
        doc_comp_new     TYPE zprt_payrq_i-doc_comp_new,
        doc_xblnr        TYPE dfkkop-xblnr,
      END OF ty_payrq_i .
    TYPES:
      tt_payrq_i TYPE STANDARD TABLE OF ty_payrq_i .
    TYPES:
      BEGIN OF ty_dfkkop,
        opbel    TYPE dfkkop-opbel,
        opupw    TYPE dfkkop-opupw,
        opupk    TYPE dfkkop-opupk,
        opupz    TYPE dfkkop-opupz,
        bukrs    TYPE dfkkop-bukrs,
        gpart    TYPE dfkkop-gpart,
        vkont    TYPE dfkkop-vkont,
        xblnr    TYPE dfkkop-xblnr,
        augbl    TYPE dfkkop-augbl,
        doc_data TYPE dfkkcoh-data1,
      END OF ty_dfkkop .
    TYPES:
      tt_dfkkop TYPE STANDARD TABLE OF ty_dfkkop .
    TYPES:
      BEGIN OF ty_dfkkcoh,
        cotyp       TYPE dfkkcoh-cotyp,
        cokey       TYPE dfkkcoh-cokey,
        gpart       TYPE dfkkcoh-gpart,
        vkont       TYPE dfkkcoh-vkont,
        adrnr       TYPE dfkkcoh-adrnr,
        aadrnr      TYPE dfkkcoh-aadrnr,
        formkey     TYPE dfkkcoh-formkey,
        formkey_rdi TYPE dfkkcoh-formkey_rdi,
        data1       TYPE dfkkcoh-data1,
        data2       TYPE dfkkcoh-data2,
        spras       TYPE dfkkcoh-spras,
        sendcontrol TYPE dfkkcoh-sendcontrol,
        persnumber  TYPE dfkkcoh-persnumber,
        corr_role   TYPE dfkkcoh-corr_role,
        formkey_hi  TYPE dfkkcohi-formkey,
      END OF ty_dfkkcoh .
    TYPES:
      tt_dfkkcoh TYPE STANDARD TABLE OF ty_dfkkcoh .
    TYPES:
      tt_payreq_h_md TYPE STANDARD TABLE OF zprt_payreq_h_md .
    TYPES:
      tt_corrhist TYPE STANDARD TABLE OF zfi_corrhist .
    TYPES:
      tt_itcoo TYPE STANDARD TABLE OF itcoo .
    TYPES:
      tt_tline TYPE STANDARD TABLE OF tline .
    TYPES ty_soli TYPE soli .
    TYPES:
      tt_soli TYPE STANDARD TABLE OF ty_soli WITH NON-UNIQUE KEY table_line .
    TYPES:
      tt_bapiret2 TYPE STANDARD TABLE OF bapiret2 .
    TYPES:
      BEGIN OF ty_crt_dfkkzr,
        nrzas     TYPE zcbo_crt_dfkkzr-nrzas,
        xblnr     TYPE zcbo_crt_dfkkzr-xblnr,
        nrzas_ori TYPE zcbo_crt_dfkkzr-nrzas_ori,
        item_type TYPE zcbo_crt_dfkkzr-item_type,
        augbl_t   TYPE zcbo_crt_dfkkzr-augbl_t,
      END OF ty_crt_dfkkzr .
    TYPES:
      tt_crt_dfkkzr TYPE STANDARD TABLE OF ty_crt_dfkkzr .

    CONSTANTS gc_objtxt_body TYPE tdobname VALUE 'ZCBO2_PRTPAY_PROCESS_EMAIL_BODY' ##NO_TEXT.
    CONSTANTS gc_objtxt_subj TYPE tdobname VALUE 'ZCBO2_PRTPAY_PROCESS_EMAIL_SUBJ' ##NO_TEXT.
    CONSTANTS gc_billing_completed TYPE zprt_dt_status_h VALUE '03' ##NO_TEXT.
    CONSTANTS gc_start_mail_process TYPE zprt_dt_status_h VALUE '04' ##NO_TEXT.
    DATA gt_payrq_h TYPE tt_payrq_h .
    DATA gt_payrq_i TYPE tt_payrq_i .
    DATA gt_dfkkop TYPE tt_dfkkop .
    DATA gt_dfkkcoh TYPE tt_dfkkcoh .
    DATA gt_payreq_h_md TYPE tt_payreq_h_md .
    DATA gt_corrhist TYPE tt_corrhist .
    DATA gt_payrq_i_upd TYPE tt_payrq_i .
    DATA gt_return TYPE tt_bapiret2 .

    METHODS select_invoice_data .
    METHODS get_correspondence_0043
      IMPORTING
        !im_opbel         TYPE opbel_kk
        !im_dfkkcoh       TYPE ty_dfkkcoh
        !im_otf           TYPE tt_itcoo OPTIONAL
      EXPORTING
        !ex_otf           TYPE tt_itcoo
        !ex_bin_file_size TYPE i
        !ex_bin_pdf_file  TYPE xstring
        !ex_pdf_lines     TYPE tt_tline .
    METHODS send_corr_email .
    METHODS get_text_body_mail
      IMPORTING
        !im_id         TYPE tdid DEFAULT 'ST'
        !im_language   TYPE spras DEFAULT sy-langu
        !im_name       TYPE tdobname
        !im_object     TYPE tdobject DEFAULT 'TEXT'
      RETURNING
        VALUE(re_body) TYPE tt_soli .
    METHODS get_text_subject_mail
      IMPORTING
        !im_id            TYPE tdid DEFAULT 'ST'
        !im_language      TYPE spras DEFAULT sy-langu
        !im_name          TYPE tdobname
        !im_object        TYPE tdobject DEFAULT 'TEXT'
      RETURNING
        VALUE(re_subject) TYPE so_obj_des .
    METHODS read_dfkkop_dfkkcoh
      IMPORTING
        !im_payrq_i TYPE ty_payrq_i
      EXPORTING
        !ex_dfkkcoh TYPE ty_dfkkcoh
        !ex_dfkkop  TYPE ty_dfkkop
        !ex_delete  TYPE abap_bool .
    METHODS add_upd_item
      IMPORTING
        !im_payrq_i TYPE ty_payrq_i .
    METHODS remove_upd_items
      IMPORTING
        !im_payrq_i TYPE ty_payrq_i .
    METHODS upd_status_his .
    METHODS upd_status_header
      IMPORTING
        !im_status   TYPE zprt_dt_status_h
        !im_payreq_h TYPE tt_payrq_h .
    METHODS cerificar_header_itens_pedido .
    METHODS generates_receipt
      IMPORTING
        !im_dfkkop       TYPE ty_dfkkop
        !im_cotyp        TYPE cotyp_kk DEFAULT '0043'
      RETURNING
        VALUE(re_return) TYPE bapiret2 .
    METHODS add_message_return
      RETURNING
        VALUE(re_return) TYPE bapiret2 .
ENDCLASS.



CLASS zcl_cbo2_prtpay_process_email IMPLEMENTATION.


  METHOD add_message_return.
    APPEND INITIAL LINE TO gt_return ASSIGNING FIELD-SYMBOL(<return>).
    CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
      EXPORTING
        type   = sy-msgty
        cl     = sy-msgid
        number = sy-msgno
        par1   = sy-msgv1
        par2   = sy-msgv2
        par3   = sy-msgv3
        par4   = sy-msgv4
      IMPORTING
        return = <return>.
    re_return = <return>.
  ENDMETHOD.


  METHOD add_upd_item.
    APPEND INITIAL LINE TO gt_payrq_i_upd ASSIGNING FIELD-SYMBOL(<upd>).
    <upd> = im_payrq_i.
  ENDMETHOD.


  METHOD cerificar_header_itens_pedido.

    LOOP AT gt_payrq_i
      ASSIGNING FIELD-SYMBOL(<gt_payrq_i>).

      me->read_dfkkop_dfkkcoh(
        EXPORTING
          im_payrq_i = <gt_payrq_i>    " Estrutura
        IMPORTING
          ex_delete  = DATA(ex_delete) ).

      IF ex_delete EQ abap_true.
        DELETE gt_payrq_i.
        CLEAR ex_delete.
        CONTINUE.
      ENDIF.

    ENDLOOP.

    LOOP AT gt_payrq_h
      ASSIGNING FIELD-SYMBOL(<gt_payrq_h>).

      READ TABLE gt_payrq_i
      TRANSPORTING NO FIELDS
      WITH KEY req_guid = <gt_payrq_h>-req_guid.

      IF NOT sy-subrc IS INITIAL.
        DELETE gt_payrq_h.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD generates_receipt.
    CALL FUNCTION 'FKK_FKH0_CCODE_SET'
      EXPORTING
        i_bukrs = im_dfkkop-bukrs.

    CALL FUNCTION 'FKK_CORR_SINGLE_CREATE'
      EXPORTING
        i_cotyp               = im_cotyp
        i_vkont               = im_dfkkop-vkont
        i_gpart               = im_dfkkop-gpart
        i_opbel               = im_dfkkop-opbel
        i_avoid_dialog        = abap_true
        i_avoid_dialog_okcode = 'PRCO'
      EXCEPTIONS
        user_cancellation     = 1
        internal_error        = 2.

    IF sy-subrc <> 0.
      re_return = me->add_message_return( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_correspondence_0043.

    DATA: r_ranges       TYPE STANDARD TABLE OF efg_ranges,
          ls_tfk070h     TYPE  tfk070h,
          ls_printparams TYPE eprintparams,
          lt_otf         TYPE tt_itcoo.

    lt_otf = im_otf.

    CALL FUNCTION 'FKK_GET_CORR_CUSTOMIZING'
      EXPORTING
        i_cotyp     = im_dfkkcoh-cotyp
        i_corr_role = im_dfkkcoh-corr_role
      IMPORTING
        e_tfk070h   = ls_tfk070h.

    IF NOT im_dfkkcoh-formkey IS INITIAL.
      ls_printparams-formkey = im_dfkkcoh-formkey.
    ELSEIF NOT im_dfkkcoh-formkey_rdi IS INITIAL.
      ls_printparams-formkey = im_dfkkcoh-formkey_rdi.
    ENDIF.

    READ TABLE gt_corrhist
    ASSIGNING FIELD-SYMBOL(<corrhist>)
    WITH KEY cotyp = im_dfkkcoh-cotyp
    BINARY SEARCH.
    IF <corrhist> IS ASSIGNED.
      IF ls_printparams-formkey IS INITIAL.
        ls_printparams-formkey = <corrhist>-formkey.
      ENDIF.
      ls_printparams-tddest = <corrhist>-printer.
      UNASSIGN <corrhist>.
    ELSE.
      IF ls_printparams-formkey IS INITIAL.
        ls_printparams-formkey = im_dfkkcoh-formkey_hi.
      ENDIF.
    ENDIF.

    IF NOT ls_printparams-formkey IS INITIAL.
      ls_printparams-formclass = ls_tfk070h-formclass.
      ls_printparams-sendcontrol    = im_dfkkcoh-sendcontrol.
      ls_printparams-rec_addr       = im_dfkkcoh-adrnr.
      ls_printparams-rec_persnumber = im_dfkkcoh-persnumber.
      ls_printparams-send_addr      = im_dfkkcoh-aadrnr.
      ls_printparams-langu          = im_dfkkcoh-spras.
      ls_printparams-tdpreview      = abap_false.
      ls_printparams-tdgetotf       = abap_true.

      r_ranges = VALUE #( sign = 'I'  option = 'EQ' ( low = im_opbel ) ).

      CALL FUNCTION 'EFG_PRINT_EXPANDED'
        EXPORTING
          x_printparams       = ls_printparams
          x_rec_addr          = im_dfkkcoh-adrnr
          x_rec_persnumber    = im_dfkkcoh-persnumber
        IMPORTING
          y_printparams       = ls_printparams
        TABLES
          xt_ranges           = r_ranges
          yt_otf_data         = ex_otf
        EXCEPTIONS
          not_qualified       = 1
          print_failed        = 2
          cancelled           = 3
          rec_addr_not_found  = 4
          send_addr_not_found = 5
          input_error         = 6.

*     Executa merge de tabelas OTF para impressão de somente um PDF.
      IF NOT lt_otf IS INITIAL.
        LOOP AT lt_otf
          ASSIGNING FIELD-SYMBOL(<lt_otf>).
          APPEND INITIAL LINE TO ex_otf ASSIGNING FIELD-SYMBOL(<ex_otf>).
          <ex_otf> = <lt_otf>.
        ENDLOOP.
      ENDIF.

      IF sy-subrc IS INITIAL.
        CLEAR: ex_bin_file_size,
               ex_bin_pdf_file,
               ex_pdf_lines.

        CALL FUNCTION 'CONVERT_OTF' ##FM_SUBRC_OK
          EXPORTING
            format                = 'PDF'
          IMPORTING
            bin_filesize          = ex_bin_file_size
            bin_file              = ex_bin_pdf_file
          TABLES
            otf                   = ex_otf
            lines                 = ex_pdf_lines
          EXCEPTIONS
            err_max_linewidth     = 1
            err_format            = 2
            err_conv_not_possible = 3
            err_bad_otf           = 4
            OTHERS                = 5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_text_body_mail.
    DATA: it_body TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = im_id
        language = im_language
        name     = im_name
        object   = im_object
      TABLES
        lines    = it_body.

    IF NOT it_body IS INITIAL.
      LOOP AT it_body
        ASSIGNING FIELD-SYMBOL(<body>).
        APPEND INITIAL LINE TO re_body ASSIGNING FIELD-SYMBOL(<contents>).
        <contents> = <body>-tdline.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_text_subject_mail.
    DATA: it_subject TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id       = im_id
        language = im_language
        name     = im_name
        object   = im_object
      TABLES
        lines    = it_subject.

    IF NOT it_subject IS INITIAL.
      LOOP AT it_subject
        ASSIGNING FIELD-SYMBOL(<subject>).
        re_subject = re_subject && <subject>-tdline.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD read_dfkkop_dfkkcoh.

    READ TABLE gt_dfkkop
    INTO ex_dfkkop
    WITH KEY opbel = im_payrq_i-doc_comp_new.

    IF sy-subrc IS INITIAL.
      READ TABLE gt_dfkkcoh
      INTO ex_dfkkcoh
      WITH KEY vkont = ex_dfkkop-vkont
               data1 = ex_dfkkop-doc_data.
      IF NOT sy-subrc IS INITIAL.
        READ TABLE gt_dfkkcoh
        INTO ex_dfkkcoh
        WITH KEY vkont = ex_dfkkop-vkont
                 data2 = ex_dfkkop-doc_data.
        IF NOT sy-subrc IS INITIAL.
          ex_delete = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD remove_upd_items.

    DELETE gt_payrq_i_upd WHERE req_guid EQ im_payrq_i-req_guid.

  ENDMETHOD.


  METHOD run_send_email.
    me->select_invoice_data( ).
    me->send_corr_email( ).
    me->upd_status_his( ).
    re_return = gt_return.
  ENDMETHOD.


  METHOD select_invoice_data.
    DATA: r_cotyp       TYPE RANGE OF dfkkcoh-cotyp,
          lr_field      TYPE REF TO cx_root,
          lt_crt_dfkkzr TYPE tt_crt_dfkkzr,
          lt_dfkkop     TYPE tt_dfkkop.

    TRY.
        SELECT req_guid, req_guid_ref, req_date, req_time, req_status, req_status_date,
               total_val_req, total_val_paid, pay_typ, numb_id, req_type
          FROM zprt_payrq_h
          INTO TABLE @gt_payrq_h
          WHERE req_status EQ @gc_billing_completed.    "#EC CI_NOFIELD

        DELETE gt_payrq_h WHERE req_type EQ '0' "ICI de notificação
                              OR req_type EQ 'S'. "Search

        SORT gt_payrq_h.

        IF NOT gt_payrq_h IS INITIAL.

          SELECT mandt, req_guid, name, street, post_code1,
                 city1, country, nif, email
            FROM zprt_payreq_h_md
            INTO TABLE @gt_payreq_h_md
            FOR ALL ENTRIES IN @gt_payrq_h
            WHERE req_guid EQ @gt_payrq_h-req_guid.

          DELETE gt_payreq_h_md WHERE email IS INITIAL.

          SORT gt_payreq_h_md BY req_guid.

          SELECT payrq_i~req_guid, payrq_i~item_type, payrq_i~req_item_aggr, payrq_i~req_item, payrq_i~source_id,
                 payrq_i~tr_id, payrq_i~new_tr_id, payrq_i~xblnr, payrq_i~lpn, payrq_i~bukrs, payrq_i~action, payrq_i~opbel, payrq_i~opupk,
                 payrq_i~betrw, payrq_i~item_status, payrq_i~item_prev_status, payrq_i~item_status_date,
                 payrq_i~req_guid_ref, payrq_i~doc_anul, payrq_i~doc_comp, payrq_i~doc_new, payrq_i~doc_comp_new
            FROM zprt_payrq_i AS payrq_i INNER JOIN zcbo_crt_dfkkzr AS crt_dfkkzr ON payrq_i~doc_comp_new = crt_dfkkzr~augbl_t
            INTO TABLE @gt_payrq_i
            FOR ALL ENTRIES IN @gt_payrq_h
            WHERE payrq_i~req_guid EQ @gt_payrq_h-req_guid.

          DELETE gt_payrq_i WHERE doc_comp_new IS INITIAL.

          SORT gt_payrq_i BY req_guid
                             doc_comp_new.

          DELETE ADJACENT DUPLICATES FROM gt_payrq_i COMPARING req_guid
                                                               doc_comp_new.

          SORT gt_payrq_i BY req_guid
                             item_type
                             req_item_aggr
                             req_item.

          IF NOT gt_payrq_i IS INITIAL.

            SELECT augbl AS opbel, opupw, opupk, opupz, bukrs, gpart, vkont, xblnr, opbel AS augbl,augbl AS doc_data
                          FROM dfkkop
                          INTO TABLE @gt_dfkkop
                          FOR ALL ENTRIES IN @gt_payrq_i
                          WHERE augbl EQ @gt_payrq_i-doc_comp_new.

            DELETE gt_dfkkop WHERE opupk <> '0001'.
            SORT gt_dfkkop BY opbel gpart vkont.
            DELETE ADJACENT DUPLICATES FROM gt_dfkkop COMPARING opbel gpart vkont.
            SORT gt_dfkkop.

            IF NOT gt_dfkkop IS INITIAL.

              r_cotyp = VALUE #( sign = 'I'  option = 'EQ' ( low = '0043' ) ).

              SELECT coh~cotyp, coh~cokey, coh~gpart, coh~vkont, coh~adrnr,
                     coh~aadrnr, coh~formkey, coh~formkey_rdi, coh~data1,
                     coh~data2, coh~spras, coh~sendcontrol, coh~persnumber,
                     coh~corr_role, cohi~formkey AS formkey_hi
                FROM dfkkcoh AS coh LEFT JOIN dfkkcohi AS cohi ON coh~cotyp = cohi~cotyp
                                                              AND coh~gpart = cohi~gpart
                                                              AND coh~vkont = cohi~vkont
                                                              AND coh~cokey = cohi~cokey
                INTO TABLE @gt_dfkkcoh
                FOR ALL ENTRIES IN @gt_dfkkop
                WHERE coh~cotyp IN @r_cotyp
                  AND coh~vkont EQ @gt_dfkkop-vkont
                  AND coh~data1 EQ @gt_dfkkop-doc_data.

              SORT gt_dfkkcoh BY data1.

              LOOP AT gt_payrq_i
              ASSIGNING FIELD-SYMBOL(<gt_payrq_i>).

                READ TABLE gt_dfkkop
                ASSIGNING FIELD-SYMBOL(<gt_dfkkop>)
                WITH KEY opbel = <gt_payrq_i>-doc_comp_new
                BINARY SEARCH.

                IF <gt_dfkkop> IS ASSIGNED.
                  READ TABLE gt_dfkkcoh
                  WITH KEY data1 = <gt_dfkkop>-doc_data
                  TRANSPORTING NO FIELDS
                  BINARY SEARCH.

                  IF NOT sy-subrc IS INITIAL.
                    DATA(ls_return) = me->generates_receipt( im_dfkkop = <gt_dfkkop> ).
                    IF ls_return IS INITIAL.
                      APPEND <gt_dfkkop> TO lt_dfkkop.
                    ENDIF.
                    CLEAR: ls_return.
                  ENDIF.

                  UNASSIGN <gt_dfkkop>.
                ENDIF.

              ENDLOOP.
              COMMIT WORK.

              IF NOT lt_dfkkop IS INITIAL.

                SELECT coh~cotyp, coh~cokey, coh~gpart, coh~vkont, coh~adrnr,
                       coh~aadrnr, coh~formkey, coh~formkey_rdi, coh~data1,
                       coh~data2, coh~spras, coh~sendcontrol, coh~persnumber,
                       coh~corr_role, cohi~formkey AS formkey_hi
                  FROM dfkkcoh AS coh LEFT JOIN dfkkcohi AS cohi ON coh~cotyp = cohi~cotyp
                                                                AND coh~gpart = cohi~gpart
                                                                AND coh~vkont = cohi~vkont
                                                                AND coh~cokey = cohi~cokey
                  APPENDING TABLE @gt_dfkkcoh
                  FOR ALL ENTRIES IN @lt_dfkkop
                  WHERE coh~cotyp IN @r_cotyp
                    AND coh~vkont EQ @lt_dfkkop-vkont
                    AND coh~data1 EQ @lt_dfkkop-doc_data.

              ENDIF.

              SORT gt_dfkkcoh.

              IF NOT gt_dfkkcoh IS INITIAL.

                SELECT mandt, cotyp, formkey, printer
                  INTO TABLE @gt_corrhist
                  FROM zfi_corrhist
                  FOR ALL ENTRIES IN @gt_dfkkcoh
                  WHERE cotyp EQ @gt_dfkkcoh-cotyp.

                SORT gt_corrhist BY cotyp.

                me->cerificar_header_itens_pedido( ).
                me->upd_status_header(
                  EXPORTING
                    im_status   = '04'
                    im_payreq_h = gt_payrq_h
                ).

              ENDIF. "NOT gt_dfkkcoh IS INITIAL.
            ENDIF. "NOT gt_dfkkop IS INITIAL.
          ENDIF. "NOT gt_payrq_i IS INITIAL.
        ENDIF. "NOT gt_payrq_h IS INITIAL.

      CATCH cx_root INTO lr_field.
        MESSAGE e001 INTO DATA(dummy).
        me->add_message_return( ).
    ENDTRY.
  ENDMETHOD.


  METHOD send_corr_email.

*BCS class for sending mail
    DATA: lr_send_email    TYPE REF TO cl_bcs,
          lr_send_request  TYPE REF TO cl_send_request_bcs,
          lr_document      TYPE REF TO cl_document_bcs,
          lr_recipient     TYPE REF TO if_recipient_bcs,
          lr_sender        TYPE REF TO if_sender_bcs,
          lr_bcs_exception TYPE REF TO cx_bcs.

    DATA: lv_att_size TYPE so_obj_len,
          lv_att_sub  TYPE so_obj_des,
          lv_att_pdf  TYPE xstring,
          lt_otf      TYPE tt_itcoo.

    DATA: lt_payreq_h TYPE tt_payrq_h.

    SORT gt_payrq_h BY req_guid.

    LOOP AT gt_payrq_i
      ASSIGNING FIELD-SYMBOL(<payrq_i>).

      TRY .

          AT NEW req_guid.
            CLEAR: lr_send_email,
                   lr_send_request,
                   lr_document,
                   lr_recipient,
                   lr_sender,
                   lt_otf.

            READ TABLE gt_payrq_h
            ASSIGNING FIELD-SYMBOL(<gt_payrq_h>)
            BINARY SEARCH
            WITH KEY req_guid = <payrq_i>-req_guid.

*       Create instance of the email class
            lr_send_email = cl_bcs=>create_persistent( ).

*           Create email lo_document inc type, subject and boby text
            lr_document = cl_document_bcs=>create_document(
                            i_type    = 'RAW' "'HTM'
                            i_subject = me->get_text_subject_mail( im_name = gc_objtxt_subj )
                            i_text    = me->get_text_body_mail( im_name = gc_objtxt_body )
                          ).
          ENDAT.

          me->read_dfkkop_dfkkcoh(
            EXPORTING
              im_payrq_i = <payrq_i>
            IMPORTING
              ex_dfkkcoh = DATA(ex_dfkkcoh)
              ex_dfkkop  = DATA(ex_dfkkop)
              ex_delete  = DATA(ex_delete)
          ).

          IF ex_delete EQ abap_true.
            CONTINUE.
          ENDIF.

          CASE ex_dfkkcoh-cotyp.
            WHEN '0043'.
              me->get_correspondence_0043(
                EXPORTING
                  im_opbel         = ex_dfkkop-opbel    " Nº de um documento da conta corrente contratual
                  im_dfkkcoh       = ex_dfkkcoh
                  im_otf           = lt_otf
                IMPORTING
                  ex_bin_pdf_file  = lv_att_pdf
                  ex_otf           = lt_otf ).
            WHEN OTHERS.
          ENDCASE.

          AT END OF req_guid.

*         Calculate size of attachment
            lv_att_size  = xstrlen( lv_att_pdf ).

*         file name
            lv_att_sub = sy-datum && sy-uzeit && ex_dfkkop-opbel.

*         Add the attachment table to the document
            CALL METHOD lr_document->add_attachment(
                i_attachment_type    = 'PDF'
                i_attachment_subject = lv_att_sub
                i_attachment_size    = lv_att_size
                i_att_content_hex    = cl_document_bcs=>xstring_to_solix( lv_att_pdf ) ).

            me->add_upd_item( im_payrq_i = <payrq_i> ).

*           Assign document and all its details to the email
            CALL METHOD lr_send_email->set_document( lr_document ).

*           Assign recipient to email
            READ TABLE gt_payreq_h_md
            ASSIGNING FIELD-SYMBOL(<payreq_h_md>)
            WITH KEY req_guid = <payrq_i>-req_guid
            BINARY SEARCH.
            IF <payreq_h_md> IS ASSIGNED.
              lr_send_email->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = <payreq_h_md>-email ) ).

              DATA(send_result) = lr_send_email->send( ).
              IF send_result EQ abap_true.
                COMMIT WORK.
              ELSE.
                ROLLBACK WORK.
                APPEND <gt_payrq_h> TO lt_payreq_h.
                MESSAGE e003 WITH <payrq_i>-req_guid INTO DATA(dummy).
                me->add_message_return( ).
                me->remove_upd_items( im_payrq_i = <payrq_i> ).
              ENDIF.

              UNASSIGN <payreq_h_md>.
            ELSE.
              APPEND <gt_payrq_h> TO lt_payreq_h.
              MESSAGE e008 WITH <payrq_i>-req_guid INTO dummy.
              me->add_message_return( ).
              me->remove_upd_items( im_payrq_i = <payrq_i> ).
            ENDIF.
            UNASSIGN <gt_payrq_h>.
          ENDAT.

          CLEAR: ex_dfkkcoh,
                 ex_dfkkop,
                 ex_delete.


        CATCH cx_bcs  INTO lr_bcs_exception.
          MESSAGE e003 WITH <payrq_i>-req_guid INTO dummy.
          me->add_message_return( ).
          me->remove_upd_items( im_payrq_i = <payrq_i> ).
      ENDTRY.

    ENDLOOP.

    IF NOT lt_payreq_h IS INITIAL.
      me->upd_status_header(
        EXPORTING
          im_status   = '03'
          im_payreq_h = lt_payreq_h
      ).
    ENDIF.

  ENDMETHOD.


  METHOD upd_status_header.
    DATA: lt_payreq_h    TYPE STANDARD TABLE OF zprt_payrq_h.

    DATA: lr_field TYPE REF TO cx_root.

    lt_payreq_h = CORRESPONDING #( im_payreq_h ).

    SORT: lt_payreq_h BY req_guid.

    LOOP AT lt_payreq_h
      ASSIGNING FIELD-SYMBOL(<lt_payreq_h>).
      <lt_payreq_h>-req_status = im_status.
    ENDLOOP.

    MODIFY zprt_payrq_h FROM TABLE lt_payreq_h.
    TRY .
      CATCH cx_root INTO lr_field.
        MESSAGE e004 INTO DATA(dummy).
        me->add_message_return( ).
    ENDTRY.

  ENDMETHOD.


  METHOD upd_status_his.
    DATA: lt_payreq_h    TYPE STANDARD TABLE OF zprt_payrq_h,
          lt_payrq_i     TYPE STANDARD TABLE OF zprt_payrq_i,
          lt_payrq_i_his TYPE STANDARD TABLE OF zprt_payrq_i_his.

    DATA: lr_field TYPE REF TO cx_root.

    SORT: gt_payrq_i_upd,
    gt_payrq_h BY req_guid.

    LOOP AT gt_payrq_i_upd
      ASSIGNING FIELD-SYMBOL(<payrq_i_upd>).

      AT NEW req_guid.
        READ TABLE gt_payrq_h
        ASSIGNING FIELD-SYMBOL(<gt_payrq_h>)
        WITH KEY req_guid = <payrq_i_upd>-req_guid
        BINARY SEARCH.
        IF <gt_payrq_h> IS ASSIGNED.
          APPEND INITIAL LINE TO lt_payreq_h ASSIGNING FIELD-SYMBOL(<lt_payreq_h>).
          <lt_payreq_h> = CORRESPONDING #( <gt_payrq_h> ).
          <lt_payreq_h>-req_status = '05'.
        ENDIF.
        UNASSIGN <gt_payrq_h>.
      ENDAT.

      APPEND INITIAL LINE TO lt_payrq_i ASSIGNING FIELD-SYMBOL(<lt_payrq_i>).
      <lt_payrq_i> = CORRESPONDING #( <payrq_i_upd> ).
      <lt_payrq_i>-item_status      = '07'.
      <lt_payrq_i>-item_prev_status = '06'.
      <lt_payrq_i>-item_status_date = sy-datum.

      APPEND INITIAL LINE TO lt_payrq_i_his ASSIGNING FIELD-SYMBOL(<lt_payrq_i_his>).
      <lt_payrq_i_his> = CORRESPONDING #( <payrq_i_upd> ).
      <lt_payrq_i_his>-item_new_status  = '07'.
      <lt_payrq_i_his>-item_prev_status = '06'.
      <lt_payrq_i_his>-new_status_date  = sy-datum.
      <lt_payrq_i_his>-new_status_hor   = sy-uzeit.

    ENDLOOP.

    TRY .
        MODIFY zprt_payrq_h FROM TABLE lt_payreq_h.
        MODIFY zprt_payrq_i FROM TABLE lt_payrq_i.
        MODIFY zprt_payrq_i_his FROM TABLE lt_payrq_i_his.
      CATCH cx_root INTO lr_field.
        MESSAGE e005 INTO DATA(dummy).
        me->add_message_return( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

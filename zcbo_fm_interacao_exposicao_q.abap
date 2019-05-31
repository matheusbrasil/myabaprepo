FUNCTION zcbo_fm_interacao_exposicao_q.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_INTERACAO) TYPE  CRMT_OBJECT_ID_DB
*"  EXPORTING
*"     VALUE(ET_EXPOSICOES) TYPE  ZCBOTT_INT_EXPOSICOES
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  CONSTANTS: gc_grupo_resp TYPE crmt_partner_fct VALUE '00000099',
             gc_operador   TYPE crmt_partner_fct VALUE '00000014',
             gc_cliente    TYPE crmt_partner_fct VALUE '00000015'.

  DATA lt_flow TYPE crmt_doc_flow_wrkt.

  DATA: lt_orderadm_h TYPE  crmt_orderadm_h_wrkt,
        lt_partner    TYPE crmt_partner_external_wrkt,
        lt_status     TYPE  crmt_status_wrkt.

  IF NOT iv_interacao IS INITIAL.

    SELECT guid
      FROM crmd_orderadm_h
      INTO @DATA(lv_guids)
        UP TO 1 ROWS
      WHERE object_id = @iv_interacao. "'2000000002'
    ENDSELECT.

    IF 1 = 2.

*      DATA(lr_associacao) = NEW zcl_cbov2_caixa_entrada( ).
*
*      lr_associacao->get_associacao_a_b_ow(
*        EXPORTING
*          i_guid       = VALUE #( ( lv_guids ) )
*          ir_process_b = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZRV1' ) )
*          ir_process   = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZIIN' )
*                                                           ( low = 'Z001' ) )
*        IMPORTING
*          e_relacao    = DATA(lt_relacao)
*          et_return    = DATA(lt_return)
*      ).

*      lr_associacao->get_associacao_a_b_ow(
*        EXPORTING
*          i_guid       = VALUE #( FOR <relacao> IN lt_relacao ( <relacao>-relacao_guid ) )
*          ir_process_b = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZIIN' )
*                                                           ( low = 'Z001' ) )
*          ir_process   = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZRV1' ) )
*        IMPORTING
*          e_relacao    = DATA(lt_exp_relacao)
*          et_return    = lt_return
*      ).

    ENDIF.

    IF sy-subrc IS INITIAL.

*      CALL FUNCTION 'CRM_DOC_FLOW_READ_OB'
*        EXPORTING
*          iv_header_guid  = lv_guids    " GUID of a CRM Order Object
*        IMPORTING
*          et_doc_flow_wrk = lt_flow.

*      DATA(lt_header_guid) = VALUE crmt_object_guid_tab( ( lv_guids ) ).
*      LOOP AT lt_flow
*       ASSIGNING FIELD-SYMBOL(<flow>).
*        APPEND CONV #( <flow>-objkey_b ) TO lt_header_guid.
*      ENDLOOP.

      DATA(lt_header_guid) = VALUE crmt_object_guid_tab( ( lv_guids ) ).

      NEW zcl_cbov2_caixa_entrada( )->get_associacao_a_b_ow(
        EXPORTING
          i_guid       = VALUE #( ( lv_guids ) )
          ir_process_b = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZRV1' ) )
          ir_process   = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZIIN' )
                                                           ( low = 'Z001' ) )
        IMPORTING
          e_relacao    = DATA(lt_relacao)
          et_return    = DATA(lt_return)
      ).

      NEW zcl_cbov2_caixa_entrada( )->get_associacao_a_b_ow(
        EXPORTING
          i_guid       = VALUE #( FOR <relacao> IN lt_relacao ( <relacao>-relacao_guid ) )
          ir_process_b = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZIIN' )
                                                           ( low = 'Z001' ) )
          ir_process   = VALUE #( sign = 'I' option = 'EQ' ( low = 'ZRV1' ) )
        IMPORTING
          e_relacao    = DATA(lt_exp_relacao)
          et_return    = lt_return
      ).

      LOOP AT lt_relacao
       ASSIGNING FIELD-SYMBOL(<relation>).
        INSERT CONV #( <relation>-relacao_guid ) INTO TABLE lt_header_guid.
      ENDLOOP.

      CALL FUNCTION 'CRM_ORDER_READ'
        EXPORTING
          it_header_guid       = lt_header_guid    " List of GUIDs
        IMPORTING
          et_orderadm_h        = lt_orderadm_h
          et_partner           = lt_partner
          et_status            = lt_status
        EXCEPTIONS
          document_not_found   = 1
          error_occurred       = 2
          document_locked      = 3
          no_change_authority  = 4
          no_display_authority = 5
          no_change_allowed    = 6
          OTHERS               = 7.

      IF sy-subrc IS INITIAL.
        IF NOT lt_orderadm_h IS INITIAL.
          SELECT process_type, user_stat_proc
            FROM crmc_proc_type
            INTO TABLE @DATA(lt_proc_type)
            FOR ALL ENTRIES IN @lt_orderadm_h
            WHERE process_type EQ @lt_orderadm_h-process_type
            ORDER BY PRIMARY KEY.

          et_exposicoes = VALUE #( FOR <orderadm_h> IN lt_orderadm_h WHERE ( guid NE lv_guids ) (
                                  nr_processo      = <orderadm_h>-object_id
                                  id_natureza      = <orderadm_h>-zzafld00001j
                                  id_estado        = VALUE #( lt_status[ guid = <orderadm_h>-guid  user_stat_proc = VALUE #( lt_proc_type[ process_type = <orderadm_h>-process_type ]-user_stat_proc OPTIONAL  ) ]-status OPTIONAL  )
                                  id_gp_resp       = |{ VALUE bu_partner( lt_partner[ ref_guid = <orderadm_h>-guid partner_fct = gc_grupo_resp ]-partner_no OPTIONAL  ) ALPHA = IN }|
                                  operador         = |{ VALUE bu_partner( lt_partner[ ref_guid = <orderadm_h>-guid partner_fct = gc_operador ]-partner_no OPTIONAL  ) ALPHA = IN }|
*                                  COUNT_INTERACAO  = REDUCE #( init x = 0 for <exp_relacao> in FILTER #( lt_exp_relacao WHERE NR_PROCESSO_GUID = <orderadm_h>-guid  ) NEXT x = x + 1 )
                                  count_interacao  = REDUCE #( INIT x = 0 FOR <exp_relacao> IN lt_exp_relacao WHERE ( nr_processo_guid = <orderadm_h>-guid ) NEXT x = x + 1 )
                                  nr_interacao     = iv_interacao
                                  data_alteracao   = <orderadm_h>-head_changed_at
                                  partner          = |{ VALUE bu_partner( lt_partner[ ref_guid = <orderadm_h>-guid partner_fct = gc_cliente ]-partner_no OPTIONAL  ) ALPHA = IN }|
                                           ) ).

        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<return>).
        CALL FUNCTION 'FS_BAPI_BAPIRET2_FILL'
          EXPORTING
            type   = sy-msgty    " Message Type
            cl     = sy-msgid    " Message Class (Message ID)
            number = sy-msgno    " Message Number
            par1   = sy-msgv1    " Variable 1
            par2   = sy-msgv2    " Variable 2
            par3   = sy-msgv3    " Variable 3
            par4   = sy-msgv4    " Variable 4
          IMPORTING
            return = <return>.    " BAPI Return Parameter
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.

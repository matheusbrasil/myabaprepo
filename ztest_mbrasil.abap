*$*$ ---------------------------------------------------------------*$*$
*$*$                         Nome do Projeto                        *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Desenvolvedor:   Yyyyyyyy - Accenture - <matrícula>            *$*$
*$*$ Analista:        Xxxxxx   - Xxxxxxxx  - <matrícula>            *$*$
*$*$ Data de criação: dd/mm/aaaa                                    *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Esp.técnica: XXX_XXX_XXXXX                                     *$*$
*$*$ Observação:  Xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ Data       | Autor      | Descrição                            *$*$
*$*$ dd/mm/aaaa | XXXXXXXXX  | XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *$*$
*$*$ ---------------------------------------------------------------*$*$
REPORT ztest_mbrasil MESSAGE-ID zmensagens
                NO STANDARD PAGE HEADING
                LINE-SIZE 222.

*======================================================================*
* DECLARAÇÃO DE TIPOS                                                  *
*======================================================================*
TYPES: BEGIN OF ty_partner,
         partner      TYPE but000-partner,
         type         TYPE but000-type,
         bpkind       TYPE but000-bpkind,
         bu_group     TYPE but000-bu_group,
         bpext        TYPE but000-bpext,
         bu_sort1     TYPE but000-bu_sort1,
         bu_sort2     TYPE but000-bu_sort2,
         source       TYPE but000-source,
         title        TYPE but000-title,
         name_org1    TYPE but000-name_org1,
         name_org2    TYPE but000-name_org2,
         name_org3    TYPE but000-name_org3,
         name_org4    TYPE but000-name_org4,
         name_last    TYPE but000-name_last,
         name_first   TYPE but000-name_first,
         name_lst2    TYPE but000-name_lst2,
         name_last2   TYPE but000-name_last2,
         namemiddle   TYPE but000-namemiddle,
         partner_guid TYPE but000-partner_guid,
       END OF ty_partner,
       tt_partner TYPE STANDARD TABLE OF ty_partner INITIAL SIZE 0.

*======================================================================*
* CONSTANTES                                                           *
*======================================================================*
* ERRADO
CONSTANTS: gc_999999999 TYPE bptaxnum      VALUE '999999999',
           gc_04        TYPE zpay_typ      VALUE '04',
           gc_d         TYPE zitem_type    VALUE 'D',
           gc_v         TYPE zitem_type    VALUE 'V',
           gc_0         TYPE zprt_action   VALUE '0',
           gc_p         TYPE zprt_action   VALUE 'P',
           gc_00        TYPE zitem_status  VALUE '00',
           gc_s         TYPE zprt_req_type VALUE 'S',
           gc_2         TYPE zprt_req_type VALUE '2'.

* CERTO
CONSTANTS: gc_nif_consumidor_final  TYPE bptaxnum      VALUE '999999999',
           gc_pgto_multi_banco      TYPE zpay_typ      VALUE '04',
           gc_documento             TYPE zitem_type    VALUE 'D',
           gc_viagem                TYPE zitem_type    VALUE 'V',
           gc_nao_pago              TYPE zprt_action   VALUE '0',
           gc_pago                  TYPE zprt_action   VALUE 'P',
           gc_estado_inicial        TYPE zitem_status  VALUE '00',
           gc_pesquisa_notificacao  TYPE zprt_req_type VALUE 'S',
           gc_pesquisa_matricula    TYPE zprt_req_type VALUE '0',
           gc_pagamento_notificacao TYPE zprt_req_type VALUE '2'.



*======================================================================*
* DECLARAÇÃO DE TABELAS INTERNAS                                       *
*======================================================================*
*STANDARD TABLE
DATA: gt_partner  TYPE tt_partner,
      gt_material TYPE STANDARD TABLE OF mara INITIAL SIZE 0.

*======================================================================*
* DECLARAÇÃO DE ESTRUTURAS                                             *
*======================================================================*
DATA: gs_partner TYPE ty_partner.

*======================================================================*
* DECLARAÇÃO DE RANGES                                                 *
*======================================================================*
DATA: r_partner TYPE RANGE OF ty_partner-partner.

*======================================================================*
* DECLARAÇÃO DE PONTEIROS                                              *
*======================================================================*
FIELD-SYMBOLS: <partner> TYPE ty_partner.

*======================================================================*
* DECLARAÇÃO DE REFERENCIAS                                            *
*======================================================================*
DATA: gr_alv TYPE REF TO cl_salv_table.

*======================================================================*
* TELA DE SELEÇÂO                                                      *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK bloco_0 WITH FRAME TITLE text-sel.
SELECT-OPTIONS: s_part FOR gs_partner-partner OBLIGATORY,
                s_type FOR gs_partner-type NO INTERVALS,
                s_kind FOR gs_partner-bpkind.
SELECTION-SCREEN END   OF BLOCK bloco_0.

*======================================================================*
* START-OF-SELECTION                                                   *
*======================================================================*
START-OF-SELECTION.

  PERFORM seleciona_dados_parceiro CHANGING gt_partner.
  PERFORM prepara_saida_alv USING gt_partner.

END-OF-SELECTION.

  PERFORM exibe_alv.

*======================================================================*
* PERFORMS PERTINENTES AO PROGRAMA                                     *
*======================================================================*
*----------------------------------------------------------------------*
* Form  seleciona_dados_parceiro
*----------------------------------------------------------------------*
* Seleciona a massa de dados para execução do programa
*----------------------------------------------------------------------*
FORM seleciona_dados_parceiro CHANGING lt_partner TYPE tt_partner.
  CLEAR: lt_partner.
  DATA: lt_but000 TYPE STANDARD TABLE OF but000.
  SELECT partner type bpkind bu_group bpext bu_sort1 bu_sort2 source
         title name_org1 name_org2 name_org3 name_org4 name_last
         name_first name_lst2 name_last2 namemiddle partner_guid
    FROM but000
    INTO TABLE lt_but000 "lt_partner
    WHERE partner IN s_part
      AND type    IN s_type
      AND bpkind  IN s_kind.

ENDFORM.                    " z_seleciona_dados_pai

*----------------------------------------------------------------------*
* Form  prepara_saida_alv
*----------------------------------------------------------------------*
* Processa os dados.
*----------------------------------------------------------------------*
FORM prepara_saida_alv USING lt_partner TYPE tt_partner..
  DATA message TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_alv
        CHANGING
          t_table      = lt_partner ).
    CATCH cx_salv_msg INTO message.
      " error handling
  ENDTRY.
ENDFORM.                    " z_processa_dados

*----------------------------------------------------------------------*
* Form  exibe_alv
*----------------------------------------------------------------------*
* Ação final. Imprime um relatório, gera um arquivo, entre outros...
*----------------------------------------------------------------------*
FORM exibe_alv.
  gr_alv->display( ).
ENDFORM.                    " z_acao_final
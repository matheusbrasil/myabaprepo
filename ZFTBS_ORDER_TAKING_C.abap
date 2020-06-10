FUNCTION ZFTBS_ORDER_TAKING_C.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IS_HEADER) TYPE  ZSTBS_HF_ORDER_TAKE_H
*"     VALUE(IT_ITEMS) TYPE  ZTTTBS_HF_ORDER_TAKE_I
*"  EXPORTING
*"     VALUE(ES_RETURN) TYPE  BAPIRET2
*"----------------------------------------------------------------------
  DATA(ls_idoc_structure) = VALUE yotorders05(
                                e1edk01 = VALUE #( action = is_header-action
                                                   augru  = is_header-augru )

                                e1edk14 = VALUE #( ( qualf = '006' "Division
                                                     orgid = COND #( WHEN NOT is_header-spart IS INITIAL
                                                                       THEN is_header-spart
                                                                       ELSE '01' ) )

                                                   ( qualf = '007' "Distribution Channel
                                                     orgid = is_header-vtweg )

                                                   ( qualf = '008' "Sales Organization
                                                     orgid = is_header-vkorg )

                                                   ( qualf = '012' "Sales Document Type
                                                     orgid = is_header-auart )

                                                   ( qualf = '019' "Customer purchase order type
                                                     orgid = is_header-bsark ) )

                                e1edk03 = VALUE #( ( iddat = '002' "Requested delivery date
                                                     datum = is_header-vdatu ) )

                                e1edka1 = VALUE #( ( parvw = 'AG' "Sold-to party
                                                     partn = is_header-customer )

                                                   ( parvw = 'WE' "Ship-to party
                                                     partn = is_header-store ) )

                                e1edk02 = VALUE #( ( qualf = '001' "Customer Purchase Order
                                                     belnr = is_header-customer_po )

                                                   ( qualf = '044' "Ship-To Party's PO Order
                                                     belnr = is_header-store_po ) )

                                e1edkt1 = COND #( WHEN NOT is_header-comment_text IS INITIAL THEN
                                            VALUE #( ( tdid        = '0001'
                                                       tsspras     = 'EN'
                                                       tsspras_iso = 'E'
                                                       tdobject    = 'VBBK'
                                                       tdobname    = COND #( WHEN NOT is_header-vbeln IS INITIAL
                                                                               THEN is_header-vbeln )
                                                       e1edkt2     = zcl_tbs_hf_portal_util=>string_to_table( iv_string = is_header-comment_text ) ) ) )

                                e1edp01 = VALUE #( FOR <item> IN it_items (
                                                    posex  = <item>-posnr
                                                    action = '002'
                                                    menge  = <item>-kwmeng
                                                    menee  = 'PK' "<item>-vrkme
                                                    matnr  = <item>-matnr

                                                    e1edp19 = VALUE #( ( qualf = '002'
                                                                         idtnr = <item>-matnr ) )

                                                    e1edpt1 = COND #( WHEN NOT <item>-comment_text IS INITIAL THEN
                                                                VALUE #( ( tdid        = '0001'
                                                                           tsspras     = 'EN'
                                                                           tsspras_iso = 'E'
                                                                           e1edpt2     = zcl_tbs_hf_portal_util=>string_to_table( iv_string = <item>-comment_text ) ) ) )
                                                  ) )
                              ).

  CALL FUNCTION 'ZTBS_IDOC_CREATE_ORDER_TAKING'
    EXPORTING
      is_orders05 = ls_idoc_structure.

ENDFUNCTION.

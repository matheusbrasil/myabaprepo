CLASS zcl_form_texts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_text_fields,
             date_text TYPE c LENGTH 30,
             company   TYPE c LENGTH 30,
           END   OF ty_text_fields.

    TYPES: BEGIN OF ty_translate_texts,
             langu TYPE sy-langu,
             field TYPE fnam_____4,
             value TYPE bdc_fval,
           END   OF ty_translate_texts,
           tt_translate_texts TYPE SORTED TABLE OF ty_translate_texts
                                WITH UNIQUE KEY langu field.

    TYPES tt_abap_componentdescr TYPE SORTED TABLE OF abap_componentdescr
                                      WITH NON-UNIQUE KEY name.

    METHODS constructor
      IMPORTING
                !i_bukrs TYPE bukrs
                !i_vkorg TYPE vkorg
      RAISING   cx_t100_msg.
    METHODS get_form_texts
      RETURNING VALUE(re_form_texts) TYPE ty_text_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: gs_text_fields TYPE ty_text_fields,                          " Substituir o tipo para a estrutura criada no DDIC.
          gv_langu       TYPE sy-langu.

    METHODS get_structure_fields
      RETURNING VALUE(re_structure_fields) TYPE tt_abap_componentdescr.
    METHODS get_texts_translated
      RETURNING VALUE(re_translate_texts) TYPE tt_translate_texts.
ENDCLASS.



CLASS zcl_form_texts IMPLEMENTATION.
  METHOD constructor.
    "Logica para atribuir o idioma ao atributo
    me->gv_langu = sy-langu.
  ENDMETHOD.

  METHOD get_form_texts.
    DATA(lt_structure_fields) = me->get_texts_translated( ).

    LOOP AT lt_structure_fields
    ASSIGNING FIELD-SYMBOL(<lt_structure_fields>).
      ASSIGN COMPONENT <lt_structure_fields>-field OF STRUCTURE re_form_texts TO FIELD-SYMBOL(<value>).
      <value> = <lt_structure_fields>-value.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_structure_fields.
    DATA: lr_stru TYPE REF TO cl_abap_structdescr.
    lr_stru ?= cl_abap_typedescr=>describe_by_data( me->gs_text_fields ).
    re_structure_fields = lr_stru->get_components( ).
  ENDMETHOD.

  METHOD get_texts_translated.
    DATA(lt_structure_fields) = me->get_structure_fields( ).

    IF NOT lt_structure_fields IS INITIAL.
*      SELECT langu field value
*      FROM ztabela_traducao
*      INTO TABLE re_translate_texts
*      FOR ALL ENTRIES IN lt_structure_fields
*      WHERE langu EQ me->gv_langu
*        AND field EQ lt_structure_fields-name(132).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
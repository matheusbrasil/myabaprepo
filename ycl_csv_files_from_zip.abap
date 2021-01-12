CLASS ycl_csv_files_from_zip DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_filetable TYPE STANDARD TABLE OF file_table WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_file,
        name       TYPE string,
        date       TYPE d,
        time       TYPE t,
        size       TYPE i,
        file_lines TYPE tt_filetable,
      END OF ty_file .
    TYPES:
      tt_files TYPE STANDARD TABLE OF ty_file WITH EMPTY KEY .

    METHODS constructor
      IMPORTING
        !it_binary_tab TYPE salv_xml_xline_tabtype OPTIONAL
        !ix_buffer     TYPE xstring OPTIONAL
        !iv_length     TYPE i OPTIONAL .
    METHODS read_zip_files
      RETURNING
        VALUE(rt_files) TYPE tt_files .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gr_zip TYPE REF TO cl_abap_zip .
    DATA gx_zip_file TYPE xstring .

    METHODS binary_to_xstring
      IMPORTING
        !input_length       TYPE i
        !binary_tab         TYPE salv_xml_xline_tabtype
      RETURNING
        VALUE(xstring_file) TYPE xstring .
    METHODS xstring_to_binary
      IMPORTING
        !xstring_file  TYPE xstring
      EXPORTING
        !binary_tab    TYPE salv_xml_xline_tabtype
        !output_length TYPE i .
    METHODS binary_to_string
      IMPORTING
        !input_length      TYPE i
        !binary_tab        TYPE salv_xml_xline_tabtype
      RETURNING
        VALUE(text_buffer) TYPE string .
    METHODS read_file
      IMPORTING
        !iv_buffer           TYPE xstring
      RETURNING
        VALUE(rt_file_lines) TYPE tt_filetable .
ENDCLASS.



CLASS ycl_csv_files_from_zip IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_CSV_FILES_FROM_ZIP->BINARY_TO_STRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT_LENGTH                   TYPE        I
* | [--->] BINARY_TAB                     TYPE        SALV_XML_XLINE_TABTYPE
* | [<-()] TEXT_BUFFER                    TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD binary_to_string.
    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = input_length
      IMPORTING
        text_buffer  = text_buffer
      TABLES
        binary_tab   = binary_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_CSV_FILES_FROM_ZIP->BINARY_TO_XSTRING
* +-------------------------------------------------------------------------------------------------+
* | [--->] INPUT_LENGTH                   TYPE        I
* | [--->] BINARY_TAB                     TYPE        SALV_XML_XLINE_TABTYPE
* | [<-()] XSTRING_FILE                   TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD binary_to_xstring.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = input_length
      IMPORTING
        buffer       = xstring_file
      TABLES
        binary_tab   = binary_tab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_CSV_FILES_FROM_ZIP->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_BINARY_TAB                  TYPE        SALV_XML_XLINE_TABTYPE(optional)
* | [--->] IX_BUFFER                      TYPE        XSTRING(optional)
* | [--->] IV_LENGTH                      TYPE        I(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    IF ix_buffer IS SUPPLIED.
      IF NOT ix_buffer IS INITIAL.
        me->gx_zip_file = ix_buffer.
      ELSE.
        "Raise an error
      ENDIF.
    ELSE.
      IF it_binary_tab IS INITIAL OR iv_length IS INITIAL.
        "Raise an error.
      ELSE.
        me->gx_zip_file = me->binary_to_xstring(
          EXPORTING
            input_length = iv_length
            binary_tab   = it_binary_tab " Table of RAW Lines of Length 255
        ).
      ENDIF.
    ENDIF.

    IF NOT me->gx_zip_file IS INITIAL.
      me->gr_zip = NEW cl_abap_zip( ).
      me->gr_zip->load(
        EXPORTING
          zip             = me->gx_zip_file
        EXCEPTIONS
          zip_parse_error = 1
          OTHERS          = 2
      ).
      IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        "Raise an error.
      ENDIF.
    ELSE.
      "Raise an error.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_CSV_FILES_FROM_ZIP->READ_FILE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_BUFFER                      TYPE        XSTRING
* | [<-()] RT_FILE_LINES                  TYPE        TT_FILETABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_file.
    DATA: ls_line TYPE file_table.

    me->xstring_to_binary(
      EXPORTING
        xstring_file  = iv_buffer
      IMPORTING
        binary_tab    = DATA(binary_tab)                 " Table of RAW Lines of Length 255
        output_length = DATA(output_length)
    ).

    DATA(lv_file_string) = me->binary_to_string(
      EXPORTING
        input_length = output_length
        binary_tab   = binary_tab " Table of RAW Lines of Length 255
    ).

    IF lv_file_string IS INITIAL.
      "raise error
    ENDIF.

*    DO.
*      IF lv_file_string CA cl_abap_char_utilities=>cr_lf.
*        SPLIT lv_file_string AT cl_abap_char_utilities=>cr_lf INTO ls_line
*                                                                   lv_file_string.
*        APPEND ls_line TO rt_file_lines.
*        CLEAR: ls_line.
*      ELSE.
*        IF NOT lv_file_string IS INITIAL.
*          ls_line = lv_file_string.
*          APPEND ls_line TO rt_file_lines.
*        ENDIF.
*        EXIT.
*      ENDIF.
*    ENDDO.

    SPLIT lv_file_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_file_lines.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_CSV_FILES_FROM_ZIP->READ_ZIP_FILES
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_FILES                       TYPE        TT_FILES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_zip_files.
    LOOP AT me->gr_zip->files ASSIGNING FIELD-SYMBOL(<file>).
      me->gr_zip->get(
        EXPORTING
          name                    = <file>-name
        IMPORTING
          content                 = DATA(lv_content)
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3
      ).
      IF sy-subrc <> 0.
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      APPEND VALUE #(
        name = <file>-name
        date = <file>-date
        time = <file>-time
        size = <file>-size
        file_lines = me->read_file( iv_buffer = lv_content )
       ) TO rt_files.

    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_CSV_FILES_FROM_ZIP->XSTRING_TO_BINARY
* +-------------------------------------------------------------------------------------------------+
* | [--->] XSTRING_FILE                   TYPE        XSTRING
* | [<---] BINARY_TAB                     TYPE        SALV_XML_XLINE_TABTYPE
* | [<---] OUTPUT_LENGTH                  TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD xstring_to_binary.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = xstring_file
      IMPORTING
        output_length = output_length
      TABLES
        binary_tab    = binary_tab.
  ENDMETHOD.
ENDCLASS.

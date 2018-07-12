*&---------------------------------------------------------------------*
*& Report  Y_AJUDA_PESQUISA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  y_ajuda_pesquisa message-id 0k .

tables: trdir. "Generated Table for View TRDIR

parameters: program like rs38m-programm. "ABAP program name

data: i_content(1024) occurs 0 with header line, msg(5000), answer.

read report program into i_content.

perform editor.

clear msg.
insert report program from i_content.
select single * from trdir where name = program.
if sy-subrc ne 0.
  clear trdir.
  trdir-name = program.
  trdir-clas = '$TMP'.
  trdir-dbna = ' '.
  trdir-fixpt = 'X'.
  trdir-rstat = 'P'.
  trdir-subc = '1'.
  trdir-rmand = sy-mandt.
  if trdir-sqlx lt 'R'.
    trdir-sqlx = 'R'.
  endif.
  modify trdir.
endif.
call function 'DB_COMMIT'.
msg = 'Lïnea:'.
generate report program line msg+10(10) message msg+50.
if sy-subrc <> 0.
  message i000 with msg(50) msg+50(50) msg+100(50) msg+150(50).
endif.
*---------------------------------------------------------------------*
*  FORM EDITOR
*---------------------------------------------------------------------*
form editor.
  data: u-index like sy-index, u-txtindex(50), u-title(50).
  concatenate 'Editando programa' program into u-title
              separated by space.
  editor-call for i_content title u-title.
  if sy-subrc = 4. stop. endif.
  call function 'EDITOR_SYNTAX_CHECK'
    exporting
      i_program       = program
    importing
      o_error_line    = u-index
      o_error_message = msg
    tables
      i_source        = i_content.
  if not msg is initial.
    u-txtindex = u-index - 2.
    shift u-txtindex left deleting leading space.
    concatenate 'Error de sintaxis en línea' u-txtindex into u-txtindex
                separated by space.
    call function 'POPUP_FOR_INTERACTION'
      exporting
        headline       = u-txtindex
        text1          = msg(60)
        text2          = msg+60(60)
        text3          = msg+120(60)
        text4          = msg+180(60)
        text5          = msg+240(60)
        text6          = msg+300(60)
        ticon          = 'E'
        button_1       = 'Finalizar.'
        button_2       = 'Volver al editor'
      importing
        button_pressed = answer
      exceptions
        others         = 1.
    if answer = '2'. perform editor. endif.
  endif.
endform.                               " EDITOR
*$*$ -------------------------------------------------------------- *$*$
*$*$                LIGHT - Serviços de Eletricidade                *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Desenvolvedor: Rodrigo Corrêa  - FAB18 - DBA                   *$*$
*$*$ Analista     : Rodrigo Corrêa  - FAB18 - DBA                   *$*$
*$*$ Data de criação: 04/05/2006                                    *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Projeto : BRACUSS                                              *$*$
*$*$ Esp.técnica:                                                   *$*$
*$*$ Finalidade : Move arquivos de um determinado diretório para    *$*$
*$*$              o a ser informado e disponibiliza o arquivo do    *$*$
*$*$              dia no diretório em qustão.                       *$*$
*$*$ -------------------------------------------------------------- *$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ Data       | Autor      | Descrição                            *$*$
*$*$ -------------------------------------------------------------- *$*$
REPORT zbefat_move_arquivos MESSAGE-ID zbracuss
                            NO STANDARD PAGE HEADING
                            LINE-SIZE 90.

TABLES: zbfat_file_d,
        zbfat_file_h.

*======================================================================*
* DECLARAÇÃO DE CONSTANTES                                             *
*======================================================================*
CONSTANTS: cc_tam_buffer TYPE i VALUE 1024,
           cc_023(4)     TYPE c VALUE '.023',
           cc_r0(2)      TYPE c VALUE 'R0',
           cc_r1(2)      TYPE c VALUE 'R1',
           cc_r2(2)      TYPE c VALUE 'R2'.

*======================================================================*
* DECLARAÇÃO DE TABELAS                                                *
*======================================================================*

*======================================================================*
* DECLARAÇÃO DE TABELAS INTERNAS                                       *
*======================================================================*
DATA: ti_file_list LIKE epsfili OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF ti_file_log OCCURS 0,
        name   LIKE epsfili-name,
        status TYPE c,
      END OF ti_file_log.

* SO-12815 (09/4/2007) - Welington  -  Inicio
DATA: BEGIN OF ti_file_cont OCCURS 0,
        tensao  LIKE zbfat_file_h-tensao,
        data    LIKE zbfat_file_h-data,
        hora    LIKE zbfat_file_h-hora,
        dremov  LIKE zbfat_file_h-d_remov,
        d1remo  LIKE zbfat_file_h-d1_remo,
        ddevol  LIKE zbfat_file_h-d_devol,
        d1mant  LIKE zbfat_file_h-d1_mant,
      END OF ti_file_cont.

DATA  BEGIN OF ti_file_h OCCURS 0.
        INCLUDE STRUCTURE zbfat_file_h.
DATA  END OF ti_file_h.
* SO-12815 (09/4/2007) - Welington  -  Inicio

DATA: ti_file_log_dev LIKE ti_file_log OCCURS 0 WITH HEADER LINE.

* SO-12815 (30/3/2007) - Welington  -  Inicio
DATA: ti_file_log_d1  LIKE ti_file_log OCCURS 0 WITH HEADER LINE.
* SO-12815 (30/3/2007) - Welington  -  Fim

* Contém os dados dos arquivos movidos.
DATA: BEGIN OF ti_arquivo OCCURS 0,
        dado(cc_tam_buffer),
      END OF ti_arquivo.

*======================================================================*
* DECLARAÇÃO DE VARIÁVEIS                                              *
*======================================================================*
DATA: vg_muda,
      vg_erro,
      vg_status(18),
      vg_adatsoll LIKE te418-adatsoll.

DATA: wc_termschl LIKE te418-termschl.

DATA: wc_lote(2).
DATA: wc_achou.

*======================================================================*
* TELA DE SELEÇÂO                                                      *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-s00.
PARAMETER: p_data LIKE sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s01.
PARAMETER: p_dir_e LIKE epsf-epsdirnam OBLIGATORY,
           p_dir_s LIKE epsf-epsdirnam OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*======================================================================*
* START-OF-SELECTION                                                   *
*======================================================================*
START-OF-SELECTION.

* Recupera lote.
  PERFORM z_busca_lote.

* Busca todos os arquivo do diretório informado.
  PERFORM z_busca_arquivos USING p_dir_e.

* Move o arquivo para o caminho de processados
  PERFORM z_move_arquivos.

* FAT_SO8502 (Welington) - 12/12/2006  -  Inicio *********************
* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*  PERFORM z_recupera_dia_util.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815
* FAT_SO8502 (Welington) - 12/12/2006  -  Fim    *********************

* Recupero o arquivo do próximo dia útil a ser disponibilizado no dia.
  PERFORM z_recupera_arquivo.

*======================================================================*
* START-OF-SELECTION                                                   *
*======================================================================*
END-OF-SELECTION.

* Imprime os arquivos movidos.
  PERFORM z_imprime_arquivos_transf.

*---------------------------------------------------------------------*
* Form  z_busca_arquivos
*---------------------------------------------------------------------*
FORM z_busca_arquivos USING p_dir.

*  DATA: lc_data(06)  TYPE c.
* Limpa a tabela de arquivos.
  REFRESH ti_file_list.

* Busca todos os nomes dos arquivos que estão no diretório de entrada
  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = p_dir
    TABLES
      dir_list               = ti_file_list
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.

  IF sy-subrc NE 0.
*   Erro na obtenção dos arquivos a serem processados.
    MESSAGE i000(zbracuss) WITH text-e01.
    STOP.
  ENDIF.

*  CONCATENATE vg_adatsoll+6(2) vg_adatsoll+4(2) vg_adatsoll+2(2)
*         INTO lc_data.
*
*  CLEAR wc_achou.
*  LOOP AT ti_file_list.
*
*     IF ti_file_list-name+2(6) = lc_data.
*        wc_achou = 'S'.
*     ENDIF.
*
*  ENDLOOP.
*
*  IF wc_achou = ' '.
**   De acordo com a data do cronograma, não existe arquivo "TXT"
**   disponível.
*    MESSAGE i000 WITH text-t20.
*    STOP.
*  ENDIF.

ENDFORM.                    " z_busca_arquivos

*---------------------------------------------------------------------*
* Form  z_move_arquivos
*---------------------------------------------------------------------*
FORM z_move_arquivos.

  DATA: vl_arq_orig(230), "Local de origem do arquivo
        vl_arq_dest(230), "Local de destino do arquivo
        vl_data_arquivo(6),
        vl_data(6),
        vl_dia(02),
        vl_comando(150).

* FAT_SO8502 (Welington) - 12/12/2006  -  Inicio *********************
* Monta a data dd.mm.aa
*  MOVE p_data+6(2) TO vl_dia.
*  ADD 1 TO vl_dia.
*  UNPACK vl_dia TO vl_dia.

*  CONCATENATE vl_dia p_data+4(2) p_data+2(2)
*         INTO vl_data.

  CONCATENATE sy-datum+2(2) sy-datum+4(2) sy-datum+6(2)
         INTO vl_data.

* FAT_SO8502 (Welington) - 12/12/2006  -  Fim    *********************

* Processa todos os arquivo encotrados.
  LOOP AT ti_file_list.

* FAT_SO8502 (Welington) - 12/12/2006  -  Inicio *********************

    CONCATENATE ti_file_list-name+6(2) ti_file_list-name+4(2)
                ti_file_list-name+2(2)
           INTO vl_data_arquivo.

*   Não mover o arquivo D+1 do dia e após.
    IF ti_file_list-name(2)  EQ cc_r1   AND
       vl_data_arquivo       GE vl_data.
* SO-12815 (30/3/2007) - Welington  -  Inicio
      CLEAR ti_file_log_d1-status.
      MOVE ti_file_list-name TO ti_file_log_d1-name.
      APPEND ti_file_log_d1.

      IF ti_file_list-name(1) = 'M'.
        ti_file_cont-tensao  = 'MT'.
      ELSE.
        ti_file_cont-tensao  = 'BT'.
      ENDIF.
      ti_file_cont-data    =  sy-datum.
      ti_file_cont-hora    =  sy-uzeit.
      ADD 1 TO ti_file_cont-d1mant.
      APPEND ti_file_cont.
      CLEAR ti_file_cont.

      zbfat_file_d-dcracao   =  text-t50.         "Arquivo D+1 mantido

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*      PERFORM atualiza_tabela_file_d.
      PERFORM atualiza_tabela_file_d USING 'X'.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815

* SO-12815 (30/3/2007) - Welington  -  Fim
      CONTINUE.
    ENDIF.
* FAT_SO8502 (Welington) - 12/12/2006  -  Fim    *********************

*   Monta os caminhos dos arquivos.
    CONCATENATE: p_dir_e ti_file_list-name INTO vl_arq_orig,
                 p_dir_s ti_file_list-name INTO vl_arq_dest.

*   Move o arquivo para o diretório de saída.
    PERFORM z_copia_arquivo USING vl_arq_orig vl_arq_dest.

*   Permissão do arquivo.
    CONCATENATE 'chmod 777' vl_arq_dest
           INTO vl_comando SEPARATED BY space.

    CALL 'SYSTEM' ID 'COMMAND' FIELD vl_comando.

  ENDLOOP.

ENDFORM.                    " z_move_arquivos

*---------------------------------------------------------------------*
* Form  z_imprime_arquivos_transf
*---------------------------------------------------------------------*
FORM z_imprime_arquivos_transf.

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
** Ordena a tabela interna.
*  SORT ti_file_log BY name status.
*
** Tabela de arquivos copiados.
*  LOOP AT ti_file_log.
*
*    AT FIRST.
*      ULINE.
*    ENDAT.
*
**   Muda a cor da linha a ser impressa, conforme seu status.
*    PERFORM z_muda_cor USING ti_file_log-status.
*
**   Imprime o arquivo com seu respctivo status.
*    WRITE: / sy-vline, ti_file_log-name, 60 vg_status, 90 sy-vline.
*
*    AT LAST.
*      ULINE.
*    ENDAT.
*
*  ENDLOOP.
*
** Tabela de arquivos devolvidos.
*  LOOP AT ti_file_log_dev.
*
**   Muda a cor da linha a ser impressa, conforme seu status.
*    PERFORM z_muda_cor USING space.
*
**   Imprime o arquivo com seu respctivo status.
*    WRITE: / sy-vline, ti_file_log_dev-name, 60 text-r03, 90 sy-vline.
*
*    AT LAST.
*      ULINE.
*    ENDAT.
*
*  ENDLOOP.
*
** SO-12815 (30/3/2007) - Welington  -  Inicio
** Tabela de arquivos D+1.
*  LOOP AT ti_file_log_d1.
*
**   Muda a cor da linha a ser impressa, conforme seu status.
*    PERFORM z_muda_cor USING space.
*
**   Imprime o arquivo com seu respctivo status.
*    WRITE: / sy-vline, ti_file_log_d1-name, 60 text-r04, 90 sy-vline.
*
*    AT LAST.
*      ULINE.
*    ENDAT.
*
*  ENDLOOP.
* SO-12815 (30/3/2007) - Welington  -  Fim

  DATA: e_file_cont LIKE ti_file_cont.
  DATA: BEGIN OF ti_file_aux OCCURS 0,
          tensao LIKE zbfat_file_h-tensao,
          d_remov TYPE i, "LIKE zbfat_file_h-d_remov,
          d1_remo TYPE i, "LIKE zbfat_file_h-d1_remo,
          d_devol TYPE i, "LIKE zbfat_file_h-d_devol,
          d1_mant TYPE i, "LIKE zbfat_file_h-d1_mant,
          staexe LIKE zbfat_file_h-staexe,
        END OF ti_file_aux.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815

* SO-12815 (09/4/2007) - Welington  -  Inicio
  DATA: ld_data_bt  LIKE  sy-datum,
        ld_hora_bt  LIKE  sy-uzeit,
        ld_data_mt  LIKE  sy-datum,
        ld_hora_mt  LIKE  sy-uzeit.

  LOOP AT ti_file_cont.
* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*    AT NEW tensao.
*      IF ti_file_cont-tensao = 'MT'.
*        ld_data_mt = ti_file_cont-data.
*        ld_hora_mt = ti_file_cont-hora.
*      ELSE.
*        ld_data_bt = ti_file_cont-data.
*        ld_hora_bt = ti_file_cont-hora.
*      ENDIF.
*    ENDAT.
*
*    ti_file_h-mandt    =  sy-mandt.
*    ti_file_h-tensao   =  ti_file_cont-tensao.
*    ti_file_h-d_remov  =  ti_file_cont-dremov.
*    ti_file_h-d1_remo  =  ti_file_cont-d1remo.
*    ti_file_h-d_devol  =  ti_file_cont-ddevol.
*    ti_file_h-d1_mant  =  ti_file_cont-d1mant.
*
*    IF vg_erro = 'X'.
*      ti_file_aux-staexe   = text-e03.
*    ELSE.
*      ti_file_aux-staexe   = text-m03.
*    ENDIF.
**     ti_file_h-mensag   =  ti_file_cont-mensag.
*    COLLECT ti_file_h.

    CLEAR e_file_cont.
    e_file_cont = ti_file_cont.

    AT NEW tensao.
      IF e_file_cont-tensao = 'MT'.
        ld_data_mt = e_file_cont-data.
        ld_hora_mt = e_file_cont-hora.
      ELSE.
        ld_data_bt = e_file_cont-data.
        ld_hora_bt = e_file_cont-hora.
      ENDIF.
    ENDAT.

    CLEAR ti_file_aux.
    ti_file_aux-tensao   =  e_file_cont-tensao.
    ti_file_aux-d_remov  =  e_file_cont-dremov.
    ti_file_aux-d1_remo  =  e_file_cont-d1remo.
    ti_file_aux-d_devol  =  e_file_cont-ddevol.
    ti_file_aux-d1_mant  =  e_file_cont-d1mant.

    IF vg_erro = 'X'.
      ti_file_aux-staexe   = text-e03.
    ELSE.
      ti_file_aux-staexe   = text-m03.
    ENDIF.
    COLLECT ti_file_aux.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815
  ENDLOOP.

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*  LOOP AT ti_file_h.
*    ti_file_h-usuario  =  sy-ucomm.
*    IF ti_file_h-tensao = 'MT'.
*      ti_file_h-data = ld_data_mt.
*      ti_file_h-hora = ld_hora_mt.
*    ELSE.
*      ti_file_h-data = ld_data_bt.
*      ti_file_h-hora = ld_hora_bt.
*    ENDIF.
*    MODIFY ti_file_h INDEX sy-tabix.
*  ENDLOOP.
*
*  INSERT zbfat_file_h FROM TABLE ti_file_h.

  LOOP AT ti_file_aux.
    ti_file_h-usuario  =  sy-uname.
    ti_file_h-mandt    =  sy-mandt.
    ti_file_h-tensao   =  ti_file_aux-tensao.
    ti_file_h-d_remov  =  ti_file_aux-d_remov.
    ti_file_h-d1_remo  =  ti_file_aux-d1_remo.
    ti_file_h-d_devol  =  ti_file_aux-d_devol.
    ti_file_h-d1_mant  =  ti_file_aux-d1_mant.
    ti_file_h-staexe   =  ti_file_aux-staexe.

    IF ti_file_h-tensao = 'MT'.
      ti_file_h-data = ld_data_mt.
      ti_file_h-hora = ld_hora_mt.
    ELSE.
      ti_file_h-data = ld_data_bt.
      ti_file_h-hora = ld_hora_bt.
    ENDIF.
    APPEND ti_file_h.
  ENDLOOP.

  MODIFY zbfat_file_h FROM TABLE ti_file_h.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815
* SO-12815 (09/4/2007) - Welington  -  Fim

ENDFORM.                    " z_imprime_arquivos_transf

*---------------------------------------------------------------------*
* Form  z_muda_cor
*---------------------------------------------------------------------*
FORM z_muda_cor USING p_status.

  IF p_status IS INITIAL.

    MOVE text-s02 TO vg_status.

    IF vg_muda IS INITIAL.
      FORMAT COLOR COL_POSITIVE INTENSIFIED ON.
      MOVE 'X' TO vg_muda.
    ELSE.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      CLEAR vg_muda.
    ENDIF.

  ELSE.

    vg_erro = 'X'.
    MOVE text-e02 TO vg_status.

    IF vg_muda IS INITIAL.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED ON.
      MOVE 'X' TO vg_muda.
    ELSE.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      CLEAR vg_muda.
    ENDIF.

  ENDIF.

ENDFORM.                    " z_muda_cor

*---------------------------------------------------------------------*
* Form  z_copia_arquivo
*---------------------------------------------------------------------*
FORM z_copia_arquivo USING l_orig TYPE any
                           l_dest TYPE any.

* Arquivo corrente.
  MOVE ti_file_list-name TO ti_file_log-name.

* SO-12815 (09/4/2007) - Welington  -  Inicio
  IF ti_file_list-name(1) = 'M'.
    ti_file_cont-tensao  = 'MT'.
  ELSE.
    ti_file_cont-tensao  = 'BT'.
  ENDIF.
  ti_file_cont-data    =  sy-datum.
  ti_file_cont-hora    =  sy-uzeit.
* Início - Leonardo Macedo - 24.05.2007 - SO 12815
*  ADD 1 TO ti_file_cont-dremov.
  IF ti_file_list-name(2)  EQ cc_r1.
    ADD 1 TO ti_file_cont-d1remo.
  ELSE.
    ADD 1 TO ti_file_cont-dremov.
  ENDIF.
* Fim - Leonardo Macedo - 24.05.2007 - SO 12815
  APPEND ti_file_cont.
  CLEAR ti_file_cont.
* SO-12815 (09/4/2007) - Welington  -  Fim

  zbfat_file_d-dcracao   =  text-t52.   "Arquivo Movido de A para B

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*  PERFORM atualiza_tabela_file_d.
  PERFORM atualiza_tabela_file_d USING space.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815

* Abre o arquivo a ser copiado.
  OPEN DATASET l_orig IN TEXT MODE ENCODING DEFAULT FOR INPUT
                         IGNORING CONVERSION ERRORS.

* Cria a cópia do arquivo em questão.
  OPEN DATASET l_dest IN TEXT MODE ENCODING DEFAULT FOR OUTPUT
                         IGNORING CONVERSION ERRORS.

* Trata abertura do arquivo.
  IF sy-subrc IS INITIAL.
    CLEAR ti_file_log-status.
    APPEND ti_file_log.
  ELSE.
    MOVE 'X' TO ti_file_log-status.
    APPEND ti_file_log.
    EXIT.
  ENDIF.

  DO.

*   Limpa a header line.
    CLEAR ti_arquivo.

*   Lê os dados do arquivo corrente.
    READ DATASET l_orig INTO ti_arquivo.

*   Todos os dados do arquivo de entrada foram lidos.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

*   Transfere a linha para o arquivo de backup.
    TRANSFER ti_arquivo-dado TO l_dest.

  ENDDO.

* Fecha arquivo corrente.
  CLOSE DATASET l_orig.

* Fecha arquivo copiado.
  CLOSE DATASET l_dest.

* Deleta o arquivo corrente do diretório de origem.
  DELETE DATASET l_orig.

ENDFORM.                    " z_copia_arquivo

*---------------------------------------------------------------------*
* Form  z_busca_lote
*---------------------------------------------------------------------*
FORM z_busca_lote .

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*  DATA: lc_adatsoll LIKE te418-adatsoll,
*        vl_pri_dia LIKE sy-datum,
*        vl_ult_dia LIKE sy-datum.
*
* Busca lotes de processamento do dia.
*  SELECT termschl
*    INTO wc_termschl
*    FROM te418 UP TO 1 ROWS                             "#EC CI_GENBUFF
*   WHERE termschl LIKE 'B%'
*     AND adatsoll GE   p_data
*     AND ts_dyn   EQ   '1'.
*  ENDSELECT.
*
*  IF NOT sy-subrc IS INITIAL.

  DATA lc_pdata LIKE sy-datum.

  MOVE p_data TO lc_pdata.
  ADD 1 TO lc_pdata.

  DO 30 TIMES.
    ADD 1 TO lc_pdata.

    CLEAR wc_termschl.
    SELECT termschl
      INTO wc_termschl
      FROM te418 UP TO 1 ROWS
     WHERE termschl LIKE 'B%'
       AND adatsoll EQ   lc_pdata
       AND ts_dyn   EQ   '1'.
    ENDSELECT.

    IF wc_termschl+01(02) GT '20'.
      CLEAR wc_termschl.
      SELECT termschl
        INTO wc_termschl
        FROM te418 UP TO 1 ROWS
       WHERE termschl LIKE 'B01%'
         AND adatsoll EQ   lc_pdata
         AND ts_dyn   EQ   '1'.
      ENDSELECT.
    ENDIF.

    IF NOT wc_termschl IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

  IF wc_termschl IS INITIAL.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815
*    MESSAGE i000 WITH 'Favor verificar disponibilidade dos lotes'.
    MESSAGE i000 WITH text-t22.
    STOP.
  ENDIF.

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*  wc_lote = wc_termschl+1(2).
*
**  ADD 2 TO lc_lote.
*
*  UNPACK wc_lote TO wc_lote.
*
*  IF wc_lote GT '20'.
*    CASE wc_lote.
*      WHEN '21'. wc_lote = '01'.
*      WHEN '22'. wc_lote = '02'.
*      WHEN OTHERS. LEAVE PROGRAM.
*    ENDCASE.
*  ENDIF.
*
*  CONCATENATE 'B' wc_lote '%' INTO wc_termschl.
*
*  PERFORM z_recuprera_competencia CHANGING vl_pri_dia vl_ult_dia.
*
** Busca data de leitura para o lote correspondente.
*  SELECT adatsoll
*    INTO lc_adatsoll
*   FROM te418 UP TO 1 ROWS                              "#EC CI_GENBUFF
*   WHERE termschl LIKE wc_termschl
*     AND ts_dyn   EQ   '1'
*    AND adatsoll  GE   vl_pri_dia
*    AND adatsoll  LE   vl_ult_dia.
*  ENDSELECT.
*
*  MOVE: lc_adatsoll TO vg_adatsoll.

  MOVE lc_pdata TO vg_adatsoll.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815

ENDFORM.                    " z_busca_lote

*---------------------------------------------------------------------*
* Form  z_recupera_arquivo
*---------------------------------------------------------------------*
FORM z_recupera_arquivo.

  DATA: l_orig(230),
        l_dest(230),
        vl_data(6),
        vl_comando(150).

* Monta a data dd.mm.aa
  CONCATENATE vg_adatsoll+6(2) vg_adatsoll+4(2) vg_adatsoll+2(2)
         INTO vl_data.

* Busca os arquivos no diretório de saída.
  PERFORM z_busca_arquivos USING p_dir_s.

* Move somente os arquivos com a data selecionada.
  LOOP AT ti_file_list.

* FAT_SO8502 (Welington) - 12/12/2006  -  Inicio *********************
*    IF ti_file_list-name(2)    EQ cc_r0   AND
*       ti_file_list-name+2(6)  EQ vl_data AND
*       ti_file_list-name+14(4) EQ cc_023.

    IF ( ti_file_list-name(2) EQ cc_r0 OR ti_file_list-name(2) EQ cc_r2 ) AND
       ti_file_list-name+2(6) EQ vl_data.
* FAT_SO8502 (Welington) - 12/12/2006  -  Fim    *********************

*     Monta os caminhos.
      CONCATENATE: p_dir_s ti_file_list-name INTO l_orig,
                   p_dir_e ti_file_list-name INTO l_dest.

*     Abre o arquivo a ser copiado.
      OPEN DATASET l_orig IN TEXT MODE ENCODING DEFAULT FOR INPUT
                             IGNORING CONVERSION ERRORS.

*     Cria a cópia do arquivo em questão.
      OPEN DATASET l_dest IN TEXT MODE ENCODING DEFAULT FOR OUTPUT
                             IGNORING CONVERSION ERRORS.

*     Trata abertura do arquivo.
      IF NOT sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      DO.

*       Limpa a header line.
        CLEAR ti_arquivo.

*       Lê os dados do arquivo corrente.
        READ DATASET l_orig INTO ti_arquivo.

*       Todos os dados do arquivo foram lidos.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.

*       Transfere os dados.
        TRANSFER ti_arquivo-dado TO l_dest.

      ENDDO.

*     Fecha arquivo corrente.
      CLOSE DATASET l_orig.

*     Fecha arquivo copiado.
      CLOSE DATASET l_dest.

*     Permissão do arquivo.
      CONCATENATE 'chmod 777' l_dest
             INTO vl_comando SEPARATED BY space.

      CALL 'SYSTEM' ID 'COMMAND' FIELD vl_comando.

*     Guarda na tabela de log o arquivo devolvido.
      MOVE ti_file_list-name TO ti_file_log_dev-name.
      APPEND ti_file_log_dev.

* SO-12815 (09/4/2007) - Welington  -  Inicio
      IF ti_file_list-name(1) = 'M'.
        ti_file_cont-tensao  = 'MT'.
      ELSE.
        ti_file_cont-tensao  = 'BT'.
      ENDIF.
      ti_file_cont-data    =  sy-datum.
      ti_file_cont-hora    =  sy-uzeit.
      ADD 1 TO ti_file_cont-ddevol.
      APPEND ti_file_cont.
      CLEAR ti_file_cont.
* SO-12815 (09/4/2007) - Welington  -  Fim

      zbfat_file_d-dcracao   =  text-t53.    "Arquivo Movido de B para A

* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*      PERFORM atualiza_tabela_file_d.
      PERFORM atualiza_tabela_file_d USING space.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815

    ENDIF.

  ENDLOOP.

ENDFORM.                    " z_recupera_arquivo

*---------------------------------------------------------------------*
* Form  z_recuprera_competencia
*---------------------------------------------------------------------*
FORM z_recuprera_competencia CHANGING p_pri_dia TYPE any
                                      p_ult_dia TYPE any.

  DATA: lc_datum  LIKE  sy-datum.

  lc_datum = sy-datum.

*  IF wc_lote GT '20'.
*     add  1  to  lc_datum.
*  ENDIF.

* Primeiro dia da competência em questão.
  CONCATENATE lc_datum(6) '01' INTO p_pri_dia.

* Ultimo dia da competência em questão.
  CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
    EXPORTING
      day_in            = lc_datum
    IMPORTING
      last_day_of_month = p_ult_dia
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc NE 0.
  ENDIF.

ENDFORM.                    " z_recuprera_competencia

*&---------------------------------------------------------------------*
*&      Form  z_recupera_dia_util
*&---------------------------------------------------------------------*
FORM z_recupera_dia_util .

  DATA: lc_datum          TYPE d,
        lc_dia_util       TYPE d,
        lc_ident_geral(2) TYPE c,
        lc_adatsoll LIKE te418-adatsoll.


  lc_datum = p_data + 1.

* Recuperar a próxima leitura programada.
  SELECT adatsoll
    INTO lc_adatsoll
   FROM te418 UP TO 1 ROWS                              "#EC CI_GENBUFF
   WHERE termschl LIKE wc_termschl
     AND ts_dyn   EQ   '1'
    AND adatsoll  GE   lc_datum.
  ENDSELECT.

*  lc_ident_geral = 'ZG'.
*
*  CALL FUNCTION 'ISU_GET_NEXT_WORKDAY'
*       EXPORTING
*         I_DATE         = vg_adatsoll
*         I_CALENDAR1    = lc_ident_geral
*       IMPORTING
*         E_WORKDAY      = lc_dia_util
*       EXCEPTIONS
*         CALENDAR_ERROR = 1
*         OTHERS         = 2.

  IF sy-subrc = 0.
    vg_adatsoll = lc_adatsoll.
  ENDIF.

ENDFORM.                    " z_recupera_dia_util

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TABELA_FILE_D
*&---------------------------------------------------------------------*
* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*FORM atualiza_tabela_file_d .
FORM atualiza_tabela_file_d USING v_mandi.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815

  IF ti_file_list-name(1) = 'M'.
    zbfat_file_d-tensao    = 'MT'.
  ELSE.
    zbfat_file_d-tensao    = 'BT'.
  ENDIF.

  zbfat_file_d-data      =  sy-datum.
  zbfat_file_d-hora      =  sy-uzeit.
  zbfat_file_d-usuario   =  sy-uname.
  zbfat_file_d-arquivo   =  ti_file_list-name.
  zbfat_file_d-diret_r   =  p_dir_e(27).
  zbfat_file_d-diret_a   =  p_dir_e+27(20).
* Início - Leonardo Macedo - 07.05.2007 - SO 12815
*  zbfat_file_d-diret_b   =  p_dir_s+27(29).
  IF v_mandi IS INITIAL.
    zbfat_file_d-diret_b   =  p_dir_s+27(29).
  ENDIF.
* Fim - Leonardo Macedo - 07.05.2007 - SO 12815
  MODIFY zbfat_file_d  FROM  zbfat_file_d.

  IF sy-subrc IS INITIAL.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " ATUALIZA_TABELA_FILE_D



*Text elements
*-------------
* IE02     Não movido
* IM03     Script encerrado com sucesso.
* IR04     D+1 Mantidos
* IS00     Seleção
* IS01     Entrada
* IS02     Movido com sucesso
* IT20     De acordo com a data do cronograma, não existe arquivo "TXT" disp
* IT22     Favor verificar disponibilidade dos lotes
* IT50     Arquivo D+1 mantido
* IT52     Arquivos movidos de A para B
* IT53     Arquivos movidos de B para A



*Selection texts
*---------------
*SP_DATA          Data de processamento
*SP_DIR_E         Diretório de entrada
*SP_DIR_S         Diretório de saída


*Messages
*-------------
* Message class: ZBRACUSS
* 000 & & & &

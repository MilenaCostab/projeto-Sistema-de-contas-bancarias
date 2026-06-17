       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO01S.
       AUTHOR. PROJETO6.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQCLIEN ASSIGN TO "../input/CLIENTES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-CLIEN.

           SELECT ARQTRXIN ASSIGN TO "../input/TRANSACOES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-TRXIN.

           SELECT ARQERROS ASSIGN TO "../output/ERROS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ERROS.

           SELECT ARQRELAT ASSIGN TO "../output/RELATORIO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-RELAT.

           SELECT ARQLOG   ASSIGN TO "../output/LOG.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-LOG.

           SELECT TBCLIEN  ASSIGN TO "../db/CLIENTES"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS IDX-CLI-ID
               FILE STATUS  IS WS-FS-TBCLI.

           SELECT TBTRXIN  ASSIGN TO "../db/TRANSACOES"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS IDX-TRX-ID
               FILE STATUS  IS WS-FS-TBTRX.

           SELECT TBERROS  ASSIGN TO "../db/ERROS"
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS IDX-ERR-ID
               FILE STATUS  IS WS-FS-TBERR.

       DATA DIVISION.
       FILE SECTION.

       FD ARQCLIEN.
       01 FS-CLIEN             PIC X(44).

       FD ARQTRXIN.
       01 FS-TRXIN             PIC X(20).

       FD ARQERROS.
       01 FS-ERROS             PIC X(120).

       FD ARQRELAT.
       01 FS-RELAT             PIC X(120).

       FD ARQLOG.
       01 FS-LOG               PIC X(120).

       FD TBCLIEN.
           COPY "../copybooks/CPYIDXCL.CPY".

       FD TBTRXIN.
           COPY "../copybooks/CPYIDXTR.CPY".

       FD TBERROS.
           COPY "../copybooks/CPYIDXER.CPY".

       WORKING-STORAGE SECTION.

           COPY "../copybooks/CPYCLIEN.CPY".
           COPY "../copybooks/CPYTRXIN.CPY".

       01 WS-FS-CLIEN          PIC XX VALUE SPACES.
       01 WS-FS-TRXIN          PIC XX VALUE SPACES.
       01 WS-FS-ERROS          PIC XX VALUE SPACES.
       01 WS-FS-RELAT          PIC XX VALUE SPACES.
       01 WS-FS-LOG            PIC XX VALUE SPACES.
       01 WS-FS-TBCLI          PIC XX VALUE SPACES.
       01 WS-FS-TBTRX          PIC XX VALUE SPACES.
       01 WS-FS-TBERR          PIC XX VALUE SPACES.

       01 WS-CTR-CLIEN-LIDOS   PIC 9(06) VALUE 0.
       01 WS-CTR-CLIEN-OK      PIC 9(06) VALUE 0.
       01 WS-CTR-CLIEN-ERRO    PIC 9(06) VALUE 0.
       01 WS-CTR-TRX-LIDAS     PIC 9(06) VALUE 0.
       01 WS-CTR-TRX-OK        PIC 9(06) VALUE 0.
       01 WS-CTR-TRX-ERRO      PIC 9(06) VALUE 0.
       01 WS-CTR-COMMIT        PIC 9(06) VALUE 0.
       01 WS-CTR-ERR-ID        PIC 9(05) VALUE 0.

       01 WS-EOF-CLIEN         PIC X VALUE 'N'.
       01 WS-EOF-TRXIN         PIC X VALUE 'N'.
       01 WS-EOF-REL           PIC X VALUE 'N'.
       01 WS-ERRO-FLAG         PIC X VALUE 'N'.
       01 WS-MSG-ERRO          PIC X(80) VALUE SPACES.

       01 WS-DATA-HOJE.
           05 WS-DH-ANO        PIC X(04).
           05 FILLER           PIC X(01) VALUE '-'.
           05 WS-DH-MES        PIC X(02).
           05 FILLER           PIC X(01) VALUE '-'.
           05 WS-DH-DIA        PIC X(02).

       01 WS-LINHA             PIC X(120) VALUE SPACES.
       01 WS-SALDO-FMT         PIC ZZZZZZZ9.

       01 WS-REL-DET.
           05 WS-RD-CLI-ID     PIC ZZZZZ9.
           05 FILLER           PIC X VALUE SPACES.
           05 WS-RD-NOME       PIC X(30).
           05 FILLER           PIC X VALUE SPACES.
           05 WS-RD-OPER       PIC X(10).
           05 FILLER           PIC X VALUE SPACES.
           05 WS-RD-STATUS     PIC X(25).

       PROCEDURE DIVISION.

       0000-INICIO.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROC-CLIENTES
           PERFORM 3000-PROC-TRANSACOES
           PERFORM 4000-GERAR-RELATORIO
           PERFORM 9000-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WS-DH-ANO
           MOVE FUNCTION CURRENT-DATE(5:2) TO WS-DH-MES
           MOVE FUNCTION CURRENT-DATE(7:2) TO WS-DH-DIA

           OPEN OUTPUT ARQERROS
           OPEN OUTPUT ARQRELAT
           OPEN OUTPUT ARQLOG

           OPEN I-O TBCLIEN
           IF WS-FS-TBCLI = '35'
               OPEN OUTPUT TBCLIEN
               CLOSE TBCLIEN
               OPEN I-O TBCLIEN
           END-IF

           OPEN I-O TBTRXIN
           IF WS-FS-TBTRX = '35'
               OPEN OUTPUT TBTRXIN
               CLOSE TBTRXIN
               OPEN I-O TBTRXIN
           END-IF

           OPEN I-O TBERROS
           IF WS-FS-TBERR = '35'
               OPEN OUTPUT TBERROS
               CLOSE TBERROS
               OPEN I-O TBERROS
           END-IF

           MOVE "=================================================="
               TO WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "  SISTEMA DE CONTAS BANCARIAS - SIMULADOR"
               TO WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE SPACES TO WS-LINHA
           STRING "  Data: " WS-DATA-HOJE
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "=================================================="
               TO WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA.

       2000-PROC-CLIENTES.
           MOVE SPACES TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA
           MOVE "--- PROCESSANDO CLIENTES ---" TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA

           OPEN INPUT ARQCLIEN
           IF WS-FS-CLIEN NOT = '00'
               MOVE 0 TO IDX-ERR-CLI-ID
               MOVE 'Erro ao abrir CLIENTES.TXT' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               GO TO 2000-FIM
           END-IF

           PERFORM UNTIL WS-EOF-CLIEN = 'S'
               READ ARQCLIEN INTO WS-REG-CLIENTE
                   AT END
                       MOVE 'S' TO WS-EOF-CLIEN
                   NOT AT END
                       ADD 1 TO WS-CTR-CLIEN-LIDOS
                       PERFORM 2100-VALIDAR-CLIENTE
               END-READ
           END-PERFORM

           CLOSE ARQCLIEN.
       2000-FIM.
           EXIT.

       2100-VALIDAR-CLIENTE.
           MOVE 'N' TO WS-ERRO-FLAG

           IF WS-CLI-NOME = SPACES
               MOVE WS-CLI-ID  TO IDX-ERR-CLI-ID
               MOVE 'Nome obrigatorio' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-CLIEN-ERRO
               MOVE 'S' TO WS-ERRO-FLAG
           END-IF

           IF WS-ERRO-FLAG = 'N'
               PERFORM 2200-UPSERT-CLIENTE
           END-IF.

       2200-UPSERT-CLIENTE.
           MOVE WS-CLI-ID TO IDX-CLI-ID
           READ TBCLIEN KEY IS IDX-CLI-ID

               INVALID KEY
                   MOVE WS-CLI-ID    TO IDX-CLI-ID
                   MOVE WS-CLI-NOME  TO IDX-CLI-NOME
                   MOVE WS-CLI-SALDO TO IDX-CLI-SALDO
                   MOVE WS-DATA-HOJE TO IDX-CLI-DTATU
                   WRITE WS-REG-IDX-CLI
                   IF WS-FS-TBCLI NOT = '00'
                       MOVE WS-CLI-ID TO IDX-ERR-CLI-ID
                       MOVE SPACES TO WS-MSG-ERRO
                       STRING "Erro INSERT FS=" WS-FS-TBCLI
                           DELIMITED SIZE INTO WS-MSG-ERRO
                       PERFORM 8000-GRAVAR-ERRO
                       ADD 1 TO WS-CTR-CLIEN-ERRO
                       MOVE 'S' TO WS-ERRO-FLAG
                   ELSE
                       MOVE 'INSERT OK' TO WS-RD-STATUS
                   END-IF

               NOT INVALID KEY
                   MOVE WS-CLI-NOME  TO IDX-CLI-NOME
                   MOVE WS-CLI-SALDO TO IDX-CLI-SALDO
                   MOVE WS-DATA-HOJE TO IDX-CLI-DTATU
                   REWRITE WS-REG-IDX-CLI
                   IF WS-FS-TBCLI NOT = '00'
                       MOVE WS-CLI-ID TO IDX-ERR-CLI-ID
                       MOVE SPACES TO WS-MSG-ERRO
                       STRING "Erro UPDATE FS=" WS-FS-TBCLI
                           DELIMITED SIZE INTO WS-MSG-ERRO
                       PERFORM 8000-GRAVAR-ERRO
                       ADD 1 TO WS-CTR-CLIEN-ERRO
                       MOVE 'S' TO WS-ERRO-FLAG
                   ELSE
                       MOVE 'UPDATE OK' TO WS-RD-STATUS
                   END-IF

           END-READ

           IF WS-ERRO-FLAG = 'N'
               ADD 1 TO WS-CTR-CLIEN-OK
               MOVE WS-CLI-ID   TO WS-RD-CLI-ID
               MOVE WS-CLI-NOME TO WS-RD-NOME
               MOVE 'UPSERT'    TO WS-RD-OPER
               WRITE FS-LOG FROM WS-REL-DET
           END-IF.

       3000-PROC-TRANSACOES.
           MOVE SPACES TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA
           MOVE "--- PROCESSANDO TRANSACOES ---" TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA

           OPEN INPUT ARQTRXIN
           IF WS-FS-TRXIN NOT = '00'
               MOVE 0 TO IDX-ERR-CLI-ID
               MOVE 'Erro ao abrir TRANSACOES.TXT' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               GO TO 3000-FIM
           END-IF

           PERFORM UNTIL WS-EOF-TRXIN = 'S'
               READ ARQTRXIN INTO WS-REG-TRANSACAO
                   AT END
                       MOVE 'S' TO WS-EOF-TRXIN
                   NOT AT END
                       ADD 1 TO WS-CTR-TRX-LIDAS
                       PERFORM 3100-VALIDAR-TRANSACAO
               END-READ
           END-PERFORM

           CLOSE ARQTRXIN.
       3000-FIM.
           EXIT.

       3100-VALIDAR-TRANSACAO.
           MOVE 'N'           TO WS-ERRO-FLAG
           MOVE WS-TRX-CLI-ID TO IDX-ERR-CLI-ID

           IF WS-TRX-TIPO NOT = 'C' AND WS-TRX-TIPO NOT = 'D'
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Tipo invalido: " WS-TRX-TIPO
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-TRX-ERRO
               MOVE 'S' TO WS-ERRO-FLAG
           END-IF

           IF WS-ERRO-FLAG = 'N' AND WS-TRX-VALOR = 0
               MOVE 'Valor zerado' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-TRX-ERRO
               MOVE 'S' TO WS-ERRO-FLAG
           END-IF

           IF WS-ERRO-FLAG = 'N'
               MOVE WS-TRX-CLI-ID TO IDX-CLI-ID
               READ TBCLIEN KEY IS IDX-CLI-ID
                   INVALID KEY
                       MOVE 'Cliente inexistente' TO WS-MSG-ERRO
                       PERFORM 8000-GRAVAR-ERRO
                       ADD 1 TO WS-CTR-TRX-ERRO
                       MOVE 'S' TO WS-ERRO-FLAG
               END-READ
           END-IF

           IF WS-ERRO-FLAG = 'N' AND WS-TRX-TIPO = 'D'
               IF IDX-CLI-SALDO < WS-TRX-VALOR
                   MOVE SPACES TO WS-MSG-ERRO
                   STRING "Saldo insuficiente saldo="
                       IDX-CLI-SALDO " valor=" WS-TRX-VALOR
                       DELIMITED SIZE INTO WS-MSG-ERRO
                   PERFORM 8000-GRAVAR-ERRO
                   ADD 1 TO WS-CTR-TRX-ERRO
                   MOVE 'S' TO WS-ERRO-FLAG
               END-IF
           END-IF

           IF WS-ERRO-FLAG = 'N'
               PERFORM 3200-PROCESSAR-TRANSACAO
           END-IF.

       3200-PROCESSAR-TRANSACAO.
           MOVE WS-TRX-ID     TO IDX-TRX-ID
           MOVE WS-TRX-CLI-ID TO IDX-TRX-CLI-ID
           MOVE WS-TRX-TIPO   TO IDX-TRX-TIPO
           MOVE WS-TRX-VALOR  TO IDX-TRX-VALOR
           MOVE WS-DATA-HOJE  TO IDX-TRX-DTPROC
           WRITE WS-REG-IDX-TRX

           IF WS-FS-TBTRX NOT = '00'
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Erro INSERT TRX FS=" WS-FS-TBTRX
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-TRX-ERRO
               GO TO 3200-FIM
           END-IF

           MOVE WS-TRX-CLI-ID TO IDX-CLI-ID
           READ TBCLIEN KEY IS IDX-CLI-ID
               INVALID KEY
                   MOVE 'Erro releitura cliente' TO WS-MSG-ERRO
                   PERFORM 8000-GRAVAR-ERRO
                   ADD 1 TO WS-CTR-TRX-ERRO
                   GO TO 3200-FIM
           END-READ

           IF WS-TRX-TIPO = 'C'
               ADD WS-TRX-VALOR      TO IDX-CLI-SALDO
           ELSE
               SUBTRACT WS-TRX-VALOR FROM IDX-CLI-SALDO
           END-IF

           MOVE WS-DATA-HOJE TO IDX-CLI-DTATU
           REWRITE WS-REG-IDX-CLI

           IF WS-FS-TBCLI NOT = '00'
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Erro UPDATE SALDO FS=" WS-FS-TBCLI
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-TRX-ERRO
               GO TO 3200-FIM
           END-IF

           ADD 1 TO WS-CTR-COMMIT
           IF WS-CTR-COMMIT >= 100
               MOVE 0 TO WS-CTR-COMMIT
               MOVE "** COMMIT simulado (100 registros) **"
                   TO WS-LINHA
               WRITE FS-LOG FROM WS-LINHA
           END-IF

           ADD 1 TO WS-CTR-TRX-OK

           MOVE WS-TRX-CLI-ID TO WS-RD-CLI-ID
           MOVE SPACES        TO WS-RD-NOME
           IF WS-TRX-TIPO = 'C'
               MOVE 'CREDITO'  TO WS-RD-OPER
           ELSE
               MOVE 'DEBITO'   TO WS-RD-OPER
           END-IF
           MOVE 'OK' TO WS-RD-STATUS
           WRITE FS-LOG FROM WS-REL-DET.
       3200-FIM.
           EXIT.

       4000-GERAR-RELATORIO.
           MOVE SPACES TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "--- TOTAIS DE PROCESSAMENTO ---" TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           STRING "Clientes lidos:       " WS-CTR-CLIEN-LIDOS
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           STRING "Clientes processados: " WS-CTR-CLIEN-OK
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           STRING "Clientes com erro:    " WS-CTR-CLIEN-ERRO
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           STRING "Transacoes lidas:     " WS-CTR-TRX-LIDAS
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           STRING "Transacoes OK:        " WS-CTR-TRX-OK
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           STRING "Transacoes com erro:  " WS-CTR-TRX-ERRO
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE SPACES TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "--- SALDOS FINAIS ---"
               TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "ID     NOME                           SALDO"
               TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "--------------------------------------------------"
               TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           MOVE 0 TO IDX-CLI-ID
           START TBCLIEN KEY >= IDX-CLI-ID
               INVALID KEY
                   MOVE "Nenhum cliente na tabela" TO WS-LINHA
                   WRITE FS-RELAT FROM WS-LINHA
           END-START

           PERFORM UNTIL WS-EOF-REL = 'S'
               READ TBCLIEN NEXT
                   AT END
                       MOVE 'S' TO WS-EOF-REL
                   NOT AT END
                       MOVE IDX-CLI-ID    TO WS-RD-CLI-ID
                       MOVE IDX-CLI-NOME  TO WS-RD-NOME
                       MOVE IDX-CLI-SALDO TO WS-SALDO-FMT
                       MOVE SPACES        TO WS-RD-OPER
                       STRING "R$ " WS-SALDO-FMT
                           DELIMITED SIZE INTO WS-RD-STATUS
                       WRITE FS-RELAT FROM WS-REL-DET
               END-READ
           END-PERFORM

           MOVE "=================================================="
               TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA.

       8000-GRAVAR-ERRO.
           MOVE SPACES TO WS-LINHA
           STRING "ERRO CLI=" IDX-ERR-CLI-ID " | " WS-MSG-ERRO
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-ERROS FROM WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA

           ADD 1 TO WS-CTR-ERR-ID
           MOVE WS-CTR-ERR-ID  TO IDX-ERR-ID
           MOVE IDX-ERR-CLI-ID TO IDX-ERR-CLI-ID
           MOVE WS-MSG-ERRO    TO IDX-ERR-DESC
           MOVE WS-DATA-HOJE   TO IDX-ERR-DTOC
           WRITE WS-REG-IDX-ERR.

       9000-FINALIZAR.
           MOVE SPACES TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA
           MOVE "PROCESSAMENTO CONCLUIDO" TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA
           MOVE "=================================================="
               TO WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           CLOSE ARQERROS
           CLOSE ARQRELAT
           CLOSE ARQLOG
           CLOSE TBCLIEN
           CLOSE TBTRXIN
           CLOSE TBERROS. 
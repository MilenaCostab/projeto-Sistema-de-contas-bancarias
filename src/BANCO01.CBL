       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO01.
       AUTHOR. PROJETO6.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQCLIEN ASSIGN TO "CLIENTES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-CLIEN.

           SELECT ARQTRXIN ASSIGN TO "TRANSACOES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-TRXIN.

           SELECT ARQERROS ASSIGN TO "ERROS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-ERROS.

           SELECT ARQRELAT ASSIGN TO "RELATORIO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-RELAT.

           SELECT ARQLOG   ASSIGN TO "LOG.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-LOG.

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

       WORKING-STORAGE SECTION.

           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.

       01 HV-CLI-ID            PIC S9(09) COMP.
       01 HV-CLI-NOME          PIC X(30).
       01 HV-CLI-SALDO         PIC S9(09) COMP.
       01 HV-CLI-DTATU         PIC X(10).
       01 HV-TRX-ID            PIC S9(09) COMP.
       01 HV-TRX-TIPO          PIC X(01).
       01 HV-TRX-VALOR         PIC S9(09) COMP.
       01 HV-ERR-CLI-ID        PIC S9(09) COMP.
       01 HV-ERR-DESC          PIC X(100).

           EXEC SQL
               DECLARE CUR-CLIENTES CURSOR FOR
                   SELECT CLI_ID, CLI_NOME, CLI_SALDO,
                          DT_ATUALIZACAO
                   FROM CLIENTES
                   ORDER BY CLI_ID
           END-EXEC.

           EXEC SQL
               DECLARE CUR-TRANSACOES CURSOR FOR
                   SELECT T.TRX_ID, T.CLI_ID, C.CLI_NOME,
                          T.TRX_TIPO, T.TRX_VALOR
                   FROM TRANSACOES T
                   JOIN CLIENTES C ON T.CLI_ID = C.CLI_ID
                   ORDER BY T.TRX_ID
           END-EXEC.

           COPY "CPYCLIEN.CPY".
           COPY "CPYTRXIN.CPY".

       01 WS-FS-CLIEN          PIC XX VALUE SPACES.
       01 WS-FS-TRXIN          PIC XX VALUE SPACES.
       01 WS-FS-ERROS          PIC XX VALUE SPACES.
       01 WS-FS-RELAT          PIC XX VALUE SPACES.
       01 WS-FS-LOG            PIC XX VALUE SPACES.

       01 WS-CTR-CLIEN-LIDOS   PIC 9(06) VALUE 0.
       01 WS-CTR-CLIEN-OK      PIC 9(06) VALUE 0.
       01 WS-CTR-CLIEN-ERRO    PIC 9(06) VALUE 0.
       01 WS-CTR-TRX-LIDAS     PIC 9(06) VALUE 0.
       01 WS-CTR-TRX-OK        PIC 9(06) VALUE 0.
       01 WS-CTR-TRX-ERRO      PIC 9(06) VALUE 0.
       01 WS-CTR-COMMIT        PIC 9(06) VALUE 0.

       01 WS-EOF-CLIEN         PIC X VALUE 'N'.
       01 WS-EOF-TRXIN         PIC X VALUE 'N'.
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

           MOVE "=================================================="
               TO WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "  SISTEMA DE CONTAS BANCARIAS "
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
           MOVE "--- CLIENTES ---" TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA

           OPEN INPUT ARQCLIEN
           IF WS-FS-CLIEN NOT = '00'
               MOVE 0 TO HV-ERR-CLI-ID
               MOVE 'Erro ao abrir CLIENTES.TXT' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               GO TO 2000-FIM
           END-IF

           PERFORM UNTIL WS-EOF-CLIEN = 'S'
               READ ARQCLIEN INTO WS-REG-CLIENTE
                   AT END     MOVE 'S' TO WS-EOF-CLIEN
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
               MOVE WS-CLI-ID TO HV-ERR-CLI-ID
               MOVE 'Nome obrigatorio' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-CLIEN-ERRO
               MOVE 'S' TO WS-ERRO-FLAG
           END-IF

           IF WS-ERRO-FLAG = 'N'
               PERFORM 2200-UPSERT-DB2
           END-IF.

       2200-UPSERT-DB2.
           MOVE WS-CLI-ID TO HV-CLI-ID

           EXEC SQL
               SELECT CLI_ID INTO :HV-CLI-ID
               FROM CLIENTES
               WHERE CLI_ID = :HV-CLI-ID
           END-EXEC

           IF SQLCODE = 0
               MOVE WS-CLI-NOME   TO HV-CLI-NOME
               MOVE WS-CLI-SALDO  TO HV-CLI-SALDO
               MOVE WS-DATA-HOJE  TO HV-CLI-DTATU

               EXEC SQL
                   UPDATE CLIENTES
                   SET CLI_NOME        = :HV-CLI-NOME,
                       CLI_SALDO       = :HV-CLI-SALDO,
                       DT_ATUALIZACAO  = :HV-CLI-DTATU
                   WHERE CLI_ID = :HV-CLI-ID
               END-EXEC

               PERFORM 2300-CHECAR-SQLCODE
               IF WS-ERRO-FLAG = 'N'
                   ADD 1 TO WS-CTR-CLIEN-OK
                   MOVE 'UPDATE OK' TO WS-RD-STATUS
               END-IF

           ELSE IF SQLCODE = 100
               MOVE WS-CLI-ID     TO HV-CLI-ID
               MOVE WS-CLI-NOME   TO HV-CLI-NOME
               MOVE WS-CLI-SALDO  TO HV-CLI-SALDO
               MOVE WS-DATA-HOJE  TO HV-CLI-DTATU

               EXEC SQL
                   INSERT INTO CLIENTES
                       (CLI_ID, CLI_NOME, CLI_SALDO, DT_ATUALIZACAO)
                   VALUES
                       (:HV-CLI-ID, :HV-CLI-NOME,
                        :HV-CLI-SALDO, :HV-CLI-DTATU)
               END-EXEC

               PERFORM 2300-CHECAR-SQLCODE
               IF WS-ERRO-FLAG = 'N'
                   ADD 1 TO WS-CTR-CLIEN-OK
                   MOVE 'INSERT OK' TO WS-RD-STATUS
               END-IF

           ELSE
               MOVE WS-CLI-ID TO HV-ERR-CLI-ID
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Erro DB2 SQLCODE=" SQLCODE
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-CLIEN-ERRO
           END-IF

           MOVE WS-CLI-ID   TO WS-RD-CLI-ID
           MOVE WS-CLI-NOME TO WS-RD-NOME
           MOVE 'UPSERT'    TO WS-RD-OPER
           WRITE FS-LOG FROM WS-REL-DET.

       2300-CHECAR-SQLCODE.
           MOVE 'N' TO WS-ERRO-FLAG
           IF SQLCODE NOT = 0
               MOVE WS-CLI-ID TO HV-ERR-CLI-ID
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Erro SQL SQLCODE=" SQLCODE
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-CLIEN-ERRO
               MOVE 'S' TO WS-ERRO-FLAG

               EXEC SQL ROLLBACK END-EXEC
           END-IF.

       3000-PROC-TRANSACOES.
           MOVE SPACES TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA
           MOVE "--- TRANSACOES ---" TO WS-LINHA
           WRITE FS-LOG FROM WS-LINHA

           OPEN INPUT ARQTRXIN
           IF WS-FS-TRXIN NOT = '00'
               MOVE 0 TO HV-ERR-CLI-ID
               MOVE 'Erro ao abrir TRANSACOES.TXT' TO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               GO TO 3000-FIM
           END-IF

           PERFORM UNTIL WS-EOF-TRXIN = 'S'
               READ ARQTRXIN INTO WS-REG-TRANSACAO
                   AT END     MOVE 'S' TO WS-EOF-TRXIN
                   NOT AT END
                       ADD 1 TO WS-CTR-TRX-LIDAS
                       PERFORM 3100-VALIDAR-TRANSACAO
               END-READ
           END-PERFORM
           CLOSE ARQTRXIN.
       3000-FIM.
           EXIT.

       3100-VALIDAR-TRANSACAO.
           MOVE 'N' TO WS-ERRO-FLAG
           MOVE WS-TRX-CLI-ID TO HV-ERR-CLI-ID

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
               MOVE WS-TRX-CLI-ID TO HV-CLI-ID
               EXEC SQL
                   SELECT CLI_ID, CLI_SALDO
                   INTO :HV-CLI-ID, :HV-CLI-SALDO
                   FROM CLIENTES
                   WHERE CLI_ID = :HV-CLI-ID
               END-EXEC

               IF SQLCODE = 100
                   MOVE 'Cliente inexistente' TO WS-MSG-ERRO
                   PERFORM 8000-GRAVAR-ERRO
                   ADD 1 TO WS-CTR-TRX-ERRO
                   MOVE 'S' TO WS-ERRO-FLAG
               ELSE IF SQLCODE NOT = 0
                   MOVE SPACES TO WS-MSG-ERRO
                   STRING "Erro DB2 SELECT SQLCODE=" SQLCODE
                       DELIMITED SIZE INTO WS-MSG-ERRO
                   PERFORM 8000-GRAVAR-ERRO
                   ADD 1 TO WS-CTR-TRX-ERRO
                   MOVE 'S' TO WS-ERRO-FLAG
               END-IF
           END-IF

           IF WS-ERRO-FLAG = 'N' AND WS-TRX-TIPO = 'D'
               IF HV-CLI-SALDO < WS-TRX-VALOR
                   MOVE SPACES TO WS-MSG-ERRO
                   STRING "Saldo insuficiente ="
                       HV-CLI-SALDO " valor=" WS-TRX-VALOR
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
           MOVE WS-TRX-ID     TO HV-TRX-ID
           MOVE WS-TRX-CLI-ID TO HV-CLI-ID
           MOVE WS-TRX-TIPO   TO HV-TRX-TIPO
           MOVE WS-TRX-VALOR  TO HV-TRX-VALOR

           EXEC SQL
               INSERT INTO TRANSACOES
                   (TRX_ID, CLI_ID, TRX_TIPO,
                    TRX_VALOR, DT_PROCESSAMENTO)
               VALUES
                   (:HV-TRX-ID, :HV-CLI-ID, :HV-TRX-TIPO,
                    :HV-TRX-VALOR, :WS-DATA-HOJE)
           END-EXEC

           IF SQLCODE NOT = 0
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Erro INSERT TRX SQLCODE=" SQLCODE
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-TRX-ERRO
               EXEC SQL ROLLBACK END-EXEC
               GO TO 3200-FIM
           END-IF

           IF WS-TRX-TIPO = 'C'
               EXEC SQL
                   UPDATE CLIENTES
                   SET CLI_SALDO = CLI_SALDO + :HV-TRX-VALOR,
                       DT_ATUALIZACAO = :WS-DATA-HOJE
                   WHERE CLI_ID = :HV-CLI-ID
               END-EXEC
           ELSE
               EXEC SQL
                   UPDATE CLIENTES
                   SET CLI_SALDO = CLI_SALDO - :HV-TRX-VALOR,
                       DT_ATUALIZACAO = :WS-DATA-HOJE
                   WHERE CLI_ID = :HV-CLI-ID
               END-EXEC
           END-IF

           IF SQLCODE NOT = 0
               MOVE SPACES TO WS-MSG-ERRO
               STRING "Erro UPDATE SALDO SQLCODE=" SQLCODE
                   DELIMITED SIZE INTO WS-MSG-ERRO
               PERFORM 8000-GRAVAR-ERRO
               ADD 1 TO WS-CTR-TRX-ERRO
               EXEC SQL ROLLBACK END-EXEC
               GO TO 3200-FIM
           END-IF

           ADD 1 TO WS-CTR-COMMIT
           IF WS-CTR-COMMIT >= 100
               EXEC SQL COMMIT END-EXEC
               MOVE 0 TO WS-CTR-COMMIT
               MOVE "** COMMIT a cada 100 registros **" TO WS-LINHA
               WRITE FS-LOG FROM WS-LINHA
           END-IF

           ADD 1 TO WS-CTR-TRX-OK

           MOVE WS-TRX-CLI-ID TO WS-RD-CLI-ID
           MOVE SPACES        TO WS-RD-NOME
           IF WS-TRX-TIPO = 'C'
               MOVE 'CREDITO' TO WS-RD-OPER
           ELSE
               MOVE 'DEBITO'  TO WS-RD-OPER
           END-IF
           MOVE 'OK' TO WS-RD-STATUS
           WRITE FS-LOG FROM WS-REL-DET.
       3200-FIM.
           EXIT.

       4000-GERAR-RELATORIO.
           MOVE SPACES TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA
           MOVE "--- TOTAIS ---" TO WS-LINHA
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
           MOVE "--- SALDOS FINAIS ---" TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA

           EXEC SQL OPEN CUR-CLIENTES END-EXEC

           PERFORM UNTIL SQLCODE = 100
               EXEC SQL
                   FETCH CUR-CLIENTES
                   INTO :HV-CLI-ID, :HV-CLI-NOME,
                        :HV-CLI-SALDO, :HV-CLI-DTATU
               END-EXEC
               IF SQLCODE = 0
                   MOVE HV-CLI-ID    TO WS-RD-CLI-ID
                   MOVE HV-CLI-NOME  TO WS-RD-NOME
                   MOVE HV-CLI-SALDO TO WS-SALDO-FMT
                   MOVE SPACES TO WS-RD-OPER
                   STRING "R$ " WS-SALDO-FMT
                       DELIMITED SIZE INTO WS-RD-STATUS
                   WRITE FS-RELAT FROM WS-REL-DET
               END-IF
           END-PERFORM

           EXEC SQL CLOSE CUR-CLIENTES END-EXEC

           MOVE "=================================================="
               TO WS-LINHA
           WRITE FS-RELAT FROM WS-LINHA.

       8000-GRAVAR-ERRO.
           MOVE SPACES TO WS-LINHA
           STRING "ERRO CLI=" HV-ERR-CLI-ID " | " WS-MSG-ERRO
               DELIMITED SIZE INTO WS-LINHA
           WRITE FS-ERROS FROM WS-LINHA
           WRITE FS-LOG   FROM WS-LINHA

           MOVE WS-MSG-ERRO TO HV-ERR-DESC
           EXEC SQL
               INSERT INTO ERROS_PROCESSAMENTO
                   (CLI_ID, DESCRICAO_ERRO)
               VALUES
                   (:HV-ERR-CLI-ID, :HV-ERR-DESC)
           END-EXEC.

       9000-FINALIZAR.
           EXEC SQL COMMIT END-EXEC

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
           CLOSE ARQLOG.
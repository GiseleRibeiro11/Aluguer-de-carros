      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 0807_T03.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 TIPO-CARRO PIC 9.
           88 VALIDAR-TIPO-CARRO VALUES 1 THRU 3.
       77 KMS-EFETUADOS PIC 9(05).
           88 VALIDAR-KMS-EFETUADOS VALUES 1 THRU 99999.
       77 DIAS-ALUGUER PIC 9(03).
           88 VALIDAR-DIAS-ALUGUER VALUES 1 THRU 365.
       77 TOTAL-ILIQUIDO PIC 9(05)V99.
       77 IVA      PIC 9(05)V99.
       77 TOTAL-FINAL PIC 9(06)V99.
       77 KMS-GRATUITOS PIC 9(05) VALUE 75.
       77 KMS-PAGOS PIC 9(05).
       77 CUSTO-DIA PIC 9(03)V99.
       77 REPETIR-PROGRAMA PIC X VALUE 'S'.
       77 TEMP PIC 9(05)V99.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY 'Bem-vindo ao sistema de aluguer de carros!'
           AT 0103.
           DISPLAY 'Introduza os dados do cliente:'           AT 0303.
           DISPLAY 'TIPO DE CARRO (1=Volkswagen, 2=Toyota, 3=Mercedes):'
           AT 0403.
           ACCEPT TIPO-CARRO                                 AT 0455.
           DISPLAY " " ERASE EOL.
           DISPLAY 'QUILOMETROS EFETUADOS:'                  AT 0603.
           ACCEPT KMS-EFETUADOS                              AT 0625.
           DISPLAY " " ERASE EOL.
           DISPLAY 'NUMERO DE DIAS DE ALUGUER:'              AT 0703.
           ACCEPT DIAS-ALUGUER                               AT 0728.
           DISPLAY " " ERASE EOL.
       CALCULO.
            IF VALIDAR-TIPO-CARRO AND VALIDAR-DIAS-ALUGUER THEN.
            IF KMS-EFETUADOS > KMS-GRATUITOS THEN.
            SUBTRACT KMS-GRATUITOS FROM KMS-EFETUADOS GIVING KMS-PAGOS.
           ELSE
            MOVE 0 TO KMS-PAGOS
           END-IF

           ELSE
            IF TIPO-CARRO = 1 VOLKSWAGEM THEN
            MOVE 30,00 TO CUSTO-DIA
            MOVE 1,20 TO CUSTO-KM
           END IF.

           ELSE
            IF TIPO-CARRO = 2 TOYOTA THEN
            MOVE 35,00 TO CUSTO-DIA
            MOVE 1,50 TO CUSTO-KM
           END-IF.

           ELSE
            IF TIPO-CARRO = 3 MERCEDES THEN
            MOVE 60,00 TO CUSTO-DIA
            MOVE 2,50 TO CUSTO-KM
            END-IF.

       RESULTADO.
           COMPUTE TEMP = (CUSTO-DIA * DIAS-ALUGUER) + (CUSTO-KM
           * KMS-PAGOS)
           MOVE TEMP TO TOTAL-ILIQUIDO.
           COMPUTE IVA = TOTAL-ILIQUIDO * 0.23
           COMPUTE TOTAL-FINAL = TOTAL-ILIQUIDO + IVA

           MOVE TOTAL-ILIQUIDO TO MASCARA-TOTAL-ILIQUIDO.
           MOVE IVA TO MASCARA-IVA.
           MOVE TOTAL-FINAL TO MASCARA-TOTAL-FINAL.

           DISPLAY 'Total ilíquido: ' MASCARA-TOTAL-ILIQUIDO
           DISPLAY 'IVA (23%): ' MASCARA-IVA
           DISPLAY 'Total final: ' MASCARA-TOTAL-FINAL
            ELSE
           DISPLAY 'Dados inválidos. Verifique o tipo de carro e os
           dias de aluguer
           END-IF.

           DISPLAY 'Deseja repetir o programa? (S/N)'.
           ACCEPT REPETIR-PROGRAMA.

           IF REPETIR-PROGRAMA = 'S' OR REPETIR-PROGRAMA = 's' THEN
           GO TO MAIN-PROCEDURE
           ELSE
           CLOSE INPUT-FILE.

           STOP RUN.
       END PROGRAM 0807_T03.

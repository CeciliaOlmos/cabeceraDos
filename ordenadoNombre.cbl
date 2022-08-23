      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT filial1 ASSIGN TO nom-arch1
               ORGANIZATION is line sequential.
           SELECT filial2 ASSIGN TO nom-arch2
               ORGANIZATION is line sequential.
           SELECT filial3 ASSIGN TO nom-arch3
               ORGANIZATION is line sequential.
           SELECT arch-sort ASSIGN to "sortwork".
           SELECT Trans-Act ASSIGN TO "..\TRANS-ACT.txt".
           SELECT LISTADO
           ASSIGN TO PRINTER,
           "D:\linux cecilia\COBOL\archivo\impTRANSACC.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  filial1.
       01  tr-cab1-reg.
           03 tr1-cab1-tipo1 pic x.
           03 tr1-cab1-filial1 pic 9.
       01  tr-cab2-reg1.
           03 tr-cab2-tipo1 pic x.
           03 tr-cab2-fecha1 pic 9(8).
       01  tr-det-reg1.
           03 tr-det-tipo1 pic x.
           03 tr-det-socio1 pic 9(4).
           03 tr-det-importe1 pic s9(7)v99.


       SD  arch-sort.
       01  srt-reg.
           03 srt-fecha pic 9(8).
           03 srt-cod-soc pic 9(4).
           03 srt-importe pic S9(8)V99.
       FD  LISTADO
           LINAGE IS 60 LINES
           with FOOTING AT 50
           lines at top 1
           lines at BOTTOM 1.
       01  lis-reg pic x(80).
       FD  Trans-Act.
       01  tra-reg.
           03 tra-fecha pic 9(8).
           03 tra-socio pic 9(4).
           03 tra-importe pic S9(8)V99.

       WORKING-STORAGE SECTION.
       01  tabla-archivos.
           03 archivos.
               05  nom-arch1 pic x(34) value "..\transacciones.txt".
               05  nom-arch2 pic x(34) value "..\transacciones2.txt".
               05  nom-arch3 pic x(34) value "..\transacciones3.txt".
           03 vecArchivos redefines archivos occurs 3 times.
               05 archTrans pic x(34).
      * 01  nom-arch1 pic x(34) value "..\transacciones.txt".
      *     MOVE archTrans(w-i) to nom-arch1
      * 01  nom-arch2 pic x(34) value "..\transacciones2.txt".
      * 01  nom-arch3 pic x(34) value "..\transacciones3.txt".
       01  w-socio-ant pic 9(4).
       01  w-imp-procesado pic s9(8)v99.
       01  w-i pic 9.
       01  w-flag1 PIC 9 value ZERO.
       01  w-flag2 PIC 9 VALUE ZERO.
       01  w-flag3 PIC 9 VALUE ZERO.
       01  w-fecha-ing pic 9(8).
       01  tab-filiales.
           03 largo pic 9.
           03 vec-filial OCCURS 1 to 3 DEPENDING on largo value zeros.
               05 vec-tr1-cab1-tipo pic x.
               05 vec-tr1-cab1-filial pic 9.
               05 vec-tr-cab2-tipo pic x.
               05 vec-tr-cab2-fecha pic 9(8).
               05 vec-tr-det-tipo pic x.
               05 vec-tr-det-socio pic 9(4).
               05 vec-tr-det-importe pic s9(7)v99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *     SORT arch-sort ASCENDING srt-fecha
      *     INPUT PROCEDURE IS DATOS-ENTRADA
      *     OUTPUT PROCEDURE IS DATOS-SALIDA.

           PERFORM 140-INGRESAR-FECHA.
           PERFORM VARYING w-i from 1 by 1 until w-i > 3

               PERFORM 100-INICIO-GENERAL
               PERFORM 150-MEZCLAR-ARCHIVOS
               PERFORM 600-FIN-GENERAL
           END-PERFORM.

       STOP RUN.
      *       DATOS-ENTRADA.

      *      DATOS-SALIDA.
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.


       120-ABRIR-ARCHIVOS.
           open input filial1.
           open input filial2.
           open input filial3.
           open EXTEND Trans-Act.

       140-INGRESAR-FECHA.
           DISPLAY "Ingrese fecha de transaccion (AAAAMMDD)".
           ACCEPT w-fecha-ing.
           PERFORM until w-fecha-ing is > 0
           DISPLAY "Error, Ingrese fecha de transaccion (AAAAMMDD)"
           ACCEPT w-fecha-ing
           END-PERFORM.

       150-MEZCLAR-ARCHIVOS.
            PERFORM 200-LEER-TRANSAC.
           PERFORM until  w-flag1 is equal 1
               PERFORM 300-INICIO-FECHA
               PERFORM 340-DESAGOTAR-DETALLE
              PERFORM UNTIL w-flag1 is equal 1 or tr-cab2-tipo1
              is equal "F"
                PERFORM 350-INICIO-SOCIO
                   PERFORM UNTIL w-flag1 is equal 1 or tr-det-socio1
                   is not equal w-socio-ant
                        PERFORM 400-PROCESO
                       PERFORM 200-LEER-TRANSAC
                   END-PERFORM
                   PERFORM 450-FIN-SOCIO
              END-PERFORM
           END-PERFORM.

       200-LEER-TRANSAC.
           read filial1 at end move 1 to w-flag1.


       300-INICIO-FECHA.
           PERFORM 330-BUSCAR-FECHA.

       330-BUSCAR-FECHA.
           PERFORM 200-LEER-TRANSAC UNTIL w-flag1 IS EQUAL 1
               OR (tr-cab2-fecha1 IS equal w-fecha-ing
               AND tr-cab2-tipo1 IS EQUAL "F").

       340-DESAGOTAR-DETALLE.
           IF tr-cab2-tipo1 is EQUAL to "F" and
              tr-cab2-fecha1 IS  EQUAL w-fecha-ing
                 PERFORM 200-LEER-TRANSAC.

       350-INICIO-SOCIO.
           MOVE tr-det-socio1 to w-socio-ant.
           MOVE ZERO to w-imp-procesado.

       400-PROCESO.
           ADD tr-det-importe1 to w-imp-procesado.

       450-FIN-SOCIO.
           PERFORM 470-ARMO-ARCHIVO.

       470-ARMO-ARCHIVO.
           MOVE w-socio-ant TO tra-socio.
           MOVE w-imp-procesado to tra-importe.
           MOVE w-fecha-ing to tra-fecha.
           write tra-reg.

       600-FIN-GENERAL.
           close filial1.
           close filial2.
           close filial3.
           close Trans-Act.

       END PROGRAM YOUR-PROGRAM-NAME.

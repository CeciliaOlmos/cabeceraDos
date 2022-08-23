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
           "..\impTRANSACC.dat".
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

       FD  filial2.
       01  tr-cab1-reg2.
           03 tr1-cab1-tipo2 pic x.
           03 tr1-cab1-filial2 pic 9.
       01  tr-cab2-reg2.
           03 tr-cab2-tipo2 pic x.
           03 tr-cab2-fecha2 pic 9(8).
       01  tr-det-reg2.
           03 tr-det-tipo2 pic x.
           03 tr-det-socio2 pic 9(4).
           03 tr-det-importe2 pic s9(7)v99.

       FD  filial3.
       01  tr-cab1-reg3.
           03 tr1-cab1-tipo3 pic x.
           03 tr1-cab1-filial3 pic 9.
       01  tr-cab2-reg3.
           03 tr-cab2-tipo3 pic x.
           03 tr-cab2-fecha3 pic 9(8).
       01  tr-det-reg3.
           03 tr-det-tipo3 pic x.
           03 tr-det-socio3 pic 9(4).
           03 tr-det-importe3 pic s9(7)v99.

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
      * 01  tabla-archivos.
      *     03 archivos.
      *         05  nom-arch1 pic x(34) value "..\transacciones.txt".
      *         05  nom-arch2 pic x(34) value "..\transacciones2.txt".
      *         05  nom-arch3 pic x(34) value "..\transacciones3.txt".
      *     03 vecArchivos redefines archivos occurs 3 times.
      *         05 archTrans pic x(34).
       01  nom-arch1 pic x(34) value "..\transacciones.txt".
       01  nom-arch2 pic x(34) value "..\transacciones2.txt".
       01  nom-arch3 pic x(34) value "..\transacciones3.txt".
       01  w-socio-ant pic 9(4).
       01  w-imp-procesado pic s9(8)v99.
       01  w-imp-procesado2 pic s9(8)v99.
       01  w-imp-procesado3 pic s9(8)v99.
       01  w-i pic 9.
       01  w-flag1 PIC 9 value ZERO.
       01  w-flag2 PIC 9 VALUE ZERO.
       01  w-flag3 PIC 9 VALUE ZERO.
       01  w-fecha-ing pic 9(8).
       01  w-socio-ant2 pic 9(4).
       01  w-socio-ant3 pic 9(4).
       01  w-flagSocSort PIC 9 VALUE ZERO.
       01  w-srt-socio-anterior pic 9(4).
       01  w-salida-acum-imp pic s9(8)v99.
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
       01  cabecera1.
           03  lin-titulo.
               05 filler pic x(30) value spaces.
               05 filler pic x(19) value "BANCO: EL CORRALITO".
               05 filler pic x(5) value spaces.
       01  cabecera2.
           03  lin-sub-titulo.
               05 filler pic x(17) value spaces.
               05 filler pic x(46) value "LISTADO DE TRANSFERENCIAS "-
           "BANCARIAS DE SOCIOS".
               05 filler pic x(17) value spaces.
       01  cabecera3.
           03  lin-guarda.
               05 filler pic x(80) value all "*".
       01  cabecera4.
           03  lin-titulo-soc.
               05 filler pic x(30) value spaces.
               05 soc-dat-cod pic x(5) value "SOCIO".
               05 filler  pic x(8) value space.
               05 soc-dat-imp pic x(7) value "IMPORTE".
       01  detalle1.
           03  lin-dat-soc.
               05 filler pic x(31) value spaces.
               05 l-soc-cod pic 9(4).
               05 filler pic x(3) value spaces.
               05 l-soc-imp pic z.zzz.zzz.zz9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT arch-sort ASCENDING srt-cod-soc
           INPUT PROCEDURE IS DATOS-ENTRADA
           OUTPUT PROCEDURE IS DATOS-SALIDA.
           STOP RUN.

           DATOS-ENTRADA.
           PERFORM 140-INGRESAR-FECHA.
           PERFORM 100-INICIO-GENERAL.
           PERFORM 150-MEZCLAR-ARCHIVOS.
           PERFORM 600-FIN-GENERAL.

           DATOS-SALIDA.
           PERFORM 700-INICIO-SALIDA.
           PERFORM 750-LEER-SOC-SORT.
           PERFORM UNTIL w-flagSocSort IS EQUALS 1
               PERFORM 800-INICIO-SOCIO
               PERFORM UNTIL w-flagSocSort is equal 1 or
               srt-cod-soc is not equal w-srt-socio-anterior
                   PERFORM 850-PROCESO-SALIDA
                   PERFORM 750-LEER-SOC-SORT
               END-PERFORM
               PERFORM 900-FIN-SOCIO-SALIDA
           END-PERFORM.
           PERFORM 1000-FIN-SALIDA.


      ******* RUTINAS INPUT-PROCEDURE *****************************************
       100-INICIO-GENERAL.
           PERFORM 120-ABRIR-ARCHIVOS.


       120-ABRIR-ARCHIVOS.
           open input filial1.
      *     open input filial2.
      *     open input filial3.
           open OUTPUT Trans-Act.

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

      *        OR  w-flag2 is equal 1
      *         or w-flag3 is equal 1
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
      *     read filial2 at end move 1 to w-flag2.
      *     read filial3 at end move 1 to w-flag3.

       300-INICIO-FECHA.
           PERFORM 330-BUSCAR-FECHA.

       330-BUSCAR-FECHA.
           PERFORM 200-LEER-TRANSAC UNTIL w-flag1 IS EQUAL 1
               OR (tr-cab2-fecha1 IS equal w-fecha-ing
               AND tr-cab2-tipo1 IS EQUAL "F").
      *     PERFORM 200-LEER-TRANSAC UNTIL w-flag2 IS EQUAL 1
      *         OR (tr-cab2-fecha2 IS equal w-fecha-ing
      *         AND tr-cab2-tipo2 IS EQUAL "F").
      *     PERFORM 200-LEER-TRANSAC UNTIL w-flag3 IS EQUAL 1
      *         OR (tr-cab2-fecha3 IS equal w-fecha-ing
      *         AND tr-cab2-tipo3 IS EQUAL "F").

       340-DESAGOTAR-DETALLE.
           IF tr-cab2-tipo1 is EQUAL to "F" and
              tr-cab2-fecha1 IS  EQUAL w-fecha-ing
                 PERFORM 200-LEER-TRANSAC.
      *     IF tr-cab2-tipo2 is EQUAL to "F" and
      *        tr-cab2-fecha2 IS  EQUAL w-fecha-ing
      *           PERFORM 200-LEER-TRANSAC.
      *     IF tr-cab2-tipo3 is EQUAL to "F" and
      *        tr-cab2-fecha3 IS  EQUAL w-fecha-ing
      *           PERFORM 200-LEER-TRANSAC.

       350-INICIO-SOCIO.
           MOVE tr-det-socio1 to w-socio-ant.
           MOVE ZERO to w-imp-procesado.
      *     MOVE tr-det-socio2 to w-socio-ant2.
      *     MOVE ZERO to w-imp-procesado2.
      *     MOVE tr-det-socio3 to w-socio-ant3.
      *     MOVE ZERO to w-imp-procesado3.
       400-PROCESO.
           ADD tr-det-importe1 to w-imp-procesado.
      *     ADD tr-det-importe2 to w-imp-procesado2.
      *     ADD tr-det-importe3 to w-imp-procesado3.

       450-FIN-SOCIO.
           PERFORM 470-ARMO-ARCHIVO.

       470-ARMO-ARCHIVO.
           move w-socio-ant TO tra-socio.
           move w-imp-procesado to tra-importe.
           move w-fecha-ing to tra-fecha.
           write tra-reg.
           move w-socio-ant TO srt-cod-soc.
           move w-imp-procesado to srt-importe.
           move w-fecha-ing to srt-fecha.
           RELEASE srt-reg.
      *     move w-socio-ant2 TO tra-socio.
      *     move w-imp-procesado2 to tra-importe.
      *     move w-fecha-ing to tra-fecha.
      *     write tra-reg.
      *     move w-socio-ant2 TO srt-cod-soc.
      *     move w-imp-procesado2 to srt-importe.
      *     move w-fecha-ing to srt-fecha.
      *     RELEASE srt-reg.
      *     move w-socio-ant3 TO tra-socio.
      *     move w-imp-procesado3 to tra-importe.
      *     move w-fecha-ing to tra-fecha.
      *     write tra-reg.
      *     move w-socio-ant3 TO srt-cod-soc.
      *     move w-imp-procesado3 to srt-importe.
      *     move w-fecha-ing to srt-importe.
      *     RELEASE srt-reg.

       600-FIN-GENERAL.
           close filial1.
      *     close filial2.
      *     close filial3.
           close Trans-Act.

      ******* RUTINAS OUTPUT-PROCEDURE ****************************************

       700-INICIO-SALIDA.
           PERFORM 710-INICIO-VARIABLES-SALIDA.
           PERFORM 720-ABRO-ARCHIVO-IMP.
           PERFORM 730-IMPRIMO-ENCABEZADO.

       710-INICIO-VARIABLES-SALIDA.

       720-ABRO-ARCHIVO-IMP.
           OPEN OUTPUT LISTADO.
       730-IMPRIMO-ENCABEZADO.
           WRITE lis-reg FROM cabecera3 AFTER 1.
           WRITE lis-reg FROM cabecera1 AFTER 1.
           WRITE lis-reg FROM cabecera2 AFTER 1.
           WRITE lis-reg FROM cabecera3 AFTER 1.
           WRITE lis-reg FROM cabecera4 AFTER 1.
       750-LEER-SOC-SORT.
           RETURN arch-sort AT END MOVE 1 TO w-flagSocSort.

       800-INICIO-SOCIO.
           move srt-cod-soc to w-srt-socio-anterior.
           MOVE ZERO TO w-salida-acum-imp.

       850-PROCESO-SALIDA.
           PERFORM 910-ARMO-LIN-SOC.
           ADD srt-importe TO w-salida-acum-imp.

       900-FIN-SOCIO-SALIDA.
      *     PERFORM 910-ARMO-LIN-SOC.
       910-ARMO-LIN-SOC.
           MOVE srt-cod-soc TO l-soc-cod.
           MOVE srt-importe TO l-soc-imp.
           WRITE lis-reg FROM detalle1 AFTER 1.
       1000-FIN-SALIDA.
           CLOSE LISTADO.
       END PROGRAM YOUR-PROGRAM-NAME.

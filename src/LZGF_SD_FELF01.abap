*----------------------------------------------------------------------*
***INCLUDE LZGF_SD_FELF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CABECERA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&      <-- GV_BOLETA
*&---------------------------------------------------------------------*
FORM cabecera  USING    data_ls TYPE  edoc_src_data_sd_invoice
CHANGING dte_ev  TYPE  zclfel_et_documento
  gv_boleta.

  DATA: cab_ls     TYPE zclfel_encabezado,
        header_ls  TYPE edoc_vbrkvb,

        cond_lt    TYPE edoc_komv_tab,
        cond_ls    LIKE komv,

        item_lt    TYPE edoc_vbrpvb_tab,
        item_lt2   TYPE edoc_vbrpvb_tab,
        item_ls    LIKE vbrpvb,
        item_ls2   LIKE vbrpvb,
        item_ls_b  LIKE vbrpvb,

        partner_lt TYPE edoc_vbpa_tab,
        partner_ls TYPE vbpavb,

        bset_lt    TYPE edoc_bset_tab,
        bset_ls    TYPE bset,
        bkpf_ls    TYPE edoc_bkpf,

        vbpa_lt    TYPE edoc_vbpa_tab,
        vbpa_ls    TYPE vbpavb.

  DATA: l_ztag1     LIKE t052-ztag1,
        l_fchven    LIKE sy-datum,
        l_netwr     LIKE vbrk-netwr,
        l_seguro    LIKE vbrk-netwr,
        l_flete     LIKE vbrk-netwr,
        l_kurs2     LIKE bkpf-kurs2,
        l_kbetr     LIKE konv-kbetr,
        l_desc      LIKE konv-kbetr,
        l_condiva   LIKE konv-kschl,
        l_kwert     LIKE konv-kwert,
        l_mwsbk     LIKE vbrk-mwsbk,
        l_totalg    LIKE vbrk-netwr,
        l_totalusd  LIKE vbrk-netwr,
        l_ivausd    LIKE vbrk-netwr,
        l_netwrusd  LIKE vbrk-netwr,
        l_dec       TYPE char4,
        l_impue     LIKE vbrk-netwr,
        l_total     LIKE vbrk-mwsbk,
        l_total_otr TYPE char18,
        l_pernr     LIKE pa0185-pernr,
        l_bsark     TYPE vbkd-bsark,
        l_name      TYPE thead-tdname,
        l_object    TYPE thead-tdobject,
        l_id        TYPE thead-tdid,
        l_bstnk     TYPE vbak-bstnk,
        l_xblnr     TYPE vbrk-xblnr,
        l_dte       TYPE char3,
        l_folio     TYPE char17,
        l_vgbel     TYPE vbak-vgbel,
        l_kunrg     TYPE vbpa-kunnr,
        p_text      TYPE tdline,
        l_nomregio  TYPE t005u-bezei,

        t001_ls     LIKE t001,
        adrc_ls     LIKE adrc,
        adr6_ls     LIKE adr6,
        kna1_ls     LIKE kna1,

        dte_lv      LIKE edocldteacc-dte_type,
        etype_lv    LIKE t003edoc-edoc_type,
        blart_lv    LIKE edocldteacc-blart.

  CLEAR: cab_ls, header_ls.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.
  MOVE-CORRESPONDING data_ls-document_item   TO item_lt.

  bset_lt[] = data_ls-bset[].
  bkpf_ls   = data_ls-bkpf.

  vbpa_lt[] = data_ls-partner_data[].

* Tipo Documento
  cab_ls-iddoc-tipodte = header_ls-xblnr+0(3).
  dte_lv          = header_ls-xblnr+0(3).

  IF cab_ls-iddoc-tipodte EQ '110' OR cab_ls-iddoc-tipodte EQ '111' OR cab_ls-iddoc-tipodte EQ '112'.
    gv_faexp = 'X'.
  ENDIF.

  SELECT SINGLE blart
  INTO blart_lv
  FROM edocldteacc
  WHERE dte_type = dte_lv.

  IF sy-subrc = 0.
    SELECT SINGLE edoc_type
    INTO etype_lv
    FROM t003edoc
    WHERE blart     = blart_lv  AND
    edoc_type = 'CL_BOLETA'.

    IF sy-subrc = 0.
      gv_boleta = abap_true.
    ENDIF.
  ENDIF.

* Folio Documento
  cab_ls-iddoc-folio  = header_ls-xblnr+4.

* Fecha de emisión
  PERFORM formateo_fecha  USING    data_ls-document_header-fkdat
  CHANGING  cab_ls-iddoc-fchemis.

* Forma de pago
  SELECT SINGLE fmapago INTO cab_ls-iddoc-fmapago
    FROM zsd_forma_pago
    WHERE zterm EQ  data_ls-document_header-zterm.

* Fecha de vencimiento
  SELECT SINGLE ztag1
  INTO l_ztag1
  FROM t052
  WHERE zterm = header_ls-zterm.

  l_fchven = header_ls-fkdat + l_ztag1.

  PERFORM formateo_fecha  USING     l_fchven
  CHANGING  cab_ls-iddoc-fchvenc.

* Rut Emisor
  SELECT SINGLE paval
  INTO cab_ls-emisor-rutemisor
  FROM t001z
  WHERE bukrs EQ header_ls-bukrs  AND
  party EQ 'RESDAT'.


  IF sy-subrc NE 0.
    SELECT SINGLE paval
     INTO cab_ls-emisor-rutemisor
     FROM t001z
     WHERE bukrs EQ data_ls-source_header-bukrs  AND
     party EQ 'TAXNR'.
  ENDIF.

  CLEAR: adrc_ls.
  SELECT SINGLE *
  INTO t001_ls
  FROM t001
  WHERE bukrs = header_ls-bukrs.

  IF sy-subrc = 0.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
    INTO adrc_ls
    FROM adrc
    WHERE addrnumber = t001_ls-adrnr.
  ENDIF.

* Razón Social

  cab_ls-emisor-rznsoc    = adrc_ls-name1.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-emisor-rznsoc WITH 'y'.

***
* Giro emisor
  cab_ls-emisor-giroemis = adrc_ls-name3.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-emisor-giroemis WITH 'y'.

* Dirección origen
  CONCATENATE adrc_ls-street
  adrc_ls-house_num1
  INTO cab_ls-emisor-dirorigen
  SEPARATED BY space.

* Comuna origen

  cab_ls-emisor-ciudadorigen  = adrc_ls-city1.

* Ciudad origen

  cab_ls-emisor-cmnaorigen    = adrc_ls-city2.

  IF cab_ls-emisor-cmnaorigen IS INITIAL.
    cab_ls-emisor-cmnaorigen = adrc_ls-city1.
  ENDIF.

* Codigo sucursal
  CLEAR item_ls.
  READ TABLE item_lt INTO item_ls INDEX 1.
  cab_ls-emisor-sucursal = item_ls-werks.

* Codigo SII Sucursal

  SELECT SINGLE acteco INTO cab_ls-emisor-cdgsiisucur
    FROM zsd_sucursales
    WHERE werks EQ item_ls-werks.


* Código del vendedor
  READ TABLE data_ls-partner_data INTO vbpa_ls WITH KEY parvw = 'VD'.
  cab_ls-emisor-cdgvendedor = vbpa_ls-kunnr.

* Nombre del vendedor

  SELECT SINGLE name1 INTO cab_ls-emisor-nomvendedor
    FROM kna1
    WHERE kunnr EQ vbpa_ls-kunnr.

* Sucursal
  READ TABLE data_ls-document_item INTO item_ls INDEX 1.
  IF sy-subrc EQ 0 AND cab_ls-iddoc-tipodte NE '039' AND cab_ls-iddoc-tipodte NE '040' AND cab_ls-iddoc-tipodte NE '042'.
    cab_ls-emisor-sucursal = item_ls-werks.
  ELSE.
    cab_ls-emisor-sucursal = ''.
  ENDIF.
* Rut receptor
  CLEAR: kna1_ls, adr6_ls.
  SELECT SINGLE *                             "#EC CI_ALL_FIELDS_NEEDED
  INTO kna1_ls
  FROM kna1
  WHERE kunnr = header_ls-kunrg.

  IF sy-subrc = 0.

    IF kna1_ls-ktokd EQ 'ZEMP'.
      CLEAR: l_pernr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = header_ls-kunrg
        IMPORTING
          output = l_pernr.

      SELECT SINGLE icnum INTO kna1_ls-stcd1
        FROM pa0185
        WHERE pernr EQ l_pernr
        AND   subty EQ '01'
        AND   endda >= sy-datum.

    ELSE.
      SELECT SINGLE *                         "#EC CI_ALL_FIELDS_NEEDED
      INTO adr6_ls
      FROM adr6
      WHERE addrnumber = kna1_ls-adrnr.
    ENDIF.


  ENDIF.

  TRANSLATE kna1_ls-stcd1 USING '. '.
  CONDENSE kna1_ls-stcd1 NO-GAPS.
  IF cab_ls-iddoc-tipodte EQ '110' OR cab_ls-iddoc-tipodte EQ '111' OR cab_ls-iddoc-tipodte EQ '112'.
    cab_ls-receptor-rutrecep  = '55555555-5'.
  ELSE.
    cab_ls-receptor-rutrecep  = kna1_ls-stcd1.
    IF kna1_ls-stcd1 IS INITIAL.

      SELECT SINGLE taxnum INTO kna1_ls-stcd1
        FROM dfkkbptaxnum
        WHERE partner EQ header_ls-kunrg.
      cab_ls-receptor-rutrecep  = kna1_ls-stcd1.
    ENDIF.
  ENDIF.



* Código interno receptor
  cab_ls-receptor-cdgintrecep = header_ls-kunrg.

* Razón social receptor
  CONCATENATE kna1_ls-name1
  kna1_ls-name2
  INTO cab_ls-receptor-rznsocrecep
  SEPARATED BY space.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-receptor-rznsocrecep WITH 'y'.

  " Giro receptor

  CONCATENATE kna1_ls-name3 kna1_ls-name4 INTO cab_ls-receptor-girorecep SEPARATED BY space.

  IF cab_ls-receptor-girorecep EQ space.
    cab_ls-receptor-girorecep = '.'.
  ENDIF.

  IF cab_ls-receptor-girorecep IS INITIAL AND ( cab_ls-iddoc-tipodte EQ '39' OR cab_ls-iddoc-tipodte EQ '40' OR cab_ls-iddoc-tipodte EQ '42').
    cab_ls-receptor-girorecep = 'Sin Giro'.
  ENDIF.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-receptor-girorecep WITH 'y'.

* Direccion Receptor

  IF gv_faexp = 'X'.
    CLEAR l_nomregio.
    SELECT SINGLE bezei INTO l_nomregio
      FROM t005u
      WHERE spras EQ sy-langu
      AND land1 EQ kna1_ls-land1
      AND bland EQ kna1_ls-regio.

    CONCATENATE kna1_ls-stras kna1_ls-pstlz kna1_ls-ort01 l_nomregio INTO cab_ls-receptor-dirrecep SEPARATED BY space.
  ELSE.
    cab_ls-receptor-dirrecep    = kna1_ls-stras.
  ENDIF.


* Comuna Receptor
  cab_ls-receptor-cmnarecep   = kna1_ls-ort02.

* Ciudad Receptor
  cab_ls-receptor-ciudadrecep = kna1_ls-ort01.

  IF cab_ls-receptor-ciudadrecep IS INITIAL.
    cab_ls-receptor-ciudadrecep = kna1_ls-ort01.
  ENDIF.

  IF cab_ls-receptor-cmnarecep IS INITIAL.
    cab_ls-receptor-cmnarecep = kna1_ls-ort01.
  ENDIF.


* Monto Neto
  IF bkpf_ls-kursf IS NOT INITIAL.
    IF header_ls-mwsbk > 0.
      CLEAR bset_ls.
      READ TABLE bset_lt INTO bset_ls INDEX 1.

      IF sy-subrc = 0.
        l_netwr = header_ls-netwr.
        l_kurs2 = bkpf_ls-kursf.
        l_kurs2 = 0 - l_kurs2.
      ENDIF.

      PERFORM formateo_montos USING     l_netwr
            bkpf_ls-waers
      CHANGING  cab_ls-totales-mntneto.

      PERFORM formateo_montos USING    bset_ls-hwbas
            bkpf_ls-hwaer
      CHANGING  cab_ls-totales-mnt_neto_otr_mnda.


      PERFORM formateo_montos USING     l_kurs2
            bkpf_ls-hwaer
      CHANGING  cab_ls-totales-tpo_cambio.

      CONDENSE cab_ls-totales-tpo_cambio.

    ELSE.

      CLEAR bset_ls.
      READ TABLE bset_lt INTO bset_ls INDEX 1.

      cab_ls-totales-mntneto           = '0'.
      cab_ls-totales-mnt_neto_otr_mnda = '0'.

      PERFORM formateo_montos USING    bset_ls-hwbas
           bkpf_ls-hwaer
     CHANGING  cab_ls-totales-mnt_exe_otr_mnda.
*  INI 01/08/2023 SOR1 4000358089
*      cab_ls-totales-tipo_moneda = 'DOLAR USA'.
      SELECT SINGLE glosa
        FROM ztsd_moneda
        INTO @DATA(lv_glosa)
        WHERE moneda EQ @bkpf_ls-waers.

      IF lv_glosa IS NOT INITIAL.
        cab_ls-totales-tipo_moneda = lv_glosa.
      ENDIF.
*  FIN 01/08/2023 SOR1 4000358089
      cab_ls-totales-mnt_tot_otr_mnda = cab_ls-totales-mnt_exe_otr_mnda.
      CONDENSE: cab_ls-totales-mnt_tot_otr_mnda, cab_ls-totales-mnt_exe_otr_mnda.

      l_kurs2 = bkpf_ls-kursf.
      l_kurs2 = 0 - l_kurs2.

      PERFORM formateo_montos USING     l_kurs2
         bkpf_ls-hwaer
   CHANGING  cab_ls-totales-tpo_cambio.

      CONDENSE cab_ls-totales-tpo_cambio.


    ENDIF.
  ELSE.
    IF header_ls-mwsbk > 0.
      IF bkpf_ls-waers = bkpf_ls-hwaer.

*        IF data_ls-document_header-fkart EQ 'ZFTU'. "Factura Turista HHERNANDEZ
*          l_netwr = header_ls-netwr + header_ls-mwsbk.
*        ELSE.

        l_netwr = header_ls-netwr.

*        ENDIF.


      ELSE.
        l_netwr = ( header_ls-netwr * header_ls-kurrf ) / 100.
      ENDIF.

      PERFORM formateo_montos USING     l_netwr
      header_ls-waerk
CHANGING  cab_ls-totales-mntneto.
    ELSE.
      cab_ls-totales-mntneto = '0'.
    ENDIF.
  ENDIF.

* Monto Exento
  IF bkpf_ls-kursf IS NOT INITIAL.
    IF header_ls-mwsbk = 0.
      CLEAR bset_ls.
      READ TABLE bset_lt INTO bset_ls INDEX 1.

      IF sy-subrc = 0.
        l_netwr = bset_ls-fwbas.
      ENDIF.
      PERFORM formateo_montos USING     l_netwr
            bkpf_ls-waers
      CHANGING  cab_ls-totales-mntexe.
      IF cab_ls-iddoc-tipodte = '33'.
        cab_ls-iddoc-tipodte = '34'.
      ENDIF.
      IF cab_ls-iddoc-tipodte = '034'.
        l_netwr = header_ls-netwr.
        PERFORM formateo_montos USING     l_netwr
             'CLP'
       CHANGING  cab_ls-totales-mntexe.
      ENDIF.
    ELSE.
      cab_ls-totales-mntexe = '0'.
      cab_ls-totales-mnt_exe_otr_mnda = '0'.
    ENDIF.
  ELSE.
    IF header_ls-mwsbk = 0.
      IF bkpf_ls-waers = bkpf_ls-hwaer.
        l_netwr = header_ls-netwr.
      ELSE.
        l_netwr = ( header_ls-netwr * header_ls-kurrf ) / 100.
      ENDIF.

      PERFORM formateo_montos USING     l_netwr
            header_ls-waerk
      CHANGING  cab_ls-totales-mntexe.
      IF cab_ls-iddoc-tipodte = '33'.
        cab_ls-iddoc-tipodte = '34'.
      ENDIF.
    ELSE.
      cab_ls-totales-mntexe = '0'.
    ENDIF.
  ENDIF.

  item_lt[] = data_ls-document_item[].
  cond_lt[] = data_ls-conditions_record[].

  IF gv_faexp IS NOT INITIAL.
    l_netwr = header_ls-netwr.
    PERFORM formateo_montos USING     l_netwr
          header_ls-waerk
    CHANGING  cab_ls-totales-mntexe.

    l_kurs2 = bkpf_ls-kurs3.
    PERFORM formateo_montos USING     l_kurs2
            bkpf_ls-hwae3
      CHANGING  cab_ls-totales-tpo_cambio.

    CONDENSE cab_ls-totales-tpo_cambio.

    CLEAR: item_ls, l_seguro, l_flete.
    LOOP AT item_lt INTO item_ls.
      l_seguro = l_seguro + item_ls-kzwi3.
      l_flete  = l_flete  + item_ls-kzwi2.
    ENDLOOP.

    IF l_seguro IS NOT INITIAL.
      PERFORM formateo_montos USING l_seguro
          header_ls-waerk
    CHANGING  cab_ls-totales-mntseguro.
    ENDIF.

    IF l_flete IS NOT INITIAL.
      PERFORM formateo_montos USING l_flete
          header_ls-waerk
    CHANGING  cab_ls-totales-mntflete.
    ENDIF.

  ENDIF.

* Tasa IVA


  CLEAR: l_kbetr, l_kwert.

  IF bkpf_ls-kursf IS NOT INITIAL.
    l_kbetr = bset_ls-kbetr.
    l_mwsbk = bset_ls-fwste.

    IF l_kbetr = 0.
      cab_ls-totales-tasaiva = space."'00.00'.
    ELSE.
      PERFORM formateo_tasa USING     l_kbetr
      CHANGING  cab_ls-totales-tasaiva.
    ENDIF.


*   IVA

    IF l_mwsbk = 0.
      cab_ls-totales-iva = space."'0'.
    ELSE.
      PERFORM formateo_montos USING     l_mwsbk
            bkpf_ls-waers
      CHANGING  cab_ls-totales-iva.

      PERFORM formateo_montos USING     bset_ls-hwste
           bkpf_ls-hwaer
     CHANGING  cab_ls-totales-ivaotr_mnda.

    ENDIF.
  ELSE.
    CLEAR: l_condiva.
    IF data_ls-document_header-fkart NE 'ZFCA'.
      l_condiva = 'MWST'.
    ELSE.
      l_condiva = 'MWSI'.
    ENDIF.

    PERFORM impuestos TABLES  item_lt
      cond_lt
    USING l_condiva
          l_kbetr
          l_kwert.

    IF l_kbetr = 0.
      cab_ls-totales-tasaiva = space." '00.00'.
    ELSE.
      PERFORM formateo_tasa USING     l_kbetr
      CHANGING  cab_ls-totales-tasaiva.
    ENDIF.

*   IVA
    IF l_kwert = 0.
      cab_ls-totales-iva = space." '0'.
    ELSE.
      IF bkpf_ls-waers = bkpf_ls-hwaer.
        l_mwsbk = l_kwert.
      ELSE.
        l_mwsbk = ( l_kwert * header_ls-kurrf ) / 100.
      ENDIF.

      PERFORM formateo_montos USING     l_mwsbk
            header_ls-waerk
      CHANGING  cab_ls-totales-iva.

      PERFORM formateo_montos USING     bset_ls-fwste
            bkpf_ls-hwaer
      CHANGING  cab_ls-totales-ivaotr_mnda.

    ENDIF.
  ENDIF.

  IF gv_faexp IS NOT INITIAL.
    cab_ls-totales-tasaiva = space."'00.00'.
    cab_ls-totales-iva = space."'0'.
  ENDIF.

  IF cab_ls-iddoc-tipodte = '061' OR cab_ls-iddoc-tipodte = '61'.
*** valido si le estoy haciendo una NC a una factura exenta ****
    CLEAR: l_bstnk, l_vgbel, l_dte, l_folio.
    SELECT SINGLE bstnk vgbel INTO ( l_bstnk, l_vgbel )
      FROM vbak
      WHERE vbeln EQ item_ls-aubel.

    SPLIT l_bstnk AT '-' INTO l_dte l_folio.

    IF l_dte EQ '034' OR l_dte EQ '34'.
      cab_ls-totales-tasaiva = '19'.
      cab_ls-totales-iva = '0'.
    ENDIF.

    CLEAR l_xblnr.
    SELECT SINGLE xblnr INTO l_xblnr
      FROM vbrk
      WHERE vbeln EQ l_vgbel.

    IF l_xblnr+0(3) EQ '034'.
      cab_ls-totales-tasaiva = '19'.
      cab_ls-totales-iva = '0'.
    ENDIF.

**** si es una nota de credito administrativa debe ir lo siguiente *****

    IF header_ls-fkart EQ 'ZAA2' OR header_ls-fkart EQ 'ZCD1'.
      cab_ls-totales-tasaiva = '19'.
      cab_ls-totales-iva = '0'.
    ENDIF.

  ENDIF.

* Impresora

  SELECT SINGLE impresora INTO cab_ls-emisor-impresora
    FROM zsd_impresoras
    WHERE usuario EQ sy-uname.

******** Observaciones texto cabecera ****

  IF data_ls-document_header-fkart NE 'ZMOD'.
    l_name = data_ls-document_header-vbeln.
    IF cab_ls-iddoc-tipodte EQ '110'.
      l_id = 'Z024'.
    ELSE.
      IF data_ls-document_header-fkart EQ 'ZFET'.
        l_id = 'Z031'.
      ELSE.
        l_id = '0002'.
      ENDIF.

    ENDIF.

    l_object = 'VBBK'.
    PERFORM get_text USING l_name
                           l_id
                           l_object
                    CHANGING p_text.

    cab_ls-receptor-observacion = p_text.
    PERFORM format_texto CHANGING cab_ls-receptor-observacion.    "Ajuste por caracteres especiales
  ENDIF.
*---------------------------------------


*-----------------------------*
* Impuestos específicos
*--------------------------------------------------------------------*
*  CLEAR l_impue.
*  CLEAR l_impue.
*  PERFORM imp_esp    TABLES cond_lt
*                     USING  header_ls-waerk
*                     CHANGING  cab_ls
*                               l_impue.

* Monto Total

  l_total = l_mwsbk + l_netwr - l_desc.



  IF bkpf_ls-kursf IS NOT INITIAL.

    IF cab_ls-iddoc-tipodte = '034'.

      PERFORM formateo_montos USING     l_total
      header_ls-waerk
        CHANGING  cab_ls-totales-mnttotal.


    ELSE.
      PERFORM formateo_montos USING     l_total
            bkpf_ls-waers
      CHANGING  cab_ls-totales-mnttotal.

      cab_ls-totales-mnt_tot_otr_mnda = cab_ls-totales-mnt_neto_otr_mnda + cab_ls-totales-ivaotr_mnda.
      CONDENSE cab_ls-totales-mnt_tot_otr_mnda.

*  INI 01/08/2023 SOR1 4000358089
*     cab_ls-totales-tipo_moneda = 'DOLAR USA'.
      SELECT SINGLE glosa
        FROM ztsd_moneda
        INTO @DATA(lv_glosa2)
        WHERE moneda EQ @bkpf_ls-waers.

      IF lv_glosa2 IS NOT INITIAL.
        cab_ls-totales-tipo_moneda = lv_glosa2.
      ENDIF.
*  FIN 01/08/2023 SOR1 4000358089

      IF cab_ls-iddoc-tipodte = '061' OR cab_ls-iddoc-tipodte = '61'.

        IF cab_ls-totales-mnt_exe_otr_mnda IS NOT INITIAL AND cab_ls-totales-mnt_exe_otr_mnda NE '0'.
          cab_ls-totales-mnt_tot_otr_mnda = cab_ls-totales-mnt_exe_otr_mnda.
        ENDIF.

      ENDIF.


    ENDIF.

  ELSE.
    PERFORM formateo_montos USING     l_total
          header_ls-waerk
    CHANGING  cab_ls-totales-mnttotal.
  ENDIF.

  IF gv_faexp IS NOT INITIAL.
    l_total = l_netwr.

    PERFORM formateo_montos USING     l_total
          header_ls-waerk
    CHANGING  cab_ls-totales-mnttotal.


    CLEAR: bset_ls, l_total.
    READ TABLE bset_lt INTO bset_ls INDEX 1.
    IF sy-subrc EQ 0.
      l_total = bset_ls-h3bas.

      PERFORM formateo_montos USING     l_total
           'CLP'
     CHANGING  cab_ls-totales-mnt_tot_otr_mnda.

      cab_ls-totales-mnt_exe_otr_mnda = cab_ls-totales-mnt_tot_otr_mnda.

      cab_ls-totales-tipo_moneda = 'PESO CL'.

    ENDIF.
  ENDIF.

  IF header_ls-fkart EQ 'ZIN1' AND header_ls-waerk NE 'CLP'.
    l_kurs2 = bkpf_ls-kurs3.
*    l_kurs2 = 0 - l_kurs2.

    PERFORM formateo_montos USING     l_kurs2
       bkpf_ls-hwae3
 CHANGING  cab_ls-totales-tpo_cambio.

    CONDENSE cab_ls-totales-tpo_cambio.

    l_netwr = header_ls-netwr.
    l_totalg =  l_netwr + l_mwsbk.
    l_totalusd = l_totalg * l_kurs2.
    l_ivausd = l_mwsbk * l_kurs2.
    l_netwrusd = l_netwr * l_kurs2.

    cab_ls-totales-mnttotal = l_totalusd.
    REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-mnttotal WITH ','.
    CONDENSE cab_ls-totales-mnttotal.


    cab_ls-totales-iva =  l_ivausd.
    REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-iva WITH ','.
    CONDENSE cab_ls-totales-iva.

    IF l_ivausd EQ 0.
      cab_ls-totales-tasaiva = '0'.
      cab_ls-totales-mntexe = l_netwrusd.
      REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-mntexe WITH ','.
      CONDENSE cab_ls-totales-mntexe.


    ELSE.
      cab_ls-totales-mntneto = l_netwrusd.
      REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-mntneto WITH ','.
      CONDENSE cab_ls-totales-mntneto.

      IF cab_ls-totales-mntneto IS INITIAL.
        cab_ls-totales-mntneto = '0'.
      ENDIF.
      cab_ls-totales-tasaiva = '19'.
    ENDIF.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mnttotal AT ',' INTO  cab_ls-totales-mnttotal l_dec.
    IF l_dec >= 50.
      cab_ls-totales-mnttotal = cab_ls-totales-mnttotal + 1.
      CONDENSE cab_ls-totales-mnttotal.
    ENDIF.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mntneto  AT ',' INTO  cab_ls-totales-mntneto  l_dec.
    IF l_dec >= 50.
      cab_ls-totales-mntneto  = cab_ls-totales-mntneto  + 1.
      CONDENSE cab_ls-totales-mntneto.
    ENDIF.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mntexe AT ',' INTO  cab_ls-totales-mntexe l_dec.
    IF l_dec >= 50.
      cab_ls-totales-mntexe = cab_ls-totales-mntexe + 1.
      CONDENSE cab_ls-totales-mntexe.
    ENDIF.


    CLEAR l_dec.
    SPLIT  cab_ls-totales-iva AT ',' INTO   cab_ls-totales-iva l_dec.
    IF l_dec >= 50.
      cab_ls-totales-iva =  cab_ls-totales-iva + 1.
      CONDENSE cab_ls-totales-iva.
    ENDIF.

  ENDIF.

********* Datos exportacion *****

  DATA l_tknum   TYPE tknum.
  DATA l_zzvdats TYPE vbak-zzvdats.
  DATA l_vsart   TYPE vsart.
  DATA l_knota   TYPE knota.
  DATA l_knotz   TYPE knotz.
  DATA l_kunwe   TYPE kunnr.
  DATA l_anzpk   TYPE anzpk.
  DATA l_landwe  TYPE land1.
  DATA l_zzmodv  TYPE vbak-zzmodv.

*Incoterms.

  SELECT SINGLE inco1_sii INTO cab_ls-exportacion-cod_clau_venta
    FROM zsd_claus_venta
    WHERE inco EQ  header_ls-inco1.

  CONDENSE cab_ls-exportacion-cod_clau_venta.

*Modalidad de venta, Fecha de vencimiento en pedido de ventas
  CLEAR: l_zzmodv, l_zzvdats.
  SELECT SINGLE zzmodv zzvdats INTO (  l_zzmodv, l_zzvdats )
    FROM vbak
    WHERE vbeln EQ item_ls-aubel.

  SELECT SINGLE mod_sii INTO cab_ls-exportacion-cod_mod_venta
    FROM zsd_modventa_exp
    WHERE zzmodv EQ l_zzmodv.

  CONDENSE  cab_ls-exportacion-cod_mod_venta.

*  PERFORM formateo_fecha  USING     l_zzvdats
* CHANGING  cab_ls-exportacion-fecha_venc.


  "Obtener documentos de transporte asociados al flujo de exportacion.

  REFRESH: lt_vbfa_tr[], lt_vttk[].
  CLEAR:   ls_vbfa_tr, ls_vttk.

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE  lt_vbfa_tr
    FROM vbfa
    WHERE vbelv EQ item_ls-vgbel
    AND   vbtyp_n EQ '8'.

  IF lt_vbfa_tr[] IS NOT INITIAL.

    SELECT  tknum                                  "#EC CI_NO_TRANSFORM
            shtyp
            tdlnr
            route
            vsart
            add01
            add02
            add03
            text1
            text2
            text3
            text4
            datbg
            signi INTO CORRESPONDING FIELDS OF TABLE lt_vttk  FROM vttk FOR ALL ENTRIES IN lt_vbfa_tr
       WHERE tknum EQ  lt_vbfa_tr-vbeln.
  ENDIF.
  IF gv_faexp = 'X'.
    CLEAR ls_vttk.
    READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT02'.

    IF sy-subrc NE 0.
      CLEAR ls_vttk.
      READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT03'.

      IF sy-subrc NE 0.
        CLEAR ls_vttk.
        READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT04'.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR ls_vttk.
    READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT01'.
  ENDIF.

  SELECT SINGLE via_transp INTO cab_ls-exportacion-cod_via_transp
    FROM zclfel_tb_viat
    WHERE shtyp EQ ls_vttk-shtyp.

*  cab_ls-exportacion-cod_via_transp = ls_vttk-vsart.
  cab_ls-exportacion-nave           = ls_vttk-text1.
*  cab_ls-exportacion-viaje          = ls_vttk-text2.
  PERFORM format_texto CHANGING cab_ls-exportacion-nave.    "Ajuste por caracteres especiales

  DATA: l_nompatente TYPE vttk_add02_t.


  CLEAR l_nompatente.
  SELECT SINGLE bezei INTO l_nompatente
    FROM vtadd02t                                       "#EC CI_GENBUFF
    WHERE add_info EQ ls_vttk-add02.

  IF sy-subrc EQ 0.
    cab_ls-transporte-nombrechofer = l_nompatente.
  ENDIF.

**** nombre transporte ****
  IF ls_vttk-shtyp EQ 'ZT02'.

    SELECT SINGLE bezei INTO cab_ls-transporte-nombre_transp
      FROM vtadd02t
      WHERE spras EQ 'S'
      AND add_info EQ ls_vttk-add02.

  ELSE.
    SELECT SINGLE mc_name1 INTO cab_ls-transporte-nombre_transp
       FROM but000
       WHERE partner EQ ls_vttk-tdlnr.
  ENDIF.

* Puerto de embarque y desembarque
  CLEAR: l_knota, l_knotz.
  SELECT SINGLE  knota knotz INTO ( l_knota, l_knotz )
    FROM vtts
    WHERE tknum EQ ls_vttk-tknum
    AND route EQ ls_vttk-route.

  SELECT SINGLE puerto_sii INTO cab_ls-exportacion-pto_embarque
    FROM zsd_puertos_exp
    WHERE knota EQ l_knota.

  SELECT SINGLE puerto_sii INTO cab_ls-exportacion-pto_desembarque
    FROM zsd_puertos_exp
    WHERE knota EQ l_knotz.

  SELECT SINGLE bezei INTO cab_ls-exportacion-lugar_destino
    FROM tvknt
    WHERE spras EQ 'E'
    AND   knote EQ l_knotz.

  PERFORM format_texto CHANGING cab_ls-exportacion-lugar_destino.   "Ajuste por caracteres especiales


*  INI 01/08/2023 SOR1 4000358089
*      cab_ls-exportacion-tipo_moneda     = 'DOLAR USA'.
  SELECT SINGLE glosa
    FROM ztsd_moneda
    INTO @DATA(lv_glosa3)
    WHERE moneda EQ @bkpf_ls-waers.

  IF lv_glosa3 IS NOT INITIAL.
    cab_ls-exportacion-tipo_moneda = lv_glosa3.
  ENDIF.
*  FIN 01/08/2023 SOR1 4000358089

  CLEAR l_kunwe.

*  SELECT SINGLE kunnr INTO l_kunwe
*    FROM likp
*    WHERE vbeln EQ item_ls-vgbel.

*  IF l_kunwe IS INITIAL.

  SELECT SINGLE kunrg INTO l_kunwe
     FROM vbrk
     WHERE vbeln EQ item_ls-vbeln.

*  ENDIF.
  IF sy-subrc EQ 0.
    SELECT SINGLE land1 INTO l_landwe
        FROM kna1
        WHERE kunnr EQ l_kunwe.
  ENDIF.

  CLEAR partner_ls.
  READ TABLE vbpa_lt INTO partner_ls WITH KEY parvw = 'WE'.

  SELECT SINGLE pais_sii INTO cab_ls-exportacion-pais_receptor
    FROM zsd_cod_pais
    WHERE land1 EQ partner_ls-land1.

  CONDENSE cab_ls-exportacion-pais_receptor.

  CLEAR partner_ls.
  READ TABLE vbpa_lt INTO partner_ls WITH KEY parvw = 'RG'.

  SELECT SINGLE pais_sii INTO cab_ls-exportacion-pais_destino
    FROM zsd_cod_pais
    WHERE land1 EQ partner_ls-land1.

  CONDENSE cab_ls-exportacion-pais_destino.



  SELECT SINGLE pais_sii INTO cab_ls-exportacion-pais_emisor
    FROM zsd_cod_pais
    WHERE land1 EQ data_ls-source_header-land.

  CONDENSE cab_ls-exportacion-pais_emisor.

**** Cantidad de bultos *****

*ini mod 4S-SD-ELP2-4000422080-Bultos Transaccion ZD003  Vtas Export

  DATA is_export TYPE abap_bool VALUE abap_false.
  DATA lo_class TYPE REF TO zcl_sd_invoice_validations.

  is_export = abap_false.
  TRY .
      lo_class    =  zcl_sd_invoice_validations=>get_instance( ).
      IF lo_class->is_export_invoice( data_ls ) = abap_true.
        cab_ls-exportacion-cantbultos = lo_class->get_export_invoice_packages( ).
        is_export = abap_true.
      ENDIF.
    CATCH cx_root INTO DATA(ex_root).
      WRITE ex_root->get_text( ). " Only for ATC compiler.
  ENDTRY.

  IF is_export = abap_false.

    REFRESH item_lt2[].
    item_lt2[] = item_lt[].

    SORT item_lt2 BY vgbel DESCENDING.
    DELETE ADJACENT DUPLICATES FROM item_lt2 COMPARING vgbel.

    CLEAR item_ls_b.
    LOOP AT item_lt2 INTO item_ls_b.

      CLEAR l_anzpk.
      SELECT SINGLE anzpk INTO l_anzpk
      FROM likp
      WHERE vbeln EQ item_ls_b-vgbel.

      cab_ls-exportacion-cantbultos = cab_ls-exportacion-cantbultos + l_anzpk.

    ENDLOOP.

  ENDIF.

*Fin mod 4S-SD-ELP2-4000422080-Bultos Transaccion ZD003  Vtas Export

  CONDENSE cab_ls-exportacion-cantbultos.


  IF partner_ls-land1 NE data_ls-document_header-land1.

    SELECT SINGLE landx INTO cab_ls-receptor-pais_cliente
      FROM t005t
      WHERE spras EQ sy-langu
      AND   land1 EQ partner_ls-land1.

    PERFORM format_texto CHANGING cab_ls-receptor-pais_cliente.    "Ajuste por caracteres especiales
  ENDIF.

*--------------------------------------------------------------------*
* BOLETA
*--------------------------------------------------------------------*
*  IF gv_boleta IS NOT INITIAL.
*    cab_ls-totales-mntexe    = cab_ls-totales-mntneto.
*    cab_ls-totales-mnttotal  = cab_ls-totales-mntneto.
*    CLEAR: cab_ls-totales-mntneto,
*    cab_ls-totales-iva,
*    cab_ls-totales-tasaiva,
*    cab_ls-totales-ivanoret.
*  ENDIF.


************************* REGULARIZACION DE DOCUMENTOS ATRASADOS (CONTINGENCIA) solo para clase de factura exportacion tributaria especial ***************

  IF header_ls-fkart EQ 'ZEET'.



    DATA: id       LIKE  thead-tdid,
          name     LIKE  thead-tdname,
          object   LIKE  thead-tdobject,
          languaje TYPE  sy-langu.

    DATA: lines_lt LIKE TABLE OF tline,
          lines_ls LIKE          tline.

    DATA: l_titulo TYPE char40.
    DATA: l_valor  TYPE char100.
    DATA: l_spras  TYPE spras.


    CLEAR l_spras.
    SELECT SINGLE spras INTO l_spras
      FROM kna1
      WHERE kunnr EQ header_ls-kunrg.

    id       = 'Z027'.
    name     = header_ls-vbeln.
    object   = 'VBBK'.
    languaje = l_spras.


    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = id
        language                = languaje
        name                    = name
        object                  = object
      TABLES
        lines                   = lines_lt
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF lines_lt[] IS NOT INITIAL.


******* via de transporte ***********

      CLEAR: lines_ls, l_titulo, l_valor.
      READ TABLE lines_lt INTO lines_ls INDEX 13.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      SELECT SINGLE via_transp INTO cab_ls-exportacion-cod_via_transp
       FROM zclfel_tb_viat
       WHERE shtyp EQ lines_ls-tdline.

      CONDENSE cab_ls-exportacion-cod_via_transp.

******* Nombre del transporte ************

      CLEAR: lines_ls, l_titulo, l_valor.
      READ TABLE lines_lt INTO lines_ls INDEX 12.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      cab_ls-transporte-nombre_transp =  lines_ls-tdline.
      CONDENSE cab_ls-transporte-nombre_transp.

***** Puerto de Embarque *********

      CLEAR: lines_ls, l_titulo, l_valor.
      READ TABLE lines_lt INTO lines_ls INDEX 18.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      CLEAR l_knota.
      l_knota = lines_ls-tdline.
      SELECT SINGLE puerto_sii INTO cab_ls-exportacion-pto_embarque
        FROM zsd_puertos_exp
        WHERE knota EQ l_knota.

************ Puerto de desembarque **********

      CLEAR: lines_ls, l_titulo, l_valor.
      READ TABLE lines_lt INTO lines_ls INDEX 19.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      CLEAR l_knotz.
      l_knotz = lines_ls-tdline.
      SELECT SINGLE puerto_sii INTO cab_ls-exportacion-pto_desembarque
        FROM zsd_puertos_exp
        WHERE knota EQ l_knotz.


********** Cantidad de bultos *******


      CLEAR: lines_ls, l_titulo, l_valor.
      READ TABLE lines_lt INTO lines_ls INDEX 14.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      cab_ls-exportacion-cantbultos = lines_ls-tdline.
      CONDENSE cab_ls-exportacion-cantbultos.

******** pais receptor ******

      CLEAR: lines_ls, l_titulo, l_valor.
*      READ TABLE lines_lt INTO lines_ls INDEX 31.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.
*
*      cab_ls-exportacion-pais_receptor = lines_ls-tdline.
*      CONDENSE cab_ls-exportacion-pais_receptor.


******** pais emisor ******

      CLEAR: lines_ls, l_titulo, l_valor.
*      READ TABLE lines_lt INTO lines_ls INDEX 31.
**      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
**      CONDENSE: l_titulo, l_valor.
*
*      cab_ls-exportacion-pais_emisor = lines_ls-tdline.
*      CONDENSE cab_ls-exportacion-pais_emisor.


***** Lugar de destino *****

      CLEAR: lines_ls, l_titulo, l_valor.
      READ TABLE lines_lt INTO lines_ls INDEX 20.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      SELECT SINGLE bezei INTO cab_ls-exportacion-lugar_destino
        FROM tvknt
        WHERE spras EQ 'E'
        AND   knote EQ lines_ls-tdline.

      CONDENSE cab_ls-exportacion-lugar_destino.

      PERFORM format_texto CHANGING cab_ls-exportacion-lugar_destino.   "Ajuste por caracteres especiales


    ENDIF.

  ENDIF.

  dte_ev-encabezado = cab_ls.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FORMATEO_FECHA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS_DOCUMENT_HEADER_FKDAT
*&      <-- CAB_LS_IDDOC_FCHEMIS
*&---------------------------------------------------------------------*
FORM formateo_fecha USING    p_datum
CHANGING p_fecha.

  IF p_datum IS INITIAL.
    p_datum = sy-datum.
  ENDIF.

  CONCATENATE p_datum+0(4)
              p_datum+4(2)
              p_datum+6(2)
  INTO p_fecha
  SEPARATED BY '-'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FORMATEO_MONTOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_NETWR
*&      --> BKPF_LS_HWAER
*&      <-- CAB_LS_TOTALES_MNTNETO
*&---------------------------------------------------------------------*
FORM formateo_montos  USING    p_monto
      p_waers
CHANGING p_montox.

  DATA: monto_lv(18)  TYPE c.



  WRITE p_monto TO monto_lv CURRENCY p_waers
  NO-SIGN NO-GROUPING
  LEFT-JUSTIFIED.

  REPLACE ALL OCCURRENCES OF '.' IN monto_lv WITH ''.
  CONDENSE  monto_lv.
  TRANSLATE monto_lv USING ',.'.

  p_montox = monto_lv.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FORMATEO_TASA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_KBETR
*&      <-- CAB_LS_TOTALES_TASAIVA
*&---------------------------------------------------------------------*
FORM formateo_tasa  USING    p_kbetr
CHANGING p_tasa.

  DATA: tasa_lv(7)  TYPE c.


  WRITE p_kbetr TO tasa_lv ROUND 1 NO-SIGN DECIMALS 2
  LEFT-JUSTIFIED.
  TRANSLATE tasa_lv USING ',.'.

  p_tasa = tasa_lv.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> L_NAME
*&      --> L_ID
*&      --> L_OBJECT
*&      <-- P_TEXT
*&---------------------------------------------------------------------*
FORM get_text  USING    p_name
      p_id
      p_object
CHANGING p_text.

  DATA: id       LIKE  thead-tdid,
        name     LIKE  thead-tdname,
        object   LIKE  thead-tdobject,
        languaje TYPE sy-langu.

  DATA: lines_lt LIKE TABLE OF tline,
        lines_ls LIKE          tline.

  id      = p_id.
  name    = p_name.
  object  = p_object.

  IF p_object EQ 'MVKE' OR p_object EQ 'VBBP'.
    languaje = 'E'.
  ELSE.
    languaje = sy-langu.
  ENDIF.

  IF id EQ 'Z015' .
    languaje = 'E'.
  ENDIF.

  CLEAR: p_text.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = id
      language                = languaje
      name                    = name
      object                  = object
    TABLES
      lines                   = lines_lt
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CLEAR lines_ls.

  LOOP AT lines_lt INTO lines_ls.
    IF sy-tabix = 1.
      p_text =  lines_ls-tdline.
    ELSE.
      CONCATENATE p_text
      lines_ls-tdline
      INTO p_text
      SEPARATED BY space.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form IMPUESTOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ITEM_LT
*&      --> COND_LT
*&      --> L_CONDIVA
*&      --> L_KBETR
*&      --> L_KWERT
*&---------------------------------------------------------------------*
FORM impuestos  TABLES  item_lt   STRUCTURE vbrpvb
  cond_lt   STRUCTURE komv
USING   p_kschl
      p_porcent
      p_suma.

  DATA: cond_ls LIKE          komv,
        item_ls LIKE          vbrpvb.

  CLEAR : p_suma,
  p_porcent.

  LOOP AT item_lt INTO item_ls.                          "#EC CI_NESTED
    LOOP AT cond_lt INTO cond_ls                         "#EC CI_NESTED
    WHERE kschl = p_kschl AND
    kposn = item_ls-posnr.

      IF cond_ls-kbetr <> ''.
        p_porcent = cond_ls-kbetr.
      ENDIF.
      p_suma = p_suma + cond_ls-kwert.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ACTECO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM acteco   USING    data_ls TYPE  edoc_src_data_sd_invoice
CHANGING dte_ev  TYPE  zclfel_et_documento.

  DATA: header_ls TYPE edoc_vbrkvb.

  DATA: paval_lv  LIKE t001z-paval.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.

  IF sy-subrc = 0.
    dte_ev-encabezado-emisor-acteco = ''.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DES_Y_REC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM des_y_rec   USING    data_ls TYPE  edoc_src_data_sd_invoice
CHANGING dte_ev  TYPE  zclfel_et_documento.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETALLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      --> VBELN_VL
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM detalle  USING    data_ls  TYPE  edoc_src_data_sd_invoice
                       vbeln_vl
              CHANGING dte_ev   TYPE  zclfel_et_documento.

  DATA: deta_lt TYPE zclfel_t_detalle,
        deta_ls TYPE zclfel_detalle,
        cod_lt  TYPE zclfel_cdgitem_t,
        cod_ls  TYPE zclfel_cdgitem.

  DATA: t001_ls LIKE t001.

  DATA: waers_lv TYPE waers.


  DATA: item_lt   TYPE edoc_vbrpvb_tab,
        item_ls   TYPE vbrpvb,
        header_ls TYPE edoc_vbrkvb.

  DATA: lv_total TYPE p DECIMALS 3.


  DATA: linea_lv         LIKE sy-tabix,
        lv_netwr         LIKE vbrk-netwr,
        lv_mtart         TYPE mtart,
        lv_descripcion   TYPE zzdescripcion,
*        lv_prec       LIKE vbrk-netwr,
        lv_prec          TYPE p DECIMALS 3,
        lv_prec6         TYPE p DECIMALS 6,
        l_ukurs          TYPE ukurs_curr,
        lv_prec_unit     LIKE vbrk-netwr,
        lv_prec_unit_usd LIKE vbrk-netwr,
        lv_prec_usd      LIKE vbrk-netwr,
        l_dec            TYPE char4,
        lv_unit          LIKE vbap-netpr,
        lv_desc          LIKE konv-kwert,
        lv_desc_char     TYPE char18,
        lv_decimal       TYPE char18,
        lv_porcd         LIKE konv-kbetr,
        lv_conversion    LIKE konv-kbetr,
        lv_name          TYPE thead-tdname,
        lv_umrez         LIKE marm-umrez,
        lv_descitem      TYPE char1025,
        caja_lv          TYPE i,
        lv_totl          TYPE p DECIMALS 3.

  DATA: bseg_lt TYPE edoc_bseg_tab,
        bseg_ls TYPE bseg,
        bkpf_ls TYPE edoc_bkpf.

  DATA: ls_konv TYPE prcd_elements.
  DATA: lt_konv TYPE STANDARD TABLE OF prcd_elements.
  DATA: lt_cond TYPE STANDARD TABLE OF zclfel_tb_cond,
        ls_cond LIKE LINE OF lt_cond.

  DATA: l_impue  LIKE vbrk-netwr.
  DATA: lv_valor6dec        TYPE p DECIMALS 6.

  SELECT kschl tipo INTO CORRESPONDING FIELDS OF TABLE lt_cond FROM zclfel_tb_cond. "#EC CI_NOWHERE

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.

  SELECT SINGLE *
  INTO t001_ls
  FROM t001
  WHERE bukrs = header_ls-bukrs.


  bseg_lt[]   = data_ls-bseg[].
  bkpf_ls     = data_ls-bkpf.

  item_lt[]   =  data_ls-document_item[].
  REFRESH lt_konv[].
  SELECT * INTO TABLE lt_konv                 "#EC CI_ALL_FIELDS_NEEDED
    FROM prcd_elements
    WHERE knumv EQ header_ls-knumv.

  LOOP AT item_lt INTO item_ls WHERE fkimg IS NOT INITIAL.
    CLEAR: deta_ls.

*   Nro Linea de Detalle
    ADD 1 TO linea_lv.
    deta_ls-nrolindet = linea_lv.

    PERFORM formateo_nlin CHANGING deta_ls-nrolindet.

*   Indicador Exención
    IF dte_ev-encabezado-iddoc-tipodte EQ '033'.
      deta_ls-indexe = '0'.
    ELSEIF dte_ev-encabezado-iddoc-tipodte EQ '034'.
      deta_ls-indexe = '1'.
    ENDIF.


*   Nombre del Item
    CLEAR: lv_mtart, lv_descripcion.
    SELECT SINGLE mtart INTO lv_mtart
      FROM mara
      WHERE matnr EQ item_ls-matnr.

    IF sy-subrc EQ 0.

      SELECT SINGLE descripcion INTO lv_descripcion
        FROM zclfel_mtart
        WHERE mtart EQ lv_mtart.

    ENDIF.

    CLEAR lv_descitem.

    CASE lv_descripcion.
      WHEN '1'.
        CLEAR lv_name.
        CONCATENATE item_ls-vbeln item_ls-posnr INTO lv_name RESPECTING BLANKS.
        PERFORM get_text  USING    lv_name
              '0001'
              'VBBP'
        CHANGING lv_descitem.

        deta_ls-nmbitem = lv_descitem+0(80).

        deta_ls-dscitem = lv_descitem+81(919).
*
        IF lv_descitem IS INITIAL.
          deta_ls-nmbitem = item_ls-arktx.
        ENDIF.

      WHEN '2'.

        CLEAR lv_name.
        CONCATENATE item_ls-matnr header_ls-vkorg header_ls-vtweg INTO lv_name RESPECTING BLANKS.
        PERFORM get_text  USING    lv_name
              '0001'
              'MVKE'
        CHANGING lv_descitem.

        deta_ls-nmbitem = lv_descitem+0(80).
        deta_ls-dscitem = lv_descitem+81(919).
      WHEN OTHERS.
        deta_ls-nmbitem = item_ls-arktx.

    ENDCASE.


*   Cantidad del Item y Unidad de medida
    PERFORM formateo_cantidad USING     item_ls-fkimg
          item_ls-vrkme
    CHANGING  deta_ls-qtyitem
      deta_ls-unmditem.

*   Monto del Item
    CLEAR: lv_netwr.


    IF bkpf_ls-kursf IS NOT INITIAL.
      CLEAR bseg_ls.

*      READ TABLE bseg_lt INTO bseg_ls
*      WITH KEY xref1 = item_ls-posnr.

*      IF sy-subrc EQ 0.
      lv_netwr = item_ls-netwr.
*      ENDIF.
      PERFORM formateo_montos USING     lv_netwr
                                        bkpf_ls-waers
                              CHANGING  deta_ls-montoitem.

    ELSE.
      IF bkpf_ls-waers = bkpf_ls-hwaer.

        IF data_ls-document_header-fkart EQ 'ZFTU'. "Factura Turista HHERNANDEZ.
          lv_netwr = ( item_ls-kzwi1 + ( item_ls-kzwi4 * -1 ) ) / '1.19' .
        ELSE.
          lv_netwr = item_ls-netwr.
        ENDIF.

      ELSE.
        lv_netwr = ( item_ls-netwr * header_ls-kurrf ) / 100.
      ENDIF.

      PERFORM formateo_montos USING     lv_netwr
                                        header_ls-waerk
                              CHANGING  deta_ls-montoitem.


    ENDIF.

    IF gv_faexp IS NOT INITIAL.
      lv_netwr = item_ls-netwr.
      PERFORM formateo_montos USING     lv_netwr
                                        header_ls-waerk
                              CHANGING  deta_ls-montoitem.
    ENDIF.


*   Precio del Item
    CLEAR: lv_prec, lv_prec6.

    LOOP AT lt_cond INTO ls_cond WHERE tipo EQ 'PRE'.    "#EC CI_NESTED
      LOOP AT lt_konv INTO ls_konv WHERE kschl EQ ls_cond-kschl AND kposn EQ item_ls-posnr AND kinak EQ ''. "#EC CI_NESTED
        IF ls_konv-kpein > 1.
*            lv_prec = Item_ls-kzwi3 / ls_konv-kpein .
          lv_prec = item_ls-kzwi1.
        ELSE.
          lv_prec = item_ls-kzwi1.
        ENDIF.

        IF item_ls-fkimg NE 0 AND gv_faexp NE 'X'.
*          CLEAR lv_unit.
*          SELECT SINGLE netpr INTO lv_unit
*            FROM vbap
*            WHERE vbeln EQ item_ls-aubel
*            AND posnr EQ item_ls-aupos.
          lv_prec6 = lv_prec / item_ls-fkimg.
        ELSEIF item_ls-fkimg NE 0 AND gv_faexp EQ 'X'.
          lv_prec = item_ls-netwr / item_ls-fkimg.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
*
*    ENDIF.

    CLEAR: lv_totl.
    IF bkpf_ls-kursf IS NOT INITIAL.
      IF lv_prec6 IS NOT INITIAL.
        PERFORM formateo_montos USING   lv_prec6
                                        bkpf_ls-waers
                                         CHANGING  deta_ls-prcitem.
        deta_ls-prcitem = deta_ls-prcitem / 10000.
        CONDENSE deta_ls-prcitem.
      ELSE.
        PERFORM formateo_montos USING  lv_prec
                                       bkpf_ls-waers
                                       CHANGING  deta_ls-prcitem.
      ENDIF.
    ELSE.
      IF lv_prec6 IS NOT INITIAL.
        PERFORM formateo_montos USING     lv_prec6
                                       header_ls-waerk
                                       CHANGING  deta_ls-prcitem.
        deta_ls-prcitem = deta_ls-prcitem / 10000.
        CONDENSE deta_ls-prcitem.
      ELSE.
        PERFORM formateo_montos USING     lv_prec
                                      header_ls-waerk
                                      CHANGING  deta_ls-prcitem.
      ENDIF.

    ENDIF.

    IF gv_faexp IS NOT INITIAL.
      PERFORM formateo_montos USING     lv_prec
                                        header_ls-waerk
                              CHANGING  deta_ls-prcitem.
      deta_ls-prcitem = deta_ls-prcitem / 10.
      CONDENSE deta_ls-prcitem.
    ELSE.
*      deta_ls-prcitem = deta_ls-prcitem / 10.
      CONDENSE deta_ls-prcitem.
    ENDIF.

* Precio unitario con impuestos incluidos D63

    CLEAR: lv_umrez, lv_conversion.
    SELECT SINGLE umrez INTO lv_umrez
      FROM marm
      WHERE matnr EQ item_ls-matnr
      AND meinh EQ item_ls-vrkme.

    lv_conversion = lv_umrez.

    IF item_ls-fkimg EQ 0.

      IF  lv_conversion EQ 0.
        lv_totl = ( item_ls-netwr + item_ls-mwsbp ).
      ELSE.
        lv_totl = ( item_ls-netwr + item_ls-mwsbp ) / lv_conversion.
      ENDIF.

    ELSE.

      IF lv_conversion EQ 0.
        IF data_ls-document_header-fkart EQ 'ZFTU'. "Factura Turista HHERNANDEZ.
          lv_totl = ( item_ls-kzwi1 + ( item_ls-kzwi4 * -1 ) ) / '1.19' .
          lv_totl = lv_totl / item_ls-fkimg.
          lv_totl = lv_totl / lv_conversion.
        ELSE.
          lv_totl = ( ( item_ls-netwr + item_ls-mwsbp ) / item_ls-fkimg ).
        ENDIF.
      ELSE.
        IF data_ls-document_header-fkart EQ 'ZFTU'. "Factura Turista HHERNANDEZ.
          lv_totl = ( item_ls-kzwi1 + ( item_ls-kzwi4 * -1 ) ) / '1.19' .
          lv_totl = lv_totl / item_ls-fkimg.
          lv_totl = lv_totl / lv_conversion.
        ELSE.
          lv_totl = ( ( item_ls-netwr + item_ls-mwsbp ) / item_ls-fkimg ) / lv_conversion.
        ENDIF.

      ENDIF.

    ENDIF.


    PERFORM formateo_montos USING item_ls-netwr
          header_ls-waerk
    CHANGING  deta_ls-totalitem.
    CONDENSE deta_ls-totalitem.

    CLEAR: l_ukurs, lv_prec_unit_usd, lv_prec_usd.

    IF header_ls-fkart EQ 'ZIN1' AND header_ls-waerk NE 'CLP'.
      l_ukurs = bkpf_ls-kurs3.

      lv_prec_unit_usd = lv_prec * l_ukurs.
      lv_prec_usd      = deta_ls-totalitem * l_ukurs.

      deta_ls-prcitem = lv_prec_unit_usd.

      CLEAR l_dec.

      CONDENSE deta_ls-prcitem.

      deta_ls-montoitem = lv_prec_usd.
      CLEAR l_dec.
      SPLIT deta_ls-montoitem AT '.' INTO deta_ls-montoitem l_dec.
      IF l_dec >= 50.
        deta_ls-montoitem = deta_ls-montoitem + 1.
      ENDIF.
      CONDENSE deta_ls-montoitem.
      deta_ls-totalitem = deta_ls-montoitem.
      CONDENSE deta_ls-totalitem.

    ENDIF.

*  Descuentos

    CLEAR: lv_desc,
           lv_porcd.

    IF dte_ev-encabezado-iddoc-tipodte EQ '039' OR dte_ev-encabezado-iddoc-tipodte EQ '040' OR dte_ev-encabezado-iddoc-tipodte EQ '042'.
      LOOP AT lt_konv INTO ls_konv WHERE kschl EQ 'ZDE3' AND kposn EQ item_ls-posnr AND kinak EQ ''. "#EC CI_NESTED
        ADD ls_konv-kwert TO lv_desc.
        ADD ls_konv-kbetr TO lv_porcd.
      ENDLOOP.
    ELSE.
      LOOP AT lt_cond INTO ls_cond WHERE tipo EQ 'DES'.  "#EC CI_NESTED
        LOOP AT lt_konv INTO ls_konv WHERE kschl EQ ls_cond-kschl AND kposn EQ item_ls-posnr AND kinak EQ ''. "#EC CI_NESTED
          ADD ls_konv-kwert TO lv_desc.
          ADD ls_konv-kbetr TO lv_porcd.

        ENDLOOP.
      ENDLOOP.
    ENDIF.
    IF lv_desc IS NOT INITIAL.
      CLEAR lv_desc_char.
      PERFORM formateo_montos USING lv_desc
            header_ls-waerk
      CHANGING  deta_ls-descuentomonto.

      IF ( dte_ev-encabezado-iddoc-tipodte EQ '039' OR dte_ev-encabezado-iddoc-tipodte EQ '040' OR dte_ev-encabezado-iddoc-tipodte EQ '042' )   AND deta_ls-qtyitem NE '0'.
        lv_desc_char =  deta_ls-descuentomonto / deta_ls-qtyitem.
        CONDENSE lv_desc_char.

        SPLIT  lv_desc_char AT '.' INTO deta_ls-descuentomonto lv_decimal.
      ENDIF.

    ENDIF.
    IF lv_porcd IS NOT INITIAL.
      PERFORM formateo_tasa USING lv_porcd
      CHANGING deta_ls-descuentopct.
      deta_ls-descuentopct = deta_ls-descuentopct * 10.
      CONDENSE deta_ls-descuentopct.
    ENDIF.

    IF deta_ls-descuentomonto IS NOT INITIAL AND ( dte_ev-encabezado-iddoc-tipodte EQ '039' OR dte_ev-encabezado-iddoc-tipodte EQ '040' OR dte_ev-encabezado-iddoc-tipodte EQ '042' ).
      CONCATENATE 'DESCUENTO' '-' deta_ls-descuentomonto INTO deta_ls-dscitem SEPARATED BY space.

    ENDIF.

    IF dte_ev-encabezado-iddoc-tipodte EQ '039' OR dte_ev-encabezado-iddoc-tipodte EQ '040' OR dte_ev-encabezado-iddoc-tipodte EQ '042'.
      deta_ls-montoitem = ( deta_ls-prcitem - deta_ls-descuentomonto ) * deta_ls-qtyitem.
      CONDENSE deta_ls-montoitem.
    ENDIF.

*   Codigos
    CLEAR: cod_ls, cod_lt.
    cod_ls-tpocodigo = 'INT1'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = item_ls-matnr
      IMPORTING
        output = cod_ls-vlrcodigo.

    APPEND cod_ls TO cod_lt.

    deta_ls-cdgitem[] = cod_lt[].

    APPEND deta_ls TO deta_lt.

  ENDLOOP.

  dte_ev-detalle[] = deta_lt[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FORMATEO_CANTIDAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ITEM_LS_FKIMG
*&      --> ITEM_LS_VRKME
*&      <-- DETA_LS_QTYITEM
*&      <-- DETA_LS_UNMDITEM
*&---------------------------------------------------------------------*
FORM formateo_cantidad  USING    p_fkimg
      p_vrkme
CHANGING p_canti
  p_unit.

  DATA: canti_lv(18) TYPE c,
        unit_lv(4)   TYPE c.

  WRITE p_vrkme TO unit_lv.

  p_unit = unit_lv.

  WRITE p_fkimg TO canti_lv UNIT unit_lv
  NO-SIGN NO-GROUPING
  LEFT-JUSTIFIED.

  TRANSLATE canti_lv USING ',.'.

  p_canti = canti_lv.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FORMATEO_NLIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- DETA_LS_NROLINDET
*&---------------------------------------------------------------------*
FORM formateo_nlin  CHANGING p_nlin.
  DATA: lin_lv(4) TYPE c.

  lin_lv = p_nlin.

  CONDENSE lin_lv NO-GAPS.

  p_nlin = lin_lv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFERENCIAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&      <-- W_REF_DOC
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM referencias  USING    data_ls   TYPE  edoc_src_data_sd_invoice
CHANGING dte_ev    TYPE  zclfel_et_documento
  w_ref_doc STRUCTURE w_ref_doc
  w_sol_nc.

  DATA: header_ls TYPE edoc_vbrkvb.

  CLEAR: header_ls.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.


  IF header_ls-vbtyp CA 'OPM'.
    PERFORM buscar_referencia USING     header_ls
                                        dte_ev-encabezado-iddoc-tipodte

    CHANGING  w_ref_doc
              w_sol_nc.


    PERFORM fill_referencia   USING     header_ls
    CHANGING  w_ref_doc
              dte_ev
              w_sol_nc.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUSCAR_REFERENCIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> HEADER_LS
*&      --> DTE_EV_ENCABEZADO_IDDOC_TIPODT
*&      <-- W_REF_DOC
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM buscar_referencia  USING     header_ls TYPE edoc_vbrkvb
                                  p_tipodte

CHANGING  w_ref_doc LIKE w_ref_doc
          w_sol_nc.
*  TABLEs t_likp.

  DATA: BEGIN OF t_vbfa OCCURS 0,
          vbeln     LIKE vbfa-vbeln,
          vbtyp_n   LIKE vbfa-vbtyp_n,
          vbelv     LIKE vbfa-vbelv,
          vbtyp_v   LIKE vbfa-vbtyp_v,
          fkdat     LIKE vbrk-fkdat,
          fksto     LIKE vbrk-fksto,
          fecha(10) TYPE c,
        END OF t_vbfa.

  DATA: BEGIN OF t_vbfa1 OCCURS 0,
          vbeln   LIKE vbfa-vbeln,
          vbtyp_n LIKE vbfa-vbtyp_n,
          vbelv   LIKE vbfa-vbelv,
          vbtyp_v LIKE vbfa-vbtyp_v,
          fkdat   LIKE vbrk-fkdat,
          fksto   LIKE vbrk-fksto,
        END OF t_vbfa1.

  DATA: wl_vauf       LIKE vbak-vbeln.
  DATA: lv_pedido     TYPE vbak-vbeln.
  DATA: l_vbtypv TYPE vbtyp_v.
  REFRESH: t_vbfa.
  CLEAR: t_vbfa, w_ref_doc, wl_vauf, l_vbtypv.

  IF p_tipodte EQ '56' OR p_tipodte EQ '056' OR p_tipodte EQ '111'.
    l_vbtypv = 'L'.
  ELSEIF p_tipodte EQ '033'.
    l_vbtypv = 'J'.
  ELSEIF p_tipodte = '61' OR p_tipodte EQ '061' OR p_tipodte EQ '112'.
    l_vbtypv = 'K'.
  ELSEIF p_tipodte = '52' OR p_tipodte EQ '052'.
    l_vbtypv = 'C'.
  ENDIF.

* Flujo de documentos
  IF l_vbtypv NE 'J'.
    SELECT SINGLE aubel INTO lv_pedido
      FROM  vbrp
      WHERE vbeln EQ header_ls-vbeln.

    w_sol_nc = lv_pedido.

    SELECT vbeln vbtyp_n vbelv vbtyp_v                  "#EC CI_NOFIELD
    INTO TABLE t_vbfa
    FROM vbfa
    WHERE vbeln     = lv_pedido.


    SORT t_vbfa BY vbeln vbtyp_n.
    DELETE ADJACENT DUPLICATES FROM t_vbfa
    COMPARING vbelv vbtyp_v.
    READ TABLE t_vbfa WITH KEY vbtyp_v = 'M'.
    IF sy-subrc NE 0.
      READ TABLE t_vbfa WITH KEY vbtyp_v = l_vbtypv."'K'.
      IF sy-subrc EQ 0.
        w_sol_nc = t_vbfa-vbelv.
        IF w_sol_nc IS INITIAL.
          w_sol_nc = lv_pedido.
        ENDIF.
        REFRESH t_vbfa.
        CLEAR t_vbfa.
        SELECT vbeln vbtyp_n vbelv vbtyp_v              "#EC CI_NOFIELD
        INTO TABLE t_vbfa
        FROM vbfa
        WHERE vbeln     = w_sol_nc AND
              vbtyp_v = 'M'.

        DELETE ADJACENT DUPLICATES FROM t_vbfa COMPARING vbelv vbtyp_v. "#EC CI_SORTED

      ENDIF.
    ENDIF.
  ELSE.  " Se referencia Guia de despacho
*--------------------------------------------------------------------*

    SELECT vbeln vbtyp_n vbelv vbtyp_v                  "#EC CI_NOFIELD
    INTO TABLE t_vbfa
    FROM vbfa
    WHERE vbeln     = header_ls-vbeln
    AND vbtyp_v EQ l_vbtypv.

    SORT t_vbfa BY vbelv.
    DELETE ADJACENT DUPLICATES FROM t_vbfa
              COMPARING vbelv vbtyp_v.


    IF t_vbfa[] IS NOT INITIAL.
      SELECT vbeln
           xblnr
           wadat_ist
    INTO TABLE t_likp
    FROM likp FOR ALL ENTRIES IN t_vbfa
    WHERE vbeln EQ t_vbfa-vbelv.
    ENDIF.

*--------------------------------------------------------------------*

  ENDIF.

* Buscar fecha de factura
  LOOP AT t_vbfa.
*   Dejar solo facturas, NC y ND
    IF t_vbfa-vbtyp_v CA 'MNOPL'.
      SELECT SINGLE fkdat fksto
      INTO (t_vbfa-fkdat, t_vbfa-fksto)
      FROM vbrk
      WHERE vbeln = t_vbfa-vbelv.

      IF sy-subrc = 0.
        MODIFY t_vbfa TRANSPORTING fkdat fksto.
      ENDIF.
    ELSE.
      DELETE t_vbfa.
    ENDIF.
  ENDLOOP.

* Dejar arriba la más reciente no anulada
  SORT t_vbfa BY fkdat DESCENDING.

  CASE header_ls-vbtyp.
    WHEN 'O'.
*     Nota de crédito
      LOOP AT t_vbfa. "where vbeln  = vbdkr-vbeln and
        SELECT SINGLE vbeln xblnr fkdat vkorg fkart
        INTO CORRESPONDING FIELDS OF w_ref_doc
        FROM vbrk
        WHERE vbeln = t_vbfa-vbelv
        AND   fksto <> 'X'.

        w_ref_doc-tidoc = w_ref_doc-xblnr+0(3).
        w_ref_doc-xblnr = w_ref_doc-xblnr+4.
        SHIFT w_ref_doc-xblnr LEFT DELETING LEADING '0'.
      ENDLOOP.

    WHEN 'P'.
*     Nota de débito
      LOOP AT t_vbfa WHERE vbelv <> header_ls-vbeln AND
      ( vbtyp_v = 'M' OR vbtyp_v = 'O' ).
        IF t_vbfa-fksto IS INITIAL.
          SELECT SINGLE vbeln xblnr fkdat vkorg fkart
          INTO CORRESPONDING FIELDS OF w_ref_doc
          FROM vbrk
          WHERE vbeln = t_vbfa-vbelv.

          w_ref_doc-tidoc = w_ref_doc-xblnr+0(3).
          w_ref_doc-xblnr = w_ref_doc-xblnr+4.
          SHIFT w_ref_doc-xblnr LEFT DELETING LEADING '0'.
        ENDIF.
      ENDLOOP.

    WHEN 'M'.
*     Factura
      PERFORM get_text  USING     header_ls-vbeln
            'Z004'
            'VBBK'
      CHANGING  w_ref_doc-tidoc.

      PERFORM get_text  USING     header_ls-vbeln
            'Z006'
            'VBBK'
      CHANGING  w_ref_doc-xblnr.

      PERFORM get_text  USING     header_ls-vbeln
            'Z005'
            'VBBK'
      CHANGING  w_ref_doc-fecha.

  ENDCASE.

  IF w_ref_doc-xblnr IS INITIAL.
    CLEAR w_ref_doc.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_REFERENCIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> HEADER_LS
*&      <-- W_REF_DOC
*&      <-- DTE_EV
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM fill_referencia  USING     header_ls TYPE edoc_vbrkvb
CHANGING  w_ref_doc LIKE w_ref_doc
          dte_ev    TYPE  zclfel_et_documento
          w_sol_nc.

  DATA: refer_lt  TYPE zclfel_t_referencia,
        refer_ls  TYPE zclfel_referencia,
        refer_ls2 TYPE zclfel_referencia,
        motivo_ls TYPE zclfel_tb_ref.

  DATA: w_vkorg    LIKE vbak-vkorg,
        w_augru    LIKE vbak-augru,
        w_auart    LIKE vbak-auart,
        lv_bstkd   TYPE vbkd-bstkd,
        lv_bstdk   TYPE vbkd-bstdk,
        lv_pedido  TYPE vbak-vbeln,
        lv_oc      TYPE vbkd-bstkd,
        lv_fechaoc TYPE vbkd-bstdk.

  DATA: linea_lv  TYPE i.

  CLEAR: refer_lt[], lv_bstkd, lv_bstdk, lv_oc, lv_pedido, lv_fechaoc.

  DATA: comwa_ls LIKE vbco3,
        kopf_ls  LIKE vbdkr,
        pos_lt   LIKE TABLE OF vbdpr.

*--------------------------------------------------------------------*
* Tablas Internas
*--------------------------------------------------------------------*
  DATA: lt_lines  TYPE STANDARD TABLE OF tline.

*--------------------------------------------------------------------*
* Areas de trabajo
*--------------------------------------------------------------------*
  DATA: ls_lines  TYPE tline.

*--------------------------------------------------------------------*
* Variables
*--------------------------------------------------------------------*
  DATA: lv_id     LIKE  thead-tdid.
  DATA: lv_name   LIKE  thead-tdname.
  DATA: lv_object LIKE  thead-tdobject.

  REFRESH: refer_lt.

  comwa_ls-spras = sy-langu.
  comwa_ls-vbeln = header_ls-vbeln.

  CALL FUNCTION 'RV_BILLING_PRINT_VIEW'
    EXPORTING
      comwa                        = comwa_ls
    IMPORTING
      kopf                         = kopf_ls
    TABLES
      pos                          = pos_lt
    EXCEPTIONS
      terms_of_payment_not_in_t052 = 1
      OTHERS                       = 2.

  IF header_ls-vbtyp CA 'OP'.
*   --------------------------
*   Notas de crédito / débito
*   --------------------------
    IF NOT w_ref_doc-vbeln IS INITIAL.
      IF w_ref_doc-tidoc IS NOT INITIAL.
        ADD 1 TO linea_lv.
        refer_ls-nrolinref = linea_lv.
*       Código del documento de referencia
        refer_ls-tpodocref = w_ref_doc-tidoc.
*       Indicador global de referencia
        refer_ls-indglobal  = ''.
*       Folio del documento de referencia
        refer_ls-folioref  = w_ref_doc-xblnr.
*       Fecha de emisión del documento de referencia
        CONCATENATE w_ref_doc-fkdat+0(4)
        w_ref_doc-fkdat+4(2)
        w_ref_doc-fkdat+6(2)
        INTO refer_ls-fchref
        SEPARATED BY '-'.

*-->Determinar motivo de pedido
        IF kopf_ls-vbeln_vauf IS INITIAL.
          kopf_ls-vbeln_vauf = w_sol_nc .
        ENDIF.

        SELECT SINGLE vkorg augru auart INTO (w_vkorg, w_augru, w_auart)
          FROM vbak WHERE vbeln = kopf_ls-vbeln_vauf.

        SELECT SINGLE * INTO motivo_ls FROM zclfel_tb_ref WHERE augru  = w_augru.
        refer_ls-codref    = motivo_ls-codref.
        refer_ls-razonref  = motivo_ls-razonref.

        APPEND refer_ls TO refer_lt.

      ENDIF.
    ELSEIF header_ls-fkart EQ 'ZANC'.
*--> JEPL 15.03.2019

*-->Determinar motivo de pedido
      IF kopf_ls-vbeln_vauf IS INITIAL.
        kopf_ls-vbeln_vauf = w_sol_nc .
      ENDIF.

      SELECT SINGLE vkorg augru auart INTO (w_vkorg, w_augru, w_auart)
        FROM vbak WHERE vbeln = kopf_ls-vbeln_vauf.

      SELECT SINGLE * INTO motivo_ls FROM zclfel_tb_ref WHERE augru  = w_augru.

*--> Factura de referencia
      REFRESH: lt_lines.
      lv_id     = 'Z001'.
      lv_name   = header_ls-vbeln.
      lv_object = 'VBBK'.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = lv_id
          language                = sy-langu
          name                    = lv_name
          object                  = lv_object
        TABLES
          lines                   = lt_lines[]
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      LOOP AT lt_lines INTO ls_lines.
        ADD 1 TO linea_lv.

        refer_ls-nrolinref = linea_lv.
        SPLIT ls_lines-tdline AT '-' INTO refer_ls-tpodocref refer_ls-folioref refer_ls-fchref.
        CONCATENATE   refer_ls-fchref+0(4) refer_ls-fchref+4(2) refer_ls-fchref+6(2) INTO refer_ls-fchref SEPARATED BY '-'.

        refer_ls-codref    = motivo_ls-codref.
        refer_ls-razonref  = motivo_ls-razonref.

        IF refer_ls-tpodocref IS NOT INITIAL.
          APPEND refer_ls TO refer_lt.
        ENDIF.
        CLEAR: refer_ls.

      ENDLOOP.

*--> Orden de Compra de Referencia
      REFRESH: lt_lines.
      lv_id     = 'Z002'.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = lv_id
          language                = sy-langu
          name                    = lv_name
          object                  = lv_object
        TABLES
          lines                   = lt_lines[]
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT lt_lines INTO ls_lines.
        ADD 1 TO linea_lv.

        refer_ls-nrolinref = linea_lv.
        SPLIT ls_lines-tdline AT '-' INTO refer_ls-tpodocref refer_ls-folioref refer_ls-fchref.
        CONCATENATE   refer_ls-fchref+0(4) refer_ls-fchref+4(2) refer_ls-fchref+6(2) INTO refer_ls-fchref SEPARATED BY '-'.

        refer_ls-codref    = motivo_ls-codref.
        refer_ls-razonref  = motivo_ls-razonref.

        IF refer_ls-tpodocref IS NOT INITIAL.
          APPEND refer_ls TO refer_lt.
        ENDIF.
        CLEAR: refer_ls.

      ENDLOOP.

      CLEAR: kopf_ls-vbeln_vauf,w_vkorg, w_augru, w_auart,motivo_ls.

    ELSE.
*     Si no tiene referencia se debe poner el indicador global de
*     referencia en '1' y los demás campos como se indican. En teoría
*     el indicador global sirve para informar que se hace referencia
*     a un número grande de documentos. Pero también se puede utilizar
*     para el caso que no podamos determinar referencias

      SELECT SINGLE bstkd bstdk INTO ( lv_bstkd, lv_bstdk )
        FROM vbkd
        WHERE vbeln EQ w_sol_nc.

      IF sy-subrc EQ 0 AND lv_bstkd IS NOT INITIAL. "JEPL 24.02.2019
        SPLIT lv_bstkd AT '-' INTO refer_ls-tpodocref refer_ls-folioref.

        ADD 1 TO linea_lv.
        refer_ls-nrolinref = linea_lv.
*      refer_ls-tpodocref = '33'.
        refer_ls-indglobal = ''.
*      refer_ls-indglobal = '1'.
*      refer_ls-folioref = '0'.
        CONCATENATE lv_bstdk+0(4) '-'
        lv_bstdk+4(2) '-'
        lv_bstdk+6(2)
        INTO refer_ls-fchref.

*   Determinar motivo de pedido

        CLEAR: w_vkorg, w_augru, w_auart.

        IF kopf_ls-vbeln_vauf IS INITIAL.
          kopf_ls-vbeln_vauf = w_sol_nc .
        ENDIF.

        SELECT SINGLE vkorg augru auart
        INTO (w_vkorg, w_augru, w_auart)
        FROM vbak
        WHERE vbeln = kopf_ls-vbeln_vauf.

        IF header_ls-fkart = 'ZCD1' OR header_ls-fkart = 'ZAA2'.
          refer_ls-codref    = '2'.
          refer_ls-razonref  = 'Corrige Texto'.
        ELSE.
          SELECT SINGLE * INTO motivo_ls FROM zclfel_tb_ref WHERE augru  = w_augru.
          refer_ls-codref    = motivo_ls-codref.
          refer_ls-razonref  = motivo_ls-razonref.
        ENDIF.

        IF refer_ls-codref IS NOT INITIAL.
          APPEND refer_ls TO refer_lt.
        ENDIF.
      ENDIF.
    ENDIF.

*    "Agrego orden de compra de la factura original
*    SELECT SINGLE aubel INTO lv_pedido
*          FROM vbrp
*          WHERE vbeln EQ w_ref_doc-vbeln.
*
*    SELECT SINGLE bstkd bstdk INTO ( lv_oc, lv_fechaoc )
*      FROM vbkd
*      WHERE vbeln EQ lv_pedido.
*
*    IF lv_oc IS NOT INITIAL.
*      ADD 1 TO linea_lv.
*      refer_ls-nrolinref  = linea_lv.
*      refer_ls-tpodocref  = '801'.
*      refer_ls-folioref   = lv_oc.
*      refer_ls-codref     = motivo_ls-codref.
*      refer_ls-razonref   = motivo_ls-razonref.
*      PERFORM formateo_fecha  USING   lv_fechaoc
*       CHANGING  refer_ls-fchref.
*      APPEND refer_ls TO refer_lt.
*    ENDIF.

  ELSEIF header_ls-vbtyp = 'M'.
    CLEAR: refer_ls.
*   Facturas
*   Aquí se pueden indicar otras referencias como ser ordenes
*   de  compra, Guías de Despacho, etc.
*--------------------------------------------------------------------*

    IF header_ls-fkart = 'ZFAP' OR header_ls-fkart = 'ZFAC' OR header_ls-fkart = 'ZFA2' OR header_ls-fkart = 'ZSA2' OR header_ls-fkart = 'ZSE2' OR header_ls-fkart = 'ZSM2' OR header_ls-fkart = 'ZFET'.
      IF NOT t_likp IS INITIAL.
        LOOP AT t_likp INTO ls_likp.
          IF ls_likp-xblnr IS NOT INITIAL.
            ADD 1 TO linea_lv.
            refer_ls-nrolinref  = linea_lv.
            refer_ls-tpodocref  = ls_likp-xblnr(3)."???.
            REPLACE ALL OCCURRENCES OF '0' IN refer_ls-tpodocref WITH space.  "
            refer_ls-folioref    = ls_likp-xblnr+4.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = refer_ls-folioref
              IMPORTING
                output = refer_ls-folioref.

            PERFORM formateo_fecha  USING   ls_likp-wadat_ist
             CHANGING  refer_ls-fchref.

            APPEND refer_ls TO refer_lt.
          ENDIF.
        ENDLOOP.
      ENDIF.

******* Orden de compra *****
      ADD 1 TO linea_lv.
      refer_ls-nrolinref  = linea_lv.
      refer_ls-tpodocref  = '801'.
      refer_ls-folioref   = kopf_ls-bstnk.
      PERFORM formateo_fecha  USING   kopf_ls-bstdk
       CHANGING  refer_ls-fchref.
      APPEND refer_ls TO refer_lt.

    ELSEIF header_ls-fkart = 'ZREC'.
*--> 18-03-2019 JEPL
*-->Determinar motivo de pedido
      CLEAR: w_vkorg, w_augru, w_auart,motivo_ls.
      IF kopf_ls-vbeln_vauf IS INITIAL.
        kopf_ls-vbeln_vauf = w_sol_nc .
      ENDIF.

      SELECT SINGLE vkorg augru auart INTO (w_vkorg, w_augru, w_auart)
        FROM vbak WHERE vbeln = kopf_ls-vbeln_vauf.

      SELECT SINGLE augru codref razonref INTO CORRESPONDING FIELDS OF motivo_ls FROM zclfel_tb_ref WHERE augru  = w_augru.

*--> Orden de Compra de Referencia
      REFRESH: lt_lines.
      lv_id     = 'Z002'.
      lv_name   = header_ls-vbeln.
      lv_object = 'VBBK'.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = lv_id
          language                = sy-langu
          name                    = lv_name
          object                  = lv_object
        TABLES
          lines                   = lt_lines[]
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT lt_lines INTO ls_lines.
        ADD 1 TO linea_lv.

        refer_ls-nrolinref = linea_lv.
        SPLIT ls_lines-tdline AT '-' INTO refer_ls-tpodocref refer_ls-folioref refer_ls-fchref.
        CONCATENATE   refer_ls-fchref+0(4) refer_ls-fchref+4(2) refer_ls-fchref+6(2) INTO refer_ls-fchref SEPARATED BY '-'.

*        refer_ls-codref    = motivo_ls-codref.
*        refer_ls-razonref  = motivo_ls-razonref.

        IF refer_ls-tpodocref IS NOT INITIAL.
          APPEND refer_ls TO refer_lt.
        ENDIF.
        CLEAR: refer_ls.

      ENDLOOP.

    ELSE.
*--------------------------------------------------------------------*
      IF NOT w_ref_doc-xblnr IS INITIAL AND w_ref_doc-tidoc NE '801'.
        ADD 1 TO linea_lv.
        refer_ls-nrolinref  = linea_lv.
        refer_ls-tpodocref  = w_ref_doc-tidoc.
        refer_ls-folioref    = w_ref_doc-xblnr.
        refer_ls-fchref = w_ref_doc-fecha.
        TRANSLATE refer_ls-fchref USING '.-'.
        APPEND refer_ls TO refer_lt.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_faexp EQ 'X' AND dte_ev-encabezado-iddoc-tipodte EQ '110'.


    REFRESH refer_lt[].
    CLEAR linea_lv.

    " Nota de pedido

    ADD 1 TO linea_lv.
    refer_ls-nrolinref  = linea_lv.
    refer_ls-tpodocref  = '802'.
    refer_ls-folioref    = kopf_ls-vbeln_vauf.

    PERFORM formateo_fecha  USING   kopf_ls-audat_vauf
       CHANGING  refer_ls-fchref.

    TRANSLATE refer_ls-fchref USING '.-'.

    APPEND refer_ls TO refer_lt.

    "DUS

    CLEAR refer_ls.
    ADD 1 TO linea_lv.
    refer_ls-nrolinref  = linea_lv.
    refer_ls-tpodocref  = '807'.

    "******* numero de DUS **
    REFRESH: lt_lines.
    lv_id     = 'Z011'.
    lv_name   = header_ls-vbeln.
    lv_object = 'VBBK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = 'E'
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.
      CONCATENATE  refer_ls-folioref ls_lines-tdline INTO refer_ls-folioref.
    ENDLOOP.
    """"" Fecha DUS """

    REFRESH: lt_lines.
    lv_id     = 'Z017'.
    lv_name   = header_ls-vbeln.
    lv_object = 'VBBK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = 'E'
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.

      refer_ls-fchref = ls_lines-tdline.
      CONCATENATE   refer_ls-fchref+6(4) refer_ls-fchref+3(2) refer_ls-fchref+0(2) INTO refer_ls-fchref SEPARATED BY '-'.

      APPEND refer_ls TO refer_lt.
      CLEAR: refer_ls.

    ENDLOOP.

    "AWB (Se obtiene del documento de transporte asociado al flujo)

    IF gv_faexp = 'X'.
      CLEAR ls_vttk.
      READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT02'.

      IF sy-subrc NE 0.
        CLEAR ls_vttk.
        READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT03'.

        IF sy-subrc NE 0.
          CLEAR ls_vttk.
          READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT04'.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR ls_vttk.
      READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = 'ZT01'.
    ENDIF.


    CLEAR refer_ls.
    ADD 1 TO linea_lv.
    refer_ls-nrolinref  = linea_lv.
    refer_ls-tpodocref  = '809'.

    "******* numero de AWB ****

    REFRESH: lt_lines.
    lv_id     = 'Z019'.
    lv_name   = ls_vttk-tknum.
    lv_object = 'VTTK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.
      CONCATENATE  refer_ls-folioref ls_lines-tdline INTO refer_ls-folioref.
    ENDLOOP.

    """"" Fecha AWB """

    REFRESH: lt_lines.
    lv_id     = 'Z021'.
    lv_name   = ls_vttk-tknum.
    lv_object = 'VTTK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.

      refer_ls-fchref = ls_lines-tdline.
      CONCATENATE   refer_ls-fchref+6(4) refer_ls-fchref+3(2) refer_ls-fchref+0(2) INTO refer_ls-fchref SEPARATED BY '-'.

      APPEND refer_ls TO refer_lt.
      CLEAR: refer_ls.

    ENDLOOP.

    "CRT (Carta de porte)
    CLEAR refer_ls.
    ADD 1 TO linea_lv.
    refer_ls-nrolinref  = linea_lv.
    refer_ls-tpodocref  = '811'.

    "******* numero de CRT **
    REFRESH: lt_lines.
    lv_id     = 'Z005'.
    lv_name   = ls_vttk-tknum.
    lv_object = 'VTTK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.
      CONCATENATE  refer_ls-folioref ls_lines-tdline INTO refer_ls-folioref.
    ENDLOOP.

    """"" Fecha CRT """

    REFRESH: lt_lines.
    lv_id     = 'Z022'.
    lv_name   = ls_vttk-tknum.
    lv_object = 'VTTK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.
      refer_ls-fchref = ls_lines-tdline.
      CONCATENATE   refer_ls-fchref+6(4) refer_ls-fchref+3(2) refer_ls-fchref+0(2) INTO refer_ls-fchref SEPARATED BY '-'.

      APPEND refer_ls TO refer_lt.
      CLEAR: refer_ls.
    ENDLOOP.

    "B/L (Conocimiento de embarque)
    CLEAR refer_ls.
    ADD 1 TO linea_lv.
    refer_ls-nrolinref  = linea_lv.
    refer_ls-tpodocref  = '808'.

    "******* numero de B/L **
    REFRESH: lt_lines.
    lv_id     = 'Z023'.
    lv_name   = ls_vttk-tknum.
    lv_object = 'VTTK'.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = lv_id
        language                = sy-langu
        name                    = lv_name
        object                  = lv_object
      TABLES
        lines                   = lt_lines[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT lt_lines INTO ls_lines.
      CONCATENATE  refer_ls-folioref ls_lines-tdline INTO refer_ls-folioref.
    ENDLOOP.

    """"" Fecha B/L """

    IF ls_vttk-datbg IS NOT INITIAL AND refer_ls-folioref IS NOT INITIAL.
      PERFORM formateo_fecha  USING   ls_vttk-datbg
         CHANGING  refer_ls-fchref.

      TRANSLATE refer_ls-fchref USING '.-'.

      APPEND refer_ls TO refer_lt.

    ENDIF.
    CLEAR: refer_ls.

*    REFRESH: lt_lines.
*    lv_id     = 'Z022'.
*    lv_name   = ls_vttk-tknum.
*    lv_object = 'VTTK'.
*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        client                  = sy-mandt
*        id                      = lv_id
*        language                = sy-langu
*        name                    = lv_name
*        object                  = lv_object
*      TABLES
*        lines                   = lt_lines[]
*      EXCEPTIONS
*        id                      = 1
*        language                = 2
*        name                    = 3
*        not_found               = 4
*        object                  = 5
*        reference_check         = 6
*        wrong_access_to_archive = 7
*        OTHERS                  = 8.
*    LOOP AT lt_lines INTO ls_lines.
*      refer_ls-fchref = ls_lines-tdline.
*      CONCATENATE   refer_ls-fchref+6(4) refer_ls-fchref+3(2) refer_ls-fchref+0(2) INTO refer_ls-fchref SEPARATED BY '-'.
*
*      APPEND refer_ls TO refer_lt.
*      CLEAR: refer_ls.
*    ENDLOOP.

  ENDIF.

************************ REGULARIZACION DE DOCUMENTOS ATRASADOS (CONTINGENCIA) solo para clase de factura exportacion tributaria especial ***************

  IF header_ls-fkart = 'ZEET'.

    DATA: id       LIKE  thead-tdid,
          name     LIKE  thead-tdname,
          object   LIKE  thead-tdobject,
          languaje TYPE sy-langu.

    DATA: lines_lt LIKE TABLE OF tline,
          lines_ls LIKE          tline.

    DATA: l_titulo TYPE char40.
    DATA: l_valor  TYPE char100.
    DATA: l_spras  TYPE spras.

    CLEAR l_spras.
    SELECT SINGLE spras INTO l_spras
      FROM kna1
      WHERE kunnr EQ header_ls-kunrg.

    id       = 'Z027'.
    name     = header_ls-vbeln.
    object   = 'VBBK'.
    languaje = l_spras.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = id
        language                = languaje
        name                    = name
        object                  = object
      TABLES
        lines                   = lines_lt
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF lines_lt[] IS NOT INITIAL.

****** ordeno la tabla de referencias para saber el ultimo numero de posicion usado *****

      SORT refer_lt BY nrolinref DESCENDING.

      CLEAR refer_ls2.
      READ TABLE refer_lt INTO refer_ls2 INDEX 1.
      IF sy-subrc EQ 0.
        CLEAR linea_lv.
        linea_lv = refer_ls2-nrolinref.
      ENDIF.

      SORT refer_lt BY nrolinref ASCENDING.

******* Numero y fecha de DUS ************
***** numero *****
      CLEAR: lines_ls, l_titulo, l_valor, refer_ls.
      READ TABLE lines_lt INTO lines_ls INDEX 10.
*
*
*      SPLIT lines_ls-tdline AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.
      IF lines_ls-tdline IS NOT INITIAL.


        ADD 1 TO linea_lv.
        refer_ls-nrolinref  = linea_lv.
        refer_ls-tpodocref  = '807'.
        refer_ls-folioref   = lines_ls-tdline.

***** fecha dus ****
        CLEAR: lines_ls, l_titulo, l_valor.
        READ TABLE lines_lt INTO lines_ls INDEX 11.
*        SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*        CONDENSE: l_titulo, l_valor.

        refer_ls-fchref = lines_ls-tdline.

        APPEND refer_ls TO refer_lt.
      ENDIF.

*************** Numero y fecha de AWB ***********

***** numero *****

      CLEAR: lines_ls, l_titulo, l_valor, refer_ls.
      READ TABLE lines_lt INTO lines_ls INDEX 26.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      IF lines_ls-tdline IS NOT INITIAL.


        ADD 1 TO linea_lv.
        refer_ls-nrolinref  = linea_lv.
        refer_ls-tpodocref  = '809'.
        refer_ls-folioref   = lines_ls-tdline.

***** fecha awb ****
        CLEAR: lines_ls, l_titulo, l_valor.
        READ TABLE lines_lt INTO lines_ls INDEX 27.
*        SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*        CONDENSE: l_titulo, l_valor.

        refer_ls-fchref = lines_ls-tdline.

        APPEND refer_ls TO refer_lt.

      ENDIF.
************** Numero y fecha de CRT ***************


***** numero *****
      CLEAR: lines_ls, l_titulo, l_valor, refer_ls.
      READ TABLE lines_lt INTO lines_ls INDEX 15.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      IF lines_ls-tdline IS NOT INITIAL.


        ADD 1 TO linea_lv.
        refer_ls-nrolinref  = linea_lv.
        refer_ls-tpodocref  = '811'.
        refer_ls-folioref   = lines_ls-tdline.

***** fecha crt ****
        CLEAR: lines_ls, l_titulo, l_valor.
        READ TABLE lines_lt INTO lines_ls INDEX 16.
*        SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*        CONDENSE: l_titulo, l_valor.

        refer_ls-fchref = lines_ls-tdline.

        APPEND refer_ls TO refer_lt.
      ENDIF.

************** Numero y fecha de BL***************


***** numero *****
      CLEAR: lines_ls, l_titulo, l_valor, refer_ls.
      READ TABLE lines_lt INTO lines_ls INDEX 24.
*      SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*      CONDENSE: l_titulo, l_valor.

      IF lines_ls-tdline IS NOT INITIAL.


        ADD 1 TO linea_lv.
        refer_ls-nrolinref  = linea_lv.
        refer_ls-tpodocref  = '808'.
        refer_ls-folioref   = lines_ls-tdline.

***** fecha BL ****
        CLEAR: lines_ls, l_titulo, l_valor.
        READ TABLE lines_lt INTO lines_ls INDEX 25.
*        SPLIT lines_ls AT ':' INTO l_titulo l_valor.
*        CONDENSE: l_titulo, l_valor.

        refer_ls-fchref = lines_ls-tdline.

        APPEND refer_ls TO refer_lt.

      ENDIF.

    ENDIF.

  ENDIF.

  DELETE refer_lt WHERE fchref    EQ '--'.
  DELETE refer_lt WHERE folioref  IS INITIAL.
  DELETE refer_lt WHERE tpodocref IS INITIAL.

  dte_ev-referencia[] = refer_lt[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CABECERA_DELIV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM cabecera_deliv  USING    data_ls TYPE  edoc_src_data_sd_gi
                     CHANGING dte_ev  TYPE  zclfel_et_documento.

  DATA: cab_ls    TYPE zclfel_encabezado,
        header_ls TYPE edoc_likp,

        item_lt   TYPE edoc_lipsvb_tab,
        item_ls   LIKE lipsvb,
        docu_item TYPE lips.

  DATA: l_ztag1    LIKE t052-ztag1,
        l_fchven   LIKE sy-datum,
        l_netwr    LIKE vbrk-netwr,
        l_neto     LIKE vbrk-netwr,
        l_iva      LIKE vbrk-netwr,
        lv_prec    LIKE vbrk-netwr,
        lv_unit2   LIKE vbrk-netwr,
        lv_iva     LIKE vbrk-mwsbk,
*        lv_iva     LIKE vbrk-netwr,
        l_kbetr    LIKE konv-kbetr,
        l_kwert    LIKE konv-kwert,
        l_mwsbk    LIKE vbrk-mwsbk,
        l_impue    LIKE vbrk-netwr,
        l_ivanr    LIKE vbrk-netwr,
        l_total    LIKE vbrk-mwsbk,
        l_totalg   LIKE vbrk-netwr,
        l_totalusd LIKE vbrk-netwr,
        l_ivausd   LIKE vbrk-netwr,
        l_netwrusd LIKE vbrk-netwr,
        l_waers    TYPE char3,
        l_transp   LIKE vbfa-vbeln,
        l_tknum    LIKE vttk-tknum,
        l_knota    TYPE knota,
        l_knotz    TYPE knotz,
        l_dec      TYPE char4,
        l_agente   LIKE but050-partner1,
        l_add01    LIKE vttk-add01,
        l_nombrech LIKE vtadd01t-bezei,
        adrnr_lv   LIKE t001-adrnr,
        t001_ls    LIKE t001,
        adrc_ls    LIKE adrc,
        adr6_ls    LIKE adr6,
        kna1_ls    LIKE kna1,
        lfa1_ls    LIKE lfa1,
        l_zterm    TYPE dzterm,
        low_lv     TYPE rvari_val_255,
        dte_lv     LIKE edocldteacc-dte_type,
        etype_lv   LIKE t003edoc-edoc_type,
        lv_netpr1  TYPE vbrk-netwr,
*        lv_netpr1  TYPE vbap-netpr,
        lv_mwsbp   TYPE vbap-mwsbp,
        lv_sdabw   TYPE vbkd-sdabw,
        lv_netpr   TYPE ekpo-netpr,
        lv_unit    TYPE ekpo-netpr,
        lv_umrez   TYPE marm-umrez,
        lv_vgbel   TYPE lips-vgbel,
        lv_lgort   TYPE lgort_d,
        lv_cdest   TYPE werks_d,
        lv_lgobe   TYPE t001l-lgobe,
        lv_entr    TYPE likp-vbeln,
        l_kunrg    TYPE vbpa-kunnr,
        l_lifnr    TYPE vbpa-lifnr,
        lv_knumv   TYPE vbak-knumv,
        l_costo    TYPE char1,
        lv_kpein   TYPE kpein,
        l_bu_group TYPE bu_group,
        l_centro   TYPE bwkey,
        l_bukrs    TYPE bukrs,
        l_pfach    TYPE pfach.

  DATA: fono_lv   LIKE adrc-tel_number.

  CLEAR: cab_ls, header_ls.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.
  MOVE-CORRESPONDING data_ls-document_item TO item_lt.


* Usuario que genera la impresion

  cab_ls-iddoc-usuario = sy-uname.

* Tipo Documento
  cab_ls-iddoc-tipodte = header_ls-xblnr+0(3).
  dte_lv          = header_ls-xblnr+0(3).

  PERFORM formateo_tipo_dte CHANGING cab_ls-iddoc-tipodte.

* Folio Documento
  cab_ls-iddoc-folio  = header_ls-xblnr+4.

* Fecha de emisión

  PERFORM formateo_fecha  USING     header_ls-wadat_ist
                          CHANGING  cab_ls-iddoc-fchemis.

* Indicador de traslado

  SELECT SINGLE ind_traslado INTO cab_ls-iddoc-indtraslado
    FROM zclfel_tb_gd
    WHERE lfart EQ header_ls-lfart.

* Rut Emisor
  SELECT SINGLE paval
  INTO cab_ls-emisor-rutemisor
  FROM t001z
  WHERE bukrs EQ data_ls-source_header-bukrs  AND
  party EQ 'RESDAT'.

  IF sy-subrc NE 0.
    SELECT SINGLE paval
     INTO cab_ls-emisor-rutemisor
     FROM t001z
     WHERE bukrs EQ data_ls-source_header-bukrs  AND
     party EQ 'TAXNR'.
  ENDIF.

  CLEAR: adrc_ls.
  SELECT SINGLE *
  INTO t001_ls
  FROM t001
  WHERE bukrs = data_ls-source_header-bukrs.

  IF sy-subrc = 0.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
    INTO adrc_ls
    FROM adrc
    WHERE addrnumber = t001_ls-adrnr.
  ENDIF.

* Razón Social

  cab_ls-emisor-rznsoc    = adrc_ls-name1.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-emisor-rznsoc WITH 'y'.

***
* Giro emisor
  cab_ls-emisor-giroemis = adrc_ls-name3.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-emisor-giroemis WITH 'y'.

* Dirección origen

  CONCATENATE adrc_ls-street
  adrc_ls-house_num1
  INTO cab_ls-emisor-dirorigen
  SEPARATED BY space.

* Comuna origen

  cab_ls-emisor-ciudadorigen  = adrc_ls-city1.

* Ciudad origen

  cab_ls-emisor-cmnaorigen    = adrc_ls-city2.

  IF cab_ls-emisor-cmnaorigen IS INITIAL.
    cab_ls-emisor-cmnaorigen = adrc_ls-city1.
  ENDIF.

* Codigo sucursal

  CLEAR item_ls.
  READ TABLE item_lt INTO item_ls INDEX 1.
  cab_ls-emisor-sucursal = item_ls-werks.

* Codigo SII Sucursal

  SELECT SINGLE acteco INTO cab_ls-emisor-cdgsiisucur
    FROM zsd_sucursales
    WHERE werks EQ item_ls-werks.


* Sucursal
  READ TABLE data_ls-document_item INTO item_ls INDEX 1.
  IF sy-subrc EQ 0 AND cab_ls-iddoc-tipodte NE '039' AND cab_ls-iddoc-tipodte NE '040' AND cab_ls-iddoc-tipodte NE '042'.
    cab_ls-emisor-sucursal = item_ls-werks.
  ELSE.
    cab_ls-emisor-sucursal = ''.
  ENDIF.




  IF header_ls-lfart EQ 'ZE01'.

    CLEAR l_lifnr.
    SELECT SINGLE lifnr INTO l_kunrg
         FROM vbpa
         WHERE vbeln EQ header_ls-vbeln
         AND parvw EQ 'TZ'.

  ELSE.
*    PERFORM guia_nacional USING header_ls.

    SELECT SINGLE kunnr INTO l_kunrg
    FROM vbpa
    WHERE vbeln EQ item_ls-vgbel
    AND parvw EQ 'RG'.

    IF sy-subrc NE 0.
      SELECT SINGLE kunnr INTO l_kunrg
        FROM vbpa
        WHERE vbeln EQ item_ls-vgbel
        AND parvw EQ 'WE'.
    ENDIF.

  ENDIF.

  IF l_kunrg IS NOT INITIAL.
* Rut receptor
    CLEAR: kna1_ls, adr6_ls.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
    INTO kna1_ls
    FROM kna1
    WHERE kunnr = l_kunrg.
  ELSE.
* Rut receptor
    CLEAR: kna1_ls, adr6_ls.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
    INTO kna1_ls
    FROM kna1
    WHERE kunnr = header_ls-kunnr.

  ENDIF.


  IF sy-subrc = 0.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
    INTO adr6_ls
    FROM adr6
    WHERE addrnumber = kna1_ls-adrnr.
  ENDIF.

  TRANSLATE kna1_ls-stcd1 USING '. '.
  CONDENSE kna1_ls-stcd1 NO-GAPS.

  IF kna1_ls-stcd1 IS INITIAL.

    IF l_kunrg IS INITIAL.
      SELECT SINGLE taxnum INTO kna1_ls-stcd1
        FROM dfkkbptaxnum
        WHERE partner EQ header_ls-kunnr.
    ELSE.
      SELECT SINGLE taxnum INTO kna1_ls-stcd1
           FROM dfkkbptaxnum
           WHERE partner EQ l_kunrg.
    ENDIF.



  ENDIF.
  cab_ls-receptor-rutrecep  = kna1_ls-stcd1.


* Código interno receptor
  cab_ls-receptor-cdgintrecep = header_ls-kunnr.

*Razón social receptor

  "si el destinatario es un centro, se debe mostrar la razon social de la sociedad a no ser que sea un centro externo (marca en el campo PFACH EQ '1'.
  CLEAR: l_bu_group, l_centro, l_bukrs.
  SELECT SINGLE bu_group INTO l_bu_group
    FROM but000
    WHERE partner EQ kna1_ls-kunnr.

  IF l_bu_group EQ 'KC01'.

    l_centro = kna1_ls-kunnr.
    SELECT SINGLE bukrs INTO l_bukrs
      FROM t001k
      WHERE bwkey EQ l_centro.

    CLEAR l_pfach.
    SELECT SINGLE pfach INTO l_pfach
       FROM t001w
       WHERE werks EQ l_centro.

    IF l_pfach EQ '1'.
      CONCATENATE kna1_ls-name1
             kna1_ls-name2
      INTO cab_ls-receptor-rznsocrecep
      SEPARATED BY space.
    ELSE.
      CLEAR: adrc_ls, t001_ls.
      SELECT SINGLE *                         "#EC CI_ALL_FIELDS_NEEDED
      INTO t001_ls
      FROM t001
      WHERE bukrs = l_bukrs.

      IF sy-subrc = 0.
        SELECT SINGLE *                       "#EC CI_ALL_FIELDS_NEEDED
        INTO adrc_ls
        FROM adrc
        WHERE addrnumber = t001_ls-adrnr.
      ENDIF.

* Razón Social receptor si es un centro se debe traer la razon social de la sociedad.

      cab_ls-receptor-rznsocrecep    = adrc_ls-name1.

    ENDIF.

  ELSE.
    CONCATENATE kna1_ls-name1
                kna1_ls-name2
    INTO cab_ls-receptor-rznsocrecep
    SEPARATED BY space.

  ENDIF.


* Giro receptor

  CONCATENATE kna1_ls-name3 kna1_ls-name4 INTO cab_ls-receptor-girorecep SEPARATED BY space.

  IF cab_ls-receptor-girorecep EQ space.
    cab_ls-receptor-girorecep = '.'.
  ENDIF.

  CLEAR: lv_vgbel, lv_entr.
  SELECT SINGLE vgbel INTO lv_vgbel
    FROM lips
    WHERE vbeln EQ header_ls-vbeln.

  SELECT SINGLE lgort INTO lv_lgort
    FROM ekpo
    WHERE ebeln EQ lv_vgbel.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = header_ls-kunnr
      IMPORTING
        output = lv_cdest.

*    lv_cdest = header_ls-kunnr.
    SELECT SINGLE lgobe INTO lv_lgobe
      FROM t001l
      WHERE werks EQ lv_cdest
      AND lgort EQ lv_lgort.
  ENDIF.


* Direccion Receptor "Se solicita modificacion Ticket 508 (no concatenar nombre y codigo de almacen a direccion receptor

* Para todos los tipos de guias de despacho debe aparecer la direccion del destinatario de mercaderia (Excepto para la guia de exportacion 'ZE01' Ticket IBASK 1345


  cab_ls-receptor-dirrecep    = kna1_ls-stras.




  cab_ls-receptor-cmnarecep   = kna1_ls-ort02.

*** datos de despacho y destinatarios de mercaderia *****

  SELECT SINGLE name1 stcd1 stras ort01 ort02 INTO ( cab_ls-receptor-nombredestinatario, cab_ls-receptor-rutdestinatario, cab_ls-transporte-dirdest, cab_ls-transporte-ciudaddest, cab_ls-transporte-cmnadest )
    FROM kna1
    WHERE kunnr EQ header_ls-kunnr.


  "comentado para igualar versiones
  IF header_ls-lfart EQ 'ZE01'.

* Direccion Receptor
    cab_ls-receptor-dirrecep    = kna1_ls-stras.

* Comuna Receptor
    cab_ls-receptor-cmnarecep   = kna1_ls-ort02.

* Ciudad Receptor
    cab_ls-receptor-ciudadrecep = kna1_ls-ort01.

  ELSE.

* Direccion Receptor
    cab_ls-receptor-dirrecep    = cab_ls-transporte-dirdest.

* Comuna Receptor
    cab_ls-receptor-cmnarecep   = cab_ls-transporte-cmnadest.

* Ciudad Receptor
    cab_ls-receptor-ciudadrecep = cab_ls-transporte-ciudaddest.


  ENDIF.

* Ciudad Receptor

*  cab_ls-receptor-ciudadrecep = kna1_ls-ort01.

  IF cab_ls-receptor-cmnarecep IS INITIAL.
    cab_ls-receptor-cmnarecep = cab_ls-receptor-ciudadrecep.
  ENDIF.



*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
  DATA: l_knumh     TYPE knumh,
        l_konwa     TYPE konp-konwa,
        l_pmata     TYPE mara-pmata,
        lv_netwr    LIKE vbrk-netwr,
        l_attyp     TYPE mara-attyp,
        l_nomtransp TYPE but000-mc_name1,
        l_matnr     TYPE mara-matnr,
        ls_vbap     TYPE vbap,
        l_ukurs     TYPE ukurs_curr,
        l_gdatu     TYPE gdatu_inv,
        l_fechatc   TYPE char8.

  DATA: l_name            TYPE thead-tdname,
        l_object          TYPE thead-tdobject,
        l_id              TYPE thead-tdid,
        p_text            TYPE string,
        p_text_booking    TYPE tdline,
        p_text_motonave   TYPE tdline,
        p_text_naviera    TYPE tdline,
        p_text_termografo TYPE tdline,
        p_text_viaje      TYPE tdline.

  DATA: lt_setleaf_t  TYPE STANDARD TABLE OF setleaf.
  DATA: ls_setleaf_t  TYPE setleaf.
  DATA: ls_mbew       TYPE mbew.

  DATA: lt_setleaf_m  TYPE STANDARD TABLE OF setleaf.
  DATA: ls_setleaf_m  TYPE setleaf.



  IF header_ls-lfart EQ 'ZE05'
  OR header_ls-lfart EQ 'ZE04'. "EXT_AMEJIAS 28.05.2021


    REFRESH lt_setleaf_t.
    CLEAR ls_setleaf_t.
    SELECT * INTO TABLE lt_setleaf_t
      FROM setleaf
      WHERE setname EQ 'ZCENTRO_GUIA_VALORIZADA'.


    LOOP AT data_ls-document_item INTO docu_item.

      CLEAR  ls_setleaf_t.
      READ TABLE lt_setleaf_t INTO ls_setleaf_t WITH KEY valfrom = docu_item-werks.
      IF sy-subrc EQ 0 AND header_ls-lfart EQ 'ZE05'.
        l_costo = abap_true.

        CLEAR ls_mbew.
        SELECT SINGLE * INTO ls_mbew
          FROM mbew
          WHERE matnr EQ docu_item-matnr
          AND   bwkey EQ docu_item-werks.

        IF ls_mbew-vprsv EQ 'S'.
          l_kbetr = '19.00'.
          l_kbetr = l_kbetr / 100.
          lv_prec = ls_mbew-stprs  * docu_item-lfimg.
*          lv_prec = ( ls_mbew-stprs / 100 ) * docu_item-lfimg.
          l_netwr = l_netwr + lv_prec.
          lv_iva = lv_prec * l_kbetr.
          l_iva   = l_iva + lv_iva.

        ELSEIF ls_mbew-vprsv EQ 'V'.
          l_kbetr = '19.00'.
          l_kbetr = l_kbetr / 100.
          lv_prec = ls_mbew-verpr  * docu_item-lfimg.
*          lv_prec = ( ls_mbew-verpr / 100 ) * docu_item-lfimg.
          l_netwr = l_netwr + lv_prec.
          lv_iva = lv_prec * l_kbetr.
          l_iva   = l_iva + lv_iva.

        ENDIF.

      ELSE.

        CLEAR: lv_knumv, lv_netpr1.

        SELECT SINGLE knumv INTO lv_knumv
          FROM ekko
          WHERE ebeln EQ docu_item-vgbel.

        SELECT SINGLE kbetr INTO lv_netpr1
             FROM prcd_elements
             WHERE knumv EQ lv_knumv
             AND   kposn EQ docu_item-vgpos
             AND   kschl EQ 'PB00'.

        l_kbetr = '19.00'.
        l_kbetr = l_kbetr / 100.
        lv_prec = ( lv_netpr1 / 100 ) * docu_item-lfimg.
        l_netwr = l_netwr + lv_prec.
        lv_iva = lv_prec * l_kbetr.
        l_iva   = l_iva + lv_iva.
      ENDIF.
    ENDLOOP.


  ELSEIF header_ls-lfart EQ 'ZE14'.

    LOOP AT data_ls-document_item INTO docu_item.

      CLEAR: lv_knumv, lv_netpr1.

      SELECT SINGLE knumv INTO lv_knumv
        FROM ekko
        WHERE ebeln EQ docu_item-vgbel.

      SELECT SINGLE kbetr INTO lv_netpr1
           FROM prcd_elements
           WHERE knumv EQ lv_knumv
           AND   kposn EQ docu_item-vgpos
           AND   kschl LIKE 'PB%'
           AND   kinak NE 'X'.

      l_kbetr = '19.00'.
      l_kbetr = l_kbetr / 100.
      lv_prec = lv_netpr1 * docu_item-lfimg.
      l_netwr = l_netwr + lv_prec.
      lv_iva = lv_prec * l_kbetr.
      l_iva   = l_iva + lv_iva.

    ENDLOOP.


  ELSE.
    LOOP AT data_ls-document_item INTO docu_item WHERE lfimg IS NOT INITIAL AND pstyv NE 'TA1'.

      CLEAR: lv_netpr1, lv_mwsbp,lv_sdabw, l_kbetr, ls_vbap.

      SELECT SINGLE * INTO ls_vbap            "#EC CI_ALL_FIELDS_NEEDED
        FROM vbap
        WHERE vbeln EQ docu_item-vgbel AND
              posnr EQ docu_item-vgpos.


      "Tomar el precio del pedido de ventas valor netwr
      CLEAR lv_kpein.
      SELECT SINGLE knumv INTO lv_knumv
        FROM vbak
        WHERE vbeln EQ docu_item-vgbel.

      SELECT SINGLE kpein INTO lv_kpein
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND kschl  LIKE 'ZB%'.

      "Tomar el precio del pedido de ventas valor netwr
      SELECT SINGLE netpr mwsbp INTO (lv_netpr1 , lv_mwsbp)
       FROM vbap
       WHERE vbeln EQ docu_item-vgbel
       AND   posnr EQ docu_item-vgpos.

      CLEAR: lv_prec, lv_iva.
      IF lv_kpein > 1.
        lv_prec = lv_netpr1 * docu_item-lfimg / lv_kpein.
      ELSE.
        lv_prec = lv_netpr1 * docu_item-lfimg.
      ENDIF.

      lv_iva = lv_prec * '0.19'.
*      lv_iva  = 0.
*      lv_iva  = lv_mwsbp.
*          lv_iva  = lv_mwsbp * docu_item-lfimg.

      l_netwr = l_netwr + lv_prec.
      l_iva   = l_iva + lv_iva.
    ENDLOOP.
  ENDIF.


******* tipo de cambio para entrega de exportacion e intercompany*****

* DATA_LS-DOCUMENT_HEADER-WADAT_IST Fecha de salida de mercaderia

  CLEAR: l_gdatu, l_ukurs, l_fechatc.

  CONCATENATE data_ls-document_header-wadat_ist+6(2) data_ls-document_header-wadat_ist+4(2) data_ls-document_header-wadat_ist+0(4) INTO l_fechatc.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = l_fechatc
    IMPORTING
      output = l_gdatu.

  SELECT SINGLE ukurs INTO l_ukurs
    FROM tcurr
    WHERE kurst   EQ 'M'
    AND   fcurr   EQ 'CLP'
    AND   tcurr   EQ 'USD'
    AND   gdatu   EQ l_gdatu.

  IF sy-subrc NE 0.
    SELECT SINGLE ukurs INTO l_ukurs
       FROM tcurr
       WHERE kurst   EQ 'M'
       AND   fcurr   EQ 'CLP'
       AND   tcurr   EQ 'USD'
       AND   gdatu   >= l_gdatu.
  ENDIF.

  l_ukurs = 0 - l_ukurs.

  l_totalg =  l_netwr + l_iva. "TOTAL
*  l_totalg =  l_netwr. "TOTAL



  IF data_ls-document_header-lfart EQ 'ZE01' OR data_ls-document_header-lfart EQ 'ZE14' OR l_costo EQ abap_true.
    l_totalusd = l_totalg * l_ukurs.
    l_ivausd = l_iva * l_ukurs.
    l_netwrusd = l_netwr * l_ukurs.

    cab_ls-totales-mnttotal = l_totalusd.
    REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-mnttotal WITH ','.
    CONDENSE cab_ls-totales-mnttotal.


    cab_ls-totales-iva =  l_ivausd.
    REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-iva WITH ','.
    CONDENSE cab_ls-totales-iva.

    IF l_ivausd EQ 0.
      cab_ls-totales-tasaiva = '0'.
      cab_ls-totales-mntexe = l_netwrusd.
      REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-mntexe WITH ','.
      CONDENSE cab_ls-totales-mntexe.


    ELSE.
      cab_ls-totales-mntneto = l_netwrusd.
      REPLACE ALL OCCURRENCES OF '.' IN cab_ls-totales-mntneto WITH ','.
      CONDENSE cab_ls-totales-mntneto.

      IF cab_ls-totales-mntneto IS INITIAL.
        cab_ls-totales-mntneto = '0'.
      ENDIF.
      cab_ls-totales-tasaiva = '19'.
    ENDIF.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mnttotal AT ',' INTO  cab_ls-totales-mnttotal l_dec.
    IF l_dec >= 50.
      cab_ls-totales-mnttotal = cab_ls-totales-mnttotal + 1.
      CONDENSE cab_ls-totales-mnttotal.
    ENDIF.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mntneto  AT ',' INTO  cab_ls-totales-mntneto  l_dec.
    IF l_dec >= 50.
      cab_ls-totales-mntneto  = cab_ls-totales-mntneto  + 1.
      CONDENSE cab_ls-totales-mntneto.
    ENDIF.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mntexe AT ',' INTO  cab_ls-totales-mntexe l_dec.
    IF l_dec >= 50.
      cab_ls-totales-mntexe = cab_ls-totales-mntexe + 1.
      CONDENSE cab_ls-totales-mntexe.
    ENDIF.


    CLEAR l_dec.
    SPLIT  cab_ls-totales-iva AT ',' INTO   cab_ls-totales-iva l_dec.
    IF l_dec >= 50.
      cab_ls-totales-iva =  cab_ls-totales-iva + 1.
      CONDENSE cab_ls-totales-iva.
    ENDIF.

  ELSE.

    l_waers = 'CLP'.

    PERFORM formateo_montos USING  l_iva
                                 l_waers
                          CHANGING  cab_ls-totales-iva.

    IF l_iva EQ 0.
      PERFORM formateo_montos USING  l_netwr
                                 l_waers
                          CHANGING  cab_ls-totales-mntexe.
      cab_ls-totales-tasaiva = '0'.
    ELSE.
      PERFORM formateo_montos USING  l_netwr
                                  l_waers
                           CHANGING  cab_ls-totales-mntneto.

      IF cab_ls-totales-mntneto IS INITIAL.
        cab_ls-totales-mntneto = '0'.
      ENDIF.

      cab_ls-totales-tasaiva = '19'.
    ENDIF.

    PERFORM formateo_montos USING  l_totalg
                                   l_waers
                            CHANGING  cab_ls-totales-mnttotal.

  ENDIF.



*--------------------------------------------------------------------*
*--------------------------------------------------------------------
**** Datos de transporte y exportacion ******************

  REFRESH: lt_vbfa_tr[], lt_vttk[].
  CLEAR:   ls_vbfa_tr, ls_vttk.

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE  lt_vbfa_tr
    FROM vbfa
    WHERE vbelv EQ header_ls-vbeln
    AND   vbtyp_n EQ '8'.

  IF lt_vbfa_tr[] IS NOT INITIAL.
    SELECT tknum                                   "#EC CI_NO_TRANSFORM
                 shtyp
                 tdlnr
                 route
                 vsart
                 add01
                 add02
                 add03
                 text1
                 text2
                 text3
                 text4
                 datbg
                 signi INTO CORRESPONDING FIELDS OF TABLE lt_vttk FROM vttk FOR ALL ENTRIES IN lt_vbfa_tr
       WHERE tknum EQ  lt_vbfa_tr-vbeln.
  ENDIF.


  REFRESH lt_setleaf_t.
  CLEAR ls_setleaf_t.
  SELECT * INTO TABLE lt_setleaf_t
    FROM setleaf
    WHERE setname EQ 'ZSD_TRANSPORTE_TERRESTRE'.

  REFRESH lt_setleaf_m.
  CLEAR ls_setleaf_m.
  SELECT * INTO TABLE lt_setleaf_m
    FROM setleaf
    WHERE setname EQ 'ZSD_TRANSPORTE_MARITIMO'.

* Determino clase de transporte terrestre

  LOOP AT lt_setleaf_t INTO ls_setleaf_t.
    CLEAR ls_vttk.
    READ TABLE lt_vttk INTO ls_vttk WITH KEY shtyp = ls_setleaf_t-valfrom.
    IF sy-subrc EQ 0.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_vttk-tdlnr
        IMPORTING
          output = cab_ls-transporte-transp_terrestre.


      EXIT.
    ENDIF.
  ENDLOOP.

* Determino clase de transporte maritimo

  LOOP AT lt_setleaf_m INTO ls_setleaf_m.
    CLEAR ls_vttk2.
    READ TABLE lt_vttk INTO ls_vttk2 WITH KEY shtyp = ls_setleaf_m-valfrom.
    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  CLEAR: l_transp, l_tknum, l_add01, l_nombrech.

  IF ls_vttk IS NOT INITIAL OR ls_vttk2 IS NOT INITIAL.

    l_tknum = l_transp.

    SELECT SINGLE mc_name1 INTO cab_ls-transporte-nomcia_transp
      FROM but000
      WHERE partner EQ ls_vttk-tdlnr.

    IF sy-subrc NE 0.
      SELECT SINGLE mc_name1 INTO cab_ls-transporte-nomcia_transp
       FROM but000
       WHERE partner EQ ls_vttk2-tdlnr.
    ENDIF.

    SELECT SINGLE taxnum INTO cab_ls-transporte-ruttrans
      FROM  dfkkbptaxnum
      WHERE partner EQ ls_vttk-tdlnr
      AND taxtype EQ 'CL1'.

    IF sy-subrc NE 0.
      SELECT SINGLE taxnum INTO cab_ls-transporte-ruttrans
      FROM  dfkkbptaxnum
      WHERE partner EQ ls_vttk2-tdlnr
      AND taxtype EQ 'CL1'.

      cab_ls-transporte-nombre_transp = cab_ls-transporte-nomcia_transp.
      cab_ls-transporte-rutchofer = ls_vttk2-add01.
      cab_ls-transporte-patente   = ls_vttk2-add02.
    ELSE.
      cab_ls-transporte-nombre_transp = cab_ls-transporte-nomcia_transp.
      cab_ls-transporte-rutchofer = ls_vttk-add01.
      cab_ls-transporte-patente   = ls_vttk-add02.

    ENDIF.
    "Comentado para igualar versiones
*    IF ls_vttk-shtyp EQ 'ZT04' OR ( ls_vttk-shtyp IS NOT INITIAL AND ls_vttk-tdlnr IS INITIAL ).
*      cab_ls-transporte-nombre_transp = cab_ls-transporte-nomcia_transp.
*      cab_ls-transporte-rutchofer = ls_vttk-add01.
*      cab_ls-transporte-patente   = ls_vttk-add02.
*    ENDIF.
    "Comentado para igualar versiones

    SELECT SINGLE bezei INTO l_nombrech
     FROM vtadd01t
     WHERE add_info EQ ls_vttk-add01
     AND spras EQ sy-langu.


    IF sy-subrc NE 0.
      SELECT SINGLE bezei INTO l_nombrech
      FROM vtadd01t
      WHERE add_info EQ ls_vttk2-add01
      AND spras EQ sy-langu.
    ENDIF.

    cab_ls-transporte-nombrechofer = l_nombrech+0(29).

    cab_ls-transporte-rampla = ls_vttk-add03.
    PERFORM format_texto CHANGING cab_ls-transporte-rampla.    "Ajuste por caracteres especiales

****** exportacion ********

    "Agente de aduana

    SELECT SINGLE partner2 INTO l_agente
      FROM but050
      WHERE partner1 EQ l_kunrg
      AND reltyp EQ 'BUR001'.

    IF  sy-subrc EQ 0.
      SELECT SINGLE name_last INTO cab_ls-exportacion-agente_aduana
        FROM but000
        WHERE partner EQ l_agente.

    ENDIF.


*** puerto de embarque y desembarque *****

    CLEAR: l_knota, l_knotz.
    SELECT SINGLE  knota knotz INTO ( l_knota, l_knotz )
      FROM vtts
      WHERE tknum EQ ls_vttk-tknum.

    SELECT SINGLE puerto_sii INTO cab_ls-exportacion-pto_embarque
      FROM zsd_puertos_exp
      WHERE knota EQ l_knota.

    SELECT SINGLE puerto_sii INTO cab_ls-exportacion-pto_desembarque
      FROM zsd_puertos_exp
      WHERE knota EQ l_knotz.

******* Booking *****

    l_name = ls_vttk-tknum.
    l_id = 'Z006'.
    l_object = 'VTTK'.

    PERFORM get_text USING l_name
                           l_id
                           l_object
                    CHANGING p_text_booking.

    IF sy-subrc NE 0.
      l_name = ls_vttk2-tknum.
      l_id = 'Z006'.
      l_object = 'VTTK'.

      PERFORM get_text USING l_name
                             l_id
                             l_object
                      CHANGING p_text_booking.
    ENDIF.

    cab_ls-exportacion-booking = p_text_booking.
    PERFORM format_texto CHANGING cab_ls-exportacion-booking.    "Ajuste por caracteres especiales

******* Motonave *********

    l_name = ls_vttk-tknum.
    l_id = 'Z024'.
    l_object = 'VTTK'.

    PERFORM get_text USING l_name
                           l_id
                           l_object
                    CHANGING p_text_motonave.

    cab_ls-exportacion-nave = p_text_motonave.
    PERFORM format_texto CHANGING cab_ls-exportacion-nave.    "Ajuste por caracteres especiales

************** Naviera *************

    l_name = ls_vttk-tknum.
    l_id = 'Z009'.
    l_object = 'VTTK'.

    PERFORM get_text USING l_name
                           l_id
                           l_object
                    CHANGING p_text_naviera.

    cab_ls-exportacion-naviera = p_text_naviera.
    PERFORM format_texto CHANGING cab_ls-exportacion-naviera.    "Ajuste por caracteres especiales

******** Termografo ************

    l_name = header_ls-vbeln.
    l_id = 'Z003'.
    l_object = 'VBBK'.

    PERFORM get_text USING l_name
                           l_id
                           l_object
                    CHANGING p_text_termografo.

    cab_ls-exportacion-termografo = p_text_termografo.

    cab_ls-exportacion-cod_via_transp = ls_vttk-vsart.


    cab_ls-exportacion-termografo = p_text_termografo.
    PERFORM format_texto CHANGING cab_ls-exportacion-termografo.    "Ajuste por caracteres especiales
************** Viaje *************

    l_name = ls_vttk-tknum.
    l_id = 'Z016'.
    l_object = 'VTTK'.

    PERFORM get_text USING l_name
                           l_id
                           l_object
                    CHANGING p_text_viaje.

    cab_ls-exportacion-viaje          =  p_text_viaje.

  ENDIF.


  "Agregado para igualar versiones

**** datos de despacho *****

  SELECT SINGLE stras ort01 ort02 INTO ( cab_ls-transporte-dirdest, cab_ls-transporte-ciudaddest, cab_ls-transporte-cmnadest )
    FROM kna1
    WHERE kunnr EQ header_ls-kunnr.
  "Agregado para igualar versiones

****** sellos *****

  cab_ls-transporte-sellos = ls_vttk-text1.
  PERFORM format_texto CHANGING cab_ls-transporte-sellos.    "Ajuste por caracteres especiales

***** telefono chofer *****

  cab_ls-transporte-telefono = ls_vttk-text4.
  PERFORM format_texto CHANGING cab_ls-transporte-telefono.    "Ajuste por caracteres especiales

*** Impresora ***


  SELECT SINGLE impresora INTO cab_ls-emisor-impresora
    FROM zsd_impresoras
    WHERE usuario EQ sy-uname.


******** Observaciones cabecera ********

  l_name = data_ls-document_header-vbeln.
  l_id = 'Z013'.
  l_object = 'VBBK'.

  PERFORM get_text USING l_name
                         l_id
                         l_object
                  CHANGING p_text.

  IF header_ls-lfart EQ 'ZE15'.
    CONCATENATE 'No Constituye Venta. Producto Resultante Servicio Maquila. ' p_text INTO cab_ls-receptor-observacion.
  ELSE.
    cab_ls-receptor-observacion = p_text.
  ENDIF.

************INI**4000335559******WVSA*******
  DATA: lv_nombre_soc TYPE tvarvc-low,
        lv_lfart_soc  TYPE tvarvc-low.

  SELECT SINGLE low
     FROM tvarvc
     INTO lv_nombre_soc
    WHERE name = 'ZSD_TEXT_OBPDF_EDOC'.
  SELECT SINGLE low
     FROM tvarvc
     INTO lv_lfart_soc
    WHERE name = 'ZSD_LFART_OBPDF_EDOC'.

  IF header_ls-lfart EQ lv_lfart_soc.
    CONCATENATE lv_nombre_soc p_text INTO cab_ls-receptor-observacion SEPARATED BY space.
  ENDIF.
************FIN**4000335559******WVSA*******
  PERFORM format_texto CHANGING cab_ls-receptor-observacion.    "Ajuste por caracteres especiales


****** contenedor ****


  IF ls_vttk IS NOT INITIAL.
    cab_ls-transporte-contenedor = ls_vttk-signi.
  ELSE.
    cab_ls-transporte-contenedor = ls_vttk2-signi.
  ENDIF.

  PERFORM format_texto CHANGING cab_ls-transporte-contenedor.    "Ajuste por caracteres especiales


***** Total de bultos *****

  cab_ls-exportacion-cantbultos = header_ls-anzpk.


  dte_ev-encabezado = cab_ls.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FORMATEO_TIPO_DTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CAB_LS_IDDOC_TIPODTE
*&---------------------------------------------------------------------*
FORM formateo_tipo_dte  CHANGING p_tdte.

  DATA: dte_lv(3) TYPE c.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_tdte
    IMPORTING
      output = dte_lv.

  CONDENSE dte_lv NO-GAPS.

  p_tdte = dte_lv.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ACTECO_DELIV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM acteco_deliv  USING    data_ls TYPE  edoc_src_data_sd_gi
                    CHANGING dte_ev  TYPE  zclfel_et_documento.


  DATA: header_ls TYPE edoc_likp.

  DATA: paval_lv  LIKE t001z-paval.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.

* ACTECO
  SELECT SINGLE paval
  INTO          paval_lv
  FROM          t001z
  WHERE         bukrs EQ data_ls-source_header-bukrs  AND
                party EQ 'ACTECO'.

  IF sy-subrc = 0.
    dte_ev-encabezado-emisor-acteco = paval_lv.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETALLE_DELIV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      --> VBELN_VL
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM detalle_deliv   USING    data_ls  TYPE  edoc_src_data_sd_gi
                                            vbeln_vl
                    CHANGING dte_ev   TYPE  zclfel_et_documento.

  DATA: deta_lt  TYPE zclfel_t_detalle,
        deta_ls  TYPE zclfel_detalle,
        matkg_lt TYPE STANDARD TABLE OF zclfel_mat_kg,
        matkg_ls TYPE zclfel_mat_kg,
        cod_lt   TYPE zclfel_cdgitem_t,
        cod_ls   TYPE zclfel_cdgitem.

  DATA: t001_ls LIKE t001.

  DATA: waers_lv TYPE waers.

*  DATA: lt_vepo TYPE STANDARD TABLE OF vepo.

  DATA: item_lt   TYPE edoc_lipsvb_tab,
        item_ls   TYPE lipsvb,
        item2_ls  TYPE lipsvb,
        header_ls TYPE edoc_likp,
        docu_flow TYPE vbfa,
        ls_vbap   TYPE vbap.

  DATA: lt_setleaf_t  TYPE STANDARD TABLE OF setleaf.
  DATA: ls_setleaf_t  TYPE setleaf.
  DATA: ls_mbew       TYPE mbew.

  DATA: linea_lv         LIKE sy-tabix,
        lv_netwr         LIKE vbrk-netwr,
        lv_netpr1        TYPE vbap-netpr,
        lv_knumv         TYPE vbak-knumv,
        lv_mwsbp         TYPE vbap-mwsbp,
        lv_sdabw         TYPE vbkd-sdabw,
        l_kbetr          LIKE konv-kbetr,
        lv_prec          LIKE vbrk-netwr,
        lv_prec_usd      LIKE vbrk-netwr,
        lv_prec_unit     LIKE vbrk-netwr,
        lv_prec_unit_usd LIKE vbrk-netwr,
        l_dec            TYPE char4,
        l_dec2           TYPE char2,
        l_costo          TYPE char1,
        lv_umrez         LIKE marm-umrez,
        lv_kpein         TYPE kpein,
        lv_name          TYPE thead-tdname,
        lv_descitem      TYPE char1025,
        l_ukurs          TYPE ukurs_curr,
        l_gdatu          TYPE gdatu_inv,
        l_fechatc        TYPE char8,
        l_cantidad       TYPE i,
        l_cajas          TYPE i,
        l_cantidad_e     TYPE lfimg,
        l_mtart          TYPE mtart.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.


**--------------------------------------------------------------------*
**--------------------------------------------------------------------*

  REFRESH lt_setleaf_t.
  CLEAR ls_setleaf_t.
  SELECT * INTO TABLE lt_setleaf_t
    FROM setleaf
    WHERE setname EQ 'ZCENTRO_GUIA_VALORIZADA'.

  REFRESH  matkg_lt[].
  SELECT * INTO TABLE matkg_lt
    FROM zclfel_mat_kg.


  LOOP AT data_ls-document_item INTO item_ls WHERE ( uecha IS INITIAL ) OR pstyv EQ 'ZNLN'  OR pstyv EQ 'NLC'.
    CLEAR: deta_ls.

*   Nro Linea de Detalle
    ADD 1 TO linea_lv.
    deta_ls-nrolindet = linea_lv.

    PERFORM formateo_nlin CHANGING deta_ls-nrolindet.

*   Indicador de exención
    deta_ls-indexe = '4'. "--> Item no venta

*   Nombre del Item

    CLEAR: lv_name, l_mtart.

    SELECT SINGLE mtart INTO l_mtart
       FROM mara
       WHERE matnr EQ item_ls-matnr.


    IF header_ls-lfart EQ 'ZE15'.

      lv_name = item_ls-matnr.
      PERFORM get_text_deliv  USING    lv_name
            'GRUN'
            'MATERIAL'
            'ZH'
      CHANGING lv_descitem.

      deta_ls-nmbitem = lv_descitem+0(80).

      deta_ls-dscitem = lv_descitem+80(919).

      IF l_mtart EQ 'ZSEM'.
        deta_ls-nmbitem = item_ls-arktx.
      ENDIF.

    ELSE.


      IF l_mtart EQ 'ZPT1'.

        lv_name = item_ls-matnr.
        PERFORM get_text_deliv  USING    lv_name
              'GRUN'
              'MATERIAL'
              'S'
        CHANGING lv_descitem.

        deta_ls-nmbitem = lv_descitem+0(80).

        deta_ls-dscitem = lv_descitem+80(919).

      ELSEIF l_mtart EQ 'ZSEM'.

        deta_ls-nmbitem = item_ls-arktx.

      ELSE.
        CONCATENATE item_ls-vbeln item_ls-posnr INTO lv_name RESPECTING BLANKS.
        PERFORM get_text_deliv  USING    lv_name
              '0001'
              'VBBP'
              'S'
        CHANGING lv_descitem.

        deta_ls-nmbitem = lv_descitem+0(80).

        deta_ls-dscitem = lv_descitem+80(919).

        IF lv_descitem IS INITIAL.
          deta_ls-nmbitem = item_ls-arktx.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: item2_ls, l_cajas, l_cantidad_e.
    READ TABLE data_ls-document_item INTO item2_ls WITH KEY uecha = item_ls-posnr.
    IF sy-subrc EQ 0.
      CLEAR item2_ls.
      LOOP AT data_ls-document_item INTO item2_ls WHERE uecha EQ item_ls-posnr.
        l_cantidad_e = l_cantidad_e + item2_ls-lfimg.

        " Cantidad de cajas por posicion.
        REFRESH t_vepo[].
        CLEAR: t_vepo, l_cantidad.

        SELECT venum vepos vbeln posnr INTO CORRESPONDING FIELDS OF TABLE t_vepo "#EC CI_NOFIELD
          FROM vepo
          WHERE vbeln EQ item2_ls-vbeln
          AND   posnr EQ item2_ls-posnr.

        DESCRIBE TABLE t_vepo LINES l_cantidad.
        l_cajas = l_cajas + l_cantidad.

      ENDLOOP.

      deta_ls-cajas = l_cajas.

      CONDENSE deta_ls-cajas.

      CLEAR matkg_ls.
      READ TABLE matkg_lt INTO matkg_ls WITH KEY matnr = item_ls-matnr.
      IF sy-subrc EQ 0.

        item_ls-lfimg = item_ls-ntgew.
        item_ls-vrkme = item_ls-gewei.
        PERFORM formateo_cantidad USING item_ls-lfimg
                                        item_ls-vrkme
                                  CHANGING deta_ls-qtyitem
                                           deta_ls-unmditem.



      ELSE.
        item_ls-lfimg = l_cantidad_e.
        PERFORM formateo_cantidad USING item_ls-lfimg
                                        item_ls-vrkme
                                  CHANGING deta_ls-qtyitem
                                           deta_ls-unmditem.

      ENDIF.

    ELSE.


      CLEAR matkg_ls.
      READ TABLE matkg_lt INTO matkg_ls WITH KEY matnr = item_ls-matnr.
      IF sy-subrc EQ 0.

        item_ls-lfimg = item_ls-ntgew.
        item_ls-vrkme = item_ls-gewei.
        PERFORM formateo_cantidad USING item_ls-lfimg
                                        item_ls-vrkme
                                  CHANGING deta_ls-qtyitem
                                           deta_ls-unmditem.



      ELSE.

        PERFORM formateo_cantidad USING item_ls-lfimg
                                       item_ls-vrkme
                                 CHANGING deta_ls-qtyitem
                                          deta_ls-unmditem.

      ENDIF.



      " Cantidad de cajas por posicion.
      REFRESH t_vepo[].
      CLEAR: t_vepo, l_cantidad.

      SELECT venum vepos vbeln posnr INTO CORRESPONDING FIELDS OF TABLE t_vepo "#EC CI_NOFIELD
        FROM vepo
        WHERE vbeln EQ item_ls-vbeln
        AND   posnr EQ item_ls-posnr.

      DESCRIBE TABLE t_vepo LINES l_cantidad.
      deta_ls-cajas = l_cantidad.
      CONDENSE deta_ls-cajas.
    ENDIF.


    IF deta_ls-unmditem EQ 'KG'.

      CLEAR l_dec.
      SPLIT deta_ls-qtyitem AT '.' INTO deta_ls-qtyitem l_dec.

      CLEAR l_dec2.
      l_dec2 = l_dec+0(2).
      IF l_dec2 IS INITIAL.
        l_dec2 = '00'.
      ENDIF.
      CONCATENATE deta_ls-qtyitem '.' l_dec2 INTO deta_ls-qtyitem.
      CONDENSE deta_ls-qtyitem.

    ENDIF.

****************** precio por posicion ********
    CLEAR l_costo.
    IF header_ls-lfart EQ 'ZNL'.

      CLEAR lv_knumv.
      SELECT SINGLE knumv INTO lv_knumv
        FROM ekko
        WHERE ebeln EQ item_ls-vgbel.

      SELECT SINGLE kbetr INTO lv_netpr1
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND   kposn EQ item_ls-vgpos
        AND   stunr EQ '050'.

      SELECT SINGLE kbetr INTO l_kbetr
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND   kposn EQ item_ls-vgpos
        AND   kschl EQ 'MWST'.
      IF sy-subrc NE 0.
        l_kbetr = '19.00'.
      ENDIF.
      l_kbetr = l_kbetr / 100.
      lv_prec = ( lv_netpr1 / 100 ) * item_ls-lfimg.
      lv_prec_unit = ( lv_netpr1 / 100 ).

    ELSEIF header_ls-lfart EQ 'ZE14'.

      CLEAR: lv_knumv, lv_netpr1.

      SELECT SINGLE knumv INTO lv_knumv
        FROM ekko
        WHERE ebeln EQ item_ls-vgbel.

      SELECT SINGLE kbetr INTO lv_netpr1
           FROM prcd_elements
           WHERE knumv EQ lv_knumv
           AND   kposn EQ item_ls-vgpos
           AND   kschl LIKE 'PB%'
           AND   kinak NE 'X'.

      l_kbetr = '19.00'.
      l_kbetr = l_kbetr / 100.
      lv_prec = lv_netpr1 * item_ls-lfimg.
      lv_prec_unit = lv_netpr1.
*      lv_iva = lv_prec * l_kbetr.

    ELSEIF header_ls-lfart EQ 'ZE05'.

      l_costo = abap_true.
      CLEAR  ls_setleaf_t.
      READ TABLE lt_setleaf_t INTO ls_setleaf_t WITH KEY valfrom = item_ls-werks.

      IF sy-subrc EQ 0.
        CLEAR ls_mbew.
        SELECT SINGLE * INTO ls_mbew          "#EC CI_ALL_FIELDS_NEEDED
        FROM mbew
        WHERE matnr EQ item_ls-matnr AND
              bwkey EQ item_ls-werks.


        IF ls_mbew-vprsv EQ 'S'.
          l_kbetr = '19.00'.
          l_kbetr = l_kbetr / 100.
          lv_prec = ls_mbew-stprs * item_ls-lfimg.
          lv_prec_unit = ls_mbew-stprs.

        ELSEIF ls_mbew-vprsv EQ 'V'.
          l_kbetr = '19.00'.
          l_kbetr = l_kbetr / 100.
          lv_prec = ls_mbew-verpr * item_ls-lfimg.
          lv_prec_unit = ls_mbew-verpr.

        ENDIF.

      ENDIF.



    ELSE.

      SELECT SINGLE * INTO ls_vbap            "#EC CI_ALL_FIELDS_NEEDED
       FROM vbap
       WHERE vbeln EQ item_ls-vgbel AND
             posnr EQ item_ls-vgpos.

      "Tomar el precio del pedido de ventas valor netwr
      CLEAR lv_kpein.
      SELECT SINGLE knumv INTO lv_knumv
        FROM vbak
        WHERE vbeln EQ item_ls-vgbel.

      SELECT SINGLE kpein INTO lv_kpein
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND kschl  LIKE 'ZP%'.

      SELECT SINGLE netpr mwsbp INTO (lv_netpr1 , lv_mwsbp)
       FROM vbap
       WHERE vbeln EQ item_ls-vgbel
       AND   posnr EQ item_ls-vgpos.

      CLEAR: lv_prec.
      IF lv_kpein > 1.
        lv_prec = lv_netpr1 * item_ls-lfimg / lv_kpein.
        lv_prec_unit = lv_netpr1 / lv_kpein.
      ELSE.
        lv_prec = lv_netpr1 * item_ls-lfimg.
        lv_prec_unit = lv_netpr1.
      ENDIF.

    ENDIF.

    IF header_ls-lfart EQ 'ZE01' OR header_ls-lfart EQ 'ZE14' OR l_costo EQ 'X'.

      CLEAR: l_gdatu, l_ukurs, l_fechatc, lv_prec_unit_usd ,  lv_prec_usd, l_dec.

      CONCATENATE data_ls-document_header-wadat_ist+6(2) data_ls-document_header-wadat_ist+4(2) data_ls-document_header-wadat_ist+0(4) INTO l_fechatc.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = l_fechatc
        IMPORTING
          output = l_gdatu.

      SELECT SINGLE ukurs INTO l_ukurs
        FROM tcurr
        WHERE kurst   EQ 'M'
        AND   fcurr   EQ 'CLP'
        AND   tcurr   EQ 'USD'
        AND   gdatu   EQ l_gdatu.


      IF sy-subrc NE 0.
        SELECT SINGLE ukurs INTO l_ukurs
           FROM tcurr
           WHERE kurst   EQ 'M'
           AND   fcurr   EQ 'CLP'
           AND   tcurr   EQ 'USD'
           AND   gdatu   >= l_gdatu.
      ENDIF.


      l_ukurs = 0 - l_ukurs.

      lv_prec_unit_usd = lv_prec_unit * l_ukurs.
      lv_prec_usd      = lv_prec * l_ukurs.

      deta_ls-prcitem = lv_prec_unit_usd.

      CLEAR l_dec.
*      SPLIT deta_ls-prcitem AT '.' INTO deta_ls-prcitem l_dec.
*
*      IF l_dec >= 50.
*        deta_ls-prcitem = deta_ls-prcitem + 1.
*      ENDIF.
      CONDENSE deta_ls-prcitem.

      deta_ls-montoitem = lv_prec_usd.
      CLEAR l_dec.
      SPLIT deta_ls-montoitem AT '.' INTO deta_ls-montoitem l_dec.
      IF l_dec >= 50.
        deta_ls-montoitem = deta_ls-montoitem + 1.
      ENDIF.
      CONDENSE deta_ls-montoitem.

*      PERFORM formateo_montos USING  lv_prec_unit
*                                            'USD'
*                                     CHANGING  deta_ls-prcitem.
*
*      PERFORM formateo_montos USING  lv_prec
*                                        'USD'
*                                 CHANGING  deta_ls-montoitem.
    ELSE.
      PERFORM formateo_montos USING  lv_prec_unit
                                          'CLP'
                                   CHANGING  deta_ls-prcitem.

      PERFORM formateo_montos USING  lv_prec
                                        'CLP'
                                 CHANGING  deta_ls-montoitem.
    ENDIF.


*   Codigos
    CLEAR: cod_ls, cod_lt.
    cod_ls-tpocodigo = 'INT1'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = item_ls-matnr
      IMPORTING
        output = cod_ls-vlrcodigo.

    IF cod_ls-vlrcodigo IS INITIAL.
      cod_ls-vlrcodigo = '0'.
    ENDIF.

*    cod_ls-vlrcodigo   = item_ls-matnr.
    APPEND cod_ls TO cod_lt.

    deta_ls-cdgitem[] = cod_lt[].

    PERFORM format_texto CHANGING deta_ls-nmbitem.   "Ajuste por caracteres especiales
    PERFORM format_texto CHANGING deta_ls-dscitem.   "Ajuste por caracteres especiales

    APPEND deta_ls TO deta_lt.

  ENDLOOP.

  dte_ev-detalle[] = deta_lt[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFERENCIAS_GUIAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&      <-- W_REF_DOC
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM referencias_guias  USING    data_ls   TYPE  edoc_src_data_sd_gi
CHANGING dte_ev    TYPE  zclfel_et_documento
  w_ref_doc STRUCTURE w_ref_doc
  w_sol_nc.


  DATA: header_ls TYPE edoc_vbrkvb.

  CLEAR: header_ls.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.


  IF header_ls-vbtyp CA 'OPMJ'.
*    PERFORM buscar_referencia USING     header_ls
*                                        dte_ev-encabezado-iddoc-tipodte
*
*    CHANGING  w_ref_doc
*              w_sol_nc.

    PERFORM buscar_referencia_guia USING     header_ls
                                        dte_ev-encabezado-iddoc-tipodte

    CHANGING  w_ref_doc
              w_sol_nc.

*

    PERFORM fill_referencia_guia   USING     header_ls
    CHANGING  w_ref_doc_guia
              dte_ev
              w_sol_nc.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUSCAR_REFERENCIA_GUIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> HEADER_LS
*&      --> DTE_EV_ENCABEZADO_IDDOC_TIPODT
*&      <-- W_REF_DOC
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM buscar_referencia_guia  USING     header_ls TYPE edoc_vbrkvb
                                  p_tipodte

CHANGING  w_ref_doc LIKE w_ref_doc
          w_sol_nc.
*  TABLEs t_likp.

  DATA: BEGIN OF t_vbfa OCCURS 0,
          vbeln     LIKE vbfa-vbeln,
          vbtyp_n   LIKE vbfa-vbtyp_n,
          vbelv     LIKE vbfa-vbelv,
          vbtyp_v   LIKE vbfa-vbtyp_v,
          fkdat     LIKE vbrk-fkdat,
          fksto     LIKE vbrk-fksto,
          fecha(10) TYPE c,
        END OF t_vbfa.

  DATA: BEGIN OF t_vbfa1 OCCURS 0,
          vbeln   LIKE vbfa-vbeln,
          vbtyp_n LIKE vbfa-vbtyp_n,
          vbelv   LIKE vbfa-vbelv,
          vbtyp_v LIKE vbfa-vbtyp_v,
          fkdat   LIKE vbrk-fkdat,
          fksto   LIKE vbrk-fksto,
        END OF t_vbfa1.

  DATA: wl_vauf       LIKE vbak-vbeln.
  DATA: lv_pedido     TYPE vbak-vbeln.
  DATA: l_vbtypv TYPE vbtyp_v.
  REFRESH: t_vbfa.
  CLEAR: t_vbfa, w_ref_doc, wl_vauf, l_vbtypv.

  IF p_tipodte EQ '56' OR p_tipodte EQ '056'.
    l_vbtypv = 'L'.
  ELSEIF p_tipodte EQ '033'.
    l_vbtypv = 'J'.
  ELSEIF p_tipodte = '61' OR p_tipodte EQ '061'.
    l_vbtypv = 'K'.
  ELSEIF p_tipodte = '52' OR p_tipodte EQ '052'.
    l_vbtypv = 'C'.
  ENDIF.

* Para obtener orden de compra y referenciar en la guia de despacho.

  IF l_vbtypv EQ 'C'.
    SELECT SINGLE vgbel INTO lv_pedido
      FROM  lips
      WHERE vbeln EQ header_ls-vbeln.


    SELECT SINGLE bstkd bstdk INTO ( w_ref_doc_guia-xblnr, w_ref_doc_guia-fkdat )
      FROM vbkd
      WHERE vbeln EQ lv_pedido.

    IF sy-subrc NE 0.
      SELECT SINGLE ebeln aedat ekorg bsart INTO ( w_ref_doc_guia-xblnr, w_ref_doc_guia-fkdat, w_ref_doc_guia-vkorg, w_ref_doc_guia-auart )
        FROM ekko
        WHERE ebeln EQ lv_pedido.

    ELSE.
      SELECT SINGLE vkorg auart INTO ( w_ref_doc_guia-vkorg, w_ref_doc_guia-auart )
        FROM vbak
        WHERE vbeln EQ lv_pedido.

    ENDIF.

    w_ref_doc_guia-vbeln = lv_pedido.
    w_ref_doc_guia-tidoc = '801'.



  ELSE.  " Se referencia Guia de despacho
*--------------------------------------------------------------------*

*    SELECT vbeln vbtyp_n vbelv vbtyp_v
*    INTO TABLE t_vbfa
*    FROM vbfa
*    WHERE vbeln     = header_ls-vbeln
*    AND vbtyp_v EQ l_vbtypv.
*
*    SORT t_vbfa BY vbelv.
*    DELETE ADJACENT DUPLICATES FROM t_vbfa
*              COMPARING vbelv vbtyp_v.
*
*
*    IF t_vbfa[] IS NOT INITIAL.
*      SELECT vbeln
*           xblnr
*           wadat_ist
*    INTO TABLE t_likp
*    FROM likp FOR ALL ENTRIES IN t_vbfa
*    WHERE vbeln EQ t_vbfa-vbelv.
*    ENDIF.

*--------------------------------------------------------------------*

  ENDIF.

  IF w_ref_doc_guia-xblnr IS INITIAL.
    CLEAR w_ref_doc.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_REFERENCIA_GUIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> HEADER_LS
*&      <-- W_REF_DOC_GUIA
*&      <-- DTE_EV
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM fill_referencia_guia   USING     header_ls TYPE edoc_vbrkvb
CHANGING  w_ref_doc_guia LIKE w_ref_doc_guia
          dte_ev    TYPE  zclfel_et_documento
          w_sol_nc.


  DATA: refer_lt  TYPE zclfel_t_referencia,
        refer_ls  TYPE zclfel_referencia,

        motivo_ls TYPE zclfel_tb_ref.

  DATA: w_vkorg  LIKE vbak-vkorg,
        w_augru  LIKE vbak-augru,
        w_auart  LIKE vbak-auart,
        lv_bstkd TYPE vbkd-bstkd,
        lv_bstdk TYPE vbkd-bstdk.

  DATA: linea_lv  TYPE i.

  CLEAR: refer_lt[], lv_bstkd, lv_bstdk.

  DATA: comwa_ls LIKE vbco3,
        kopf_ls  LIKE vbdkr,

        pos_lt   LIKE TABLE OF vbdpr.



  IF header_ls-vbtyp EQ 'J'.
* Referencia para las guias de despacho
*   --------------------------
    IF NOT w_ref_doc_guia-vbeln IS INITIAL.
      IF w_ref_doc_guia-tidoc IS NOT INITIAL.
        ADD 1 TO linea_lv.
        refer_ls-nrolinref = linea_lv.
*       Código del documento de referencia
        refer_ls-tpodocref = w_ref_doc_guia-tidoc.
*       Indicador global de referencia
        refer_ls-indglobal  = ''.
*       Folio del documento de referencia
        refer_ls-folioref  = w_ref_doc_guia-xblnr.
*       Fecha de emisión del documento de referencia
        CONCATENATE w_ref_doc_guia-fkdat+0(4)
        w_ref_doc_guia-fkdat+4(2)
        w_ref_doc_guia-fkdat+6(2)
        INTO refer_ls-fchref
        SEPARATED BY '-'.
      ENDIF.
    ELSE.
*     Si no tiene referencia se debe poner el indicador global de
*     referencia en '1' y los demás campos como se indican. En teoría
*     el indicador global sirve para informar que se hace referencia
*     a un número grande de documentos. Pero también se puede utilizar
*     para el caso que no podamos determinar referencias

      SELECT SINGLE bstkd bstdk INTO ( lv_bstkd, lv_bstdk )
        FROM vbkd
        WHERE vbeln EQ w_sol_nc.
      IF sy-subrc EQ 0. "JEPL 10.02.2019

        SPLIT lv_bstkd AT '-' INTO refer_ls-tpodocref refer_ls-folioref.

        ADD 1 TO linea_lv.
        refer_ls-nrolinref = linea_lv.
*      refer_ls-tpodocref = '33'.
        refer_ls-indglobal = ''.
*      refer_ls-indglobal = '1'.
*      refer_ls-folioref = '0'.
        CONCATENATE lv_bstdk+0(4) '-'
        lv_bstdk+4(2) '-'
        lv_bstdk+6(2)
        INTO refer_ls-fchref.
      ENDIF.
    ENDIF.

*   Determinar motivo de pedido

    CLEAR: w_vkorg, w_augru, w_auart.

    IF kopf_ls-vbeln_vauf IS INITIAL.
      kopf_ls-vbeln_vauf = w_sol_nc .
    ENDIF.

    SELECT SINGLE vkorg augru auart
    INTO (w_vkorg, w_augru, w_auart)
    FROM vbak
    WHERE vbeln = kopf_ls-vbeln_vauf.

    SELECT SINGLE *
    INTO motivo_ls
    FROM zclfel_tb_ref
    WHERE augru  = w_augru.

    IF header_ls-fkart = 'ZCD1' OR header_ls-fkart = 'ZAA2'.
      refer_ls-codref    = '2'.
      refer_ls-razonref  = 'Corrige Texto'.

    ELSE.
      refer_ls-codref    = motivo_ls-codref.
      refer_ls-razonref  = motivo_ls-razonref.

    ENDIF.

    APPEND refer_ls TO refer_lt.

  ELSEIF header_ls-vbtyp = 'M'.
    CLEAR: refer_ls.
*   Facturas
*   Aquí se pueden indicar otras referencias como ser ordenes
*   de  compra, Guías de Despacho, etc.
*--------------------------------------------------------------------*
    IF header_ls-fkart = 'ZFAP' OR header_ls-fkart = 'ZFAC' OR header_ls-fkart = 'ZREF'.
      IF NOT t_likp IS INITIAL.
        LOOP AT t_likp INTO ls_likp.
          IF ls_likp-xblnr IS NOT INITIAL.
            ADD 1 TO linea_lv.
            refer_ls-nrolinref  = linea_lv.
            refer_ls-tpodocref  = ls_likp-xblnr(3)."???.
            REPLACE ALL OCCURRENCES OF '0' IN refer_ls-tpodocref WITH space.  "
            refer_ls-folioref    = ls_likp-xblnr+4.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = refer_ls-folioref
              IMPORTING
                output = refer_ls-folioref.

            PERFORM formateo_fecha  USING   ls_likp-wadat_ist
             CHANGING  refer_ls-fchref.

            APPEND refer_ls TO refer_lt.
          ENDIF.
        ENDLOOP.
      ENDIF.

******* Orden de compra *****
      ADD 1 TO linea_lv.
      refer_ls-nrolinref  = linea_lv.
      refer_ls-tpodocref  = '801'.
      refer_ls-folioref   = kopf_ls-bstnk.
      PERFORM formateo_fecha  USING   kopf_ls-bstdk
       CHANGING  refer_ls-fchref.
      APPEND refer_ls TO refer_lt.

    ELSE.
*--------------------------------------------------------------------*
      IF NOT w_ref_doc-xblnr IS INITIAL.
        ADD 1 TO linea_lv.
        refer_ls-nrolinref  = linea_lv.
        refer_ls-tpodocref  = w_ref_doc-tidoc.
        refer_ls-folioref    = w_ref_doc-xblnr.

        refer_ls-fchref = w_ref_doc-fecha.
        TRANSLATE refer_ls-fchref USING '.-'.

      ENDIF.

      APPEND refer_ls TO refer_lt.
    ENDIF.
  ENDIF.

  dte_ev-referencia[] = refer_lt[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETALLE_NC_ADM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      --> VBELN_VL
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM detalle_nc_adm  USING    data_ls  TYPE  edoc_src_data_sd_invoice
                       vbeln_vl
              CHANGING dte_ev   TYPE  zclfel_et_documento.


  DATA: deta_lt TYPE zclfel_t_detalle,
        deta_ls TYPE zclfel_detalle,
        cod_lt  TYPE zclfel_cdgitem_t,
        cod_ls  TYPE zclfel_cdgitem.

  DATA: t001_ls LIKE t001.

  DATA: waers_lv TYPE waers.


  DATA: item_lt   TYPE edoc_vbrpvb_tab,
        item_ls   TYPE vbrpvb,
        header_ls TYPE edoc_vbrkvb.

  DATA: lv_total TYPE p DECIMALS 3.


  DATA: linea_lv      LIKE sy-tabix,
        lv_netwr      LIKE vbrk-netwr,
        lv_mtart      TYPE mtart,
*        lv_prec       LIKE vbrk-netwr,
        lv_prec       TYPE p DECIMALS 3,
        lv_unit       LIKE vbap-netpr,
        lv_desc       LIKE konv-kwert,
        lv_desc_char  TYPE char18,
        lv_decimal    TYPE char18,
        lv_porcd      LIKE konv-kbetr,
        lv_conversion LIKE konv-kbetr,
        lv_name       TYPE thead-tdname,
        lv_umrez      LIKE marm-umrez,
        lv_descitem   TYPE char1025,
        caja_lv       TYPE i,
        lv_totl       TYPE p DECIMALS 3.

  DATA: bseg_lt TYPE edoc_bseg_tab,
        bseg_ls TYPE bseg,
        bkpf_ls TYPE edoc_bkpf.

  DATA: ls_konv TYPE prcd_elements.
  DATA: lt_konv TYPE STANDARD TABLE OF prcd_elements.
  DATA: lt_cond TYPE STANDARD TABLE OF zclfel_tb_cond,
        ls_cond LIKE LINE OF lt_cond.

  DATA: id       LIKE  thead-tdid,
        name     LIKE  thead-tdname,
        object   LIKE  thead-tdobject,
        languaje TYPE sy-langu.

  DATA: lines_lt LIKE TABLE OF tline,
        lines_ls LIKE          tline.



  DATA: l_impue  LIKE vbrk-netwr.
  DATA: lv_valor6dec        TYPE p DECIMALS 6.

  SELECT kschl tipo INTO CORRESPONDING FIELDS OF TABLE lt_cond FROM zclfel_tb_cond. "#EC CI_NOWHERE

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.

  SELECT SINGLE *
  INTO t001_ls
  FROM t001
  WHERE bukrs = header_ls-bukrs.


  bseg_lt[]   = data_ls-bseg[].
  bkpf_ls     = data_ls-bkpf.

  item_lt[]   =  data_ls-document_item[].
  REFRESH lt_konv[].
  SELECT * INTO TABLE lt_konv                 "#EC CI_ALL_FIELDS_NEEDED
    FROM prcd_elements
    WHERE knumv EQ header_ls-knumv.


****** Se llama a la funcion para leer el texto de cabecera de NC administrativa y mostrarla en las posiciones. ***


  id      = 'Z015'.
  name    = header_ls-vbeln..
  object  = 'VBBK'.

  IF header_ls-fkart EQ 'ZAA2'.
    languaje = sy-langu.
  ELSE.
    languaje = 'E'.
  ENDIF.


  REFRESH lines_lt.
  CLEAR lines_ls.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = id
      language                = languaje
      name                    = name
      object                  = object
    TABLES
      lines                   = lines_lt
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.


  CLEAR: lines_ls, item_ls.

  READ TABLE item_lt INTO item_ls INDEX 1.
  IF lines_lt[] IS NOT INITIAL.

    LOOP AT lines_lt INTO lines_ls.

      CLEAR: deta_ls.

*   Nro Linea de Detalle
      ADD 1 TO linea_lv.
      deta_ls-nrolindet = linea_lv.

      PERFORM formateo_nlin CHANGING deta_ls-nrolindet.
*    Indicador Exención
      IF dte_ev-encabezado-iddoc-tipodte EQ '033'.
        deta_ls-indexe = '0'.
      ELSEIF dte_ev-encabezado-iddoc-tipodte EQ '034'.
        deta_ls-indexe = '1'.
      ENDIF.

*   Nombre del Item

      deta_ls-nmbitem = lines_ls-tdline.

      IF deta_ls-nmbitem IS INITIAL.
        deta_ls-nmbitem = '/'.
      ENDIF.

      deta_ls-qtyitem  = '1'.
      deta_ls-unmditem = 'UN'.
*   Monto del Item
      CLEAR: lv_netwr.

      lv_netwr = item_ls-netwr.
      PERFORM formateo_montos USING     lv_netwr
                                        header_ls-waerk
                              CHANGING  deta_ls-montoitem.


*   Precio del Item
      CLEAR: lv_prec.

      lv_prec = item_ls-kzwi1.

      CLEAR: lv_totl.

      PERFORM formateo_montos USING     lv_prec
                                        bkpf_ls-waers
      CHANGING  deta_ls-prcitem.

* Precio unitario con impuestos incluidos D63

      PERFORM formateo_montos USING item_ls-netwr
            header_ls-waerk
      CHANGING  deta_ls-totalitem.
      CONDENSE deta_ls-totalitem.

*   Codigos
*    CLEAR: cod_ls, cod_lt.
*    cod_ls-tpocodigo = 'INT1'.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = item_ls-matnr
*      IMPORTING
*        output = cod_ls-vlrcodigo.
*
*    cod_ls-vlrcodigo = ' '.
*    APPEND cod_ls TO cod_lt.
*
*    deta_ls-cdgitem[] = cod_lt[].

      APPEND deta_ls TO deta_lt.


    ENDLOOP.

  ELSE.

    IF header_ls-fkart EQ 'ZAA2'.
      MESSAGE 'Debe ingresar texto de cabecera' TYPE 'E'.
    ELSE.
      MESSAGE 'Debe ingresar texto en cabecera en idioma INGLES' TYPE 'E'.
    ENDIF.


  ENDIF.



*  LOOP AT item_lt INTO item_ls WHERE posnr EQ '000010'.
**    CLEAR: deta_ls.
**
***   Nro Linea de Detalle
**    ADD 1 TO linea_lv.
**    deta_ls-nrolindet = linea_lv.
**
**    PERFORM formateo_nlin CHANGING deta_ls-nrolindet.
**
***   Indicador Exención
**    IF dte_ev-encabezado-iddoc-tipodte EQ '033'.
**      deta_ls-indexe = '0'.
**    ELSEIF dte_ev-encabezado-iddoc-tipodte EQ '034'.
**      deta_ls-indexe = '1'.
**    ENDIF.
*
*
**   Nombre del Item
*
*    CLEAR lv_descitem.
*    CLEAR lv_name.
**    CONCATENATE item_ls-vbeln item_ls-posnr INTO lv_name RESPECTING BLANKS.
*    lv_name = item_ls-vbeln.
*    PERFORM get_text  USING    lv_name
*          'Z015'
*          'VBBK'
*    CHANGING lv_descitem.
*
*    deta_ls-nmbitem = lv_descitem+0(80).
*
*    deta_ls-dscitem = lv_descitem+80(918).
*
*    IF lv_descitem IS INITIAL.
*      deta_ls-nmbitem = item_ls-arktx.
*    ENDIF.
*
*
*    deta_ls-qtyitem  = '1'.
*    deta_ls-unmditem = 'UN'.
**   Monto del Item
*    CLEAR: lv_netwr.
*
*    lv_netwr = item_ls-netwr.
*    PERFORM formateo_montos USING     lv_netwr
*                                      header_ls-waerk
*                            CHANGING  deta_ls-montoitem.
*
*
**   Precio del Item
*    CLEAR: lv_prec.
*
*    lv_prec = item_ls-kzwi1.
*
*    CLEAR: lv_totl.
*
*    PERFORM formateo_montos USING     lv_prec
*                                      bkpf_ls-waers
*    CHANGING  deta_ls-prcitem.
*
** Precio unitario con impuestos incluidos D63
*
*    PERFORM formateo_montos USING item_ls-netwr
*          header_ls-waerk
*    CHANGING  deta_ls-totalitem.
*    CONDENSE deta_ls-totalitem.
*
**   Codigos
**    CLEAR: cod_ls, cod_lt.
**    cod_ls-tpocodigo = 'INT1'.
**
**    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
**      EXPORTING
**        input  = item_ls-matnr
**      IMPORTING
**        output = cod_ls-vlrcodigo.
**
**    cod_ls-vlrcodigo = ' '.
**    APPEND cod_ls TO cod_lt.
**
**    deta_ls-cdgitem[] = cod_lt[].
*
*    APPEND deta_ls TO deta_lt.
*
*  ENDLOOP.

  dte_ev-detalle[] = deta_lt[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CABECERA_COMPRA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&      <-- GV_BOLETA
*&---------------------------------------------------------------------*
FORM cabecera_compra  USING    data_ls TYPE  edoc_src_data_invoice_verif
                               p_asigna TYPE string
CHANGING dte_ev  TYPE  zclfel_et_documento
  gv_boleta.

  DATA: cab_ls    TYPE zclfel_encabezado,
        header_ls TYPE rbkp,

        cond_lt   TYPE edoc_komv_tab,
        cond_ls   LIKE komv,

        item_lt   TYPE edoc_rseg_tab,
        item_ls   LIKE rseg,

        bset_lt   TYPE edoc_bset_tab,
        bset_ls   TYPE bset,
        bkpf_ls   TYPE edoc_bkpf,

        tax_lt    TYPE edoc_rbtx_tab,
        tax_ls    TYPE rbtx.


  DATA: dte_lv         LIKE edocldteacc-dte_type,
        l_ztag1        LIKE t052-ztag1,
        l_fchven       LIKE sy-datum,
        t001_ls        LIKE t001,
        adrc_ls        LIKE adrc,
        adr6_ls        LIKE adr6,
        lfa1_ls        LIKE lfa1,
        v_montc        TYPE c LENGTH 15,
        l_neto         TYPE rmwwr,
        l_totalb       TYPE c LENGTH 18,
        l_ret          TYPE fwstev,
        l_retclp       TYPE char15,
        l_retencion    TYPE c LENGTH 15,
        l_retencionclp TYPE char15,
        l_kursf        LIKE rbkp-kursf,
        l_ukurs        TYPE ukurs_curr,
        l_gdatu        TYPE gdatu_inv,
        l_fechatc      TYPE char8,
        l_totalclp     LIKE vbrk-netwr,
        l_ivaclp       LIKE vbrk-netwr,
        l_dec          TYPE char4,
        l_netwrclp     LIKE vbrk-netwr,
        l_zterm        TYPE ekko-zterm.

  DATA: c_mwskz  TYPE rf82t-mwskz VALUE 'D1',
        it_ftaxp TYPE TABLE OF ftaxp,
        ix_ftaxp TYPE ftaxp.

  DATA: imp_lt TYPE zclfel_t_imptos,
        imp_ls TYPE zclfel_imptoreten.

  DATA: l_cantidad LIKE inri-quantity.
  DATA: l_numero   TYPE string.
  DATA: l_retorno  TYPE inri-returncode.
  DATA: l_awkey    TYPE awkey.
  DATA: l_xblnr    TYPE xblnr.

  CLEAR: cab_ls, header_ls.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.
  MOVE-CORRESPONDING data_ls-document_item   TO item_lt.
  MOVE-CORRESPONDING data_ls-tax_data        TO tax_lt.

***EMISOR ***

  IF p_asigna EQ 'X'.

    CLEAR: l_numero, l_cantidad, l_retorno.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '46'
        object                  = 'EDOC_CL046'
        quantity                = '1'
        subobject               = header_ls-bukrs
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = l_numero
        quantity                = l_cantidad
        returncode              = l_retorno
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE 'Error al obtener folio revisar rango de numeros' TYPE 'E'.
    ELSE.
      CLEAR l_xblnr.
      l_xblnr = l_numero.
      CONCATENATE '046-' l_xblnr INTO header_ls-xblnr.

      CALL FUNCTION 'ZCLSD_FM_UPDATE_XBLNR'
        EXPORTING
*         I_VBELN    =
*         I_BELNR    =
          i_belnr_mm = header_ls-belnr
          i_bukrs    = header_ls-bukrs
          i_gjahr    = header_ls-gjahr
          i_xblnr    = header_ls-xblnr
          i_tipodte  = '46'
*         I_LOG      =
*         I_CODIGO   =
*       EXCEPTIONS
*         NOT_SUCESS = 1
*         OTHERS     = 2
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
  ELSE.

    CONCATENATE  header_ls-belnr header_ls-gjahr INTO l_awkey RESPECTING BLANKS.

    SELECT SINGLE xblnr INTO header_ls-xblnr
      FROM bkpf
      WHERE bukrs EQ header_ls-bukrs
      AND awtyp EQ 'RMRP'
      AND awkey EQ l_awkey.

  ENDIF.

* Tipo Documento
  cab_ls-iddoc-tipodte = header_ls-xblnr+0(3).
  dte_lv          = header_ls-xblnr+0(3).

* Folio Documento
  cab_ls-iddoc-folio  = header_ls-xblnr+4.

* Fecha de emisión
  PERFORM formateo_fecha  USING    data_ls-document_header-budat
  CHANGING  cab_ls-iddoc-fchemis.

* Forma de pago

  CLEAR: item_ls, l_zterm.
  READ TABLE data_ls-document_item INTO item_ls INDEX 1.

  SELECT SINGLE zterm INTO l_zterm
    FROM ekko
    WHERE ebeln EQ item_ls-ebeln.

  SELECT SINGLE fmapago INTO cab_ls-iddoc-fmapago
    FROM zsd_forma_pago
    WHERE zterm EQ l_zterm.

  IF cab_ls-iddoc-fmapago IS INITIAL.
    cab_ls-iddoc-fmapago = '1'.
  ENDIF.

* Fecha de vencimiento
  SELECT SINGLE ztag1
  INTO l_ztag1
  FROM t052
  WHERE zterm = l_zterm.

  l_fchven = header_ls-budat + l_ztag1.

  PERFORM formateo_fecha  USING     l_fchven
  CHANGING  cab_ls-iddoc-fchvenc.

* Rut Emisor
  SELECT SINGLE paval
  INTO cab_ls-emisor-rutemisor
  FROM t001z
  WHERE bukrs EQ header_ls-bukrs  AND
  party EQ 'RESDAT'.

  IF sy-subrc NE 0.
    SELECT SINGLE paval
      INTO cab_ls-emisor-rutemisor
      FROM t001z
      WHERE bukrs EQ header_ls-bukrs  AND
      party EQ 'TAXNR'.
  ENDIF.

  CLEAR: adrc_ls.
  SELECT SINGLE *
  INTO t001_ls
  FROM t001
  WHERE bukrs = header_ls-bukrs.

  IF sy-subrc = 0.
    SELECT SINGLE *                           "#EC CI_ALL_FIELDS_NEEDED
    INTO adrc_ls
    FROM adrc
    WHERE addrnumber = t001_ls-adrnr.
  ENDIF.

* Razón Social

  cab_ls-emisor-rznsoc    = adrc_ls-name1.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-emisor-rznsoc WITH 'y'.

***
* Giro emisor
  cab_ls-emisor-giroemis = adrc_ls-name3.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-emisor-giroemis WITH 'y'.

* Dirección origen
  CONCATENATE adrc_ls-street
  adrc_ls-house_num1
  INTO cab_ls-emisor-dirorigen
  SEPARATED BY space.

* Comuna origen

  cab_ls-emisor-ciudadorigen  = adrc_ls-city1.

* Ciudad origen

  cab_ls-emisor-cmnaorigen    = adrc_ls-city2.

  IF cab_ls-emisor-cmnaorigen IS INITIAL.
    cab_ls-emisor-cmnaorigen = adrc_ls-city1.
  ENDIF.

* Codigo sucursal
  CLEAR item_ls.
  READ TABLE item_lt INTO item_ls INDEX 1.
  cab_ls-emisor-sucursal = item_ls-werks.

* Codigo SII Sucursal

  SELECT SINGLE acteco INTO cab_ls-emisor-cdgsiisucur
    FROM zsd_sucursales
    WHERE werks EQ item_ls-werks.

* Sucursal
  READ TABLE data_ls-document_item INTO item_ls INDEX 1.
  IF sy-subrc EQ 0 AND cab_ls-iddoc-tipodte NE '039' AND cab_ls-iddoc-tipodte NE '040' AND cab_ls-iddoc-tipodte NE '042'.
    cab_ls-emisor-sucursal = item_ls-werks.
  ELSE.
    cab_ls-emisor-sucursal = ''.
  ENDIF.

* Código del vendedor
  cab_ls-emisor-cdgvendedor = 0.


****** RECEPTOR ************


* Rut receptor
  CLEAR: lfa1_ls, adr6_ls.
  SELECT SINGLE *                             "#EC CI_ALL_FIELDS_NEEDED
  INTO lfa1_ls
  FROM lfa1
  WHERE lifnr = header_ls-lifnr.

  SELECT SINGLE *                             "#EC CI_ALL_FIELDS_NEEDED
     INTO adr6_ls
     FROM adr6
     WHERE addrnumber = lfa1_ls-adrnr.

  TRANSLATE lfa1_ls-stcd1 USING '. '.
  CONDENSE lfa1_ls-stcd1 NO-GAPS.

  cab_ls-receptor-rutrecep  = lfa1_ls-stcd1.

* Código interno receptor
  cab_ls-receptor-cdgintrecep = header_ls-lifnr.

* Razón social receptor
  CONCATENATE lfa1_ls-name1
  lfa1_ls-name2
  INTO cab_ls-receptor-rznsocrecep
  SEPARATED BY space.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-receptor-rznsocrecep WITH 'y'.

  " Giro receptor

  CONCATENATE lfa1_ls-name3 lfa1_ls-name4 INTO cab_ls-receptor-girorecep SEPARATED BY space.

  IF cab_ls-receptor-girorecep EQ space.
    cab_ls-receptor-girorecep = 'Sin Informacion'.
  ENDIF.

  REPLACE ALL OCCURRENCES OF '&' IN cab_ls-receptor-girorecep WITH 'y'.

* Direccion Receptor

  cab_ls-receptor-dirrecep    = lfa1_ls-stras.

* Comuna Receptor
  cab_ls-receptor-cmnarecep   = lfa1_ls-ort02.

* Ciudad Receptor
  cab_ls-receptor-ciudadrecep = lfa1_ls-ort01.

  IF cab_ls-receptor-ciudadrecep IS INITIAL.
    cab_ls-receptor-ciudadrecep = lfa1_ls-ort02.
  ENDIF.


******* Totales *****

*----------------------------------------------------------------
*  Totales
*----------------------------------------------------------------

  READ TABLE tax_lt INTO tax_ls WITH KEY mwskz = 'C1'.

  IF sy-subrc NE 0.
    READ TABLE tax_lt INTO tax_ls WITH KEY mwskz = 'C2'.
  ENDIF.

  IF header_ls-waers NE 'CLP'.


    " Tipo de cambio
    CLEAR: l_gdatu, l_ukurs, l_fechatc.

    CONCATENATE data_ls-document_header-budat+6(2) data_ls-document_header-budat+4(2) data_ls-document_header-budat+0(4) INTO l_fechatc.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = l_fechatc
      IMPORTING
        output = l_gdatu.

    SELECT SINGLE ukurs INTO l_ukurs
      FROM tcurr
      WHERE kurst   EQ 'M'
      AND   fcurr   EQ 'CLP'
      AND   tcurr   EQ header_ls-waers
      AND   gdatu   EQ l_gdatu.

    l_ukurs = 0 - l_ukurs.

    " IVA
    l_ivaclp = tax_ls-wmwst * l_ukurs.

    cab_ls-totales-iva = l_ivaclp.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-iva  AT '.' INTO  cab_ls-totales-iva  l_dec.

    IF l_dec >= 50.
      cab_ls-totales-iva  = cab_ls-totales-iva  + 1.
      CONDENSE cab_ls-totales-iva.
    ENDIF.

    cab_ls-totales-tasaiva = '19.0'.

    " Tipo de cambio
    cab_ls-totales-tpo_cambio = l_ukurs.
    CONDENSE cab_ls-totales-tpo_cambio.
    cab_ls-totales-tpo_cambio = cab_ls-totales-tpo_cambio+0(6).


    cab_ls-totales-mntexe = 0.

    "Total b

    CLEAR: l_dec, l_totalb, l_ret, l_retclp, l_retencionclp.
    l_totalb = cab_ls-totales-mntneto + cab_ls-totales-iva.
    CONDENSE l_totalb NO-GAPS.

    l_ret = ( header_ls-wmwst1 -  tax_ls-hwste ) * -1.

    l_retclp = l_ret * l_ukurs.

    SPLIT  l_retclp  AT '.' INTO  l_retclp  l_dec.
    IF l_dec >= 50.
      l_retclp  = l_retclp  + 1.
      CONDENSE l_retclp.
    ELSE.
      CONDENSE l_retclp.
    ENDIF.

    l_retencionclp = header_ls-wmwst1 * l_ukurs.
    CLEAR l_dec.
    SPLIT  l_retencionclp  AT '.' INTO  l_retencionclp  l_dec.
    IF l_dec >= 50.
      l_retencionclp  = l_retencionclp  + 1.
      CONDENSE l_retencionclp.
    ELSE.
      CONDENSE l_retencionclp.
    ENDIF.

    l_retencion = l_retencionclp.

    "TOTAL

    l_totalclp = header_ls-rmwwr * l_ukurs.

    cab_ls-totales-mnttotal = l_totalclp - l_retencion.

    CLEAR l_dec.
    SPLIT  cab_ls-totales-mnttotal  AT '.' INTO  cab_ls-totales-mnttotal  l_dec.

    IF l_dec >= 50.
      cab_ls-totales-mnttotal  = cab_ls-totales-mnttotal  + 1.
      CONDENSE cab_ls-totales-mnttotal.
    ENDIF.

    "NETO
    l_neto =  cab_ls-totales-mnttotal + cab_ls-totales-iva - l_retencion.
    cab_ls-totales-mntneto = l_neto.
    CLEAR l_dec.
    SPLIT  cab_ls-totales-mntneto  AT '.' INTO  cab_ls-totales-mntneto  l_dec.

    IF l_dec >= 50.
      cab_ls-totales-mntneto  = cab_ls-totales-mntneto  + 1.
      CONDENSE cab_ls-totales-mntneto.
    ELSE.
      CONDENSE cab_ls-totales-mntneto.
    ENDIF.

  ELSE.
    " IVA
    cab_ls-totales-tasaiva = '19.0'.

    PERFORM formateo_montos USING     tax_ls-wmwst
              header_ls-waers
        CHANGING  cab_ls-totales-iva.
    CONDENSE cab_ls-totales-iva.

    "IVA No retenido

    cab_ls-totales-ivanoret = '0'.

    "Tipo de cambio
    CLEAR l_kursf.
    l_kursf = header_ls-kursf.
    l_kursf = 0 - l_kursf.

    PERFORM formateo_montos USING     l_kursf
             header_ls-waers
       CHANGING  cab_ls-totales-tpo_cambio.
    CONDENSE cab_ls-totales-tpo_cambio.

    cab_ls-totales-tpo_cambio = cab_ls-totales-tpo_cambio+0(6).

    cab_ls-totales-mntexe = 0.

    "Total b
    l_totalb = cab_ls-totales-mntneto + cab_ls-totales-iva.
    CONDENSE l_totalb NO-GAPS.
    l_ret = ( header_ls-wmwst1 -  tax_ls-hwste ) * -1.

    PERFORM formateo_montos USING   header_ls-wmwst1
           header_ls-waers
     CHANGING  l_retencion.

    "TOTAL

    PERFORM formateo_montos USING    header_ls-rmwwr
            header_ls-waers
      CHANGING  cab_ls-totales-mnttotal.


    cab_ls-totales-mnttotal = cab_ls-totales-mnttotal - l_retencion.
    CONDENSE cab_ls-totales-mnttotal.

    "NETO

    cab_ls-totales-mntneto = cab_ls-totales-mnttotal + cab_ls-totales-iva - l_retencion.
    CONDENSE cab_ls-totales-mntneto.

    cab_ls-totales-mntexe = 0.





  ENDIF.


  "TASA IVA
  REFRESH it_ftaxp.
  CLEAR   ix_ftaxp.

  CALL FUNCTION 'GET_TAX_PERCENTAGE'
    EXPORTING
      aland   = 'CL'
      datab   = header_ls-bldat
      mwskz   = tax_ls-mwskz
      txjcd   = space
    TABLES
      t_ftaxp = it_ftaxp.

  READ TABLE it_ftaxp INTO ix_ftaxp INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM formateo_tasa USING ix_ftaxp-kbetr CHANGING cab_ls-totales-tasaiva.
  ENDIF.

********* impuestos retenidos ****

  imp_ls-tipoimp  = '15'.
  imp_ls-tasaimp  = cab_ls-totales-tasaiva.
  imp_ls-montoimp = l_retencion.

  APPEND imp_ls TO imp_lt.

  cab_ls-totales-imptoreten[] = imp_lt[].

  dte_ev-encabezado = cab_ls.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETALLE_COMPRAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      --> VBELN_VL
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM detalle_compras  USING    data_ls  TYPE  edoc_src_data_invoice_verif
                       vbeln_vl
              CHANGING dte_ev   TYPE  zclfel_et_documento.


  DATA: deta_lt TYPE zclfel_t_detalle,
        deta_ls TYPE zclfel_detalle,
        tax_lt  TYPE edoc_rbtx_tab,
        tax_ls  TYPE rbtx,
        cod_lt  TYPE zclfel_cdgitem_t,
        cod_ls  TYPE zclfel_cdgitem.

  DATA: t001_ls LIKE t001.

  DATA: waers_lv TYPE waers.


  DATA: item_lt     TYPE edoc_rseg_tab,
        item_ls     LIKE rseg,
        header_ls   TYPE rbkp,
        lv_mtart    TYPE mtart,
        lv_arktx    TYPE makt-maktx,
        lv_prec     TYPE p DECIMALS 3,
        lv_precclp  TYPE p DECIMALS 3,
        lv_tot      TYPE p DECIMALS 3,
        lv_totclp   TYPE p DECIMALS 3,
        lv_iva      TYPE p DECIMALS 3,
        l_netpr     TYPE ekpo-netpr,
        l_dec       TYPE char4,
        l_ukurs     TYPE ukurs_curr,
        l_gdatu     TYPE gdatu_inv,
        l_fechatc   TYPE char8,
        lv_descitem TYPE char1025,
        lv_name     TYPE thead-tdname.

  DATA: lv_total TYPE p DECIMALS 3.
  DATA: linea_lv LIKE sy-tabix.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.
  MOVE-CORRESPONDING data_ls-document_item   TO item_lt.
  MOVE-CORRESPONDING data_ls-tax_data        TO tax_lt.

  LOOP AT item_lt INTO item_ls.
    CLEAR: deta_ls.
*   Nro Linea de Detalle
    ADD 1 TO linea_lv.
    deta_ls-nrolindet = linea_lv.

    PERFORM formateo_nlin CHANGING deta_ls-nrolindet.

*   Indicador Exención
    IF dte_ev-encabezado-iddoc-tipodte EQ '033'.
      deta_ls-indexe = '0'.
    ELSEIF dte_ev-encabezado-iddoc-tipodte EQ '034'.
      deta_ls-indexe = '1'.
    ENDIF.

*   Nombre del Item
    CLEAR: lv_mtart, lv_arktx.
    SELECT SINGLE maktx INTO lv_arktx
      FROM makt
      WHERE matnr EQ item_ls-matnr.

    deta_ls-nmbitem = lv_arktx.

*   Cantidad del Item y Unidad de medida
    PERFORM formateo_cantidad USING     item_ls-menge
          item_ls-bstme
    CHANGING  deta_ls-qtyitem
      deta_ls-unmditem.

    CLEAR tax_ls.
    READ TABLE tax_lt INTO tax_ls WITH KEY buzei = item_ls-buzei.


    IF sy-subrc EQ 0.

      IF data_ls-document_header-waers NE 'CLP'.

        " Tipo de cambio
        CLEAR: l_gdatu, l_ukurs, l_fechatc.

        CONCATENATE data_ls-document_header-budat+6(2) data_ls-document_header-budat+4(2) data_ls-document_header-budat+0(4) INTO l_fechatc.

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            input  = l_fechatc
          IMPORTING
            output = l_gdatu.

        SELECT SINGLE ukurs INTO l_ukurs
          FROM tcurr
          WHERE kurst   EQ 'M'
          AND   fcurr   EQ 'CLP'
          AND   tcurr   EQ header_ls-waers
          AND   gdatu   EQ l_gdatu.

        l_ukurs = 0 - l_ukurs.

        CLEAR l_netpr.
        SELECT SINGLE netpr INTO l_netpr
          FROM ekpo
          WHERE ebeln EQ item_ls-ebeln
          AND   ebelp EQ item_ls-ebelp.


        lv_precclp = l_netpr * l_ukurs.

        deta_ls-prcitem = lv_precclp.
        CONDENSE deta_ls-prcitem.

        lv_totclp = tax_ls-fwbas * l_ukurs.
        deta_ls-totalitem = lv_totclp.

        CLEAR l_dec.
        SPLIT  deta_ls-totalitem  AT '.' INTO  deta_ls-totalitem  l_dec.

        IF l_dec >= 50.
          deta_ls-totalitem  = deta_ls-totalitem  + 1.
          CONDENSE deta_ls-totalitem.
        ELSE.
          CONDENSE deta_ls-totalitem.
        ENDIF.

      ELSE.

        CLEAR l_netpr.
        SELECT SINGLE netpr INTO l_netpr
          FROM ekpo
          WHERE ebeln EQ item_ls-ebeln
          AND   ebelp EQ item_ls-ebelp.

        PERFORM formateo_montos USING l_netpr
                                      header_ls-waers
         CHANGING deta_ls-prcitem.

        CONDENSE  deta_ls-prcitem.

        lv_tot = tax_ls-fwbas.

        PERFORM formateo_montos USING lv_tot
              header_ls-waers
        CHANGING  deta_ls-totalitem.

        deta_ls-totalitem = deta_ls-totalitem / 10.

        CONDENSE deta_ls-totalitem.
      ENDIF.

    ENDIF.

*   Codigos
    CLEAR: cod_ls, cod_lt.
    cod_ls-tpocodigo = 'INT1'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = item_ls-matnr
      IMPORTING
        output = cod_ls-vlrcodigo.

    APPEND cod_ls TO cod_lt.

    deta_ls-cdgitem[] = cod_lt[].

    APPEND deta_ls TO deta_lt.

  ENDLOOP.

  dte_ev-detalle[] = deta_lt[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFERENCIAS_COMPRAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      <-- DTE_EV
*&      <-- W_REF_DOC
*&      <-- W_SOL_NC
*&---------------------------------------------------------------------*
FORM referencias_compras  USING    data_ls   TYPE  edoc_src_data_invoice_verif
CHANGING dte_ev    TYPE  zclfel_et_documento
  w_ref_doc STRUCTURE w_ref_doc
  w_sol_nc.

  DATA: header_ls   TYPE rbkp.
  DATA: item_lt TYPE edoc_rseg_tab,
        item_ls LIKE rseg.

  DATA: refer_lt    TYPE zclfel_t_referencia,
        refer_ls    TYPE zclfel_referencia,
        lv_fecha_oc TYPE ekko-bedat,
        linea_lv    TYPE i.

  CLEAR: header_ls.

  MOVE-CORRESPONDING data_ls-document_header TO header_ls.
  MOVE-CORRESPONDING data_ls-document_item   TO item_lt.

  CLEAR: item_ls, lv_fecha_oc, linea_lv.
  READ TABLE item_lt INTO item_ls INDEX 1.

  SELECT SINGLE bedat INTO lv_fecha_oc
    FROM ekko
    WHERE ebeln EQ item_ls-ebeln.

  ADD 1 TO linea_lv.
  refer_ls-nrolinref  = linea_lv.
  refer_ls-tpodocref  = '801'.
  refer_ls-folioref   = item_ls-ebeln.
  PERFORM formateo_fecha  USING   lv_fecha_oc
  CHANGING  refer_ls-fchref.
  APPEND refer_ls TO refer_lt.

  dte_ev-referencia[] = refer_lt[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_TEXT_DELIV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_NAME
*&      --> P_
*&      --> P_
*&      <-- LV_DESCITEM
*&---------------------------------------------------------------------*
FORM get_text_deliv  USING p_name
      p_id
      p_object
      p_langu
CHANGING p_text.

  DATA: id       LIKE  thead-tdid,
        name     LIKE  thead-tdname,
        object   LIKE  thead-tdobject,
        languaje TYPE sy-langu.

  DATA: lines_lt LIKE TABLE OF tline,
        lines_ls LIKE          tline.

  id       = p_id.
  name     = p_name.
  object   = p_object.
  languaje = p_langu.

  CLEAR: p_text.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = id
      language                = languaje
      name                    = name
      object                  = object
    TABLES
      lines                   = lines_lt
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  CLEAR lines_ls.

  LOOP AT lines_lt INTO lines_ls.
    IF sy-tabix = 1.
      p_text =  lines_ls-tdline.
    ELSE.
      CONCATENATE p_text
      lines_ls-tdline
      INTO p_text
      SEPARATED BY space.
    ENDIF.
  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form FORMAT_TEXTO
*&---------------------------------------------------------------------*
*& Eliminar caracteres especiales de los textos
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
FORM format_texto CHANGING p_texto.

  DATA lv_format TYPE c LENGTH 300.
  lv_format = p_texto.

  REPLACE ALL OCCURRENCES OF ';' IN lv_format WITH '-'.
  REPLACE ALL OCCURRENCES OF '|' IN lv_format WITH '-'.

  p_texto = lv_format.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DETALLE_DELIV_CONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA_LS
*&      --> VBELN_VL
*&      <-- DTE_EV
*&---------------------------------------------------------------------*
FORM detalle_deliv_cons  USING    data_ls  TYPE  edoc_src_data_sd_gi
                                            vbeln_vl
                    CHANGING dte_ev   TYPE  zclfel_et_documento.


  TYPES: BEGIN OF ty_agrupa
     , vbeln  TYPE vbeln
     , posnr  TYPE posnr
     , pstyv  TYPE pstyv
     , matnr  TYPE matnr
     , arktx  TYPE arktx
     , uecha  TYPE uecha
     , lfimg  TYPE lfimg
     , vrkme  TYPE vrkme
     , ntgew  TYPE ntgew
     , brgew  TYPE brgew
     , gewei  TYPE gewei
     , vgbel  TYPE vgbel
     , vgpos  TYPE vgpos
     , cajas  TYPE i
     , agrupa TYPE string
     ,
  END OF ty_agrupa.

  DATA: lt_agrupa  TYPE STANDARD TABLE OF ty_agrupa.
  DATA: lt_agrupa2 TYPE STANDARD TABLE OF ty_agrupa.
  DATA: lt_zvmmmatcharc TYPE STANDARD TABLE OF zvmmmatcharc.

  DATA: ls_zvmmmatcharc TYPE zvmmmatcharc.
  DATA: ls_agrupa  TYPE ty_agrupa.
  DATA: ls_agrupa2 TYPE ty_agrupa.


  DATA: deta_lt  TYPE zclfel_t_detalle,
        deta_ls  TYPE zclfel_detalle,
        matkg_lt TYPE STANDARD TABLE OF zclfel_mat_kg,
        matkg_ls TYPE zclfel_mat_kg,
        cod_lt   TYPE zclfel_cdgitem_t,
        cod_ls   TYPE zclfel_cdgitem.

  DATA: t001_ls LIKE t001.

  DATA: waers_lv TYPE waers.

  DATA: item_lt   TYPE edoc_lipsvb_tab,
        item_ls   TYPE lipsvb,
        item2_ls  TYPE lipsvb,
        header_ls TYPE edoc_likp,
        docu_flow TYPE vbfa,
        ls_vbap   TYPE vbap.

  DATA: linea_lv         LIKE sy-tabix,
        lv_netwr         LIKE vbrk-netwr,
        lv_netpr1        TYPE vbap-netpr,
        lv_knumv         TYPE vbak-knumv,
        lv_mwsbp         TYPE vbap-mwsbp,
        lv_sdabw         TYPE vbkd-sdabw,
        l_kbetr          LIKE konv-kbetr,
        lv_prec          LIKE vbrk-netwr,
        lv_prec_usd      LIKE vbrk-netwr,
        lv_prec_unit     LIKE vbrk-netwr,
        lv_prec_unit_usd LIKE vbrk-netwr,
        l_dec            TYPE char4,
        l_dec2           TYPE char2,
        lv_umrez         LIKE marm-umrez,
        lv_kpein         TYPE kpein,
        lv_name          TYPE thead-tdname,
        lv_descitem      TYPE char1025,
        l_ukurs          TYPE ukurs_curr,
        l_gdatu          TYPE gdatu_inv,
        l_fechatc        TYPE char8,
        l_cantidad       TYPE i,
        l_cajas          TYPE i,
        l_cantidad_e     TYPE lfimg,
        l_mtart          TYPE mtart.



  MOVE-CORRESPONDING data_ls-document_header TO header_ls.


**--------------------------------------------------------------------*
**--------------------------------------------------------------------*

  REFRESH  matkg_lt[].
  SELECT * INTO TABLE matkg_lt
    FROM zclfel_mat_kg.


  IF data_ls-document_item[] IS NOT INITIAL.

    REFRESH lt_zvmmmatcharc[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE  lt_zvmmmatcharc
      FROM zvmmmatcharc FOR ALL ENTRIES IN data_ls-document_item
      WHERE matnr EQ data_ls-document_item-matnr.
  ENDIF.

  CLEAR item_ls.
  LOOP AT data_ls-document_item INTO item_ls.
    CLEAR ls_agrupa.
    ls_agrupa-vbeln  = item_ls-vbeln.
    ls_agrupa-posnr  = item_ls-posnr.
    ls_agrupa-matnr  = item_ls-matnr.
    ls_agrupa-arktx  = item_ls-arktx.
    ls_agrupa-uecha  = item_ls-uecha.
    ls_agrupa-lfimg  = item_ls-lfimg.
    ls_agrupa-vrkme  = item_ls-vrkme.
    ls_agrupa-ntgew  = item_ls-ntgew.
    ls_agrupa-vgbel  = item_ls-vgbel.
    ls_agrupa-vgpos  = item_ls-vgpos.

    REFRESH t_vepo[].
    CLEAR: t_vepo, l_cantidad.

    SELECT venum vepos vbeln posnr INTO CORRESPONDING FIELDS OF TABLE t_vepo "#EC CI_NOFIELD
      FROM vepo
      WHERE vbeln EQ ls_agrupa-vbeln
      AND   matnr EQ ls_agrupa-matnr
      AND   posnr EQ ls_agrupa-posnr.

    DESCRIBE TABLE t_vepo LINES l_cantidad.
    ls_agrupa-cajas  = l_cantidad.


    CLEAR ls_zvmmmatcharc.
    READ TABLE lt_zvmmmatcharc INTO ls_zvmmmatcharc WITH KEY matnr = item_ls-matnr.
    IF sy-subrc EQ 0.
      CONCATENATE ls_zvmmmatcharc-familia_atwtb "Familia
              ls_zvmmmatcharc-especie_atwtb "Especie
              ls_zvmmmatcharc-proceso_atwtb "Proceso
              ls_zvmmmatcharc-preparacion_1_atwtb "Preparacion 1
              ls_zvmmmatcharc-preparacion_2_atwtb "Preparacion 2
              INTO ls_agrupa-agrupa SEPARATED BY space.
    ENDIF.
    APPEND ls_agrupa TO lt_agrupa.
  ENDLOOP.


  CLEAR ls_agrupa.
  LOOP AT lt_agrupa INTO ls_agrupa.

    CLEAR ls_agrupa2.
    READ TABLE lt_agrupa2 INTO ls_agrupa2 WITH KEY agrupa = ls_agrupa-agrupa.

    IF sy-subrc EQ 0.
      ls_agrupa2-lfimg = ls_agrupa2-lfimg + ls_agrupa-lfimg.
      ls_agrupa2-ntgew = ls_agrupa2-ntgew + ls_agrupa-ntgew.
      ls_agrupa2-brgew = ls_agrupa2-brgew + ls_agrupa-brgew.
      ls_agrupa2-cajas = ls_agrupa2-cajas + ls_agrupa-cajas.
      MODIFY lt_agrupa2 FROM ls_agrupa2 INDEX sy-tabix.
    ELSE.
      ls_agrupa2 = ls_agrupa.
      APPEND ls_agrupa2 TO lt_agrupa2.
    ENDIF.

  ENDLOOP.

  CLEAR ls_agrupa2.
  LOOP AT lt_agrupa2 INTO ls_agrupa2 WHERE ( uecha IS INITIAL ) OR pstyv EQ 'ZNLN'  OR pstyv EQ 'NLC'.
    CLEAR: deta_ls.

*   Nro Linea de Detalle
    ADD 1 TO linea_lv.
    deta_ls-nrolindet = linea_lv.

    PERFORM formateo_nlin CHANGING deta_ls-nrolindet.

*   Indicador de exención
    deta_ls-indexe = '4'. "--> Item no venta

*   Nombre del Item
*

    " Para el caso de la agrupacion por caracteristicas, se debe traer la concatenacion de las caracteristicas (solictado por Gustavo Carrasco)

    lv_descitem = ls_agrupa2-agrupa.

    deta_ls-nmbitem = lv_descitem+0(80).

    deta_ls-dscitem = lv_descitem+80(919).


    CLEAR: ls_agrupa, l_cajas, l_cantidad_e.
    READ TABLE lt_agrupa2 INTO ls_agrupa WITH KEY uecha = ls_agrupa2-posnr.
    IF sy-subrc EQ 0.
      CLEAR ls_agrupa.
      LOOP AT lt_agrupa2 INTO ls_agrupa WHERE uecha EQ ls_agrupa2-posnr.
        l_cantidad_e = l_cantidad_e + ls_agrupa-lfimg.

*        " Cantidad de cajas por posicion.
*        REFRESH t_vepo[].
*        CLEAR: t_vepo, l_cantidad.
*
*        SELECT venum vepos vbeln posnr INTO CORRESPONDING FIELDS OF TABLE t_vepo "#EC CI_NOFIELD
*          FROM vepo
*          WHERE vbeln EQ ls_agrupa-vbeln
*          AND   posnr EQ ls_agrupa-posnr.
*
*        DESCRIBE TABLE t_vepo LINES l_cantidad.
*        l_cajas = l_cajas + l_cantidad.

      ENDLOOP.

*      deta_ls-cajas = l_cajas.
      deta_ls-cajas = ls_agrupa2-cajas.

      CONDENSE deta_ls-cajas.

      CLEAR matkg_ls.
      READ TABLE matkg_lt INTO matkg_ls WITH KEY matnr = ls_agrupa2-matnr.
      IF sy-subrc EQ 0.

        ls_agrupa2-lfimg = ls_agrupa2-ntgew.
        ls_agrupa2-vrkme = ls_agrupa2-gewei.
        PERFORM formateo_cantidad USING ls_agrupa2-lfimg
                                        ls_agrupa2-vrkme
                                  CHANGING deta_ls-qtyitem
                                           deta_ls-unmditem.



      ELSE.
        ls_agrupa2-lfimg = l_cantidad_e.
        PERFORM formateo_cantidad USING ls_agrupa2-lfimg
                                        ls_agrupa2-vrkme
                                  CHANGING deta_ls-qtyitem
                                           deta_ls-unmditem.

      ENDIF.

    ELSE.


      CLEAR matkg_ls.
      READ TABLE matkg_lt INTO matkg_ls WITH KEY matnr = ls_agrupa2-matnr.
      IF sy-subrc EQ 0.

        ls_agrupa2-lfimg = ls_agrupa2-ntgew.
        ls_agrupa2-vrkme = ls_agrupa2-gewei.
        PERFORM formateo_cantidad USING ls_agrupa2-lfimg
                                        ls_agrupa2-vrkme
                                  CHANGING deta_ls-qtyitem
                                           deta_ls-unmditem.



      ELSE.

        PERFORM formateo_cantidad USING ls_agrupa2-lfimg
                                        ls_agrupa2-vrkme
                                 CHANGING deta_ls-qtyitem
                                          deta_ls-unmditem.

      ENDIF.



*      " Cantidad de cajas por posicion.
*      REFRESH t_vepo[].
*      CLEAR: t_vepo, l_cantidad.
*
*      SELECT venum vepos vbeln posnr INTO CORRESPONDING FIELDS OF TABLE t_vepo "#EC CI_NOFIELD
*        FROM vepo
*        WHERE vbeln EQ ls_agrupa2-vbeln
*        AND   matnr EQ ls_agrupa2-matnr.
*
*      DESCRIBE TABLE t_vepo LINES l_cantidad.
      deta_ls-cajas = ls_agrupa2-cajas.
*      deta_ls-cajas = l_cantidad.
      CONDENSE deta_ls-cajas.
    ENDIF.


    IF deta_ls-unmditem EQ 'KG'.

      CLEAR l_dec.
      SPLIT deta_ls-qtyitem AT '.' INTO deta_ls-qtyitem l_dec.

      CLEAR l_dec2.
      l_dec2 = l_dec+0(2).
      IF l_dec2 IS INITIAL.
        l_dec2 = '00'.
      ENDIF.
      CONCATENATE deta_ls-qtyitem '.' l_dec2 INTO deta_ls-qtyitem.
      CONDENSE deta_ls-qtyitem.

    ENDIF.

****************** precio por posicion ********
    IF header_ls-lfart EQ 'ZNL'.

      CLEAR lv_knumv.
      SELECT SINGLE knumv INTO lv_knumv
        FROM ekko
        WHERE ebeln EQ ls_agrupa2-vgbel.

      SELECT SINGLE kbetr INTO lv_netpr1
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND   kposn EQ ls_agrupa2-vgpos
        AND   stunr EQ '050'.

      SELECT SINGLE kbetr INTO l_kbetr
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND   kposn EQ ls_agrupa2-vgpos
        AND   kschl EQ 'MWST'.
      IF sy-subrc NE 0.
        l_kbetr = '19.00'.
      ENDIF.
      l_kbetr = l_kbetr / 100.
      lv_prec = ( lv_netpr1 / 100 ) * ls_agrupa2-lfimg.
      lv_prec_unit = ( lv_netpr1 / 100 ).
    ELSEIF header_ls-lfart EQ 'ZE14'.

      CLEAR: lv_knumv, lv_netpr1.

      SELECT SINGLE knumv INTO lv_knumv
        FROM ekko
        WHERE ebeln EQ ls_agrupa2-vgbel.

      SELECT SINGLE kbetr INTO lv_netpr1
           FROM prcd_elements
           WHERE knumv EQ lv_knumv
           AND   kposn EQ ls_agrupa2-vgpos
           AND   kschl LIKE 'PB%'
           AND   kinak NE 'X'.

      l_kbetr = '19.00'.
      l_kbetr = l_kbetr / 100.
      lv_prec = lv_netpr1 * ls_agrupa2-lfimg.
      lv_prec_unit = lv_netpr1.
*      lv_iva = lv_prec * l_kbetr.

    ELSE.

      SELECT SINGLE * INTO ls_vbap            "#EC CI_ALL_FIELDS_NEEDED
       FROM vbap
       WHERE vbeln EQ ls_agrupa2-vgbel AND
             posnr EQ ls_agrupa2-vgpos.

      "Tomar el precio del pedido de ventas valor netwr
      CLEAR lv_kpein.
      SELECT SINGLE knumv INTO lv_knumv
        FROM vbak
        WHERE vbeln EQ ls_agrupa2-vgbel.

      SELECT SINGLE kpein INTO lv_kpein
        FROM prcd_elements
        WHERE knumv EQ lv_knumv
        AND kschl  LIKE 'ZP%'.

      SELECT SINGLE netpr mwsbp INTO (lv_netpr1 , lv_mwsbp)
       FROM vbap
       WHERE vbeln EQ ls_agrupa2-vgbel
       AND   posnr EQ ls_agrupa2-vgpos.

      CLEAR: lv_prec.
      IF lv_kpein > 1.
        lv_prec = lv_netpr1 * ls_agrupa2-lfimg / lv_kpein.
        lv_prec_unit = lv_netpr1 / lv_kpein.
      ELSE.
        lv_prec = lv_netpr1 * ls_agrupa2-lfimg.
        lv_prec_unit = lv_netpr1.
      ENDIF.

    ENDIF.

    IF header_ls-lfart EQ 'ZE01' OR header_ls-lfart EQ 'ZE14'.

      CLEAR: l_gdatu, l_ukurs, l_fechatc, lv_prec_unit_usd ,  lv_prec_usd, l_dec.

      CONCATENATE data_ls-document_header-wadat_ist+6(2) data_ls-document_header-wadat_ist+4(2) data_ls-document_header-wadat_ist+0(4) INTO l_fechatc.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = l_fechatc
        IMPORTING
          output = l_gdatu.

      SELECT SINGLE ukurs INTO l_ukurs
        FROM tcurr
        WHERE kurst   EQ 'M'
        AND   fcurr   EQ 'CLP'
        AND   tcurr   EQ 'USD'
        AND   gdatu   EQ l_gdatu.

      IF sy-subrc NE 0.
        SELECT SINGLE ukurs INTO l_ukurs
        FROM tcurr
        WHERE kurst   EQ 'M'
        AND   fcurr   EQ 'CLP'
        AND   tcurr   EQ 'USD'
        AND   gdatu   >= l_gdatu.

      ENDIF.

      l_ukurs = 0 - l_ukurs.

      lv_prec_unit_usd = lv_prec_unit * l_ukurs.
      lv_prec_usd      = lv_prec * l_ukurs.

      deta_ls-prcitem = lv_prec_unit_usd.

      CLEAR l_dec.
*      SPLIT deta_ls-prcitem AT '.' INTO deta_ls-prcitem l_dec.
*
*      IF l_dec >= 50.
*        deta_ls-prcitem = deta_ls-prcitem + 1.
*      ENDIF.
      CONDENSE deta_ls-prcitem.

      deta_ls-montoitem = lv_prec_usd.
      CLEAR l_dec.
      SPLIT deta_ls-montoitem AT '.' INTO deta_ls-montoitem l_dec.
      IF l_dec >= 50.
        deta_ls-montoitem = deta_ls-montoitem + 1.
      ENDIF.
      CONDENSE deta_ls-montoitem.

*      PERFORM formateo_montos USING  lv_prec_unit
*                                            'USD'
*                                     CHANGING  deta_ls-prcitem.
*
*      PERFORM formateo_montos USING  lv_prec
*                                        'USD'
*                                 CHANGING  deta_ls-montoitem.
    ELSE.
      PERFORM formateo_montos USING  lv_prec_unit
                                          'CLP'
                                   CHANGING  deta_ls-prcitem.

      PERFORM formateo_montos USING  lv_prec
                                        'CLP'
                                 CHANGING  deta_ls-montoitem.
    ENDIF.

*   Codigos
    CLEAR: cod_ls, cod_lt.
    cod_ls-tpocodigo = 'INT1'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_agrupa2-matnr
      IMPORTING
        output = cod_ls-vlrcodigo.

*    cod_ls-vlrcodigo   = item_ls-matnr.
    APPEND cod_ls TO cod_lt.

    deta_ls-cdgitem[] = cod_lt[].

    PERFORM format_texto CHANGING deta_ls-nmbitem.   "Ajuste por caracteres especiales
    PERFORM format_texto CHANGING deta_ls-dscitem.   "Ajuste por caracteres especiales


    APPEND deta_ls TO deta_lt.

  ENDLOOP.

  dte_ev-detalle[] = deta_lt[].



ENDFORM.
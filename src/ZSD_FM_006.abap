FUNCTION ZSD_FM_006
  IMPORTING
    SD_INVOICE_IV TYPE EDOC_SRC_DATA_SD_INVOICE
  EXPORTING
    DTE_EV TYPE ZCLFEL_ET_DOCUMENTO.




  DATA: data_ls   TYPE edoc_src_data_sd_invoice.
  DATA: vbeln_vl  LIKE likp-vbeln.
  DATA: w_sol_nc  TYPE vbeln.     " Pedido NC
  DATA: lt_vbfa_tr TYPE STANDARD TABLE OF vbfa.
  DATA: ls_vbfa_tr TYPE vbfa.

  MOVE-CORRESPONDING sd_invoice_iv TO data_ls.


* Cabecera
  PERFORM cabecera    USING     data_ls
  CHANGING  dte_ev
    gv_boleta.
*
** Referencias
  PERFORM referencias USING     data_ls
  CHANGING  dte_ev
    w_ref_doc
    w_sol_nc.
*
** ACTECO
  PERFORM acteco      USING     data_ls
  CHANGING  dte_ev.

** Detalle

  IF sd_invoice_iv-document_header-fkart EQ 'ZCD1' OR sd_invoice_iv-document_header-fkart EQ 'ZAA2'.

    PERFORM detalle_nc_adm     USING     data_ls
            vbeln_vl
      CHANGING  dte_ev.

  ELSE.
    PERFORM detalle     USING     data_ls
          vbeln_vl
    CHANGING  dte_ev.

  ENDIF.


** Descuentos y Recargos
  PERFORM des_y_rec   USING     data_ls
  CHANGING  dte_ev.
*



ENDFUNCTION.
************************************************************************
* Programa   : ZCL_DEMO_SINGLETON                              *
* Descripción: Invoice Validations                                     *
* Fecha      : 10.09.2024                                              *
* Autor      : Eduardo Linares                                         *
* Desarrollo :                                 *                       *
************************************************************************
* MODIFICACIONES                                                       *
*                                                                      *
* Fecha      :                                                         *
* Nombre     :                                                         *
* Descripción:                                                         *
************************************************************************
class ZCL_DEMO_SINGLETON definition
  public
  final
  create private .

public section.

  types:
    invoice_class_range_type TYPE RANGE OF fkart .

  methods IS_EXPORT_INVOICE
    importing
      !INVOICE_HEADER type EDOC_SRC_DATA_SD_INVOICE
    returning
      value(IS_VALID) type ABAP_BOOL .
  methods GET_EXPORT_INVOICE_PACKAGES
    returning
      value(CANTBULTOS) type STRING .
  class-methods GET_INSTANCE
    returning
      value(RO_MANAGER) type ref to ZCL_DEMO_SINGLETON .
  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.

  data _EDOC_DATA_INVOICE type EDOC_SRC_DATA_SD_INVOICE .
  class-data _UNIQUE_INSTANCE type ref to ZCL_DEMO_SINGLETON .

  methods ALLOWED_EXPORT_INVOICE_CLASS
    returning
      value(ALLOWED_EXPORT_INVOICE_CLASS) type INVOICE_CLASS_RANGE_TYPE .
  methods GET_PACKAGES_FROM_DELIVERY
    returning
      value(CAJAS_CASES) type ZDE_SD_CAJAS_CASES .
ENDCLASS.



CLASS ZCL_DEMO_SINGLETON IMPLEMENTATION.


  METHOD allowed_export_invoice_class.

    SELECT  valsign
            valoption
            valfrom
            valto
            INTO TABLE allowed_export_invoice_class
            FROM setleaf
            WHERE setname = 'ZSD_EXPORT_INVOICE_CLASS'.

    IF syst-subrc IS NOT INITIAL.

      allowed_export_invoice_class = VALUE  invoice_class_range_type(
                                  LET s = 'I'
                                      o = 'EQ'
                                  IN sign   = s
                                     option = o
                                     ( low = 'ZEX1' )
                                    ).
    ENDIF.


  ENDMETHOD.


  method CLASS_CONSTRUCTOR.

* create singleton on first access
  CREATE OBJECT _unique_instance.

  endmethod.


  METHOD get_export_invoice_packages.

    DATA: cajas_cases TYPE zde_sd_cajas_cases.

    CLEAR cajas_cases.

    IF _edoc_data_invoice-document_item IS INITIAL.
      RETURN.
    ENDIF.

    SELECT vbeln,
           posnr,
           vgbel,
           vgpos
           FROM vbrp
           INTO TABLE @DATA(lt_vbrp)
           FOR ALL ENTRIES IN @_edoc_data_invoice-document_item
           WHERE vbeln = @_edoc_data_invoice-document_item-vbeln AND
                posnr = @_edoc_data_invoice-document_item-posnr.

    IF lt_vbrp IS NOT INITIAL.

      SELECT a~vbeln, a~posnr, a~uecha, a~charg,          "#EC CI_SUBRC
              b~venum, b~matnr
       FROM lips AS a INNER JOIN vepo AS b
         ON a~vbeln = b~vbeln AND
            a~posnr = b~posnr
       INTO TABLE @DATA(lt_lips_vepo)
       FOR ALL ENTRIES IN @lt_vbrp                 "#EC CI_NO_TRANSFORM
       WHERE a~vbeln EQ @lt_vbrp-vgbel
         AND a~uecha EQ @lt_vbrp-vgpos.

      SELECT a~vbeln, a~posnr, a~uecha, a~charg,          "#EC CI_SUBRC
             b~venum, b~matnr
      FROM lips AS a INNER JOIN vepo AS b
        ON a~vbeln = b~vbeln AND
           a~posnr = b~posnr
      APPENDING TABLE @lt_lips_vepo
      FOR ALL ENTRIES IN @lt_vbrp                  "#EC CI_NO_TRANSFORM
      WHERE a~vbeln EQ @lt_vbrp-vgbel
        AND a~posnr EQ @lt_vbrp-vgpos
        AND a~lfimg GT 0.

    ENDIF.

    LOOP AT lt_vbrp INTO DATA(ls_vbrp).
      LOOP AT lt_lips_vepo INTO DATA(ls_lips) WHERE ( vbeln EQ ls_vbrp-vgbel AND uecha EQ ls_vbrp-vgpos )
                                                    OR ( vbeln EQ ls_vbrp-vgbel AND posnr EQ ls_vbrp-vgpos ).
        cajas_cases = cajas_cases + 1.

      ENDLOOP.
    ENDLOOP.

    IF cajas_cases IS INITIAL.
      cajas_cases = get_packages_from_delivery( ).
    ENDIF.

    cantbultos = |{ cajas_cases  ALPHA = OUT }|.   "Remove leading zeros .
    CONDENSE cantbultos NO-GAPS.

  ENDMETHOD.


  method GET_INSTANCE.

* return a reference to this object
* object is created at first access
  ro_manager = _unique_instance.

  endmethod.


  METHOD get_packages_from_delivery.

    DATA l_anzpk     TYPE anzpk.

    IF _edoc_data_invoice-document_item IS INITIAL.
      RETURN.
    ENDIF.

    SORT _edoc_data_invoice-document_item BY vgbel DESCENDING.
    DELETE ADJACENT DUPLICATES FROM _edoc_data_invoice-document_item COMPARING vgbel.

    LOOP AT _edoc_data_invoice-document_item INTO DATA(item_ls_b).

      CLEAR l_anzpk.
      SELECT SINGLE anzpk INTO l_anzpk
      FROM likp
      WHERE vbeln EQ item_ls_b-vgbel.

      cajas_cases = cajas_cases + l_anzpk.

    ENDLOOP.


  ENDMETHOD.


  METHOD is_export_invoice.

    CLEAR _edoc_data_invoice.
    is_valid =  abap_false.

    IF invoice_header-document_header-fkart IN allowed_export_invoice_class( ).
      is_valid =  abap_true.
      _edoc_data_invoice =  invoice_header.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
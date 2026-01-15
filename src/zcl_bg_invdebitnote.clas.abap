CLASS zcl_bg_invdebitnote DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_bgmc_operation .
    INTERFACES if_bgmc_op_single_tx_uncontr .
    INTERFACES if_serializable_object .

    METHODS constructor
      IMPORTING
        iv_bill TYPE zchar10.

  PROTECTED SECTION.
    DATA : im_bill TYPE zchar10.

    METHODS modify
      RAISING
        cx_bgmc_operation.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_BG_INVDEBITNOTE IMPLEMENTATION.


  METHOD constructor.
    im_bill = iv_bill.
  ENDMETHOD.


  METHOD if_bgmc_op_single_tx_uncontr~execute.
    modify( ).
  ENDMETHOD.


  METHOD modify.

    DATA : wa_data TYPE zdb_invdebitnote.
    DATA :lv_pdftest TYPE string.
    DATA lo_pfd TYPE REF TO zcl_invdebitnote.

    CREATE OBJECT lo_pfd.

    lo_pfd->get_pdf_64( EXPORTING io_billingdoc = im_bill RECEIVING pdf_64 = DATA(pdf_64) ).


    wa_data-invno    = im_bill.
    wa_data-base64_3 = pdf_64.

    MODIFY zdb_invdebitnote FROM @wa_data.

  ENDMETHOD.
ENDCLASS.

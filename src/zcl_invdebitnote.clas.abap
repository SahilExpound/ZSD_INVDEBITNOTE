CLASS zcl_invdebitnote DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_pdf_64
      IMPORTING
        VALUE(io_billingdoc) TYPE char10 RETURNING VALUE(pdf_64) TYPE string..

    TYPES : BEGIN OF ty_header,
              comp_nm(50)        TYPE c,
              comp_addr(200)     TYPE c,
              panno(30)          TYPE c,
              gstin(10)          TYPE c,
              cinno(30)          TYPE c,
              inv_no(20)         TYPE c,
              inv_dt(30)         TYPE c,
              ref_no(100)        TYPE c,
              ref_dt(30)         TYPE c,
              state_code(30)     TYPE c,
              state_nm(50)       TYPE c,
              billto_nm(50)      TYPE c,
              billto_gstin(50)   TYPE c,
              billto_addr(50)    TYPE c,
              billto_state(50)   TYPE c,
              billto_statecd(50) TYPE c,
              billto_pan(30)     TYPE c,
              shipto_nm(50)      TYPE c,
              shipto_gstin(50)   TYPE c,
              shipto_addr(50)    TYPE c,
              shipto_state(50)   TYPE c,
              shipto_statecd(50) TYPE c,
              shipto_pan(30)     TYPE c,

            END OF ty_header.

    TYPES : BEGIN OF ty_table,
              sr_no(4)        TYPE c,
              maktx(200)      TYPE c,
              hsncode(30)     TYPE c,
              qty(20)         TYPE c,
              net(30)         TYPE c,
              uom(4)          TYPE c,
              rate(20)        TYPE c,
              taxable_amt(30) TYPE c,
              pqty(30)        TYPE c,
              cgst_p(30)      TYPE c,
              cgst_amt(30)    TYPE c,
              sgst_p(30)      TYPE c,
              sgst_amt(30)    TYPE c,
              total_amt(30)   TYPE c,
            END OF ty_table.

    TYPES : BEGIN OF ty_footer,
              tot_qty(30)     TYPE c,
              tot_taxamt(30)  TYPE c,
              tot_cgstamt(30) TYPE c,
              tot_sgstamt(30) TYPE c,
              tot_amt(30)     TYPE c,
            END OF ty_footer.

    TYPES : BEGIN OF ty_total,
              text(30) TYPE c,
              amt      TYPE string,
            END OF ty_total.

    TYPES : BEGIN OF ty_hsntax,
              hsn_sac(30)     TYPE c,
              taxable_amt(30) TYPE c,
              cgst_rate(20)   TYPE c,
              cgst_amt(20)    TYPE c,
              sgst_rate(20)   TYPE c,
              sgst_amt(20)    TYPE c,
              igst_rate(20)   TYPE c,
              igst_amt(20)    TYPE c,
              cess_rate(20)   TYPE c,
              cess_amt(20)    TYPE c,
              tot_taxamt(20)  TYPE c,
            END OF ty_hsntax.

    TYPES : BEGIN OF ty_hsntax1,
              hsn_sac(30)     TYPE c,
              taxable_amt(13) TYPE p DECIMALS 2,
              cgst_rate(13)   TYPE p DECIMALS 2,
              cgst_amt(13)    TYPE p DECIMALS 2,
              sgst_rate(13)   TYPE p DECIMALS 2,
              sgst_amt(13)    TYPE p DECIMALS 2,
              igst_rate(13)   TYPE p DECIMALS 2,
              igst_amt(13)    TYPE p DECIMALS 2,
              cess_rate(13)   TYPE p DECIMALS 2,
              cess_amt(13)    TYPE p DECIMALS 2,
              tot_taxamt(13)  TYPE p DECIMALS 2,
            END OF ty_hsntax1.

    TYPES : BEGIN OF ty_hsntax_footer,
              hsn_sac(30)     TYPE c,
              taxable_amt(30) TYPE c,
              cgst_amt(20)    TYPE c,
              sgst_amt(20)    TYPE c,
              igst_amt(20)    TYPE c,
              cess_amt(20)    TYPE c,
              tot_taxamt(20)  TYPE c,
            END OF ty_hsntax_footer.

    DATA : gt_total     TYPE TABLE OF ty_total,
           gs_total     TYPE ty_total,
           gs_header    TYPE  ty_header,
           gs_table     TYPE  ty_table,
           gt_table     TYPE TABLE OF ty_table,
           gs_footer    TYPE ty_footer,
           gt_hsntax    TYPE TABLE OF ty_hsntax,
           gs_hsntax    TYPE ty_hsntax,
           gt_hsntax1   TYPE TABLE OF ty_hsntax,
           gs_hsntax1   TYPE ty_hsntax,
           gs_hsnfooter TYPE ty_hsntax_footer.

    DATA : lv_item       TYPE string,
           lv_header     TYPE string,
           lv_footer     TYPE string,
           lv_table2     TYPE string,
           lv_tab2footer TYPE string,
           lv_hsntab     TYPE string,
           lv_hsnfooter  TYPE string,
           lv_xml        TYPE string,
           tax_tab       TYPE string.

    DATA : lv_city(20) TYPE c,
           lv_pin(10)  TYPE c,
           lv_sr       TYPE int4.

    DATA lv_dec9   TYPE p LENGTH 16 DECIMALS 2.

    DATA : iv_num         TYPE string,
           rv_words       TYPE string,
           iv_level       TYPE i,
           lv_cgsttxt(10) TYPE c,
           lv_sgsttxt(10) TYPE c.

    DATA: lv_totgross(13)   TYPE p DECIMALS 2,
          lv_tottaxable(13) TYPE p DECIMALS 2,
          lv_totalcgst(13)  TYPE p DECIMALS 2,
          lv_totalsgst(13)  TYPE p DECIMALS 2,
          lv_totalamt(13)   TYPE p DECIMALS 2,
          lv_totaltax(13)   TYPE p DECIMALS 2,
          lv_tcs(13)        TYPE p DECIMALS 2,
          lv_rate(13)       TYPE p DECIMALS 3.  ""for main table footer

    DATA : lv_amtwords    TYPE string,
           lv_taxamtwords TYPE string,
           lv_taxamt      TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_invdebitnote IMPLEMENTATION.


  METHOD get_pdf_64.

    DATA template TYPE string.

    DATA: lv_roundoff_amount TYPE i_billingdocitemprcgelmntbasic-conditionamount,
          lv_state_cd(2)     TYPE c.

    DATA lo_words TYPE REF TO zcl_amt_word_inr.

    CREATE OBJECT lo_words.

    READ ENTITY i_billingdocumenttp
      ALL FIELDS WITH VALUE #( ( billingdocument = io_billingdoc ) )
       RESULT FINAL(billingheader)
       FAILED FINAL(failed_data1).

    READ ENTITY i_billingdocumenttp
    BY \_item
      ALL FIELDS WITH VALUE #( ( billingdocument = io_billingdoc ) )
      RESULT FINAL(billingdata)
      FAILED FINAL(failed_data).

    SELECT SINGLE billingdocument,
                  companycode,
                  creationdate,
                  creationtime,
                  billingdocumentdate,
                  accountingexchangerate,
                  transactioncurrency
     FROM i_billingdocument
    WHERE billingdocument = @io_billingdoc
     INTO @DATA(wa_billing).

    IF sy-subrc  = 0.
      SELECT SINGLE * FROM I_CompanyCode
      WHERE CompanyCode = @wa_billing-CompanyCode
      INTO @DATA(wa_cocd).

      IF wa_cocd IS NOT INITIAL.
        SELECT SINGLE * FROM I_Address_2
        WITH PRIVILEGED ACCESS
        WHERE AddressID = @wa_cocd-AddressID
        INTO @DATA(wa_cocd_addr).

        SELECT SINGLE * FROM i_countrytext WHERE country = @wa_cocd_addr-country
           AND language = @sy-langu
           INTO @DATA(wa_country_comp).

        SELECT SINGLE * FROM i_regiontext WHERE country = @wa_country_comp-country
            AND region = @wa_cocd_addr-region
            AND language = @sy-langu
           INTO @DATA(wa_region_comp).

        gs_header-comp_addr = |{ wa_cocd_addr-Street },{ wa_cocd_addr-CityName },{ wa_cocd_addr-PostalCode } \n { wa_region_comp-RegionName } , { wa_country_comp-CountryName }| .
        gs_header-state_code = wa_cocd_addr-region .
        gs_header-state_nm = wa_region_comp-RegionName .
      ENDIF.

      SELECT * FROM i_billingdocumentpartnerbasic
                WHERE BillingDocument = @wa_billing-BillingDocument
                INTO TABLE @DATA(it_vbpa).

      SELECT billingdocument,
             billingdocumentitem,
             salesdocumentitemcategory,
             createdbyuser,
             creationdate,
             creationtime,
             product,
             ProductGroup,
             batch,
             plant,
             storagelocation,
             billingdocumentitemtext,
             billingquantity,
             billingquantityunit,
             billingquantityinbaseunit,
             baseunit,
             netamount,
             taxamount,
             transactioncurrency,
             salesdocument,
             salesdocumentitem,
             referencesddocument,
             referencesddocumentitem,
             SoldToParty
        FROM i_billingdocumentitem
       WHERE billingdocument = @wa_billing-billingdocument
       INTO TABLE @DATA(it_item).

      SELECT * FROM I_SalesOrderPartner
             FOR ALL ENTRIES IN @it_item
             WHERE SalesOrder =  @it_item-SalesDocument
             AND PartnerFunction = 'WE'
             INTO TABLE @DATA(it_so_shipto).

      SELECT * FROM i_customer
      FOR ALL ENTRIES IN @it_vbpa
      WHERE customer = @it_vbpa-Customer
      INTO TABLE @DATA(it_customer).

      IF it_so_shipto[] IS NOT INITIAL.
        SELECT * FROM i_customer
         FOR ALL ENTRIES IN @it_so_shipto
         WHERE customer = @it_so_shipto-Customer
         APPENDING TABLE @it_customer.
      ENDIF.

      SELECT * FROM I_Address_2
       WITH PRIVILEGED ACCESS
       FOR ALL ENTRIES IN @it_customer
       WHERE addressid = @it_customer-AddressID
       INTO TABLE @DATA(it_cust_addr).

      SELECT * FROM i_countrytext
        FOR ALL ENTRIES IN @it_cust_addr
        WHERE country = @it_cust_addr-country
        AND language = @sy-langu
        INTO TABLE @DATA(it_country_cust).

      SELECT * FROM i_regiontext
         FOR ALL ENTRIES IN @it_cust_addr
         WHERE country = @it_cust_addr-country
         AND region = @it_cust_addr-region
         AND language = @sy-langu
         INTO TABLE @DATA(it_region_cust).



    ENDIF.

    IF  it_item IS NOT INITIAL.
      SELECT * FROM i_billingdocumentitemprcgelmnt
      FOR ALL ENTRIES IN @it_item
      WHERE BillingDocument = @it_item-BillingDocument
      AND   BillingDocumentItem = @it_item-BillingDocumentItem
      INTO TABLE @DATA(it_prcd).

      SELECT * FROM I_ProductPlantBasic
      FOR ALL ENTRIES IN  @it_item
      WHERE product = @it_item-Product
      INTO TABLE @DATA(it_hsn).

    ENDIF.

    gs_header-comp_nm = wa_cocd-companycodename.
    gs_header-inv_no = wa_billing-BillingDocument.
    gs_header-inv_dt = |{ wa_billing-billingdocumentdate+6(2) }.{ wa_billing-billingdocumentdate+4(2) }.{ wa_billing-billingdocumentdate+0(4) }|.

    READ TABLE it_vbpa INTO DATA(wa_soldto) WITH KEY PartnerFunction = 'AG'.
    IF  sy-subrc IS INITIAL.
      READ TABLE it_customer INTO DATA(wa_cust_soldto) WITH KEY customer = wa_soldto-Customer.
      IF sy-subrc IS INITIAL.
        gs_header-billto_nm = wa_cust_soldto-CustomerName .
        READ TABLE it_cust_addr INTO DATA(wa_soldto_addr) WITH KEY addressid = wa_cust_soldto-AddressID.
        IF sy-subrc IS INITIAL.
          READ TABLE it_country_cust INTO DATA(wa_country_cust) WITH KEY country = wa_soldto_addr-Country.
          READ TABLE it_region_cust INTO DATA(wa_region_cust) WITH KEY country = wa_soldto_addr-Country region = wa_soldto_addr-Region .
          gs_header-billto_addr = |{ wa_soldto_addr-StreetName },{ wa_soldto_addr-HouseNumber },{ wa_soldto_addr-CityName },{ wa_soldto_addr-PostalCode }, \n { wa_region_cust-RegionName }{ wa_country_cust-CountryName }| .
          gs_header-billto_state =  wa_region_cust-RegionName .
          gs_header-billto_statecd =  wa_region_cust-Region .
          gs_header-billto_gstin  =  wa_cust_soldto-TaxNumber3 .
          SELECT SINGLE BPIdentificationNumber FROM  I_BuPaIdentification WHERE BusinessPartner =  @wa_soldto-Customer
         AND BPIdentificationType = 'PAN' INTO @gs_header-billto_pan .
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE it_so_shipto INTO DATA(wa_shipto) WITH KEY PartnerFunction = 'WE'.
    IF  sy-subrc IS INITIAL.
      READ TABLE it_customer INTO DATA(wa_cust_shipto) WITH KEY customer = wa_shipto-Customer.
      IF sy-subrc IS INITIAL.
        gs_header-shipto_nm = wa_cust_shipto-CustomerName .
        READ TABLE it_cust_addr INTO DATA(wa_shipto_addr) WITH KEY addressid = wa_cust_shipto-AddressID.
        IF sy-subrc IS INITIAL.
          READ TABLE it_country_cust INTO DATA(wa_country_shipto) WITH KEY country = wa_shipto_addr-Country.
          READ TABLE it_region_cust INTO DATA(wa_region_shipto) WITH KEY country = wa_shipto_addr-Country region = wa_shipto_addr-Region .
          gs_header-shipto_addr = |{ wa_shipto_addr-StreetName },{ wa_shipto_addr-HouseNumber },{ wa_shipto_addr-CityName },{ wa_shipto_addr-PostalCode }, \n { wa_region_shipto-RegionName }{ wa_country_shipto-CountryName }| .
          gs_header-shipto_state =  wa_region_shipto-RegionName .
          gs_header-shipto_statecd =  wa_region_shipto-Region .
          gs_header-shipto_gstin  =  wa_cust_shipto-TaxNumber3 .
          SELECT SINGLE BPIdentificationNumber FROM  I_BuPaIdentification WHERE BusinessPartner =  @wa_soldto-Customer
        AND BPIdentificationType = 'PAN' INTO @gs_header-shipto_pan .
        ENDIF.
        CLEAR : wa_shipto_addr .
      ENDIF.
      CLEAR : wa_shipto .
    ENDIF.

    IF it_item[] IS NOT INITIAL.
      LOOP AT it_item ASSIGNING FIELD-SYMBOL(<fs_item>).
        gs_table-maktx = <fs_item>-BillingDocumentItemText .
        gs_table-qty   = <fs_item>-BillingQuantity .
        lv_totgross += <fs_item>-BillingQuantity .
        gs_table-uom   = <fs_item>-BillingQuantityUnit .
        READ TABLE it_hsn INTO DATA(wa_hsn) WITH KEY product =  <fs_item>-Product plant = <fs_item>-Plant.
        IF sy-subrc IS INITIAL.
          gs_table-hsncode = wa_hsn-ConsumptionTaxCtrlCode .
          gs_hsntax1-hsn_sac = wa_hsn-ConsumptionTaxCtrlCode .
        ENDIF.

        READ TABLE it_prcd INTO DATA(wa_prcd) WITH KEY BillingDocument = <fs_item>-BillingDocument BillingDocumentItem = <fs_item>-BillingDocumentItem
        ConditionType = 'PPR0'.
        IF sy-subrc IS INITIAL.
          CLEAR : lv_rate .
          lv_rate =   wa_prcd-ConditionRateAmount .
          gs_table-rate = lv_rate .
          gs_table-taxable_amt =  wa_prcd-ConditionAmount .
          lv_tottaxable += wa_prcd-ConditionAmount .
        ENDIF.

        READ TABLE it_prcd INTO DATA(wa_cgst) WITH KEY BillingDocument = <fs_item>-BillingDocument BillingDocumentItem = <fs_item>-BillingDocumentItem
        ConditionType = 'JOCG'.
        IF sy-subrc IS INITIAL.
          CLEAR : lv_rate .
          lv_rate = wa_cgst-ConditionRateAmount .
          gs_table-cgst_p = lv_rate .
          gs_table-cgst_amt =  wa_cgst-ConditionAmount .
          gs_hsntax1-cgst_rate = lv_rate .
          gs_hsntax1-cgst_amt = wa_cgst-ConditionAmount .
          lv_totalcgst += wa_cgst-ConditionAmount .
          lv_cgsttxt = 'CGST' .
        ENDIF.

        READ TABLE it_prcd INTO DATA(wa_Sgst) WITH KEY BillingDocument = <fs_item>-BillingDocument BillingDocumentItem = <fs_item>-BillingDocumentItem
        ConditionType = 'JOSG'.
        IF sy-subrc IS INITIAL.
          CLEAR : lv_rate .
          lv_rate = wa_Sgst-ConditionRateAmount .
          gs_table-sgst_p = lv_rate .
          gs_table-sgst_amt =  wa_Sgst-ConditionAmount .
          gs_hsntax1-sgst_rate = lv_rate .
          gs_hsntax1-sgst_amt = wa_sgst-ConditionAmount .
          lv_totalsgst += wa_Sgst-ConditionAmount .
          lv_sgsttxt = 'SGST' .
        ENDIF.

        READ TABLE it_prcd INTO DATA(wa_igst) WITH KEY BillingDocument = <fs_item>-BillingDocument BillingDocumentItem = <fs_item>-BillingDocumentItem
        ConditionType = 'JOIG'.
        IF sy-subrc IS INITIAL.
          CLEAR : lv_rate .
          lv_rate = wa_igst-ConditionRateAmount .
          gs_table-cgst_p = lv_rate.
          gs_table-cgst_amt =  wa_igst-ConditionAmount .
          gs_hsntax1-igst_rate = lv_rate.
          gs_hsntax1-igst_amt = wa_igst-ConditionAmount .
          lv_totalcgst +=  wa_igst-ConditionAmount .
          lv_cgsttxt = 'IGST' .

        ENDIF.

        READ TABLE it_prcd INTO DATA(wa_ugst) WITH KEY BillingDocument = <fs_item>-BillingDocument BillingDocumentItem = <fs_item>-BillingDocumentItem
        ConditionType = 'JOUG'.
        IF sy-subrc IS INITIAL.
          CLEAR : lv_rate .
          lv_rate = wa_ugst-ConditionRateAmount .
          gs_table-sgst_p = lv_rate .
          gs_table-sgst_amt =  wa_ugst-ConditionAmount .
          gs_hsntax1-sgst_rate = lv_rate .
          gs_hsntax1-sgst_amt = wa_ugst-ConditionAmount .
          lv_totalsgst +=  wa_ugst-ConditionAmount .
          lv_sgsttxt = 'UGST' .
        ENDIF.

*        LOOP at it_prcd INTO DATA(wa_TCS)  where BillingDocument = <fs_item>-BillingDocument and BillingDocumentItem = <fs_item>-BillingDocumentItem
*         and ( ConditionType EQ  'JTC1' OR ConditionType EQ 'JTC2' ) .
**        IF sy-subrc IS INITIAL.
*         lv_tcs += wa_TCS-ConditionAmount .
**        ENDIF.
*        ENDLOOP.
        gs_table-total_amt =  wa_prcd-ConditionAmount + wa_cgst-ConditionAmount + wa_sgst-ConditionAmount +
                              wa_igst-ConditionAmount + wa_ugst-ConditionAmount.

        APPEND gs_hsntax1 TO gt_hsntax1.
        APPEND gs_table TO gt_table .

        CLEAR : gs_table , gs_hsntax1, wa_prcd, wa_igst , wa_cgst , wa_ugst .
      ENDLOOP.

      DATA(gt_hsntax2) = gt_hsntax1[].
      SORT gt_hsntax2 BY hsn_sac .
      DELETE ADJACENT DUPLICATES FROM gt_hsntax2 COMPARING hsn_sac .
      IF gt_hsntax2[] IS NOT INITIAL.
        LOOP AT gt_hsntax2 ASSIGNING FIELD-SYMBOL(<fs_hsntax2>).
          gs_hsntax-hsn_sac = <fs_hsntax2>-hsn_sac .
          gs_hsntax-cgst_rate = <fs_hsntax2>-cgst_rate .
          gs_hsntax-sgst_rate = <fs_hsntax2>-sgst_rate .
          gs_hsntax-igst_rate = <fs_hsntax2>-igst_rate .
          gs_hsntax1-cgst_amt
           = REDUCE dmbtr( INIT sum1 = CONV dmbtr( 0 )
                  FOR wa_tax1 IN gt_hsntax1
                  WHERE ( hsn_sac  = <fs_hsntax2>-hsn_sac )
                  NEXT sum1 = sum1 + wa_tax1-cgst_amt ).
          gs_hsntax-cgst_amt = gs_hsntax1-cgst_amt .
          gs_hsntax1-sgst_amt = REDUCE dmbtr( INIT sum2 = CONV dmbtr( 0 )
                FOR wa_tax1 IN gt_hsntax1
                WHERE ( hsn_sac  = <fs_hsntax2>-hsn_sac )
                NEXT sum2 = sum2 + wa_tax1-sgst_amt ).
          gs_hsntax-sgst_amt = gs_hsntax1-sgst_amt .
          gs_hsntax1-igst_amt = REDUCE dmbtr( INIT sum3 = CONV dmbtr( 0 )
               FOR wa_tax1 IN gt_hsntax1
               WHERE ( hsn_sac  = <fs_hsntax2>-hsn_sac )
               NEXT sum3 = sum3 + wa_tax1-igst_amt ).
          gs_hsntax-igst_amt = gs_hsntax1-igst_amt .
          gs_hsntax1-tot_taxamt =   gs_hsntax1-cgst_amt + gs_hsntax1-sgst_amt + gs_hsntax1-igst_amt  .
          gs_hsntax-tot_taxamt = gs_hsntax1-tot_taxamt .
          APPEND gs_hsntax TO gt_hsntax .
          CLEAR : gs_hsntax , gs_hsntax1 .
        ENDLOOP.

      ENDIF.

      lv_tcs =  REDUCE dmbtr( INIT sum = CONV dmbtr( 0 )
                    FOR gs IN it_prcd
                    WHERE ( billingdocument = wa_billing-billingdocument
                    AND (   conditiontype = 'JTC1'
                     OR  conditiontype = 'JTC2'  ) )
                    NEXT sum = sum + gs-conditionamount ).

      gs_total-text = 'Total Amount Before Tax' .
      gs_total-amt  = lv_tottaxable .
      gs_hsnfooter-taxable_amt = lv_tottaxable .
      APPEND  gs_total TO gt_total .
      CLEAR :  gs_total .

      IF lv_totalcgst IS NOT INITIAL .
        gs_total-text = lv_cgsttxt .
        gs_total-amt  = lv_totalcgst .
        APPEND  gs_total TO gt_total .
        IF lv_cgsttxt = 'IGST'.
          gs_hsnfooter-igst_amt = lv_totalcgst .
        ELSE.
          gs_hsnfooter-cgst_amt = lv_totalcgst .
        ENDIF.
      ENDIF.
      CLEAR :  gs_total .

      IF lv_totalsgst IS NOT INITIAL .
        gs_total-text = lv_sgsttxt .
        gs_total-amt  = lv_totalsgst .
        gs_hsnfooter-sgst_amt = lv_totalsgst .
        APPEND  gs_total TO gt_total .
        CLEAR :  gs_total .
      ENDIF.

      IF lv_tcs IS NOT INITIAL.
        gs_total-text = 'TCS' .
        gs_total-amt  = lv_tcs .
        APPEND  gs_total TO gt_total .
        CLEAR :  gs_total .
      ENDIF.

      lv_totaltax = lv_totalcgst + lv_totalSgst .
      gs_hsnfooter-tot_taxamt = lv_totaltax .
      IF lv_totaltax IS NOT INITIAL.
        lv_taxamt = lv_totaltax .
        CALL METHOD lo_words->amt_rupee
          EXPORTING
            iv_num   = lv_taxamt
          RECEIVING
            rv_words = lv_taxamtwords.

      ENDIF.

      lv_totalamt = lv_tottaxable + lv_totalcgst + lv_totalSgst + lv_tcs .

      IF lv_totalamt IS NOT INITIAL.
*        gs_total-text = 'Total Amount' .
        gs_total-amt  = lv_totalamt .
*        APPEND  gs_total TO gt_total .


        CALL METHOD lo_words->amt_rupee
          EXPORTING
            iv_num   = gs_total-amt
          RECEIVING
            rv_words = lv_amtwords.
        CLEAR :  gs_total .
      ENDIF.


    ENDIF.

    lv_header = |<form1>| &&
                |  <Subform1>| &&
                |    <comp_nm>{ gs_header-comp_nm }</comp_nm>| &&
                |    <comp_addr>{ gs_header-comp_addr }</comp_addr>| &&
                |    <panno> </panno>| &&
                |    <gstin> </gstin>| &&
                |    <cinno> </cinno>| &&
                |  </Subform1>| &&

                |  <Subform2>| &&
                |    <Subform3>| &&
                |      <state_code>{ gs_header-state_code }</state_code>| &&
                |      <state_nm>{ gs_header-state_nm }</state_nm>| &&
                |      <inv_dt>{ gs_header-inv_no }</inv_dt>| &&
                |      <inv_no>{ gs_header-inv_dt }</inv_no>| &&
                |    </Subform3>| &&

                |    <Subform4>| &&
                |      <ref_no>{ gs_header-ref_no }</ref_no>| &&
                |      <ref_dt>{ gs_header-ref_dt }</ref_dt>| &&
                |    </Subform4>| &&

                |    <Subform5>| &&
                |      <billto_gstin>{ gs_header-billto_gstin }</billto_gstin>| &&
                |      <billto_pan>{ gs_header-panno }</billto_pan>| &&
                |      <billto_statecd>{ gs_header-billto_statecd }</billto_statecd>| &&
                |      <billto_state>{ gs_header-billto_state }</billto_state>| &&
                |      <billto_nm>{ gs_header-billto_nm }</billto_nm>| &&
                |      <billto_addr>{ gs_header-billto_addr }</billto_addr>| &&
                |    </Subform5>| &&

                |    <Subform6>| &&
                |      <Subform5>| &&
                |        <shipto_gstin>{ gs_header-shipto_gstin }</shipto_gstin>| &&
                |        <shipto_pan>{ gs_header-shipto_pan }</shipto_pan>| &&
                |        <shipto_statecd>{ gs_header-shipto_statecd }</shipto_statecd>| &&
                |        <shipto_state>{ gs_header-shipto_state }</shipto_state>| &&
                |        <shipto_nm>{ gs_header-shipto_nm }</shipto_nm>| &&
                |        <shipto_addr>{ gs_header-shipto_addr }</shipto_addr>| &&
                |      </Subform5>| &&
                |    </Subform6>| &&
                |  </Subform2>| .

    IF lv_cgsttxt = 'IGST'.
      tax_tab = |<igst>| &&
                |<Table_N1>| &&
                |<HeaderRow/>|.

      LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<fs_table2>).
        CONDENSE : <fs_table2>-cgst_amt,<fs_table2>-cgst_p,<fs_table2>-rate, <fs_table2>-sgst_p ,
        <fs_table2>-sgst_amt , <fs_table2>-qty , <fs_table2>-taxable_amt, <fs_table2>-total_amt .
        lv_item = lv_item &&
                       |<Row1>| &&
                       |<srno>{ gs_table-sr_no }</srno>| &&
                       |<desc>{ <fs_table2>-maktx }</desc>| &&
                       |<hsn>{ <fs_table2>-hsncode }</hsn>| &&
                       |<qty>{ <fs_table2>-qty }</qty>| &&
                       |<uom>{ <fs_table2>-uom }</uom>| &&
                       |<rate>{ <fs_table2>-rate }</rate>| &&
                       |<taxable_amt>{ <fs_table2>-taxable_amt }</taxable_amt>| &&
                       |<igst_rate>{ <fs_table2>-cgst_p }</igst_rate>| &&
                       |<igst_amt>{ <fs_table2>-cgst_amt }</igst_amt>| &&
                       |<total>{ <fs_table2>-total_amt }</total>| &&
                       |</Row1>|.


      ENDLOOP.

      lv_footer = |<FooterRow>| &&
              |  <qty_tot>{ gs_footer-tot_qty }</qty_tot>| &&
              |  <taxamt_tot>{ gs_footer-tot_taxamt }</taxamt_tot>| &&
              |  <cgst_tot>{ gs_footer-tot_cgstamt }</cgst_tot>| &&
              |  <amt_tot>{ gs_footer-tot_amt }</amt_tot>| &&
              |</FooterRow>| &&
              |</Table_N1>| &&
              |</igst>| &&
              |<Subform8/>| &&
              |<Subform9>| &&
              |<Table1>|
               .

    ELSEif lv_cgsttxt = 'CGST' or lv_cgsttxt = ''.
      tax_tab =    |<cgst>| &&
                   |<Table1>| &&
                   |<HeaderRow/>|.

      LOOP AT gt_table ASSIGNING FIELD-SYMBOL(<fs_table1>).
        CONDENSE : <fs_table1>-cgst_amt,<fs_table1>-cgst_p,<fs_table1>-rate, <fs_table1>-sgst_p ,
        <fs_table1>-sgst_amt , <fs_table1>-qty , <fs_table1>-taxable_amt, <fs_table1>-total_amt .
        lv_item = lv_item &&
                       |<Row1>| &&
                       |<srno>{ gs_table-sr_no }</srno>| &&
                       |<desc>{ <fs_table1>-maktx }</desc>| &&
                       |<hsn>{ <fs_table1>-hsncode }</hsn>| &&
                       |<qty>{ <fs_table1>-qty }</qty>| &&
                       |<uom>{ <fs_table1>-uom }</uom>| &&
                       |<rate>{ <fs_table1>-rate }</rate>| &&
                       |<taxable_amt>{ <fs_table1>-taxable_amt }</taxable_amt>| &&
                       |<cgst_rate>{ <fs_table1>-cgst_p }</cgst_rate>| &&
                       |<cgst_amt>{ <fs_table1>-cgst_amt }</cgst_amt>| &&
                       |<sgst_rate>{ <fs_table1>-sgst_p }</sgst_rate>| &&
                       |<sgst_amt>{ <fs_table1>-sgst_amt }</sgst_amt>| &&
                       |<total>{ <fs_table1>-total_amt }</total>| &&
                       |</Row1>|.


      ENDLOOP.

      lv_footer = |<FooterRow>| &&
              |  <qty_tot>{ gs_footer-tot_qty }</qty_tot>| &&
              |  <taxamt_tot>{ gs_footer-tot_taxamt }</taxamt_tot>| &&
              |  <cgst_tot>{ gs_footer-tot_cgstamt }</cgst_tot>| &&
              |  <sgst_tot>{ gs_footer-tot_sgstamt }</sgst_tot>| &&
              |  <amt_tot>{ gs_footer-tot_amt }</amt_tot>| &&
              |</FooterRow>| &&
              |</Table1>| &&
              |</cgst>| &&
              |<Subform8/>| &&
              |<Subform9>| &&
              |<Table1>| &&
              |<HeaderRow/>| &&
              |<HeaderRow/>| .

    ENDIF.

    LOOP AT gt_total INTO gs_total .
      lv_table2 =  lv_table2 &&
                   |<Row1>| &&
                   |<hsn_sac>{ gs_total-text }</hsn_sac>| &&
                   |<taxable_amt>{ gs_total-amt }</taxable_amt>| &&
                   |</Row1>| .

      CLEAR : gs_total .
    ENDLOOP.

    lv_tab2footer = |<FooterRow>| &&
                    |<lv_totgross>{ lv_totalamt }</lv_totgross>| &&
                    |</FooterRow>| &&
                    |</Table1>| &&
                    |</Subform9>| &&
                    |<amt_inwords>{ lv_amtwords }</amt_inwords>| &&
                    |<Table1>| &&
                    |<HeaderRow/>| &&
                    |<HeaderRow/>|.

    LOOP AT gt_hsntax INTO gs_hsntax.
      lv_hsntab = lv_hsntab &&
                  |<Row1>| &&
                  |  <hsn_sac>{ gs_hsntax-hsn_sac }</hsn_sac>| &&
                  |  <taxable_amt>{ gs_hsntax-taxable_amt }</taxable_amt>| &&
                  |  <cgst_rate>{ gs_hsntax-cgst_rate }</cgst_rate>| &&
                  |  <cgst_amt>{ gs_hsntax-cgst_amt }</cgst_amt>| &&
                  |  <sgst_rate>{ gs_hsntax-sgst_rate }</sgst_rate>| &&
                  |  <sgst_amt>{ gs_hsntax-sgst_amt }</sgst_amt>| &&
                  |  <igst_rate>{ gs_hsntax-igst_rate }</igst_rate>| &&
                  |  <igst_amt>{ gs_hsntax-igst_amt }</igst_amt>| &&
                  |  <cess_rate>{ gs_hsntax-cess_rate }</cess_rate>| &&
                  |  <cess_amt>{ gs_hsntax-cess_amt }</cess_amt>| &&
                  |  <taxamt>{ gs_hsntax-tot_taxamt }</taxamt>| &&
                  |</Row1>|.

      CLEAR : gs_hsntax .
    ENDLOOP.

    lv_hsnfooter =    |<FooterRow>| &&
                      |  <cgst_amt>{ gs_hsnfooter-cgst_amt }</cgst_amt>| &&
                      |  <tot_sgst>{ gs_hsnfooter-sgst_amt }</tot_sgst>| &&
                      |  <tot_igst>{ gs_hsnfooter-igst_amt }</tot_igst>| &&
                      |  <tot_cess>{ gs_hsnfooter-cess_amt }</tot_cess>| &&
                      |  <tot_taxamt>{ gs_hsnfooter-taxable_amt }</tot_taxamt>| &&
                      |</FooterRow>| &&
                      |</Table1>| &&
                      |<tax_amtwords>{ lv_taxamtwords }</tax_amtwords>| &&
                      |<Subform7/>| &&
                      |</form1>| .


    lv_xml = |{ lv_header }{ tax_tab }{ lv_item }{ lv_footer }{ lv_table2 }{ lv_tab2footer }{ lv_hsntab }{ lv_hsnfooter }|.

    CALL METHOD zadobe_call=>format_xml
      EXPORTING
        xmldata    = lv_xml
      IMPORTING
        result_xml = lv_xml.

    CALL METHOD zadobe_call=>getpdf
      EXPORTING
        template = 'ZSD_INVDEBITNOTE/ZSD_INVDEBITNOTE'
        xmldata  = lv_xml
      RECEIVING
        result   = DATA(lv_result).

    IF lv_result IS NOT INITIAL.
      pdf_64 = lv_result.

    ENDIF.


  ENDMETHOD.
ENDCLASS.

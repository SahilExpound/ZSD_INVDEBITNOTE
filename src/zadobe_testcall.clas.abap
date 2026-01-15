CLASS zadobe_testcall DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZADOBE_TESTCALL IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

    DATA template TYPE string.


    TYPES: BEGIN OF ty_final,
             name TYPE string,
             id   TYPE string,
             desg TYPE string,
           END OF ty_final.

    DATA ls_final TYPE ty_final.

*    SELECT * FROM ZC_INVEPCG_SH INTO TABLE @DATA(lv_data).

*    SELECT * FROM i_supplier  WHERE SUPPLIER = '0010600001' INTO TABLE @DATA(lv_data1).


    ls_final-name = 'Ganesh Tate'.
    ls_final-id = '000001'.
    ls_final-desg = 'sap ABAP Consultant'.

    DATA(lv_xml) = |<form1>| &&
                   |<Name>Ganesh Tate</Name>| &&
                   |<Id>000001</Id>| &&
                   |<Desg>sap ABAP Consultant</Desg>| &&
                   |</form1>|.


    CALL METHOD zadobe_call=>getpdf
      EXPORTING
        template = 'ZTEST/ZTEMPLATE'
        xmldata  = lv_xml
      RECEIVING
        result   = DATA(lv_result).



  ENDMETHOD.
ENDCLASS.

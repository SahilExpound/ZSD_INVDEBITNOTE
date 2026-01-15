CLASS zcl_amt_word_inr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS amt_rupee
      IMPORTING
        iv_num          TYPE string
      CHANGING
        iv_level        TYPE i OPTIONAL
      RETURNING
        VALUE(rv_words) TYPE string.

    METHODS amt_words
      IMPORTING
        iv_num          TYPE string
        lv_curr         TYPE string OPTIONAL
        lv_deccurr      TYPE string OPTIONAL
      CHANGING
        iv_level        TYPE i OPTIONAL
      RETURNING
        VALUE(rv_words) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AMT_WORD_INR IMPLEMENTATION.


  METHOD amt_rupee.
    TYPES: BEGIN OF str_d,
             num   TYPE i,
             word1 TYPE string,
             word2 TYPE string,
           END OF str_d.
    TYPES : BEGIN OF ty_value,
              num  TYPE i,
              word TYPE c LENGTH 20,
            END OF ty_value.
    DATA : lt_d TYPE TABLE OF    ty_value,
           ls_d TYPE  ty_value.

    DATA: ls_h TYPE str_d,
          ls_k TYPE str_d,
          ls_m TYPE str_d,
          ls_b TYPE str_d,
          ls_t TYPE str_d,
          ls_o TYPE str_d.

    DATA lv_int TYPE i.
    DATA lv_int1 TYPE i.
    DATA lv_int2 TYPE i.
    DATA lv_dec_s TYPE string.
    DATA lv_dec   TYPE i.
    DATA lv_wholenum TYPE i.
    DATA lv_inp1 TYPE string.
    DATA lv_inp2 TYPE string.
    DATA lv_dec_words TYPE string.

    IF iv_num IS INITIAL.
      RETURN.
    ENDIF.

    lt_d = VALUE #(  (  num = 0 word = 'Zero'  )
    (  num = 1 word = 'One' )
    (  num = 2 word = 'Two' )
    (  num = 3 word = 'Three' )
    (  num = 4 word = 'Four' )
    (  num = 5 word = 'Five' )
    (  num = 6 word = 'Six' )
    (  num = 7 word = 'Seven' )
    (  num = 8 word = 'Eight' )
    (  num = 9 word = 'Nine' )
    (  num = 10 word = 'Ten' )
    (  num = 11 word = 'Eleven' )
    (  num = 12 word = 'Twelve' )
    (  num = 13 word = 'Thirteen' )
    (  num = 14 word = 'Fourteen' )
    (  num = 15 word = 'Fifteen' )
    (  num = 16 word = 'Sixteen' )
    (  num = 17 word = 'Seventeen' )
    (  num = 18 word = 'Eighteen' )
    (  num = 19 word = 'Nineteen' )
    (  num = 20 word = 'Twenty' )
    (  num = 30 word = 'Thirty' )
    (  num = 40 word = 'Fourty' )
    (  num = 50 word = 'Fifty' )
    (  num = 60 word = 'Sixty' )
    (  num = 70 word = 'Seventy' )
    (  num = 80 word = 'Eighty' )
    (  num = 90 word = 'Ninety' ) ).

*    Use the following if this is required in Lakhs/Crores instead of Millions/Billions
*
    ls_h-num = 100.
    ls_h-word1 = 'Hundred' .
    ls_h-word2 = 'Hundred '.  "and   .

    ls_k-num = ls_h-num * 10.
    ls_k-word1 = 'Thousand'  .
    ls_k-word2 = 'Thousand'  .

    ls_m-num = ls_k-num * 100.
    ls_m-word1 = 'Lakh'   .
    ls_m-word2 = 'Lakh'   .

    ls_b-num = ls_m-num * 100.
    ls_b-word1 = 'Crore'  .
    ls_b-word2 = 'Crore'  .

    SPLIT iv_num AT '.' INTO lv_inp1 lv_inp2.

    lv_int = lv_inp1.
    lv_wholenum = lv_int.

    IF iv_level IS INITIAL.
      IF lv_inp2 IS NOT INITIAL.
        CONDENSE lv_inp2.
        lv_dec_s   = lv_inp2.
        lv_dec     = lv_inp2.
      ENDIF.
    ENDIF.
    iv_level = iv_level + 1.

*   Whole Number converted to Words
    IF lt_d IS NOT INITIAL.
      IF lv_int <= 20.
        READ TABLE lt_d  INTO ls_d WITH KEY num = lv_int.
        rv_words =  ls_d-word .
      ELSEIF lv_int < 100 AND lv_int > 20.
        DATA(mod) = lv_int MOD 10.
        DATA(floor) = floor( lv_int DIV 10 ).
        IF mod = 0.
          READ TABLE lt_d  INTO ls_d WITH KEY num = lv_int.
          rv_words = ls_d-word.
        ELSE.
          READ TABLE lt_d  INTO ls_d WITH KEY num = floor * 10.
          DATA(pos1) = ls_d-word.
          READ TABLE lt_d  INTO ls_d WITH KEY num = mod.
          DATA(pos2) = ls_d-word.
          rv_words = |{ pos1 } | && |{ pos2 } |.
        ENDIF.
      ELSE.
        IF lv_int  < ls_k-num.
          ls_o = ls_h.
        ELSEIF lv_int < ls_m-num.
          ls_o = ls_k.
        ELSEIF lv_int < ls_b-num.
          ls_o = ls_m.
        ELSE.
          ls_o = ls_b.
        ENDIF.
        mod = lv_int MOD ls_o-num.
        floor = floor( iv_num DIV ls_o-num ).
        lv_inp1 = floor.
        lv_inp2 = mod.

        IF mod = 0.
          DATA(output2) = amt_rupee( EXPORTING iv_num  = lv_inp1
                                     CHANGING iv_level = iv_level ).
          rv_words =  |{ output2 } | && |{ ls_o-word1 } |.
        ELSE.
          output2 = amt_rupee( EXPORTING iv_num   = lv_inp1
                               CHANGING  iv_level = iv_level ).
          DATA(output3) = amt_rupee( EXPORTING iv_num  = lv_inp2
                                     CHANGING iv_level = iv_level ).
          rv_words = |{ output2 } | && |{ ls_o-word2 } | && |{ output3 } |.
        ENDIF.
      ENDIF.

      iv_level = iv_level - 1.
      IF iv_level IS INITIAL.
*       "Dollars" is base monetary unit used in this sample,
*       but this could change as per the currency of the scenario.
*       It must be ensured that relative fractional monetary unit
*       shall be updated later in the code relative to the base unit
        rv_words = |Rupees { rv_words }|  .
        IF lv_dec <= 20.
          READ TABLE lt_d REFERENCE INTO DATA(ls_d2) WITH KEY num = lv_dec.
          IF sy-subrc = 0.
            lv_dec_words = |{ ls_d2->word }|.
          ENDIF.
        ELSEIF lv_dec < 100 AND lv_dec > 20.
          DATA(mod1) = lv_dec MOD 10.
          DATA(floor1) = floor( lv_dec DIV 10 ).
          IF mod1 = 0.
            READ TABLE lt_d REFERENCE INTO ls_d2 WITH KEY num = lv_dec.
            IF sy-subrc = 0.
              lv_dec_words = ls_d2->word.
            ENDIF.
          ELSE.
            READ TABLE lt_d REFERENCE INTO ls_d2 WITH KEY num = floor1 * 10.
            IF sy-subrc = 0.
              DATA(pos1_d) = ls_d2->word.
            ENDIF.
            READ TABLE lt_d REFERENCE INTO ls_d2 WITH KEY num = mod1.
            IF sy-subrc = 0.
              DATA(pos2_d) = ls_d2->word.
            ENDIF.
            IF pos1_d IS NOT INITIAL AND pos2_d IS NOT INITIAL.
              lv_dec_words = |{ pos1_d } | && |{ pos2_d } |.

            ENDIF.
          ENDIF.
        ENDIF.
*       Since "Dollars" was used for base monetary unit, "Cents"
*       has been used as fractional monetary unit in the code below
*       This can be handled dynamically as well based on the requirement
        CONCATENATE 'And' lv_dec_words INTO lv_dec_words SEPARATED BY space.
        rv_words = |{ rv_words } { lv_dec_words } Paise Only |   .
*     CONDENSE rv_words.
      ENDIF.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD amt_words.
    TYPES: BEGIN OF str_d,
             num   TYPE i,
             word1 TYPE string,
             word2 TYPE string,
           END OF str_d.
    TYPES : BEGIN OF ty_value,
              num  TYPE i,
              word TYPE c LENGTH 20,
            END OF ty_value.
    DATA : lt_d TYPE TABLE OF    ty_value,
           ls_d TYPE  ty_value.

    DATA: ls_h TYPE str_d,
          ls_k TYPE str_d,
          ls_m TYPE str_d,
          ls_b TYPE str_d,
          ls_t TYPE str_d,
          ls_o TYPE str_d.

    DATA lv_int TYPE i.
    DATA lv_int1 TYPE i.
    DATA lv_int2 TYPE i.
    DATA lv_dec_s TYPE string.
    DATA lv_dec   TYPE i.
    DATA lv_wholenum TYPE i.
*    DATA lv_inp1 TYPE string.
*    DATA lv_inp2 TYPE string.
    DATA lv_inp1 TYPE string.
    DATA lv_inp2 TYPE string.
    DATA lv_dec_words TYPE string.

    IF iv_num IS INITIAL.
      RETURN.
    ENDIF.

    lt_d = VALUE #(  (  num = 0 word = 'Zero' )
    (  num = 1 word = 'One' )
    (  num = 2 word = 'Two' )
    (  num = 3 word = 'Three' )
    (  num = 4 word = 'Four' )
    (  num = 5 word = 'Five' )
    (  num = 6 word = 'Six' )
    (  num = 7 word = 'Seven' )
    (  num = 8 word = 'Eight' )
    (  num = 9 word = 'Nine' )
    (  num = 10 word = 'Ten' )
    (  num = 11 word = 'Eleven' )
      (  num = 12 word = 'Twelve' )
        (  num = 13 word = 'Thirteen' )
          (  num = 14 word = 'Fourteen' )
            (  num = 15 word = 'Fifteen' )
              (  num = 16 word = 'Sixteen' )
                (  num = 17 word = 'Seventeen' )

                (  num = 18 word = 'Eighteen' )
                (  num = 19 word = 'Nineteen' )
                  (  num = 20 word = 'Twenty' )
                    (  num = 30 word = 'Thirty' )
                      (  num = 40 word = 'Fourty' )
                        (  num = 50 word = 'Fifty' )
                          (  num = 60 word = 'Sixty' )
                            (  num = 70 word = 'Seventy' )
                              (  num = 80 word = 'Eighty' )
                               (  num = 90 word = 'Ninety' )






    ).

    ls_h-num = 100.
    ls_h-word1 = 'Hundred'.
    ls_h-word2 = 'Hundred '.  "and

    ls_k-num = ls_h-num * 10.
    ls_k-word1 = 'Thousand'.
    ls_k-word2 = 'Thousand'.

    ls_m-num = ls_k-num * 1000.
    ls_m-word1 = 'Million'.
    ls_m-word2 = 'Million'.

    ls_b-num = ls_m-num * 1000.
    ls_b-word1 = 'Billion'.
    ls_b-word2 = 'Billion'.

*    Use the following if this is required in Lakhs/Crores instead of Millions/Billions

    SPLIT iv_num AT '.' INTO lv_inp1 lv_inp2.

    lv_int = lv_inp1.
    lv_wholenum = lv_int.

    IF iv_level IS INITIAL.
      IF lv_inp2 IS NOT INITIAL.
        CONDENSE lv_inp2.
        lv_dec_s   = lv_inp2.
        lv_dec     = lv_inp2.
      ENDIF.
    ENDIF.
    iv_level = iv_level + 1.

*   Whole Number converted to Words
    IF lt_d IS NOT INITIAL.
      IF lv_int <= 20.
        READ TABLE lt_d  INTO ls_d WITH KEY num = lv_int.
        rv_words =  ls_d-word .
      ELSEIF lv_int < 100 AND lv_int > 20.
        DATA(mod) = lv_int MOD 10.
        DATA(floor) = floor( lv_int DIV 10 ).
        IF mod = 0.
          READ TABLE lt_d  INTO ls_d WITH KEY num = lv_int.
          rv_words = ls_d-word.
        ELSE.
          READ TABLE lt_d  INTO ls_d WITH KEY num = floor * 10.
          DATA(pos1) = ls_d-word.
          READ TABLE lt_d  INTO ls_d WITH KEY num = mod.
          DATA(pos2) = ls_d-word.
          rv_words = |{ pos1 } | && |{ pos2 } |.
        ENDIF.
      ELSE.
        IF lv_int  < ls_k-num.
          ls_o = ls_h.
        ELSEIF lv_int < ls_m-num.
          ls_o = ls_k.
        ELSEIF lv_int < ls_b-num.
          ls_o = ls_m.
        ELSE.
          ls_o = ls_b.
        ENDIF.
        mod = lv_int MOD ls_o-num.
        floor = floor( iv_num DIV ls_o-num ).
        lv_inp1 = floor.
        lv_inp2 = mod.

        IF mod = 0.
          DATA(output2) = amt_words( EXPORTING iv_num  = lv_inp1
                                     CHANGING iv_level = iv_level ).
          rv_words =  |{ output2 } | && |{ ls_o-word1 } |.
        ELSE.
          output2 = amt_words( EXPORTING iv_num   = lv_inp1
                               CHANGING  iv_level = iv_level ).
          DATA(output3) = amt_words( EXPORTING iv_num  = lv_inp2
                                     CHANGING iv_level = iv_level ).
          rv_words = |{ output2 } | && |{ ls_o-word2 } | && |{ output3 } |.
        ENDIF.
      ENDIF.

      iv_level = iv_level - 1.
      IF iv_level IS INITIAL.

*       "Dollars" is base monetary unit used in this sample,
*       but this could change as per the currency of the scenario.
*       It must be ensured that relative fractional monetary unit
*       shall be updated later in the code relative to the base unit

*        rv_words = |{ rv_words } Dollars |.
        rv_words = |{ lv_curr } { rv_words }| .
        IF lv_dec <= 20.
          READ TABLE lt_d REFERENCE INTO DATA(ls_d2) WITH KEY num = lv_dec.
          IF sy-subrc = 0.
            lv_dec_words = |{ ls_d2->word }|.
          ENDIF.
        ELSEIF lv_dec < 100 AND lv_dec > 20.
          DATA(mod1) = lv_dec MOD 10.
          DATA(floor1) = floor( lv_dec DIV 10 ).
          IF mod1 = 0.
            READ TABLE lt_d REFERENCE INTO ls_d2 WITH KEY num = lv_dec.
            IF sy-subrc = 0.
              lv_dec_words = ls_d2->word.
            ENDIF.
          ELSE.
            READ TABLE lt_d REFERENCE INTO ls_d2 WITH KEY num = floor1 * 10.
            IF sy-subrc = 0.
              DATA(pos1_d) = ls_d2->word.
            ENDIF.
            READ TABLE lt_d REFERENCE INTO ls_d2 WITH KEY num = mod1.
            IF sy-subrc = 0.
              DATA(pos2_d) = ls_d2->word.
            ENDIF.
            IF pos1_d IS NOT INITIAL AND pos2_d IS NOT INITIAL.

              lv_dec_words = |{ pos1_d } | && |{ pos2_d } |.

            ENDIF.
          ENDIF.
        ENDIF.
*       Since "Dollars" was used for base monetary unit, "Cents"
*       has been used as fractional monetary unit in the code below
*       This can be handled dynamically as well based on the requirement
        IF lv_dec_words IS NOT INITIAL .
          CONCATENATE 'And' lv_dec_words INTO lv_dec_words SEPARATED BY space .
          rv_words = |{ rv_words } { lv_dec_words } { lv_deccurr } Only |.
        ELSE.
          rv_words = |{ rv_words } Only |.
        ENDIF.
        CONDENSE rv_words.

      ENDIF.
      RETURN.
    ENDIF.



  ENDMETHOD.
ENDCLASS.

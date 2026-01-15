CLASS lhc_ZI_INVDEBIT DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_invdebit RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_invdebit RESULT result.

    METHODS zprint FOR MODIFY
      IMPORTING keys FOR ACTION zi_invdebit~zprint RESULT result.

ENDCLASS.

CLASS lhc_ZI_INVDEBIT IMPLEMENTATION.

  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD zprint.
*    DATA lo_pfd TYPE REF TO zcl_bg_invdebitnote.
*
*    CREATE OBJECT lo_pfd.

    READ ENTITIES OF zi_invdebitnote IN LOCAL MODE
    ENTITY zi_invdebitnote
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(lt_result).

    LOOP AT lt_result INTO DATA(lw_result).


      DATA : update_lines TYPE TABLE FOR UPDATE zi_invdebitnote,
             update_line  TYPE STRUCTURE FOR UPDATE zi_invdebitnote.

      update_line-%tky                   = lw_result-%tky.
      update_line-base64                 = 'A'.

      IF update_line-base64 IS NOT INITIAL.

        APPEND update_line TO update_lines.


        MODIFY ENTITIES OF zi_invdebitnote IN LOCAL MODE
        ENTITY zi_invdebitnote
        UPDATE
        FIELDS ( base64 )
                   WITH update_lines
           REPORTED reported
           FAILED failed
           MAPPED mapped.

        READ ENTITIES OF zi_invdebitnote IN LOCAL MODE ENTITY zi_invdebitnote
        ALL FIELDS WITH CORRESPONDING #( lt_result ) RESULT DATA(lt_final).

        result =  VALUE #( FOR  lw_final IN  lt_final ( %tky = lw_final-%tky
            %param = lw_final  )  ).

                    APPEND VALUE #( %tky = keys[ 1 ]-%tky
                           %msg = new_message_with_text(
                           severity = if_abap_behv_message=>severity-success
                           text = 'PDF Generated!' )
                            ) TO reported-zi_invdebit .
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZI_INVDEBITNOTE DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS save_modified REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZI_INVDEBITNOTE IMPLEMENTATION.

  METHOD save_modified.
***    DATA lo_pfd TYPE REF TO zcl_invdebitnote.
***
***    CREATE OBJECT lo_pfd.

    IF update-zi_invdebit IS NOT INITIAL.

      LOOP AT update-zi_invdebit INTO DATA(ls_data).

*        DATA(new) = NEW zcl_bg_process_bill_print( iv_bill = ls_data-billingdocument ).
        DATA(new) = NEW zcl_bg_invdebitnote( iv_bill = ls_data-billingdocument ).

        DATA background_process TYPE REF TO if_bgmc_process_single_op.

        TRY.

            background_process = cl_bgmc_process_factory=>get_default( )->create( ).

            background_process->set_operation_tx_uncontrolled( new ).

            background_process->save_for_execution( ).

          CATCH cx_bgmc INTO DATA(exception).
            "handle exception
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.

@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root View foR Custom advance Invoice'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_INVdebitnote
  as select from    I_BillingDocument as a
    left outer join zdb_invdebitnote  as b on a.BillingDocument = b.invno

{
  key a.BillingDocument,
      a.BillingDocumentDate,
      a.BillingDocumentType,
      a.CompanyCode,
      a.FiscalYear,
      a.SalesOrganization,
      a.Division,
      a.DistributionChannel,
      a.SoldToParty,
      a._SoldToParty.CustomerName,
      b.base64_3 as base64
      //    _association_name // Make association public
}

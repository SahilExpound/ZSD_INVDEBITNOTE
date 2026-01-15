@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View Billing Document'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define root view entity ZC_INVDEBITNOTE
  provider contract transactional_query
  as projection on ZI_INVdebitnote

{

      @EndUserText.label: 'Billing Document'
  key BillingDocument,
      @EndUserText.label: 'Billing Document Date'
      BillingDocumentDate,
      @EndUserText.label: 'Billing Document Type'
      BillingDocumentType,
      @EndUserText.label: 'Company Code'
      CompanyCode,
      @EndUserText.label: 'Fiscal Year'
      FiscalYear,
      @EndUserText.label: 'Sales Organization'
      SalesOrganization,
      @EndUserText.label: 'Division'
      Division,
      @EndUserText.label: 'Distribution Channel'
      DistributionChannel,
      @EndUserText.label: 'Sold To Party'
      SoldToParty,
      @EndUserText.label: 'Customer Name'
      CustomerName,
      @EndUserText.label: 'base64'
      base64
}

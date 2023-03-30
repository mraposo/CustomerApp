DEFINE TEMP-TABLE ttCustomer{&Suffix} NO-UNDO
     FIELD RowIdent     AS ROWID
     FIELD CustNum      AS INTEGER   LABEL "Cust Num" FORMAT ">>>>9"    HELP "Please enter a customer number." 
     FIELD Name         AS CHARACTER LABEL "Name" FORMAT "x(30)"        HELP "Please enter a name." 
     FIELD Address      AS CHARACTER LABEL "Address" FORMAT "x(35)"     HELP "Please enter an address." 
     FIELD Address2     AS CHARACTER LABEL "Address2" FORMAT "x(35)"    HELP "Please enter an address." 
     FIELD City         AS CHARACTER LABEL "City" FORMAT "x(25)"        HELP "Please enter a city." 
     FIELD State        AS CHARACTER LABEL "State" FORMAT "x(20)"       HELP "Please enter standard state abbreviation." 
     FIELD Country      AS CHARACTER LABEL "Country" FORMAT "x(20)"     HELP "Please enter a country." 
     FIELD Phone        AS CHARACTER LABEL "Phone" FORMAT "x(20)"       HELP "Please enter a phone number" 
     FIELD SalesRep     AS CHARACTER LABEL "Sales Rep" FORMAT "x(4)"    HELP "Please Enter a Sales Rep." 
     FIELD PostalCode   AS CHARACTER LABEL "Postal Code" FORMAT "x(10)" HELP "Please enter the appropriate Postal Code." 
     FIELD EmailAddress AS CHARACTER LABEL "Email" FORMAT "x(50)"       HELP "Please enter an full Internet Email Address."
     FIELD Orders       AS INTEGER 
    .
    
DEFINE TEMP-TABLE ttSalesRep{&Suffix} NO-UNDO LIKE SalesRep
    FIELD RowIdent AS ROWID.
    
DEFINE TEMP-TABLE ttOrder{&Suffix} NO-UNDO LIKE Order
    FIELD RowIdent AS ROWID.

DEFINE TEMP-TABLE ttInvoice{&Suffix} NO-UNDO LIKE Invoice
       FIELD RowIdent AS ROWID.
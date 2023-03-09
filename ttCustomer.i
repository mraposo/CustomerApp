DEFINE TEMP-TABLE ttCustomer NO-UNDO
    FIELD RowIdent     AS ROWID
    FIELD CustNum      LIKE Customer.CustNum
    FIELD Name         LIKE Customer.Name
    FIELD Comments     LIKE Customer.Comments
    FIELD Address      LIKE Customer.Address
    FIELD Address2     LIKE Customer.Address2
    FIELD State        LIKE Customer.State
    FIELD City         LIKE Customer.City
    FIELD PostalCode   LIKE Customer.PostalCode
    FIELD Country      LIKE Customer.Country
    FIELD Phone        LIKE Customer.Phone
    FIELD EmailAddress LIKE Customer.EmailAddress
    FIELD SalesRep     LIKE Customer.SalesRep      
    FIELD Orders       AS INTEGER
    INDEX RowIdent RowIdent.

DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO
    FIELD RowIdent     AS ROWID
    FIELD CustNum      LIKE Customer.CustNum
    FIELD Name         LIKE Customer.Name
    FIELD Comments     LIKE Customer.Comments
    FIELD Address      LIKE Customer.Address
    FIELD Address2     LIKE Customer.Address2
    FIELD State        LIKE Customer.State
    FIELD City         LIKE Customer.City
    FIELD PostalCode   LIKE Customer.PostalCode
    FIELD Country      LIKE Customer.Country
    FIELD Phone        LIKE Customer.Phone
    FIELD EmailAddress LIKE Customer.EmailAddress
    FIELD SalesRep     LIKE Customer.SalesRep      
    FIELD Orders       AS INTEGER
    INDEX RowIdent RowIdent.
    
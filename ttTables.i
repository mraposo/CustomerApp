DEFINE TEMP-TABLE ttSalesRep NO-UNDO LIKE SalesRep
    FIELD RowIdent AS ROWID.
    
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Sports2000.Order
    FIELD RowIdent AS ROWID.
    
DEFINE TEMP-TABLE ttOrderCount NO-UNDO
    FIELD CustNum    LIKE Order.CustNum
    FIELD OrderCount AS INTEGER 
    INDEX CustNumIndex CustNum.
    
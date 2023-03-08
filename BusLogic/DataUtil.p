&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomer NO-UNDO 
       FIELD RowIdent AS ROWID
       FIELD CustNum LIKE Customer.CustNum
       FIELD Name LIKE Customer.Name
       FIELD Comments LIKE Customer.Comments
       FIELD Address LIKE Customer.Address
       FIELD Address2 LIKE Customer.Address2
       FIELD State LIKE Customer.State
       FIELD City LIKE Customer.City
       FIELD PostalCode LIKE Customer.PostalCode
       FIELD Country LIKE Customer.Country
       FIELD Phone LIKE Customer.Phone
       FIELD EmailAddress LIKE Customer.EmailAddress
       FIELD SalesRep LIKE Customer.SalesRep      
       FIELD Orders AS INTEGER
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttInvoice NO-UNDO LIKE Sports2000.Invoice
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttItem NO-UNDO LIKE Sports2000.Item.
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Sports2000.Order
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttOrderLine NO-UNDO LIKE Sports2000.OrderLine
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttSalesrep NO-UNDO LIKE Sports2000.Salesrep
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : DataUtil.p
    Purpose     : Accesses database to retrieve, modify, and delete records. 

    Syntax      :

    Description : Used by user interface procedures to operate on the database.
                  Run persistently to acccess internal procedures.

    Author(s)   : Mario Raposo
    Created     : 2023
      
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomer T "?" NO-UNDO Sports2000 Customer
      ADDITIONAL-FIELDS:
          FIELD Orders   AS INTEGER
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
      TABLE: ttInvoice T "?" NO-UNDO Sports2000 Invoice
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
      TABLE: ttItem T "?" NO-UNDO Sports2000 Item
      TABLE: ttOrder T "?" NO-UNDO Sports2000 Order
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
          
      END-FIELDS.
      TABLE: ttOrderLine T "?" NO-UNDO Sports2000 OrderLine
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
      TABLE: ttSalesrep T "?" NO-UNDO Sports2000 Salesrep
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* Shutdown when the user exits the application. */

SUBSCRIBE TO "Shutdown":U ANYWHERE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-DeleteCustomer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteCustomer Procedure 
PROCEDURE DeleteCustomer :
/*------------------------------------------------------------------------------
  Purpose: Delete customer records from the database.    
  Parameters:  prRowIdent - RowID of the record to be deleted.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prRowIdent AS ROWID NO-UNDO.
    
    FIND Customer WHERE ROWID(Customer) = prRowIdent EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Customer THEN 
    DO:
        DELETE Customer.
        RETURN.
    END.
    ELSE IF LOCKED (Customer) THEN
            RETURN "The record is locked.  Try later.".
         ELSE
            RETURN "The record has already been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteInvoice Procedure 
PROCEDURE DeleteInvoice :
/*------------------------------------------------------------------------------
  Purpose: Delete invoice records from the database.    
  Parameters:  prRowIdent - RowID of the record to be deleted.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prRowIdent AS ROWID NO-UNDO.
    
    FIND Invoice WHERE ROWID(Invoice) = prRowIdent EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Invoice THEN  
    DO:
        DELETE Invoice. 
        RETURN RETURN-VALUE.
    END.
    ELSE IF LOCKED (Invoice) THEN
            RETURN "The record is locked.  Try later.".
         ELSE
            RETURN "The record has already been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteOrder Procedure 
PROCEDURE DeleteOrder :
/*------------------------------------------------------------------------------
  Purpose: Delete order records from the database.    
  Parameters:  prRowIdent - RowID of the record to be deleted.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prRowIdent AS ROWID NO-UNDO.
    
    FIND Order WHERE ROWID(Order) = prRowIdent EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE Order THEN  
    DO:
        DELETE Order.
        RETURN.
    END.
    ELSE IF LOCKED (Order) THEN
            RETURN "The record is locked.  Try later.".
         ELSE
            RETURN "The record has already been deleted!".
            
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteOrderline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteOrderline Procedure 
PROCEDURE DeleteOrderline :
/*------------------------------------------------------------------------------
  Purpose: Delete OrderLine records from the database.    
  Parameters:  prRowIdent - RowID of the record to be deleted.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prRowIdent AS ROWID NO-UNDO.
    
    FIND OrderLine WHERE ROWID(OrderLine) = prRowIdent EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE OrderLine THEN  
    DO:
        DELETE OrderLine.
        RETURN.
    END.
    ELSE IF LOCKED (OrderLine) THEN
            RETURN "The record is locked.  Try later.".
         ELSE
            RETURN "The record has already been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteSalesrep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteSalesrep Procedure 
PROCEDURE DeleteSalesrep :
/*------------------------------------------------------------------------------
  Purpose: Delete salesrep records from the database.    
  Parameters:  prRowIdent - RowID of the record to be deleted.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER prRowIdent AS ROWID NO-UNDO.
    
    FIND SalesRep WHERE ROWID(SalesRep) = prRowIdent EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE SalesRep THEN  
    DO:
        DELETE SalesRep.
        RETURN.
    END.
    ELSE IF LOCKED (SalesRep) THEN
            RETURN "The record is locked.  Try later.".
         ELSE
            RETURN "The record has already been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCustData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCustData Procedure 
PROCEDURE GetCustData :
/*------------------------------------------------------------------------------
  Purpose: Retrieve all the customer records and pass back to the calling 
            procedure.    
  Parameters:  ttCustomer - Temp-table used to pass records between procedures.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttCustomer.
    EMPTY TEMP-TABLE ttCustomer.
         
    /*
    FOR EACH Customer NO-LOCK: 
        CREATE ttCustomer.

       iCounter = 0.   
       FOR EACH Order WHERE Order.OrderStatus = "Ordered" AND Order.CustNum = Customer.CustNum NO-LOCK:
            iCounter = iCounter + 1.
            BUFFER-COPY Customer TO ttCustomer.
            ASSIGN 
                ttCustomer.RowIdent = ROWID(Customer)
                ttCustomer.Orders = iCounter.
        END.  
    END.
    */
    
     
    FOR EACH Customer NO-LOCK:
        iCounter = 0. 
        FOR EACH Order WHERE Order.CustNum = Customer.CustNum NO-LOCK:
            iCounter = iCounter + 1.
        END.
        
        CREATE ttCustomer.
        BUFFER-COPY Customer TO ttCustomer.

        ASSIGN 
            ttCustomer.RowIdent = ROWID(Customer)
            ttCustomer.Orders   = iCounter.
    END.    
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCustRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCustRecord Procedure 
PROCEDURE GetCustRecord :
/*------------------------------------------------------------------------------
  Purpose: Retrieve a specific customer record and pass it back to the calling 
            procedure.    
  Parameters:  
        ttCustomer - Temp-table used to pass record between procedures.
        prCustomerRow - RowID of the customer record to retrieve.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TABLE FOR ttCustomer.
  DEFINE INPUT  PARAMETER prCustomerRow AS ROWID NO-UNDO.

  EMPTY TEMP-TABLE ttCustomer NO-ERROR.

  IF prCustomerRow <> ? THEN
      FIND Customer WHERE ROWID(Customer) = prCustomerRow NO-LOCK NO-ERROR.
  ELSE
      FIND LAST Customer NO-LOCK NO-ERROR.  
  IF AVAILABLE Customer THEN
  DO:
      CREATE ttCustomer.
      BUFFER-COPY Customer TO ttCustomer.
      ASSIGN 
        ttCustomer.RowIdent = ROWID(Customer).
      RETURN.
  END.
  ELSE
      RETURN "Record has been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetInvoiceData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInvoiceData Procedure 
PROCEDURE GetInvoiceData :
/*------------------------------------------------------------------------------
  Purpose: Retrieve all the invoice records and pass back to the calling 
            procedure.    
  Parameters:  ttInvoice - Temp-table used to pass records between procedures.
               piKeyValue - The customer number for which invoices are required.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttInvoice.
    DEFINE INPUT  PARAMETER piKeyValue  AS INTEGER    NO-UNDO.

    EMPTY TEMP-TABLE ttInvoice NO-ERROR.
    
    IF piKeyValue = ? THEN
        FOR EACH Invoice NO-LOCK: 
            CREATE ttInvoice.
            BUFFER-COPY Invoice TO ttInvoice.
            ASSIGN ttInvoice.RowIdent = ROWID(Invoice).
        END.
    ELSE
        FOR EACH Invoice WHERE Invoice.CustNum = piKeyValue NO-LOCK: 
            CREATE ttInvoice.
            BUFFER-COPY Invoice TO ttInvoice.
            ASSIGN ttInvoice.RowIdent = ROWID(Invoice).
        END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetInvoiceRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInvoiceRecord Procedure 
PROCEDURE GetInvoiceRecord :
/*------------------------------------------------------------------------------
  Purpose: Retrieve a specific invoice record and pass it back to the calling 
            procedure.    
  Parameters:  
        ttInvoice - Temp-table used to pass record between procedures.
        prowInvoiceRow - RowID of the record to retrieve.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TABLE FOR ttInvoice.
  DEFINE INPUT  PARAMETER prInvoiceRow AS ROWID NO-UNDO.

  EMPTY TEMP-TABLE ttInvoice NO-ERROR.

  IF prInvoiceRow <> ? THEN
      FIND Invoice WHERE ROWID(Invoice) = prInvoiceRow NO-LOCK NO-ERROR.
  ELSE
      FIND LAST Invoice NO-LOCK NO-ERROR.  
  IF AVAILABLE Invoice THEN
  DO:
      CREATE ttInvoice.
      BUFFER-COPY Invoice TO ttInvoice.
      ASSIGN ttInvoice.RowIdent = ROWID(Invoice).
      RETURN.
  END.
  ELSE
      RETURN "Record has been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetItemData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetItemData Procedure 
PROCEDURE GetItemData :
/*------------------------------------------------------------------------------
  Purpose: Retrieve all the item records and pass back to the calling 
            procedure.    
  Parameters:  ttItem - Temp-table used to pass records between procedures.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttItem.

    EMPTY TEMP-TABLE ttItem NO-ERROR.
    FOR EACH Item NO-LOCK: 
        CREATE ttItem.
        BUFFER-COPY Item TO ttItem.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetNewLineNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetNewLineNum Procedure 
PROCEDURE GetNewLineNum :
/*------------------------------------------------------------------------------
  Purpose: Compute line number for new OrderLine    
  Parameters:  INPUT - piOrderNum = The OrderNum of the associated Order 
               OUTPUT - piLineNum = The generated Line Number
  Notes:  First Line Number is 1     
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER piOrderNum AS INTEGER    NO-UNDO.
    DEFINE OUTPUT  PARAMETER piLineNum AS INTEGER    NO-UNDO.
    FIND LAST OrderLine WHERE OrderLine.OrderNum = piOrderNum NO-LOCK NO-ERROR.
    IF AVAILABLE OrderLine THEN 
        piLineNum = OrderLine.LineNum + 1.
    ELSE
        piLineNum = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOrderData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOrderData Procedure 
PROCEDURE GetOrderData :
/*------------------------------------------------------------------------------
  Purpose: Retrieve all the order records and pass back to the calling 
            procedure.    
  Parameters:  ttOrder - Temp-table used to pass records between procedures.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttOrder.

    EMPTY TEMP-TABLE ttOrder NO-ERROR.

    FOR EACH Order NO-LOCK: 
        CREATE ttOrder.
        BUFFER-COPY Order TO ttOrder.
        ASSIGN ttOrder.rowIdent = ROWID(Order).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOrderlineData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOrderlineData Procedure 
PROCEDURE GetOrderlineData :
/*------------------------------------------------------------------------------
  Purpose: Retrieve the orderline records for a specific order and pass back 
            to the calling procedure.    
  Parameters:  ttOrderline - Temp-table used to pass records between procedures.
               piKeyValue - The OrderNum for which orderlines are required
  Notes: The procedure uses a query to find the orderlines for a specific
         order.     
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttOrderline.
    DEFINE INPUT  PARAMETER piKeyValue  AS INTEGER    NO-UNDO.

    EMPTY TEMP-TABLE ttOrderline NO-ERROR.

    FOR EACH Orderline WHERE Orderline.OrderNum = piKeyValue NO-LOCK: 
        CREATE ttOrderline.
        BUFFER-COPY Orderline TO ttOrderline.
        ASSIGN ttOrderline.rowIdent = ROWID(Orderline).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOrderlineRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOrderlineRecord Procedure 
PROCEDURE GetOrderlineRecord :
/*------------------------------------------------------------------------------
  Purpose: Retrieve a specific orderline record and pass it back to the calling 
            procedure.    
  Parameters:  
        ttOrderline - Temp-table used to pass record between procedures.
        prOrderlineRow - RowID of the record to retrieve.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TABLE FOR ttOrderLine.
  DEFINE INPUT  PARAMETER prOrderLineRow AS ROWID NO-UNDO.

  EMPTY TEMP-TABLE ttOrderLine NO-ERROR.

  IF prOrderLineRow <> ? THEN
      FIND OrderLine WHERE ROWID(OrderLine) = prOrderLineRow NO-LOCK NO-ERROR.
  ELSE
      FIND LAST OrderLine NO-LOCK NO-ERROR.  
  IF AVAILABLE OrderLine THEN
  DO:
      CREATE ttOrderLine.
      BUFFER-COPY OrderLine TO ttOrderLine.
      ASSIGN ttOrderLine.RowIdent = ROWID(OrderLine).
      RETURN.
  END.
  ELSE
      RETURN "Record has been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOrderRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOrderRecord Procedure 
PROCEDURE GetOrderRecord :
/*------------------------------------------------------------------------------
  Purpose: Retrieve a specific order record and pass it back to the calling 
            procedure.    
  Parameters:  
        ttOrder - Temp-table used to pass record between procedures.
        prOrderRow - RowID of the record to retrieve.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TABLE FOR ttOrder.
  DEFINE INPUT  PARAMETER prOrderRow AS ROWID NO-UNDO.

  EMPTY TEMP-TABLE ttOrder NO-ERROR.

  IF prOrderRow <> ? THEN
      FIND Order WHERE ROWID(Order) = prOrderRow NO-LOCK NO-ERROR.
  ELSE
      FIND LAST Order NO-LOCK NO-ERROR.  
  IF AVAILABLE Order THEN
  DO:
      CREATE ttOrder.
      BUFFER-COPY Order TO ttOrder.
      ASSIGN ttOrder.RowIdent = ROWID(Order).
      RETURN.
  END.
  ELSE
      RETURN "Record has been deleted!".           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetRepData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRepData Procedure 
PROCEDURE GetRepData :
/*------------------------------------------------------------------------------
  Purpose: Retrieve all the sales rep records and pass back to the calling 
            procedure.    
  Parameters:  ttSalesRep - Temp-table used to pass records between procedures.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttSalesRep.

    EMPTY TEMP-TABLE ttSalesRep NO-ERROR.

    FOR EACH SalesRep NO-LOCK: 
        CREATE ttSalesRep.
        BUFFER-COPY SalesRep TO ttSalesRep.
        ASSIGN ttSalesRep.RowIdent = ROWID(SalesRep).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetRepRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRepRecord Procedure 
PROCEDURE GetRepRecord :
/*------------------------------------------------------------------------------
  Purpose: Retrieve a specific sales rep record and pass it back to the calling 
            procedure.    
  Parameters:  
        ttSalesRep - Temp-table used to pass record between procedures.
        prSalesrepRow - RowID of the record to retrieve.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TABLE FOR ttSalesRep.
  DEFINE INPUT  PARAMETER prSalesRepRow AS ROWID NO-UNDO.

  EMPTY TEMP-TABLE ttSalesRep NO-ERROR.

  IF prSalesRepRow <> ? THEN
      FIND SalesRep WHERE ROWID(SalesRep) = prSalesRepRow NO-LOCK NO-ERROR.
  ELSE
      FIND LAST SalesRep NO-LOCK NO-ERROR.  
  IF AVAILABLE SalesRep THEN
  DO:
      CREATE ttSalesRep.
      BUFFER-COPY SalesRep TO ttSalesRep.
      ASSIGN ttSalesRep.RowIdent = ROWID(SalesRep).
      RETURN.
  END.
  ELSE
      RETURN "Record has been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveCustRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveCustRecord Procedure 
PROCEDURE SaveCustRecord :
/*------------------------------------------------------------------------------
  Purpose: Commit a customer record to the database.    
  Parameters:  
               ttCustomer - Temp-table used to pass record in to save and return
                            modified record to calling procedure.
               pcMode     - Identify whether this is a modified "Mod" or
                            new "New" record.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR  ttCustomer.
    DEFINE INPUT PARAMETER pcMode AS CHARACTER  NO-UNDO.
    
    FIND FIRST ttCustomer.
    DO TRANSACTION:
        IF pcMode = "New" THEN 
            CREATE Customer.
        ELSE
            FIND Customer WHERE ROWID(Customer) = ttCustomer.RowIdent 
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        /* Do the following for both new and modified records */
        IF AVAILABLE Customer THEN
            BUFFER-COPY ttCustomer EXCEPT RowIdent CustNum Orders TO Customer.
        ELSE
            IF LOCKED (Customer) THEN
                RETURN "Record is locked.  Try later.".
            ELSE 
                RETURN "Record has been deleted!".
    END. /* Transaction */
    FIND CURRENT Customer NO-LOCK.  /* Remove the lock */
    
    BUFFER-COPY Customer TO ttCustomer.
    ASSIGN 
        ttCustomer.Orders = iCounter  
        ttCustomer.rowIdent = ROWID(Customer).
    RETURN.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveInvoiceRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveInvoiceRecord Procedure 
PROCEDURE SaveInvoiceRecord :
/*------------------------------------------------------------------------------
  Purpose: Commit a invoice record to the database.    
  Parameters:  
               ttInvoice - Temp-table used to pass record in to save and return
                            modified record to calling procedure.
               pcMode     - Identify whether this is a modified "Mod" or
                            new "New" record.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR  ttInvoice.
    DEFINE INPUT PARAMETER pcMode AS CHARACTER  NO-UNDO.

    FIND FIRST ttInvoice.

    DO TRANSACTION:
        IF pcMode = "New":U THEN DO:
            IF CAN-FIND(Invoice WHERE Invoice.OrderNum = ttInvoice.OrderNum) THEN
            DO:
                EMPTY TEMP-TABLE ttInvoice.
                RETURN "Cannot duplicate invoice. Transaction aborted.".
            END.
            ELSE
                CREATE Invoice.
        END.
        ELSE
            FIND Invoice WHERE ROWID(Invoice) = ttInvoice.RowIdent 
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        /* Do the following for both new and modified records */
        IF AVAILABLE Invoice THEN
            BUFFER-COPY ttInvoice EXCEPT RowIdent InvoiceNum TO Invoice.
        ELSE
            IF LOCKED (Invoice) THEN
                RETURN "Record is locked.  Try later.".
            ELSE 
                RETURN "Record has been deleted!".
    END. /* Transaction */
    FIND CURRENT Invoice NO-LOCK.
    BUFFER-COPY Invoice TO ttInvoice.
    ttInvoice.rowIdent = ROWID(Invoice).
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveOrderlineRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveOrderlineRecord Procedure 
PROCEDURE SaveOrderlineRecord :
/*------------------------------------------------------------------------------
  Purpose: Commit a orderline record to the database.    
  Parameters:  
               ttOrderline - Temp-table used to pass record in to save and return
                            modified record to calling procedure.
               pcMode     - Identify whether this is a modified "Mod" or
                            new "New" record.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR  ttOrderLine.
    DEFINE INPUT PARAMETER pcMode AS CHARACTER  NO-UNDO.

    FIND FIRST ttOrderLine.
    DO TRANSACTION:
        IF pcMode = "New" THEN 
            CREATE OrderLine.
        ELSE
            FIND OrderLine WHERE ROWID(OrderLine) = ttOrderLine.RowIdent 
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        /* Do the following for both new and modified records */
        IF AVAILABLE OrderLine THEN
            BUFFER-COPY ttOrderLine EXCEPT RowIdent TO OrderLine.
        ELSE
            IF LOCKED (OrderLine) THEN
                RETURN "Record is locked.  Try later.".
            ELSE 
                RETURN "Record has been deleted!".
    END. /* Transaction */
    FIND CURRENT OrderLine NO-LOCK NO-ERROR.
    BUFFER-COPY OrderLine TO ttOrderLine.
    ttOrderLine.rowIdent = ROWID(OrderLine).
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveOrderRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveOrderRecord Procedure 
PROCEDURE SaveOrderRecord :
/*------------------------------------------------------------------------------
  Purpose: Commit a order record to the database.    
  Parameters:  
               ttOrder - Temp-table used to pass record in to save and return
                            modified record to calling procedure.
               pcMode     - Identify whether this is a modified "Mod" or
                            new "New" record.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR  ttOrder.
    DEFINE INPUT PARAMETER pcMode AS CHARACTER  NO-UNDO.

    FIND FIRST ttOrder.
    DO TRANSACTION:
        IF pcMode = "New" THEN 
            CREATE Order.
        ELSE
            FIND Order WHERE ROWID(Order) = ttOrder.RowIdent 
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        /* Do the following for both new and modified records */
        IF AVAILABLE Order THEN
            BUFFER-COPY ttOrder EXCEPT RowIdent OrderNum TO Order.
        ELSE
            IF LOCKED (Order) THEN
                RETURN "Record is locked.  Try later.".
            ELSE 
                RETURN "Record has been deleted!".
    END. /* Transaction */
    FIND CURRENT Order NO-LOCK NO-ERROR.
    BUFFER-COPY Order TO ttOrder.
    ttOrder.rowIdent = ROWID(Order).
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveRepRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRepRecord Procedure 
PROCEDURE SaveRepRecord :
/*------------------------------------------------------------------------------
  Purpose: Commit a sales rep record to the database.    
  Parameters:  
               ttSalesrep - Temp-table used to pass record in to save and return
                            modified record to calling procedure.
               pcMode     - Identify whether this is a modified "Mod" or
                            new "New" record.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR  ttSalesRep.
    DEFINE INPUT PARAMETER pcMode AS CHARACTER  NO-UNDO.

    FIND FIRST ttSalesRep.
    DO TRANSACTION:
        IF pcMode = "New" THEN 
            CREATE SalesRep.
        ELSE
            FIND SalesRep WHERE ROWID(SalesRep) = ttSalesRep.RowIdent 
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        /* Do the following for both new and modified records */
        IF AVAILABLE SalesRep THEN
            BUFFER-COPY ttSalesRep EXCEPT RowIdent TO SalesRep.
        ELSE
            IF LOCKED (SalesRep) THEN
                RETURN "Record is locked.  Try later.".
            ELSE 
                RETURN "Record has been deleted!".
    END. /* Transaction */
    FIND CURRENT SalesRep NO-LOCK NO-ERROR.
    BUFFER-COPY SalesRep TO ttSalesRep.
    ttSalesRep.rowIdent = ROWID(SalesRep).
    RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Shutdown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Shutdown Procedure 
PROCEDURE Shutdown :
/*------------------------------------------------------------------------------
  Purpose: Delete the procedure from memory if it is running persistently.    
  Parameters:  <none>
  Notes: Normally executed in response to a Shutdown event.      
------------------------------------------------------------------------------*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


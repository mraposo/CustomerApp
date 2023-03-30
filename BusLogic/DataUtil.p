&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/

/* Temp-Table and Buffer definitions                                    */

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
{ttTables.i}
{ttTables.i &Suffix=Upd}
/* ***************************  Definitions  ************************** */

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
      TABLE: ttOrder T "?" NO-UNDO Sports2000 Order
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

&IF DEFINED(EXCLUDE-DeleteOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteOrder Procedure
PROCEDURE DeleteOrder:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piOrderNum AS INTEGER NO-UNDO.

FIND Order WHERE Order.Ordernum = piOrderNum EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE Order THEN 
DO:
    DELETE Order.
    RETURN.
END.
ELSE IF LOCKED (Order) THEN
    RETURN "The record is locked. Try again later..".
  ELSE
    RETURN "Record has already been deleted!".
    

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
    DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttCustomer.
    FOR EACH Customer NO-LOCK:
        iCounter = 0.

        FOR EACH Order WHERE Order.CustNum = Customer.CustNum:
                iCounter = iCounter + 1.
        END.
        
        IF iCounter > 0 THEN 
        DO:  
            CREATE ttCustomer.
            BUFFER-COPY Customer TO ttCustomer.
            ASSIGN ttCustomer.Orders = iCounter
                   ttCustomer.RowIdent = ROWID(Customer).
        END.    
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
    DEFINE INPUT PARAMETER piNumOfOrders AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prCustomerRow AS ROWID NO-UNDO.

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
            ttCustomer.Orders = piNumOfOrders
            ttCustomer.RowIdent = ROWID(Customer).
        RETURN.
    END.
    ELSE
        RETURN "Record has been deleted!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/*
&IF DEFINED(EXCLUDE-GetInvoiceData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInvoiceData Procedure
PROCEDURE GetInvoiceData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  wordt niet gebruikt 
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER TABLE FOR ttInvoice.
DEFINE INPUT PARAMETER piValue AS INTEGER NO-UNDO.
MESSAGE piValue
VIEW-AS ALERT-BOX.
EMPTY TEMP-TABLE ttInvoice NO-ERROR.

FOR EACH Invoice WHERE Invoice.OrderNum = piValue NO-LOCK:
    CREATE ttInvoice.
    BUFFER-COPY Invoice TO ttInvoice.
    ASSIGN ttInvoice.RowIdent = ROWID(Invoice).
    RETURN.
END.    

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF
*/

&IF DEFINED(EXCLUDE-GetInvoiceRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInvoiceRecord Procedure
PROCEDURE GetInvoiceRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TABLE FOR ttInvoice.
  DEFINE INPUT  PARAMETER piOrderNum AS INTEGER NO-UNDO.

  EMPTY TEMP-TABLE ttInvoice NO-ERROR.

/*  IF piValue <> ? THEN                                               */
/*      FIND Invoice WHERE Invoice.OrderNum = piValue NO-LOCK NO-ERROR.*/
/*  ELSE                                                               */
/*      FIND LAST Invoice NO-LOCK NO-ERROR.                            */
/*  IF AVAILABLE Invoice THEN                                          */
/*  DO:                                                                */
/*      CREATE ttInvoice.                                              */
/*      BUFFER-COPY Invoice TO ttInvoice.                              */
/*      ASSIGN ttInvoice.RowIdent = ROWID(Invoice).                    */
/*      RETURN.                                                        */
/*  END.                                                               */
/*  ELSE                                                               */
/*      RETURN "Record has been deleted!".                             */

    FIND FIRST Invoice WHERE Invoice.OrderNum = piOrderNum NO-LOCK NO-ERROR.
    IF AVAILABLE Invoice THEN 
    DO:
        CREATE ttInvoice.
        BUFFER-COPY Invoice TO ttInvoice.
        ASSIGN ttInvoice.RowIdent = ROWID(Invoice).
        RETURN.
    END.
    ELSE DO:
        MESSAGE "Invoice not available" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY. // scherm blijft openen..  is niet de bedoeling als er geen invoice is.
    END.
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
                   piCustNum = Number of the Customer.
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttOrder.

    EMPTY TEMP-TABLE ttOrder NO-ERROR.
    
    FOR EACH Order NO-LOCK WHERE Order.CustNum = piCustNum: 
        CREATE ttOrder.
        BUFFER-COPY Order TO ttOrder.
        ASSIGN 
            ttOrder.rowIdent = ROWID(Order).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetOrderRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOrderRecord Procedure
PROCEDURE GetOrderRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttOrder.
    DEFINE INPUT PARAMETER piOrderNum AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttOrder.

    FIND Order NO-LOCK WHERE Order.Ordernum = piOrderNum.
    IF AVAILABLE Order THEN
    DO:
        CREATE ttOrder.
        BUFFER-COPY Order TO ttOrder.
        RETURN.
    END.
    ELSE
    DO:
        MESSAGE "Order not available" VIEW-AS ALERT-BOX.
        RETURN.
    END.

/*
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
*/


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
        ASSIGN 
            ttSalesRep.RowIdent = ROWID(SalesRep).
    END.
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
    DEFINE INPUT PARAMETER piNumOfOrders AS INTEGER NO-UNDO.
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
            BUFFER-COPY ttCustomer EXCEPT RowIdent CustNum TO Customer.
        ELSE
            IF LOCKED (Customer) THEN
                RETURN "Record is locked.  Try later.".
            ELSE 
                RETURN "Record has been deleted!".
    END. /* Transaction */
    FIND CURRENT Customer NO-LOCK.  /* Remove the lock */
    
    BUFFER-COPY Customer TO ttCustomer.
    ASSIGN 
        ttCustomer.rowIdent = ROWID(Customer)
        ttCustomer.Orders   = piNumOfOrders.  
    RETURN.    
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveOrderRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveOrderRecord Procedure
PROCEDURE SaveOrderRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrder.
DEFINE INPUT PARAMETER pcMode AS CHARACTER NO-UNDO.

FIND FIRST ttOrder.
    DO TRANSACTION:
        IF pcMode = "New" THEN 
            CREATE Order.
        ELSE
            FIND Order WHERE Order.Ordernum = ttOrder.Ordernum 
                EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        /* Do the following for both new and modified records */
        IF AVAILABLE Order THEN
            BUFFER-COPY ttOrder TO Order.
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


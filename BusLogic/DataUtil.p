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
{ttCustomer.i}
{ttCustomer.i &Suffix=Upd}
{ttTables.i}
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
        
        IF iCounter <> 0 THEN 
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


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME mainFrame

/* Temp-Table and Buffer definitions                                    */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS mainFrame 
/*------------------------------------------------------------------------

  File: 

  Description: Customer Maintenance

  Input Parameters:
      

  Output Parameters:
     

  Author: Mario Raposo 

  Created: Maart 2023
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ttTables.i}
{ttTables.i &Suffix=Upd}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pcMode        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phProcLib     AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER piNumOfOrders AS INTEGER NO-UNDO. // number of orders per customer 
DEFINE INPUT PARAMETER prowRowId     AS ROWID NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttCustomerUpd.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hDataUtil   AS HANDLE  NO-UNDO.
DEFINE VARIABLE lEmailCheck  AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME mainFrame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomerUpd

/* Definitions for DIALOG-BOX mainFrame                                 */
&Scoped-define FIELDS-IN-QUERY-mainFrame ttCustomerUpd.Name ~
ttCustomerUpd.Address ttCustomerUpd.Address2 ttCustomerUpd.City ~
ttCustomerUpd.State ttCustomerUpd.Country ttCustomerUpd.PostalCode ~
ttCustomerUpd.Phone ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep ~
ttCustomerUpd.CustNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-mainFrame ttCustomerUpd.Name ~
ttCustomerUpd.Address ttCustomerUpd.Address2 ttCustomerUpd.City ~
ttCustomerUpd.State ttCustomerUpd.Country ttCustomerUpd.PostalCode ~
ttCustomerUpd.Phone ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
&Scoped-define ENABLED-TABLES-IN-QUERY-mainFrame ttCustomerUpd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-mainFrame ttCustomerUpd
&Scoped-define QUERY-STRING-mainFrame FOR EACH ttCustomerUpd SHARE-LOCK
&Scoped-define OPEN-QUERY-mainFrame OPEN QUERY mainFrame FOR EACH ttCustomerUpd SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-mainFrame ttCustomerUpd
&Scoped-define FIRST-TABLE-IN-QUERY-mainFrame ttCustomerUpd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ttCustomerUpd.Name ttCustomerUpd.Address ~
ttCustomerUpd.Address2 ttCustomerUpd.City ttCustomerUpd.State ~
ttCustomerUpd.Country ttCustomerUpd.PostalCode ttCustomerUpd.Phone ~
ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
&Scoped-define ENABLED-TABLES ttCustomerUpd
&Scoped-define FIRST-ENABLED-TABLE ttCustomerUpd
&Scoped-Define ENABLED-OBJECTS btnSave Btn_Cancel RECT-19 
&Scoped-Define DISPLAYED-FIELDS ttCustomerUpd.Name ttCustomerUpd.Address ~
ttCustomerUpd.Address2 ttCustomerUpd.City ttCustomerUpd.State ~
ttCustomerUpd.Country ttCustomerUpd.PostalCode ttCustomerUpd.Phone ~
ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep ttCustomerUpd.CustNum 
&Scoped-define DISPLAYED-TABLES ttCustomerUpd
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomerUpd


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EmailValidation mainFrame 
FUNCTION EmailValidation RETURNS LOGICAL
  ( INPUT cEmail AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NameValidation mainFrame 
FUNCTION NameValidation RETURNS CHARACTER
  ( INPUT cNaam AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PostalcodeValidation mainFrame 
FUNCTION PostalcodeValidation RETURNS CHARACTER
  ( INPUT cPostalCode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateField mainFrame
FUNCTION ValidateField RETURNS LOGICAL 
  ( INPUT phFieldHandle AS HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSave 
     LABEL "Opslaan" 
     SIZE 15 BY 1.13.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Sluiten" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 13.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY mainFrame FOR 
      ttCustomerUpd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME mainFrame
     ttCustomerUpd.Name AT ROW 4 COL 15.75 WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Address AT ROW 5 COL 13.62 WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Address2 AT ROW 6 COL 12.5 WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     ttCustomerUpd.City AT ROW 7 COL 17.75 WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 14 
     ttCustomerUpd.State AT ROW 8 COL 16.12 WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomerUpd.Country AT ROW 9 COL 14.12 WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 14 
     ttCustomerUpd.PostalCode AT ROW 10 COL 10.25 WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Phone AT ROW 11 COL 15.25 WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomerUpd.EmailAddress AT ROW 12 COL 16.12 WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 14 
     ttCustomerUpd.SalesRep AT ROW 13 COL 20 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None"
          DROP-DOWN-LIST
          SIZE 29 BY 1
     btnSave AT ROW 2.5 COL 96 WIDGET-ID 30
     Btn_Cancel AT ROW 3.75 COL 96
     ttCustomerUpd.CustNum AT ROW 3 COL 12.25 WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     "Customer" VIEW-AS TEXT
          SIZE 10 BY .63 AT ROW 1.5 COL 8 WIDGET-ID 28
          FGCOLOR 1 FONT 6
     RECT-19 AT ROW 1.75 COL 3 WIDGET-ID 26
     SPACE(1.99) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "CustMaintanence"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomerUpd T "?" NO-UNDO sports2000 Customer
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
      TABLE: ttSalesrep T "?" NO-UNDO sports2000 Salesrep
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
          INDEX RowIdent RowIdent
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX mainFrame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME mainFrame:SCROLLABLE       = FALSE
       FRAME mainFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ttCustomerUpd.Address IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.Address2 IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.City IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.Country IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.CustNum IN FRAME mainFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ttCustomerUpd.EmailAddress IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.Name IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.Phone IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.PostalCode IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.State IN FRAME mainFrame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX mainFrame
/* Query rebuild information for DIALOG-BOX mainFrame
     _TblList          = "Temp-Tables.ttCustomerUpd"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX mainFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME mainFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mainFrame mainFrame
ON WINDOW-CLOSE OF FRAME mainFrame /* cDialogTitle */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave mainFrame
ON CHOOSE OF btnSave IN FRAME mainFrame /* Opslaan */
DO:
  RUN ProcessForm. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Name mainFrame
ON LEAVE OF ttCustomerUpd.Name IN FRAME mainFrame /* Name */
DO:
  ttCustomerUpd.Name:SCREEN-VALUE = NameValidation(ttCustomerUpd.NAME:INPUT-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.PostalCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.PostalCode mainFrame
ON LEAVE OF ttCustomerUpd.PostalCode IN FRAME mainFrame /* Postal Code */
DO:
  IF ttCustomerUpd.Country:INPUT-VALUE = "NL" OR  
     ttCustomerUpd.Country:INPUT-VALUE = "Nederland" OR
     ttCustomerUpd.Country:INPUT-VALUE = "Netherlands" THEN
  DO:
     ttCustomerUpd.PostalCode:SCREEN-VALUE = PostalcodeValidation(ttCustomerUpd.PostalCode:INPUT-VALUE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK mainFrame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitializeObjects.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI mainFrame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME mainFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI mainFrame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-mainFrame}
  GET FIRST mainFrame.
  IF AVAILABLE ttCustomerUpd THEN 
    DISPLAY ttCustomerUpd.Name ttCustomerUpd.Address ttCustomerUpd.Address2 
          ttCustomerUpd.City ttCustomerUpd.State ttCustomerUpd.Country 
          ttCustomerUpd.PostalCode ttCustomerUpd.Phone 
          ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep 
          ttCustomerUpd.CustNum 
      WITH FRAME mainFrame.
  ENABLE ttCustomerUpd.Name ttCustomerUpd.Address ttCustomerUpd.Address2 
         ttCustomerUpd.City ttCustomerUpd.State ttCustomerUpd.Country 
         ttCustomerUpd.PostalCode ttCustomerUpd.Phone 
         ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep btnSave Btn_Cancel 
         RECT-19 
      WITH FRAME mainFrame.
  VIEW FRAME mainFrame.
  {&OPEN-BROWSERS-IN-QUERY-mainFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects mainFrame 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 hDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN phProcLib, "DataUtil.p":U).

 DO WITH FRAME {&FRAME-NAME}:
    ttCustomerUpd.SalesRep:DELIMITER = ";":U.
    RUN GetRepData IN hDataUtil(OUTPUT TABLE ttSalesRep).
    FOR EACH ttSalesRep:
        ttCustomerUpd.SalesRep:ADD-LAST(ttSalesRep.RepName, ttSalesRep.SalesRep).
    END.
 END.
 
 IF pcMode = "Mod":U THEN
 DO:
    RUN GetCustRecord IN hDataUtil (OUTPUT TABLE ttCustomerUpd,
                                     INPUT piNumOfOrders,
                                     INPUT prowRowId).                                  
    IF RETURN-VALUE = "" THEN
        FIND FIRST ttCustomerUpd.
        FRAME {&FRAME-NAME}:TITLE = "Customer: ":U + ttCustomerUpd.Name.   
 END.                                   
 ELSE DO:
    CREATE ttCustomerUpd.
    FRAME {&FRAME-NAME}:TITLE = "New Customer".
 END.
 
 PUBLISH "CloseWindow".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessForm mainFrame 
PROCEDURE ProcessForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
 lEmailCheck = EmailValidation(ttCustomerUpd.EmailAddress:INPUT-VALUE).

 IF NOT ValidateField(ttCustomerUpd.Name:HANDLE)           THEN RETURN NO-APPLY.
 IF NOT ValidateField(ttCustomerUpd.Address:HANDLE)        THEN RETURN NO-APPLY.
 IF NOT ValidateField(ttCustomerUpd.City:HANDLE)           THEN RETURN NO-APPLY.
 IF NOT ValidateField(ttCustomerUpd.Country:HANDLE)        THEN RETURN NO-APPLY.
 IF NOT ValidateField(ttCustomerUpd.PostalCode:HANDLE)     THEN RETURN NO-APPLY.
 IF NOT ValidateField(ttCustomerUpd.EmailAddress:HANDLE)   THEN RETURN NO-APPLY.
 IF NOT ValidateField(ttCustomerUpd.SalesRep:HANDLE)       THEN RETURN NO-APPLY.
 IF NOT lEmailCheck THEN
    DO:
      MESSAGE "Emailadres klopt niet!" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO ttCustomerUpd.EmailAddress.
      RETURN NO-APPLY.
    END.     
 ELSE DO:
      MESSAGE "Weet u zeker dat u" ttCustomerUpd.Name:INPUT-VALUE "wilt opslaan?" 
           VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lAnswer AS LOGICAL.
                      
    IF lAnswer THEN
     DO:
        ASSIGN {&DISPLAYED-FIELDS}.
        RUN SaveCustRecord IN hDataUtil (INPUT-OUTPUT TABLE ttCustomerUpd,
                                         INPUT piNumOfOrders,
                                         INPUT pcMode).                                                           
       IF RETURN-VALUE <> "" THEN
       DO:
           MESSAGE RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN RETURN-VALUE.
       END.
          APPLY "END-ERROR":U TO SELF.
     END. // End lAnswer 
    ELSE 
       RETURN.
 END. // END ELSE STATEMENT
END.  // END FRAME
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EmailValidation mainFrame 
FUNCTION EmailValidation RETURNS LOGICAL
  ( INPUT cEmail AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iChar    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-length AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-left   AS CHARACTER FORMAT "x(50)" NO-UNDO .
    DEFINE VARIABLE v-right  AS CHARACTER FORMAT "x(50)" NO-UNDO .
    DEFINE VARIABLE v-at     AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-dot    AS INTEGER NO-UNDO.
    
    cEmail = TRIM(cEmail).
    v-length = LENGTH(cEmail).
    IF v-length < 5 THEN // Moet minimaal zijn: X@X.X
        RETURN FALSE.

    v-at =    INDEX(cEmail, "@").
    v-left =  SUBSTRING (cEmail, 1, (v-at - 1)).
    v-right = SUBSTRING(cEmail, (v-at + 1), (v-length - (v-at ))).
    v-dot =   INDEX(v-right,".").
    
    IF v-at = 0 OR v-dot = 0 OR length(v-left) = 0 OR length(v-right) = 0 THEN
    DO:
        RETURN FALSE.
    END.

    DO iChar = 1 TO LENGTH(v-left) :
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_" , CAPS(SUBSTRING(v-left,iChar,1))) = 0 THEN
        DO:
            RETURN FALSE.
        END.
    END.
    iChar = 0.
    DO iChar = 1 TO LENGTH(v-right) :
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-" , CAPS(SUBSTRING(v-right,iChar,1))) = 0 THEN
        DO:
            RETURN FALSE.
        END.
    END.

    RETURN TRUE. // Function return value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NameValidation mainFrame 
FUNCTION NameValidation RETURNS CHARACTER
  ( INPUT cNaam AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.

    IF cNaam = "" THEN RETURN "" .

    DO iCount = 1 TO NUM-ENTRIES(cNaam," "):
     ASSIGN 
        cOutput = cOutput +
                  CAPS(SUBSTRING(ENTRY(iCount,cNaam," "),1,1)) +
                  LC(SUBSTRING(ENTRY(iCount,cNaam," "),2,LENGTH(ENTRY(iCount,cNaam," ")))) + " " .
    END.

    RETURN cOutput.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PostalcodeValidation mainFrame 
FUNCTION PostalcodeValidation RETURNS CHARACTER
  ( INPUT cPostalCode AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  
    DO iCount = 1 TO NUM-ENTRIES(cPostalCode):
       cOutput = SUBSTRING(ENTRY(iCount,cPostalCode),1,4) + 
                 CAPS(SUBSTRING(ENTRY(iCount,cPostalCode),5,6)).
    
  cOutput = REPLACE(
            REPLACE(
            REPLACE(
            REPLACE(
            REPLACE(
            REPLACE(
            REPLACE(
            REPLACE(
            REPLACE(cOutput, ",", ""), ".", ""), "-", ""), ";", ""), "=", ""), "+", ""), "_", ""), " ", ""), "?", "").
             
        cOutput = SUBSTRING(cOutput, 1, 6).         
    END.
    RETURN cOutput.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateField mainFrame
FUNCTION ValidateField RETURNS LOGICAL 
  ( INPUT phFieldHandle AS HANDLE ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO INITIAL TRUE.
    
    IF phFieldHandle:SCREEN-VALUE = "" OR phFieldHandle:SCREEN-VALUE = ? THEN
        DO:
            MESSAGE phFieldHandle:LABEL "vergeten in te vullen!".   
            APPLY "ENTRY" TO phFieldHandle.
            lResult = FALSE. 
        END.        
    RETURN lResult.
END FUNCTION.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



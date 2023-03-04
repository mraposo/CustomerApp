&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME mainFrame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO LIKE Customer
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.
DEFINE TEMP-TABLE ttSalesrep NO-UNDO LIKE Salesrep
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS mainFrame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER pcMode       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER phProcLib    AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER prowRowId    AS ROWID NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttCustomerUpd.

/* Local Variable Definitions ---                                       */

//DEFINE VARIABLE glResponse AS LOGICAL NO-UNDO.

DEFINE VARIABLE ghDataUtil       AS HANDLE    NO-UNDO.
DEFINE VARIABLE gcDialogboxTitle AS CHARACTER INITIAL "New Customer" NO-UNDO.
DEFINE VARIABLE cEmail           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValidEmail      AS LOGICAL   NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnSave Btn_Sluiten RECT-19 
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
    (INPUT cEmail AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD nameValidation mainFrame 
FUNCTION nameValidation RETURNS CHARACTER
  ( INPUT cNaam AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSave 
     LABEL "&Opslaan" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Sluiten AUTO-END-KEY 
     LABEL "&Sluiten" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 12.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY mainFrame FOR 
      ttCustomerUpd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME mainFrame
     ttCustomerUpd.Name AT ROW 3.43 COL 16.8 WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Address AT ROW 4.52 COL 14.6 WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Address2 AT ROW 5.52 COL 13.6 WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     ttCustomerUpd.City AT ROW 6.52 COL 18.8 WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 14 
     ttCustomerUpd.State AT ROW 7.52 COL 17.2 WIDGET-ID 22
          LABEL "State"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomerUpd.Country AT ROW 8.52 COL 15.2 WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 14 
     ttCustomerUpd.PostalCode AT ROW 9.52 COL 11.2 WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Phone AT ROW 10.52 COL 16.2 WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ttCustomerUpd.EmailAddress AT ROW 11.52 COL 17.2 WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 14 
     ttCustomerUpd.SalesRep AT ROW 12.52 COL 12.8 WIDGET-ID 24
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None"
          DROP-DOWN-LIST
          SIZE 29 BY 1
     btnSave AT ROW 1.95 COL 93 WIDGET-ID 30
     Btn_Sluiten AT ROW 3.14 COL 93
     ttCustomerUpd.CustNum AT ROW 2.52 COL 13.2 WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     RECT-19 AT ROW 1.48 COL 3 WIDGET-ID 26
     SPACE(1.79) SKIP(0.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "gcDialogboxTitle"
         CANCEL-BUTTON Btn_Sluiten WIDGET-ID 100.


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
/* SETTINGS FOR COMBO-BOX ttCustomerUpd.SalesRep IN FRAME mainFrame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN ttCustomerUpd.State IN FRAME mainFrame
   ALIGN-L EXP-LABEL                                                    */
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
ON WINDOW-CLOSE OF FRAME mainFrame /* gcDialogboxTitle */
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
    DEFINE VARIABLE cNaam AS CHARACTER NO-UNDO.
    cNaam = nameValidation(ttCustomerUpd.NAME:INPUT-VALUE).
    ttCustomerUpd.NAME:SCREEN-VALUE = cNaam.
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
         ttCustomerUpd.EmailAddress ttCustomerUpd.SalesRep btnSave Btn_Sluiten 
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

    ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN phProcLib, "DataUtil.p":U).

    DO WITH FRAME {&FRAME-NAME}:
        ttCustomerUpd.SalesRep:DELIMITER = ";":U.
        RUN GetRepData IN ghDataUtil(OUTPUT TABLE ttSalesRep).
        FOR EACH ttSalesRep:
            ttCustomerUpd.SalesRep:ADD-LAST(ttSalesRep.RepName, ttSalesRep.SalesRep).
        END.
    END.
 
    IF pcMode = "Mod":U THEN
    DO:
        RUN GetCustRecord IN ghDataUtil (OUTPUT TABLE ttCustomerUpd,
            INPUT prowRowId).                                  
        IF RETURN-VALUE = "" THEN
            FIND FIRST ttCustomerUpd.
        gcDialogboxTitle = "Customer: " + ttCustomerUpd.Name.
    END.                                   
    ELSE 
    DO:
        CREATE ttCustomerUpd.
        ttCustomerUpd.Country = "".
    END.
 
    PUBLISH "CloseWindow".
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessForm mainFrame 
PROCEDURE ProcessForm :
/*------------------------------------------------------------------------------
      Purpose: Form Validation and Save Customer to the Database    
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lAnswer AS LOGICAL NO-UNDO.
      
    DO WITH FRAME {&FRAME-NAME}:
        cEmail = ttCustomerUpd.EmailAddress:INPUT-VALUE.
        lValidEmail = EmailValidation(cEmail).
  
        IF ttCustomerUpd.Name:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "Naam vergeten in te vullen!" VIEW-AS ALERT-BOX.  
            APPLY "ENTRY":U TO ttCustomerUpd.NAME IN FRAME {&FRAME-NAME}.
            RETURN.
        END.
        IF ttCustomerUpd.Address:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "Adres vergeten in te vullen!" VIEW-AS ALERT-BOX.   
            APPLY "ENTRY":U TO ttCustomerUpd.Address IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        
        IF ttCustomerUpd.City:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "Woonplaats vergeten in te vullen!" VIEW-AS ALERT-BOX. 
            APPLY "ENTRY":U TO ttCustomerUpd.City IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        
        IF ttCustomerUpd.Country:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "Land vergeten in te vullen!" VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.Country IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        
        IF ttCustomerUpd.PostalCode:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "Postcode vergeten in te vullen!" VIEW-AS ALERT-BOX. 
            APPLY "ENTRY":U TO ttCustomerUpd.PostalCode IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        
        IF ttCustomerUpd.EmailAddress:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "Email vergeten in te vullen!" VIEW-AS ALERT-BOX.  
            APPLY "ENTRY":U TO ttCustomerUpd.EmailAddress IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        IF SalesRep:INPUT-VALUE = "" THEN
        DO:
            MESSAGE "SalesRep vergeten in te vullen!" VIEW-AS ALERT-BOX. 
            APPLY "ENTRY":U TO SalesRep IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        IF NOT lValidEmail THEN
        DO:
            MESSAGE "Email Adres klopt niet!" VIEW-AS ALERT-BOX.
            APPLY "ENTRY":U TO ttCustomerUpd.EmailAddress IN FRAME {&FRAME-NAME}.    
            RETURN.
        END.
        
        ELSE 
        DO:
            MESSAGE "Wilt u" ttCustomerUpd.Name:INPUT-VALUE "opslaan?" 
                VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lAnswer.
            
            IF lAnswer THEN
            DO:
                ASSIGN {&DISPLAYED-FIELDS}.
                RUN SaveCustRecord IN ghDataUtil (INPUT-OUTPUT TABLE ttCustomerUpd,
                    INPUT pcMode).                             
                IF RETURN-VALUE <> "" THEN
                DO:
                    MESSAGE RETURN-VALUE
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN RETURN-VALUE.
                END.
                APPLY "END-ERROR":U TO SELF. //SLUITEN NA HET OPLSLAAN
            END. 
            ELSE 
            DO:
                FIND FIRST ttCustomerUpd.
                IF pcMode = "New" THEN
                DO:
                    pcMode = "Mod".
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EmailValidation mainFrame 
FUNCTION EmailValidation RETURNS LOGICAL
    (INPUT cEmail AS CHARACTER ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/    
    
    DEFINE VARIABLE nChar    AS INTEGER.
    DEFINE VARIABLE v-length AS INTEGER.
    DEFINE VARIABLE v-left   AS CHARACTER FORMAT "x(50)" NO-UNDO .
    DEFINE VARIABLE v-right  AS CHARACTER FORMAT "x(50)" NO-UNDO .
    DEFINE VARIABLE v-at     AS INTEGER.
    DEFINE VARIABLE v-dot    AS INTEGER.
    cEmail = TRIM(cEmail).
    v-length = LENGTH(cEmail).
    IF v-length< 5 THEN // Moet minimaal zijn: X@X.X
        RETURN FALSE.

    v-at = INDEX(cEmail, "@").
    v-left = SUBSTRING (cEmail, 1, (v-at - 1)).
    v-right = SUBSTRING(cEmail, (v-at + 1), (v-length - (v-at ))).
    v-dot = INDEX(v-right,".").
    DISPLAY v-left.
    DISPLAY v-right.

    IF v-at = 0 OR v-dot = 0 OR length(v-left) = 0 OR length(v-right) = 0 THEN
    DO:
        RETURN FALSE.
    END.


    DO nChar = 1 TO LENGTH(v-left) :
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_" , CAPS(SUBSTRING(v-left,nChar,1))) = 0 THEN
        DO:
            RETURN FALSE.
        END.
    END.
    nChar = 0.
    DO nChar = 1 TO LENGTH(v-right) :
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-" , CAPS(SUBSTRING(v-right,nChar,1))) = 0 THEN
        DO:
            RETURN FALSE.
        END.
    END.

    RETURN TRUE. // Function return value.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION nameValidation mainFrame 
FUNCTION nameValidation RETURNS CHARACTER
  ( INPUT cNaam AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  
  RETURN "test".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


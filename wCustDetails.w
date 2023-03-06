&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomers NO-UNDO LIKE Customer
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent
       .
DEFINE TEMP-TABLE ttSalesrep NO-UNDO LIKE Salesrep
       FIELD RowIdent AS ROWID
       INDEX RowIdent RowIdent
       .



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghProcLib AS HANDLE NO-UNDO.
DEFINE VARIABLE ghDataUtil AS HANDLE NO-UNDO.
DEFINE VARIABLE lLastNavButtons AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE lFirstNavButtons  AS LOGICAL INITIAL YES NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomers

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME ttCustomers.CustNum ~
ttCustomers.Name ttCustomers.Address ttCustomers.Address2 ttCustomers.State ~
ttCustomers.City ttCustomers.PostalCode ttCustomers.Country ~
ttCustomers.Phone ttCustomers.EmailAddress ttCustomers.SalesRep 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ttCustomers SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ttCustomers SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ttCustomers
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ttCustomers


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 BtnDone 
&Scoped-Define DISPLAYED-FIELDS ttCustomers.CustNum ttCustomers.Name ~
ttCustomers.Address ttCustomers.Address2 ttCustomers.State ttCustomers.City ~
ttCustomers.PostalCode ttCustomers.Country ttCustomers.Phone ~
ttCustomers.EmailAddress ttCustomers.SalesRep 
&Scoped-define DISPLAYED-TABLES ttCustomers
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomers


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Sluiten" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnFirst 
     IMAGE-UP FILE "adeicon/first.bmp":U
     LABEL "&First" 
     SIZE 5 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnLast 
     IMAGE-UP FILE "adeicon/last.bmp":U
     LABEL "&Last" 
     SIZE 5 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnNext 
     IMAGE-UP FILE "adeicon/next.bmp":U
     LABEL "&Next" 
     SIZE 5 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnPrev 
     IMAGE-UP FILE "adeicon/prev.bmp":U
     LABEL "&Prev" 
     SIZE 5 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 13.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ttCustomers SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnDone AT ROW 2.19 COL 80 WIDGET-ID 24
     BtnFirst AT ROW 3.38 COL 80 WIDGET-ID 26
     BtnPrev AT ROW 3.38 COL 85 WIDGET-ID 30
     BtnNext AT ROW 3.38 COL 90 WIDGET-ID 28
     BtnLast AT ROW 3.38 COL 95 WIDGET-ID 32
     ttCustomers.CustNum AT ROW 3.62 COL 19 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.Name AT ROW 4.57 COL 19 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.Address AT ROW 5.52 COL 19 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.Address2 AT ROW 6.48 COL 19 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.State AT ROW 7.43 COL 19 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.City AT ROW 8.38 COL 19 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.PostalCode AT ROW 9.33 COL 19 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.Country AT ROW 10.29 COL 19 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.Phone AT ROW 11.24 COL 19 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.EmailAddress AT ROW 12.19 COL 19 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
          FGCOLOR 1 FONT 6
     ttCustomers.SalesRep AT ROW 13.14 COL 19 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None"
          DROP-DOWN-LIST
          SIZE 30 BY 1
          FGCOLOR 1 FONT 6
     "Details" VIEW-AS TEXT
          SIZE 8 BY .95 AT ROW 1.24 COL 11 WIDGET-ID 36
          FGCOLOR 9 FONT 6
     RECT-14 AT ROW 1.71 COL 3 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.4 BY 15.67
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomers T "?" NO-UNDO sports2000 Customer
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

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer Details"
         HEIGHT             = 15.24
         WIDTH              = 106.2
         MAX-HEIGHT         = 19.71
         MAX-WIDTH          = 111.8
         VIRTUAL-HEIGHT     = 19.71
         VIRTUAL-WIDTH      = 111.8
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN ttCustomers.Address IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.Address2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnFirst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnLast IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnNext IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnPrev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.City IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.Country IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.CustNum IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.EmailAddress IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.Name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.Phone IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.PostalCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX ttCustomers.SalesRep IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomers.State IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "Temp-Tables.ttCustomers"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer Details */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer Details */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Sluiten */
DO:
  
      APPLY "CLOSE":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFirst C-Win
ON CHOOSE OF BtnFirst IN FRAME DEFAULT-FRAME /* First */
DO:
      FIND FIRST ttCustomers NO-LOCK.
      IF NOT CAN-FIND(ttCustomers) THEN 
        lFirstNavButtons = NO.
        
      DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
      PUBLISH "CustBrowseNavigation":U(ttCustomers.CustNum, "First").
      PUBLISH "fetchOrders":U(ttCustomers.CustNum,ttCustomers.NAME).  //updates the Order Window
      
      lLastNavButtons = YES.
      RUN SetWindowName(ttCustomers.NAME).
      RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLast C-Win
ON CHOOSE OF BtnLast IN FRAME DEFAULT-FRAME /* Last */
DO:
     FIND LAST ttCustomers NO-LOCK.
     IF NOT CAN-FIND(ttCustomers) THEN 
         lLastNavButtons = NO.
      DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
      PUBLISH "CustBrowseNavigation":U(ttCustomers.CustNum, "Last").
      PUBLISH "fetchOrders":U(ttCustomers.CustNum,ttCustomers.NAME).
      
      lFirstNavButtons = YES.
      RUN SetWindowName(ttCustomers.NAME).
      RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext C-Win
ON CHOOSE OF BtnNext IN FRAME DEFAULT-FRAME /* Next */
DO:
     FIND NEXT ttCustomers NO-LOCK.
     IF AVAILABLE ttCustomers THEN
     DO:
        DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}. 
     END.
     ELSE
        IF NOT AVAILABLE ttCustomers THEN 
            lLastNavButtons = YES.

  
  PUBLISH "CustBrowseNavigation":U(ttCustomers.CustNum, "Next").
  PUBLISH "fetchOrders":U(ttCustomers.CustNum,Customer.NAME).
  
    lFirstNavButtons = YES.
  RUN SetWindowName(ttCustomers.NAME).
  RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev C-Win
ON CHOOSE OF BtnPrev IN FRAME DEFAULT-FRAME /* Prev */
DO:
    FIND PREV ttCustomers NO-LOCK.
     IF AVAILABLE ttCustomers THEN
     DO:
        DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}. 
     END.
     ELSE
         IF NOT AVAILABLE ttCustomers THEN 
        lFirstNavButtons = YES.
    PUBLISH "CustBrowseNavigation":U(ttCustomers.CustNum, "Prev").
    PUBLISH "fetchOrders":U(ttCustomers.CustNum,ttCustomers.NAME).
    
    lLastNavButtons = YES. 
    RUN SetWindowName(ttCustomers.NAME).
    RUN SetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitializeObjects.
  RUN enable_UI .
  SUBSCRIBE TO "FindCustomer" IN SOURCE-PROCEDURE.
  SUBSCRIBE TO "CloseWindow" ANYWHERE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseWindow C-Win 
PROCEDURE CloseWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE ttCustomers THEN 
    DISPLAY ttCustomers.CustNum ttCustomers.Name ttCustomers.Address 
          ttCustomers.Address2 ttCustomers.State ttCustomers.City 
          ttCustomers.PostalCode ttCustomers.Country ttCustomers.Phone 
          ttCustomers.EmailAddress ttCustomers.SalesRep 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-14 BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindCustomer C-Win 
PROCEDURE FindCustomer :
/*------------------------------------------------------------------------------
  Purpose: Find the customer and display the customer    
  Parameters:  piCustNum = Customer Number 
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
      
   FIND FIRST ttCustomers WHERE ttCustomers.CustNum = piCustNum NO-LOCK.
   DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
   {&window-name}:TITLE = 'Customer: ' + ttCustomers.NAME.
   
   RUN SetButtons.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects C-Win 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN PersistentProc.p PERSISTENT SET ghProcLib.
 ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN ghProcLib, "DataUtil.p":U).
 
 RUN GetCustData IN ghDataUtil (OUTPUT TABLE ttCustomers).
 
 DO WITH FRAME {&FRAME-NAME}:
    ttCustomers.SalesRep:DELIMITER = ";":U.
    RUN GetRepData IN ghDataUtil(OUTPUT TABLE ttSalesRep).
    FOR EACH ttSalesRep:
        ttCustomers.SalesRep:ADD-LAST(ttSalesRep.RepName, ttSalesRep.SalesRep).
    END.
 END.
 
 RUN SetButtons.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetButtons C-Win 
PROCEDURE SetButtons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF lFirstNavButtons THEN
 DO:
    ENABLE 
        btnFirst
        btnPrev
        WITH FRAME {&FRAME-NAME}.  
 END.
 ELSE DO:              
        DISABLE
        btnFirst            
        btnPrev
        WITH FRAME {&FRAME-NAME}.
  
 END. 
 
IF lLastNavButtons THEN
 DO:
    ENABLE 
        btnLast
        btnNext
        WITH FRAME {&FRAME-NAME}.  
 END.
 ELSE DO:              
        DISABLE
        btnLast
        btnNext
        WITH FRAME {&FRAME-NAME}.
  
 END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWindowName C-Win 
PROCEDURE SetWindowName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcCustName AS CHARACTER NO-UNDO.
  
  {&window-name}:TITLE = 'Customer: ' + pcCustName.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


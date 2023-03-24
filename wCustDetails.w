&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Mario Raposo

  Created: Maart 2023

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
{ttCustomer.i}
{ttCustomer.i &Suffix=Upd}
{ttTables.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghProcLib         AS HANDLE NO-UNDO.
DEFINE VARIABLE ghDataUtil        AS HANDLE NO-UNDO.
DEFINE VARIABLE lLastNavButtons   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lFirstNavButtons  AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME ttCustomer.CustNum ~
ttCustomer.Name ttCustomer.Address ttCustomer.Address2 ttCustomer.State ~
ttCustomer.City ttCustomer.PostalCode ttCustomer.Country ttCustomer.Phone ~
ttCustomer.EmailAddress ttCustomer.SalesRep 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ttCustomer SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ttCustomer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ttCustomer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 BtnDone 
&Scoped-Define DISPLAYED-FIELDS ttCustomer.CustNum ttCustomer.Name ~
ttCustomer.Address ttCustomer.Address2 ttCustomer.State ttCustomer.City ~
ttCustomer.PostalCode ttCustomer.Country ttCustomer.Phone ~
ttCustomer.EmailAddress ttCustomer.SalesRep 
&Scoped-define DISPLAYED-TABLES ttCustomer
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomer


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
     SIZE 20 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnFirst 
     IMAGE-UP FILE "adeicon/first.bmp":U
     LABEL "&First" 
     SIZE 5 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnLast 
     IMAGE-UP FILE "adeicon/last.bmp":U
     LABEL "&Last" 
     SIZE 5 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnNext 
     IMAGE-UP FILE "adeicon/next.bmp":U
     LABEL "&Next" 
     SIZE 5 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BtnPrev 
     IMAGE-UP FILE "adeicon/prev.bmp":U
     LABEL "&Prev" 
     SIZE 5 BY 1.12
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 12.54.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnDone AT ROW 2.27 COL 80 WIDGET-ID 24
     ttCustomer.CustNum AT ROW 2.62 COL 19 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          FGCOLOR 1 FONT 6
     BtnFirst AT ROW 3.42 COL 80 WIDGET-ID 26
     BtnPrev AT ROW 3.42 COL 85 WIDGET-ID 30
     BtnNext AT ROW 3.42 COL 90 WIDGET-ID 28
     BtnLast AT ROW 3.42 COL 95 WIDGET-ID 32
     ttCustomer.Name AT ROW 3.58 COL 19 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.Address AT ROW 4.54 COL 19 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.Address2 AT ROW 5.46 COL 19 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.State AT ROW 6.42 COL 19 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.City AT ROW 7.38 COL 19 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.PostalCode AT ROW 8.35 COL 19 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 15.57 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.Country AT ROW 9.27 COL 19 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.Phone AT ROW 10.27 COL 19 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.EmailAddress AT ROW 11.19 COL 19 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 52 BY 1
          FGCOLOR 1 FONT 6
     ttCustomer.SalesRep AT ROW 12.12 COL 19 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None"
          DROP-DOWN-LIST
          SIZE 30 BY 1
          FGCOLOR 1 FONT 6
     "Details" VIEW-AS TEXT
          SIZE 8 BY .92 AT ROW 1.5 COL 12 WIDGET-ID 36
          FGCOLOR 9 FONT 6
     RECT-14 AT ROW 2 COL 3 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 105.38 BY 13.84
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer Details"
         HEIGHT             = 13.88
         WIDTH              = 106.29
         MAX-HEIGHT         = 19.73
         MAX-WIDTH          = 111.72
         VIRTUAL-HEIGHT     = 19.73
         VIRTUAL-WIDTH      = 111.72
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/customer.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/customer.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN ttCustomer.Address IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Address2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnFirst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnLast IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnNext IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BtnPrev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.City IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Country IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.CustNum IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.EmailAddress IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Name IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.Phone IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.PostalCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX ttCustomer.SalesRep IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttCustomer.State IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "Temp-Tables.ttCustomer"
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
      PUBLISH "CustBrowseNavigation":U("First").
      PUBLISH "fetchOrders":U(ttCustomer.CustNum,ttCustomer.NAME).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLast C-Win
ON CHOOSE OF BtnLast IN FRAME DEFAULT-FRAME /* Last */
DO:
      PUBLISH "CustBrowseNavigation":U("Last").
      PUBLISH "fetchOrders":U(ttCustomer.CustNum,ttCustomer.NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext C-Win
ON CHOOSE OF BtnNext IN FRAME DEFAULT-FRAME /* Next */
DO:
     PUBLISH "CustBrowseNavigation":U("Next").
     PUBLISH "fetchOrders":U(ttCustomer.CustNum,Customer.NAME).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev C-Win
ON CHOOSE OF BtnPrev IN FRAME DEFAULT-FRAME /* Prev */
DO:
    PUBLISH "CustBrowseNavigation":U("Prev").
    PUBLISH "fetchOrders":U(ttCustomer.CustNum,ttCustomer.NAME).
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
    SUBSCRIBE TO "FetchCustomer":U  IN SOURCE-PROCEDURE.
    SUBSCRIBE TO "SetButtons":U     IN SOURCE-PROCEDURE.
    SUBSCRIBE TO "CloseWindow":U ANYWHERE.
    SUBSCRIBE TO "Shutdown":U    ANYWHERE.
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
  IF AVAILABLE ttCustomer THEN 
    DISPLAY ttCustomer.CustNum ttCustomer.Name ttCustomer.Address 
          ttCustomer.Address2 ttCustomer.State ttCustomer.City 
          ttCustomer.PostalCode ttCustomer.Country ttCustomer.Phone 
          ttCustomer.EmailAddress ttCustomer.SalesRep 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-14 BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FetchCustomer C-Win 
PROCEDURE FetchCustomer :
/*------------------------------------------------------------------------------
  Purpose: Find the customer, display the customer & change the title of the window    
  Parameters:  piCustNum = Customer Number 
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
      
   FIND FIRST ttCustomer WHERE ttCustomer.CustNum = piCustNum NO-LOCK.
   DISPLAY {&DISPLAYED-FIELDS} WITH FRAME {&FRAME-NAME}.
   {&window-name}:TITLE = 'Customer: ' + ttCustomer.NAME.
  
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
 
 RUN GetCustData IN ghDataUtil (OUTPUT TABLE ttCustomer).
 
 DO WITH FRAME {&FRAME-NAME}:
    ttCustomer.SalesRep:DELIMITER = ";":U.
    RUN GetRepData IN ghDataUtil(OUTPUT TABLE ttSalesRep).
    FOR EACH ttSalesRep:
        ttCustomer.SalesRep:ADD-LAST(ttSalesRep.RepName, ttSalesRep.SalesRep).
    END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetButtons C-Win 
PROCEDURE SetButtons :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
 ------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER pcValue AS CHARACTER NO-UNDO.
    CASE pcValue:
        WHEN "FirstOff" THEN 
            lFirstNavButtons = NO.  
        WHEN "FirstOn" THEN  
            lFirstNavButtons = YES.    
        WHEN "LastOff" THEN 
            lLastNavButtons = NO.
        WHEN "LastOn" THEN  
            lLastNavButtons = YES.    
    END CASE.
          
    IF lFirstNavButtons THEN
    DO:
        ENABLE 
            btnFirst
            btnPrev
            WITH FRAME {&FRAME-NAME}.  
    END.
    ELSE 
    DO:              
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
    ELSE 
    DO:              
        DISABLE
            btnLast
            btnNext
            WITH FRAME {&FRAME-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Shutdown C-Win 
PROCEDURE Shutdown :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    IF THIS-PROCEDURE:PERSISTENT THEN
        DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME PDC-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Order
       FIELD RowIdent AS ROWID.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS PDC-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Mario Raposo

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
{ttCustomer.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE ghProcLib     AS HANDLE NO-UNDO.
DEFINE VARIABLE ghDataUtil    AS HANDLE NO-UNDO.
DEFINE VARIABLE hDetails      AS HANDLE NO-UNDO.
DEFINE VARIABLE hOrders       AS HANDLE NO-UNDO.

//DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO LIKE ttCustomer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brCustomers

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for BROWSE brCustomers                                   */
&Scoped-define FIELDS-IN-QUERY-brCustomers ttCustomer.CustNum ttCustomer.Name ttCustomer.Orders   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCustomers   
&Scoped-define SELF-NAME brCustomers
&Scoped-define QUERY-STRING-brCustomers FOR EACH ttCustomer NO-LOCK
&Scoped-define OPEN-QUERY-brCustomers OPEN QUERY {&SELF-NAME} FOR EACH ttCustomer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brCustomers ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-brCustomers ttCustomer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brCustomers}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brCustomers fiCustNum fiCustName fiSalesRep ~
btnOrders 
&Scoped-Define DISPLAYED-OBJECTS fiCustNum fiCustName fiSalesRep 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR PDC-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_File 
       MENU-ITEM m_New          LABEL "New"           
       MENU-ITEM m_Edit         LABEL "Edit"          
       MENU-ITEM m_Delete       LABEL "Delete"        
       RULE
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE SUB-MENU m_Navigation 
       MENU-ITEM m_First        LABEL "First"         
       MENU-ITEM m_Last         LABEL "Last"          .

DEFINE SUB-MENU m_Help 
       MENU-ITEM m_Version      LABEL "Version"       .

DEFINE MENU MENU-BAR-PDC-Win MENUBAR
       SUB-MENU  m_File         LABEL "File"          
       SUB-MENU  m_Navigation   LABEL "Navigation"    
       SUB-MENU  m_Help         LABEL "Help"          .

DEFINE MENU POPUP-MENU-brCustomers 
       MENU-ITEM m_brEdit       LABEL "Edit"          
       MENU-ITEM m_brDelete     LABEL "Delete"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOrders 
     LABEL "Orders" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustNum AS INTEGER FORMAT "->,>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiSalesRep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCustomers FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCustomers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCustomers PDC-Win _FREEFORM
  QUERY brCustomers NO-LOCK DISPLAY
      ttCustomer.CustNum FORMAT ">>>>9":U WIDTH 12.25
      ttCustomer.Name FORMAT "x(30)":U WIDTH 40.25
      ttCustomer.Orders FORMAT ">>>>9":U WIDTH 12.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 85 BY 11.44 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brCustomers AT ROW 1.47 COL 5 WIDGET-ID 200
     fiCustNum AT ROW 12.91 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fiCustName AT ROW 12.91 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiSalesRep AT ROW 15 COL 14 COLON-ALIGNED WIDGET-ID 26
     btnOrders AT ROW 15 COL 74 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.25 BY 15.66 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttOrder T "?" NO-UNDO sports2000 Order
      ADDITIONAL-FIELDS:
          FIELD RowIdent AS ROWID
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW PDC-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customers"
         HEIGHT             = 15.75
         WIDTH              = 92
         MAX-HEIGHT         = 39.66
         MAX-WIDTH          = 156
         VIRTUAL-HEIGHT     = 39.66
         VIRTUAL-WIDTH      = 156
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-PDC-Win:HANDLE.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT PDC-Win:LOAD-ICON("adeicon/progress.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/progress.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW PDC-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brCustomers 1 DEFAULT-FRAME */
ASSIGN 
       brCustomers:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-brCustomers:HANDLE
       brCustomers:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(PDC-Win)
THEN PDC-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCustomers
/* Query rebuild information for BROWSE brCustomers
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCustomer NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE brCustomers */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME PDC-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PDC-Win PDC-Win
ON END-ERROR OF PDC-Win /* Customers */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PDC-Win PDC-Win
ON WINDOW-CLOSE OF PDC-Win /* Customers */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCustomers
&Scoped-define SELF-NAME brCustomers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers PDC-Win
ON ENTRY OF brCustomers IN FRAME DEFAULT-FRAME
DO:
  APPLY "VALUE-CHANGED" TO brCustomers.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers PDC-Win
ON MOUSE-SELECT-DBLCLICK OF brCustomers IN FRAME DEFAULT-FRAME
DO:  
    IF NOT VALID-HANDLE(hDetails) THEN
      RUN wCustDetails.w PERSISTENT SET hDetails.
      
    PUBLISH "FetchCustomer"(ttCustomer.CustNum).
    APPLY "VALUE-CHANGED" TO brCustomers.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers PDC-Win
ON START-SEARCH OF brCustomers IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE hSortColumn  AS WIDGET-HANDLE.
  DEFINE VARIABLE hQueryHandle AS HANDLE NO-UNDO.

  DEFINE VARIABLE iCount AS INTEGER.
  DEFINE VARIABLE iRowNumber AS INTEGER.
  DEFINE VARIABLE rowRowId AS ROWID.
  
  rowRowId = ROWID(ttCustomer).
  DO iCount = 1 TO brCustomers:NUM-ITERATIONS:
    IF brCustomers:IS-ROW-SELECTED(iCount) THEN LEAVE. 
  END.
  
  hSortColumn  = BROWSE brCustomers:CURRENT-COLUMN.
  hQueryHandle = BROWSE brCustomers:QUERY.
  hQueryHandle:QUERY-CLOSE().
  hQueryHandle:QUERY-PREPARE("FOR EACH ttCustomer NO-LOCK BY " + hSortColumn:NAME).
  hQueryHandle:QUERY-OPEN().
  
  brCustomers:SET-REPOSITIONED-ROW(iCount,"ALWAYS").
  REPOSITION brCustomers TO ROWID rowRowId.
  iRowNumber = brCustomers:GET-REPOSITIONED-ROW().
  
  APPLY "VALUE-CHANGED" TO brCustomers.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers PDC-Win
ON VALUE-CHANGED OF brCustomers IN FRAME DEFAULT-FRAME
DO:

    FOR EACH Salesrep WHERE SalesRep.SalesRep = ttCustomer.SalesRep NO-LOCK:
    fiSalesRep = SalesRep.Repname.
    END.
  
    DISPLAY fiSalesRep WITH FRAME {&FRAME-NAME}. 
    RUN "NavButtonSwitch".
    PUBLISH "fetchOrders"(ttCustomer.CustNum,ttCustomer.NAME).
    PUBLISH "FetchCustomer"(ttCustomer.CustNum).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOrders PDC-Win
ON CHOOSE OF btnOrders IN FRAME DEFAULT-FRAME /* Orders */
DO:
    IF NOT VALID-HANDLE(hOrders) THEN
    DO:
      RUN wOrders.w PERSISTENT SET hOrders.  
      PUBLISH "fetchOrders"(ttCustomer.CustNum,ttCustomer.NAME) .
      
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustName PDC-Win
ON VALUE-CHANGED OF fiCustName IN FRAME DEFAULT-FRAME
DO:
      IF fiCustNum:INPUT-VALUE = 0 THEN DO:         
        IF (fiCustName:INPUT-VALUE = "") THEN 
        DO:
            OPEN QUERY brCustomers FOR EACH ttCustomer.
        END.
        ELSE
            OPEN QUERY brCustomers FOR EACH ttCustomer 
                WHERE ttCustomer.NAME BEGINS fiCustName:INPUT-VALUE.
      END.
      
      IF fiCustNum:INPUT-VALUE <> 0 THEN DO:
          IF (fiCustName:INPUT-VALUE = "") THEN 
          DO:
              OPEN QUERY brCustomers FOR EACH ttCustomer. 
              APPLY "VALUE-CHANGED" TO fiCustNum.
          END.
          ELSE
              OPEN QUERY brCustomers FOR EACH ttCustomer
              WHERE ttCustomer.Name BEGINS fiCustName:INPUT-VALUE AND
                    ttCustomer.CustNum >= fiCustNum:INPUT-VALUE.                 
      END. 
              
    APPLY "VALUE-CHANGED" TO brCustomers.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustNum PDC-Win
ON VALUE-CHANGED OF fiCustNum IN FRAME DEFAULT-FRAME
DO:
        DEFINE VARIABLE iInputValue AS INTEGER NO-UNDO.
        iInputValue = fiCustNum:INPUT-VALUE.
        
        IF iInputValue = 0 THEN   
        DO:
            OPEN QUERY brCustomers FOR EACH ttCustomer NO-LOCK.
        END.
        ELSE 
            OPEN QUERY brCustomers FOR EACH ttCustomer 
                WHERE ttCustomer.CustNum >= iInputValue NO-LOCK.
        
        APPLY "VALUE-CHANGED" TO brCustomers.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_brDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_brDelete PDC-Win
ON CHOOSE OF MENU-ITEM m_brDelete /* Delete */
DO:
  RUN DelCustomer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_brEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_brEdit PDC-Win
ON CHOOSE OF MENU-ITEM m_brEdit /* Edit */
DO:
  RUN EditCustomer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete PDC-Win
ON CHOOSE OF MENU-ITEM m_Delete /* Delete */
DO:
  RUN DelCustomer.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Edit PDC-Win
ON CHOOSE OF MENU-ITEM m_Edit /* Edit */
DO:
  RUN EditCustomer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit PDC-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_First PDC-Win
ON CHOOSE OF MENU-ITEM m_First /* First */
DO:
   GET FIRST brCustomers NO-LOCK.
    IF AVAILABLE ttCustomer THEN
        REPOSITION brCustomers TO ROWID ROWID(ttCustomer).
    APPLY "VALUE-CHANGED" TO brCustomers IN FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Last PDC-Win
ON CHOOSE OF MENU-ITEM m_Last /* Last */
DO:
    GET LAST brCustomers NO-LOCK.
    IF AVAILABLE ttCustomer THEN
        brCustomers:SET-REPOSITIONED-ROW(13) IN FRAME {&FRAME-NAME}.
        REPOSITION brCustomers TO ROWID ROWID(ttCustomer).
    APPLY "VALUE-CHANGED" TO brCustomers IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_New
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_New PDC-Win
ON CHOOSE OF MENU-ITEM m_New /* New */
DO:
  RUN NewCustomer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Version
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Version PDC-Win
ON CHOOSE OF MENU-ITEM m_Version /* Version */
DO:
  RUN gVersion.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK PDC-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
   DO:
    PUBLISH "Shutdown". 
    RUN disable_UI.
  END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN InitializeObjects.
   SUBSCRIBE TO "CustBrowseNavigation" ANYWHERE.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CustBrowseNavigation PDC-Win 
PROCEDURE CustBrowseNavigation :
/*------------------------------------------------------------------------------
  Purpose: Re-positions the browse with nav buttons on the wCustDetails.w window            
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pcCustChanged AS CHARACTER NO-UNDO.
  
  CASE pcCustChanged:
  WHEN "First" THEN
  DO:
    GET FIRST brCustomers NO-LOCK.
    REPOSITION brCustomers TO ROWID ROWID(ttCustomer).
  END.
  WHEN "Prev" THEN
  DO:
    BROWSE brCustomers:SELECT-PREV-ROW() NO-ERROR.
  END.
  WHEN "Next" THEN
  DO:
    BROWSE brCustomers:SELECT-NEXT-ROW() NO-ERROR.
  END.
  WHEN "Last" THEN
  DO:
    GET LAST brCustomers NO-LOCK.
    brCustomers:SET-REPOSITIONED-ROW(13) IN FRAME {&FRAME-NAME}.
    REPOSITION brCustomers TO ROWID ROWID(ttCustomer).
  END.
 END CASE.
 APPLY "VALUE-CHANGED" TO brCustomers IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelCustomer PDC-Win 
PROCEDURE DelCustomer :
/*------------------------------------------------------------------------------
      Purpose:  Delete Customer     
      Parameters:  
      Notes:       
------------------------------------------------------------------------------*/
    
    MESSAGE "Wilt u" ttCustomer.Name "verwijderen?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lResponse AS LOGICAL.

 IF lResponse THEN
  DO:
      RUN DeleteCustomer IN ghDataUtil(INPUT ttCustomer.RowIdent).
      IF RETURN-VALUE = "" OR RETURN-VALUE MATCHES "*deleted*" THEN
      DO:
          DELETE ttCustomer.
          brCustomers:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.
      END.
      ELSE
      DO:
          MESSAGE RETURN-VALUE 
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      END. 
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI PDC-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(PDC-Win)
  THEN DELETE WIDGET PDC-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditCustomer PDC-Win 
PROCEDURE EditCustomer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.
   DEFINE VARIABLE iCount      AS INTEGER.

   DO iCount = 1 TO brCustomers:NUM-ITERATIONS IN FRAME {&FRAME-NAME}:
        IF brCustomers:IS-ROW-SELECTED(iCount) THEN LEAVE. 
   END. 
   RUN gCustMaint.w ( INPUT "Mod":U,  
                      INPUT ghProcLib,
                      INPUT ttCustomer.Orders,
                      INPUT ttCustomer.RowIdent,
                      OUTPUT TABLE ttCustomerUpd).
                                        
    FIND FIRST ttCustomerUpd.
    rowRowIdent = ttCustomerUpd.RowIdent.
    BUFFER-COPY ttCustomerUpd TO ttCustomer.

    brCustomers:SET-REPOSITIONED-ROW(iCount) IN FRAME {&FRAME-NAME}.
    FIND ttCustomer WHERE ttCustomer.rowIdent = rowRowIdent.
    REPOSITION brCustomers TO ROWID ROWID(ttCustomer) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI PDC-Win  _DEFAULT-ENABLE
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
  DISPLAY fiCustNum fiCustName fiSalesRep 
      WITH FRAME DEFAULT-FRAME IN WINDOW PDC-Win.
  ENABLE brCustomers fiCustNum fiCustName fiSalesRep btnOrders 
      WITH FRAME DEFAULT-FRAME IN WINDOW PDC-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW PDC-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects PDC-Win 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE iCounter AS INTEGER NO-UNDO.
 
 RUN PersistentProc.p PERSISTENT SET ghProcLib.
 ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN ghProcLib, "DataUtil.p":U).
 
 RUN GetCustData  IN ghDataUtil (OUTPUT TABLE ttCustomer).
 RUN GetOrderData IN ghDataUtil (OUTPUT TABLE ttOrder).

 brCustomers:LOAD-MOUSE-POINTER("Glove") IN FRAME {&FRAME-NAME}.
 APPLY "VALUE-CHANGED" TO brCustomers.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NavButtonSwitch PDC-Win 
PROCEDURE NavButtonSwitch :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCurrentRowInBrowse     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iLastRowInBrowse        AS INTEGER NO-UNDO.
    
    iCurrentRowInBrowse = CURRENT-RESULT-ROW("brCustomers").
    iLastRowInBrowse = NUM-RESULTS("brCustomers").   

    IF iCurrentRowInBrowse = 1 THEN 
        PUBLISH "SetButtons"("FirstOff").
    ELSE
        PUBLISH "SetButtons"("FirstOn").
        
    IF iCurrentRowInBrowse = iLastRowInBrowse THEN 
        PUBLISH "SetButtons"("LastOff").
    ELSE 
        PUBLISH "SetButtons"("LastOn").   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewCustomer PDC-Win 
PROCEDURE NewCustomer :
/*------------------------------------------------------------------------------
  Purpose: Add a new Customer   
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.
   
   RUN gCustMaint.w ( INPUT "New":U,  
                      INPUT ghProcLib,
                      INPUT ttCustomer.Orders,
                      INPUT ttCustomer.RowIdent,
                      OUTPUT TABLE ttCustomerUpd).
                      
  FIND FIRST ttCustomerUpd.  //find record returned.
  
  rowRowIdent = ttCustomerUpd.RowIdent.  // save Rowid
  
  IF rowRowIdent <> ? THEN DO:
    CREATE ttCustomer.
    BUFFER-COPY ttCustomerUpd TO ttCustomer.
    
    /* ---  
    New Customer wordt niet in de browse getoont aangezien hij toch geen orders heeft...   
    
    FIND ttCustomer WHERE ttCustomer.rowident = rowRowIdent.
    brCustomers:SET-REPOSITIONED-ROW(13) IN FRAME {&FRAME-NAME}. // hardcoded 13 = positie in de browse
    REPOSITION brCustomers TO ROWID ROWID(ttCustomer) NO-ERROR.
    */  
  END.
                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



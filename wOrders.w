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
     

  Output Parameters:
   
   
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
{ttTables.i}
{ttTables.i &Suffix=Upd}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hProcLib   AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataUtil  AS HANDLE NO-UNDO.
DEFINE VARIABLE iCustNum    AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brOrders

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOrder

/* Definitions for BROWSE brOrders                                      */
&Scoped-define FIELDS-IN-QUERY-brOrders ttOrder.CustNum ttOrder.Ordernum ttOrder.OrderStatus ttOrder.Creditcard ttOrder.Carrier   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brOrders   
&Scoped-define SELF-NAME brOrders
&Scoped-define QUERY-STRING-brOrders FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brOrders OPEN QUERY {&SELF-NAME} FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brOrders ttOrder
&Scoped-define FIRST-TABLE-IN-QUERY-brOrders ttOrder


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brOrders}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 brOrders btnNewOrder BtnDone 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-brOrders 
       MENU-ITEM m_Invoice      LABEL "Invoice"       
       RULE
       MENU-ITEM m_Edit         LABEL "Edit Order"    
       MENU-ITEM m_Delete       LABEL "Delete Order"  .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Sluiten" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON btnNewOrder 
     LABEL "New Order" 
     SIZE 15 BY 1.12.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 14.12.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brOrders FOR 
      ttOrder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brOrders C-Win _FREEFORM
  QUERY brOrders NO-LOCK DISPLAY
      ttOrder.CustNum     FORMAT ">>>>9":U
      ttOrder.Ordernum    FORMAT "zzzzzzzzz9":U
      ttOrder.OrderStatus FORMAT "x(20)":U
      ttOrder.Creditcard  FORMAT "x(25)":U
      ttOrder.Carrier     FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 10.77 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brOrders AT ROW 2.31 COL 7 WIDGET-ID 200
     btnNewOrder AT ROW 13.92 COL 7 WIDGET-ID 8
     BtnDone AT ROW 13.92 COL 82 WIDGET-ID 2
     "Orders" VIEW-AS TEXT
          SIZE 8 BY .92 AT ROW 1 COL 10 WIDGET-ID 6
          FGCOLOR 9 FONT 6
     RECT-15 AT ROW 1.46 COL 3 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.63 BY 17.22
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
         TITLE              = "Orders"
         HEIGHT             = 14.81
         WIDTH              = 99.57
         MAX-HEIGHT         = 17.54
         MAX-WIDTH          = 156.57
         VIRTUAL-HEIGHT     = 17.54
         VIRTUAL-WIDTH      = 156.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/orders.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/orders.ico"
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
/* BROWSE-TAB brOrders RECT-15 DEFAULT-FRAME */
ASSIGN 
       brOrders:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-brOrders:HANDLE
       brOrders:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brOrders
/* Query rebuild information for BROWSE brOrders
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brOrders */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brOrders
&Scoped-define SELF-NAME brOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brOrders C-Win
ON DEFAULT-ACTION OF brOrders IN FRAME DEFAULT-FRAME
DO:
  RUN gOrderDetails.w (INPUT ttOrder.Ordernum, INPUT "View":U, OUTPUT TABLE ttOrderUpd).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brOrders C-Win
ON START-SEARCH OF brOrders IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE iCount       AS INTEGER.
    DEFINE VARIABLE rowRowId     AS ROWID.

    rowRowId = ROWID(ttOrder).
    DO iCount = 1 TO {&BROWSE-NAME}:NUM-ITERATIONS IN FRAME {&FRAME-NAME}:
        IF {&BROWSE-NAME}:IS-ROW-SELECTED(iCount) THEN LEAVE.
    END.

    RUN SortOrders("ttOrder","","BY " + BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME).
    
    {&BROWSE-NAME}:SET-REPOSITIONED-ROW(iCount).
    REPOSITION {&BROWSE-NAME} TO ROWID rowRowId.  
      
  APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
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


&Scoped-define SELF-NAME btnNewOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNewOrder C-Win
ON CHOOSE OF btnNewOrder IN FRAME DEFAULT-FRAME /* New Order */
DO:
  /*
  DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.  
  RUN gOrderMaint.w (INPUT ttOrder.Ordernum, 
                     INPUT "New":U,
                     OUTPUT TABLE ttOrderUpd).
                     
  //FIND FIRST ttOrderUpd.
  //CREATE ttOrder.
  //BUFFER-COPY ttOrderUpd TO ttOrder.
  */
    DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER.
    RUN gOrderMaint.w (INPUT ttOrder.Ordernum, 
                       INPUT "New":U,
                       INPUT ttOrder.RowIdent, 
                       OUTPUT TABLE ttOrderUpd).
    
    //FIND FIRST ttOrder.
    //rowRowIdent = ttOrderUpd.RowIdent.
    //BUFFER-COPY ttOrderUpd TO ttOrder.

    //brOrders:SET-REPOSITIONED-ROW(iCount) IN FRAME {&FRAME-NAME}. 
    //FIND ttOrder WHERE ttOrder.rowIdent = rowRowIdent.
    //REPOSITION brOrders TO ROWID ROWID(ttOrder) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Delete C-Win
ON CHOOSE OF MENU-ITEM m_Delete /* Delete Order */
DO:
   MESSAGE "Wilt u deze bestelling verwijderen?"   
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lAnswer AS LOGICAL .
   
   IF lAnswer THEN 
   DO:
    RUN DeleteOrder IN hDataUtil (INPUT ttOrder.Ordernum).
    brOrders:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Edit C-Win
ON CHOOSE OF MENU-ITEM m_Edit /* Edit Order */
DO:
    DEFINE VARIABLE rowRowIdent AS ROWID NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER.
    
    DO iCount = 1 TO {&BROWSE-NAME}:NUM-ITERATIONS IN FRAME {&FRAME-NAME}:
        IF {&BROWSE-NAME}:IS-ROW-SELECTED(iCount) THEN LEAVE. 
    END.
    
    RUN gOrderMaint.w (INPUT ttOrder.Ordernum, 
                       INPUT "Edit":U, 
                       INPUT ttOrder.RowIdent, 
                       OUTPUT TABLE ttOrderUpd).
    
    FIND FIRST ttOrderUpd.
    rowRowIdent = ttOrderUpd.RowIdent.
    BUFFER-COPY ttOrderUpd TO ttOrder.

    brOrders:SET-REPOSITIONED-ROW(iCount) IN FRAME {&FRAME-NAME}. 
    FIND ttOrder WHERE ttOrder.rowIdent = rowRowIdent.
    REPOSITION brOrders TO ROWID ROWID(ttOrder) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Invoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Invoice C-Win
ON CHOOSE OF MENU-ITEM m_Invoice /* Invoice */
DO:
      RUN gInvoice.w (INPUT ttOrder.Ordernum).
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
   SUBSCRIBE TO "fetchOrders":U IN SOURCE-PROCEDURE.
   SUBSCRIBE TO "Shutdown":U    ANYWHERE.
  RUN InitializeObjects.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  ENABLE RECT-15 brOrders btnNewOrder BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fetchOrders C-Win 
PROCEDURE fetchOrders :
/*------------------------------------------------------------------------------
  Purpose:  Fetch orders from wMain.w  and change the title dynamicly    
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pcCustName AS CHARACTER NO-UNDO.
    
    RUN GetOrderData IN hDataUtil (INPUT piCustNum, OUTPUT TABLE ttOrder).
     
   {&OPEN-QUERY-brOrders}
      
   {&window-name}:TITLE = 'Orders for: ' + pcCustName.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects C-Win 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN PersistentProc.p PERSISTENT SET hProcLib.
    hDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN hProcLib, "DataUtil.p":U).
    
    {&BROWSE-NAME}:LOAD-MOUSE-POINTER("Glove") IN FRAME {&FRAME-NAME}.
    //APPLY "VALUE-CHANGED" TO brOrders IN FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortOrders C-Win
PROCEDURE SortOrders:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER pcTableName      AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER pcWhereClause    AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER pcSort           AS CHARACTER NO-UNDO.

 DEFINE VARIABLE hQuery      AS HANDLE      NO-UNDO.
 DEFINE VARIABLE cPredicate  AS CHARACTER   NO-UNDO.
 
 cPredicate = SUBSTITUTE("FOR EACH &1 NO-LOCK &2 &3":U,pcTableName,pcWhereClause,pcSort).
               
 hQuery = BROWSE {&BROWSE-NAME}:QUERY.
 hQuery:QUERY-CLOSE().
 hQuery:QUERY-PREPARE(cPredicate).
 hQuery:QUERY-OPEN().

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



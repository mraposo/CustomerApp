&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME PDC-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomers NO-UNDO LIKE Customer
       FIELD RowIdent AS ROWID.
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

DEFINE VARIABLE hDetails      AS HANDLE NO-UNDO.
DEFINE VARIABLE hOrders       AS HANDLE NO-UNDO.
DEFINE VARIABLE hBrowse       AS HANDLE NO-UNDO. // to check first of browse
DEFINE VARIABLE glResponse    AS LOGICAL NO-UNDO.
DEFINE VARIABLE gcWhereClause AS CHARACTER  NO-UNDO.
DEFINE VARIABLE gcSortClause  AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO LIKE ttCustomers.

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
&Scoped-define INTERNAL-TABLES ttCustomers

/* Definitions for BROWSE brCustomers                                   */
&Scoped-define FIELDS-IN-QUERY-brCustomers ttCustomers.CustNum ~
ttCustomers.Name ttCustomers.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCustomers 
&Scoped-define QUERY-STRING-brCustomers FOR EACH ttCustomers NO-LOCK
&Scoped-define OPEN-QUERY-brCustomers OPEN QUERY brCustomers FOR EACH ttCustomers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brCustomers ttCustomers
&Scoped-define FIRST-TABLE-IN-QUERY-brCustomers ttCustomers


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brCustomers}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 brCustomers fiCustNum fiCustName ~
fiComments btnOrders 
&Scoped-Define DISPLAYED-OBJECTS fiCustNum fiCustName fiComments fiSalesRep ~
fiOrders 

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

DEFINE SUB-MENU m_Sort_By 
       MENU-ITEM m_Cust_Num     LABEL "Cust Num"      
       MENU-ITEM m_Name         LABEL "Name"          
       MENU-ITEM m_Contact      LABEL "Comments"      .

DEFINE SUB-MENU m_Action 
       SUB-MENU  m_Sort_By      LABEL "Sort By"       .

DEFINE SUB-MENU m_Help 
       MENU-ITEM m_Version      LABEL "Version"       .

DEFINE MENU MENU-BAR-PDC-Win MENUBAR
       SUB-MENU  m_File         LABEL "File"          
       SUB-MENU  m_Navigation   LABEL "Navigation"    
       SUB-MENU  m_Action       LABEL "Action"        
       SUB-MENU  m_Help         LABEL "Help"          .

DEFINE MENU POPUP-MENU-brCustomers 
       MENU-ITEM m_brEdit       LABEL "Edit"          
       MENU-ITEM m_brDelete     LABEL "Delete"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOrders 
     LABEL "Orders" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiComments AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE fiCustNum AS INTEGER FORMAT "->,>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrders AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Orders" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiSalesRep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCustomers FOR 
      ttCustomers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCustomers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCustomers PDC-Win _STRUCTURED
  QUERY brCustomers NO-LOCK DISPLAY
      ttCustomers.CustNum FORMAT ">>>>9":U WIDTH 12.25
      ttCustomers.Name FORMAT "x(30)":U WIDTH 35.75
      ttCustomers.Comments FORMAT "x(80)":U WIDTH 46
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 11.44 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brCustomers AT ROW 1.47 COL 5 WIDGET-ID 200
     fiCustNum AT ROW 12.91 COL 3 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fiCustName AT ROW 12.91 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fiComments AT ROW 12.91 COL 52 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fiSalesRep AT ROW 14.56 COL 15 COLON-ALIGNED WIDGET-ID 26
     btnOrders AT ROW 15.28 COL 88 WIDGET-ID 8
     fiOrders AT ROW 15.53 COL 15 COLON-ALIGNED WIDGET-ID 28
     RECT-21 AT ROW 14.34 COL 5 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.6 BY 19.62 WIDGET-ID 100.


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
      END-FIELDS.
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
         HEIGHT             = 16.38
         WIDTH              = 107.38
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW PDC-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brCustomers RECT-21 DEFAULT-FRAME */
ASSIGN 
       brCustomers:POPUP-MENU IN FRAME DEFAULT-FRAME             = MENU POPUP-MENU-brCustomers:HANDLE
       brCustomers:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN fiOrders IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSalesRep IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(PDC-Win)
THEN PDC-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCustomers
/* Query rebuild information for BROWSE brCustomers
     _TblList          = "Temp-Tables.ttCustomers"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ttCustomers.CustNum
"CustNum" ? ? "integer" ? ? ? ? ? ? no ? no no "12.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ttCustomers.Name
"Name" ? ? "character" ? ? ? ? ? ? no ? no no "35.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ttCustomers.Comments
"Comments" ? ? "character" ? ? ? ? ? ? no ? no no "46" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
      PUBLISH "FindCustomer"(ttCustomers.CustNum).
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
  
  rowRowId = ROWID(ttCustomers).
  DO iCount = 1 TO brCustomers:NUM-ITERATIONS:
    IF brCustomers:IS-ROW-SELECTED(iCount) THEN LEAVE. 
  END.
  
  hSortColumn = BROWSE brCustomers:CURRENT-COLUMN.
  hQueryHandle = BROWSE brCustomers:QUERY.
  hQueryHandle:QUERY-CLOSE().
  hQueryHandle:QUERY-PREPARE("FOR EACH ttCustomers NO-LOCK BY " + hSortColumn:NAME).
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
    fiOrders = 0.
    
    FOR EACH ttOrder WHERE ttOrder.CustNum = ttCustomers.CustNum:
     fiOrders = fiOrders + 1.
    END.
      
    FOR EACH Salesrep WHERE SalesRep.SalesRep = ttCustomers.SalesRep:
    fiSalesRep = SalesRep.Repname.
    END.
  
    DISPLAY fiSalesRep fiOrders WITH FRAME {&FRAME-NAME}. 
 
    PUBLISH "fetchOrders"(ttCustomers.CustNum,ttCustomers.NAME).
    PUBLISH "FindCustomer"(ttCustomers.CustNum).
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
      PUBLISH "fetchOrders"(ttCustomers.CustNum,ttCustomers.NAME) .
      
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiComments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiComments PDC-Win
ON VALUE-CHANGED OF fiComments IN FRAME DEFAULT-FRAME
DO:
    IF (fiComments:INPUT-VALUE = "") THEN 
    DO:
       OPEN QUERY brCustomers FOR EACH ttCustomers.
     END.
     ELSE
        OPEN QUERY brCustomers FOR EACH ttCustomers 
        WHERE ttCustomers.Comments MATCHES "*" + fiComments:INPUT-VALUE + "*".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustName PDC-Win
ON VALUE-CHANGED OF fiCustName IN FRAME DEFAULT-FRAME
DO:
        IF (fiCustName:INPUT-VALUE = "") THEN 
        DO:
            OPEN QUERY brCustomers FOR EACH ttCustomers.
        END.
        ELSE
            OPEN QUERY brCustomers FOR EACH ttCustomers 
                WHERE ttCustomers.NAME BEGINS fiCustName:INPUT-VALUE.
            
    //MESSAGE fiName:INPUT-VALUE VIEW-AS ALERT-BOX.  
    

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
            OPEN QUERY brCustomers FOR EACH ttCustomers NO-LOCK.
            //MESSAGE iInputValue VIEW-AS ALERT-BOX.
        END.
        ELSE 
            OPEN QUERY brCustomers FOR EACH ttCustomers 
                WHERE ttCustomers.CustNum >= iInputValue NO-LOCK.
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


&Scoped-define SELF-NAME m_Contact
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Contact PDC-Win
ON CHOOSE OF MENU-ITEM m_Contact /* Comments */
DO:
  OPEN QUERY brCustomers FOR EACH ttCustomers BY Comments.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cust_Num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cust_Num PDC-Win
ON CHOOSE OF MENU-ITEM m_Cust_Num /* Cust Num */
DO:
   OPEN QUERY brCustomers FOR EACH ttCustomers BY CustNum.
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
    IF AVAILABLE ttCustomers THEN
        REPOSITION brCustomers TO ROWID ROWID(ttCustomers). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Last PDC-Win
ON CHOOSE OF MENU-ITEM m_Last /* Last */
DO:
    GET LAST brCustomers NO-LOCK.
    IF AVAILABLE ttCustomers THEN
        REPOSITION brCustomers TO ROWID ROWID(ttCustomers). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Name PDC-Win
ON CHOOSE OF MENU-ITEM m_Name /* Name */
DO:
  OPEN QUERY brCustomers FOR EACH ttCustomers BY NAME.
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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN InitializeObjects.
   SUBSCRIBE TO "CustBrowseNavigation" ANYWHERE.
   SUBSCRIBE TO "ReopenQuery" ANYWHERE.
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
  Purpose: Repositions the browse with nav buttons on the wCustDetails.w window            
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piCustNum AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER pcCustChanged AS CHARACTER NO-UNDO.
  
  CASE pcCustChanged:
    WHEN "First" THEN
     DO:
        GET FIRST brCustomers NO-LOCK.
        IF AVAILABLE ttCustomers THEN
        REPOSITION brCustomers TO ROWID ROWID(ttCustomers).
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
        IF AVAILABLE ttCustomers THEN
        brCustomers:SET-REPOSITIONED-ROW(13) IN FRAME {&FRAME-NAME}.
        REPOSITION brCustomers TO ROWID ROWID(ttCustomers).
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
   MESSAGE "Wilt u" ttCustomers.Name "verwijderen?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE glResponse.

 IF glResponse THEN
  DO:
      RUN DeleteCustomer IN ghDataUtil(INPUT ttCustomers.RowIdent).
      IF RETURN-VALUE = "" OR RETURN-VALUE MATCHES "*deleted*" THEN
      DO:
          DELETE ttCustomers.
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
   
   RUN gCustMaint.w ( INPUT "Mod":U,  
                      INPUT ghProcLib,
                      INPUT ttCustomers.RowIdent,
                      OUTPUT TABLE ttCustomerUpd).
                                        
    FIND FIRST ttCustomerUpd.
    
    rowRowIdent = ttCustomerUpd.RowIdent.
    
    BUFFER-COPY ttCustomerUpd TO ttCustomers.
    
    RUN ReopenQuery.
    FIND ttCustomers WHERE ttCustomers.rowIdent = rowRowIdent.
    REPOSITION brCustomers TO ROWID ROWID(ttCustomers) NO-ERROR.
    
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
  DISPLAY fiCustNum fiCustName fiComments fiSalesRep fiOrders 
      WITH FRAME DEFAULT-FRAME IN WINDOW PDC-Win.
  ENABLE RECT-21 brCustomers fiCustNum fiCustName fiComments btnOrders 
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
 RUN PersistentProc.p PERSISTENT SET ghProcLib.
 ghDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN ghProcLib, "DataUtil.p":U).
 
 RUN GetCustData IN ghDataUtil (OUTPUT TABLE ttCustomers).
 RUN GetOrderData IN ghDataUtil (OUTPUT TABLE ttOrder).

 brCustomers:LOAD-MOUSE-POINTER("Glove") IN FRAME {&FRAME-NAME}.
 
 APPLY "VALUE-CHANGED" TO brCustomers.

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
                      INPUT ttCustomers.RowIdent,
                      OUTPUT TABLE ttCustomerUpd).
                      
  FIND FIRST ttCustomerUpd.  //find record returned.
  
  rowRowIdent = ttCustomerUpd.RowIdent.  // save Rowid
  
  IF rowRowIdent <> ? THEN DO:
  
    CREATE ttCustomers.
    BUFFER-COPY ttCustomerUpd TO ttCustomers.
    RUN ReopenQuery.
    
    FIND ttCustomers WHERE ttCustomers.rowident = rowRowIdent.
    
    REPOSITION brCustomers TO ROWID ROWID(ttCustomers) NO-ERROR.
  END.                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReopenQuery PDC-Win 
PROCEDURE ReopenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   QUERY brCustomers:QUERY-PREPARE(
        SUBSTITUTE("FOR EACH ttCustomers NO-LOCK &1":U, gcSortClause)).
   QUERY brCustomers:QUERY-OPEN(). 
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
//DEFINE TEMP-TABLE ttOrderUpd NO-UNDO LIKE Order.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
{ttTables.i}
{ttTables.i &Suffix=Upd}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER piOrderNum AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER pcMode     AS CHARACTER NO-UNDO.
//DEFINE INPUT PARAMETER phProcLib     AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER rowRowId AS ROWID NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttOrderUpd.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE hProcLib     AS HANDLE NO-UNDO.
DEFINE VARIABLE hDataUtil    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOrderUpd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ttOrderUpd.CustNum ~
ttOrderUpd.Ordernum ttOrderUpd.Creditcard ttOrderUpd.BillToID ttOrderUpd.Instructions ~
ttOrderUpd.OrderDate ttOrderUpd.OrderStatus ttOrderUpd.Carrier ttOrderUpd.PO ~
ttOrderUpd.PromiseDate ttOrderUpd.SalesRep ttOrderUpd.ShipDate ttOrderUpd.ShipToID ~
ttOrderUpd.Terms ttOrderUpd.WarehouseNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ttOrderUpd.Creditcard ~
ttOrderUpd.BillToID ttOrderUpd.Instructions ttOrderUpd.OrderDate ttOrderUpd.OrderStatus ~
ttOrderUpd.Carrier ttOrderUpd.PO ttOrderUpd.PromiseDate ttOrderUpd.SalesRep ~
ttOrderUpd.ShipDate ttOrderUpd.ShipToID ttOrderUpd.Terms ttOrderUpd.WarehouseNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ttOrderUpd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ttOrderUpd
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ttOrderUpd SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ttOrderUpd SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ttOrderUpd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ttOrderUpd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ttOrderUpd.Creditcard ttOrderUpd.BillToID ~
ttOrderUpd.Instructions ttOrderUpd.OrderDate ttOrderUpd.OrderStatus ttOrderUpd.Carrier ~
ttOrderUpd.PO ttOrderUpd.PromiseDate ttOrderUpd.SalesRep ttOrderUpd.ShipDate ~
ttOrderUpd.ShipToID ttOrderUpd.Terms ttOrderUpd.WarehouseNum 
&Scoped-define ENABLED-TABLES ttOrderUpd
&Scoped-define FIRST-ENABLED-TABLE ttOrderUpd
&Scoped-Define ENABLED-OBJECTS RECT-1 btnSave BtnExit 
&Scoped-Define DISPLAYED-FIELDS ttOrderUpd.CustNum ttOrderUpd.Ordernum ~
ttOrderUpd.Creditcard ttOrderUpd.BillToID ttOrderUpd.Instructions ttOrderUpd.OrderDate ~
ttOrderUpd.OrderStatus ttOrderUpd.Carrier ttOrderUpd.PO ttOrderUpd.PromiseDate ~
ttOrderUpd.SalesRep ttOrderUpd.ShipDate ttOrderUpd.ShipToID ttOrderUpd.Terms ~
ttOrderUpd.WarehouseNum 
&Scoped-define DISPLAYED-TABLES ttOrderUpd
&Scoped-define FIRST-DISPLAYED-TABLE ttOrderUpd


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnExit DEFAULT 
     LABEL "Exit" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON btnSave 
     LABEL "Save" 
     SIZE 15 BY 1.12.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 17.77.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ttOrderUpd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ttOrderUpd.CustNum AT ROW 2.35 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 7.43 BY 1
     btnSave AT ROW 2.35 COL 66 WIDGET-ID 38
     ttOrderUpd.Ordernum AT ROW 3.42 COL 18 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     BtnExit AT ROW 3.69 COL 66 WIDGET-ID 34
     ttOrderUpd.Creditcard AT ROW 4.5 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Visa","American Express","Master Card" 
          DROP-DOWN-LIST
          SIZE 25 BY 1
     ttOrderUpd.BillToID AT ROW 5.58 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttOrderUpd.Instructions AT ROW 6.65 COL 18 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 42.72 BY 1
     ttOrderUpd.OrderDate AT ROW 7.73 COL 18 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 10.86 BY 1
     ttOrderUpd.OrderStatus AT ROW 8.81 COL 18 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Ordered","Back Ordered","Partially Shipped","Shipped" 
          DROP-DOWN-LIST
          SIZE 25 BY 1
     ttOrderUpd.Carrier AT ROW 9.88 COL 18 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 26.72 BY 1
     ttOrderUpd.PO AT ROW 10.96 COL 18 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 21.72 BY 1
     ttOrderUpd.PromiseDate AT ROW 12.04 COL 18 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 10.86 BY 1
     ttOrderUpd.SalesRep AT ROW 13.12 COL 18 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 8.72 BY 1
     ttOrderUpd.ShipDate AT ROW 14.19 COL 18 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttOrderUpd.ShipToID AT ROW 15.27 COL 18 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttOrderUpd.Terms AT ROW 16.35 COL 18 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 21.72 BY 1
     ttOrderUpd.WarehouseNum AT ROW 17.42 COL 18 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     "Order" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.54 COL 8 WIDGET-ID 42
          FGCOLOR 1 FONT 6
     RECT-1 AT ROW 1.81 COL 2 WIDGET-ID 40
     SPACE(1.28) SKIP(0.26)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order View"
         DEFAULT-BUTTON BtnExit WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttOrderUpd T "?" NO-UNDO Sports2000 Order
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ttOrderUpd.CustNum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttOrderUpd.Ordernum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.ttOrderUpd"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order View */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExit Dialog-Frame
ON CHOOSE OF BtnExit IN FRAME Dialog-Frame /* Exit */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
   MESSAGE "Weet u dat zeker?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lAnswer AS LOGICAL.
   
   IF lAnswer THEN 
   DO:
     ASSIGN {&DISPLAYED-FIELDS}.
     RUN SaveOrderRecord IN hDataUtil (INPUT-OUTPUT TABLE ttOrderUpd,
                                       INPUT pcMode).    
                                                                                             
       IF RETURN-VALUE <> "" THEN
       DO:
           MESSAGE RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN RETURN-VALUE.
       END.
          APPLY "END-ERROR":U TO SELF.
    END. // End lAnswer                                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE ttOrderUpd THEN 
    DISPLAY ttOrderUpd.CustNum ttOrderUpd.Ordernum ttOrderUpd.Creditcard ttOrderUpd.BillToID 
          ttOrderUpd.Instructions ttOrderUpd.OrderDate ttOrderUpd.OrderStatus 
          ttOrderUpd.Carrier ttOrderUpd.PO ttOrderUpd.PromiseDate ttOrderUpd.SalesRep 
          ttOrderUpd.ShipDate ttOrderUpd.ShipToID ttOrderUpd.Terms ttOrderUpd.WarehouseNum 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 btnSave BtnExit ttOrderUpd.Creditcard ttOrderUpd.BillToID 
         ttOrderUpd.Instructions ttOrderUpd.OrderDate ttOrderUpd.OrderStatus 
         ttOrderUpd.Carrier ttOrderUpd.PO ttOrderUpd.PromiseDate ttOrderUpd.SalesRep 
         ttOrderUpd.ShipDate ttOrderUpd.ShipToID ttOrderUpd.Terms ttOrderUpd.WarehouseNum 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects Dialog-Frame 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  RUN PersistentProc.p PERSISTENT SET hProcLib.
  hDataUtil = DYNAMIC-FUNCTION('RunPersistent' IN hProcLib, "DataUtil.p":U).
  
  IF pcMode = "Edit" THEN 
    RUN GetOrderRecord IN hDataUtil (OUTPUT TABLE ttOrderUpd, INPUT piOrderNum).

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


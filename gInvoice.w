&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
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
{ttTables.i}
{ttTables.i &Suffix=Upd}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/*DEFINE INPUT PARAMETER pcMode        AS CHARACTER NO-UNDO.*/
/*DEFINE INPUT PARAMETER phProcLib     AS HANDLE    NO-UNDO.*/
/*DEFINE INPUT PARAMETER prowRowId     AS ROWID     NO-UNDO.*/
DEFINE INPUT PARAMETER piOrderNum AS INTEGER NO-UNDO.


//DEFINE OUTPUT PARAMETER TABLE FOR ttInvoice.
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
&Scoped-define INTERNAL-TABLES ttInvoice

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ttInvoice.CustNum ~
ttInvoice.OrderNum ttInvoice.Invoicenum ttInvoice.InvoiceDate ~
ttInvoice.Amount ttInvoice.Adjustment ttInvoice.ShipCharge ~
ttInvoice.TotalPaid 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ttInvoice SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ttInvoice SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ttInvoice
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ttInvoice


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS ttInvoice.CustNum ttInvoice.OrderNum ~
ttInvoice.Invoicenum ttInvoice.InvoiceDate ttInvoice.Amount ~
ttInvoice.Adjustment ttInvoice.ShipCharge ttInvoice.TotalPaid 
&Scoped-define DISPLAYED-TABLES ttInvoice
&Scoped-define FIRST-DISPLAYED-TABLE ttInvoice


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.15
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ttInvoice SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.81 COL 44
     ttInvoice.CustNum AT ROW 2.08 COL 19 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 7.43 BY 1
     ttInvoice.OrderNum AT ROW 3.15 COL 19 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     Btn_Cancel AT ROW 3.15 COL 44
     ttInvoice.Invoicenum AT ROW 4.23 COL 19 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttInvoice.InvoiceDate AT ROW 5.31 COL 19 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttInvoice.Amount AT ROW 6.38 COL 19 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttInvoice.Adjustment AT ROW 7.46 COL 19 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttInvoice.ShipCharge AT ROW 8.54 COL 19 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     ttInvoice.TotalPaid AT ROW 9.62 COL 19 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 13.14 BY 1
     SPACE(28.57) SKIP(0.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Invoice Details"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ttInvoice.Adjustment IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.Amount IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.CustNum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.InvoiceDate IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.Invoicenum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.OrderNum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.ShipCharge IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ttInvoice.TotalPaid IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.ttInvoice"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Invoice Details */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
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
  IF AVAILABLE ttInvoice THEN 
    DISPLAY ttInvoice.CustNum ttInvoice.OrderNum ttInvoice.Invoicenum 
          ttInvoice.InvoiceDate ttInvoice.Amount ttInvoice.Adjustment 
          ttInvoice.ShipCharge ttInvoice.TotalPaid 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK Btn_Cancel 
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
 
  RUN GetInvoiceRecord IN hDataUtil(OUTPUT TABLE ttInvoice,
                                    INPUT piOrderNum).
                                                                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


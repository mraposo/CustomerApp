
/*------------------------------------------------------------------------
    File        : PersistentProc.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : mario
    Created     : Tue Mar 07 20:04:00 CET 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION RunPersistent RETURNS HANDLE 
    ( INPUT pcProcedureName AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */
SUBSCRIBE TO "Shutdown":U ANYWHERE.


/* **********************  Internal Procedures  *********************** */

PROCEDURE Shutdown:
/*------------------------------------------------------------------------------
 Purpose:  Shutdown procedure if running persistent
 Notes:
------------------------------------------------------------------------------*/
 IF THIS-PROCEDURE:PERSISTENT THEN 
    DELETE PROCEDURE THIS-PROCEDURE. 

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION RunPersistent RETURNS HANDLE 
    ( INPUT pcProcedureName AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:   Checks to see if a persistent procedure is already 
            running. If not, it runs the procedure persistently. It returns
            the handle to the persistent procedure.
 Notes:
------------------------------------------------------------------------------*/    
  DEFINE VARIABLE hproc AS HANDLE NO-UNDO.

  hProc = SESSION:FIRST-PROCEDURE.
  DO WHILE VALID-HANDLE(hProc) AND hProc:FILE-NAME NE pcProcedureName:
      hProc = hProc:NEXT-SIBLING.
  END.

  IF NOT VALID-HANDLE(hProc) THEN
      RUN VALUE(pcProcedureName) PERSISTENT SET hProc.

  RETURN hProc.

        
END FUNCTION.

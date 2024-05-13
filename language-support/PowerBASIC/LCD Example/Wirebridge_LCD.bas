#PBFORMS CREATED V2.00
'------------------------------------------------------------------------------
' The first line in this file is a PB/Forms metastatement.
' It should ALWAYS be the first line of the file. Other
' PB/Forms metastatements are placed at the beginning and
' end of "Named Blocks" of code that should be edited
' with PBForms only. Do not manually edit or delete these
' metastatements or PB/Forms will not be able to reread
' the file correctly.  See the PB/Forms documentation for
' more information.
' Named blocks begin like this:    #PBFORMS BEGIN ...
' Named blocks end like this:      #PBFORMS END ...
' Other PB/Forms metastatements such as:
'     #PBFORMS DECLARATIONS
' are used by PB/Forms to insert additional code.
' Feel free to make changes anywhere else in the file.
' Databytes

'------------------------------------------------------------------------------
%D4=5   'note these are the pin numbers on the Wirebridge
%D5=6
%D6=7
%D7=8
%E =4
%RS=3
#COMPILE EXE
#DIM ALL

'------------------------------------------------------------------------------
'   ** Includes **
'------------------------------------------------------------------------------
#PBFORMS BEGIN INCLUDES
'#RESOURCE "Wirebridge_LCD.pbr"
#INCLUDE ONCE "WIN32API.INC"
#INCLUDE ONCE "COMMCTRL.INC"
#INCLUDE ONCE "PBForms.INC"
#PBFORMS END INCLUDES
'------------------------------------------------------------------------------
#INCLUDE ONCE "Wirebridge.inc"
'------------------------------------------------------------------------------
'   ** Constants **
'------------------------------------------------------------------------------
#PBFORMS BEGIN CONSTANTS
%IDD_DIALOG1     =  101
%IDD_DLG_HLP     =  102
%IDR_MENU1       =  103
%IDC_BUT_ENUM    = 1003
%IDC_LABEL3      = 1015
%IDC_LCD_TOPL    = 1002
%IDC_LABEL4      = 1016
%IDC_LCD_BOTL    = 1014
%IDC_BUT_CLEAR   = 1008
%IDC_BUT_SEND    = 1005
%IDC_SCROLL_L    = 1017
%IDC_SCROLL_R    = 1018
%IDC_QUIT        = 1001
%IDC_LAB_WB_INFO = 1004
%IDC_AUTOSCROLL  = 1020
%IDC_BUTTON1     = 1010
%IDC_LABEL2      = 1013
%IDM_HELP_HELP   = 1019
%ID_TIMER        =  200
%IDC_LAB_ERR     = 1006
#PBFORMS END CONSTANTS
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'   ** Declarations **
'------------------------------------------------------------------------------
DECLARE CALLBACK FUNCTION ShowDIALOG1Proc()
DECLARE FUNCTION ShowDIALOG1(BYVAL hParent AS DWORD) AS LONG
DECLARE CALLBACK FUNCTION ShowDLG_HLPProc()
DECLARE FUNCTION ShowDLG_HLP(BYVAL hDlg AS DWORD) AS LONG
DECLARE FUNCTION AttachMENU1(BYVAL hDlg AS DWORD) AS DWORD
#PBFORMS DECLARATIONS
'------------------------------------------------------------------------------
'Need to get system ready to write to pins
'need a char to nibble function

GLOBAL GWB_DEVHANDLE AS LONG
'------------------------------------------------------------------------------
'   ** Main Application Entry Point **
'------------------------------------------------------------------------------
FUNCTION PBMAIN()
    LOCAL bresult  AS BYTE

    PBFormsInitComCtls (%ICC_WIN95_CLASSES OR %ICC_DATE_CLASSES OR _
        %ICC_INTERNET_CLASSES)
    ShowDIALOG1 %HWND_DESKTOP

    BResult = WB_CloseDevice( gWB_DevHandle)
END FUNCTION
'------------------------------------------------------------------------------


 SUB EnumWB(hdlg AS LONG,WB_DEVHANDLE AS DWORD, reply AS STRING, MyError AS STRING)

    LOCAL WbErr AS WB_ERROR PTR
    LOCAL WB_ERR AS WB_ERROR
    LOCAL DT AS WB_Datetime
    LOCAL DTP AS WB_DATETIME PTR
    LOCAL result, I,J AS LONG
    LOCAL WB_DEVENUMERATION AS DWORD
    LOCAL wb_dev AS  WB_ENUMDEVINFO
    LOCAL devinfo AS WB_ENUMDEVINFO PTR
    LOCAL wb_outcome AS BYTE
    LOCAL WB_Path AS WSTRINGZ * %MAX_PATH
    LOCAL model,temp AS STRING

    WBERR=VARPTR(WB_ERR)
    wb_path = STRING$$(%MAX_PATH, 32) ' filled with spaces :)
    wb_dev.USB_path = VARPTR (wb_path) 'point to string for path
    devinfo = VARPTR(wb_dev)
    dtp=VARPTR(DT)

    WB_DEVENUMERATION =  WB_EnumerateDevicesBegin ( wberr) '  GET HANDLE
    IF WB_DEVENUMERATION=%False THEN 'if windows has am entry then there will be a Handle returned
           myerror=WB_ERRORTYPE(@wberr.WB_ERRORTYPE)      'RETURN ERROR
           EXIT SUB
    END IF
          DO
            wb_outcome = WB_EnumerateDevicesNext( WB_DEVENUMERATION, devinfo, Wberr)
            IF wb_outcome  = 0  THEN   ' got one!
                 CONTROL SET USER hdlg,%IDC_BUT_ENUM,0,0
                 wb_Path=wb_dev.@USB_PATH     'make a copy of the path get Device handle
                 model = wb_dev.devinfo.productname
                 IF INSTR (model,"1B") THEN
                    reply="Wirebridge 1B found"
                    EXIT LOOP
                 ELSEIF INSTR (model,"1A") THEN
                    reply="Wirebridge 1A found"
                    EXIT LOOP

                ELSEIF INSTR (model,"2") THEN
'                    reply="Wirebridge 2 found"
'                    EXIT LOOP
                END IF

           ELSE    'none attached
                 myerror="Device not detected"
                 CONTROL SET USER hdlg,%IDC_BUT_ENUM,0,1
                 EXIT SUB
            END IF
          LOOP

          Result = WB_EnumerateDevicesEnd(WB_DEVENUMERATION)    'close it
       '   "Now Open the device and make the handle global"
          WB_DevHandle = WB_OpenDevice( VARPTR(wb_path) , WBERR )
          IF result=%False THEN '
            myerror=WB_ERRORTYPE(@wberr.WB_ERRORTYPE)
            EXIT SUB
          END IF
          'firmware version
       '   Byte =  WB_GetFirmwareRevision (WB_DEVHANDLE , WBERR)  'returns model
         wb_outcome =  WB_GetFirmwareRevision (WB_DEVHANDLE , WBERR)  'returns version  in wb_outcome

         temp="WB Firmware Ver. "+TRIM$(STR$(wb_outcome))

          result = WB_BuildTimestamp(dtp)
              I= VAL(PARSE$($Dlldate,"/",1))*1000+VAL(PARSE$($Dlldate,"/",2))*40 +VAL(PARSE$($Dlldate,"/",3))
              J= dt.year*1000+ DT.MONTH*40+ DT.Day
              IF J=>I THEN  ' great its later or the same
                  Reply = Reply+$CRLF+"Wirebridge DLL suitable"+$CRLF

              ELSEIF I>J THEN 'problem
               Reply =Reply+$CRLF+"DLL is too old, Newer needed"+$CRLF
               Reply=Reply+"Use after "+$DLLDATE
               CONTROL SET USER hdlg,%IDC_BUT_ENUM,0,1
               MyError="Wirebridge DLL out of date"
               EXIT SUB
              END IF


          reply=reply+"Requires DLL Date "+$DLLDATE +$CRLF +_
          "Using DLL"+STR$(dt.year)+"/"+TRIM$(STR$(DT.MONTH))+"/"+ _
          TRIM$(STR$(DT.Day))+STR$(DT.Hour)+":"+RIGHT$("0"+TRIM$(STR$(dt.minute)),2)
          reply = reply+$CRLF+Temp
          GWB_DEVHANDLE = WB_DevHandle
 END SUB

'------------------------------------------------------------------------------

  SUB WB_Setup
  LOCAL wb_outcome AS BYTE
  LOCAL WbErr AS WB_ERROR PTR
  LOCAL WB_ERR AS WB_ERROR
  LOCAL RS,E,D4,D5,D6,D7 AS BYTE
  'as setup called the WB is found!

   WbErr = VARPTR (WB_ERR)
   'setup the LCD pins
   rs=%WB_Model1A.pin3
   E= %WB_Model1A.pin4
   D4=%WB_Model1A.pin5
   D5=%WB_Model1A.pin6
   D6=%WB_Model1A.pin7
   D7=%WB_Model1A.pin8

    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin1 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin2 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin3 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin4 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin5 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin6 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin7 , %PinDir.Dir_out,BYVAL %NULL)
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle, %WB_Model1A.pin8 , %PinDir.Dir_out,BYVAL %NULL)

     sendcommand(2)         'this only works on initilization 2 is hi nibble of the command
     sendcommand(&H28)      ' then 28 is the low nibble 2 lines, 5x8 matrix,4-bit mode
     sendcommand(&H0C)      'Home the cussor
     sendcommand(&H01)      'Clear screen
     sendcommand(&H02)      'home

  END SUB

 SUB  sendtext(cmd AS BYTE)
  LOCAL wb_outcome AS BYTE
  LOCAL WbErr AS WB_ERROR PTR
  LOCAL WB_ERR AS WB_ERROR
  LOCAL RS,E AS BYTE
  DIM pinmask(7) AS WB_SMPD_ENTRY
  LOCAL Pmask,pcount AS LONG'  Used as pointer

   WbErr = VARPTR (WB_ERR)
   'setup the LCD pins
   rs= %WB_Model1A.pin3
   E = %WB_Model1A.pin4

   pinmask(0).wb_pin_number=%WB_Model1A.pin5
   pinmask(1).wb_pin_number=%WB_Model1A.pin6
   pinmask(2).wb_pin_number=%WB_Model1A.pin7
   pinmask(3).wb_pin_number=%WB_Model1A.pin8
   pinmask(4).wb_pin_number=%WB_Model1A.pin5
   pinmask(5).wb_pin_number=%WB_Model1A.pin6
   pinmask(6).wb_pin_number=%WB_Model1A.pin7
   pinmask(7).wb_pin_number=%WB_Model1A.pin8

'   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)     ' enable
'   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,RS,%DP_Value.high, WBERR)    ' data

  pinmask(0).wb_smpd_state = BIT(cmd,0)
  pinmask(1).wb_smpd_state = BIT(cmd,1)
  pinmask(2).wb_smpd_state = BIT(cmd,2)
  pinmask(3).wb_smpd_state = BIT(cmd,3)
  pinmask(4).wb_smpd_state = BIT(cmd,4)
  pinmask(5).wb_smpd_state = BIT(cmd,5)
  pinmask(6).wb_smpd_state = BIT(cmd,6)
  pinmask(7).wb_smpd_state = BIT(cmd,7)

   'makesure E is high and RS low (command)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)     ' enable
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,RS,%DP_Value.high, WBERR)    ' data (register select)

 'send HIGH first
    PMask = VARPTR(pinmask(4))    ' point to the High nibble
    pcount=4 '4pins at once
   wb_outcome =  WB_SetMultiplePinsDigital(gWB_Devhandle,pMask,PCOUNT,BYVAL %NULL)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.low, WBERR)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)
   'now send LoW
   PMask = VARPTR(pinmask(0))    ' point to the High nibble
   pcount=4
   wb_outcome =  WB_SetMultiplePinsDigital(gWB_Devhandle,pMask,PCOUNT,BYVAL %NULL)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.low, WBERR)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)

END SUB

SUB sendcommand(cmd AS BYTE)
  LOCAL wb_outcome AS BYTE
  LOCAL WbErr AS WB_ERROR PTR
  LOCAL WB_ERR AS WB_ERROR
  LOCAL RS,E AS BYTE
  LOCAL I AS LONG
  DIM pinmask(7) AS WB_SMPD_ENTRY
  LOCAL Pmask,pcount AS LONG'  Used as pointer

   WbErr = VARPTR (WB_ERR)
   'setup the LCD pins
   rs=%WB_Model1A.pin3
   E =%WB_Model1A.pin4
   pinmask(0).wb_pin_number=%WB_Model1A.pin5
   pinmask(1).wb_pin_number=%WB_Model1A.pin6
   pinmask(2).wb_pin_number=%WB_Model1A.pin7
   pinmask(3).wb_pin_number=%WB_Model1A.pin8
   pinmask(4).wb_pin_number=%WB_Model1A.pin5
   pinmask(5).wb_pin_number=%WB_Model1A.pin6
   pinmask(6).wb_pin_number=%WB_Model1A.pin7
   pinmask(7).wb_pin_number=%WB_Model1A.pin8
   'makesure E is high and RS low (command)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)    'Enable
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,RS,%DP_Value.low, WBERR)    'Command Register select

  pinmask(0).wb_smpd_state = BIT(cmd,0)
  pinmask(1).wb_smpd_state = BIT(cmd,1)
  pinmask(2).wb_smpd_state = BIT(cmd,2)
  pinmask(3).wb_smpd_state = BIT(cmd,3)
  pinmask(4).wb_smpd_state = BIT(cmd,4)
  pinmask(5).wb_smpd_state = BIT(cmd,5)
  pinmask(6).wb_smpd_state = BIT(cmd,6)
  pinmask(7).wb_smpd_state = BIT(cmd,7)
      'send HIGH first
    PMask = VARPTR(pinmask(4))    ' point to the High nibble
    pcount=4 '4pins at once
   wb_outcome =  WB_SetMultiplePinsDigital(gWB_Devhandle,pMask,PCOUNT,BYVAL %NULL)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.low, WBERR)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)
   'now send LoW
   PMask = VARPTR(pinmask(0))    ' point to the LOW nibble
   pcount=4
   wb_outcome =  WB_SetMultiplePinsDigital(gWB_Devhandle,pMask,PCOUNT,BYVAL %NULL)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.low, WBERR)
   wb_outcome = WB_WritePinDigital(gWB_Devhandle ,E,%DP_Value.high, WBERR)

END SUB


SUB autoscroll (hdlg AS LONG,X AS LONG)
    STATIC scrolling AS LONG
    LOCAL B AS BYTE


    IF scrolling = 1 AND X=1 THEN
        'do a scroll
            b = &B00011000
            sendcommand(b)
    ELSEIF X=0 THEN
        scrolling=0
    ELSEIF X=-1 THEN
        scrolling=1
    END IF

END SUB
'------------------------------------------------------------------------------
'   ** CallBacks **
'------------------------------------------------------------------------------
CALLBACK FUNCTION ShowDIALOG1Proc()
  LOCAL T2,T1, tmp AS STRING
  LOCAL WB_DEVHANDLE AS DWORD
  LOCAL b AS BYTE
  LOCAL i AS LONG
  STATIC Xscroll AS LONG
    SELECT CASE AS LONG CB.MSG
        CASE %WM_INITDIALOG
            ' Initialization handler
               SetTimer(CB.HNDL, %ID_Timer, 650, BYVAL %NULL)
        CASE %WM_NCACTIVATE
            STATIC hWndSaveFocus AS DWORD
            IF ISFALSE CB.WPARAM THEN
                ' Save control focus
                hWndSaveFocus = GetFocus()
            ELSEIF hWndSaveFocus THEN
                ' Restore control focus
                SetFocus(hWndSaveFocus)
                hWndSaveFocus = 0
            END IF
         CASE %WM_TIMER
                 CALL Autoscroll(CB.HNDL,1)
         CASE %WM_COMMAND
            ' Process control notifications
            SELECT CASE AS LONG CB.CTL
                ' /* Inserted by PB/Forms 04-12-2024 13:47:03


               CASE %IDC_AUTOSCROLL
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN

                    IF Xscroll=0 THEN
                     CALL Autoscroll(CB.HNDL,-1)
                     Xscroll=1
                    ELSE
                     xscroll=0
                      CALL Autoscroll(CB.HNDL,0 )
                     END IF
                  END IF

                CASE %IDC_SCROLL_L
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                        b = &B00011000
                        sendcommand(b)
                    END IF

                CASE %IDC_SCROLL_R
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                        b = &B00011100
                        sendcommand(b)
                    END IF

                CASE %IDM_HELP_HELP
                   CALL ShowDLG_HLP(CB.HNDL)
                CASE %IDC_BUT_CLEAR
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                           sendcommand(&H01)
                           sendcommand(&H02)
                    END IF

                CASE %IDC_BUT_ENUM
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                      'start by query
                        EnumWB(CB.HNDL,WB_DEVHANDLE,t2,tmp)
                        CONTROL SET TEXT CB.HNDL,%IDC_LAB_ERR,tmp
                        IF WB_DEVHANDLE = %False THEN
                       '  MSGBOX "Failed to get handle" +$CRLF+Tmp  ,,"WB-PWM Test"
                         CONTROL SET TEXT CB.HNDL, %IDC_LAB_WB_INFO, "Check Wirebridge is attached"
                               CONTROL DISABLE CB.HNDL, %IDC_BUT_CLEAR
                               CONTROL DISABLE CB.HNDL, %IDC_BUT_SEND
                               CONTROL DISABLE CB.HNDL, %IDC_SCROLL_L
                               CONTROL DISABLE CB.HNDL, %IDC_SCROLL_R
                               CONTROL DISABLE CB.HNDL, %IDC_LCD_TOPL
                               CONTROL DISABLE CB.HNDL, %IDC_LCD_BOTL
                         ELSE
                            CONTROL ENABLE  CB.HNDL,%IDC_BUT_SEND
                            CONTROL SET TEXT CB.HNDL, %IDC_LAB_WB_INFO, T2
                         END IF
                         CONTROL GET USER CB.HNDL,%IDC_BUT_ENUM,0 TO I
                         IF I=0 THEN
                                CALL WB_Setup
                                CONTROL ENABLE CB.HNDL, %IDC_BUT_CLEAR
                                CONTROL ENABLE CB.HNDL, %IDC_BUT_SEND
                                CONTROL ENABLE CB.HNDL, %IDC_SCROLL_L
                                CONTROL ENABLE CB.HNDL, %IDC_SCROLL_R
                                CONTROL ENABLE CB.HNDL, %IDC_AUTOSCROLL
                                CONTROL ENABLE CB.HNDL, %IDC_LCD_TOPL
                                CONTROL ENABLE CB.HNDL, %IDC_LCD_BOTL

                         END IF
                    END IF

                CASE %IDC_BUT_SEND      '
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                        CONTROL GET TEXT CB.HNDL,%IDC_LCD_TOPL TO T1
                        IF LEN(T1)>40 THEN
                            t1=LEFT$(t1,40)
                            CONTROL SET TEXT CB.HNDL,%IDC_LCD_TOPL , T1
                         END IF
                        CONTROL GET TEXT  CB.HNDL,%IDC_LCD_BOTL TO T2
                        IF LEN(T1)>40 THEN
                            t1=LEFT$(t1,40)
                            CONTROL SET TEXT CB.HNDL,%IDC_LCD_BOTL , T2
                         END IF
                        tmp=STRING$(80," " )
                        IF LEN(T1)<15 THEN
                           I=(16-LEN(t1))/2
                           t1=STRING$(I," ")+T1+STRING$(I," ")
                         END IF
                         IF LEN(T2)<15 THEN
                           I=(16-LEN(t2))/2
                           t2=STRING$(I," ")+T2+STRING$(I," ")
                         END IF
                        MID$(tmp,1,LEN(T1))= T1   'top line
                        MID$(tmp,41,LEN(T2))=T2   'bottom line

                           sendcommand(&H01)
                           sendcommand(&H02)

                        FOR I=1 TO LEN(tmp)
                            B=ASC(MID$(tmp,I,1))
                            sendtext(B)
                        NEXT
                    END IF
                ' */

                CASE %IDC_QUIT
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN

                         sendcommand(&H01)
                         sendcommand(&H02)

                       CALL Quit(CB.HNDL)
                    END IF



            END SELECT
    END SELECT
END FUNCTION
'------------------------------------------------------------------------------
'   ** Dialogs **
'------------------------------------------------------------------------------
FUNCTION ShowDIALOG1(BYVAL hParent AS DWORD) AS LONG
    LOCAL lRslt AS LONG

#PBFORMS BEGIN DIALOG %IDD_DIALOG1->%IDR_MENU1->
    LOCAL hDlg  AS DWORD

    DIALOG NEW PIXELS, hParent, "Wirebridge LCD", 416, 167, 388, 304, _
        %WS_POPUP OR %WS_BORDER OR %WS_DLGFRAME OR %WS_SYSMENU OR _
        %WS_MINIMIZEBOX OR %WS_CLIPSIBLINGS OR %WS_VISIBLE OR %DS_MODALFRAME _
        OR %DS_3DLOOK OR %DS_NOFAILCREATE OR %DS_SETFONT, _
        %WS_EX_CONTROLPARENT OR %WS_EX_LEFT OR %WS_EX_LTRREADING OR _
        %WS_EX_RIGHTSCROLLBAR, TO hDlg
    CONTROL ADD BUTTON,  hDlg, %IDC_BUT_ENUM, "Find and Initilise Wirebridge " + _
        "", 73, 8, 248, 27, %WS_CHILD OR %WS_VISIBLE OR %BS_TEXT OR _
        %BS_PUSHBUTTON OR %BS_CENTER OR %BS_VCENTER, %WS_EX_LEFT OR _
        %WS_EX_LTRREADING
    CONTROL ADD LABEL,   hDlg, %IDC_LABEL3, "Top Line", 8, 128, 74, 18, _
        %WS_CHILD OR %WS_VISIBLE OR %SS_RIGHT, %WS_EX_LEFT OR _
        %WS_EX_LTRREADING
    CONTROL ADD TEXTBOX, hDlg, %IDC_LCD_TOPL, "Welcome to Wirebridge", 88, _
        124, 253, 20
    CONTROL ADD LABEL,   hDlg, %IDC_LABEL4, "Bottom Line", 8, 153, 74, 18, _
        %WS_CHILD OR %WS_VISIBLE OR %SS_RIGHT, %WS_EX_LEFT OR _
        %WS_EX_LTRREADING
    CONTROL ADD TEXTBOX, hDlg, %IDC_LCD_BOTL, "Welcome to Wirebridge", 88, _
        153, 253, 20
    CONTROL ADD BUTTON,  hDlg, %IDC_BUT_CLEAR, "Clear Screen", 88, 179, 94, _
        25
    CONTROL ADD BUTTON,  hDlg, %IDC_BUT_SEND, "Send Text", 198, 180, 93, 25
    CONTROL ADD BUTTON,  hDlg, %IDC_SCROLL_L, "Scroll Text Left", 31, 212, _
        93, 25
    CONTROL ADD BUTTON,  hDlg, %IDC_SCROLL_R, "Scroll Text Right", 138, 212, _
        93, 25
    CONTROL ADD BUTTON,  hDlg, %IDC_QUIT, "Quit", 275, 251, 104, 25
    CONTROL ADD LABEL,   hDlg, %IDC_LAB_WB_INFO, "Requires  DLL  2024/20/02 " + _
        "or later", 88, 48, 221, 73, %WS_CHILD OR %WS_VISIBLE OR %SS_CENTER, _
        %WS_EX_LEFT OR %WS_EX_LTRREADING
    CONTROL ADD BUTTON,  hDlg, %IDC_AUTOSCROLL, "AutoScroll On/Off", 244, _
        212, 93, 25

    AttachMENU1 hDlg
#PBFORMS END DIALOG
     CONTROL DISABLE hDlg, %IDC_LCD_TOPL
     CONTROL DISABLE hDlg, %IDC_LCD_BOTL
     CONTROL DISABLE HDLG, %IDC_BUT_CLEAR
     CONTROL DISABLE HDLG, %IDC_BUT_SEND
     CONTROL DISABLE HDLG, %IDC_SCROLL_L
     CONTROL DISABLE HDLG, %IDC_SCROLL_R
     CONTROL DISABLE HDLG, %IDC_AUTOSCROLL
    DIALOG SHOW MODAL hDlg, CALL ShowDIALOG1Proc TO lRslt

#PBFORMS BEGIN CLEANUP %IDD_DIALOG1
#PBFORMS END CLEANUP

    FUNCTION = lRslt
END FUNCTION
'------------------------------------------------------------------------------
SUB Quit (Ahndl AS DWORD)
    ' Kill the dialog and let PBMAIN() continue
    ' or close main.

    DIALOG END Ahndl, 0
END SUB
'------------------------------------------------------------------------------
CALLBACK FUNCTION ShowDLG_HLPProc()

    SELECT CASE AS LONG CB.MSG
        CASE %WM_INITDIALOG
            ' Initialization handler

        CASE %WM_NCACTIVATE
            STATIC hWndSaveFocus AS DWORD
            IF ISFALSE CB.WPARAM THEN
                ' Save control focus
                hWndSaveFocus = GetFocus()
            ELSEIF hWndSaveFocus THEN
                ' Restore control focus
                SetFocus(hWndSaveFocus)
                hWndSaveFocus = 0
            END IF

        CASE %WM_COMMAND
            ' Process control notifications
            SELECT CASE AS LONG CB.CTL
                CASE %IDC_BUTTON1
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                        CALL quit(CB.HNDL)
                    END IF

            END SELECT
    END SELECT
END FUNCTION
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
FUNCTION ShowDLG_HLP(BYVAL hParent AS DWORD) AS LONG
    LOCAL lRslt AS LONG
    LOCAL tmp AS STRING
    Tmp="This program uses a Wirebridge 1B to drive a standard 16 character by 2 row LCD that" + _
    " is compatible with the Hitachi HD44780 driver. "+ _
    "Drive is Via Pins or L2C serial"+$CRLF
    tmp=tmp+ " One or 2 lines of text can be displayed, Under 16 charactes may be centered in the LCD"+$CRLF
    tmp=tmp+ $CRLF +" Wiring for the Wirebridge " + $CRLF  +$CRLF
    tmp=tmp+" Wirebridge 1B      LCD Display" + $CRLF
    tmp=tmp+"     Pin3       to      RS " + $CRLF
    tmp=tmp+"     Pin4       to      E" + $CRLF
    tmp=tmp+"     Pin5       to      D4" + $CRLF
    tmp=tmp+"     Pin6       to      D5" + $CRLF
    tmp=tmp+"     Pin7       to      D6" + $CRLF
    tmp=tmp+"     Pin8       to      D7" + $CRLF
    tmp=tmp+"     Ground     to      VSS RW and led K"  + $CRLF
    tmp=tmp+"     +5v        to      VDD AND LED A"+ $CRLF
    tmp=tmp+"                        via a suitable"  + $CRLF
    tmp=tmp+"                        resistor"  + $CRLF

#PBFORMS BEGIN DIALOG %IDD_DLG_HLP->->
    LOCAL hDlg   AS DWORD
    LOCAL hFont1 AS DWORD

    DIALOG NEW PIXELS, hParent, "Help", 268, 22, 371, 400, %WS_POPUP OR _
        %WS_BORDER OR %WS_DLGFRAME OR %WS_SYSMENU OR %WS_MINIMIZEBOX OR _
        %WS_VISIBLE OR %DS_MODALFRAME OR %DS_3DLOOK OR %DS_NOFAILCREATE OR _
        %DS_SETFONT, %WS_EX_CONTROLPARENT OR %WS_EX_LEFT OR _
        %WS_EX_LTRREADING OR %WS_EX_RIGHTSCROLLBAR, TO hDlg
    CONTROL ADD BUTTON, hDlg, %IDC_BUTTON1, "Done", 296, 360, 60, 25
    CONTROL ADD LABEL,  hDlg, %IDC_LABEL2, "", 24, 8, 328, 344

    FONT NEW "Courier", 9, 0, %ANSI_CHARSET TO hFont1

    CONTROL SET FONT hDlg, %IDC_LABEL2, hFont1
#PBFORMS END DIALOG
    CONTROL SET TEXT hdlg, %IDC_LABEL2, tmp
    DIALOG SHOW MODAL hDlg, CALL ShowDLG_HLPProc TO lRslt

#PBFORMS BEGIN CLEANUP %IDD_DLG_HLP
    FONT END hFont1
#PBFORMS END CLEANUP

    FUNCTION = lRslt
END FUNCTION
'------------------------------------------------------------------------------
'------------------------------------------------------------------------------
FUNCTION AttachMENU1(BYVAL hDlg AS DWORD) AS DWORD
#PBFORMS BEGIN MENU %IDR_MENU1->%IDD_DIALOG1
    LOCAL hMenu   AS DWORD
    LOCAL hPopUp1 AS DWORD

    MENU NEW BAR TO hMenu
    MENU NEW POPUP TO hPopUp1
    MENU ADD POPUP, hMenu, "Help", hPopUp1, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Help", %IDM_HELP_HELP, %MF_ENABLED

    MENU ATTACH hMenu, hDlg
#PBFORMS END MENU
    FUNCTION = hMenu
END FUNCTION
'------------------------------------------------------------------------------

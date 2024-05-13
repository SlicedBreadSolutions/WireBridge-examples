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
' WHEN using PBforme - remember to delete the local hmenu in the Attach menu
'V1 fixed the image sizes and Landscape and portrait
'------------------------------------------------------------------------------
%MOSI = 10  ' pin 6     %WB_Model1A.SPI_MOSI
%MISO =  9  ' pin 7     %WB_Model1A.SPI_MISO
%CLK  =  8  ' pin 8     %WB_Model1A.SPI_SCK
%DC   = 11  ' pin 5     %WB_Model1A.Pin5
%RES  = 12  ' pin 4     %WB_Model1A.Pin4
%BLK  = 13   'pin 3  or %WB_Model1A.Pin3    used to turn on and off the backlight
%BLK_Toggle = 1  'used for backlight toggle (its a flag)


$VER = "V 1.00"
#RESOURCE VERSIONINFO
#RESOURCE FILEFLAGS %VS_FF_PRERELEASE
#RESOURCE FILEVERSION 1,0,0,0
#RESOURCE PRODUCTVERSION 1,0,0,0
#RESOURCE STRINGINFO "0809","0000"


#RESOURCE VERSION$ "Comments",         "Demonstration of Wirebridge 1B/1B driving a ILI9431 TFT"
'#RESOURCE VERSION$ "CompanyName",      ""
#RESOURCE VERSION$ "FileDescription",  "For Demonstration"
#RESOURCE VERSION$ "FileVersion",      "1.00"
#RESOURCE VERSION$ "InternalName",     "WBDemo"
#RESOURCE VERSION$ "LegalCopyright",   "Wirebridge is Copyright Sliced Bread Solutions 2024"
#RESOURCE VERSION$ "LegalTrademarks",  "Wirebridge"
#RESOURCE VERSION$ "OriginalFilename", "WB_TFT_9431_Demo.exe"
#RESOURCE VERSION$ "ProductName",      "WB_TFT_9431_Demo"
#RESOURCE VERSION$ "ProductVersion",   "1.00"

#RESOURCE BITMAP, 110, "WBdemo2.bmp"
#RESOURCE BITMAP, 111, "WBdemo1.bmp"

#RESOURCE ICON,   111, "Target.ico"

#COMPILE EXE
#DIM ALL

#STACK 1024*1024 '*150
'------------------------------------------------------------------------------
'   ** Includes **
'#INCLUDE "E:\myprograms\PBWin10\samples\SC_Common\De1.inc"
'------------------------------------------------------------------------------
#PBFORMS BEGIN INCLUDES
'#RESOURCE "WB_TFT_9431_demo.pbr"
#INCLUDE ONCE "WIN32API.INC"
#INCLUDE ONCE "COMMCTRL.INC"
#INCLUDE ONCE "PBForms.INC"
#PBFORMS END INCLUDES
#INCLUDE ONCE "pb_ili9431.inc"
#INCLUDE ONCE "wirebridge.inc"
#INCLUDE ONCE "de1.inc"

'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'   ** Constants **
'------------------------------------------------------------------------------
#PBFORMS BEGIN CONSTANTS
%IDD_MAIN                          =  101
%IDC_GRAPHIC1                      = 1001
%IDC_LABEL1                        = 1002
%IDR_MENU1                         =  102
%IDM_FILE_OPEN                     = 1003
%IDM_FILE_EXIT                     = 1004
%IDM_TOOLS_CONVCLR                 = 1005   '*
%IDM_TOOLS_SENDTODISPLAY           = 1006
%IDM_UTILITY_INITILISEWIREBRIDGE1A = 1007
%IDM_UTILITY_SETUPTHEILI9431       = 1008
%IDM_UTILITY_CLEARSCREEN           = 1009   '*
%IDD_WB_SETUP                      =  103
%IDC_WB_BUT                        = 1010
%IDC_LABEL2                        = 1011
%IDC_LABCLK                        = 1012
%IDC_LABEL3                        = 1013
%IDC_LABEL4                        = 1014
%IDC_LABEL7                        = 1017
%IDC_LABEL8                        = 1018
%IDC_LABEL9                        = 1019
%IDC_LABEL10                       = 1020
%IDC_LABEL11                       = 1021   '*
%IDC_LABEL12                       = 1022   '*
%IDC_LABEL13                       = 1023   '*
%IDC_LABEL14                       = 1024   '*
%IDC_LABEL15                       = 1025
%IDC_CHECKCS                       = 1026
%IDC_TEXTBLK                       = 1029
%IDC_TEXTCS                        = 1030
%IDC_CHECKBAK                      = 1031
%IDC_UPDATE                        = 1032
%IDC_BUTSEND                       = 1033
%IDC_LAB_WBINFO                    = 1034
%IDM_UTILITY_PINSETTINGS           = 1035
%IDC_LABEL16                       = 1037
%IDC_TEXTBAUD                      = 1036   '*
%IDC_COMBOBAUD                     = 1038
%IDC_BACKLIGHT_TOG                         = 1039
%IDC_LABEL17                       = 1040
%IDC_LABEL18                       = 1041   '*
%IDC_TEXTBOX1                      = 1042   '*
%IDC_LABEL19                       = 1044
%IDC_CHECKBLANK                    = 1043
%IDC_CHECKBACKLIGHT                = 1045
%IDM_FILE_1                        = 1046
%IDM_FILE_2                        = 1047
%IDM_FILE_3                        = 1048
%IDM_FILE_4                        = 1049
%IDM_FILE_5                        = 1050
%IDC_LABDC                         = 1016
%IDC_LABRES                        = 1015
%IDC_TEXT_PIN_DC                   = 1028
%IDC_TEXT_PINRES                   = 1027
%IDC_LABEL5                        = 1051
%IDC_GRAPHIC2                      = 1053
%IDC_PROGRESSBAR1                  = 1054
%IDC_PROGRESSBAR2                  = 1055
%IDC_LAB_VER                       = 1056
%IDM_TOOLS_PASTE                   = 1057
%IDC_BUT_TEST                      = 1058
#PBFORMS END CONSTANTS
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'   ** Declarations **
'------------------------------------------------------------------------------
DECLARE FUNCTION AttachMENU1(BYVAL hDlg AS DWORD) AS DWORD
DECLARE CALLBACK FUNCTION ShowMAINProc()
DECLARE CALLBACK FUNCTION ShowWB_SETUPProc()
DECLARE FUNCTION ShowMAIN(BYVAL hParent AS DWORD) AS LONG
DECLARE FUNCTION ShowWB_SETUP(BYVAL hParent AS DWORD) AS LONG
#PBFORMS DECLARATIONS
'------------------------------------------------------------------------------

TYPE RGBA
    R AS BYTE
    G AS BYTE
    B AS BYTE
    A AS BYTE
END TYPE

  GLOBAL GWB_DEVHANDLE AS LONG
  GLOBAL Gdevinfo AS wb_devinfo
  GLOBAL GWB_DEVENUMERATION AS DWORD
  GLOBAL tiffBytes AS STRING
  GLOBAL Lbuffer AS STRING
  GLOBAL B5() AS BYTE
  GLOBAL B6() AS BYTE
  GLOBAL hMenu  AS LONG



'------------------------------------------------------------------------------
'   ** Main Application Entry Point **
'------------------------------------------------------------------------------
FUNCTION PBMAIN()
    DIM b6(255) AS BYTE
    DIM B5(255) AS BYTE
    LOCAL I AS LONG
    LOCAL tmp AS STRING
    LOCAL byte_result AS BYTE
    PBFormsInitComCtls (%ICC_WIN95_CLASSES OR %ICC_DATE_CLASSES OR _
        %ICC_INTERNET_CLASSES)
       ILInit()
       ILuInit()

'Exe folder + name of settings file
     tmp = EXE.PATH$+"WB_TFT_SETTING.INI"

    IF NOT ISFILE(tmp) THEN
         savesetting( "WB_Setup","SPI_Baud","250000")
         savesetting( "WB_Setup","SPI_Reset PIN","5")
         savesetting("WB_Setup","SPI_DC PIN","4")
         savesetting("WB_Setup","SPI_CS PIN","3")
         savesetting("WB_Setup","SPI_BackLight PIN","2")
         savesetting("WB_Setup","SPI_BackLight Use","0")
         savesetting("WB_Setup","SPI_CS Use","0")
         Savesetting("WB_Setup","SPI_BacklightOffonSend","0")
         Savesetting("WB_Setup","SPI_DisplayOffonSend","0")
         Savesetting("WB_Setup","SPI_DisplayOffonSend","0")

      ELSE

     END IF

    FOR I=0 TO 255
          B6(I)=(63 * I/255)
          b5(I)=(31 * I/255)
    NEXT
    ShowMAIN %HWND_DESKTOP
   Byte_result =  WB_SPI_Release(GWB_Devhandle,BYVAL(%NULL))
   Byte_result = WB_CloseDevice(GWB_DevHandle )


END FUNCTION
'------------------------------------------------------------------------------

FUNCTION BRGBA(A AS RGBA)AS WORD
    LOCAL R,G,B AS WORD
'    LOCAL result AS WORD
     R = B5(A.R)
     G = B6(A.G)
     B = B5(A.B)
     SHIFT LEFT R, 11
     SHIFT LEFT G,5
    FUNCTION = R+G+B
END FUNCTION
FUNCTION BBGRA(A AS  RGBA) AS WORD
    LOCAL R,G,B AS WORD
     R = B5(A.B)  'note swap
     G = B6(A.G)
     B = B5(A.R)
     SHIFT LEFT R, 11
     SHIFT LEFT G,5
     FUNCTION = R+G+B
END FUNCTION

'------------------------------------------------------------------------------
'   ** Menus **
'------------------------------------------------------------------------------
FUNCTION AttachMENU1(BYVAL hDlg AS DWORD) AS DWORD
#PBFORMS BEGIN MENU %IDR_MENU1->%IDD_MAIN
      LOCAL hPopUp1 AS DWORD

    MENU NEW BAR TO hMenu
    MENU NEW POPUP TO hPopUp1
    MENU ADD POPUP, hMenu, "File", hPopUp1, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Open", %IDM_FILE_OPEN, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Exit", %IDM_FILE_EXIT, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "1 ", %IDM_FILE_1, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "2 ", %IDM_FILE_2, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "3 ", %IDM_FILE_3, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "4 ", %IDM_FILE_4, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "5 ", %IDM_FILE_5, %MF_ENABLED
    MENU NEW POPUP TO hPopUp1
    MENU ADD POPUP, hMenu, "Tools", hPopUp1, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Send to Display", _
            %IDM_TOOLS_SENDTODISPLAY, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Paste", %IDM_TOOLS_PASTE, %MF_ENABLED
    MENU NEW POPUP TO hPopUp1
    MENU ADD POPUP, hMenu, "Utility", hPopUp1, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Initilise Wirebridge 1A", _
            %IDM_UTILITY_INITILISEWIREBRIDGE1A, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Setup the ILI9431", _
            %IDM_UTILITY_SETUPTHEILI9431, %MF_ENABLED
        MENU ADD STRING, hPopUp1, "Pin Settings for Model 1A", _
            %IDM_UTILITY_PINSETTINGS, %MF_ENABLED

MENU ATTACH hMenu, hDlg
#PBFORMS END MENU
    FUNCTION = hMenu
END FUNCTION
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'   ** CallBacks **
'------------------------------------------------------------------------------
CALLBACK FUNCTION ShowMAINProc()
 LOCAL WB_DEVHANDLE,clipvar AS DWORD
 LOCAL T2,tmp,defs AS STRING
 LOCAL I AS LONG
 LOCAL filename AS STRING
 LOCAL Byte_Result AS BYTE
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
                ' /* Inserted by PB/Forms 02-23-2024 11:00:06


                ' /* Inserted by PB/Forms 02-22-2024 09:26:59
                CASE %IDC_PROGRESSBAR1

                CASE %IDC_PROGRESSBAR2

'
'
'                  END IF

                ' /* Inserted by PB/Forms 02-20-2024 14:52:06
                CASE %IDC_BACKLIGHT_TOG
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                       getSetting("WB_Setup","SPI_BackLight Use",DEFS)
                    IF VAL(defs)=1 THEN
                           'get toggle

                            CONTROL GET USER CB.HNDL,%IDC_BACKLIGHT_TOG,%BLK_Toggle TO I
                             IF I=0 THEN '  SPI_BackLight PIN
                               getSetting("WB_Setup","SPI_BackLight PIN",DEFS)

                               CONTROL SET USER CB.HNDL,%IDC_BACKLIGHT_TOG,%BLK_Toggle,1
                               'convert to WB pin
                               Byte_result = WB_WritePinDigital(gWB_Devhandle ,P2N(VAL(defs)),%DP_Value.low , BYVAL(%NULL))
                             ELSE
                                getSetting("WB_Setup","SPI_BackLight PIN",DEFS)
                               CONTROL SET USER CB.HNDL,%IDC_BACKLIGHT_TOG,%BLK_Toggle,0
                                Byte_result = WB_WritePinDigital(gWB_Devhandle ,P2N(VAL(defs)),%DP_Value.high , BYVAL(%NULL))
                            END IF
                         END IF
                     END IF
                ' */

                CASE %IDC_BUTSEND,%IDM_TOOLS_SENDTODISPLAY
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                         getSetting("WB_Setup","SPI_BackLight Use",DEFS)
                         I=VAL(defs)
                        Getsetting("WB_Setup","SPI_BacklightOffonSend",defs)
                          I=VAL(defs)+I
                        CONTROL SET TEXT CB.HNDL, %IDC_LABEL5,"Please Wait,Sending Image"
                    IF I=2 THEN
                      getSetting("WB_Setup","SPI_BackLight PIN",DEFS)
                       Byte_result = WB_WritePinDigital(gWB_Devhandle ,P2N(VAL(defs)),%DP_Value.low , BYVAL(%NULL))
                    END IF
                     CONTROL DISABLE CB.HNDL, %IDC_BUTSEND
                     CONTROL DISABLE CB.HNDL, %IDC_BACKLIGHT_TOG

                       send2TFT(CB.HNDL)

                    CONTROL ENABLE CB.HNDL,  %IDC_BACKLIGHT_TOG
                    CONTROL ENABLE CB.HNDL, %IDC_BUTSEND
                     Byte_result = WB_WritePinDigital(gWB_Devhandle ,P2N(VAL(defs)),%DP_Value.high , BYVAL(%NULL))
                     CONTROL SET TEXT CB.HNDL, %IDC_LABEL5, ""
                    END IF

                CASE %IDM_FILE_1

                    getsetting("LastUsedFile","F1",defs)
                    filename=defs
                    CALL FileLocateOpen(CB.HNDL,filename )
                     IF filename=defs THEN
                      updateLUF(Filename,1,1)
                      ELSE
                      updateLUF(Filename,0,0)
                     END IF
                CASE %IDM_FILE_2
                    getsetting("LastUsedFile","F2",defs)
                    filename=defs
                    CALL FileLocateOpen(CB.HNDL,filename )
                     IF filename=defs THEN
                      updateLUF(Filename,2,1)
                      ELSE
                      updateLUF(Filename,0,0)
                     END IF

                CASE %IDM_FILE_3
                    getsetting("LastUsedFile","F3",defs)
                    filename=defs
                    CALL FileLocateOpen(CB.HNDL,filename )

                     IF filename=defs THEN
                      updateLUF(Filename,3,1)
                      ELSE
                      updateLUF(Filename,0,0)
                     END IF

                CASE %IDM_FILE_4
                    getsetting("LastUsedFile","F4",defs)
                    filename=defs
                    CALL FileLocateOpen(CB.HNDL,filename )

                     IF filename=defs THEN
                      updateLUF(Filename,4,1)
                      ELSE
                      updateLUF(Filename,0,0)
                     END IF

                CASE %IDM_FILE_5
                    getsetting("LastUsedFile","F5",defs)
                    filename=defs
                    CALL FileLocateOpen(CB.HNDL,filename )

                     IF filename=defs THEN
                      updateLUF(Filename,5,1)
                      ELSE
                      updateLUF(Filename,0,0)
                     END IF

                CASE %IDM_FILE_OPEN
                    getsetting("LastUsedFile","LastPath",filename)
                    CALL FileLocateOpen(CB.HNDL,FileName )
                      updateLUF(Filename, 0,0)
                CASE %IDM_FILE_EXIT
                    CALL Quit(CB.HNDL)

                 CASE %IDM_TOOLS_PASTE
                     CLIPBOARD GET BITMAP  TO ClipVar     ' hbmp

                CASE %IDM_UTILITY_PINSETTINGS
                   ShowWB_SETUP CB.HNDL

                CASE %IDM_UTILITY_INITILISEWIREBRIDGE1A

                        EnumWB(CB.HNDL,WB_DEVHANDLE,t2,tmp)
                        IF T2="" THEN 'nothing found

                        ELSE
                          CONTROL SET TEXT CB.HNDL,%IDC_LAB_WBINFO,tmp
                        ' msgbox str$( WB_DEVHANDLE)
                        END IF
                          IF WB_DEVHANDLE = %False THEN
                            MSGBOX "Failed to get handle" +$CRLF+Tmp  ,,"Demo"
                            CONTROL SET TEXT CB.HNDL, %IDC_LAB_WBINFO, "Check Wirebridge is attached"
                            CONTROL SET USER CB.HNDL, %IDC_LAB_WBINFO,1,0 'off
                         ELSE
                            CONTROL SET TEXT CB.HNDL, %IDC_LAB_WBINFO, T2
                            CONTROL SET USER CB.HNDL, %IDC_LAB_WBINFO,1,1 'on
                         END IF



                CASE %IDM_UTILITY_SETUPTHEILI9431
                    CONTROL GET USER CB.HNDL, %IDC_LAB_WBINFO,1 TO I

                       IF I=1 THEN
                           CALL WB_ILISETUP(CB.HNDL)
                           CONTROL ENABLE CB.HNDL, %IDC_BUTSEND
                           CONTROL ENABLE CB.HNDL,%IDC_BACKLIGHT_TOG
                           MENU SET STATE hMenu, BYCMD  %IDM_TOOLS_SENDTODISPLAY, %MFS_ENABLED
                        ELSE
                           MSGBOX "The Wirebridge is not available / ready yet",,"Demo"
                       END IF
                    'unlock the send



            END SELECT
    END SELECT
END FUNCTION
'------------------------------------------------------------------------------
SUB WB_ILISetup(hdlg AS LONG)
  LOCAL I,J, SPI_MODE  AS LONG
  LOCAL wb_outcome, zcount AS BYTE
  LOCAL WbErr AS WB_ERROR PTR
  LOCAL WB_ERR AS WB_ERROR
  LOCAL myerror,defs,t1,T2 AS STRING
  LOCAL rexbuffer AS STRING
  LOCAL sbp,rbp AS STRING PTR
  DIM Dset(1,1) AS STRING ' this will be redimensioned
  LOCAL BaudX AS DWORD

    getsetting( "WB_Setup","SPI_Baud",defs) 'retrieve the set value
    FOR I=1 TO LEN(defs)    'remove formatting!
      T1=MID$(defs,I,1)    ' to leave a number
        IF T1=" " OR T1="," THEN   T1=""
        t2=t2+T1
    NEXT
      BaudX=VAL(t2)
      IF BaudX < 10  THEN   'no baud saved, first run?
          MSGBOX "Please Set the Baud rate in the"+$CRLF+ _
                 "    Utility Menu - Setup"+$CRLF+ _
                 "  Then run the setup again",%MB_ICONWARNING, "Demo"
          EXIT SUB   'Bail out now and set the rate
       END IF
    'as setup called and we are here the WB is found and a baud rate is available
    IF gwb_devhandle = 0 THEN EXIT SUB  'Check the handle is valid bail on no handle

    WbErr = VARPTR (WB_ERR)   'Setup the error reporting
    wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%WB_Model1A.Pin1 ,%PinDir.Dir_out,WBERR)
    myerror= WB_ERRORTYPE(@wberr.WB_ERRORTYPE)'  Convert error to texr
    IF VAL(myerror)<>0 THEN
         MSGBOX Myerror,,"DEMO"
         EXIT SUB 'bail on error
    END IF
    'if it did not error we can use BYVAL %NULL instead of wberr makes it more readable
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%WB_Model1A.Pin2, %PinDir.Dir_out,BYVAL %NULL )
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%BLK,   %PinDir.Dir_out, BYVAL %NULL )
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%MOSI,  %PinDir.Dir_out, BYVAL %NULL )
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%MISO,  %PinDir.Dir_in, BYVAL %NULL )
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%CLK,   %PinDir.Dir_out, BYVAL %NULL )
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%DC,    %PinDir.Dir_out, BYVAL %NULL )
     wb_outcome = WB_SetPinDirection ( gWB_Devhandle,%RES,   %PinDir.Dir_out, BYVAL %NULL )
     'thats all the pins setup   now set the pin outputs
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.high ,BYVAL %NULL)
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%WB_Model1A.Pin1,%DP_Value.high ,BYVAL %NULL)
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%WB_Model1A.Pin2,%DP_Value.low ,BYVAL %NULL)
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%BLK,%DP_Value.high , BYVAL %NULL)
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%MOSI,%DP_Value.low ,BYVAL %NULL)
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%MISO,%DP_Value.low,BYVAL %NULL)
     wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%CLK,%DP_Value.low ,BYVAL %NULL)
    'RESET ---- FLIP THE RESET (%RES) LINE  down then up
    wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%res,%DP_Value.low , BYVAL %NULL)  'reset
    wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%res,%DP_Value.high , BYVAL %NULL)
    SLEEP 50  ' let the init run for 50 ms
    'should not really be needed as the time between USB sends ought to be enough
'       MODE0 = 0 MODE1 = 1 MODE2 = 2  MODE3 = 3
        SPI_MODE=0         'mode    0 for this tft
      wb_outcome =WB_SPI_Init(gWB_Devhandle,SPI_MODE,BaudX, BYVAL %NULL)

      RexBuffer=STRING$(50," ") 'Recieve buffer
      rbp=VARPTR(rexbuffer)
      CALL Init9431( Dset()) ' Get the command list
      I=VAL(dset(0,0)) '  this is the command count

 FOR J=1 TO I
    sbp=STRPTR(dset(J,0))
    Zcount=LEN(dset(J,0))
 'command first
    wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.low, BYVAL %NULL)  'command
    wb_outcome = WB_SPI_Transfer(gWB_Devhandle, zCount,Sbp,rbp ,BYVAL %NULL)
    wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.high, BYVAL %NULL)

    'now data
      sbp=STRPTR(dset(J,1))
      Zcount=LEN(dset(J,1))
     IF zcount>0 THEN ' The command has both a command byte and data following
         wb_outcome = WB_SPI_Transfer(gWB_Devhandle, zCount,Sbp,rbp ,BYVAL %NULL)
     END IF
     ' do not send on zero bytes!!!
 NEXT
    'finally makesure the DC (data command switch) is high
    wb_outcome = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.high, BYVAL %NULL)
END SUB


SUB EnumWB(hdlg AS LONG,WB_DEVHANDLE AS DWORD,t2 AS STRING, MyError AS STRING)

    LOCAL WbErr AS WB_ERROR PTR
    LOCAL WB_ERR AS WB_ERROR
    LOCAL DT AS WB_Datetime
    LOCAL DTP AS WB_DATETIME PTR
    LOCAL result AS LONG
    LOCAL WB_DEVENUMERATION AS DWORD
    LOCAL wb_dev AS  WB_ENUMDEVINFO
    LOCAL devinfo AS WB_ENUMDEVINFO PTR
    LOCAL wb_outcome,firmware AS BYTE
    LOCAL WB_Path AS WSTRINGZ * %MAX_PATH
    LOCAL model AS STRING

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
     'its OK, save the Handle


     gWB_DEVENUMERATION =  WB_DEVENUMERATION

          DO
             wb_outcome = WB_EnumerateDevicesNext( WB_DEVENUMERATION, devinfo, Wberr)
             IF  wb_outcome =0 THEN   ' got one!
                 wb_Path=wb_dev.@USB_PATH     'make a copy of the path get Device handle
                 model = wb_dev.devinfo.productname
                 Gdevinfo = wb_dev.devinfo
                 IF INSTR (model,"1B") THEN
'                     T2 = "Wirebridge Model 1B found"
'                     EXIT LOOP
                 ELSEIF INSTR (model,"1A") THEN
                     T2 = "Wirebridge Model 1A found"
                     EXIT LOOP
                 ELSEIF INSTR (model,"2") THEN
'                     T2 = "Wirebridge Model 2 found"
'                     EXIT LOOP
                 ELSE    'nout attached
                    myerror="Device not detected"
                     EXIT SUB
                 END IF
             ELSE
                 t2=""
                 Result = WB_EnumerateDevicesEnd(WB_DEVENUMERATION)    'close it
                 EXIT SUB
             END IF
          LOOP
          Result = WB_EnumerateDevicesEnd(WB_DEVENUMERATION)    'close it
       '   "Now Open the device and make the handle global"
          WB_DevHandle = WB_OpenDevice( VARPTR(wb_path) , WBERR )
          IF result=%False THEN '
              myerror=WB_ERRORTYPE(@wberr.WB_ERRORTYPE)
              GWB_DEVHANDLE = 0
              EXIT SUB
          END IF
          Firmware =  WB_GetFirmwareRevision (WB_DEVHANDLE , WBERR)  'returns version
          result = WB_BuildTimestamp(dtp)
          T2=t2+$CRLF+"Firmware Ver."+STR$(Firmware) + "  (V2 or later required)"
          T2=T2 +$CRLF+"Program Requires DLL Date "+$DLLDATE +$CRLF +_
          "Using"+STR$(dt.year)+"/"+TRIM$(STR$(DT.MONTH))+"/"+TRIM$(STR$(DT.Day))+STR$(DT.Hour)+":"+RIGHT$("0"+TRIM$(STR$(dt.minute)),2)
          GWB_DEVHANDLE = WB_DevHandle
 END SUB

'------------------------------------------------------------------------------
CALLBACK FUNCTION ShowWB_SETUPProc()
 LOCAL I,RE,DxC,CS,BK,CtrlID  AS LONG
 LOCAL tmp,t2,defs AS STRING
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
                ' /* Inserted by PB/Forms 02-22-2024 09:26:59

                ' /* Inserted by PB/Forms 02-21-2024 21:25:46

                ' */


                CASE %IDC_WB_BUT
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                           CALL quit(CB.HNDL)
                    END IF
                CASE %IDC_COMBOBAUD
                    CONTROL SET USER CB.HNDL, %IDC_COMBOBAUD, 1,1
                CASE %IDOK
                 IF CB.CTLMSG = %BN_CLICKED THEN
                    CtrlID = GetDlgCtrlId(GetFocus)
                   SELECT CASE CtrlID
                          CASE%IDC_TEXTCS
                             CONTROL GET TEXT CB.HNDL,%IDC_TEXTCS TO tmp
                             tmp=LEFT$(tmp,1)
                             t2 = RETAIN$(tmp, ANY "12345" )
                             CONTROL SET TEXT CB.HNDL,%IDC_TEXTCS , t2
                          CASE %IDC_TEXT_PIN_DC
                             CONTROL GET TEXT CB.HNDL, %IDC_TEXT_PIN_DC TO tmp
                             tmp=LEFT$(tmp,1)
                             t2 = RETAIN$(tmp, ANY "12345" )
                             CONTROL SET TEXT CB.HNDL, %IDC_TEXT_PIN_DC , t2
                          CASE %IDC_TEXT_PINRES
                             CONTROL GET TEXT CB.HNDL,%IDC_TEXT_PINRES TO tmp
                             tmp=LEFT$(tmp,1)
                             t2 = RETAIN$(tmp, ANY "12345" )
                             CONTROL SET TEXT CB.HNDL,%IDC_TEXT_PINRES , t2
                          CASE %IDC_TEXTBLK
                             CONTROL GET TEXT CB.HNDL,%IDC_TEXTBLK TO tmp
                             tmp=LEFT$(tmp,1)
                             t2 = RETAIN$(tmp, ANY "12345" )
                             CONTROL SET TEXT CB.HNDL,%IDC_TEXTBLK , t2

                   END SELECT
                END IF




                CASE %IDC_UPDATE
                    IF CB.CTLMSG = %BN_CLICKED OR CB.CTLMSG = 1 THEN
                         CONTROL GET TEXT CB.HNDL,%IDC_TEXT_PINRES TO tmp
                          t2 = RETAIN$(tmp, ANY "12345" )
                          re=VAL(t2)
                          CONTROL SET TEXT CB.HNDL,%IDC_TEXT_PINRES, T2

                          CONTROL GET TEXT CB.HNDL,%IDC_TEXTBLK TO tmp
                          t2 = RETAIN$(tmp, ANY "12345" )
                          BK=VAL(t2)
                          CONTROL SET TEXT CB.HNDL,%IDC_TEXTBLK, T2

                          CONTROL GET TEXT CB.HNDL, %IDC_TEXT_PIN_DC TO tmp
                           t2 = RETAIN$(tmp, ANY "12345" )
                           DxC=VAL(t2)
                           CONTROL SET TEXT CB.HNDL, %IDC_TEXT_PIN_DC, T2

                         CONTROL GET TEXT CB.HNDL,%IDC_TEXTCS TO tmp
                           t2 = RETAIN$(tmp, ANY "12345" )
                           CS=VAL(t2)
                           CONTROL SET TEXT CB.HNDL,%IDC_TEXTCS, T2

                         IF RE=BK OR RE=DxC OR RE=CS OR BK=DxC OR BK=CS OR CS=DxC OR RE=0 OR CS=0 OR DxC=0 OR BK= 0 THEN
                             MSGBOX " there is a conflict in pins",,"Demo"
                             EXIT SELECT
                         END IF
                         CONTROL GET TEXT CB.HNDL, %IDC_COMBOBAUD TO defs
                         T2=REMOVE$(defs,",")
                         IF testbaud(VAL(T2))=%False THEN
                            MSGBOX "There is a Baud rate problen"+$CRLF+"Please reselect the Baud",,"Demo"
                            EXIT SELECT
                          END IF
                         savesetting( "WB_Setup","SPI_Baud",defs)
                         CONTROL GET TEXT CB.HNDL, %IDC_TEXT_PINRES TO t2
                         savesetting("WB_Setup","SPI_Reset PIN",T2)
                         CONTROL GET TEXT CB.HNDL,%IDC_TEXTBLK TO T2
                         CONTROL GET CHECK CB.HNDL,  %IDC_CHECKBAK TO I
                         savesetting("WB_Setup","SPI_BackLight PIN",T2)
                         savesetting("WB_Setup","SPI_BackLight Use",STR$(I))
                         CONTROL GET TEXT CB.HNDL,%IDC_TEXTCS TO T2
                         CONTROL GET CHECK CB.HNDL,  %IDC_CHECKCS TO I
                         savesetting("WB_Setup","SPI_CS PIN",T2)
                         savesetting("WB_Setup","SPI_CS Use",STR$(I))
                         CONTROL GET TEXT CB.HNDL,%IDC_TEXT_PIN_DC    TO T2
                         savesetting("WB_Setup","SPI_DC PIN",T2)
                         CONTROL GET CHECK CB.HNDL,%IDC_CHECKBACKLIGHT TO I
                         Savesetting("WB_Setup","SPI_BacklightOffonSend",STR$(I))
                          CONTROL GET CHECK CB.HNDL,%IDC_CHECKBLANK TO I
                         Savesetting("WB_Setup","SPI_DisplayOffonSend",STR$(I))
                         CONTROL GET USER CB.HNDL, %IDC_COMBOBAUD, 1 TO I
                         IF I=1 THEN
                         MSGBOX "If the program has already sent images"+$CRLF +_
                                "to the TFT screen. Altering the Baud rate"+$CRLF +_
                                "may need restarting the program" , %MB_ICONWARNING ," TFT Screen Demo "
                          END IF
                          CONTROL SET USER CB.HNDL, %IDC_COMBOBAUD, 1,0
                          CALL quit(CB.HNDL)
                        ' MSGBOX "Save To Do" ,,"Demo"
                    END IF

            END SELECT
    END SELECT
END FUNCTION
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'   ** Dialogs **
'------------------------------------------------------------------------------
FUNCTION ShowMAIN(BYVAL hParent AS DWORD) AS LONG
    LOCAL lRslt,H,W  AS LONG
    LOCAL defs AS STRING
    H =   %ILI9341_TFTWIDTH +8' = 240     ' ILI9341 max TFT width
    W =  %ILI9341_TFTHEIGHT +8' = 320
#PBFORMS BEGIN DIALOG %IDD_MAIN->%IDR_MENU1->
    LOCAL hDlg   AS DWORD
    LOCAL hFont1 AS DWORD

    DIALOG NEW PIXELS, hParent, "TFT Picture Display", , , 553, 369, _
        %WS_POPUP OR %WS_BORDER OR %WS_DLGFRAME OR %WS_CAPTION OR _
        %WS_SYSMENU OR %WS_MINIMIZEBOX OR %WS_CLIPSIBLINGS OR %WS_VISIBLE OR _
        %DS_MODALFRAME OR %DS_3DLOOK OR %DS_NOFAILCREATE OR %DS_SETFONT, _
        %WS_EX_CONTROLPARENT OR %WS_EX_LEFT OR %WS_EX_LTRREADING OR _
        %WS_EX_RIGHTSCROLLBAR, TO hDlg
    DIALOG  SET ICON         hDlg, "#111" ' + FORMAT$(%IDR_IMGFILE1)
    CONTROL ADD GRAPHIC,     hDlg, %IDC_GRAPHIC1, "", 208, 50, 10, 10, _
        %WS_CHILD OR %WS_VISIBLE OR %WS_BORDER OR %SS_SUNKEN, _
        %WS_EX_CLIENTEDGE
    CONTROL ADD GRAPHIC,     hDlg, %IDC_GRAPHIC2, "", 290, 5, 10, 10, _
        %WS_CHILD OR %WS_VISIBLE OR %WS_BORDER OR %SS_SUNKEN, _
        %WS_EX_CLIENTEDGE
    CONTROL ADD LABEL,       hDlg, %IDC_LABEL1, "QVGA  Display 320 * 240", _
        16, 0, 163, 55, %WS_CHILD OR %WS_VISIBLE OR %SS_CENTER, %WS_EX_LEFT _
        OR %WS_EX_LTRREADING
    CONTROL ADD BUTTON,      hDlg, %IDC_BUTSEND, "Send Image", 40, 212, 124, _
        24
    CONTROL ADD LABEL,       hDlg, %IDC_LAB_WBINFO, "WireBridge Info", 8, 63, _
        184, 96, %WS_CHILD OR %WS_VISIBLE OR %SS_LEFT, %WS_EX_STATICEDGE OR _
        %WS_EX_LEFT OR %WS_EX_LTRREADING
    CONTROL ADD BUTTON,      hDlg, %IDC_BACKLIGHT_TOG, "Toggle Backlight", 40, 252, _
        124, 24
    CONTROL ADD LABEL,       hDlg, %IDC_LABEL5, "", 38, 169, 128, 32
    CONTROL ADD PROGRESSBAR, hDlg, %IDC_PROGRESSBAR1, "ProgressBar1", 208, _
        312, 328, 24 ,  %WS_CHILD OR %WS_VISIBLE OR %PBS_SMOOTH
    CONTROL ADD PROGRESSBAR, hDlg, %IDC_PROGRESSBAR2, "ProgressBar2", 232, 8, _
        26, 296, %PBS_VERTICAL OR %WS_CHILD OR %WS_VISIBLE OR %PBS_SMOOTH
    CONTROL ADD LABEL,       hDlg, %IDC_LAB_VER, "", 24, 320, 80, 16
'     CONTROL ADD BUTTON,      hDlg, %IDC_BUT_TEST, "TEST", 38, 290, 124, 24
    FONT NEW "Arial Black", 14, 1, %ANSI_CHARSET TO hFont1

    CONTROL SET FONT hDlg, %IDC_LABEL1, hFont1

    AttachMENU1 hDlg
#PBFORMS END DIALOG
   CONTROL SET TEXT hdlg,%IDC_LAB_VER,$VER

   PROGRESSBAR SET RANGE hDlg, %IDC_PROGRESSBAR2, 0, 100
   PROGRESSBAR SET STEP hDlg, %IDC_PROGRESSBAR2, 1
   PROGRESSBAR SET RANGE hDlg, %IDC_PROGRESSBAR1, 0, 100
   PROGRESSBAR SET STEP hDlg, %IDC_PROGRESSBAR1, 1

'    GRAPHIC ATTACH hdlg, %IDC_GRAPHIC1
'    CONTROL HIDE HDlg, %IDC_GRAPHIC2
'    CONTROL HIDE HDlg, %IDC_PROGRESSBAR2
'    GRAPHIC SET SIZE W ,H
'    GRAPHIC RENDER BITMAP "#111", (0,0)-(319,239)


  GRAPHIC ATTACH hdlg, %IDC_GRAPHIC2
  CONTROL HIDE HDlg, %IDC_GRAPHIC1
  CONTROL HIDE HDlg, %IDC_PROGRESSBAR1
  GRAPHIC SET SIZE H,W
  GRAPHIC RENDER BITMAP "#110", (0,0)-(239,319)

'setup the progress


    'load the Files
      CONTROL DISABLE HDLG, %IDC_BUTSEND
      CONTROL DISABLE hdlg,%IDC_BACKLIGHT_TOG
      MENU SET STATE hMenu, BYCMD  %IDM_TOOLS_SENDTODISPLAY, %MFS_DISABLED
      getsetting("LastUsedFile","F1",defs)
      MENU SET TEXT hMenu, BYCMD  %IDM_FILE_1, dispfilename(defs)
      getsetting("LastUsedFile","F2",defs)
      MENU SET TEXT hMenu, BYCMD  %IDM_FILE_2, dispfilename(defs)
      getsetting("LastUsedFile","F3",defs)
      MENU SET TEXT hMenu, BYCMD  %IDM_FILE_3, dispfilename(defs)
      getsetting("LastUsedFile","F4",defs)
      MENU SET TEXT hMenu, BYCMD  %IDM_FILE_4, dispfilename(defs)
      getsetting("LastUsedFile","F5",defs)
      MENU SET TEXT hMenu, BYCMD  %IDM_FILE_5, dispfilename(defs)
    DIALOG SHOW MODAL hDlg, CALL ShowMAINProc TO lRslt

#PBFORMS BEGIN CLEANUP %IDD_MAIN
    FONT END hFont1
#PBFORMS END CLEANUP

    FUNCTION = lRslt
END FUNCTION
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
FUNCTION ShowWB_SETUP(BYVAL hParent AS DWORD) AS LONG
    LOCAL lRslt AS LONG
    LOCAL defs AS STRING
    LOCAL I AS DWORD
#PBFORMS BEGIN DIALOG %IDD_WB_SETUP->->
    LOCAL hDlg  AS DWORD

    DIALOG NEW PIXELS, hParent, "Wirebridge Setup", ,, 308, 285, _
        %WS_POPUP OR %WS_BORDER OR %WS_DLGFRAME OR %WS_CAPTION OR _
        %WS_SYSMENU OR %WS_CLIPSIBLINGS OR %WS_VISIBLE OR %DS_MODALFRAME OR _
        %DS_3DLOOK OR %DS_NOFAILCREATE OR %DS_SETFONT, %WS_EX_CONTROLPARENT _
        OR %WS_EX_LEFT OR %WS_EX_LTRREADING OR %WS_EX_RIGHTSCROLLBAR, TO _
        hDlg
    CONTROL ADD BUTTON,   hDlg, %IDC_WB_BUT, "Cancel", 72, 248, 72, 32
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL2, "SPI Pins for Wirebridge Model " + _
        "1A", 24, 9, 199, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABCLK, "CLK / SLK ", 22, 36, 79, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL3, "MISO   (Not used) ", 22, 54, _
        97, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL4, "MOSI ", 22, 72, 94, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABRES, "RES  Reset  ", 23, 108, 94, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABDC, "DC   Data/Command ", 22, 90, _
        121, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL7, "BLK Background LED ", 22, 126, _
        121, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL8, "Fixed Pin 8", 128, 36, 56, 16, _
        %WS_CHILD OR %WS_VISIBLE OR %SS_RIGHT, %WS_EX_LEFT OR _
        %WS_EX_LTRREADING
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL9, "Fixed Pin 7", 128, 54, 56, 16, _
        %WS_CHILD OR %WS_VISIBLE OR %SS_RIGHT, %WS_EX_LEFT OR _
        %WS_EX_LTRREADING
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL10, "Fixed Pin 6", 128, 72, 56, 16, _
        %WS_CHILD OR %WS_VISIBLE OR %SS_RIGHT, %WS_EX_LEFT OR _
        %WS_EX_LTRREADING
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL15, "CS ChipSelect if present ", _
        22, 144, 121, 18
    CONTROL ADD CHECKBOX, hDlg, %IDC_CHECKCS, "Enable", 218, 142, 56, 16
    CONTROL ADD TEXTBOX,  hDlg, %IDC_TEXT_PIN_DC, "5", 160, 86, 19, 18
    CONTROL ADD TEXTBOX,  hDlg, %IDC_TEXT_PINRES, "4", 160, 104, 19, 18
    CONTROL ADD TEXTBOX,  hDlg, %IDC_TEXTBLK, "3", 160, 122, 19, 18
    CONTROL ADD TEXTBOX,  hDlg, %IDC_TEXTCS, "2", 160, 140, 19, 18
    CONTROL ADD CHECKBOX, hDlg, %IDC_CHECKBAK, "Enable", 217, 125, 56, 16
    CONTROL ADD BUTTON,   hDlg, %IDC_UPDATE, "Update && Exit", 168, 248, 136, _
        32
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL16, "baud rate", 16, 164, 78, 24, _
        %WS_CHILD OR %WS_VISIBLE OR %SS_RIGHT OR %SS_CENTERIMAGE, _
        %WS_EX_LEFT OR %WS_EX_LTRREADING
    CONTROL ADD COMBOBOX, hDlg, %IDC_COMBOBAUD, , 104, 164, 104, 80, _
        %WS_CHILD OR %WS_VISIBLE OR %WS_TABSTOP OR %CBS_DROPDOWN, _
        %WS_EX_LEFT OR %WS_EX_LTRREADING OR %WS_EX_RIGHTSCROLLBAR
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL17, "Blank Display On Send", 24, _
        192, 121, 19
    CONTROL ADD CHECKBOX, hDlg, %IDC_CHECKBLANK, "Enable", 171, 191, 56, 16
    CONTROL ADD LABEL,    hDlg, %IDC_LABEL19, "Turn Off Display On Send. " + _
        "Needs Background enabled", 24, 212, 133, 29
    CONTROL ADD CHECKBOX, hDlg, %IDC_CHECKBACKLIGHT, "Enable", 171, 217, 56, _
        18
#PBFORMS END DIALOG
Getsetting("WB_Setup","SPI_BacklightOffonSend",defs)
I=VAL(defs):IF I<>0 THEN I=1
CONTROL SET CHECK hdlg,%IDC_CHECKBACKLIGHT,I
Getsetting("WB_Setup","SPI_DisplayOffonSend",defs)
I=VAL(defs):IF I<>0 THEN I=1
CONTROL SET CHECK hdlg,%IDC_CHECKBLANK,I
Getsetting("WB_Setup","SPI_CS PIN",defs)
CONTROL SET TEXT hdlg, %IDC_textCS,TRIM$(defs)
Getsetting("WB_Setup","SPI_CS Use",defs)
I=VAL(defs):IF I<>0 THEN I=1
CONTROL SET CHECK hdlg,%IDC_CHECKCS,I
Getsetting("WB_Setup","SPI_Reset PIN",defs)
CONTROL SET TEXT hdlg, %IDC_TEXT_PINRES,TRIM$(defs)
Getsetting("WB_Setup","SPI_DC PIN",defs)
CONTROL SET TEXT hdlg, %IDC_TEXT_PIN_DC   ,TRIM$(defs)
Getsetting("WB_Setup","SPI_Backlight PIN",defs)
CONTROL SET TEXT hdlg, %IDC_textBLK,TRIM$(defs)
Getsetting("WB_Setup","SPI_Backlight Use",defs)
I=VAL(defs):IF I<>0 THEN I=1
CONTROL SET CHECK hdlg,%IDC_CHECKBAK,I
  LOCAL word_result AS DWORD
  LOCAL index AS WORD
  index = 0
   Word_Result =  WB_GetBauds(gdevinfo,index )
 ' msgbox "Index" +Str$(index)+" num = "+dec$(word_Result)

IF Word_result> 30  THEN 'check really there
 FOR I=1 TO 30
    index=I
     Word_Result =  WB_GetBauds(gdevinfo,index )
     COMBOBOX ADD hdlg, %IDC_COMBOBAUD,TRIM$(USING$("###,###,###" ,word_result) )
 NEXT
END IF
    getsetting( "WB_Setup","SPI_Baud",defs)
    'msgbox defs
   COMBOBOX FIND hDlg, %IDC_COMBOBAUD, 1, (TRIM$(defs)) TO I

  IF I=0 THEN I=12
     COMBOBOX SELECT hdlg, %IDC_COMBOBAUD,I


    DIALOG SHOW MODAL hDlg, CALL ShowWB_SETUPProc TO lRslt

#PBFORMS BEGIN CLEANUP %IDD_WB_SETUP
#PBFORMS END CLEANUP

    FUNCTION = lRslt
END FUNCTION
'------------------------------------------------------------------------------
FUNCTION testBaud(rate AS LONG)AS LONG
    LOCAL I AS LONG
    LOCAL index AS WORD
    LOCAL Word_result AS DWORD
    index = 0
   Word_Result =  WB_GetBauds(gdevinfo,index )
  '  msgbox "Index" +Str$(index)+" num = "+dec$(word_Result)

IF Word_result > 1 THEN 'check really there
 FOR I=1 TO word_result
   Word_Result =  WB_GetBauds(gdevinfo,I)
   IF rate=word_result THEN
       FUNCTION = %TRUE
       EXIT FUNCTION

   END IF
 NEXT
END IF
  FUNCTION=%False
END FUNCTION



'---------------------------------------------------------------------------
SUB Quit (Ahndl AS DWORD)
    ' Kill the dialog and let PBMAIN() continue
    DIALOG END Ahndl, 0
END SUB


SUB Init9431(DataList() AS STRING)
     REDIM datalist(23,1) AS STRING
     Datalist(0,0)="23" 'Number of entries
     datalist(1,0)=CHR$(&HEF)
     datalist(1,1)=CHR$(&H03, &H80, &H02)
     datalist(2,0)=CHR$(&HCF)
     datalist(2,1)=CHR$(&H00, &HC1, &H30)
     datalist(3,0)=CHR$(&HED)
     datalist(3,1)=CHR$(&H64, &H03, &H12, &H81)
     datalist(4,0)=CHR$(&HE8)
     datalist(4,1)=CHR$(&H85, &H00, &H78)
     datalist(5,0)=CHR$(&HCB)
     datalist(5,1)=CHR$(&H39, &H2C, &H00, &H34, &H02)
     datalist(6,0)=CHR$(&HF7)
     datalist(6,1)=CHR$(&H20)
     datalist(7,0)=CHR$(&HEA)
     datalist(7,1)=CHR$(&H00, &H00)
     datalist(8,0)=CHR$(%ILI9341_PWCTR1 )
     datalist(8,1)=CHR$(&H23)
     datalist(9,0)=CHR$(%ILI9341_PWCTR2 )
     datalist(9,1)=CHR$(&H10)
     datalist(10,0)=CHR$(%ILI9341_VMCTR1)
     datalist(10,1)=CHR$(&H3e, &H28)
     datalist(11,0)=CHR$(%ILI9341_VMCTR2 )
     datalist(11,1)=CHR$(&H86)
     datalist(12,0)=CHR$(%ILI9341_MADCTL)
     datalist(12,1)=CHR$(&H48)
     datalist(13,0)=CHR$(%ILI9341_VSCRSADD)
     datalist(13,1)=CHR$(&H00)
     datalist(14,0)=CHR$(%ILI9341_PIXFMT)
     datalist(14,1)=CHR$(&H55)
     datalist(15,0)=CHR$(%ILI9341_FRMCTR1 )
     datalist(15,1)=CHR$(&H00, &H18)
     datalist(16,0)=CHR$(%ILI9341_DFUNCTR)
     datalist(16,1)=CHR$(&H08, &H82, &H27)
     datalist(17,0)=CHR$(&HF2)
     datalist(17,1)=CHR$(&H00)
     datalist(18,0)=CHR$(%ILI9341_GAMMASET)
     datalist(18,1)=CHR$(&H01)
     datalist(19,0)=CHR$(%ILI9341_GMCTRP1)
     datalist(19,1)=CHR$(&H0F, &H31, &H2B, &H0C, &H0E, &H08, &H4E, &HF1, &H37, &H07, &H10, &H03, &H0E, &H09, &H00)
     datalist(20,0)=CHR$(%ILI9341_GMCTRN1)
     datalist(20,1)=CHR$(&H00, &H0E, &H14, &H03, &H11, &H07, &H31, &HC1, &H48, &H08, &H0F, &H0C, &H31, &H36, &H0F)
     datalist(21,0)=CHR$(%ILI9341_SLPOUT)
     datalist(21,1)=CHR$(&H80)
     datalist(22,0)=CHR$(%ILI9341_DISPON)
     datalist(22,1)=CHR$(&H80)
     datalist(23,0)=CHR$(&H00)
     datalist(23,1)=""

END SUB


#INCLUDE "WB_IMPORT.INC"

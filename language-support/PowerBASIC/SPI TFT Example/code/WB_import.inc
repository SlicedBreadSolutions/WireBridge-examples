
FUNCTION Dispfilename(fn AS STRING) AS STRING
    LOCAL a,b AS STRING
      IF LEN( fn) >50 THEN
        a=LEFT$(fn,10)
        b=RIGHT$(FN,35)
        A=A+"....."+B
        FUNCTION = A
     ELSE
         FUNCTION=fn
     END IF

 END FUNCTION

SUB updateluf (filename AS STRING,Position AS LONG,Xnew AS LONG )

LOCAL F1,F2,F3,F4,F5,F1P AS STRING

IF xnew=0 THEN 'its new
       IF filename="" THEN EXIT SUB
       getsetting ("LastUsedFile","F4",F4)
       savesetting("LastUsedFile","F5",F4)
        MENU SET TEXT hMenu, BYCMD  %IDM_FILE_5, dispfilename(F4)
       getsetting ("LastUsedFile","F3",F3)
       savesetting("LastUsedFile","F4",F3)
        MENU SET TEXT hMenu, BYCMD  %IDM_FILE_4,dispfilename(F3)
       getsetting ("LastUsedFile","F2",F2)
       savesetting("LastUsedFile","F3",F2)
        MENU SET TEXT hMenu, BYCMD  %IDM_FILE_3,dispfilename(F2)
       getsetting ("LastUsedFile","F1",F1)
       savesetting("LastUsedFile","F2",F1)
        MENU SET TEXT hMenu, BYCMD  %IDM_FILE_2,dispfilename(F1)
       savesetting("LastUsedFile","F1",filename)
        MENU SET TEXT hMenu, BYCMD  %IDM_FILE_1, dispfilename(filename)
       F1P = PATHNAME$(PATH, filename)
        savesetting("LastUsedFile","LastPath",F1P)
ELSE 'must be a reused one

    SELECT CASE Position
        CASE 1 ' top of list
           ' Nothing to do, keep position and path
        CASE 2  'need to swap 1 and 2 and update then path
          getsetting ("LastUsedFile","F1",F1)
          getsetting ("LastUsedFile","F2",F2)
          SWAP F1,F2       'F1>>F2 f2>>f1
           Savesetting ("LastUsedFile","F1",F1)
           Savesetting ("LastUsedFile","F2",f2)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_1, dispfilename(F1)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_2, dispfilename(F2)

        CASE 3
           getsetting ("LastUsedFile","F1",F1)
           GetSetting ("LastUsedFile","F2",F2)
           getsetting ("LastUsedFile","F3",F3)
           SWAP F1,F3  ' old F1 >> F3  F3 >> F1
           SWAP F2,F3  ' F2 >> f3   F3>> F2
           Savesetting ("LastUsedFile","F1",F1)
           Savesetting ("LastUsedFile","F2",F2)
           Savesetting ("LastUsedFile","F3",F3)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_1, dispfilename(F1)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_2, dispfilename(F2)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_3, dispfilename(F3)

        CASE 4
           getsetting ("LastUsedFile","F1",F1)
           GetSetting ("LastUsedFile","F2",F2)
           getsetting ("LastUsedFile","F3",F3)
           getsetting ("LastUsedFile","F4",F4)
           SWAP F1,F4  ' old F1 >> F4  F4 >> F1
           SWAP F2,F4  ' F2 >> f4   F4>> F2
           SWAP F3,F4  ' F3 >> F4   F4>> F3
           Savesetting ("LastUsedFile","F1",F1)
           Savesetting ("LastUsedFile","F2",F2)
           Savesetting ("LastUsedFile","F3",F3)
           Savesetting ("LastUsedFile","F4",F4)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_1, dispfilename(F1)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_2, dispfilename(F2)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_3, dispfilename(F3)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_4, dispfilename(F4)

         CASE 5
           getsetting ("LastUsedFile","F1",F1)
           GetSetting ("LastUsedFile","F2",F2)
           getsetting ("LastUsedFile","F3",F3)
           getsetting ("LastUsedFile","F4",F4)
           getsetting ("LastUsedFile","F5",F5)
           SWAP F1,F5  ' old F1 >> F4  F4 >> F1
           SWAP F2,F5  ' F2 >> f4   F4>> F2
           SWAP F3,F5  ' F3 >> F4   F4>> F3
           SWAP F4,F5    'F4 >> F5   F5>>F4
           Savesetting ("LastUsedFile","F1",F1)
           Savesetting ("LastUsedFile","F2",F2)
           Savesetting ("LastUsedFile","F3",F3)
           Savesetting ("LastUsedFile","F4",F4)
           Savesetting ("LastUsedFile","F5",F5)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_1, dispfilename(F1)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_2, dispfilename(F2)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_3, dispfilename(F3)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_4, dispfilename(F4)
           MENU SET TEXT hMenu, BYCMD  %IDM_FILE_5, dispfilename(F5)
         END SELECT
          Getsetting ("LastUsedFile","F1",F1)
           F1P= PATHNAME$(PATH, filename)
           savesetting("LastUsedFile","LastPath",F1P)

  END IF
END SUB

SUB getsetting (sec AS STRING , key AS STRING ,defs AS STRING)
'this is OK

LOCAL Wfname AS WSTRINGZ * 255
LOCAL result AS LONG
LOCAL tmp AS STRING
LOCAL secw AS WSTRINGZ * 30 , keyw AS WSTRINGZ * 30 ,defsw AS WSTRINGZ * 30
tmp = EXE.PATH$+"WB_TFT_SETTING.INI"
Wfname=tmp
secw=sec:keyw=key
result = getPrivateProfileStringW (SecW, KeyW, "", defsW, 256, Wfname)
defs=defsW

END SUB

SUB SaveSetting(sec AS STRING , key AS STRING ,defs AS STRING )

LOCAL tmp AS STRING
DIM fnamew AS WSTRINGZ * %MAX_PATH , result AS LONG
DIM xtmp AS STRING
LOCAL secW AS WSTRINGZ * 30
LOCAL keyW AS WSTRINGZ * 30
LOCAL defsW AS WSTRINGZ * 255


tmp = EXE.PATH$+"WB_TFT_SETTING.INI"
fnamew=tmp
secW=sec
KeyW=key
defsw=defs
result = WritePrivateProfileStringW (Secw, Keyw, Defsw, FNamew )
IF result=0 THEN
     result=Getlasterror
     SELECT CASE result
        CASE 2
          xtmp = "Error 2 - File not found"
        CASE 3
          xtmp = "Error 3 - Path not found"
        CASE 5
         xtmp = "Error 5 - Access denied (read only?)"
        CASE 19
         xtmp = "Error 19 - Write protected media"
        CASE ELSE
        xtmp = "Error"+STR$(result)

     END SELECT
    MSGBOX "Failed to write to:- "+$CRLF+fnamew+$CRLF+xtmp, _
    %MB_ICONERROR, "SpaceControl"

END IF


END SUB



FUNCTION Echeck AS LONG
   LOCAL emsg AS STRING
        FUNCTION=0
       CALL DeErr(Emsg)
    IF emsg<>"" THEN
      MSGBOX emsg ,%MB_ICONERROR,"DEMO"
      FUNCTION=1
    END IF
    clrerr
END FUNCTION

SUB FileLocateOpen (Hdlg AS LONG,filename AS STRING)     ' load data -tif - file
    DIM filevar AS WSTRING
    LOCAL ofn AS LONG
    ofn = %OFN_EXPLORER OR %OFN_FILEMUSTEXIST
  IF NOT ISFILE(filename) THEN
   #DEBUG CODE OFF
  DISPLAY OPENFILE hdlg, , , "Image Files",filename, _
  CHR$("Image Files", 0, "*.tif;*.psd;*.png;*.gif;*.jpg;*.bmp", 0) + _
  CHR$("Jpeg files  *.jpg *.jpeg", 0, "*.jpg;*.jpeg", 0) + _
  CHR$("Tiff files *.tif", 0, "*.tif", 0) + _
  CHR$("PNG Files  *.png", 0, "*.png", 0) + _
  CHR$("PSD Files *.psd", 0, "*.psd", 0) + _
  CHR$("GIF Files *gif ", 0, "*.gif", 0) + _
  CHR$("Windows bitmap *.bmp", 0, "*.bmp", 0)  _
   , "", "", Ofn TO filevar
  ELSE
      filevar=filename
  END IF

  IF filevar="" THEN
      filename=""
      EXIT SUB
  END IF
  Filename=FileVar

  CALL ReadImage(hdlg,filevar)
 ' MSGBOX filevar
  IF filevar="" THEN
      filename=""
      EXIT SUB
  END IF

'  CALL storefile(filevar)

 END SUB

SUB readimage(hdlg AS LONG,BYREF tiffile AS WSTRING)

    DIM fname AS WSTRINGZ * %MAX_PATH
    DIM Sname AS STRINGZ * %MAX_PATH
    DIM Tname AS WSTRINGZ * %MAX_PATH
    LOCAL SX,SY,J AS LONG
    LOCAL X,Y,NX,NY AS DWORD
    DIM pimages AS DWORD
    DIM result AS LONG
    LOCAL Pbuffer,nbuff AS DWORD
    DIM bsize AS LONG
    LOCAL tmp AS STRING
    LOCAL tmpW AS WSTRING
    LOCAL dibhead AS STRING
   LOCAL ra,rb AS DOUBLE

   DIM TempPath AS ASCIIZ * (%MAX_PATH+2)

    tiffile = TRIM$(tiffile,CHR$(34))
    nbuff = %MAX_PATH+2
    Result = GetTempPathA(  nBuff, tempPath)
    tmp = temppath
     tmpW = PATHNAME$(EXTN, tiffile) ' returns  ".ext"
     sname =tmp+"\Vtemp"+tmpW
     fname = tiffile
     Tname = sname
     result = CopyFileW ( BYREF fname ,BYREF tname, %False)
      IF result = 0 THEN ';error
            'run with original
            sname = tiffile
            tname=""
       END IF
    clrerr

   ilEnable(%IL_ORIGIN_SET)
   IF echeck = 1 THEN tiffile="": EXIT SUB
   ILOriginFunc (%IL_ORIGIN_UPPER_LEFT)
   IF echeck = 1 THEN tiffile="": EXIT SUB
   ILGenImages(1,pimages)
   IF echeck = 1 THEN tiffile="": EXIT SUB
   ILBindImage (pimages)
   IF echeck = 1 THEN tiffile="": EXIT SUB
   result=ILLoadImage(sname)

    J=result
   IF Tname<>"" THEN KILL tname
    IF result=%IL_FALSE THEN
    IF echeck = 1 THEN tiffile="": EXIT SUB
   END IF
    ILGetIntegerv (%IL_IMAGE_HEIGHT,Y)
   IF echeck = 1 THEN tiffile="": EXIT SUB
    ILGetIntegerv (%IL_IMAGE_WIDTH,X)

   IF echeck = 1 THEN tiffile="": EXIT SUB
     SY= %ILI9341_TFTWIDTH   'the screen is 240 / 320 but we use 320 / 240
     SX= %ILI9341_TFTHEIGHT '
     SX= %ILI9341_TFTHEIGHT '

  IF Y>X THEN   'portrait
   CONTROL NORMALIZE HDlg, %IDC_GRAPHIC2
   CONTROL HIDE HDlg, %IDC_GRAPHIC1
   CONTROL NORMALIZE HDlg,  %IDC_PROGRESSBAR2
   CONTROL HIDE HDlg, %IDC_PROGRESSBAR1
   GRAPHIC ATTACH hdlg, %IDC_GRAPHIC2
   PROGRESSBAR SET RANGE hDlg, %IDC_PROGRESSBAR2, 0, 100
   PROGRESSBAR SET STEP hDlg, %IDC_PROGRESSBAR1, 2
   GRAPHIC SET SIZE %ILI9341_TFTWIDTH+8, %ILI9341_TFTHEIGHT+8

  ELSE   'landscape
   CONTROL NORMALIZE HDlg, %IDC_GRAPHIC1
   CONTROL HIDE HDlg, %IDC_GRAPHIC2
   CONTROL NORMALIZE HDlg,  %IDC_PROGRESSBAR1
   CONTROL HIDE HDlg, %IDC_PROGRESSBAR2
   GRAPHIC ATTACH hdlg, %IDC_GRAPHIC1
   PROGRESSBAR SET RANGE hDlg, %IDC_PROGRESSBAR1, 0, 100
   PROGRESSBAR SET STEP hDlg, %IDC_PROGRESSBAR1, 1

      GRAPHIC SET SIZE  %ILI9341_TFTHEIGHT+8 ,%ILI9341_TFTWIDTH+8
     SWAP SY,SX
    '
  END IF

   'This is the supplied size

  '    SWAP x,y
     ra = Y / X
     rb = SY / SX

     IF RB=RA THEN    '
         X =  SX
         Y =  SY
     ELSEIF X<SX AND Y<SY THEN
         X =  X
         Y =  Y
     ELSEIF RA < 1 THEN '  landscape set X
            RB = X/SY
            X=Sy
            Y=Y/RB
            IF Y> sy THEN
            rb=Y/SX
            Y=SX
            X=X/rb
            END IF
     ELSEIF RA > 1  THEN  'this is portrait  so set the Y
             ' msgbox "RA+ sx sy  "+Using$("#####.##",RA, sx, sy)
           RB = X/SY
           y= Y/RB '/ rb  'X/RB
           X= SY '/rb ' SY
           IF Y>SX THEN
            rb=Y/SX
            Y=SX
            X=X/rb
           END IF
     ELSE
          MSGBOX  "not covered",, "Demo"
     END IF

      result= ILuScale (X,Y,1)
      ILGetIntegerv (%IL_IMAGE_HEIGHT,Y)
       IF echeck = 1 THEN tiffile="": EXIT SUB
      ILGetIntegerv (%IL_IMAGE_WIDTH,X)
       IF echeck = 1 THEN tiffile="": EXIT SUB
           bsize=(X*Y*4)
           tiffbytes=STRING$(bsize,0)
           pbuffer=STRPTR(Tiffbytes)
      result = ILCopyPixels ( 0,0,0,X,Y,1,%IL_RGBA,%IL_UNSIGNED_BYTE ,PBuffer )

      IF result=%IL_FALSE THEN
          MSGBOX "Failed to Convert Image",%MB_ICONWARNING,"Demo"
      IF echeck = 1 THEN
           tiffile="":
           IF Tname<>"" THEN KILL tname
           EXIT SUB
      END IF
      IF Tname<>"" THEN KILL tname
         EXIT SUB
      END IF
    IF echeck = 1 THEN tiffile="": EXIT SUB

       IF X>Y THEN   ' create the Landscape buffer Lbuffer
              result = ilClearColour(BYVAL 0,BYVAL 0, BYVAL 200, BYVAL 0)
              result= iluEnlargeCanvas (500 ,500,1)
              ILURotate(BYVAL 90)
                  ' off=(320-Y)/2

              result = iluCrop (BYVAL 130, BYVAL 90,BYVAL 0,BYVAL 240,BYVAL 320,1 )
              ILGetIntegerv (%IL_IMAGE_HEIGHT,NY)
              ILGetIntegerv (%IL_IMAGE_WIDTH,NX)
              bsize=(NX*NY*4)
              lbuffer=STRING$(bsize,0)
              Pbuffer = STRPTR(LBuffer)
              result = ILCopyPixels ( 0,0,0,NX,NY,1,%IL_RGBA,%IL_UNSIGNED_BYTE ,PBuffer )
              dibhead= MKL$(NX)   ')         'Screen X in pixels
              dibhead=dibhead+MKL$(NY)
              Lbuffer=dibhead+lbuffer
             '  MSGBOX USING$("######",NX,NY,SX,SY)
          END IF

' pad functions
J=0
CALL padimage(X ,Y ,J )   'makes the onscreen paded version
IF X>Y THEN '(landscape)
    J=1
 '   CALL padimage(X ,Y ,J )
'makes the portrait version for tft screen
END IF

  '    GRAPHIC SET BITS Lbuffer

END SUB

SUB Padimage(X AS LONG,Y AS LONG,J AS LONG)
LOCAL dibhead AS STRING
LOCAL bmpbuff AS STRING
  'this fills the edges , placing the image in the centre.
   LOCAL Pixptr AS RGBA PTR
   LOCAL Pix AS rgba
    LOCAL PAD AS rgba
   LOCAL M,sx,sy,I,K,L AS LONG
   LOCAL Ypad,Xpad,XpadL,xpadR,YpadT,YpadB AS LONG
   LOCAL XPR,XPL,ypt,ypb AS STRING
   LOCAL Lin,tmp AS STRING
   IF J=0 THEN 'use tiffbytes 1= Lbuffer - this is portrait
      GRAPHIC GET BITS TO BMPBuff
      DIBHEAD = LEFT$(bmpbuff,8)
      bmpbuff = RIGHT$(bmpbuff,-8)
      SX= CVL (LEFT$(DibHead,4))  'Screen X in pixels
      SY= CVL (MID$(DibHead,5,4)) ' Screen Y (hight) pixels
   ELSE
    SX=240
    sy=320
    DIBHEAD = LEFT$(Lbuffer,8)
    X= CVL (LEFT$(DibHead,4))
    Y= CVL (MID$(DibHead,5,4))
     ' MSGBOX USING$("######",X,Y,SX,SY),,"DEMO"
   END IF
         Xpad = SX - X
         XpadL=INT(Xpad/2)
         XpadR=Xpad-XpadL
         Ypad = SY - Y '(lines)
         YPadT = INT(Ypad/2)
         YpadB = Ypad-YpadT
         PAD.R=0:Pad.G=0:Pad.B=0:Pad.A=0
         tmp=CHR$(pad.R,pad.G,pad.b,0)
         XPR= REPEAT$(XPADR,tmp )
         XPL= REPEAT$(XPADL,tmp)
         YPT = REPEAT$(YPADT*SX,tmp)
         YPb = REPEAT$(YPADb*SX,tmp)
IF J=0 THEN
         m=STRPTR(tiffbytes)
         K=LEN(tiffbytes)
ELSE
      m=STRPTR(Lbuffer)
      K=LEN(Lbuffer)
END IF
 FOR I=1 TO K STEP 4
     Pixptr=M
     pix=@pixptr
     SWAP pix.R,pix.B
     @pixptr=pix
     m=m+4
 NEXT
     L= X*4
     Lin =  ypt   'top padding
     tmp=""
     FOR i=1 TO K STEP L
      IF J=0 THEN
         TMP = xpl+MID$(tiffbytes,i,L)+xPR
      ELSE
         TMP = xpl+MID$(Lbuffer,i,L)+xPR
      END IF
          lIN=lIN+TMP
         TMP=""
     NEXT
     LIN=lin + Ypb
     BMPBUfF=dibhead+lin
     IF J=0 THEN
      GRAPHIC SET BITS  BMPBuff
     ELSE
        dibhead= MKL$(X)   ')         'Screen X in pixels
        dibhead=dibhead+MKL$(Y)
       Lbuffer = dibhead+bmpbuff
     END IF
     X=sx
     Y=Sy

END SUB


SUB send2TFT(hdlg AS LONG)
    LOCAL bmpbuff,tmp,lin ,dibhead AS STRING
    LOCAL M AS STRING PTR
    LOCAL Pixptr AS RGBA PTR
    LOCAL Pix AS RGBA
    LOCAL byte_result,zcount AS BYTE
    LOCAL sx,sy,I,j,X,bs,landscape,xbp AS LONG  '   ,SX=320 SY=240
    LOCAL SB, RB AS STRING
    LOCAL RBp, sbp AS STRING PTR
    BS=60    'batch size of bytes  60 default  can be played with
    landscape = 0
      GRAPHIC GET BITS TO BMPBuff
      DIBHEAD = LEFT$(bmpbuff,8)
      tmp = RIGHT$(bmpbuff,-8)
      SX= CVL (LEFT$(DibHead,4))  'Screen X in pixels
      SY= CVL (MID$(DibHead,5,4)) ' Screen Y (height) pixels
      BMPBuff=tmp 'Rawdata in BGR format 4bytes /pixel

  IF SX>Sy THEN 'its landscape so use the Lbuffer
       landscape=1
       BMPbuff   = Lbuffer
       DIBHEAD = LEFT$(bmpbuff,8)
       tmp = RIGHT$(bmpbuff,-8)
       SX= CVL (LEFT$(DibHead,4))  'Screen X in pixels
       SY= CVL (MID$(DibHead,5,4)) ' Screen Y (height) pixels
       BMPBuff=tmp 'Rawdata in BGR format 4bytes /pixel
  END IF

     'its formated as 320x 240y screen is actuall 240/320 and fed as 240
     ' convert from BGR to RGB 16 first uses BBGRA or BRGBA
M=STRPTR(bmpbuff)  'NOTE this is BGR!!!!!!
lin=""
 FOR I=1 TO LEN(bmpBuff) STEP 4
     Pixptr=M
     pix=@pixptr
     M=M+4
     IF landscape=0 THEN
     Lin = lin + MKWRD$(BBGRA(PIX))
     ELSE
     Lin = lin + MKWRD$(BRGBA(PIX))
     END IF
 NEXT      'returns with buffer (lin) made of 2byte pixels for the TFT
 IF landscape=1 THEN
    J=%IDC_PROGRESSBAR1
    CONTROL NORMALIZE HDlg, %IDC_PROGRESSBAR1
    lin=RIGHT$(lin,-1)+CHR$(0)
 ELSE
    J=%IDC_PROGRESSBAR2
    CONTROL NORMALIZE HDlg, %IDC_PROGRESSBAR2
    lin=RIGHT$(lin,-1)+CHR$(0)
 END IF
 x=LEN(lin)' this is the pixel buffer
   RB="    "        'setup the recieve buffer
   rbp=STRPTR(rb)   'and pointer
   sb=CHR$(&H2c)+CHR$(0)  'now the command to send data
   sbp=STRPTR(sb)         ' and its pointer - its actually one byte
   zcount = 1
   'DC is set for Command
   Byte_result = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.low , BYVAL %NULL)
   Byte_result = WB_SPI_Transfer(gWB_Devhandle, zcount,Sbp,rbp ,BYVAL %NULL) ' writeCmd(&H2C)
   Byte_result = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.high, BYVAL %NULL)
   PROGRESSBAR SET RANGE hDlg, J, 0, 100
   PROGRESSBAR SET STEP hDlg, J, 1
       sbp=STRPTR(lin)   'Pointer to Start of buffer
       Xbp=sbp
FOR I=0 TO x STEP bs    'Step along the buffer in BS (60) byte chunks
       Byte_result = WB_SPI_Transfer(gWB_Devhandle, bs,Xbp,rbp ,BYVAL %NULL)  'Send
       PROGRESSBAR SET POS hDlg, J, INT(100*(I/X))
       Xbp=Xbp+60        'add current position (I)
       DIALOG DOEVENTS 0     'required to keep progress bar running
NEXT
   sb=CHR$(&H00) 'send the finished command!
   sbp=STRPTR(sb)
   Byte_result = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.low , BYVAL %NULL)
   Byte_result = WB_SPI_Transfer(gWB_Devhandle, 1,Sbp,rbp ,BYVAL %NULL) ' writeCmd(&H2C)
   Byte_result = WB_WritePinDigital(gWB_Devhandle ,%DC,%DP_Value.high , BYVAL %NULL)

END SUB


FUNCTION P2N(pin AS LONG)AS BYTE

     SELECT CASE pin
         CASE 1
           FUNCTION = %WB_Model1B.pin1
         CASE 2
           FUNCTION = %WB_Model1B.pin2
         CASE 3
           FUNCTION = %WB_Model1B.pin3
         CASE 4
           FUNCTION = %WB_Model1B.pin4
         CASE 5
           FUNCTION = %WB_Model1B.pin5
         CASE 6
           FUNCTION = %WB_Model1B.pin6
         CASE 7
           FUNCTION = %WB_Model1B.pin7
         CASE 8
           FUNCTION = %WB_Model1B.pin8
         CASE ELSE
           FUNCTION = 0
      END SELECT

END FUNCTION

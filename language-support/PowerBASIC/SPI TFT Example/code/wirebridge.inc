  ' PowerBasic Inc file for Wirebridge  2024 feb (2)02 dll is OK
 'This file is free to use by anyone for any purpose including for commercial purposes.
 '


%MAX_LABEL_CHARS = 30              'use in main program to set the label
$DLLDATE = "2024/04/05" '  YY MM DD " 'track the DLL date/ime an update can break
'                                  'the functions / enums or types

' usage in PB  Numeric Variable = %name.equatename
' example X = %wb_Model1A.Pin1  would set x to 5
' Same for all enum
 '  typedef unsigned char WB_PIN_NUMBER; wb_pin_number as byte
     ENUM WB_OUTCOME  ': byte {
                Success = 0
                Failure = 1
     END ENUM

     ENUM WB_MODEL ' : byte {
                NONE = 0
                Unsupported = 255
                Model1A = 1
                Model1B = 2
                Model2A = 3
                Model2B = 4
                Model3 = 5
                Model4 = 6
                LAST_MODEL = 6
      END ENUM


    ENUM WB_Model1A ': unsigned char {
      Pin1 = 5
      Pin2 = 4
      Pin3 = 13
      Pin4 = 12
      Pin5 = 11
      Pin6 = 10
      Pin7 = 9
      Pin8 = 8
      PWM_Channel1 = 5
      PWM_Channel2 = 13
      PWM_Channel3 = 11
      SPI_MOSI = 10
      SPI_MISO = 9
      SPI_SCK =  8
      I2C_SDA =  9     'i2c
      I2C_SCL =  8     'i2c
      LED_Pin =  8
    END ENUM

    ENUM WB_Model1B   ' : unsigned char {
      Pin1 = 5          '1
      Pin2 = 4          '2
      Pin3 = 13         '3
      Pin4 = 12         '4
      Pin5 = 11         '5
      Pin6 = 10         '6
      Pin7 = 9          '7
      Pin8 = 8          '8
      PWM_Channe11 = 5    '1
      PWM_Channel2 = 13    '3
      PWM_Channel3 = 11    '5
      SPI_MOSI = 10       '6
      SPI_MISO = 9        '7
      SPI_SCK = 8         '8
      I2C_SDA = 9         '7
      I2C_SCL = 8         '8
      LED_Pin = 5         '1
      Switch_Pin = 5      '1
    END ENUM

    ENUM WB_Model2B    ' : unsigned char {
      InputA = 4       '1
      InputB = 5       '2'
      Output1 = 8      '3
      Output2 = 9       '4
      Output3 = 10      '5
      Output4 = 11     '6
      Output5 = 12     '7
      Output6 = 13     '8
      PWM_Channel1 = 13 '6   output 4
      PWM_Channel2 = 11  '8  output 6
    END ENUM


   ENUM PinDir
     DIR_IN  = 0
     DIR_OUT = 1
   END ENUM

       '  enum class WB_DIGITAL_PIN_VALUE : byte { Low = 0, High = 1, Floating = 2 };
   ENUM DP_Value
     Low = 0
     High =1
     float=2
   END ENUM

   '   enum class WB_SMPD_STATE : byte { OutputLow = 0b00, OutputHigh = 0b01, OutputFloat = 0b10, Input = 0b11 };
       ENUM WB_SMPD_STATE
            OutputLow   = &b00
            OutputHigh  = &b01
            OutputFloat = &b10
            InputSet = &b11
        END ENUM
'     enum class WB_PULL_DIR : byte { NONE = 0, UP = 1, DOWN = 2 };
  ENUM WB_PULL_DIR
    Non3 = 0        ' note None is a reserved word
    UP   = 1
    Down = 2
  END ENUM

'            enum class WB_POLARITY : byte { ActiveHigh = 0, ActiveLow = 1 };
  ENUM WB_POLARITY
    ActiveHigh = 0
    ActiveLow  = 1
  END ENUM
'            enum class WB_SPI_MODE : byte {
  ENUM WB_SPI_MODE
    MODE0 = 0         ',      // clock idle low; data sampled on rising edge and shifted out on the falling edge
    MODE1 = 1         ',      // clock idle low; data sampled on the falling edge and shifted out on the rising edge
    MODE2 = 2         ',      // clock idle high; data sampled on the falling edge and shifted out on the rising edge
    MODE3 = 3           ' // clock idle high; data sampled on rising edge and shifted out on the falling edge
  END ENUM

   ENUM WB_I2C_SPEED   'Dword : unsigned long {
       Speed100Kbit = 107142
       Speed400Kbit = 545454
    END ENUM

  ENUM  WB_I2C_SLEW
    Standard  = 1
    HighSpeed = 0
  END ENUM

  ENUM  WB_I2C_COMPAT
    I2C   = 0
    SMBus = 1
  END ENUM

  ENUM  WB_I2C_OP
    S3ND    = 0       'note send is a reserved word
    Receive = 1
  END ENUM

  '    enum class WB_I2C_STATUS { Success = 0, NACK = 1, Error = 2 };
  ENUM WB_I2C_STATUS
    Success  = 0
    NACK     = 1
    Err_or   = 2   'Err and Error are reserved
  END ENUM

  ENUM WB_CAPTURE_TIME_UNITS
      Micros = 0
      Millis = 1
      Seconds = 2
  END ENUM

  ENUM WB_ERRORTYPE  ': byte {
      NON3 = 0
      GENERAL_API = 1
      BUFFER_TOO_SMALL = 2
      INSUFFICIENT_BYTES_WRITTEN = 3
      READ_TIMEOUT = 4
      DEVICE_REMOVED = 5
      INVALID_OPERATION = 6
      COUNT_TOO_LARGE = 7
      NULL_POINTER = 8
      INVALID_ARGUMENTS = 10
      DEVICE_IN_USE = 11
      NOT_SUPPORTED = 12
      WRITE_TIMEOUT = 13
      GENERAL_USB = 14
      INSUFFICIENT_BYTES_RECEIVED = 15
      ACCESS_DENIED = 16
  END ENUM

'..............TYPES......................
 TYPE WB_DEVDESCRIPTOR      'struct WB_DEVDESCRIPTOR {
    WB_MODEL   AS BYTE       ' model;
    hwRevision AS BYTE       'byte hwRevision;
  END TYPE

  TYPE WB_DEVINFO                         'struct WB_DEVINFO {
    descriptor   AS WB_DEVDESCRIPTOR      '  see WB_DVDescriptor
    productName  AS WSTRINGZ * 128        'WCHAR productName[128];
    labelString  AS WSTRINGZ * 128        'WCHAR labelString[128];
    serialString AS WSTRINGZ * 128        'WCHAR serialString[128];
    fwRevision   AS BYTE
  END TYPE

  TYPE WB_ENUMDEVINFO         ' struct WB_ENUMDEVINFO {
    devinfo  AS WB_DEVINFO    ' WB_DEVINFO devInfo;
    USB_Path AS WSTRINGZ PTR  ' wchar_t* path;
    Inuse    AS BYTE          ' WB_BOOL
  END TYPE

  TYPE WB_ERROR           'struct WB_ERROR {   An INSTANCE OF THIS structure may be passed TO many OF the API
    WB_ERRORTYPE AS BYTE  'WB_ERRORTYPE type;  functions IN order TO receive information about errors that occur.
    extra1      AS DWORD  'DWORD extra1 = 0;   NOTE: It IS always permitted TO pass nullptr where a WB_ERROR*
    extra2      AS LONG   'long extra2 = 0;    functions IN order TO receive information about errors that occur.
  END TYPE
'9 bytes

'Note  "NulPTR = BYVAL %NULL
  TYPE WB_FEATURES
    WB_Passive_Features AS WORD
    WB_Active_Features  AS WORD
  END TYPE


  TYPE WB_PIN_DEFINITION
    Pin_num       AS BYTE         'WB_PIN_NUMBER is byte   danger of tghis being a word
    NameField     AS STRINGZ PTR  'points to char string
    Features      AS WB_FEATURES  '2 words as features
    MaxDecaVolts  AS WORD         'unsigned short
    MaxMilliAmps  AS WORD         'unsigned short
    ActiveLow     AS BYTE         ' bool is a byte maybe long possibly might be word
  END TYPE

  TYPE WB_PIN_STATUS         '
     Number         AS BYTE  'WB_PIN_NUMBER is byte
     CurrentFeature AS WORD  'WB_ACTIVE_FEATURES
     Direction      AS BYTE  'WB_PIN_DIRECTION '     Direction as WB_PIN_DIRECTION
     inputValue     AS BYTE  'WB_DIGITAL_PIN_Value
     OutputValue    AS BYTE  'WB_DIGITAL_PIN_Value
     Pull           AS BYTE  '
  END TYPE

  TYPE WB_DATETIME
    year   AS INTEGER
    month  AS BYTE
    day    AS BYTE
    hour   AS BYTE
    minute AS BYTE
  END TYPE

  TYPE WB_SMPD_ENTRY
         WB_PIN_NUMBER AS BYTE 'pin_number;
         WB_SMPD_STATE AS BYTE 'state;
  END TYPE
'................................ Functions ............................

    FUNCTION ModelWB(X AS BYTE) AS STRING  'returns a text string
      LOCAL T AS STRING
        SELECT CASE X
          CASE 1
           T = "Model 1A"
          CASE 2
           T = "Model 1B"
          CASE 3
           T = "Model 2A"
          CASE 4
           T = "Model 2B"
          CASE 5
           T = "Model 3"
          CASE 6
           T = "Model 4"
          CASE ELSE
           T = "Unknown"
        END SELECT
      FUNCTION = T
    END FUNCTION

    FUNCTION ACT_Feature(activ AS INTEGER)AS STRING
      LOCAL tmp AS STRING
        IF activ=0 THEN FUNCTION="None":EXIT FUNCTION
        IF BIT(activ,0)=1 THEN tmp=" ADC"
        IF BIT(activ,1)=1 THEN tmp=tmp+" DAC"
        IF BIT(activ,2)=1 THEN tmp=tmp+" PWM"
        IF BIT(activ,3)=1 THEN tmp=tmp+" CWG"
        IF BIT(activ,4)=1 THEN tmp=tmp+" I2C"
        IF BIT(activ,5)=1 THEN tmp=tmp+" SPI"
        IF BIT(activ,6)=1 THEN tmp=tmp+" UART"
        IF BIT(activ,7)=1 THEN tmp=tmp+" NCO"
      FUNCTION=TRIM$(tmp)
    END FUNCTION

    FUNCTION PAS_Feature(Passiv AS INTEGER)AS STRING    'returns text
      LOCAL tmp AS STRING
        IF BIT(Passiv,0)=1 THEN tmp=" InputOnly"
        IF BIT(Passiv,1)=1 THEN tmp=tmp+" OutputOnly"
        IF BIT(Passiv,2)=1 THEN tmp=tmp+" Bidirectional"
        IF BIT(Passiv,3)=1 THEN tmp=tmp+" Isolation"
        IF BIT(Passiv,4)=1 THEN tmp=tmp+" FixedPull"
        IF BIT(Passiv,5)=1 THEN tmp=tmp+" OptionalPull"
        IF BIT(Passiv,6)=1 THEN tmp=tmp+" OVProtection"
        IF BIT(Passiv,7)=1 THEN tmp=tmp+" OCProtection"
        IF BIT(Passiv,8)=1 THEN tmp=tmp+" Indicator"
        IF BIT(Passiv,9)=1 THEN tmp=tmp+" OpenDrain"
'         add more as  supplied
     FUNCTION= TRIM$(tmp)
   END FUNCTION

   FUNCTION wb_ERRORTYPE(MyError AS BYTE)  AS STRING
     LOCAL tmp AS STRING
         ' returns readable errors
       SELECT  CASE MyError
         CASE 0
           tmp = "0 none"
         CASE 1
           tmp = "1 GENERAL_API"
         CASE 2
           tmp = "2 BUFFER_TOO_SMALL"
         CASE 3
           tmp = "3 INSUFFICIENT_BYTES_WRITTEN"
         CASE 4
           tmp = "4 READ_TIMEOUT"
         CASE 5
           tmp = "5 DEVICE_REMOVED"
         CASE 6
           tmp = "6 INVALID_OPERATION"
        CASE 7
           tmp = "7 COUNT_TOO_LARGE2"
        CASE 8
           tmp = "8 NULL_POINTER"
        CASE 9
           tmp = "9 I2C_NACK"
        CASE 10
           tmp = "10 INVALID_ARGUMENTS"
        CASE 11
           tmp = "11 DEVICE_IN_USE"
        CASE 12
           tmp = "12 NOT_SUPPORTED"+ STR$(MyError)
        CASE 13
           tmp = "13 WRITE_TIMEOUT"
        CASE 14
          tmp = "14 GENERAL_USB"
      CASE 15
          tmp = "15 INSUFFICIENT_BYTES_RECEIVED"
      CASE 16
          tmp = "16 ACCESS_DENIED"
      END SELECT
    FUNCTION = tmp
  END FUNCTION

  FUNCTION WB_ERRORCAUSE ( X AS LONG) AS STRING
    LOCAL tmp AS STRING
      SELECT CASE X
         CASE 0
           tmp = "UNSPECIFIED"
         CASE 1
           tmp = "SET_PIN_DIRECTION"
         CASE 2
           tmp = "SET_PIN_PULL"
         CASE 3
           tmp = "QUERY_PIN"
         CASE 4
           tmp = "READ_PIN"
         CASE 5
           tmp = "WRITE_PIN"
         CASE 6
           tmp = "PIN_IS_FLOATING"
         CASE 7
           tmp = "PEEK"
         CASE 8
           tmp = "POKE = 8"
         CASE 9
           tmp = "PWM = 9"
       END SELECT
     FUNCTION = tmp
  END FUNCTION


'....................Wirebridge.API.CDLL-x86.dll functions ..24/02/06.................
''''''''''''''''''''''''''''''''''Model Functions '''''''''''''''''''''''''''''''''''
 '                  DLL byte WB_GetNumberOfPins(WB_DEVINFO);
        DECLARE FUNCTION WB_GetNumberOfPins LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetNumberOfPins" _
                     (BYVAL wb_devinfo ) AS BYTE
 '  Byte_result = WB_GetNumberOfPins ( WB_devinfo )

'                DLL WB_FEATURES WB_GetFeatures(WB_DEVINFO);
       DECLARE FUNCTION WB_GetFeatures LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetFeatures" _
                     (BYVAL  wb_devinfo ) AS DWORD
'  result = WB_GetFeatures ( WB_devinfo) 'returns wb_features as as a dword/ lo and hi word are the PASive and Active

'                DLL void WB_GetPinDefinitions(WB_DEVINFO, WB_PIN_DEFINITION *buffer);
 DECLARE FUNCTION WB_GetPinDefinitions LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetPinDefinitions" _
                             (BYVAL WB_DEVINFO   , _
                              BYVAL Buff_Ptr AS wb_pin_definition PTR) AS LONG
'  result = N WB_GetPinDefinitions( devinfo , Buff_Ptr )

'            DLL unsigned long WB_GetBauds(WB_DEVINFO, unsigned short index);
 DECLARE FUNCTION WB_GetBauds LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetBauds" _
                             (BYVAL WB_DEVINFO   , _
                              BYVAL Index AS WORD ) AS DWORD
'    Dword_result= WB_GetBauds( WB_DEVINFO ,Index )

'''''''''''''''''''''''''// Device functions '''''''''''''''''''''''''''''
'            DLL WB_DEVENUMERATION WB_EnumerateDevicesBegin(WB_ERROR*);
  DECLARE FUNCTION WB_EnumerateDevicesBegin LIB "WireBridge.API.C32.dll" ALIAS "WB_EnumerateDevicesBegin" _
                  ( BYVAL WBERR AS WB_ERROR PTR) AS DWORD    ' = WB_DEVENUMERATION get WB_DEVENUMERATION Here.
' Result =  WB_EnumerateDevicesBegin(WBERR)

          '
          '  DLL WB_OUTCOME WB_EnumerateDevicesNext(WB_DEVENUMERATION, WB_ENUMDEVINFO*, WB_ERROR*);
 DECLARE FUNCTION WB_EnumerateDevicesNext  LIB "WireBridge.API.C32.dll" ALIAS "WB_EnumerateDevicesNext" _
                 ( BYVAL WB_DEVENUMERATION AS DWORD, _
                  BYVAL DEVINFO AS WB_ENUMDEVINFO PTR, _   'store the path from here
                  BYVAL WBERR AS WB_ERROR PTR)  AS BYTE
'    wb_outcome =  WB_EnumerateDevicesNext( DevHandle, DEVINFO,WBERR)

 '               DLL void WB_EnumerateDevicesEnd(WB_DEVENUMERATION);
     DECLARE FUNCTION WB_EnumerateDevicesEnd LIB "WireBridge.API.C32.dll" ALIAS "WB_EnumerateDevicesEnd" _
                 ( BYVAL WB_DEVENUMERATION AS DWORD) AS DWORD
' Result = WB_EnumerateDevicesEnd( DevHandle)        ' do call this when finished enumeration to prevent memory leaks

'               DLL void WB_GetDeviceInfo(WB_DEVHANDLE, WB_DEVINFO *);
 DECLARE FUNCTION WB_GetDeviceInfo  LIB "WireBridge.API.C32.dll" ALIAS "WB_GetDeviceInfo" _
                 ( BYVAL DevHandle AS DWORD, _
                  BYVAL devinfo AS wb_devinfo PTR)AS LONG
' Result =  WB_GetDeviceInfo(DevHandle,devinfo)

'                DLL const wchar_t *WB_GetSerialNumber(WB_DEVHANDLE);
 DECLARE FUNCTION  WB_GetSerialNumber LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetSerialNumber" _
                  (BYVAL WB_DEVHANDLE AS DWORD) AS LONG    ' this is a pointer need to test
' Result =  WB_GetSerialNumber (WB_DEVHANDLE)     '      returns serial no as long

'                DLL byte WB_GetFirmwareRevision(WB_DEVHANDLE, WB_ERROR *);
 DECLARE FUNCTION WB_GetFirmwareRevision LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetFirmwareRevision" _
                  (BYVAL WB_DEVHANDLE AS DWORD, _
                  BYVAL WBERR AS WB_ERROR PTR) AS BYTE
 '  Byte_Result =  WB_GetFirmwareRevision (WB_DEVHANDLE , WBERR)  'returns model

'            DLL WB_BOOL WB_IsWiped(WB_DEVINFO);
 DECLARE FUNCTION WB_IsWiped LIB  "WireBridge.API.C32.dll" ALIAS "WB_IsWiped" _
                 (BYVAL wb_devinfo)AS BYTE
' Byte_Result =  WB_IsWiped(DevDesc )           'returns %True / %False, hopefully never need to use.

'                DLL WB_BOOL WB_IsInitialised(WB_DEVHANDLE);
 DECLARE FUNCTION WB_IsInitialised LIB  "WireBridge.API.C32.dll" ALIAS "WB_IsInitialised" _
                  (BYVAL WB_DEVHANDLE AS DWORD)AS BYTE
'  Byte_Result =  WB_IsInitialised(WB_DEVHANDLE )   'Should never need this

   '                 DLL WB_OUTCOME WB_Reset(WB_DEVHANDLE, WB_ERROR*);
 DECLARE FUNCTION WB_Reset LIB  "WireBridge.API.C32.dll" ALIAS "WB_Reset" _
                  ( BYVAL WB_DevHandle AS DWORD, _
                  BYVAL WBERR AS WB_ERROR PTR) AS BYTE
'    wb_outcome = WB_Reset(WB_DevHandle,WBERR)      ' its a reset back to as delivered.


 '   DLL WB_OUTCOME WB_Clear(WB_DEVHANDLE, WB_ERROR*);
               DECLARE FUNCTION WB_Clear LIB  "WireBridge.API.C32.dll" ALIAS "WB_Clear" _
                     ( BYVAL WB_DevHandle AS DWORD, _
                      BYVAL WBERR AS WB_ERROR PTR) AS BYTE
'   wb_outcome = WB_Clear(WB_DevHandle,WBerr)

'                DLL const wchar_t *WB_GetLabel(WB_DEVHANDLE);
 DECLARE FUNCTION WB_GetLabel LIB  "WireBridge.API.C32.dll" ALIAS "WB_GetLabel" _
                    ( BYVAL WB_DevHandle AS DWORD) AS LONG 'note this is a pointer
  '  result = WB_GetLabel(WB_DevHandle)   'returns a pointer to the label (max 30 characters )

'             DLL WB_OUTCOME WB_SetLabel(WB_DEVHANDLE, const wchar_t *label, WB_ERROR*);
 DECLARE FUNCTION WB_SetLabel LIB  "WireBridge.API.C32.dll" ALIAS "WB_SetLabel" _
                    ( BYVAL WB_DevHandle AS DWORD, _
                    BYVAL LabelPtr AS WSTRINGZ PTR, _
                    BYVAL WBERR AS WB_ERROR PTR)   AS BYTE
 '      wb_outcome = WB_SetLabel(WB_DevHandle,labelPtr,WBERR)   'note the 30 char max

'                DLL WB_DEVHANDLE WB_OpenDevice(const wchar_t *path, WB_ERROR *);
 DECLARE FUNCTION WB_OpenDevice LIB  "WireBridge.API.C32.dll" ALIAS "WB_OpenDevice" _
                 (BYVAL Mypath AS DWORD, _               ,this is  the path from WB_EnumerateDevicesNext
                 BYVAL WBERR AS WB_ERROR PTR) AS LONG    ' this is the Device Handle
'  WB_DEVHNDL = WB_OpenDevice(Mypath, WBERR )    ' Return the devHandle  a Very useful function

'                DLL WB_BOOL WB_CloseDevice(WB_DEVHANDLE);
 '            DLL WB_OUTCOME WB_CloseDevice(WB_DEVHANDLE);
 DECLARE FUNCTION WB_CloseDevice LIB  "WireBridge.API.C32.dll" ALIAS "WB_CloseDevice" _
                 ( BYVAL WB_DevHandle AS DWORD) AS BYTE
'  Byte_Result = WB_CloseDevice( WB_DevHandle)  'always a good idea to close devices when done

'                DLL WB_BOOL WB_IsPinDrafted(WB_DEVHANDLE, WB_PIN_NUMBER, WB_ERROR *);
  DECLARE FUNCTION WB_IsPinDrafted LIB  "WireBridge.API.C32.dll" ALIAS "WB_IsPinDrafted" _
                     (BYVAL WB_DevHandle AS DWORD, _
                     BYVAL WB_PIN_NUMBER AS BYTE, _
                     BYVAL WBERR AS WB_ERROR PTR) AS BYTE
' Byte_result =  WB_IsPinDrafted(WB_DevHandle,WB_PIN_NUMBER,wberr )


''''''''''''''''''''''''' // Pin functions'''''''''''''''''''''''''''

'                DLL WB_DIGITAL_PIN_VALUE WB_ReadPinDigital(WB_DEVHANDLE, WB_PIN_NUMBER, WB_ERROR *);
  DECLARE FUNCTION WB_ReadPinDigital LIB  "WireBridge.API.C32.dll" ALIAS "WB_ReadPinDigital" _
                     (BYVAL WB_Devhandle AS DWORD, _
                     BYVAL WB_PIN_NUMBER AS BYTE, _
                     BYVAL  WBERR AS WB_ERROR PTR)AS LONG
'  result = WB_ReadPinDigital(WB_Devhandle,WB_PIN_NUMBER, WBERR)

'                DLL wb_outcome WB_WritePinDigital(WB_DEVHANDLE, WB_PIN_NUMBER, WB_DIGITAL_PIN_VALUE, WB_ERROR *);
 DECLARE FUNCTION WB_WritePinDigital LIB  "WireBridge.API.C32.dll" ALIAS "WB_WritePinDigital" _
                     (BYVAL WB_Devhandle AS DWORD, _
                      BYVAL WB_PIN_NUMBER AS BYTE, _
                      BYVAL WB_DIGITAL_PIN_VALUE AS WORD,_
                      BYVAL  WBERR AS WB_ERROR PTR )AS BYTE
'   wb_outcome = WB_WritePinDigital(WB_Devhandle ,WB_PIN_NUMBER,WB_DIGITAL_PIN_VALUE, WBERR)

 '                DLL wb_outcome WB_SetPinPull(WB_DEVHANDLE, WB_PIN_NUMBER, bool, WB_ERROR *);
 DECLARE FUNCTION WB_SetPinPull LIB  "WireBridge.API.C32.dll" ALIAS "WB_SetPinPull" _
                      (BYVAL WB_Devhandle AS DWORD, _
                      BYVAL WB_PIN_NUMBER AS BYTE, _
                      BYVAL PAD AS BYTE,_
                      BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
'    wb_outcome= WB_SetPinPull(WB_Devhandle,pn,PAD,WBERR)

 '                DLL WB_BOOL WB_SetPinDirection(WB_DEVHANDLE, WB_PIN_NUMBER, WB_PIN_DIR, WB_ERROR *);
  DECLARE FUNCTION WB_SetPinDirection LIB  "WireBridge.API.C32.dll" ALIAS "WB_SetPinDirection" _
                      (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL WB_PIN_NUMBER AS BYTE, _
                       BYVAL WB_PIN_DIR AS BYTE,_
                      BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
'  wb_outcome = WB_SetPinDirection ( WB_Devhandle, WB_PIN_NUMBER, WB_PIN_DIR,WBERR)

   '  DLL WB_OUTCOME WB_SetMultiplePinsDigital(WB_DEVHANDLE, WB_SMPD_ENTRY*, int count, WB_ERROR*);
     DECLARE FUNCTION WB_SetMultiplePinsDigital LIB "WireBridge.API.C32.dll" ALIAS "WB_SetMultiplePinsDigital" _
                       (BYVAL WB_Devhandle AS DWORD, _
                        BYVAL WB_SMPD_ENTRY AS DWORD, _    'pointer to array of WPSMPD
                        BYVAL PCOUNT AS DWORD, _
                        BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
    '   wb_outcome =  WB_SetMultiplePinsDigital(WB_Devhandle,WB_SMPD_ENTRY,PCOUNT,WBERR)

      '    DLL wb_outcome WB_QueryPins(WB_DEVHANDLE hDevice, int count, byte* pinNumbers, WB_PIN_STATUS* buffer, WB_ERROR*);
  DECLARE FUNCTION WB_QueryPins LIB  "WireBridge.API.C32.dll" ALIAS "WB_QueryPins" _
                      (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL pCount AS DWORD, _
                       BYVAL Pin_NUMBERS AS BYTE PTR,_
                       BYVAL Buffer AS WB_PIN_STATUS PTR, _
                      BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
'     wb_outcome = WB_QueryPins (WB_Devhandle, PCount, pnum_ptr,qp_ptr,WBERR)

 '              DLL wb_outcome WB_QueryAllPins(WB_DEVHANDLE, WB_PIN_STATUS *buffer, WB_ERROR *);
       DECLARE FUNCTION WB_QueryAllPins LIB  "WireBridge.API.C32.dll" ALIAS "WB_QueryAllPins" _
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL Buffer AS WB_PIN_STATUS PTR, _
                      BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
'    wb_outcome = WB_QueryAllPins (GWB_Devhndl, QP_Ptr,WBERR

 ''''''''''''''''''''''// PWM functions-----------------------------
'                DLL wb_outcome WB_PWM_Init(WB_DEVHANDLE hDevice, byte period, byte speed, WB_ERROR* err);
  DECLARE FUNCTION WB_PWM_Init LIB  "WireBridge.API.C32.dll" ALIAS "WB_PWM_Init"_
                      (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL Period AS BYTE, _
                       BYVAL Xspeed AS BYTE, _          'Speed is reserved word
                      BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
'   wb_outcomet= WB_PWM_Init (GWB_Devhndl,Period,Xspeed,WBERR )

'                DLL wb_outcome WB_PWM_Start(WB_DEVHANDLE hDevice, byte channel, byte duty, WB_POLARITY polarity, WB_ERROR* err);
   DECLARE FUNCTION WB_PWM_Start LIB  "WireBridge.API.C32.dll" ALIAS "WB_PWM_Start"_
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL channel AS BYTE,_
                       BYVAL Duty AS BYTE, _
                       BYVAL Polarity AS BYTE , _
                      BYVAL  WBERR AS WB_ERROR PTR)AS BYTE
' wb_outcome = WB_PWM_Start(WB_Devhandle,channel,Duty ,Polarity ,WBERR)

'                DLL wb_outcome WB_PWM_Stop(WB_DEVHANDLE hDevice, byte channel, WB_ERROR* err);
  DECLARE FUNCTION WB_PWM_Stop LIB  "WireBridge.API.C32.dll" ALIAS "WB_PWM_Stop"_
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL channel AS BYTE,_           '1 to x depends on wirebridge
                       BYVAL WBERR AS WB_ERROR PTR)AS BYTE
'  wb_outcome = WB_PWM_Stop(GWB_Devhndl, channel,WBERR )

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'                // SPI functions

'                DLL wb_outcome  WB_SPI_Init(WB_DEVHANDLE hDevice, WB_SPI_MODE mode, byte timing, WB_ERROR* err);
  DECLARE FUNCTION WB_SPI_Init LIB  "WireBridge.API.C32.dll" ALIAS "WB_SPI_Init"_
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL Modes AS BYTE,_                   ' note mode is reserved modes 0-3
                       BYVAL timing AS LONG, _
                       BYVAL WBERR AS WB_ERROR PTR)AS BYTE
' wb_outcome  =WB_SPI_Init(WB_Devhandle ,Modes,timing,WBERR )

'                DLL wb_outcome  WB_SPI_Transfer(WB_DEVHANDLE hDevice, byte count, byte sendBuffer[], byte receiveBuffer[], WB_ERROR* err);
 DECLARE FUNCTION WB_SPI_Transfer LIB  "WireBridge.API.C32.dll" ALIAS "WB_SPI_Transfer"_
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL zCount AS BYTE,_                   'note count is a reserved word
                       BYVAL Sendbuffer AS STRING PTR, _           'need to check out sendBuffer[]
                       BYVAL recievebuffer AS STRING PTR, _
                       BYVAL WBERR AS WB_ERROR PTR)AS BYTE
'  wb_outcome  = WB_SPI_Transfer(WB_Devhandle, zCount,Sendbuffer_ptr,recievebuffer_ptr ,WBERR)

'                DLL wb_outcome  WB_SPI_Release(WB_DEVHANDLE hDevice, WB_ERROR* err);
 DECLARE FUNCTION WB_SPI_Release LIB  "WireBridge.API.C32.dll" ALIAS "WB_SPI_Release"_
                        (BYVAL WB_Devhandle AS DWORD, _
                        BYVAL WBERR AS WB_ERROR PTR)AS BYTE
'  wb_outcome  =  WB_SPI_Release(WB_Devhandle,WBERR)

'''''''''''''''''''''''''''''''''' // I2C functions '''''''''''''''''''''''''''''''''''''''''''

      '          DLL WB_OUTCOME WB_I2C_End(WB_DEVHANDLE, WB_ERROR*);
              ' NOTE Begin ... data ...data   End  is fine as is Begin  data begin .. data end.

    '          DLL WB_OUTCOME WB_I2C_Init(WB_DEVHANDLE, WB_I2C_SPEED SPEED, WB_ERROR*);
    '           DLL WB_OUTCOME WB_I2C_Init(WB_DEVHANDLE, unsigned LONG BAUD, WB_I2C_SLEW, WB_I2C_COMPAT, WB_ERROR*);
      DECLARE FUNCTION WB_I2C_Init LIB  "WireBridge.API.C32.dll" ALIAS "WB_I2C_Init"_
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL baudX AS DWORD ,_      '      WB_IC2_Speed.Speed100Kbit = 107142, wb_IC2_Speed.Speed400Kbit = 545454
                       BYVAL slew AS BYTE , _        '    WB_I2C_SLEW  Standard = 1, HighSpeed = 0
                       BYVAL Compat AS BYTE , _      '   WB_I2C_COMPAT  I2C = 0, SMBus = 1
                       BYVAL WBERR AS WB_ERROR PTR)AS BYTE
'  wb_outcome = WB_I2C_Init(WB_Devhandle,baudx,slew,compat,WBERR)

'          DLL WB_I2C_STATUS WB_I2C_Transfer(WB_DEVHANDLE, byte address, WB_I2C_OP, byte count, byte buffer[], WB_ERROR*);
   DECLARE FUNCTION WB_I2C_Transfer LIB  "WireBridge.API.C32.dll" ALIAS "WB_I2C_Transfer"_
                       (BYVAL WB_Devhandle AS DWORD, _
                       BYVAL address AS BYTE ,_
                       BYVAL Operation AS  BYTE, _           ' SEND = 0  Receive = 1       careful as send is reserved
                       BYVAL zCount  AS BYTE, _                'count is reserved
                       BYVAL buffer AS STRING PTR, _              '  Standard = 1 HighSpeed = 0
                       BYVAL WBERR AS WB_ERROR PTR)AS LONG     'returns status
   '  result =  WB_I2C_Transfer(WB_Devhandle,address,Operation,zCount,buffer,WBERR)
'   result =     Success  = 0,    NACK     = 1 ,  Err_or   = 2   'Err and Error are reserved

  '                DLL WB_BOOL WB_I2C_Release(WB_DEVHANDLE, WB_ERROR*);
  DECLARE FUNCTION WB_I2C_Release LIB  "WireBridge.API.C32.dll" ALIAS "WB_I2C_Release"_
                       (BYVAL WB_Devhandle AS DWORD, _
                        BYVAL WBERR AS WB_ERROR PTR)AS BYTE
' WB_OUTCOME = WB_I2C_Release(WB_Devhandle,WBERR)

'             DLL WB_I2C_STATUS WB_I2C_Begin(WB_DEVHANDLE, byte address, WB_I2C_OP, WB_ERROR*);
    DECLARE FUNCTION WB_I2C_Begin LIB  "WireBridge.API.C32.dll" ALIAS "WB_I2C_Begin"_
                       (BYVAL WB_Devhandle AS DWORD, _
                         BYVAL address AS BYTE ,_
                         BYVAL  Operation AS  BYTE, _           ' SEND = 0  Receive = 1       careful as send is reserved
                         BYVAL WBERR AS WB_ERROR PTR)AS BYTE
  '  Byte_RESULT = WB_I2C_Begin(WB_Devhandle,address,Operation,WBERR )
               ' DLL WB_I2C_STATUS WB_I2C_Data(WB_DEVHANDLE, WB_I2C_OP, unsigned short count, byte buffer[], WB_ERROR*);
'                DLL WB_I2C_STATUS WB_I2C_Data(WB_DEVHANDLE, WB_I2C_OP, unsigned short count, byte buffer[], WB_ERROR*);
     DECLARE FUNCTION  WB_I2C_Data LIB  "WireBridge.API.C32.dll" ALIAS "WB_I2C_Data" _
                       (BYVAL WB_Devhandle AS DWORD, _
                         BYVAL Operation AS BYTE ,_   ' SEND = 0  Receive = 1       careful as send is reserved
                         BYVAL zcount AS  WORD, _           '
                         BYVAL buffer AS STRING PTR ,_
                         BYVAL WBERR AS WB_ERROR PTR)AS BYTE
'   byte_result =  WB_I2C_Data(WB_Devhandle, Operation, zcount,buffer, WBERR)

'   DLL WB_OUTCOME WB_I2C_End(WB_DEVHANDLE, WB_ERROR*);
     DECLARE FUNCTION  WB_I2C_End LIB  "WireBridge.API.C32.dll" ALIAS "WB_I2C_End"_
                       (BYVAL WB_Devhandle AS DWORD, _
                        BYVAL WBERR AS WB_ERROR PTR)AS BYTE
  '    WB_OUTCOME = WB_I2C_End( WB_Devhandle,wberr)
''''''''''''''''''''''''''' // Utility functions''''''''''''''''''''''''''''

'                DLL WB_OUTCOME WB_BeginUSBHotplugDetection(USBHotplugEventCallback, WB_ERROR*);
'                DLL void WB_EndUSBHotplugDetection();
'                DLL WB_BOOL WB_PollUSBHotplugDetection();
               ' DLL WB_DATETIME WB_BuildTimestamp();

              '  WB_DATETIME WB_BuildTimestamp(WC_Datetime *);    WORKS
  DECLARE FUNCTION  WB_BuildTimestamp  LIB "WireBridge.API.C32.dll" ALIAS "WB_BuildTimestamp" _
                 (BYVAL DT AS wb_datetime PTR) AS LONG
'  Result =  WB_BuildTimestamp ( DT )

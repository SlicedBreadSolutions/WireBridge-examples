%ILI9341_TFTWIDTH  = 240     ' ILI9341 max TFT width
%ILI9341_TFTHEIGHT = 320     ' ILI9341 max TFT height

%ILI9341_NOP       = &H00    ' No-op register
%ILI9341_SWRESET   = &H01    ' Software reset register
%ILI9341_RDDID     = &H04    ' Read display identification information
%ILI9341_RDDST     = &H09    ' Read Display Status

%ILI9341_SLPIN       = &H10    ' Enter Sleep Mode
%ILI9341_SLPOUT      = &H11    ' Sleep Out
%ILI9341_PTLON       = &H12    ' Partial Mode ON
%ILI9341_NORON       = &H13    ' Normal Display Mode ON

%ILI9341_RDMODE      = &H0A    ' Read Display Power Mode
%ILI9341_RDMADCTL    = &H0B    ' Read Display MADCTL
%ILI9341_RDPIXFMT    = &H0C    ' Read Display Pixel Format
%ILI9341_RDIMGFMT    = &H0D    ' Read Display Image Format
%ILI9341_RDSELFDIAG  = &H0F    ' Read Display Self-Diagnostic Result

%ILI9341_INVOFF      = &H20    ' Display Inversion OFF
%ILI9341_INVON       = &H21    ' Display Inversion ON
%ILI9341_GAMMASET    = &H26    ' Gamma Set
%ILI9341_DISPOFF     = &H28    ' Display OFF
%ILI9341_DISPON      = &H29    ' Display ON

%ILI9341_CASET       = &H2A    ' Column Address Set
%ILI9341_PASET       = &H2B    ' Page Address Set
%ILI9341_RAMWR       = &H2C    ' Memory Write
%ILI9341_RAMRD       = &H2E    ' Memory Read

%ILI9341_PTLAR       = &H30    ' Partial Area
%ILI9341_MADCTL      = &H36    ' Memory Access Control
%ILI9341_VSCRSADD    = &H37    ' Vertical Scrolling Start Address
%ILI9341_PIXFMT      = &H3A    ' COLMOD: Pixel Format Set

%ILI9341_FRMCTR1     = &HB1    ' Frame Rate Control (In Normal Mode/Full Colors)
%ILI9341_FRMCTR2     = &HB2    ' Frame Rate Control (In Idle Mode/8 colors)
%ILI9341_FRMCTR3     = &HB3    ' Frame Rate control (In Partial Mode/Full Colors)
%ILI9341_INVCTR      = &HB4    ' Display Inversion Control
%ILI9341_DFUNCTR     = &HB6    ' Display Function Control

%ILI9341_PWCTR1      = &HC0    ' Power Control 1
%ILI9341_PWCTR2      = &HC1    ' Power Control 2
%ILI9341_PWCTR3      = &HC2    ' Power Control 3
%ILI9341_PWCTR4      = &HC3    ' Power Control 4
%ILI9341_PWCTR5      = &HC4    ' Power Control 5
%ILI9341_VMCTR1      = &HC5    ' VCOM Control 1
%ILI9341_VMCTR2      = &HC7    ' VCOM Control 2

%ILI9341_RDID1       = &HDA    ' Read ID 1
%ILI9341_RDID2       = &HDB    ' Read ID 2
%ILI9341_RDID3       = &HDC    ' Read ID 3
%ILI9341_RDID4       = &HDD    ' Read ID 4

%ILI9341_GMCTRP1     = &HE0    ' Positive Gamma Correction
%ILI9341_GMCTRN1     = &HE1    ' Negative Gamma Correction
'//%ILI9341_PWCTR6      = &HFC

'// COLOR definitions
%ILI9341_BLACK        = &H0000 '   0,   0,   0
%ILI9341_NAVY         = &H000F '   0,   0, 123
%ILI9341_DARKGREEN    = &H03E0 '   0, 125,   0
%ILI9341_DARKCYAN     = &H03EF '   0, 125, 123
%ILI9341_MAROON       = &H7800 ' 123,   0,   0
%ILI9341_PURPLE       = &H780F ' 123,   0, 123
%ILI9341_OLIVE        = &H7BE0 ' 123, 125,   0
%ILI9341_LIGHTGREY    = &HC618 ' 198, 195, 198
%ILI9341_DARKGREY     = &H7BEF ' 123, 125, 123
%ILI9341_BLUE         = &H001F '   0,   0, 255
%ILI9341_GREEN        = &H07E0 '   0, 255,   0
%ILI9341_CYAN         = &H07FF '   0, 255, 255
%ILI9341_RED          = &HF800 ' 255,   0,   0
%ILI9341_MAGENTA      = &HF81F ' 255,   0, 255
%ILI9341_YELLOW       = &HFFE0 ' 255, 255,   0
%ILI9341_WHITE        = &HFFFF ' 255, 255, 255
%ILI9341_ORANGE       = &HFD20 ' 255, 165,   0
%ILI9341_GREENYELLOW  = &HAFE5 ' 173, 255,  41
%ILI9341_PINK         = &HFC18 ' 255, 130, 198

%MADCTL_MY   = &H80 ' Bottom to top
%MADCTL_MX   = &H40 ' Right to left
%MADCTL_MV   = &H20 ' Reverse Mode
%MADCTL_ML   = &H10 ' LCD refresh Bottom to top
%MADCTL_RGB  = &H00 ' Red-Green-Blue pixel order
%MADCTL_BGR  = &H08 ' Blue-Green-Red pixel order
%MADCTL_MH   = &H04 ' LCD refresh right to left
' THESE ARE CMD / BYTE COUNT/BYTES/ CMD.....
$INITCMD = CHR$(  &HEF, 3, &H03, &H80, &H02, &HCF, 3, &H00, &HC1, &H30, _
                  &HED, 4, &H64, &H03, &H12, &H81, &HE8, 3, &H85, &H00, &H78,  _
                  &HCB, 5, &H39, &H2C, &H00, &H34, &H02, &HF7, 1, &H20, &HEA, 2, &H00, &H00, _
                  %ILI9341_PWCTR1  , 1, &H23, _      // Power control VRH[5:0]
                  %ILI9341_PWCTR2  , 1, &H10, _            '// Power control SAP[2:0];BT[3:0]
                  %ILI9341_VMCTR1  , 2, &H3e, &H28,_       '// VCM control
                  %ILI9341_VMCTR2  , 1, &H86, _            '// VCM control2
                  %ILI9341_MADCTL  , 1, &H48, _            '// Memory Access Control
                  %ILI9341_VSCRSADD, 1, &H00, _             '// Vertical scroll zero
                  %ILI9341_PIXFMT  , 1, &H55, _
                  %ILI9341_FRMCTR1 , 2, &H00, &H18,_
                  %ILI9341_DFUNCTR , 3, &H08, &H82, &H27,_ ' // Display Function Control
                  &HF2, 1, &H00, _                        '// 3Gamma Function Disable
                  %ILI9341_GAMMASET , 1, &H01,_             '// Gamma curve selected
                  %ILI9341_GMCTRP1 , 15, &H0F, &H31, &H2B, &H0C, &H0E, &H08, _ '// Set Gamma
                  &H4E, &HF1, &H37, &H07, &H10, &H03, &H0E, &H09, &H00, _
                  %ILI9341_GMCTRN1 , 15, &H00, &H0E, &H14, &H03, &H11, &H07, _ '// Set Gamma
                  &H31, &HC1, &H48, &H08, &H0F, &H0C, &H31, &H36, &H0F, _
                  %ILI9341_SLPOUT  , &H80,  _              '// Exit Sleep
                  %ILI9341_DISPON  , &H80,  _              '// Display on
                  &H00  )                                 '// End of list

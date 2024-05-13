'types for  bitmaps


TYPE IFH
    head AS WORD
    Chk AS WORD
    IFD1 AS DWORD
END TYPE

TYPE IFDEntry
    tag AS WORD
    fieldTyp AS WORD
    Cnt AS DWORD
    valoff AS DWORD
END TYPE

TYPE TifImg
    adr AS DWORD
    bytes AS DWORD
END TYPE

TYPE JFIFHEAD
  SOI AS WORD
  APP0 AS WORD
  Length AS WORD
  Identifier AS STRING *5
  Version AS WORD
  UNIT AS BYTE
  Xdensity  AS WORD
  Ydensity  AS WORD
  XThumbnail AS BYTE
  YThumbnail AS BYTE
END TYPE
 TYPE EXIFhead
      SOI AS WORD
      APP0 AS WORD
      Length AS WORD
      Identifier AS STRING *6
      LB AS STRING*2   'Alignment MM/II
     ignore AS WORD
      st AS DWORD '(00 00 00 8) start
      dEntry AS WORD
 END TYPE

TYPE Exif
    tag AS WORD
    DF AS WORD
    d1 AS DWORD
    d2 AS DWORD
END TYPE
TYPE CMYKT
    C AS BYTE
    M AS BYTE
    Y AS BYTE
    K AS BYTE
    T AS BYTE
END TYPE
TYPE CMYT
    C AS BYTE
    M AS BYTE
    Y AS BYTE
    T AS BYTE
END TYPE
TYPE RGBT
    R AS BYTE
    G AS BYTE
    B AS BYTE
    T AS BYTE
END TYPE
TYPE BGRT
    B AS BYTE
    G AS BYTE
    R AS BYTE
    T AS BYTE
END TYPE
TYPE b3
    R AS BYTE
    G AS BYTE
    B AS BYTE
END TYPE

TYPE tifinfo
    unit AS STRING*2
    xpixel AS DWORD
    ypixel AS DWORD
    xwidth AS SINGLE
    ywidth AS SINGLE
    xres AS SINGLE
    yres AS SINGLE
    orig_res AS SINGLE
    Pmetric AS BYTE
END TYPE
'ilu type

TYPE ILinfo
    Id  AS INTEGER         '// the image's id
    PData AS LONG          '// the image's data
    Width AS INTEGER      '// the image's width
    Height AS INTEGER     '// the image's height
    Depth AS INTEGER       '// the image's depth
    Bpp AS BYTE            '// bytes per pixel (not bits) of the image
    SizeOfData AS INTEGER  '// the total size of the data (in bytes)
    Format  AS DWORD       '// image format (in IL enum style)
    Type  AS DWORD         '// image type (in IL enum style)
    Origin  AS DWORD       '// origin of the image
    Ppallette  AS LONG     '// the image's palette
    PalType  AS DWORD      '// palette type
    PalSize AS INTEGER     '// palette size
    CubeFlags  AS DWORD    '// flags for what cube map sides are present
    NumNext AS INTEGER      '// number of images following
    NumMips AS INTEGER      '// number of mipmaps
    NumLayers AS INTEGER    '// number of layers
END TYPE

TYPE ILpointf
    x AS DOUBLE
    y AS DOUBLE
END TYPE

TYPE ILpointi
    x AS INTEGER
    Y AS INTEGER
END TYPE




'''''''''''''''''''''''
%IL_FALSE     =       0
%IL_TRUE      =       1

'  Matches OpenGL's right now.
'! Data formats \link Formats Formats\endlink
%IL_COLOUR_INDEX     = &H1900
%IL_COLOR_INDEX      = &H1900
%IL_ALPHA            = &H1906
%IL_RGB              = &H1907
%IL_RGBA             = &H1908
%IL_BGR              = &H80E0
%IL_BGRA             = &H80E1
%IL_LUMINANCE        = &H1909
%IL_LUMINANCE_ALPHA  = &H190A

'! Data types \link Types Types\endlink
%IL_BYTE           = &H1400
%IL_UNSIGNED_BYTE  = &H1401
%IL_SHORT          = &H1402
%IL_UNSIGNED_SHORT = &H1403
%IL_INT            = &H1404
%IL_UNSIGNED_INT   = &H1405
%IL_FLOAT          = &H1406
%IL_DOUBLE         = &H140A
%IL_HALF           = &H140B


%IL_VENDOR   = &H1F00
%IL_LOAD_EXT = &H1F01
%IL_SAVE_EXT = &H1F02


'
' IL-specific #define's
'

'%IL_VERSION_1_7_8 1
'%IL_VERSION       178
'

' Attribute Bits
%IL_ORIGIN_BIT          = &H00000001
%IL_FILE_BIT            = &H00000002
%IL_PAL_BIT             = &H00000004
%IL_FORMAT_BIT          = &H00000008
%IL_TYPE_BIT            = &H00000010
%IL_COMPRESS_BIT        = &H00000020
%IL_LOADFAIL_BIT        = &H00000040
%IL_FORMAT_SPECIFIC_BIT = &H00000080
%IL_ALL_ATTRIB_BITS     = &H000FFFFF


' Palette types
%IL_PAL_NONE   = &H0400
%IL_PAL_RGB24  = &H0401
%IL_PAL_RGB32  = &H0402
%IL_PAL_RGBA32 = &H0403
%IL_PAL_BGR24  = &H0404
%IL_PAL_BGR32  = &H0405
%IL_PAL_BGRA32 = &H0406


' Image types
%IL_TYPE_UNKNOWN = &H0000
%IL_BMP          = &H0420  '!< Microsoft Windows Bitmap - .bmp extension
%IL_CUT          = &H0421  '!< Dr. Halo - .cut extension
%IL_DOOM         = &H0422  '!< DooM walls - no specific extension
%IL_DOOM_FLAT    = &H0423  '!< DooM flats - no specific extension
%IL_ICO          = &H0424  '!< Microsoft Windows Icons and Cursors - .ico and .cur extensions
%IL_JPG          = &H0425  '!< JPEG - .jpg, .jpe and .jpeg extensions
%IL_JFIF         = &H0425  '!<
%IL_ILBM         = &H0426  '!< Amiga IFF (FORM ILBM) - .iff, .ilbm, .lbm extensions
%IL_PCD          = &H0427  '!< Kodak PhotoCD - .pcd extension
%IL_PCX          = &H0428  '!< ZSoft PCX - .pcx extension
%IL_PIC          = &H0429  '!< PIC - .pic extension
%IL_PNG          = &H042A  '!< Portable Network Graphics - .png extension
%IL_PNM          = &H042B  '!< Portable Any Map - .pbm, .pgm, .ppm and .pnm extensions
%IL_SGI          = &H042C  '!< Silicon Graphics - .sgi, .bw, .rgb and .rgba extensions
%IL_TGA          = &H042D  '!< TrueVision Targa File - .tga, .vda, .icb and .vst extensions
%IL_TIF          = &H042E  '!< Tagged Image File Format - .tif and .tiff extensions
%IL_CHEAD        = &H042F  '!< C-Style Header - .h extension
%IL_RAW          = &H0430  '!< Raw Image Data - any extension
%IL_MDL          = &H0431  '!< Half-Life Model Texture - .mdl extension
%IL_WAL          = &H0432  '!< Quake 2 Texture - .wal extension
%IL_LIF          = &H0434  '!< Homeworld Texture - .lif extension
%IL_MNG          = &H0435  '!< Multiple-image Network Graphics - .mng extension
%IL_JNG          = &H0435  '!<
%IL_GIF          = &H0436  '!< Graphics Interchange Format - .gif extension
%IL_DDS          = &H0437  '!< DirectDraw Surface - .dds extension
%IL_DCX          = &H0438  '!< ZSoft Multi-PCX - .dcx extension
%IL_PSD          = &H0439  '!< Adobe PhotoShop - .psd extension
%IL_EXIF         = &H043A  '!<
%IL_PSP          = &H043B  '!< PaintShop Pro - .psp extension
%IL_PIX          = &H043C  '!< PIX - .pix extension
%IL_PXR          = &H043D  '!< Pixar - .pxr extension
%IL_XPM          = &H043E  '!< X Pixel Map - .xpm extension
%IL_HDR          = &H043F  '!< Radiance High Dynamic Range - .hdr extension
%IL_ICNS         = &H0440  '!< Macintosh Icon - .icns extension
%IL_JP2          = &H0441  '!< Jpeg 2000 - .jp2 extension
%IL_EXR          = &H0442  '!< OpenEXR - .exr extension
%IL_WDP          = &H0443  '!< Microsoft HD Photo - .wdp and .hdp extension
%IL_VTF          = &H0444  '!< Valve Texture Format - .vtf extension
%IL_WBMP         = &H0445  '!< Wireless Bitmap - .wbmp extension
%IL_SUN          = &H0446  '!< Sun Raster - .sun, .ras, .rs, .im1, .im8, .im24 and .im32 extensions
%IL_IFF          = &H0447  '!< Interchange File Format - .iff extension
%IL_TPL          = &H0448  '!< Gamecube Texture - .tpl extension
%IL_FITS         = &H0449  '!< Flexible Image Transport System - .fit and .fits extensions
%IL_DICOM        = &H044A  '!< Digital Imaging and Communications in Medicine (DICOM) - .dcm and .dicom extensions
%IL_IWI          = &H044B  '!< Call of Duty Infinity Ward Image - .iwi extension
%IL_BLP          = &H044C  '!< Blizzard Texture Format - .blp extension
%IL_FTX          = &H044D  '!< Heavy Metal: FAKK2 Texture - .ftx extension
%IL_ROT          = &H044E  '!< Homeworld 2 - Relic Texture - .rot extension
%IL_TEXTURE      = &H044F  '!< Medieval II: Total War Texture - .texture extension
%IL_DPX          = &H0450  '!< Digital Picture Exchange - .dpx extension
%IL_UTX          = &H0451  '!< Unreal (and Unreal Tournament) Texture - .utx extension
%IL_MP3          = &H0452  '!< MPEG-1 Audio Layer 3 - .mp3 extension


%IL_JASC_PAL     = &H0475  '!< PaintShop Pro Palette


' Error Types
%IL_NO_ERROR             = &H0000
%IL_INVALID_ENUM         = &H0501
%IL_OUT_OF_MEMORY        = &H0502
%IL_FORMAT_NOT_SUPPORTED = &H0503
%IL_INTERNAL_ERROR       = &H0504
%IL_INVALID_VALUE        = &H0505
%IL_ILLEGAL_OPERATION    = &H0506
%IL_ILLEGAL_FILE_VALUE   = &H0507
%IL_INVALID_FILE_HEADER  = &H0508
%IL_INVALID_PARAM        = &H0509
%IL_COULD_NOT_OPEN_FILE  = &H050A
%IL_INVALID_EXTENSION    = &H050B
%IL_FILE_ALREADY_EXISTS  = &H050C
%IL_OUT_FORMAT_SAME      = &H050D
%IL_STACK_OVERFLOW       = &H050E
%IL_STACK_UNDERFLOW      = &H050F
%IL_INVALID_CONVERSION   = &H0510
%IL_BAD_DIMENSIONS       = &H0511
%IL_FILE_READ_ERROR      = &H0512  ' 05/12/2002: Addition by Sam.
%IL_FILE_WRITE_ERROR     = &H0512

%IL_LIB_GIF_ERROR  = &H05E1
%IL_LIB_JPEG_ERROR = &H05E2
%IL_LIB_PNG_ERROR  = &H05E3
%IL_LIB_TIFF_ERROR = &H05E4
%IL_LIB_MNG_ERROR  = &H05E5
%IL_LIB_JP2_ERROR  = &H05E6
%IL_LIB_EXR_ERROR  = &H05E7
%IL_UNKNOWN_ERROR  = &H05FF


' Origin Definitions
%IL_ORIGIN_SET        = &H0600
%IL_ORIGIN_LOWER_LEFT = &H0601
%IL_ORIGIN_UPPER_LEFT = &H0602
%IL_ORIGIN_MODE       = &H0603


' Format and Type Mode Definitions
%IL_FORMAT_SET  = &H0610
%IL_FORMAT_MODE = &H0611
%IL_TYPE_SET    = &H0612
%IL_TYPE_MODE   = &H0613


' File definitions
%IL_FILE_OVERWRITE   = &H0620
%IL_FILE_MODE        = &H0621


' Palette definitions
%IL_CONV_PAL         = &H0630


' Load fail definitions
%IL_DEFAULT_ON_FAIL  = &H0632


' Key colour and alpha definitions
%IL_USE_KEY_COLOUR   = &H0635
%IL_USE_KEY_COLOR    = &H0635
%IL_BLIT_BLEND       = &H0636


' Interlace definitions
%IL_SAVE_INTERLACED  = &H0639
%IL_INTERLACE_MODE   = &H063A


' Quantization definitions
%IL_QUANTIZATION_MODE = &H0640
%IL_WU_QUANT          = &H0641
%IL_NEU_QUANT         = &H0642
%IL_NEU_QUANT_SAMPLE  = &H0643
%IL_MAX_QUANT_INDEXS  = &H0644 'XIX : ILint : Maximum number of colors to reduce to, default of 256. and has a range of 2-256
%IL_MAX_QUANT_INDICES = &H0644 ' Redefined, since the above %is misspelled


' Hints
%IL_FASTEST          = &H0660
%IL_LESS_MEM         = &H0661
%IL_DONT_CARE        = &H0662
%IL_MEM_SPEED_HINT   = &H0665
%IL_USE_COMPRESSION  = &H0666
%IL_NO_COMPRESSION   = &H0667
%IL_COMPRESSION_HINT = &H0668


' Compression
%IL_NVIDIA_COMPRESS  = &H0670
%IL_SQUISH_COMPRESS  = &H0671


' Subimage types
%IL_SUB_NEXT   = &H0680
%IL_SUB_MIPMAP = &H0681
%IL_SUB_LAYER  = &H0682


' Compression definitions
%IL_COMPRESS_MODE = &H0700
%IL_COMPRESS_NONE = &H0701
%IL_COMPRESS_RLE  = &H0702
%IL_COMPRESS_LZO  = &H0703
%IL_COMPRESS_ZLIB = &H0704


' File format-specific values
%IL_TGA_CREATE_STAMP        = &H0710
%IL_JPG_QUALITY             = &H0711
%IL_PNG_INTERLACE           = &H0712
%IL_TGA_RLE                 = &H0713
%IL_BMP_RLE                 = &H0714
%IL_SGI_RLE                 = &H0715
%IL_TGA_ID_STRING           = &H0717
%IL_TGA_AUTHNAME_STRING     = &H0718
%IL_TGA_AUTHCOMMENT_STRING  = &H0719
%IL_PNG_AUTHNAME_STRING     = &H071A
%IL_PNG_TITLE_STRING        = &H071B
%IL_PNG_DESCRIPTION_STRING  = &H071C
%IL_TIF_DESCRIPTION_STRING  = &H071D
%IL_TIF_HOSTCOMPUTER_STRING = &H071E
%IL_TIF_DOCUMENTNAME_STRING = &H071F
%IL_TIF_AUTHNAME_STRING     = &H0720
%IL_JPG_SAVE_FORMAT         = &H0721
%IL_CHEAD_HEADER_STRING     = &H0722
%IL_PCD_PICNUM              = &H0723
%IL_PNG_ALPHA_INDEX = &H0724 'XIX : ILint : the color in the palette at this index value (0-255) is considered transparent, -1 for no trasparent color
%IL_JPG_PROGRESSIVE         = &H0725
%IL_VTF_COMP                = &H0726


' DXTC definitions
%IL_DXTC_FORMAT      = &H0705
%IL_DXT1             = &H0706
%IL_DXT2             = &H0707
%IL_DXT3             = &H0708
%IL_DXT4             = &H0709
%IL_DXT5             = &H070A
%IL_DXT_NO_COMP      = &H070B
%IL_KEEP_DXTC_DATA   = &H070C
%IL_DXTC_DATA_FORMAT = &H070D
%IL_3DC              = &H070E
%IL_RXGB             = &H070F
%IL_ATI1N            = &H0710
%IL_DXT1A            = &H0711  ' Normally the same as IL_DXT1, except for nVidia Texture Tools.

' Environment map definitions
%IL_CUBEMAP_POSITIVEX = &H00000400
%IL_CUBEMAP_NEGATIVEX = &H00000800
%IL_CUBEMAP_POSITIVEY = &H00001000
%IL_CUBEMAP_NEGATIVEY = &H00002000
%IL_CUBEMAP_POSITIVEZ = &H00004000
%IL_CUBEMAP_NEGATIVEZ = &H00008000
%IL_SPHEREMAP         = &H00010000


' Values
%IL_VERSION_NUM           = &H0DE2
%IL_IMAGE_WIDTH           = &H0DE4
%IL_IMAGE_HEIGHT          = &H0DE5
%IL_IMAGE_DEPTH           = &H0DE6
%IL_IMAGE_SIZE_OF_DATA    = &H0DE7
%IL_IMAGE_BPP             = &H0DE8
%IL_IMAGE_BYTES_PER_PIXEL = &H0DE8
%IL_IMAGE_BPP             = &H0DE8
%IL_IMAGE_BITS_PER_PIXEL  = &H0DE9
%IL_IMAGE_FORMAT          = &H0DEA
%IL_IMAGE_TYPE            = &H0DEB
%IL_PALETTE_TYPE          = &H0DEC
%IL_PALETTE_SIZE          = &H0DED
%IL_PALETTE_BPP           = &H0DEE
%IL_PALETTE_NUM_COLS      = &H0DEF
%IL_PALETTE_BASE_TYPE     = &H0DF0
%IL_NUM_FACES             = &H0DE1
%IL_NUM_IMAGES            = &H0DF1
%IL_NUM_MIPMAPS           = &H0DF2
%IL_NUM_LAYERS            = &H0DF3
%IL_ACTIVE_IMAGE          = &H0DF4
%IL_ACTIVE_MIPMAP         = &H0DF5
%IL_ACTIVE_LAYER          = &H0DF6
%IL_ACTIVE_FACE           = &H0E00
%IL_CUR_IMAGE             = &H0DF7
%IL_IMAGE_DURATION        = &H0DF8
%IL_IMAGE_PLANESIZE       = &H0DF9
%IL_IMAGE_BPC             = &H0DFA
%IL_IMAGE_OFFX            = &H0DFB
%IL_IMAGE_OFFY            = &H0DFC
%IL_IMAGE_CUBEFLAGS       = &H0DFD
%IL_IMAGE_ORIGIN          = &H0DFE
%IL_IMAGE_CHANNELS        = &H0DFF
 ' ILU


%ILU_VERSION_1_8_0  = 1
%ILU_VERSION        = 180


%ILU_FILTER        = &H2600
%ILU_NEAREST       = &H2601
%ILU_LINEAR        = &H2602
%ILU_BILINEAR      = &H2603
%ILU_SCALE_BOX     = &H2604
%ILU_SCALE_TRIANGLE= &H2605
%ILU_SCALE_BELL    = &H2606
%ILU_SCALE_BSPLINE = &H2607
%ILU_SCALE_LANCZOS3= &H2608
%ILU_SCALE_MITCHELL= &H2609


'// Error types
%ILU_INVALID_ENUM     = &H0501
%ILU_OUT_OF_MEMORY    = &H0502
%ILU_INTERNAL_ERROR   = &H0504
%ILU_INVALID_VALUE    = &H0505
%ILU_ILLEGAL_OPERATION= &H0506
%ILU_INVALID_PARAM    = &H0509


'// Values
%ILU_PLACEMENT         = &H0700
%ILU_LOWER_LEFT        = &H0701
%ILU_LOWER_RIGHT       = &H0702
%ILU_UPPER_LEFT        = &H0703
%ILU_UPPER_RIGHT       = &H0704
%ILU_CENTER            = &H0705
%ILU_CONVOLUTION_MATRIX= &H0710

%ILU_VERSION_NUM = %IL_VERSION_NUM
'%ILU_VENDOR      = %IL_VENDOR


'// Languages
%ILU_ENGLISH           = &H0800
%ILU_ARABIC            = &H0801
%ILU_DUTCH             = &H0802
%ILU_JAPANESE          = &H0803
%ILU_SPANISH           = &H0804
%ILU_GERMAN            = &H0805
%ILU_FRENCH            = &H0806
%ILU_ITALIAN           = &H0807


'// Filters

%ILU_FILTER_BLUR        = &H0803
%ILU_FILTER_GAUSSIAN_3x3 = &H0804
%ILU_FILTER_GAUSSIAN_5X5 = &H0805
%ILU_FILTER_EMBOSS1     = &H0807
%ILU_FILTER_EMBOSS2     = &H0808
%ILU_FILTER_LAPLACIAN1  = &H080A
%ILU_FILTER_LAPLACIAN2  = &H080B
%ILU_FILTER_LAPLACIAN3  = &H080C
%ILU_FILTER_LAPLACIAN4  = &H080D
%ILU_FILTER_SHARPEN1    = &H080E
%ILU_FILTER_SHARPEN2    = &H080F
%ILU_FILTER_SHARPEN3    = &H0810



'
'// Initialize ILU
'iluInit();
DECLARE FUNCTION  ILInit LIB "Devil.dll" ALIAS "ilInit" () AS LONG
DECLARE FUNCTION  ILuInit LIB "ilu.dll" ALIAS "iluInit" () AS LONG
DECLARE FUNCTION ilClearColour LIB "Devil.dll" ALIAS "ilClearColour"(BYVAL Red AS BYTE, _
       BYVAL Green AS BYTE,BYVAL BLUE AS BYTE,BYVAL Alpha AS BYTE ) AS LONG
DECLARE FUNCTION  ILOriginFunc LIB "Devil.dll" ALIAS "ilOriginFunc" (BYVAL origin AS LONG) AS LONG
DECLARE FUNCTION  ILEnable LIB "Devil.dll" ALIAS "ilEnable" (BYVAL origin AS DWORD) AS LONG
DECLARE FUNCTION  ILGetError LIB "Devil.dll" ALIAS "ilGetError" () AS LONG
DECLARE FUNCTION  ILGetBoolean LIB "Devil.dll" ALIAS "ilGetBoolean"  (BYVAL ILenum AS DWORD)AS LONG
DECLARE FUNCTION  ILGetInteger LIB "Devil.dll" ALIAS "ilGetInteger"  (BYVAL ILenum AS DWORD)AS LONG

DECLARE FUNCTION  ILLoadImage LIB "Devil.dll" ALIAS "ilLoadImage" ( Fname AS ASCIIZ ) AS LONG
DECLARE FUNCTION  ILGetData LIB "Devil.dll" ALIAS "ilGetData" () AS DWORD
DECLARE FUNCTION  ILGetIntegerv LIB "Devil.dll" ALIAS "ilGetIntegerv"(BYVAL ILenum AS DWORD,result AS LONG) AS LONG
DECLARE FUNCTION  ILGenImages LIB "Devil.dll" ALIAS "ilGenImages"(BYVAL ILSIZE AS LONG,ILunit AS DWORD) AS LONG
DECLARE FUNCTION  ILBindImage LIB "Devil.dll" ALIAS "ilBindImage"( BYVAL Ilunit AS DWORD) AS LONG
DECLARE FUNCTION  ILConvertImage LIB "Devil.dll" ALIAS "ilConvertImage"  (BYVAL ILenum AS DWORD, BYVAL ILunit AS DWORD)  AS LONG
DECLARE FUNCTION  ILSaveImage LIB "Devil.dll" ALIAS "ilSaveImage" (  Fname AS ASCIIZ ) AS LONG
DECLARE FUNCTION  ILCopyPixels LIB "Devil.dll" ALIAS "ilCopyPixels"(BYVAL XOff AS LONG,BYVAL Yoff AS LONG, _
       BYVAL ZOff AS LONG,BYVAL Wdth AS LONG,BYVAL Height AS LONG,BYVAL Depth AS LONG,BYVAL Format AS LONG, _
       BYVAL Typ AS LONG,BYVAL Buffer AS DWORD) AS LONG
'ILU
DECLARE FUNCTION  ILuScale LIB "ILU.dll" ALIAS "iluScale"(BYVAL WIDTHY AS DWORD,BYVAL Height AS DWORD,BYVAL depth AS DWORD) AS LONG
DECLARE FUNCTION  iluRotate LIB "ILU.dll" ALIAS "iluRotate" (BYVAL Angle AS SINGLE )AS LONG
DECLARE FUNCTION  iluNegative LIB "ILU.dll" ALIAS "iluNegative"() AS LONG
'DECLARE FUNCTION  iluImageParameter LIB "ILU.dll" ALIAS "iluImageParameter"(BYVAL ILenum AS DWORD, BYVAL ILenum AS DWORD) AS LONG
'DECLARE FUNCTION  iluEnlargeCanvas LIB "ILU.dll" ALIAS "iluEnlargeCanvas" (BYVAL xWidth AS DWORD,Height AS DWORD,Depth AS DWORD) as long

DECLARE FUNCTION  iluAlienify LIB "ILU.dll" ALIAS "iluAlienify " ()AS LONG
DECLARE FUNCTION  iluBlurAvg LIB "ILU.dll" ALIAS " iluBlurAvg" ( BYCALIter AS INTEGER)AS LONG
DECLARE FUNCTION  iluBlurGaussian LIB "ILU.dll" ALIAS " iluBlurGaussian"(BYVAL Iter AS INTEGER)AS LONG
DECLARE FUNCTION  iluBuildMipmaps LIB "ILU.dll" ALIAS "iluBuildMipmaps" () AS LONG
DECLARE FUNCTION  iluColoursUsed LIB "ILU.dll" ALIAS "iluColoursUsed" () AS INTEGER
DECLARE FUNCTION  iluCompareImage LIB "ILU.dll" ALIAS "iluCompareImage" (BYVAL Comp AS INTEGER)AS LONG
DECLARE FUNCTION  iluContrast LIB "ILU.dll" ALIAS "iluContrast" (BYVAL ontrast AS DOUBLE)AS LONG
DECLARE FUNCTION  iluCrop LIB "ILU.dll" ALIAS "iluCrop"(BYVAL XOff AS INTEGER, BYVAL YOff AS INTEGER, _
                  BYVAL ZOff AS INTEGER, BYVAL Width AS INTEGER, BYVAL Height AS INTEGER, _
                  BYVAL Depth AS INTEGER)AS LONG
'ILAPI void           ILAPIENTRY iluDeleteImage LIB "ILU.dll" ALIAS "ILuint Id); // Deprecated
DECLARE FUNCTION  iluEdgeDetectE LIB "ILU.dll" ALIAS "iluEdgeDetectE" () AS LONG
DECLARE FUNCTION  iluEdgeDetectP LIB "ILU.dll" ALIAS "iluEdgeDetectP" () AS LONG
DECLARE FUNCTION  iluEdgeDetectS LIB "ILU.dll" ALIAS "iluEdgeDetectS" () AS LONG
DECLARE FUNCTION  iluEmboss LIB "ILU.dll" ALIAS "iluEmboss" () AS LONG
DECLARE FUNCTION  iluEnlargeCanvas LIB "ILU.dll" ALIAS "iluEnlargeCanvas" (BYVAL xWidth AS INTEGER, _
                  BYVAL Height AS INTEGER, BYVAL Depth AS INTEGER)AS LONG
DECLARE FUNCTION  iluEnlargeImage LIB "ILU.dll" ALIAS "iluEnlargeImage" ( BYVAL XDim AS DOUBLE, _
                   BYVAL YDim AS DOUBLE,  BYVAL ZDim AS DOUBLE)AS LONG
DECLARE FUNCTION  iluEqualize LIB "ILU.dll" ALIAS "iluEqualizevoid"()AS LONG
DECLARE FUNCTION  iluEqualize2 LIB "ILU.dll" ALIAS " iluEqualize2"()AS LONG
DECLARE FUNCTION  iluErrorString LIB "ILU.dll" ALIAS "iluErrorString"(BYVAL xError AS LONG)AS LONG
DECLARE FUNCTION  iluConvolution LIB "ILU.dll" ALIAS " iluConvolution" ( BYVAL Pmatrix AS INTEGER, _
                    BYVAL Xscale AS INTEGER, BYVAL bias AS INTEGER ) AS LONG
DECLARE FUNCTION  iluFlipImage LIB "ILU.dll" ALIAS "iluFlipImage" () AS LONG
DECLARE FUNCTION  iluGammaCorrect LIB "ILU.dll" ALIAS "iluGammaCorrect" (BYVAL Gamma AS DOUBLE)AS LONG
'ILAPI ILuint         ILAPIENTRY iluGenImage LIB "ILU.dll" ALIAS "void); // Deprecated
DECLARE FUNCTION  iluGetImageInfo LIB "ILU.dll" ALIAS "iluGetImageInfo"(BYVAL Pinfo AS LONG)AS LONG
DECLARE FUNCTION  iluGetInteger LIB "ILU.dll" ALIAS "iluGetInteger"(BYVAL xmode AS LONG) AS INTEGER
DECLARE FUNCTION  iluGetIntegerv LIB "ILU.dll" ALIAS "iluGetIntegerv"(BYVAL xMode AS LONG, _
                  BYVAL PParam AS LONG)AS LONG
DECLARE FUNCTION  iluGetString LIB "ILU.dll" ALIAS "iluGetString"(BYVAL StringName AS LONG)AS LONG
DECLARE FUNCTION  iluImageParameter LIB "ILU.dll" ALIAS "iluImageParameter"(BYVAL ParmName AS LONG, _
                  BYVAL Param AS LONG)AS LONG
DECLARE FUNCTION  iluInvertAlpha LIB "ILU.dll" ALIAS "iluInvertAlpha"()AS LONG
DECLARE FUNCTION  iluLoadImage LIB "ILU.dll" ALIAS "iluLoadImage"(Fname AS ASCIIZ)AS LONG
DECLARE FUNCTION  iluMirror LIB "ILU.dll" ALIAS "iluMirror"()AS LONG
DECLARE FUNCTION  iluNegative LIB "ILU.dll" ALIAS "iluNegative"()AS LONG
'DECLARE FUNCTION  iluNoisify LIB "ILU.dll" ALIAS "iluNoisify"(BYVAL tolerance as ILclampf)as long
DECLARE FUNCTION  iluPixelize LIB "ILU.dll" ALIAS "iluPixelize"(BYVAL PixSize AS INTEGER) AS LONG
DECLARE FUNCTION  iluRegionfv LIB "ILU.dll" ALIAS "iluRegionfv"(BYVAL Points AS ILpointf PTR,n AS INTEGER)AS LONG
DECLARE FUNCTION  iluRegioniv LIB "ILU.dll" ALIAS "iluRegioniv"(BYVAL Points AS ILpointI PTR,n AS INTEGER)AS LONG
DECLARE FUNCTION  iluReplaceColour LIB "ILU.dll" ALIAS "iluReplaceColour"(BYVAL Red AS BYTE, _
                 BYVAL Green AS BYTE, BYVAL Blue AS BYTE, BYVAL Tolerance AS DOUBLE)AS LONG
DECLARE FUNCTION  iluRotate3D LIB "ILU.dll" ALIAS "iluRotate3D"(BYVAL x AS DOUBLE, BYVAL y AS DOUBLE, _
                 BYVAL z AS DOUBLE, BYVAL Angle AS DOUBLE)AS LONG
DECLARE FUNCTION  iluSaturate1f LIB "ILU.dll" ALIAS "iluSaturate1f"(BYVAL Saturation AS DOUBLE) AS LONG
DECLARE FUNCTION  iluSaturate4f LIB "ILU.dll" ALIAS "iluSaturate4f"(BYVAL r AS DOUBLE,BYVAL g AS DOUBLE, _
                 BYVAL b AS DOUBLE,BYVAL Saturation AS DOUBLE)AS LONG
DECLARE FUNCTION  iluScaleAlpha LIB "ILU.dll" ALIAS "iluScaleAlpha"(BYVAL xscale AS DOUBLE)AS LONG
DECLARE FUNCTION  iluScaleColours LIB "ILU.dll" ALIAS "iluScaleColours"(BYVAL r AS DOUBLE, _
                  BYVAL g AS DOUBLE,BYVAL b AS DOUBLE)AS LONG
DECLARE FUNCTION  iluSepia LIB "ILU.dll" ALIAS "iluSepia"() AS LONG
DECLARE FUNCTION  iluSetLanguage LIB "ILU.dll" ALIAS "iluSetLanguage"(BYVAL Language AS LONG)AS LONG
DECLARE FUNCTION  iluSharpen LIB "ILU.dll" ALIAS "iluSharpen"(BYVAL Factor AS DOUBLE, BYVAL Iter AS INTEGER)AS LONG
DECLARE FUNCTION  iluSwapColours LIB "ILU.dll" ALIAS "iluSwapColours"()AS LONG
DECLARE FUNCTION  iluWave LIB "ILU.dll" ALIAS "iluWave"(BYVAL Angle AS DOUBLE)AS LONG



SUB clrerr
 LOCAL exr AS LONG
 LOCAL I AS LONG
 FOR I = 1 TO 200
     exr = ilgeterror
    IF exr=0 THEN EXIT FOR
 NEXT
END SUB

SUB DeErr(msg AS STRING)
LOCAL exr AS LONG
     exr= ilgeterror
msg=""
 SELECT CASE exr
     ' Error Types
  CASE 0
         msg = ""
  CASE  &H0501
         msg  =  "INVALID ENUM"
  CASE &H0502
         msg  =  "OUT OF MEMORY"
  CASE &H0503
         msg  =  "FORMAT NOT SUPPORTED"
  CASE &H0504
         msg  =  "INTERNAL ERROR"
  CASE &H0505
         msg  =  "INVALID VALUE"
  CASE &H0506
         msg  =  "ILLEGAL OPERATION"
  CASE &H0507
         msg  =  "ILLEGAL FILE VALUE"
  CASE &H0508
         msg  =  "INVALID FILE HEADER"
  CASE &H0509
         msg  =  "INVALID PARAMATER"
  CASE &H050A
         msg  =  "UNABLE TO OPEN FILE"
  CASE &H050B
         msg  =  "INVALID EXTENSION"
  CASE &H050C
         msg  =  "FILE ALREADY EXISTS"
  CASE &H050D
         msg  =  "OUT FORMAT SAME"
  CASE &H050E
         msg  =  "STACK OVERFLOW "
  CASE &H050F
         msg  =  "STACK UNDERFLOW"
  CASE &H0510
         msg  =  "INVALID CONVERSION"
  CASE &H0511
         msg  =  "BAD DIMENSIONS"
  CASE &H0512  ' 05/12/2002: Addition by Sam.
         msg  =  "FILE READ ERROR "
  CASE &H0512
         msg  =  "FILE WRITE ERROR "
  CASE &H05E1
         msg  =  "LIB GIF ERROR"
  CASE &H05E2
         msg  =  "LIB JPEG ERROR"
  CASE &H05E3
         msg  =  "LIB PNG ERROR"
  CASE &H05E4
         msg  =  "LIB TIFF ERROR"
  CASE &H05E5
         msg  =  "LIB MNG ERROR"
  CASE &H05E6
         msg  =  "LIB JP2 ERROR"
  CASE &H05E7
         msg  =  "LIB EXR ERROR"
  CASE &H05FF
         msg  =  "UNKNOWN ERROR"
  CASE ELSE
      msg= "Error "+HEX$(exr)
END SELECT

END SUB

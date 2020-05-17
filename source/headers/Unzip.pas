unit Unzip;

interface

{$IFDEF FPC}
  {$MODE Delphi}

  {$IFNDEF WINDOWS}
    {$LINKLIB c}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils, Classes
  {$IFDEF Win32}, Windows{$ENDIF}
  {$IFDEF Win64}, Windows{$ENDIF}
  {$IFDEF Linux}, X, XLib, XUtil, Ctypes{$ENDIF}
  ;

{ windll.h }
const
  IDM_REPLACE_NO        = 100;
  IDM_REPLACE_TEXT      = 101;
  IDM_REPLACE_YES       = 102;
  IDM_REPLACE_ALL       = 103;
  IDM_REPLACE_NONE      = 104;
  IDM_REPLACE_RENAME    = 105;
  IDM_REPLACE_HELP      = 106;

// Return (and exit) values of the public UnZip API functions.
const
  // external return codes
  PK_OK                 = 0; { no error }
  PK_COOL               = 0; { no error }
  PK_GNARLY             = 0; { no error }
  PK_WARN               = 1; { warning error }
  PK_ERR                = 2; { error in zipfile }
  PK_BADERR             = 3; { severe error in zipfile }
  PK_MEM                = 4; { insufficient memory (during initialization) }
  PK_MEM2               = 5; { insufficient memory (password failure) }
  PK_MEM3               = 6; { insufficient memory (file decompression) }
  PK_MEM4               = 7; { insufficient memory (memory decompression) }
  PK_MEM5               = 8; { insufficient memory (not yet used) }
  PK_NOZIP              = 9; { zipfile not found }
  PK_PARAM              = 10; { bad or illegal parameters specified }
  PK_FIND               = 11; { no files found }
  PK_DISK               = 50; { disk full }
  PK_EOF                = 51; { unexpected EOF }

  IZ_CTRLC              = 80; { user hit ^C to terminate }
  IZ_UNSUP              = 81; { no files found: all unsup. compr/encrypt. }
  IZ_BADPWD             = 82; { no files found: all had bad password }

  // internal and DLL-only return codes
  IZ_DIR                = 76; { potential zipfile is a directory }
  IZ_CREATED_DIR        = 77; { directory created: set time and permissions }
  IZ_VOL_LABEL          = 78; { volume label, but can't set on hard disk }
  IZ_EF_TRUNC           = 79; { local extra field truncated (PKZIP'd) }

  // return codes of password fetches (negative = user abort; positive = error)
  IZ_PW_ENTERED          = 0; { got some password string; use/try it }
  IZ_PW_CANCEL           = -1; { no password available (for this entry) }
  IZ_PW_CANCELALL        = -2; { no password, skip any further pwd. request }
  IZ_PW_ERROR            = 5; { = PK_MEM2 : failure (no mem, no tty, ...) }

  // flag values for status callback function
  UZ_ST_START_EXTRACT    = 1;
  UZ_ST_IN_PROGRESS      = 2;
  UZ_ST_FINISH_MEMBER    = 3;

  // return values of status callback function
  UZ_ST_CONTINUE         = 0;
  UZ_ST_BREAK            = 1;



type
  ulong                 = culong;
  uint                  = cuint;
  TCallbackPrint    = function(Buffer: PAnsiChar; Size: ULONG): Integer; stdcall;
  TCallbackSound    = procedure; stdcall;
  TCallbackPassword = function(P: PAnsiChar; N: Integer; M, Name: PAnsiChar): Integer; stdcall;
  TCallbackService  = function(CurFile: PAnsiChar; Size: ULONG): Integer; stdcall;

  // a pointer to the application's replace routine. The
  // replace routine may modify the content of the efnam
  // string buffer, but must not write beyond its fixed
  // size of FILNAMSIZ! (This is a potential security
  // leak of the windll interface.) When modifying the
  // efnam buffer, the replace routine should return
  // the status IDM_REPLACE_RENAME.
  TCallbackReplace  = function(FileName: PAnsiChar): Integer; stdcall;

  TCallbackMessage  = procedure( UnCompSize : ULONG;
                                 CompSize   : ULONG;
                                 Factor     : UINT;
                                 Month      : UINT;
                                 Day        : UINT;
                                 Year       : UINT;
                                 Hour       : UINT;
                                 Minute     : UINT;
                                 C          : AnsiChar;
                                 FileName   : PAnsiChar;
                                 MethBuf    : PAnsiChar;
                                 CRC        : ULONG;
                                 Crypt      : AnsiChar); stdcall;


type

_version_type = packed record
  major:                    Byte;                   // major version number
  minor:                    Byte;                   // minor version number
  patchlevel:               Byte;                   // patch level
  not_used:                 Byte;
end;

//------------------------------------------------------------------------------
TUzpVersionInfo = packed record
  // length of the struct being passed
  structlen: Longword;
  // bit 0: is_beta   bit 1: uses_zlib
  flag: Longword;
  // e.g., "g BETA" or ""
  betalevel: array[0..9 ] of AnsiChar;
  // e.g., "4 Sep 95" (beta) or "4 September 1995"
  date: array[0..19] of AnsiChar;
  // e.g., "0.95" or NULL
  zlib_version: array[0..9 ] of AnsiChar;
  unzip  : _version_type;
  zipinfo: _version_type;
  os2dll : _version_type;
  windll : _version_type;
end;
PUzpVersionInfo = ^TUzpVersionInfo;

// User callbacks.
//
// Theese are required for the functions Wiz_Grep, Wiz_Init, Wiz_SingleEntryUnzip
// and Wiz_UnzipToMemory
//------------------------------------------------------------------------------
TUserFunctions = record
  Print                  : TCallbackPrint;
  Sound                  : TCallbackSound;
  Replace                : TCallbackReplace;
  Password               : TCallbackPassword;
  SendApplicationMessage : TCallbackMessage;
  ServCallBk             : TCallbackService;
  TotalSizeComp          : Cardinal;
  TotalSize              : Cardinal;
  CompFactor             : Integer;
  NumMembers             : Cardinal;
  cchComment             : Cardinal;
end;
PUserFunctions = ^TUserFunctions;


//------------------------------------------------------------------------------
TDCL = record
  ExtractOnlyNewer:         Longint;                // 1 - extract only newer
  SpaceToUnderscore:        Longint;                // 1 - convert space to underscore
  PromptToOverwrite:        Longint;                // 1 - prompt before overwrite
  fQuiet:                   Longint;                // 0 - return all messages, 1 = fewer messages, 2 = no messages
  ncflag:                   Longint;                // 1 - write to stdout
  ntflag:                   Longint;                // 1 - test zip file
  // 0 = Extract
  // 1 = List Zip Contents
  nvflag:                   Longint;                // 1 - give a verbose listing
  nfflag:                   Longint;                // 1 - freshen existing files only
  nzflag:                   Longint;                // 1 - display a zip file comment
  ndflag:                   Longint;                // >0 - recreate directories, <2 - skip "../"
  noflag:                   Longint;                // 1 - over-write all files
  naflag:                   Longint;                // 1 - convert CR to CRLF
  nZIflag:                  Longint;                // 1 - verbose zip info
  C_flag:                   Longint;                // 0 - case sensitive, 1 - case insensitive
  fPrivilege:               Longint;                // 1 - ACL, 2 - priv
  lpszZipFN:                PAnsiChar;              // archive name
  lpszExtractDir:           PAnsiChar;              // target directory, NULL - current directory
end;
PDCL = ^ TDCL;

// Buffer for unzipping to memory
//------------------------------------------------------------------------------
TUzpBuffer = packed record
  Size : Cardinal;       { length of string }
  Data : PAnsiChar;      { pointer to string }
end;
PUzpBuffer = ^TUzpBuffer;


// http://svn.assembla.com/svn/os2utils/unzip60f/windll/windll.txt

// dll prototypes
type
  PPAnsiChar = ^PAnsiChar;

  // Initialize the unzip library
  TWiz_Init = function(pG: Pointer; UserFunc: PUserFunctions): Boolean; stdcall;

  TUzpVersion = procedure(Version: PUzpVersionInfo); stdcall;

  // Validate an archive
  TWiz_Validate = function(Archive: PAnsiChar; AllCodes: Integer): Integer; stdcall;


  TWiz_SetOpts = function (pG: Pointer; c: PDCL): LongBool; stdcall;

  TWiz_Unzip = function(pG: Pointer; ifnc: Integer; ifnv: PPAnsiChar; xfnc: Integer; xfnv: PPAnsiChar): Integer; stdcall;


  TWiz_SingleEntryUnzip = function(ifnc: Longint; ifnv: PPAnsiChar; xfnc: Longint; xfnv: PPAnsiChar; Options: PDCL; UserFunc: PUserFunctions): Integer; stdcall;

  // Unzip a single file to a memory buffer
  TWiz_UnzipToMemory = function(Archive: PAnsiChar; FileName: PAnsiChar; UserFunctions: PUserFunctions; Buffer: PUzpBuffer): Integer; stdcall;
  // Unzip a single file to a stream
  TWiz_UnzipToStream = function(Archive: PAnsiChar; FileName: PAnsiChar; UserFunctions: PUserFunctions; Stream: TStream): Integer;

  // Free a buffer returned from TWiz_UnzipToMemory
  TUzpFreeMemBuffer = procedure(Buffer: PUzpBuffer); stdcall;


const

{$IFDEF WIN32}
  LIBNAME = 'unzip32.dll';
{$ELSE}
  {$IFDEF darwin}
    LIBNAME = 'unzip32.dylib';
  {$ELSE}
    LIBNAME = 'unzip32.so.1';
  {$ENDIF}
{$ENDIF}

var
  LibHandle: Pointer = nil;

//------------------------------------------------------------------------------
function LoadUnzipLib(const LibName: String): Boolean;

var UzpVersion: TUzpVersion;

var Wiz_Init      : TWiz_Init;
var Wiz_Validate  : TWiz_Validate;
var Wiz_SetOpts   : TWiz_SetOpts;

var Wiz_Unzip     : TWiz_Unzip;
var Wiz_UnzipToMemory: TWiz_UnzipToMemory;
// Helper function to unzip a archived file to a stream
var Wiz_UnzipToStream: TWiz_UnzipToStream;

var Wiz_SingleEntryUnzip: TWiz_SingleEntryUnzip;

var UzpFreeMemBuffer : TUzpFreeMemBuffer;

// Convert a error code to a string
function Wiz_ErrorToStr(Code: Integer): String;


var DefaultUserFunctions: TUserFunctions;


implementation


//------------------------------------------------------------------------------
function CallbackPrint(Buffer: PAnsiChar; Size: Cardinal): integer; stdcall;
begin
  //frmMain.Memo2.Lines.Add(Buffer);
  Result := Size;
end;

//------------------------------------------------------------------------------
function CallbackPassword(P: PAnsiChar; N: Integer; M, Name: PAnsiChar): integer;  stdcall;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------
function CallbackService(CurFile: PAnsiChar; Size: Cardinal): integer; stdcall;
begin
  Result := 0;
end;

//------------------------------------------------------------------------------
function CallbackReplace(FileName: PAnsiChar): integer; stdcall;
begin
  Result := 1;
end;

//------------------------------------------------------------------------------
procedure CallbackMessage(UnCompSize : Cardinal;
                     CompSize   : Cardinal;
                     Factor     : Cardinal;
                     Month      : Cardinal;
                     Day        : Cardinal;
                     Year       : Cardinal;
                     Hour       : Cardinal;
                     Minute     : Cardinal;
                     C          : AnsiChar;
                     FileName   : PAnsiChar;
                     MethBuf    : PAnsiChar;
                     CRC        : Cardinal;
                     Crypt      : AnsiChar); stdcall;
begin
end;








//------------------------------------------------------------------------------
function LoadLibrary(Name: PChar): Pointer;
begin
  {$IFDEF Win32}
  Result := Pointer(Windows.LoadLibrary(Name));
  {$ENDIF}
  {$IFDEF Win64}
  Result := Pointer(Windows.LoadLibrary(Name));
  {$ENDIF}

  {$IFDEF LINUX}
  Result := dlopen(Name, RTLD_LAZY);
  {$ENDIF}
end;

//------------------------------------------------------------------------------
function FreeLibrary(LibHandle: Pointer): Boolean;
begin
  if LibHandle = nil then
    Result := False
  else
  begin
    {$IFDEF Win32}
    Result := Windows.FreeLibrary(HMODULE(LibHandle));
    {$ENDIF}

    {$IFDEF LINUX}
    Result := dlclose(LibHandle) = 0;
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------
function GetProcAddress(ProcName: PAnsiChar): Pointer;
begin
  {$IFDEF Win32}
  Result := Windows.GetProcAddress(HMODULE(LibHandle), ProcName);
  {$ENDIF}

  {$IFDEF DGL_LINUX}
  Result := dlsym(LibHandle, ProcName);
  {$ENDIF}
end;


//------------------------------------------------------------------------------
function _Wiz_UnzipToStream(Archive: PAnsiChar; FileName: PAnsiChar; UserFunctions: PUserFunctions; Stream: TStream): Integer;
var Buffer: TUzpBuffer;
begin
  Buffer.Size:= 0;
  Buffer.Data:= nil;

  Result:= Wiz_UnzipToMemory(Archive, FileName, UserFunctions, @Buffer);

  if Result = 1 then
  begin
    Stream.Write(Buffer.Data^, Buffer.Size);

    Stream.Position:= 0;

    Result:= PK_OK
  end else
  if Result = 0 then
  begin
    Result:= PK_ERR;
  end;

  UzpFreeMemBuffer(@buffer);
end;


//------------------------------------------------------------------------------
function LoadUnzipLib(const LibName: String ): Boolean;
begin
  Result := False;

  // free opened libraries
  if LibHandle <> nil then
  begin
    FreeLibrary(LibHandle);
  end;

  // load library
  LibHandle := LoadLibrary(PChar(LibName));

  // load GL functions
  if (LibHandle <> nil) then
  begin
    UzpVersion        := GetProcAddress('UzpVersion2');

    Wiz_Init            := GetProcAddress('Wiz_Init');
    Wiz_Validate        := GetProcAddress('Wiz_Validate');
    Wiz_SetOpts         := GetProcAddress('Wiz_SetOpts');
    Wiz_Unzip           := GetProcAddress('Wiz_Unzip');
    Wiz_UnzipToMemory   := GetProcAddress('Wiz_UnzipToMemory');

    Wiz_SingleEntryUnzip:= GetProcAddress('Wiz_SingleEntryUnzip');

    UzpFreeMemBuffer  := GetProcAddress('UzpFreeMemBuffer');


    Wiz_UnzipToStream:= _Wiz_UnzipToStream;
    Result := True;
  end;

end;

//------------------------------------------------------------------------------
function Wiz_ErrorToStr(Code: Integer): String;
begin
   case code of
      PK_OK:          Result := 'Operation completed successfully';
      PK_WARN:        Result := 'Warnings occurred on one or more files';
      PK_ERR:         Result := 'Errors occurred on one or more files';
      PK_BADERR:      Result := 'Severe error in archive';
      PK_MEM,
      PK_MEM2,
      PK_MEM3,
      PK_MEM4,
      PK_MEM5:        Result := 'Insufficient memory';
      PK_NOZIP:       Result := 'Archive not found';
      PK_PARAM:       Result := 'Bad or illegal parameters specified';
      PK_FIND:        Result := 'No files found';
      PK_DISK:        Result := 'Disk full';
      PK_EOF:         Result := 'Unexpected end of file';
      IZ_CTRLC:       Result := 'Canceled by user';
      IZ_UNSUP:       Result := 'No files found: All unsupported';
      IZ_BADPWD:      Result := 'No files found: Bad password';
   else
      Result := 'Unknown error';
   end;
end;



initialization

  with DefaultUserFunctions do
  begin
    print := CallbackPrint;
    sound := nil;
    replace := CallbackReplace;
    password := CallbackPassword;
    SendApplicationMessage := CallbackMessage;
    ServCallBk := CallbackService;
  end;

finalization
  // free opened libraries
  if LibHandle <> nil then
  begin
    FreeLibrary(LibHandle);
  end;
end.

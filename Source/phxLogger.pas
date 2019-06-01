////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxLogger;
//< Logging framework

interface

{$I phxConfig.inc}

uses Classes, SysUtils;

{$IFNDEF PHX_LOGGING}
  {$MESSAGE Hint 'TPHXLog: logging is disabled'}
{$ELSE}
  {$IFNDEF PHX_VERBOSE_LOGGING}
    {$MESSAGE Hint 'TPHXLog: Verbose logging is disabled'}
  {$ENDIF}
{$ENDIF}

const
  PHXLOG_VERSION = '1.0';

type

// The four log levels
//------------------------------------------------------------------------------
TPHXLogLevel = (
  // Debug message
  logDebug  = 0,
  // Information message
  logInfo = 1,
  // Warning
  logWarn = 2,
  // Error
  logError
);

TPHXLogEvent = procedure(const Source: String; const Text: String) of object;

// Abstract handler for log messages
//------------------------------------------------------------------------------
TPHXLogHandler = class(TObject)
  public
    // Log a debug message
    procedure Debug(const Source: String; const Text: String); virtual; abstract;
    // Log an information message
    procedure Info(const Source: String; const Text: String); virtual; abstract;
    // Log a warning
    procedure Warn(const Source: String; const Text: String); virtual; abstract;
    // Log a debug message
    procedure Error(const Source: String; const Text: String); virtual; abstract;
  end;

{$REGION 'TPHXLogHandlerEvent'}

// Log handler that calls a event for each log
//------------------------------------------------------------------------------
TPHXLogHandlerEvent = class(TPHXLogHandler)
  private
    FOnDebug: TPHXLogEvent;
    FOnInfo: TPHXLogEvent;
    FOnWarn: TPHXLogEvent;
    FOnError: TPHXLogEvent;
  public
    // Creates a new event log handler
    constructor Create;
    // Default destructor
    destructor Destroy; override;

    // Log a debug message
    procedure Debug(const Source: String; const Text: String); override;
    // Log an information message
    procedure Info(const Source: String; const Text: String); override;
    // Log a warning
    procedure Warn(const Source: String; const Text: String); override;
    // Log a debug message
    procedure Error(const Source: String; const Text: String); override;

    property OnDebug: TPHXLogEvent read FOnDebug write FOnDebug;
    property OnInfo: TPHXLogEvent read FOnInfo write FOnInfo;
    property OnWarn: TPHXLogEvent read FOnWarn write FOnWarn;
    property OnError: TPHXLogEvent read FOnError write FOnError;
  end;

{$ENDREGION}

{$REGION 'TPHXLogHandlerText'}

// Log handler that print log messages to a simple text file
//------------------------------------------------------------------------------
TPHXLogHandlerText = class(TPHXLogHandler)
  private
    FLogName: String;
    FLogFile: TextFile;

    procedure Open;
    procedure Close;
  public
    // Creates a new text log handler
    constructor Create(const ALogName: String);
    // Default destructor
    destructor Destroy; override;

    // Log a debug message
    procedure Debug(const Source: String; const Text: String); override;
    // Log an information message
    procedure Info(const Source: String; const Text: String); override;
    // Log a warning
    procedure Warn(const Source: String; const Text: String); override;
    // Log a debug message
    procedure Error(const Source: String; const Text: String); override;

    // Filename of the log file
    property LogName: String  read FLogName;
    // The handle to the log file
    property LogFile: TextFile  read FLogFile;
  end;

{$ENDREGION}

{$REGION 'TPHXLogSystemXML'}

//------------------------------------------------------------------------------
TPHXLogSystemXML = class(TPHXLogHandler)
  private
    FFileName: String;
    FLine    : Cardinal;
    FLogFile : TextFile;
    procedure Print(Level: TPHXLogLevel; const Source: String; const Text: String);

    Procedure Open;
    Procedure Close;

    procedure SetFileName(const Value: String);
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;

    // Log a debug message
    procedure Debug(const Source: String; const Text: String); override;
    // Log an information message
    procedure Info(const Source: String; const Text: String); override;
    // Log a warning
    procedure Warn(const Source: String; const Text: String); override;
    // Log a debug message
    procedure Error(const Source: String; const Text: String); override;

    Property FileName: String   read FFileName write SetFileName;
    Property Line    : Cardinal read FLine     write FLine;
  end;

{$ENDREGION}

{$REGION 'TPHXLogSystemConsole'}

//------------------------------------------------------------------------------
TPHXLogSystemConsole = class(TPHXLogHandler)
  public
    // Log a debug message
    procedure Debug(const Source: String; const Text: String); override;
    // Log an information message
    procedure Info(const Source: String; const Text: String); override;
    // Log a warning
    procedure Warn(const Source: String; const Text: String); override;
    // Log a debug message
    procedure Error(const Source: String; const Text: String); override;
  end;

{$ENDREGION}

{$REGION 'TPHXLogSystemStrings'}

// Log system that adds messages to a string list
//------------------------------------------------------------------------------
TPHXLogSystemStrings = class(TPHXLogHandler)
  private
    FLines: TStrings;
  public
    constructor Create(const ALines: TStrings);
    destructor Destroy; override;

    // Log a debug message
    procedure Debug(const Source: String; const Text: String); override;
    // Log an information message
    procedure Info(const Source: String; const Text: String); override;
    // Log a warning
    procedure Warn(const Source: String; const Text: String); override;
    // Log a debug message
    procedure Error(const Source: String; const Text: String); override;
  end;

{$ENDREGION}

// Logging framework
//------------------------------------------------------------------------------
TPHXLogger = class
  public
    // Remove and free all log handlers
    class procedure Clear;

    // Add a log handler to the logging framework
    class function Add(Handler: TPHXLogHandler): TPHXLogHandler;
    // Add a text handler to the logger
    class function AddText: TPHXLogHandlerText overload;
    // Add a text handler to the logger
    class function AddText(const ALogName: String): TPHXLogHandlerText overload;
  public
    // Log a debug message
    class procedure Debug(const Source: String; const Text: String); overload;
    // Log a debug message
    class procedure Debug(const Source: String; const Text: String; Args: array of const); overload;

    // Log an information message
    class procedure Info(const Source: String; const Text: String); overload;
    // Log an information message
    class procedure Info(const Source: String; const Text: String; Args: array of const); overload;

    // Log a warning
    class procedure Warn(const Source: String; const Text: String); overload;
    // Log a warning
    class procedure Warn(const Source: String; const Text: String; Args: array of const); overload;

    // Log a debug message
    class procedure Error(const Source: String; const Text: String); overload;
    // Log a debug message
    class procedure Error(const Source: String; const Text: String; Args: array of const); overload;
  end;

implementation

//------------------------------------------------------------------------------
const LogLevelName : array[TPHXLogLevel] of String = (
  'Debug',
  'Info',
  'Warn',
  'Error'
);

{$IFDEF FPC}
  {
//------------------------------------------------------------------------------
procedure LogUnhandledException(Obj : TObject; Addr, ExceptAddr: Pointer);
begin
  // BackTraceStrFunc(ExceptAddr)
end;
     }
{$ELSE}

//------------------------------------------------------------------------------
procedure LogUnhandledException(ExceptObject: TObject; ExceptAddr: Pointer); far;
var
  Buffer: array[0..1023] of Char;
begin
  ExceptionErrorMessage(ExceptObject, ExceptAddr, Buffer, SizeOf(Buffer));
  {$IFDEF WINDOWS}
  CharToOemA(Buffer, Buffer); //Necessary ANSI Conversion
  {$ENDIF}

  TPHXLogger.Error('EException', Buffer);

  Halt(1);
end;

{$ENDIF}


{$REGION 'TPHXLogHandlerEvent'}

// TPHXLogHandlerEvent
//==============================================================================
constructor TPHXLogHandlerEvent.Create;
begin

end;


//------------------------------------------------------------------------------
destructor TPHXLogHandlerEvent.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerEvent.Debug(const Source: String; const Text: String);
begin
  if Assigned(OnDebug) then OnDebug(Source, Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerEvent.Info(const Source: String; const Text: String);
begin
  if Assigned(OnInfo) then OnInfo(Source, Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerEvent.Warn(const Source: String; const Text: String);
begin
  if Assigned(OnWarn) then OnWarn(Source, Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerEvent.Error(const Source: String; const Text: String);
begin
  if Assigned(OnError) then OnError(Source, Text);
end;

{$ENDREGION}

{$REGION 'TPHXLogHandlerText'}

// TPHXLogText
//==============================================================================
constructor TPHXLogHandlerText.Create(const ALogName: String);
begin
  FLogName:= ALogName;

  Open();
end;

//------------------------------------------------------------------------------
destructor TPHXLogHandlerText.Destroy;
begin
  if TTextRec(FLogFile).Handle <> 0 then Close;
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerText.Close;
begin
  CloseFile(FLogFile);

  TTextRec(FLogFile).Handle:=0;
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerText.Open;
begin
  if TTextRec(FLogFile).Handle <> 0 then Close;

  AssignFile(FLogFile, FLogName);

  Rewrite(FLogfile);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerText.Debug(const Source: String; const Text: String);
begin
  WriteLn(FLogFile, '[' + TimeToStr(Now) + '] ' + Source + ': ' + Text);

  Flush(FLogFile);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerText.Info(const Source: String; const Text: String);
begin
  WriteLn(FLogFile, '[' + TimeToStr(Now) + '] ' + Source + ': ' + Text);

  Flush(FLogFile);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerText.Warn(const Source: String; const Text: String);
begin
  WriteLn(FLogFile, '[' + TimeToStr(Now) + '] ' + Source + ': ' + Text);

  Flush(FLogFile);
end;

//------------------------------------------------------------------------------
procedure TPHXLogHandlerText.Error(const Source: String; const Text: String);
begin
  WriteLn(FLogFile, '[' + TimeToStr(Now) + '] ' + Source + ': ' + Text);

  Flush(FLogFile);
end;

{$ENDREGION}

{$REGION 'TPHXLogSystemXML'}


// TPHXLogSystemXML
//==============================================================================
constructor TPHXLogSystemXML.Create(const FileName: String);
begin
  FFileName:= FileName;

  Open();
end;

//------------------------------------------------------------------------------
destructor TPHXLogSystemXML.Destroy;
begin
  if TTextRec(FLogFile).Handle <> 0 then Close;
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Close;
begin
  WriteLn(FLogFile, '</log>');

  CloseFile(FLogFile);

  TTextRec(FLogFile).Handle:=0;
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Open;
begin
  if TTextRec(FLogFile).Handle <> 0 then Close;

  AssignFile(FLogFile, FileName);

  Rewrite(FLogfile);
  WriteLn(FLogFile, '<?xml version="1.0"?>');
  WriteLn(FLogFile, '<?xml-stylesheet type="text/xsl" href="Phoenix.log.xsl"?>');

  WriteLn(FLogFile, '');
  WriteLn(FLogFile, '<log>');
  WriteLn(FLogFile, '<header>');
  WriteLn(FLogFile, '   <version>'    + PHXLOG_VERSION    + '</version>');
  WriteLn(FLogFile, '   <date>'       + DateToStr(Now) + '</date>');
  WriteLn(FLogFile, '   <time>'       + TimeToStr(Now) + '</time>');
  WriteLn(FLogFile, '   <executable>' + ParamStr(0)    + '</executable>');

  {$IFDEF WIN32}
  WriteLn(FLogFile, '   <os>Windows</os>');
  {$ENDIF}
  {$IFDEF LINUX}
  WriteLn(FLogFile, '   <os>Linux</os>');
  {$ENDIF}
  {$IFDEF MAC}
  WriteLn(FLogFile, '   <os>Macintosh</os>');
  {$ENDIF}
 (*
  {$IFDEF FPC}
    {$IFDEF VER1}
    WriteLn(FLogFile, '   <compiler>Free Pascal 1.x.x</compiler>');
    {$ELSIF VER1_0}
    WriteLn(FLogFile, '   <compiler>Free Pascal 1.x.x</compiler>');
    {$ELSE}
    WriteLn(FLogFile, '   <compiler>Free Pascal</compiler>');
    {$ENDIF}
  {$ELSE}
    {$IFDEF VER180}
    WriteLn(FLogFile, '   <compiler>Delphi 2006</compiler>');
    {$ENDIF}

    {$IFDEF VER170}
    WriteLn(FLogFile, '   <compiler>Delphi 2006</compiler>');
    {$ENDIF}

    {$$IFDEF VER160}
    WriteLn(FLogFile, '   <compiler>Delphi 8</compiler>');
    {$ENDIF}

    {$IFDEF VER150}
    WriteLn(FLogFile, '   <compiler>Delphi 7</compiler>');
    {$ENDIF}

    {$IFDEF VER140}
    WriteLn(FLogFile, '   <compiler>Delphi 6</compiler>');
    {$ENDIF}

    {$IFDEF VER130}
    WriteLn(FLogFile, '   <compiler>Delphi 5</compiler>');
    {$ENDIF}
  {$ENDIF}   *)
  WriteLn(FLogFile, '</header>');

  FLine := 0;
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Print(Level: TPHXLogLevel; const Source: String; const Text: String);
var Time: String;
begin
  Time:= DateTimeToStr(Now) ;

  WriteLn(FLogFile, '  <item id="' + IntToStr(Line) + '">');
  WriteLn(FLogFile, '    <time>'      + Time                + '</time>');
  WriteLn(FLogFile, '    <level>'     + LogLevelName[Level] + '</level>');
  WriteLn(FLogFile, '    <source>'    + Source              + '</source>');
  WriteLn(FLogFile, '    <message>'   + Text                + '</message>');
  WriteLn(FLogFile, '  </item>');

  Flush(FLogFile);

  Inc(FLine);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.SetFileName(const Value: String);
begin
  if FileName <> Value then
  begin
    Close;
    FFileName := Value;
    Open;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Debug(const Source: String; const Text: String);
begin
  Print(logDebug, Source, Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Info(const Source: String; const Text: String);
begin
  Print(logInfo, Source, Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Warn(const Source: String; const Text: String);
begin
  Print(logWarn, Source, Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemXML.Error(const Source: String; const Text: String);
begin
  Print(logError, Source, Text);
end;

{$ENDREGION}

{$REGION 'TPHXLogSystemConsole'}

// TPHXConsoleLogger
//==============================================================================
procedure TPHXLogSystemConsole.Debug(const Source: String; const Text: String);
begin
  WriteLn(Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemConsole.Info(const Source: String; const Text: String);
begin
  WriteLn(Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemConsole.Warn(const Source: String; const Text: String);
begin
  WriteLn(Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemConsole.Error(const Source: String; const Text: String);
begin
  WriteLn(Text);
end;

// TPHXLogSystemLines
//==============================================================================
constructor TPHXLogSystemStrings.Create(const ALines: TStrings);
begin
  Assert(Assigned(ALines));

  FLines:= ALines;
end;

//------------------------------------------------------------------------------
destructor TPHXLogSystemStrings.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemStrings.Debug(const Source: String; const Text: String);
begin
  FLines.Add('[Debug]: ' + TimeToStr(Now) + ' ' + Source + ': ' + Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemStrings.Info(const Source: String; const Text: String);
begin
  FLines.Add('[Info]: ' + TimeToStr(Now) + ' ' + Source + ': ' + Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemStrings.Warn(const Source: String; const Text: String);
begin
  FLines.Add('[Warn]: ' + TimeToStr(Now) + ' ' + Source + ': ' + Text);
end;

//------------------------------------------------------------------------------
procedure TPHXLogSystemStrings.Error(const Source: String; const Text: String);
begin
  FLines.Add('[Error]: ' + TimeToStr(Now) + ' ' + Source + ': ' + Text);
end;









var LogHandlers: TList;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Clear;
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Free;
  end;
  LogHandlers.Clear;
end;

//------------------------------------------------------------------------------
class function TPHXLogger.Add(Handler: TPHXLogHandler): TPHXLogHandler;
begin
  Result:= Handler;

  LogHandlers.Add(Result);
end;

//------------------------------------------------------------------------------
class function TPHXLogger.AddText: TPHXLogHandlerText;
var LogName: String;
begin
  LogName:= ChangeFileExt(ParamStr(0), '.log');

  Result:= AddText(LogName);
end;


//------------------------------------------------------------------------------
class function TPHXLogger.AddText(const ALogName: String): TPHXLogHandlerText;
begin
  Result:= TPHXLogHandlerText.Create(ALogName);

  LogHandlers.Add(Result);
end;


//------------------------------------------------------------------------------
class procedure TPHXLogger.Debug(const Source: String; const Text: String);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  {$IFDEF PHX_VERBOSE_LOGGING}
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Debug(Source, Text);
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Debug(const Source: String; const Text: String; Args: array of const);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  {$IFDEF PHX_VERBOSE_LOGGING}
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Debug(Source, Format(Text, Args) );
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Info(const Source: String; const Text: String);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  {$IFDEF PHX_VERBOSE_LOGGING}
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Info(Source, Text);
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Info(const Source: String; const Text: String; Args: array of const);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  {$IFDEF PHX_VERBOSE_LOGGING}
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Info(Source, Format(Text, Args) );
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Warn(const Source: String; const Text: String);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Warn(Source, Text);
  end;
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Warn(const Source: String; const Text: String; Args: array of const);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Warn( Source, Format(Text, Args) );
  end;
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Error(const Source: String; const Text: String);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Error(Source, Text);
  end;
end;

//------------------------------------------------------------------------------
class procedure TPHXLogger.Error(const Source: String; const Text: String; Args: array of const);
var Index  : Integer;
var Handler: TPHXLogHandler;
begin
  for Index:=0 to LogHandlers.Count - 1 do
  begin
    Handler:= TPHXLogHandler(LogHandlers.List[Index]);

    Handler.Error( Source, Format(Text, Args) );
  end;
end;































initialization
  LogHandlers:= TList.Create;
finalization
  // Remove all handlers
  TPHXLogger.Clear;

  LogHandlers.Free;

 // CLogSystem:= TPHXLogSystemText.Create( ChangeFileExt(ParamStr(0), '.log'));
 // {$IFNDEF FPC}
  // This wraps delphi exceptions to the logger
//  ExceptProc := @LogUnhandledException;
 // {$ENDIF}
end.

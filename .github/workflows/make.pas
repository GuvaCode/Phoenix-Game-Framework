//https://castle-engine.io/modern_pascal

program Make;
{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  FileUtil,
  Zipper,
  fphttpclient,
  RegExpr,
  openssl,
  LazUTF8,
  opensslsockets,
  eventlog,
  Process;

  function OutLog(const Knd: TEventType; const Msg: string): string;
  begin
    case Knd of
      etError: Result := #27'[31m%s'#27'[0m';
      etInfo:  Result := #27'[32m%s'#27'[0m';
      etDebug: Result := #27'[33m%s'#27'[0m';
    end;
    Writeln(stderr, UTF8ToConsole(Result.Format([Msg])));
  end;

  function AddPackage(const Path: string): string;
  begin
    if RunCommand('lazbuild', ['--add-package-link', Path], Result, [poStderrToOutPut]) then
      OutLog(etDebug, 'Add package:'#9 + Path)
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result);
    end;
  end;

  function SelectString(const Input, Reg: string): string;
  var
    Line: string;
  begin
    Result := EmptyStr;
    with TRegExpr.Create do
    begin
      Expression := Reg;
      for Line in Input.Split(LineEnding) do
        if Exec(Line) then
          Result += Line + LineEnding;
      Free;
    end;
  end;

  function RunTest(const Path: String): string;
  begin
    OutLog(etDebug, #9'run:'#9 + Path);
    if RunCommand(Path, ['--all', '--format=plain'], Result, [poStderrToOutPut]) then
      OutLog(etInfo, #9'success!')
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result);
    end;
  end;

  function AddDDL(const Path: String): string;
  begin
    OutLog(etDebug, #9'add:'#9 + Path);
    if RunCommand('sudo', ['bash', '-c', 'cp %s /usr/lib/; ldconfig --verbose'.Format([Path])], Result, [poStderrToOutPut]) then
      OutLog(etInfo, #9'success!')
    else
    begin
      ExitCode += 1;
      OutLog(etError, Result);
    end;
  end;

  function BuildProject(const Path: string): string;
  begin
    OutLog(etDebug, 'Build from:'#9 + Path);
    if RunCommand('lazbuild',
      ['--build-all', '--recursive', '--no-write-project', Path], Result, [poStderrToOutPut]) then
    begin
      Result := SelectString(Result, 'Linking').Split(' ')[2].Replace(LineEnding, EmptyStr);
      OutLog(etInfo, #9'to:'#9 + Result);
      if ReadFileToString(Path.Replace('.lpi', '.lpr')).Contains('consoletestrunner') then
        RunTest(Result)
      else if ReadFileToString(Path.Replace('.lpi', '.lpr')).Contains('exports') then
        AddDDL(Result)
    end
    else
    begin
      ExitCode += 1;
      OutLog(etError, SelectString(Result, '(Fatal|Error):'));
    end;
  end;

  function DownloadFile(const Uri: string): string;
  var
    OutFile: TStream;
  begin
    InitSSLInterface;
    Result := GetTempFileName;
    OutFile := TFileStream.Create(Result, fmCreate or fmOpenWrite);
    with TFPHttpClient.Create(nil) do
    begin
      try
        AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
        AllowRedirect := True;
        Get(Uri, OutFile);
        OutLog(etDebug, 'Download from %s to %s'.Format([Uri, Result]));
      finally
        Free;
        OutFile.Free;
      end;
    end;
  end;

  procedure UnZip(const ZipFile, ZipPath: string);
  begin
    with TUnZipper.Create do
    begin
      try
        FileName := ZipFile;
        OutputPath := ZipPath;
        Examine;
        UnZipAllFiles;
        OutLog(etDebug, 'Unzip from'#9 + ZipFile + #9'to'#9 + ZipPath);
        DeleteFile(ZipFile);
      finally
        Free;
      end;
    end;
  end;

  function InstallOPM(const Path: string): string;
  begin
    Result :=
      {$IFDEF MSWINDOWS}
      GetEnvironmentVariable('APPDATA') + '\.lazarus\onlinepackagemanager\packages\'
      {$ELSE}
      GetEnvironmentVariable('HOME') + '/.lazarus/onlinepackagemanager/packages/'
      {$ENDIF}
      + Path;
    if not DirectoryExists(Result) then
    begin
      if ForceDirectories(Result) then
        UnZip(DownloadFile('https://packages.lazarus-ide.org/%s.zip'.Format([Path])), Result);
    end;
  end;

  function BuildAll(const Target: string; const Dependencies: array of string): string;
  var
    List: TStringList;
    DT: TDateTime;
  begin
    DT := Time;
    if FileExists('.gitmodules') then
      if RunCommand('git', ['submodule', 'update', '--init', '--recursive',
        '--force', '--remote'], Result, [poStderrToOutPut]) then
        OutLog(etInfo, Result)
      else
      begin
        ExitCode += 1;
        OutLog(etError, Result);
      end;
    List := FindAllFiles(GetCurrentDir, '*.lpk');
    try
      for Result in Dependencies do
        List.AddStrings(FindAllFiles(InstallOPM(Result), '*.lpk'));
      for Result in List do
        AddPackage(Result);
      List := FindAllFiles(Target, '*.lpi');
      List.Sort;
      for Result in List do
        BuildProject(Result);
    finally
      List.Free;
    end;
    if not RunCommand('delp', ['-r', GetCurrentDir], Result, [poStderrToOutPut]) then
      OutLog(etError, Result);
    OutLog(etDebug, 'Duration:'#9 + FormatDateTime('hh:nn:ss', DT - Time));
  end;

begin
  try
    BuildAll('demos', []);
    case ExitCode of
      0: OutLog(etInfo, 'Errors:'#9 + ExitCode.ToString);
      else
        OutLog(etError, 'Errors:'#9 + ExitCode.ToString);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, #9, E.Message);
  end;
end.

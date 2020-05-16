unit pgf_descriprors;


 {$I phxConfig.inc}
interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, Controls, Forms;

type
    { TPGFApplicationDescriptor }
    TPGFApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

    { TPGFFileUnit }
    TPGFFileUnit = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetUnitDirectives: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetInterfaceSource(const {%H-}aFilename, {%H-}aSourceName,{%H-}aResourceName: string): string; override;
    end;

const LE = #10;

   procedure Register;

implementation

procedure Register;
begin
  RegisterProjectFileDescriptor(TPGFFileUnit.Create,FileDescGroupName);
  RegisterProjectDescriptor(TPGFApplicationDescriptor.Create);
end;

function FileDescriptorByName() : TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName('PGF Unit');
end;

{ TPGFFileUnit }

constructor TPGFFileUnit.Create;
begin
  inherited Create;
  Name:='PGF Unit';
  UseCreateFormStatements:=False;
end;

function TPGFFileUnit.GetInterfaceUsesSection: string;
begin
  Result:=
'SysUtils,'+LE+
'  phxLogger,'+LE+
'  phxTypes,'+LE+
'  phxEvents,'+LE+
'  phxMath,'+LE+
'  phxApplication,'+LE+
'  phxDevice,'+LE+
'  phxCanvas,'+LE+
'  phxTexture'
end;

function TPGFFileUnit.GetUnitDirectives: string;
begin
   result := '{$mode delphi}{$H+}'
end;

function TPGFFileUnit.GetImplementationSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:=
  'uses'+LE+
  ' phxOpenGL_GLFW3,'+LE+
   {$IFDEF PHX_VAMPIRE}
  ' phxGraphics_Vampyre;'+LE+
   {$ELSE}
  ' phxGraphics_FreeImage;'+LE+
   {$ENDIF}
  ''+LE+
  'constructor TGame.Create;'+LE+
  'begin'+LE+
  '  inherited;'+LE+
  ''+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Init;'+LE+
  'begin'+LE+
  '  // Create the device using the default provider'+LE+
  '  Device:= TPHXDevice.Create;'+LE+
  '  // Initialize the device'+LE+
  '  Device.Initialize(''Phoenix Demo'', 800, 600);'+LE+
  '  // Create the canvas'+LE+
  '  Canvas:= Device.CreateCanvas;'+LE+
  '  // Create the timer'+LE+
  '  Timer:= TPHXTimer.Create;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Update;'+LE+
  'begin'+LE+
  '  Timer.Update;'+LE+
  '  Device.Update;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Render;'+LE+
  'begin'+LE+
  '  Device.Clear;'+LE+
  '  Canvas.Flush;'+LE+
  ''+LE+
  '  Device.Flip;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.Shutdown;'+LE+
  'begin'+LE+
  '  Timer.Free;'+LE+
  '  Canvas.Free;'+LE+
  '  Device.Free;'+LE+
  'end;'+LE+
  ''+LE+
  'procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);'+LE+
  'begin'+LE+
  '  inherited;'+LE+
  '  // Terminate the application with esc'+LE+
  '  if Key = VK_ESC then'+LE+
  '  begin'+LE+
  '    Terminate;'+LE+
  '  end;'+LE+
  'end;'+LE+''+LE;
end;

function TPGFFileUnit.GetInterfaceSource(const aFilename, aSourceName,
  aResourceName: string): string;
begin
 Result:=
'type'+LE+
'TGame = class(TPHXApplication)'+LE+
'  private'+LE+
'    Device : TPHXDevice;'+LE+
'    Timer  : TPHXTimer;'+LE+
'    Canvas : TPHXCanvas;'+LE+
'  protected'+LE+
'    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;'+LE+
'  public'+LE+
'    constructor Create; override;'+LE+
'    procedure Init; override;'+LE+
'    procedure Update; override;'+LE+
'    procedure Render; override;'+LE+
'    procedure Shutdown; override;'+LE+
'  end;'
end;

{ TPGFApplicationDescriptor }

constructor TPGFApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'Phoenix Game Framework Blank Application';
end;

function TPGFApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'Phoenix Game Framework Blank Application';
end;

function TPGFApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result := 'Phoenix Game Framework Blank Application'+LE+LE
           +'The Phoenix Game Framework is a set of classes'+LE+
           'for helping in the creation of 2D and 3D games in pascal.';
end;

function TPGFApplicationDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('Game.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.LoadDefaultIcon;
  AProject.LazCompilerOptions.SyntaxMode:='Delphi';

   // create program source
  NewSource:=
  'program Game1;'+LE+
   ''+LE+
  'uses'+LE+
  '   SysUtils;'+LE+
  ''+LE+
  ''+LE+
  'var Game: TGame;'+LE+
  ''+LE+
  'begin'+LE+
  '  Game:= TGame.Create;'+LE+
  '  Game.Run;'+LE+
  '  Game.Free;'+LE+
  'end.'+LE;

  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('PGF');
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='Game';

end;

function TPGFApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorByName,'','',[nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

end.


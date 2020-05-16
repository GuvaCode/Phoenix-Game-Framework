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
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
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
  Result:= LineEnding+
  ' SysUtils,'                                         +LineEnding+
  ' phxTypes,'                                         +LineEnding+
  ' phxClasses,'                                       +LineEnding+
  ' phxMath,'                                          +LineEnding+
  ' phxDevice,'                                        +LineEnding+
  ' phxApplication,'                                   +LineEnding+
  ' phxGraphics,'                                      +LineEnding+
  ' phxCanvas,'                                        +LineEnding+
  {$IFDEF phx_test}
  ' phxTESTFILE,'                                       +LineEnding+
  {$ENDIF}
  ' phxTexture'                                        +LineEnding;
end;

function TPGFFileUnit.GetLocalizedName: string;
begin
  Result:='Phoenix Game Framework blank unit';
end;

function TPGFFileUnit.GetLocalizedDescription: string;
begin
  Result:='Create a new phoenix game framework blank unit';
end;

function TPGFFileUnit.GetUnitDirectives: string;
begin
   result := '{$mode delphi}{$H+}'
end;

function TPGFFileUnit.GetImplementationSource(const Filename, SourceName,
  ResourceName: string): string;
begin
{$I mincode.inc}
end;

function TPGFFileUnit.GetInterfaceSource(const aFilename, aSourceName,
  aResourceName: string): string;
var Source:String;
begin
Source:='type'                                      +LineEnding+LineEnding+
 'TGame = class(TPHXApplication)'                   +LineEnding+
 '  private'                                        +LineEnding+
 '    Device : TPHXDevice;'                       +LineEnding+
 '    Canvas : TPHXCanvas;'                       +LineEnding+
 '  public'                                         +LineEnding+
 '    procedure Init; override;'                    +LineEnding+
 '    procedure Update; override;'                  +LineEnding+
 '    procedure Render; override;'                  +LineEnding+
 '    procedure Shutdown; override;'                +LineEnding+
 '  end;'                                           +LineEnding+LineEnding;
  Result:=Source;
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

  MainFile:=AProject.CreateProjectFile('myGame.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.LoadDefaultIcon;
  AProject.LazCompilerOptions.SyntaxMode:='Delphi';

   // create program source
  {$I prjbody.inc}
  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.AddPackageDependency('FCL');
  AProject.AddPackageDependency('pgf');
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='Game';
end;

function TPGFApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorByName,'','',[nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

end.


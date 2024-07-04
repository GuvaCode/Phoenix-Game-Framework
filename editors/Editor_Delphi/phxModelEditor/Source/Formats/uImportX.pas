unit uImportX;

interface

uses
  Windows, Messages, SysUtils, Classes,

  uImport,

  phxTypes,
  phxMath,
  phxModel;

type

// http://msdn.microsoft.com/en-us/library/bb173014(VS.85).aspx
//----------------------------------------------------------------------------
TImporterX = class(TImporter)
  private
  public
    constructor Create; override;
    destructor Destroy; override;

    Procedure LoadFromStream(Stream : TStream);

    Procedure ImportMesh(Stream: TStream); override;

    procedure CopyModel(Mesh: TPHXMesh);
  end;

implementation


{ TImporterX }
//------------------------------------------------------------------------------
constructor TImporterX.Create;
begin
  inherited;
end;

//------------------------------------------------------------------------------
destructor TImporterX.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TImporterX.CopyModel(Mesh: TPHXMesh);
begin

end;

procedure TImporterX.ImportMesh(Stream: TStream);
begin
  inherited;

end;

procedure TImporterX.LoadFromStream(Stream: TStream);
begin

end;


initialization
  TImporterFactory.Add('DirectX file', '.x', TImporterX);
end.

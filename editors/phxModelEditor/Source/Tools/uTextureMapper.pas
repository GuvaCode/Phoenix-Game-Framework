unit uTextureMapper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  phxTypes,

  phxDevice,
  phxModel,
  phxTexture,
  phxGraphics,
  phxGraphicsEx;


type
  TFrmTextureMapper = class(TForm)
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
  private
    FMesh   : TPHXMesh;
    FTexture: TBitmap;

    function CoordToScreen(const Position: TVector2f): TPoint;

    procedure SetMesh(const Value: TPHXMesh);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    function Execute: Boolean;
    // The particle effect for the document
    property Mesh: TPHXMesh read FMesh write SetMesh;
  end;

var
  FrmTextureMapper: TFrmTextureMapper;

implementation

{$R *.dfm}

uses uActions;

// TFrmTextureMapper
//==============================================================================
constructor TFrmTextureMapper.Create(AOwner: TComponent);
begin
  inherited;

  FTexture:= TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor TFrmTextureMapper.Destroy;
begin
  FTexture.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TFrmTextureMapper.Execute: Boolean;
begin
  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmTextureMapper.SetMesh(const Value: TPHXMesh);
var Texture: TPHXTexture;
begin
  FMesh := Value;

  if Assigned(FMesh) then
  begin
      Texture:= ModActions.FindTexture(Mesh.Material.Textures.Find(tkDiffuse));

      if Assigned(Texture) then
      begin
        Texture.ToBitmap(FTexture);
      end;

  end;
end;

//------------------------------------------------------------------------------
function TFrmTextureMapper.CoordToScreen(const Position: TVector2f): TPoint;
begin
  Result.X:= Round(Position.X * FTexture.Width);
  Result.Y:= Round(Position.Y * FTexture.Height);
end;

//------------------------------------------------------------------------------
procedure TFrmTextureMapper.PaintBox1Paint(Sender: TObject);
var Index: Integer;
var T0: TVector2f;
var T1: TVector2f;
var T2: TVector2f;
var P0: TPoint;
var P1: TPoint;
var P2: TPoint;
begin
  with PaintBox1.Canvas do
  begin
    // Draw the texture
    Draw(0,0, FTexture);


    // Draw the texture coordinates
    for Index := 0 to Mesh.Triangles.Count - 1 do
    begin
      T0:= Mesh.Vertices.List^[Mesh.Triangles.List^[Index].Vertex0].TexCoord0;
      T1:= Mesh.Vertices.List^[Mesh.Triangles.List^[Index].Vertex1].TexCoord0;
      T2:= Mesh.Vertices.List^[Mesh.Triangles.List^[Index].Vertex2].TexCoord0;

      P0:= CoordToScreen(T0);
      P1:= CoordToScreen(T1);
      P2:= CoordToScreen(T2);

      Pen.Color:= clRed;

      Polyline( [P0, P1, P2, P0]);

    end;
  end;
end;


end.

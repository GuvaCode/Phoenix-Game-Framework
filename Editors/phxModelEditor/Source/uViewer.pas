unit uViewer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  phxTypes,
  phxMath,
  phxClasses,
  phxCamera,
  phxGraphics,
  phxCanvas,
  phxDraw,
  phxTexture,
  phxModel,
  phxEffect,
  phxTimer,
  phxPrimitives;

type

//------------------------------------------------------------------------------
TFrmViewer = class(TFrame)
    PHXDraw1: TPHXDraw;
    procedure PHXDraw1Init(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1Update(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1Render(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
    procedure PHXDraw1MouseWheel(Sender: TObject; Shift: TShiftState;  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PHXDraw1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PHXDraw1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FMesh: TPHXMesh;
    FKeys: array[Word] of Boolean;

    MouseX: Single;
    MouseY: Single;

    Camera: TPHXEditorCamera;
    Timer   : TPHXTimer;

    FEffect: TPHXEffect;
    FBuffer : TPHXBuffer;
    FPrimitives: TPHXPrimitives;

    TextureTag: TPHXTexture;

    procedure SetMesh(const Value: TPHXMesh);
    procedure RenderMesh(Device: TPHXDevice);
    procedure RenderTags(Device: TPHXDevice);
  protected
    procedure SetParent(AParent: TWinControl); override;

    //Geometry: TPHXBuffer;


   // Rotation: Single;
   // Distance: Single;
   // Matrix  : TMatrix4f;
  public
    procedure SetFocus; override;
    property Mesh: TPHXMesh read FMesh write SetMesh;

    property Effect: TPHXEffect read FEffect;
    property Buffer: TPHXBuffer read FBuffer;
    property Primitives: TPHXPrimitives read FPrimitives;
  end;

implementation

{$R *.dfm}

uses
  uActions,
  uMain;

const TagTexture : {$I Tag.inc}

(*

  Geometry:= PHXDraw1.Device.CreateBuffer;



  TextureJoint:= PHXDraw1.Device.CreateTexture;
//  TextureJoint.LoadTexture(ImageList1, 0);
  TextureJoint.Settings.FilterMin:= tfNearest;
  TextureJoint.Settings.Filtermag:= tfNearest;
  TextureJoint.LoadTexture('Tag.png');
//  TextureJoint.LoadTexture('mUD.bmp');


  Camera:= TPHXEditorCamera.Create(PHXDraw1.Device);

*)
//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1Init(Sender: TObject; Device: TPHXDevice);
begin
  ModActions.Device:= Device;

  FBuffer:= Device.CreateBuffer;
  FEffect:= Device.CreateEffect;

  TextureTag:= Device.CreateTexture;
  TextureTag.Import(32, 32, pfRGBA, @TagTexture);

  FPrimitives:= TPHXPrimitives.Create(Buffer);

  Camera:= TPHXEditorCamera.Create(Device);
  Timer := TPHXTimer.Create;
end;


//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1Update(Sender: TObject; Device: TPHXDevice);
begin
  Timer.Update;

 // case ModActions.Settings.ViewMode of
 //   vmPerspective            :
  //  vmOrthoTop, vmOrthoBottom:
 //   vmOrthoY: ;
 //   vmOrthoZ: ;
 // end;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1Render(Sender: TObject; Device: TPHXDevice);
begin
  Device.Clear;
  Device.SetBlending(bmAlpha);
  Device.SetDepthTest(True);

  Effect.Projection:= Camera.Projection;
  Effect.View      := Camera.View;
  Effect.World     := Matrix_CreateRotationY(Timer.ElapsedTime * 45);
  Effect.World     := Matrix4_Identity;
  Effect.Texture   := nil;


  if Assigned(FMesh) then
  begin
    RenderMesh(Device);

    if FrmMain.PageControl1.ActivePage = FrmMain.TabTags then
    begin
      RenderTags(Device);
    end;
  end;

  if ModActions.Settings.ShowGrid then
  begin
    Effect.Texture:= nil;

    Primitives.GridY(Vector3f_Zero, 100, 1, 10);
       (*
    case ModActions.Settings.ViewMode of
      vmPerspective            :  Primitives.GridY(Vector3f_Zero, 100, 1, 10);
      vmOrthoTop, vmOrthoBottom:  Primitives.GridX(Vector3f_Zero, 100, 1, 10);
      vmOrthoY: ;
      vmOrthoZ: ;
    end;
    *)
    Buffer.Upload;

    Effect.Render(Buffer);
    Buffer.Clear;
  end;


 // Effect.View      := Camera.View;
 // Effect.World     := Matrix_CreateTranslation(20, 0 , 0) * Matrix_CreateRotationY(Timer.ElapsedTime * 45);
 // Effect.Projection:= Camera.Projection;
 // Effect.Texture   := Texture;
 // Effect.Render(Buffer);

//  Device.SetDepthTest(False);

  //Fonts[0].TextOut(0, 0, Format('%d fps', [Timer.FrameRate]));

 // Canvas.Texture:= nil;
 // Canvas.FilledRectangle(100, 100, 200, 200);

 // Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.RenderMesh(Device: TPHXDevice);
var Texture: String;
begin
  Texture:= Mesh.Material.Textures.Find(tkDiffuse);

  Effect.Texture:= ModActions.FindTexture(Texture);

  Mesh.Render(Buffer);

  Buffer.Upload;
  if ModActions.Settings.ShowWireframe then
  begin
    Device.SetWireFrame(True);
    Effect.Render(Buffer);
    Device.SetWireFrame(False);
  end else
  begin
    Effect.Render(Buffer);
  end;
  Buffer.Clear;


  // Draw normals
  if ModActions.Settings.ShowNormals then
  begin
    Mesh.RenderNormals(Buffer, 1.0, clrYellow);
    Buffer.Upload;

    Effect.Texture:= nil;
    Effect.Render(Buffer);

    Buffer.Clear;
  end;

  // Draw bounding box
  if ModActions.Settings.ShowBounds then
  begin
  //  PHXDraw1.Device.SetTexture(nil);
  //  Geometry.Color:= clrMaroon;
   // Geometry.BoundingBox(Mesh.Bounds);
    Primitives.Color:= clrMaroon;
    Primitives.BoundingBox(Mesh.Bounds);

    Buffer.Upload;

    Effect.Texture:= nil;
    Effect.Render(Buffer);

    Buffer.Clear;
  end;

  Primitives.Origin(TVector3f.Create(0, 10, 0), 1);
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.RenderTags(Device: TPHXDevice);
var Index: Integer;
var Tag  : TPHXMeshTag;
var Normal: TVector3f;
begin
  for Index := 0 to Mesh.Tags.Count - 1 do
  begin
    Tag:= Mesh.Tags.List^[Index];

    if Index = FrmMain.FrmTags.Selected then
    begin
      Primitives.Color:= clrYellow;
    end else
    begin
      Primitives.Color:= clrWhite;
    end;

    Primitives.Cube(Tag.Position, TVector3f.Create(0.5, 0.5, 0.5));

    Normal:= VectorCross(Tag.Direction, Vector3f_One);

    Primitives.Cube(Tag.Position + Tag.Direction * 0.5, TVector3f.Create(Tag.Direction.X + Normal.X * 0.1, Tag.Direction.Y + Normal.Y * 0.1, Tag.Direction.Z + Normal.Z * 0.1));
  end;
  Buffer.Upload;

  //Device.SetBlending(bmNormal);

  Effect.Texture:= TextureTag;
  Effect.Render(Buffer);

  Buffer.Clear;
end;



//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  FKeys[Key]:= True;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1KeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
  FKeys[Key]:= False;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseX:= X;
  MouseY:= Y;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    Camera.RotationX:= Camera.RotationX + MouseY - Y;
    Camera.RotationY:= Camera.RotationY + MouseX - X;
  end else

  if ssRight in Shift then
  begin

    //StatusBar1.Panels[2].Text:= Format('X: %.2f, Y: %.2f, Z: %.2f', [WorldDelta   .X, WorldDelta   .Y, WorldDelta   .Z]);
  end else
  begin
  end;

  MouseX:= X;
  MouseY:= Y;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.PHXDraw1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.Distance:= Camera.Distance + WheelDelta / 100;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.SetFocus;
begin
  inherited;
   PHXDraw1.SetFocus;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.SetMesh(const Value: TPHXMesh);
var Size  : TVector3f;
var Center:  TVector3f;
begin
  FMesh := Value;

  if Assigned(FMesh) then
  begin
    Size  := Mesh.MeasureSize;
    Center:= Mesh.MeasureCenter;

    Camera.Distance:= 1.25 * Sqrt( Sqr(Size.X) +  Sqr(Size.Y) + Sqr(Size.Z));
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmViewer.SetParent(AParent: TWinControl);
begin
  inherited;

  if Assigned(AParent) then
  begin
    PHXDraw1.Initialize;
  end;
end;











end.

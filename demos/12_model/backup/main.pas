unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  phxTypes,
  phxClasses,
  phxEvents,
  phxMath,
  phxDevice,
  phxGraphics,
  phxCanvas,
  phxApplication,
  phxFont,

  phxTexture,
  phxEffect,

  phxCamera,
  phxModel;

type

{ TGame }

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Buffer : TPHXBuffer;
    Effect : TPHXEffect;
    Texture: TPHXTexture;
    Fonts  : TPHXFontList;
    Mesh   : TPHXMesh;
    Camera : TPHXEditorCamera;
    procedure Test;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
    procedure MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton); override;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

TVertex = record
  Position: TVector3f;
  Normal: TVector3f;
  Texture: TVector2f;
end;

const
  VertexFormat: TVertexComponents = [vcPosition, vcNormal, vcCoord1];

implementation

uses
  phxOpenGL_GLFW3,
  phxGraphics_Vampyre;


constructor TGame.Create;
begin
  inherited;

end;

procedure TGame.Init;
begin
  Test;

   Device:= TPHXDevice.Create;
   Device.Flags:= Device.Flags - [wfVerticalSync];
   Device.Initialize('Phoenix Demo', 1024, 768);;

   Timer:= TPHXTimer.Create;
   Timer.Interval:=0;
   // Create our canvas for 2D rendering
   Canvas:= Device.CreateCanvas;

   // To render 3D modes we need a render buffer and a effect, the render buffer
   // uploads verticies to the GPU while the effect handles the vertex and fragment
   // shaders.
   Buffer:= Device.CreateBuffer;
   Effect:= Device.CreateEffect;

   Fonts := TPHXFontList.Create(Device, Canvas);
   Fonts.LoadFont('data/calibri12.phxfnt');

   Texture:= Device.CreateTexture;
   Texture.LoadTexture('data/Fighter.jpg');

   Mesh := TPHXMesh.Create;
   Mesh.LoadFromFile('data/Fighter.phxmesh');
   Mesh.Upload(Buffer);

   Camera := TPHXEditorCamera.Create(Device);
   Camera.Distance:= 50;
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
 // Camera.RotateLeft(45 * Timer.FrameTime);
end;

procedure TGame.Render;
begin
  Device.Clear;
  Device.SetBlending(bmAlpha);
  Device.SetDepthTest(True);

  Effect.View      := Camera.View;
  Effect.World     := Matrix_CreateRotationX(Timer.ElapsedTime * 45);
  Effect.World     := Matrix_CreateRotationX(50);
  Effect.Projection:=  Camera.Projection;
  Effect.Texture   := Texture;

  Effect.Render(Buffer);

 // Effect.RenderEx(Buffer, TPHXVertex.Declaration);



  Device.SetDepthTest(False);

  Fonts[0].TextOut(0, 0, Format('%d fps', [Timer.FrameRate]));

 // Canvas.Texture:= nil;
 // Canvas.FilledRectangle(100, 100, 200, 200);

  Canvas.Flush;

  Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Timer.Free;
  Texture.Free;
  Canvas.Free;
  Device.Free;
  Buffer.Free;
  Effect.Free;
end;

procedure TGame.Test;
var Decl: TPHXVertexDeclaration;
var V: TVertex;
begin
  Decl:= TPHXVertex.Declaration;

  Decl:= TPHXVertexDeclaration.Create([vcPosition, vcNormal, vcCoord1]);
  Decl.Offsets[vcPosition]:= Integer(@V.Position) - Integer(@V);
  Decl.Offsets[vcNormal  ]:= Integer(@V.Normal  ) - Integer(@V);
  Decl.Offsets[vcCoord1  ]:= Integer(@V.Texture ) - Integer(@V);

  if Decl.Size <> SizeOf(TVertex) then
  begin
    Exit;
  end;
//  Decl.Size;

  if vcPosition in Decl.Components then
  begin
    // glVertexAttribPointer(FAttrib_Position, 3, GL_FLOAT, false, Declaration.Size, Declaration.Offset[vcPosition]);
  end;

end;

procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;
end;

procedure TGame.MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates;
  Button: TPHXMouseButton);
begin
  inherited;
  Effect.LightingEnabled:= not Effect.LightingEnabled;
  Effect.LightPosition:= Vector3f(1, 0, 0);
end;

end.


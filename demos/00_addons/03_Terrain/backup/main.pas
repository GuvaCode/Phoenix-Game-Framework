unit main;

{$mode delphi}{$H+}

interface

uses SysUtils, Classes, Math,



  phxTypes,
  phxEvents,
  phxMath,
  phxDevice,
  phxCanvas,
  phxApplication,
  phxFont,
  phxImage,

  phxGraphics,
  phxTexture,

  phxShape,
  phxSprite,
  phxSpriteTerrain,
 // phxSpriteBox2D
  phxSprite_Box2D;

type


TTriangle = record
  Position: array[0..2] of TVector3f;

  outerline: array[0..1] of Integer;
end;

//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Images : TPHXImageList;
    Fonts : TPHXFontList;

    Texture: TPHXTexture;
    Terrain: TPHXSpriteTerrain;
    Minimap: TPHXTexture;

    Brush  : TPHXTerrainBrush;

    Engine: TPHXPhysicsEngine;


    DownSprite: TPHXSprite;
    DownPosition: TVector2f;
    MovePosition: TVector2f;

    Initialized: Boolean;
    Paused: Boolean;

    DrawPhysics: Boolean;
    DrawDebug  : Boolean;
    procedure DrawFPS;
    procedure DrawMinimap(const X, Y: Single);
  protected

    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;

    procedure MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;
    procedure MouseMoved(X: Integer; Y: Integer; Shift: TPHXShiftStates); override;
    procedure MouseReleased(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;

    function GetContentPath: string; override;
  public
    procedure Init; override;
    procedure Shutdown; override;
    procedure Update; override;
    procedure Render; override;
  end;


implementation

uses
    phxOpenGL_GLFW3,
  phxGraphics_Vampyre;


//------------------------------------------------------------------------------
function TGame.GetContentPath: string;
begin
 // Result:= inherited 'content/Terrain' + PathDelim;
end;

//------------------------------------------------------------------------------
const data: array[0..7, 0..7] of Single =
(
  (0,0,0,0,0,0,0,0),
  (0,1,1,1,1,0,0,0),
  (0,0,0,1,1,0,0,0),
  (0,0,0,0,1,0,0,0),
  (0,1,0,1,1,0,0,0),
  (0,1,1,0,1,0,0,1),
  (0,1,1,1,1,0,0,0),
  (0,0,0,0,0,0,0,0)
);

//------------------------------------------------------------------------------
const explosion: array[0..4, 0..4] of Single =
(
  (0.00, 0.00, 0.10, 0.00, 0.00),
  (0.00, 0.37, 0.55, 0.37, 0.00),
  (0.10, 0.55, 1.00, 0.55, 0.10),
  (0.00, 0.37, 0.55, 0.37, 0.00),
  (0.00, 0.00, 0.10, 0.00, 0.00)
);


// http://wiki.unity3d.com/index.php?title=MarchingSquares
//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 1440, 1000);



  Timer:= TPHXTimer.Create;
  // Create our canvas
  Canvas:= Device.CreateCanvas;


  Texture:= Device.CreateTexture;
  Texture.LoadTexture('content/Terrain/Dirt.png');
  Texture.LoadTexture('content/Terrain/6855.jpg');

  Minimap:= Device.CreateTexture;

  Images:= TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('content/Terrain/Ball.png').Patterns.AddSingle(True);

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('content/Terrain/Calibri12.phxfnt');
  Fonts.LoadFont('content/Terrain/Tahoma.phxfnt');

  Engine:= TPHXPhysicsEngine.Create(Device);
  Engine.Images:= Images;

  Brush:= TPHXTerrainBrush.Create;
  Brush.Import(5, 5, @explosion);
  Brush.Strength:= 0.10;
  Brush.Strength:= 1.00;

//  Threshold:= 0.52;
//  Threshold:= 0.14;
  Engine.Initialize;

  Terrain:= TPHXSpriteTerrain.Create(Engine.PhysicsWorld);
  Terrain.Scale:= Vector2f(16, 16);
  Terrain.Data.Resize(64, 64);
  Terrain.Data.Generate(RandSeed, 0.2);
  Terrain.Data.SaveData('content/Terrain/Test.png');

 // Terrain.Data.LoadData(ContentPath + 'Terrain32.png');
  Terrain.Data.LoadData('content/Terrain/Terrain2_64.png');
  Terrain.Data.LoadData('content/Terrain/Terrain3_64.png');

  Terrain.Data.GenerateTexture(Minimap, clrWhite);


//  Terrain.Data.LoadData(8, 8, @Data);
//  Terrain.Font:= Fonts[1];
  Terrain.Texture:= Texture;
  Terrain.Threshold:= 0.20;

  Initialized:= False;

  Terrain.Initialize;

  Paused:= False;
end;


//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;

  if not Paused then
  begin
    Engine.Update(Timer.FrameTime);
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
const YesNo: array[Boolean] of String = ('No', 'Yes');
begin
  Device.Clear;

  Engine.Render;
//  Engine.RenderBounds(Canvas);
 // Engine.RenderShapes(Canvas);

  Canvas.Texture:= Texture;

  Terrain.Render(Canvas);
  if DrawDebug then
  begin
    Terrain.RenderDebug(Canvas, Fonts[1]);
  end;
  if DrawPhysics then
  begin
    Terrain.RenderPhysics(Canvas);
  end;

  if Assigned(DownSprite) then
  begin
    Engine.RenderBounds(Canvas, clrMaroon);
    //Engine.RenderBounds(Canvas, DownSprite, clrMaroon);
    Canvas.Arrow(DownPosition, MovePosition, 10);
    Canvas.Color:= clrWhite;
  end;

  DrawMinimap(63*16, 0);

 // if not Initialized then
 // begin
//  end;

  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, 'Press Space to draw onto the terrain');

  Fonts[0].FormatText(4, 4 + Fonts[0].Height * 10, 'Threshold: %.2f', [Terrain.Threshold]);
  Fonts[0].FormatText(4, 4 + Fonts[0].Height * 11, 'Draw debug   [Ctrl+D]: %s', [YesNo[DrawDebug]]);
  Fonts[0].FormatText(4, 4 + Fonts[0].Height * 12, 'Draw physics [Ctrl+P]: %s', [YesNo[DrawPhysics]]);



  DrawFPS;

  //  if Paused then
 // begin
 //   Fonts[0].TextOut(70, 0, 'Press [P] to unpause the physics.', clrWhite, clrMaroon);
 // end else
//  begin
 //   Fonts[0].TextOut(70, 0, 'Press [P] to pause the physics.', clrWhite, clrMaroon);
 // end;

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.DrawFPS;
var Text: String;
var X: Integer;
begin
  Text:= Format('%d fps', [Timer.FrameRate]);

  X:= Device.Width - Fonts[0].TextWidth(Text);

  Fonts[0].TextOut(X, 0, Text, clrWhite, clrGray);
end;

//------------------------------------------------------------------------------
procedure TGame.DrawMinimap(const X,Y: Single);
begin
  Canvas.Texture:= Minimap;
  Canvas.FilledRectangle(X, Y, X + 64 * 2, Y + 64 * 2);
end;


//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Canvas.Free;
  Images.Free;
  Fonts.Free;
  Engine.Free;
  Device.Free;
end;

//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
var X,Y: Integer;
begin
  inherited;
  if ssCtrl in Shift then
  begin
    if Key = VK_D then
    begin
      DrawDebug:= not DrawDebug;
    end;
     if Key = VK_P then
    begin
      DrawPhysics:= not DrawPhysics;
    end;

  end else
  begin

    if Key = VK_P then
    begin
      Paused:= not Paused;
    end;
  end;
  {
  if Key = Ord('1') then
  begin
     Terrain.Data.LoadData(ContentPath + 'Terrain32.png');

     Terrain.Initialize(Threshold);
  end;
  if Key = Ord('2') then
  begin
    Terrain.Data.LoadData(8, 8, @Data);
    Terrain.Initialize(Threshold);
  end;    }


  if Key = VK_SPACE then
  begin
   // Terrain.Data[2,2]:= 0;

    X:= Round( MovePosition.X / Terrain.Scale.X) - 2;
    Y:= Round( MovePosition.Y / Terrain.Scale.Y) - 2;
       {
    Rect.Left  := X - 2;
    Rect.Top   := Y - 2;
    Rect.Right := Rect.Left + 5;
    Rect.Bottom:= Rect.Top + 5;

  //  Terrain.Data.Fill(Recti(X,Y,X+4,Y+4), 0);

    Terrain.Data.Draw(X-2,Y-2,5,5, @explosion, 0.10);
    Terrain.Data.GenerateTexture(Minimap, clrWhite);

    Terrain.Invalidate(Rect);  }

    Terrain.Draw(X, Y, Brush);

    Terrain.Data.GenerateTexture(Minimap, clrWhite);
  end;
  {
  if Key = VK_RETURN then
  begin
    Terrain.InitializePhysics(Threshold, Engine.PhysicsWorld);

    Initialized:= True;
  end;
         }

  if Key = VK_NUM_ADD then
  begin
    Terrain.Threshold:= Min(1.0, Terrain.Threshold + 0.01);

    Terrain.Invalidate;

    Terrain.Data.GenerateTexture(Minimap, clrWhite);
  end;
  if Key = VK_NUM_SUBTRACT then
  begin
    Terrain.Threshold:= Max(0.0, Terrain.Threshold - 0.01);

    Terrain.Invalidate;

    Terrain.Data.GenerateTexture(Minimap, clrWhite);
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.MouseMoved(X, Y: Integer; Shift: TPHXShiftStates);
begin
  inherited;
  MovePosition:= Vector2f(X, Y);
end;

//------------------------------------------------------------------------------
procedure TGame.MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
var Sprite: TPHXPhysicsSprite;
var Shape: TPHXCircle;
begin
  inherited;

  if Button = mbLeft then
  begin
    DownSprite:= Engine.SpriteAt(X,Y);
    DownPosition:= Vector2f(X, Y);
  end;
  if Button = mbRight then
  begin
    Shape:= TPHXCircle.Create(7);
    try
      Sprite:= TPHXPhysicsSprite.Create(Engine);
      Sprite.X          := X;
      Sprite.Y          := Y;
      Sprite.Image      := Images[0].Name;
      Sprite.Pattern    := Images[0].Name;
      Sprite.LinkedShape:= Shape;
      Sprite.Parent     := Engine.Root;
      Sprite.Kind       := bkDynamic;
      Sprite.Restitution:= 0.5;
      Sprite.Friction   := 0.8;

      Sprite.Initialize;
    finally
      Shape.Free;
    end;
  end;

 end;

//------------------------------------------------------------------------------
procedure TGame.MouseReleased(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
var Impulse: TVector2d;
begin
  inherited;

  if Assigned(DownSprite) and (DownSprite is TPHXPhysicsSprite) then
  begin
    Impulse.X:= (X - DownPosition.X) * 0.10;
    Impulse.Y:= (Y - DownPosition.Y) * 0.10;

   // Impulse:= VectorMul( VectorNormalize(Impulse), 100000);

    TPHXPhysicsSprite(DownSprite).ApplyImpulse(Impulse, Vector2f(X,Y));
    //(Vector2f(X,Y), Impulse);
  end;

  DownSprite:= nil;

  inherited;
end;

end.

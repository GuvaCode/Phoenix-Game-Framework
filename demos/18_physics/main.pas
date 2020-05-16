unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes,
  phxTypes,
  phxEvents,
  phxMath,
  phxDevice,
  phxCanvas,
  phxApplication,
  phxFont,
  phxImage,

  phxShape,
  phxSprite,
  phxSprite_Box2D;

type

{ TGame }

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Images : TPHXImageList;
    Fonts : TPHXFontList;

    Paused: Boolean;
    Engine: TPHXPhysicsEngine;
    Sprite: TPHXPhysicsSprite;

    DownSprite: TPHXSprite;
    DownPosition: TVector2f;
    MovePosition: TVector2f;

    Sprites: TList;
    procedure CreateWalls;
    procedure CreateSprites;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
    procedure MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;
    procedure MouseMoved(X: Integer; Y: Integer; Shift: TPHXShiftStates); override;
    procedure MouseReleased(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;


implementation

uses
  phxOpenGL_GLFW3,
  phxGraphics_FreeImage;

constructor TGame.Create;
begin
  inherited;

end;

procedure TGame.Init;
var Shape: TPHXCircle;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;

  Timer:= TPHXTimer.Create;
  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Images:= TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('data/physics/Crate.png' ).Patterns.AddSingle(True);
  Images.LoadImage('data/physics/Barrel.png').Patterns.AddSingle(True);
  Images.LoadImage('data/physics/Grass.png').Patterns.AddSingle(True);
  Images.LoadImage('data/physics/Brick.png').Patterns.AddSingle(True);
  Images.LoadImage('data/physics/Ball.png').Patterns.AddSingle(True);

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/calibri12.phxfnt');

  Engine:= TPHXPhysicsEngine.Create(Device);
  Engine.Images:= Images;

  Sprites:= TList.Create;

  CreateSprites;
  CreateWalls;

  Shape:= TPHXCircle.Create(28);
  try
    Sprite:= TPHXPhysicsSprite.Create(Engine);
    Sprite.Image  := Images[1].Name;
    Sprite.Pattern:= Images[1].Name;
    Sprite.LinkedShape  := Shape;
    Sprite.X      := 100;
    Sprite.Y      := 300;
    Sprite.Parent := Engine.Root;
    Sprite.Kind   := bkStatic;

    Sprite:= TPHXPhysicsSprite.Create(Engine);
    Sprite.Image  := Images[1].Name;
    Sprite.Pattern:= Images[1].Name;
    Sprite.LinkedShape  := Shape;
    Sprite.X      := 200;
    Sprite.Y      := 400;
    Sprite.Parent := Engine.Root;
    Sprite.Kind   := bkStatic;

    Sprite:= TPHXPhysicsSprite.Create(Engine);
    Sprite.Image  := Images[1].Name;
    Sprite.Pattern:= Images[1].Name;
    Sprite.LinkedShape  := Shape;
    Sprite.X      := 300;
    Sprite.Y      := 400;
    Sprite.Parent := Engine.Root;
    Sprite.Kind   := bkStatic;

  finally
    Shape.Free;
  end;
  Engine.Initialize;

  Paused:= true;
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
    if not Paused then
  begin
    Engine.Update(Timer.FrameTime);
  end;
end;

procedure TGame.Render;
begin
  Device.Clear;

   Engine.Render;
 //  Engine.RenderBounds(Canvas);
  // Engine.RenderShapes(Canvas);

   if Assigned(DownSprite) then
   begin
     DownSprite.RenderBounds(Canvas, clrMaroon);

 //    Canvas.Line(MovePosition, DownPosition);
     Canvas.Arrow(DownPosition, MovePosition, 10);

     Canvas.Color:= clrWhite;
   end;

   if Paused then
   begin
     Fonts[0].TextOut(70, 0, 'Press [P] to unpause the physics.', clrWhite, clrMaroon);
   end else
   begin
     Fonts[0].TextOut(70, 0, 'Press [P] to pause the physics.', clrWhite, clrMaroon);
   end;

   Fonts[0].TextOut(70, 20, 'Press [R] to reset the physics.', clrWhite, clrMaroon);

   Fonts[0].TextOut(70, 60, 'Select and drag a sprite with the left mouse to apply a impule to a sprite.', clrWhite, clrMaroon);

   Fonts[0].TextOut(700, 0, Format('%d fps', [Timer.FrameRate]), clrWhite, clrGray);

   Canvas.Flush;

   Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Timer.Free;
  Canvas.Free;
  Images.Free;
  Fonts.Free;
  Engine.Free;
  Device.Free;

  Sprites.Free;
end;

procedure TGame.CreateWalls;
var Index: Integer;
var Sprite: TPHXPhysicsSprite;
begin
  // Bottom
  for Index := 0 to 12 do
  begin
    Sprite:= TPHXPhysicsSprite.Create(Engine);
    Sprite.Image  := Images[2].Name;
    Sprite.Pattern:= Images[2].Name;
    Sprite.X      := 16 + Index * Sprite.Width;
    Sprite.Y      := Device.Height-32;
    Sprite.Parent := Engine.Root;
    Sprite.Kind   := bkStatic;

    Sprite.Initialize;
  end;
  // Left
  for Index := 0 to 20 do
  begin
    Sprite:= TPHXPhysicsSprite.Create(Engine);
    Sprite.Image  := Images[3].Name;
    Sprite.Pattern:= Images[3].Name;
    Sprite.X      := 32;
    Sprite.Y      := Device.Height-32 - 64 - Index * Sprite.Height;
    Sprite.Parent := Engine.Root;
    Sprite.Kind   := bkStatic;

    Sprite.Initialize;
  end;
  // Right
  for Index := 0 to 20 do
  begin
    Sprite:= TPHXPhysicsSprite.Create(Engine);
    Sprite.Image  := Images[3].Name;
    Sprite.Pattern:= Images[3].Name;
    Sprite.X      := Device.Width  - 32 ;
    Sprite.Y      := Device.Height - 32 - 64 - Index * Sprite.Height;
    Sprite.Parent := Engine.Root;
    Sprite.Kind   := bkStatic;

    Sprite.Initialize;
  end;

end;

procedure TGame.CreateSprites;
var Index: Integer;
var Sprite: TPHXPhysicsSprite;
var Shape: TPHXCircle;
begin
  for Index := 0 to Sprites.Count - 1 do
  begin
    TPHXPhysicsSprite(Sprites[Index]).Free;
  end;
  Sprites.Clear;


  Shape:= TPHXCircle.Create(15);
  try
    // Create box
    for Index := 0 to 4*8-1 do
    begin
      Sprite:= TPHXPhysicsSprite.Create(Engine);
      Sprite.X      := 32*3 + Index mod 16 * 32;
      Sprite.Y      := 32*3 + Index div 16 * 32;
      Sprite.Image  := Images[0].Name;
      Sprite.Pattern:= Images[0].Name;
      Sprite.Parent := Engine.Root;
      Sprite.Kind   := bkDynamic;

      Sprite.Initialize;

      Sprites.Add(Sprite)
    end;
    // create balls
    for Index := 0 to 5 do
    begin
      Sprite:= TPHXPhysicsSprite.Create(Engine);
      Sprite.X          := 32*3 + 48 * Index;
      Sprite.Y          := 32*1;
      Sprite.Image      := Images[4].Name;
      Sprite.Pattern    := Images[4].Name;
      Sprite.Parent     := Engine.Root;
      Sprite.Kind       := bkDynamic;
      Sprite.Restitution:= 0.5;
      Sprite.Friction   := 0.5;

      Sprite.LinkedShape:= Shape;

      Sprite.Initialize;

      Sprites.Add(Sprite)
    end;

  finally
    Shape.Free;
  end;
 end;

procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
    if Key = VK_P then
  begin
    Paused:= not Paused;
  end;
  if Key = VK_R then
  begin
    CreateSprites;
  end;
  if Key = VK_SPACE then
  begin
    Engine.ApplyExplosion(MovePosition, 500, 200);
  end;
  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;
end;

procedure TGame.MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates;
  Button: TPHXMouseButton);
var Sprite: TPHXPhysicsSprite;
var Shape: TPHXCircle;
begin
  inherited;
  DownSprite:= nil;

  if Button = mbLeft then
  begin
    DownSprite:= Engine.SpriteAt(X,Y);
    DownPosition:= Vector2f(X, Y);
  end else
  if Button = mbRight then
  begin
    Shape:= TPHXCircle.Create(15);
    try
      Sprite:= TPHXPhysicsSprite.Create(Engine);
      Sprite.X          := X;
      Sprite.Y          := Y;
      Sprite.Image      := Images[4].Name;
      Sprite.Pattern    := Images[4].Name;
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

procedure TGame.MouseMoved(X: Integer; Y: Integer; Shift: TPHXShiftStates);
begin
  inherited;
  MovePosition:= Vector2f(X, Y);
end;

procedure TGame.MouseReleased(X: Integer; Y: Integer; Shift: TPHXShiftStates;
  Button: TPHXMouseButton);
var Impulse: TVector2d;
begin
  inherited;
  if Button = mbLeft then
  if Assigned(DownSprite) and (DownSprite is TPHXPhysicsSprite) then
  begin
    Impulse.X:= (X - DownPosition.X);// * 1;
    Impulse.Y:= (Y - DownPosition.Y);// * 1;

   // Impulse:= VectorMul( VectorNormalize(Impulse), 100000);

    TPHXPhysicsSprite(DownSprite).ApplyImpulse(Impulse, Vector2f(X,Y));
  end;

  DownSprite:= nil;
end;

end.


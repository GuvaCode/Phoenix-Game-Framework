unit main;

{$mode delphi}{$H+}

interface

uses SysUtils, Classes,

  phxLogger,
  phxTypes,
  phxEvents,
  phxMath,
  phxApplication,
  phxDevice,
  phxCanvas,
  phxInput,
  phxImage,
  phxFont,
  phxShape,
  phxSprite,

  Tiles;

type



// Forward declarations
TPlayer = class;

//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device    : TPHXDevice;
    Timer     : TPHXTimer;
    Input     : TPHXInput;
    Canvas    : TPHXCanvas;
    Fonts     : TPHXFontList;
    Images    : TPHXImageList;
    Animations: TPHXAnimationList;
    Shapes    : TPHXShapeList;
    Sprites   : TPHXSpriteEngine;
    Player    : TPlayer;

    ShowNames : Boolean;
    ShowBounds: Boolean;
    ShowColliders: Boolean;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

// Our player class
//------------------------------------------------------------------------------
TPlayer = class(TPHXAnimatedSprite)
  public
    // Falling velocity
    FVelocity: Single;
    // Previous movement direction
    FDirection: Byte;
    FSpeed: Single;

    OnPlatform: Boolean;
    constructor Create(AEngine: TPHXSpriteEngine); override;


    procedure Update(const DeltaTime: Double; Input: TPHXInput); reintroduce;
    procedure Collided(Sprite: TPHXSprite); override;
  end;


implementation

uses
//  phxOpenGL_GLFW3,
  phxOpenGL_SDL2,
  phxGraphics_Vampyre;

var Map: TMap;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  // Create the device using the default provider
  Device:= TPHXDevice.Create;
  Device.Flags:= Device.Flags - [wfVerticalSync];
  // This loads a new icon for the window

  // Initialize the device
//  Device.Initialize('Phoenix Demo', 1024, 768,true);
  Device.Initialize('Phoenix Demo', 800, 600);
  // Create the timer
  Timer:= TPHXTimer.Create;
  // Create the input
  Input:= TPHXInput.Create;

  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/calibri12.phxfnt');
  Fonts.LoadFont('data/tahoma10_en.phxfnt');

  Images:= TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('data/platformer/Tiles.phximg'  , 'Tiles'  );
  Images.LoadImage('data/platformer/Sprites.phximg', 'Sprites');
  Images.LoadImage('data/platformer/Background.png', 'Background');

  Animations:= TPHXAnimationList.Create(Images);
  Animations.LoadAnimation('data/platformer/PlayerL.phxani');
  Animations.LoadAnimation('data/platformer/PlayerR.phxani');
  Animations.LoadAnimation('data/platformer/Powerup.phxani');

  Shapes:= TPHXShapeList.Create;

  ShowNames := False;
  ShowBounds:= False;
  ShowColliders:= False;

  // Setting the width and height of the sprite engine makes collision testing
  // much faster, If a sprite moves outside this bounds collisions for it might
  // not be working
  Sprites:= TPHXSpriteEngine.Create(Device, 100*32, 20*32);
  Sprites.Images    := Images;
  Sprites.Shapes    := Shapes;
  Sprites.Animations:= Animations;

  // Disable the automatic sprite collisions
  Sprites.Options:= Sprites.Options - [soCollide] + [soLimitScroll];

  // Normally the sprite engine uses brute force collisions, but there is a
  // collider using spatial hashes avaiable
  // Sprites.Collider:= TPHXSpatialHashCollider.Create(Sprites, TVector2f.Create(250, 250));

  Map:= TMap.Create(Images.Find('Tiles'));
  Map.LoadMap('data/platformer/Platformer.tmx', Sprites);

  Player:= TPlayer.Create(Sprites);
  Player.Name    := 'Player';
  Player.X       := 200;
  Player.Y       := 0;
  Player.Image   := 'Sprites';
  Player.Pattern := 'Player R00';
  Player.Collider:= True;
  Player.Mode    := cmDynamic;
  Player.Group   := GroupSprites;
  Player.Parent  := Sprites.Root;


  Sprites.Initialize;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Device.Update;

  Timer.Update;
  Input.Update;

  Player.Update(Timer.FrameTime, Input);

  Sprites.Update(Timer.FrameTime);

  Sprites.Camera.CenterOn(Player);
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
var Pos: TVector2f;
var Time: Double;
begin
  Device.SetClearColor(125/255, 150/255, 255/255);

  Device.Clear;

  // Draw parallax background
  Images.Find('Background').TileDraw(Sprites.Camera.ScrollX * 0.5 - 256, Sprites.Camera.ScrollY * 0.5 + 256, 10, 1);

  Sprites.Render;

  // Render the bounding boxes of all sprites
  if ShowBounds then
  begin
    Sprites.RenderBounds(Canvas, clrWhite);
  end;
  // Render all sprites that are marked as colliders
  if ShowColliders then
  begin
    Sprites.RenderColliders(Canvas, clrWhite);
  end;
  // Render the sprite names
  if ShowNames then
  begin
    Sprites.RenderNames(Fonts[1], clrWhite);
  end;

  Pos:= Sprites.ScreenToWorld(Input.Mouse.Position);

  Fonts[0].TextOut(400, 4, Format('Mouse.X: %f'#13'Mouse.Y: %f', [Pos.X, Pos.Y]));

  Timer.MeasureTime();
    Sprites.Collide;
  Time:= Timer.MeasureTime;

  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, Format('Sprites: %1d ', [Sprites.Count]));
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 1, Format('Tests  : %d'  , [Sprites.Collider.Tests]));
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 2, Format('Time   : %.1f ms', [Time * 1000]));

  Fonts[0].TextOut(4, 100 + Fonts[0].Height * 0, 'Show names [N]: ' + BoolToStr(ShowNames, True));
  Fonts[0].TextOut(4, 100 + Fonts[0].Height * 1, 'Show bounds [B]: ' + BoolToStr(ShowBounds, True));
  Fonts[0].TextOut(4, 100 + Fonts[0].Height * 2, 'Show colliders [C]: ' + BoolToStr(ShowColliders, True));

  Fonts[0].TextOut(200, 4 + Fonts[0].Height * 0, 'Bananas ' + IntToStr(Map.Bananas.Count));
  Fonts[0].TextOut(200, 4 + Fonts[0].Height * 1, 'Apples '  + IntToStr(Map.Apples.Count));

  // Draw FPS
  Fonts[0].TextOut(Device.Bounds.Inflate(-4,-4), Format('FPS: %d', [Timer.FrameRate]), taTopRight);

  Fonts[0].TextOut(550, 4, Format('Scroll.X: %f'#13'Scroll.Y: %f', [Sprites.Camera.ScrollX, Sprites.Camera.ScrollY]));

  // Flush the canvas
  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Input.Free;

  Images.Free;
  Fonts.Free;
  Shapes.Free;
  Sprites.Free;
  Canvas.Free;

  Device.Free;

  Map.Free;
end;

//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;

  if Key = TPHXVirtualKey(ord('1')) then
  begin
    Sprites.Collider:= TPHXSpriteCollider.Create(Sprites);
  end;
  if Key = TPHXVirtualKey(ord('2')) then
  begin
    Sprites.Collider:= TPHXSpatialHashCollider.Create(Sprites, TVector2f.Create(250, 250));
  end;

  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;

  if Key = VK_N then
  begin
    ShowNames:= not ShowNames;
  end;
  if Key = VK_B then
  begin
    ShowBounds:= not ShowBounds;
  end;
  if Key = VK_C then
  begin
    ShowColliders:= not ShowColliders;
  end;
end;



// TPlayer
//------------------------------------------------------------------------------
constructor TPlayer.Create(AEngine: TPHXSpriteEngine);
begin
  inherited;
  FSpeed:= 1.0;

end;
//------------------------------------------------------------------------------
procedure TPlayer.Update(const DeltaTime: Double; Input: TPHXInput);
var Delta: Single;
begin
  FVelocity:= FVelocity + 400 * DeltaTime;

  // Test if there is a sprite one pixel down
  OnPlatform:= Collide(Vector2f(0, 1), GroupTiles);

  // Stuck in ground, this should only happen due to numeric errors with large
  // float values.
  if OnPlatform and Collide(Vector2f(0, 0), GroupTiles)  then
  begin
    Y:=Y - 0.1;
  end;

  // Calculate the falling delta
  Delta:= FVelocity * DeltaTime;

  // Dont allow the player to jump outside the top border of the world
  if Y + Delta < 0 then
  begin
    Delta:= 0;
    FVelocity:= 0;
  end;

  // If we are inside a tile after moving down due to falling reset the velocity
  if Collide(Vector2f(0, Delta), GroupTiles)  then
  begin
    FVelocity:= 0;
  end else
  begin
    Y:= Y + Delta;
  end;

  // Moving left
  if isLeft in Input.States then
  begin
    Delta:= 200 * DeltaTime * FSpeed;

    // Move left if we doesnt collide with a wall
    if not Collide(Vector2f(-Delta, -1), GroupTiles)  then
    begin
      MoveLeft(Delta);
    end;
    FDirection:= 1;

    Animation:= 'PlayerL';
  end else
  if isRight in Input.States then
  begin
    Delta:= 200 * DeltaTime * FSpeed;

    // Move right if we doesnt collide with a wall
    if not Collide(Vector2f(+Delta, -1), GroupTiles)  then
    begin
      MoveRight(Delta);
    end;
    FDirection:= 2;

    Animation:= 'PlayerR';
  end else
  begin
    Animation:= '';

    if FDirection = 1 then
    begin
      Pattern:= 'Player L00';
    end else
    begin
      Pattern:= 'Player R00';
    end;

  end;

  // Jump
  if (isUp in Input.States) and OnPlatform then
  begin
    FVelocity:= -400 * FSpeed;
  end;
end;

//------------------------------------------------------------------------------
procedure TPlayer.Collided(Sprite: TPHXSprite);
begin
  inherited;
  if Sprite is TApple then
  begin
    Map.Apples.Remove(Sprite);

    Sprite.Kill;
  end;
  if Sprite is TBanana then
  begin
    Map.Bananas.Remove(Sprite);

    Sprite.Kill;
  end;
  if Sprite is TPowerup then
  begin
    FSpeed:= 2.0;

    Sprite.Kill;
  end;


end;



end.

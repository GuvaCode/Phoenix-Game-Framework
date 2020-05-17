unit main;

{$mode delphi}{$H+}

interface

uses SysUtils,

//Generics.Collections,

  phxTypes,
  phxMath,
  phxDevice,
  phxCanvas,
  phxApplication,
  phxImage,
  phxFont,
  phxInput,
  phxSprite;

type

// Player sprite
//------------------------------------------------------------------------------
TPlayer = class(TPHXSprite)

end;

// Enemy sprite
//------------------------------------------------------------------------------
TEnemy = class(TPHXSprite)
  public
    procedure Collided(Sprite: TPHXSprite); override;
  end;

//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Images : TPHXImageList;
    Fonts  : TPHXFontList;
    Input  : TPHXInput;
    Sprites: TPHXSpriteEngine;
    Player : TPlayer;

    DrawBounds: Boolean;
    DrawNames : Boolean;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;


implementation

// Use opengl for rendering and freeimage for texture loading
uses
        phxOpenGL_GLFW3,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
procedure TGame.Init;
var Sprite: TPHXSprite;
  Index: Integer;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;

   Timer:= TPHXTimer.Create;
  Input:= TPHXInput.Create;

  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Images := TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('content/Sprites.png', 'Sprites').Patterns.AddRectangular(32, 32, True);
  Images.LoadImage('content/Ball.png'   , 'Ball'  ).Patterns.AddSingle(True);

  Sprites:= TPHXSpriteEngine.Create(Device);
  Sprites.Images:= Images;

  Player:= TPlayer.Create(Sprites);
  Player.Name   := 'Player';
  Player.Image  := 'Ball';
  Player.Pattern:= 'Ball';
  Player.Parent := Sprites.Root;
  Player.X:= 400;
  Player.Y:= 100;
  // Allow collision testing
  Player.Collider:= True;

  Randomize;

  for Index := 1 to 100 do
  begin
    Sprite:= TEnemy.Create(Sprites);
    Sprite.Name   := 'Sprite' + IntToStr(Index);
    Sprite.Image  := 'Sprites';
    Sprite.Pattern:= Images[0].Patterns[Random(Images[0].Patterns.Count)].Name;
    Sprite.Parent := Sprites.Root;
    Sprite.X:= 16 + Random(Device.Width  - 32);
    Sprite.Y:= 16 + Random(Device.Height - 32);
    Sprite.Collider:= True;
  end;

  Sprites.Initialize;

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont(ContentPath + 'Calibri12.phxfnt');
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Device.Update;

  Timer.Update;

  Input.Update;

  Player.RotateRight(Timer.FrameTime * 45);
  Player.X:= Input.Mouse.X;
  Player.Y:= Input.Mouse.Y;

  Sprites.Update(Timer.FrameTime);
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;

  Sprites.Render;

  if DrawBounds then
  begin
    Sprites.RenderBounds(Canvas, clrYellow);
  end;
  if DrawNames then
  begin
    Sprites.RenderNames(Fonts[0],clrBlue);

  end;
  // The TPHXSpriteEngine.Sprites contains all our sprites plus the root sprite
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, Format('Sprites: %d', [Sprites.Sprites.Count-1]));

  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 1, Format('DrawBounds (SHIFT+B): %s', [BoolToStr(DrawBounds, True)]));
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 2, Format('DrawNames  (SHIFT+N): %s', [BoolToStr(DrawNames , True)]));

  Fonts[0].TextOut(Device.Width-100, 4, Format('%d fps', [Timer.FrameRate]));

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Sprites.Free;
  Fonts.Free;
  Images.Free;
  Timer.Free;
  Canvas.Free;
  Device.Free;
end;


//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
  if (Key = VK_B) and (ssShift in Shift) then
  begin
    DrawBounds:= not DrawBounds;
  end;
  if (Key = VK_N) and (ssShift in Shift) then
  begin
    DrawNames:= not DrawNames;
  end;
end;


{ TEnemy }

procedure TEnemy.Collided(Sprite: TPHXSprite);
begin
  inherited;
  Kill;
end;



end.

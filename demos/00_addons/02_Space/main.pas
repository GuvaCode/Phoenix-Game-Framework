unit main;

{$mode delphi}{$H+}

interface

uses SysUtils,

  phxTypes,
  phxEvents,
  phxMath,
  phxDevice,

  phxGraphics,

  phxCanvas,
  phxApplication,
  phxFont,
  phxTexture,
  phxImage,
  phxSprite,
  phxInput,

  uPlayer;

type

// Using the game template is the easy way to use Phoenix.
// Check the source in phxTemplate.pas to get an idea what the application class
// does.
//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Input  : TPHXInput;

    Canvas : TPHXCanvas;
    Fonts  : TPHXFontList;
    Images : TPHXImageList;

    Background: TPHXTexture;
    Stars     : TPHXTexture;

    Sprites: TPHXSpriteEngine;
    Player: TPlayer;
    Target : TPHXSprite;

    procedure RenderBackground;
    procedure RenderTarget;
  protected
    function GetContentPath: string; override;

    procedure MouseMoved(X: Integer; Y: Integer; Shift: TPHXShiftStates); override;
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
//  phxOpenGL_SDL2,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
function TGame.GetContentPath: String;
begin
  Result:= inherited GetContentPath + 'Space' + PathDelim;
end;

//------------------------------------------------------------------------------
procedure TGame.Init;
var Sprite: TPHXSprite;
begin
  Device:= TPHXDevice.Create;
  //Device.Flags:= Device.Flags - [wfVerticalSync];
  Device.Flags:=[wfCursor, wfResizable] ;
  Device.Initialize('Phoenix Demo', 1024,768,false);

  // Device.Initialize('Phoenix Demo', 1024,768);






  Timer:= TPHXTimer.Create;

  Input:= TPHXInput.Create;

  Background:= Device.CreateTexture;
  Background.LoadTexture('content/Space/goldSpace.png');

  Stars:= Device.CreateTexture;
  Stars.LoadTexture('content/Space/Stars.png');

  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Images := TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('content/Space/Stars.png');
  Images.LoadImage('content/Space/Images.phximg');

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('content/Calibri12.phxfnt');

  Sprites:= TPHXSpriteEngine.Create(Device);
  Sprites.Images:= Images;

  Sprite:= TPHXSprite.Create(Sprites);
  Sprite.Name:= 'Ship001';
  Sprite.Image:='Images';
  Sprite.Pattern:= 'Ship001';
  Sprite.Parent:= Sprites.Root;
  Sprite.X:= 400;
  Sprite.Y:= 100;
  Sprite.Color:= clrRed;
  Sprite.Collider:= True;

  Sprite:= TPHXSprite.Create(Sprites);
  Sprite.Name:= 'Ship002';
  Sprite.Image:='Images';
  Sprite.Pattern:= 'Ship002';
  Sprite.Parent:= Sprites.Root;
  Sprite.X:= 500;
  Sprite.Y:= 100;
  Sprite.Collider:= True;

  Sprite:= TPHXSprite.Create(Sprites);
  Sprite.Name:= 'Ship004';
  Sprite.Image:='Images';
  Sprite.Pattern:= 'Ship004';
  Sprite.Parent:= Sprites.Root;
  Sprite.X:= 250;
  Sprite.Y:= 300;
  Sprite.Collider:= True;

  Sprite:= TPHXSprite.Create(Sprites);
  Sprite.Name:= 'Planet001';
  Sprite.Image:='Images';
  Sprite.Pattern:= 'Planet001';
  Sprite.Parent:= Sprites.Root;
  Sprite.X:= 100;
  Sprite.Collider:= True;
  Sprite.Y:= 300;

  Player:= TPlayer.Create(Sprites);
  Player.Image:='Images';
  Player.Pattern:= 'Ship001';
  Player.Parent:= Sprites.Root;
  Player.X:= 100;
  Player.Y:= 100;
  Player.Collider:= False;

  Sprites.Initialize;
end;


//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Background.Free;
  Stars.Free;
  Canvas.Free;
  Device.Free;
  Input.Free;
end;


//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;
  Input.Update;

  Device.Update;
  Player.Update(Timer.FrameTime, Input);

  Sprites.Camera.ScrollX:= Device.Width div 2 - Player.Position.x ;
  Sprites.Camera.ScrollY:= Device.Height div 2 - Player.Position.Y;


   //WorldToScreenPlayer.Position;

  Sprites.Update(Timer.FrameTime);

  Sprites.Collider.Update;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;
 RenderBackground;

 // Canvas.Color  := clrSilver;
 // Canvas.FilledRectangle(200, 200, 200+256, 200+256);

  Sprites.Render;
//  Sprites.DrawBounds(Canvas);

  RenderTarget;

  Fonts[0].TextOut(0, 0, Format('%d fps', [Timer.FrameRate]));

  Fonts[0].TextOut(0, 20, Format('Velocity: %.0f', [Player.Velocity]));

  if Assigned(Target) then
  begin
    Fonts[0].TextOut(0, 200, 'Target: ' + Target.Name);
  end else
  begin
    Fonts[0].TextOut(0, 200, 'Target: null');
  end;
  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.RenderTarget;
var Image: TPHXImage;
var Position: TVector2f;
begin
  if Assigned(Target) then
  begin
   // Position:= Sprites.WorldToScreen(Target.Position);
    Position:=Sprites.WorldToScreen(Target.Position.X,Target.Position.Y);

    Image:= Images.Find('Images');

    Image.DrawRotate(Position.X, Position.Y, Timer.ElapsedTime * 90 , Image.Patterns.IndexOf('Target'));
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.RenderBackground;
var X,Y: Single;
var Rect: TRectf;
var Coord: TRectf;
begin
  Rect.Left  := 0;
  Rect.Top   := 0;
  Rect.Right := Device.Width;
  Rect.Bottom:= Device.Height;

  Coord.Left  := ((-Sprites.Camera.ScrollX * 0.25) / Background.Width);
  Coord.Right := ((-Sprites.Camera.ScrollX * 0.25) / Background.Width) + (Device.Width / Background.Width);

  Coord.Top   := ((-Sprites.Camera.ScrollY * 0.25) / Background.Height);
  Coord.Bottom:= ((-Sprites.Camera.ScrollY * 0.25) / Background.Height) + (Device.Height / Background.Height);


  Canvas.Texture:= Background;
  Canvas.Color  := clrWhite;
//  Canvas.Color  := clrGray;
  Canvas.FilledRectangle(Rect, Coord);



  Coord.Left  := (-Sprites.Camera.ScrollX*0.25) / Stars.Width;
  Coord.Right := (-Sprites.Camera.ScrollX*0.25) / Stars.Width + (Device.Width / Stars.Width);

  Coord.Top   := (-Sprites.Camera.ScrollY*0.25) / Stars.Width;
  Coord.Bottom:= (-Sprites.Camera.ScrollY*0.25) / Stars.Width + (Device.Height / Stars.Height);


  Canvas.Texture:= Stars;
  Canvas.Color  := Color4f(1.0, 1.0, 1.0, 0.50);
  Canvas.FilledRectangle(Rect, Coord);

  Exit;


  X:= Sprites.Camera.ScrollX * 0.10;
  Y:= Sprites.Camera.ScrollY * 0.10;

  Canvas.Texture:= Stars;
  Canvas.Color  := clrGray;
  Canvas.FilledRectangle(Rect, Coord);

 // Canvas.FilledRectangle(0, 0, 1024, 1024);
//  Canvas.FilledRectangle(0+X, 0+Y, 1024+X, 1024+Y);

//  Device.SetRenderState(PHX_BLENDMODE, PHX_BLENDMODE_ALPHABLEND);
  X:= Sprites.Camera.ScrollX * 0.25;
  Y:= Sprites.Camera.ScrollY * 0.25;

  Canvas.Texture:= Images.Find('stars.png').Texture;
  Canvas.Color  := clrGray;
  Canvas.FilledRectangle(0+X, 0+Y, 512+X, 512+Y);

end;

   //       Background: TPHXTexture;
   //  Stars    : TPHXTexture;

//------------------------------------------------------------------------------
procedure TGame.MouseMoved(X, Y: Integer; Shift: TPHXShiftStates);
var Position: TVector2f;
begin
  inherited;
  Position:= Sprites.ScreenToWorld(X,Y);

  Target:= Sprites.SpriteAt(Position);

  Player.Target:= Target;
end;

end.

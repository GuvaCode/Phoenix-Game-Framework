unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  phxLogger,
  phxTypes,
  phxEvents,
  phxMath,
  phxApplication,
  phxDevice,
  phxCanvas,
  phxTexture,
  phxFont;

type
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Textures  : TPHXTextureList;
    Fonts     : TPHXFontList;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

implementation

uses
  phxOpenGL_SDL2,
  phxGraphics_Vampyre;

constructor TGame.Create;
begin
  inherited;

end;

procedure TGame.Init;
begin
  // Create the device using the default provider
  Device:= TPHXDevice.Create;
  // Initialize the device
  Device.Initialize;
  // Create the canvas
  Canvas:= Device.CreateCanvas;
  // Create the timer
  Timer:= TPHXTimer.Create;

  Textures:= TPHXTextureList.Create(Device);
  Textures.LoadTexture('data/background.png');
  Textures.LoadTexture('data/mud.bmp');

  Fonts := TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/calibri12.phxfnt');
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
end;

procedure TGame.Render;
var Rect : TRectf;
var Coord: TRectf;
begin
  Device.Clear;

    // By scaling the texture coordinates we can tiledraw the background texture with only one primitive
    Rect := Rectf(0,0, Device.Width, Device.Height);
    Coord:= Rectf(0,0, Device.Width / Textures[0].Width, Device.Height / Textures[0].Height);

    // Draw background
    Canvas.Color    := clrWhite;
    Canvas.Texture  := Textures[0];
    Canvas.FilledRectangle(Rect, Coord);

    // Draw a textured rectangle
    Canvas.Color    := clrWhite;
    Canvas.Texture  := Textures[1];
    Canvas.Rectangle(100, 50, 100+100, 50+100);

    // Draw a textured rectangle
    Canvas.Color    := clrWhite;
    Canvas.Texture  := Textures[1];
    Canvas.FilledRectangle(250, 50, 250+100, 50+100);

    // Draw a alpha blended non textured rectangle
    Canvas.Texture   := nil;
    Canvas.Blending  := bmAlpha;
    Canvas.Color     := Color4f(clrWhite, 0.5);
    Canvas.FilledRectangle(400, 50, 400+100, 50+100);

    // Draw a elipse using 32 slices
    Canvas.Texture   := Textures[1];
    Canvas.Color     := clrWhite;
    Canvas.Ellipse(Vector2f(150, 250), Vector2f(50, 50), 32);

    // Draw a elipse using 32 slices
    Canvas.Texture   := Textures[1];
    Canvas.Color     := clrWhite;
    Canvas.FilledEllipse(Vector2f(300, 250), Vector2f(50, 50), 32);

    // Draw a arc using 32 slices on a full revolution
    Canvas.Texture   := Textures[1];
    Canvas.Color     := clrWhite;
    Canvas.Arc(Vector2f(150, 400), Vector2f(50, 50), Rangef(0, 45 * Timer.ElapsedTime), 32);

    // Draw a filled arc using 32 slices on a full revolution
    Canvas.Texture   := Textures[1];
    Canvas.Color     := clrWhite;
    Canvas.FilledArc(Vector2f(300, 400), Vector2f(50, 50), Rangef(0, 45 * Timer.ElapsedTime), 32);

    Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, Format('%d fps', [Timer.FrameRate]));
    Fonts[0].TextOut(4, 4 + Fonts[0].Height * 1, 'Press W for wireframe');
    Fonts[0].TextOut(4, 4 + Fonts[0].Height * 2, 'Press C for reset wireframe');
    Canvas.Flush;

    Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Textures.Free;
  Timer.Free;
  Canvas.Free;
  Device.Free;
end;

procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
    if Key =  VK_W then
  begin
    Device.SetWireFrame(True);
  end;
     if Key =  VK_C then
  begin
    Device.SetWireFrame(False);
  end;
  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;
end;

end.


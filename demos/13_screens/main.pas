unit main;

{$mode delphi}{$H+}

interface

uses SysUtils,

  phxTypes,
  phxEvents,
  phxMath,
  phxDevice,
  phxCanvas,
  phxApplication,
  phxFont,
  phxImage,
  phxInput;

type

// Using the game template is the easy way to use Phoenix.
// Check the source in phxTemplate.pas to get an idea what the application class
// does.
//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Canvas : TPHXCanvas;
    Timer  : TPHXTimer;
    Input  : TPHXInput;
    Fonts  : TPHXFontList;
    Images : TPHXImageList;
    Screens: TPHXGameScreens;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end;

//------------------------------------------------------------------------------
TMainScreen =  class(TPHXGameScreen)
  private
    Owner: TGame;

    Device: TPHXDevice;
    Canvas : TPHXCanvas;
    Images: TPHXImageList;
    Fonts : TPHXFontList;
    Input : TPHXInput;
  public
    constructor Create(AOwner: TPHXGameScreens; AData: Pointer); override;

    procedure Update; override;
    procedure Render; override;
  end;

//------------------------------------------------------------------------------
TMenuScreen =  class(TPHXGameScreen)
  private
    Owner: TGame;

    Device: TPHXDevice;
    Canvas : TPHXCanvas;
    Images: TPHXImageList;
    Fonts : TPHXFontList;
    Input : TPHXInput;
  public
    constructor Create(AOwner: TPHXGameScreens; AData: Pointer); override;

    procedure Update; override;
    procedure Render; override;
  end;


implementation

uses
  phxOpenGL_GLFW3,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;

  Device.Icon:= 'data/Phoenix.bmp';

  Timer:= TPHXTimer.Create;
  Input:= TPHXInput.Create;

  Input.Bindings.Add(isButton32, VK_ESC);
  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Images:= TPHXImageList.Create(Device, Canvas);
  Images.LoadImage('data/Background0.png');
  Images.LoadImage('data/Background1.png');

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/calibri12.phxfnt');

  Screens:= TPHXGameScreens.Create;
  Screens.Add(TMainScreen, 'Main', Self);
  Screens.Add(TMenuScreen, 'Menu', Self);

  Screens.Open('Main');
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;
  Input.Update;

  Screens.Update(Timer.FrameTime);

  Device.Update;

end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;

  Screens.Render;

  if Assigned(Screens.Active) then
  begin
    Fonts[0].TextOut(2,2, 'Active screen: ' + Screens.Active.Name);
  end else
  begin
    Fonts[0].TextOut(2,2, 'Active screen: null');
  end;

  Fonts[0].TextOut(2, Device.Height - 20, 'Press ESC to change screen');

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Input.Free;
  Fonts.Free;
  Images.Free;
  Canvas.Free;
  Device.Free;
end;



// TMainScreen
//------------------------------------------------------------------------------
constructor TMainScreen.Create(AOwner: TPHXGameScreens; AData: Pointer);
begin
  inherited Create(AOwner, AData);
  Owner:= TGame(Data);

  Device:= Owner.Device;
  Canvas:= Owner.Canvas;
  Images:= Owner.Images;
  Fonts := Owner.Fonts;
  Input := Owner.Input;
end;

//------------------------------------------------------------------------------
procedure TMainScreen.Render;
begin
  Images[0].Draw(0,0);
end;

//------------------------------------------------------------------------------
procedure TMainScreen.Update;
begin
  if isButton32 in Input.States then
  begin
    Input.States:= Input.States - [isButton32];

    Open('Menu');
  end;
end;



// TMenuScreen
//------------------------------------------------------------------------------
constructor TMenuScreen.Create(AOwner: TPHXGameScreens; AData: Pointer);
begin
  inherited Create(AOwner, AData);
  Owner:= TGame(Data);

  Device:= Owner.Device;
  Canvas:= Owner.Canvas;
  Images:= Owner.Images;
  Fonts := Owner.Fonts;
  Input := Owner.Input;
end;

//------------------------------------------------------------------------------
procedure TMenuScreen.Render;
var W, H: Integer;
begin
  W:= Device.Width;
  H:= Device.Height;

  Canvas.Overlay(W,H, Color4f(0.0, 0.0, 0.0, 0.50 * Transition.Position) );

//  Fonts[0].TextOut(Rectf(0,0, W,H), 'Paused', taTop, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TMenuScreen.Update;
begin
  if isButton32 in Input.States then
  begin
    Input.States:= Input.States - [isButton32];

    Close;
  end;
end;




end.

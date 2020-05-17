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
  phxSkin,
  phxSimpleGUI;

type

//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Font   : TPHXFont;
    Skin   : TPHXSkin;
    Gui    : TPHXSimpleGUI;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
    procedure Resized; override;

  end;


implementation

uses
  phxOpenGL_GLFW3,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  Device.Icon:= ContentPath + 'Phoenix.bmp';
  Device.Flags:= Device.Flags + [wfResizable];
  Device.Initialize('Phoenix Demo', 800, 600);

  Device.SetClearColor(0.8, 0.8, 0.8);

  Timer:= TPHXTimer.Create;
  Canvas:= Device.CreateCanvas;

  Font:= TPHXFont.Create(Device, Canvas);
  Font.LoadFromFile('data/calibri12.phxfnt');

  Skin:= TPHXSkin.Create(Device, Canvas);
  Skin.LoadFromFile('data/Default.phxskn');

  // Create the gui
  Gui:= TPHXSimpleGUI.Create(Canvas, Skin, Font);
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Skin.Free;
  Font.Free;
  Canvas.Free;
  Device.Free;
  Gui.Free;
end;

var Player: String = 'Player 1';
var Option1: Boolean;
var Option2: Boolean;
var Volume: Single = 40;
var Progress: Single = 0;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;

  Gui.Update(Timer.FrameTime);

  Progress:= Progress + Timer.FrameTime / 10;
  if Progress > 1.0 then Progress:= 0.0;
end;


//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;

  // With the position property you can offset the gui position
  Gui.Position:= Vector2i((Device.Width - 800) div 2, 50);

  // Call this before rendering any widgets
  Gui.Prepare;

  Gui.Window(100,100, 400, 400);
  Gui.Panel(110, 130, 300, 100);

  if Gui.Button(120, 185, 111, 29, 'Button1') then
  begin
    Font.TextOut(2,2, 'Button 1 pressed');
  end;
  if Gui.Button(240, 185, 111, 29, 'Button2') then
  begin
    Font.TextOut(2,2, 'Button 2 pressed');
  end;

  Gui.Edit(120, 140, 220, 27, Player);

  Gui.CheckBox(120, 300, 100, 14, 'Option 1', Option1);
  Gui.CheckBox(120, 320, 100, 14, 'Option 2', Option2);

  Gui.Slider(120, 340, 200, 14, 0, 100, Volume);

  Gui.Text(120+200+8, 340, 200, 14, Format('%.0f', [Volume]));

  Gui.ProgressBar(120, 400, 200, 14, Progress, pdHorisontal);
  Gui.ProgressBar(440, 200, 14, 200, Progress, pdVertical);

  // Call this after the gui rendering is completed
  Gui.Finish;

  Font.TextOut(200, 2 + Font.Height * 0, 'Player: '  + Player );
  Font.TextOut(200, 2 + Font.Height * 1, 'Option1: ' + BoolToStr(Option1, True) );
  Font.TextOut(200, 2 + Font.Height * 2, 'Option2: ' + BoolToStr(Option2, True) );
  Font.TextOut(200, 2 + Font.Height * 3, 'Volume: '  + FloatToStr(Volume) );

  // Show some gui state
  Font.TextOut(500, 2 + Font.Height * 0, Format('Count  : %d', [Gui.Count]  ));
  Font.TextOut(500, 2 + Font.Height * 1, Format('Hover  : %d', [Gui.Hover]  ));
  Font.TextOut(500, 2 + Font.Height * 2, Format('Pressed: %d', [Gui.Pressed] ));
  Font.TextOut(500, 2 + Font.Height * 3, Format('Focused: %d', [Gui.Focused] ));

  Canvas.Flush;

  Device.Flip;
end;







procedure TGame.Resized;
begin
  inherited;
  Device.SetViewport(0,0, Device.Width, Device.Height);
end;

end.

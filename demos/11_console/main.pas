unit main;

{$mode delphi}{$H+}

interface

uses SysUtils,

  phxTypes,
  phxMath,
  phxApplication,
  phxDevice,
  phxCanvas,
  phxTexture,
  phxFont,
  phxConsole;

type

//------------------------------------------------------------------------------

{ TGame }

TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Font   : TPHXFont;
    Console: TPHXConsole;

    PlayerName : String;
    PlayerColor: TColor4f;

    ServerAddress: String;

    procedure ConsoleConnect(Console: TPHXConsole; Parameters: TPHXConsoleParameters);
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
  phxOpenGL_GLFW3,
  phxGraphics_Vampyre;

//------------------------------------------------------------------------------
constructor TGame.Create;
begin
  inherited;

end;

//------------------------------------------------------------------------------
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  // This loads a new icon for the window
  Device.Icon:= ContentPath + 'Phoenix.bmp';


  Device.Initialize('Phoenix Demo', 800, 600);;

  Canvas:= Device.CreateCanvas;;

  Font:= TPHXFont.Create(Device, Canvas);
  Font.LoadFromFile('data/calibri12.phxfnt');

  PlayerName := 'Player 1';
  PlayerColor:= clrRed;

  ServerAddress:= '0.0.0.0';

  // Create the console
  Console:= TPHXConsole.Create(Device, Canvas, Font);
  // Add a console variable for the player name
  Console.Items.Add('name' , PlayerName);
  // Add a console variable for the player color
  Console.Items.Add('color', PlayerColor);
  // Add a function callback to the console
  Console.Items.Add('connect', ConsoleConnect);

  Console.Lines.Add('Press TAB to show the console items');

  // Execute a command from code
  Console.Execute('connect localhost');

  Timer:= TPHXTimer.Create;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;
  Input.Update;
  Console.Update(Timer.FrameTime);
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;


  if not Console.Visible then
  begin
    Font.TextOut(4, 4, Format('Press %s to open the console', [VirtualKeyToString(Console.Shortcut)]));

    Font.TextOut(4, 4 + Font.Height * 1, 'Player: ' + PlayerName, PlayerColor);
  end;

  Font.TextOut(4,Device.Height - 40, 'Server: ' + ServerAddress);

  Canvas.Color:= PlayerColor;
  Canvas.Texture:= nil;
  Canvas.Rectangle(500,500,600,600);

  // Draw the console last to make it on top of the rest of the graphics
  Console.Render;

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Canvas.Free;
  Device.Free;
end;

//------------------------------------------------------------------------------
procedure TGame.ConsoleConnect(Console: TPHXConsole; Parameters: TPHXConsoleParameters);
begin
  if Parameters.Count >= 1 then
  begin
    ServerAddress:= Parameters[0].AsString;
  end else
  begin
    Console.Add('Please specify host name');
  end;
end;

procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited KeyPressed(Key, Shift);
  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;

end;






end.

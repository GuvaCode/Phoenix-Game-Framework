unit gameunit;

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
  phxTexture;

type
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public
    constructor Create; override;
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Shutdown; override;
  end; implementation

uses
  //phxOpenGL_GLFW3,
  phxOpenGL_SDL2,
  phxGraphics_FreeImage;

constructor TGame.Create;
begin
  inherited;

end;

procedure TGame.Init;
begin
  // Create the device using the default provider
  Device:= TPHXDevice.Create;
  // Initialize the device
  Device.Initialize('Phoenix Demo', 800, 600);
  // Create the canvas
  Canvas:= Device.CreateCanvas;
  // Create the timer
  Timer:= TPHXTimer.Create;
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
end;

procedure TGame.Render;
begin
  Device.Clear;
  // Change the color to white
  Canvas.Color:= clrWhite;
  // Draw a rectangle
  Canvas.Rectangle(100, 100, 200, 200);
  // Change the color to red
  Canvas.Color:= clrRed;
  // Draw a rectangle
  Canvas.Rectangle(300, 100, 400, 200);
  // Flush the canvas
  Canvas.Flush;
  // Flip
  Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Timer.Free;
  Canvas.Free;
  Device.Free;
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

end.


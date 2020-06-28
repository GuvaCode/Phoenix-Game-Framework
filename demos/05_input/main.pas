unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  phxTypes,
  phxEvents,
  phxMath,
  phxDevice,
  phxCanvas,
  phxApplication,
  phxFont,
  phxInput;

type
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;

    Fonts   : TPHXFontList;
    Input   : TPHXInput;
    Joystick: TPHXJoystick;
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
  phxOpenGL_GLFW3,
  phxGraphics_Vampyre;

constructor TGame.Create;
begin
  inherited;

end;

procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
    Device.Initialize('Phoenix Demo', 800, 600);;

    Timer:= TPHXTimer.Create;
    // Create our canvas
    Canvas:= Device.CreateCanvas;

    Fonts:= TPHXFontList.Create(Device, Canvas);
    Fonts.LoadFont('data/calibri12.phxfnt');

    Input:= TPHXInput.Create;

    Input.Bindings.Add(isButton3, VK_Q);
    Input.Bindings.Add(isButton4, VK_E);

    Input.Bindings.Add(isButton4, PHX_JOYSTICK_1, PHX_JOYSTICK_BUTTON_1);

    if Input.Joysticks.Count > 0 then
    begin
      Joystick:= Input.Joysticks[0];
    end else
    begin
      Joystick:= nil;
    end;

    Input.SaveBindings('data/input.cfg');
    Input.Bindings.Clear;
    Input.LoadBindings('data/input.cfg');
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
end;

procedure TGame.Render;
var Index: Integer;
var State: TPHXInputState;
begin
  Device.Clear;

  Fonts[0].TextOut(0, 600, Format('%d fps', [Timer.FrameRate]));


  Index:= 1;
  Fonts[0].TextOut(0, 200, 'Input states', clrMaroon, clrWhite);
  for State:= Low(TPHXInputState) to High(TPHXInputState) do
  begin
    if State in Input.States then
    begin
      Fonts[0].TextOut(0, 200+Fonts[0].Height * Index, InputStateToString(State));

      Inc(Index);
    end;
  end;

  Index:= 1;
  Fonts[0].TextOut(150, 200, 'Keyboard states', clrMaroon, clrWhite);
  for State:= Low(TPHXInputState) to High(TPHXInputState) do
  begin
    if State in Input.Keyboard.States then
    begin
      Fonts[0].TextOut(150, 200+Fonts[0].Height * Index, InputStateToString(State));

      Inc(Index);
    end;
  end;

  Fonts[0].TextOut(300, 4 + Fonts[0].Height * 0, Format('Input.Mouse.X: %d', [Input.Mouse.X]), clrSilver);
  Fonts[0].TextOut(300, 4 + Fonts[0].Height * 1, Format('Input.Mouse.Y: %d', [Input.Mouse.Y]), clrSilver);

  Index:= 1;
  Fonts[0].TextOut(300, 200, 'Mouse states', clrMaroon, clrWhite);
  for State:= Low(TPHXInputState) to High(TPHXInputState) do
  begin
    if State in Input.Mouse.States then
    begin
      Fonts[0].TextOut(300, 200+Fonts[0].Height * Index, InputStateToString(State));

      Inc(Index);
    end;
  end;

  if Assigned(Joystick) then
  begin
    Fonts[0].TextOut(450, 4 + Fonts[0].Height * 0, Format('Joystick.Axis[0]: %d', [Joystick.Axes[PHX_JOYSTICK_AXIS_1]]), clrSilver);
    Fonts[0].TextOut(450, 4 + Fonts[0].Height * 1, Format('Joystick.Axis[1]: %d', [Joystick.Axes[PHX_JOYSTICK_AXIS_2]]), clrSilver);
    Fonts[0].TextOut(450, 4 + Fonts[0].Height * 2, Format('Joystick.Axis[2]: %d', [Joystick.Axes[PHX_JOYSTICK_AXIS_3]]), clrSilver);
    Fonts[0].TextOut(450, 4 + Fonts[0].Height * 3, Format('Joystick.Axis[3]: %d', [Joystick.Axes[PHX_JOYSTICK_AXIS_4]]), clrSilver);

    Index:= 1;
    Fonts[0].TextOut(450, 200, 'Joystick states', clrMaroon, clrWhite);
    for State:= Low(TPHXInputState) to High(TPHXInputState) do
    begin
      if State in Joystick.States then
      begin
        Fonts[0].TextOut(450, 200+Fonts[0].Height * Index, InputStateToString(State));

        Inc(Index);
      end;
    end;
  end;

  Canvas.Flush;

  Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Timer.Free;
  Canvas.Free;
  Device.Free;
  Input.Free;
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


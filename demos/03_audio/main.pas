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

  phxAudio,
  phxAudio_Bass;
//  phxAudio_OpenAL;

type
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;

    Fonts  : TPHXFontList;

    Audio: TPHXAudioEngine;
    Music: TPHXAudioStream;
    Boom : TPHXAudioSample;

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
  phxGraphics_FreeImage;

constructor TGame.Create;
begin
 // inherited;

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

  Audio:= TPHXAudioEngine.Create;
  Audio.Initialize;

  Boom:= Audio.Samples.LoadSample('data/missile.wav');

  Music:= Audio.LoadStream('data/music.wav');
  Music.Looped:= True;
  Music.Play;
end;

procedure TGame.Update;
begin
  Timer.Update;
  Device.Update;
  Audio.Update;
end;

procedure TGame.Render;
var Index: Integer;
begin
  Device.Clear;
  Fonts[0].TextOut(0, 0, Format('%d fps', [Timer.FrameRate]));
  Fonts[0].TextOut(0, 50, 'Devices: ');
  for Index := 0 to Audio.Devices.Count - 1 do
  begin
    Fonts[0].TextOut(0, 50 + (Index + 1) * Fonts[0].Height, Format('%d %s', [Index, Audio.Devices[Index].Name]));
  end;
  Fonts[0].TextOut(0200, 0, Format('Position: %.1f / %.1f', [Music.Position, Music.Duration]));
  Canvas.Flush;
  Device.Flip;
end;

procedure TGame.Shutdown;
begin
  Timer.Free;
  Canvas.Free;
  Device.Free;
  Audio.Free;
end;

procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
  // Terminate the application with esc
  if Key = VK_ESC then
  begin
    Terminate;
  end;
    if Key = VK_SPACE then
  begin
    Boom.Play;
  end;
  if Key = VK_RETURN then
  begin
    Music.Volume:= 0.5;
  end;
end;

end.


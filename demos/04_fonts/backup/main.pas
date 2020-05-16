unit main;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  phxTypes,
  phxClasses,
  phxMath,
  phxDevice,
  phxApplication,
  phxInput,
  phxGraphics,
  phxTexture,
  phxCanvas,
  phxFont;

type
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Fonts     : TPHXFontList;
    Input     : TPHXInput;

    Text_jn: WideString;
    Text_en: WideString;

    Width : Single;
    Height: Single;

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
  inherited;

end;

procedure TGame.Init;
begin
  // Create the device using the default provider
   Device:= TPHXDevice.Create;
   // Initialize the device
   Device.Initialize;

   // Create the timer
   Timer:= TPHXTimer.Create;

   Input:= TPHXInput.Create;

   // Create the device
   Canvas:= Device.CreateCanvas;

   Fonts := TPHXFontList.Create(Device, Canvas);
   Fonts.LoadFont('data/calibri12.phxfnt');
   Fonts.LoadFont('data/tahoma10_en.phxfnt');
   Fonts.LoadFont('data/tahoma10_jn.phxfnt');

   Text_en:= LoadTextFileUTF8('data/Lorem ipsum_en.txt');
   Text_jn:= LoadTextFileUTF8('data/Lorem ipsum_jn.txt');

   Width := 261;
   Height:= 400;
end;

procedure TGame.Update;
begin
   Timer.Update;
   Device.Update;
   Input.Update;

  if isLeft in Input.States then
  begin
    Width:= Width - Timer.FrameTime * 20;
  end;
  if isRight in Input.States then
  begin
    Width:= Width + Timer.FrameTime * 20;
  end;

  if isUp in Input.States then
  begin
    Height:= Height - Timer.FrameTime * 20;
  end;
  if isDown in Input.States then
  begin
    Height:= Height + Timer.FrameTime * 20;
  end;
end;

procedure TGame.Render;
begin
  var Rect: TRectf;
  begin
    Device.Clear;

    Fonts[0].TextOut(0, 0, Format('%d fps', [Timer.FrameRate]));

    Rect:= TRectf.Create(50, 50, 50 + Width, 50 + Height);

    Canvas.Texture:= nil;
    Canvas.Color  := clrSilver;
    Canvas.FilledRectangle(Rect);
    // Draw word wrapped text, this function will return true if all text fitted
    // inside the rectangle, false if it was cropped
    Fonts[1].WrapText(Rect, Text_en, alRight);

    Rect:= TRectf.Create(450, 50, 500 + Width, 50 + Height);

    Canvas.Texture:= nil;
    Canvas.Color  := clrSilver;
    Canvas.FilledRectangle(Rect);

    // Draw word wrapped text, this function will return true if all text fitted
    // inside the rectangle, false if it was cropped
    Fonts[2].WrapText(Rect, Text_jn, alLeft);

    Canvas.Flush;

    Device.Flip;
end;

procedure TGame.Shutdown;
begin
   Timer.Free;
   Canvas.Free;
   Device.Free;
   Fonts.Free;
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


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
  phxInput,
  phxTranslation;

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
    Fonts  : TPHXFontList;
  protected
    procedure KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates); override;
  public
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
procedure TGame.Init;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;
  Timer:= TPHXTimer.Create;
  // Create our canvas
  Canvas:= Device.CreateCanvas;
  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/Tahoma.phxfnt');
  Fonts.LoadFont('data/Tahoma.phxfnt');

  Translation.Load('data/translation_en.lang');

  // to raise a exception when accessing a missing tag, add TAG_EXCEPTION to the
  // defines
  Translation['missing_tag'];
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
var Text: WideString;
begin
  Text:= Translation['welcome'];

  Device.Clear;

  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, '1: English');
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 1, '2: Japanese');
  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 2, '3: Swedish');

  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 4, 'Current: ' + Translation.Language.Name);

  Fonts[1].TextOut(204, 4, Text);

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
  if Key = VK_1 then
  begin
    Fonts[1].LoadFromFile('data/Tahoma.phxfnt');

    Translation.Load('data/translation_en.lang');
  end;
  if Key = VK_2 then
  begin
    Fonts[1].LoadFromFile('data/Tahoma_jn.phxfnt');

    Translation.Load('data/translation_jn.lang');
  end;
  if Key = VK_3 then
  begin
    Fonts[1].LoadFromFile('data/Tahoma_sv.phxfnt');

    Translation.Load('data/translation_sv.lang');
  end;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Fonts.Free;
  Canvas.Free;
  Device.Free;
end;




end.

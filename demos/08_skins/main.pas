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
  phxSkin;

type

// Using the game template is the easy way to use Phoenix.
// Check the source in phxTemplate.pas to get an idea what the application class
// does.
//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device : TPHXDevice;
    Timer  : TPHXTimer;
    Canvas : TPHXCanvas;
    Font   : TPHXFont;
    Skin   : TPHXSkin;
    Transition: TPHXSkinTransition;
  protected

    procedure MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;
    procedure MouseReleased(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;
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


  Device.SetClearColor(0.8, 0.8, 0.8);

  Timer:= TPHXTimer.Create;
  Canvas:= Device.CreateCanvas;

  Font:= TPHXFont.Create(Device, Canvas);
  Font.LoadFromFile('data/calibri12.phxfnt');


  Skin:= TPHXSkin.Create(Device, Canvas);
  Skin.LoadFromFile('data/Default.phxskn');

  // The transition is used to transition between a current and a previous skin element
  Transition:= TPHXSkinTransition.Create;
  Transition.Duration:= 0.2;
  Transition.Element := Skin.Parts[teButtonNormal];
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Skin.Free;
  Font.Free;
  Canvas.Free;
  Device.Free;
  Transition.Free;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;

  Transition.Update(Timer.FrameTime);
end;


//------------------------------------------------------------------------------
procedure TGame.Render;
var Transform: TMatrix4f;
begin
  Device.Clear;

  Skin.Draw(teWindowNormal  , TVector3f.Create(100, 100, 0.02), TVector2f.Create(400, 400), 1.0);
  Skin.Draw(teButtonNormal  , TVector3f.Create(120, 185, 0.01), TVector2f.Create(111, 29), 1.0);
  Skin.Draw(teButtonDisabled, TVector3f.Create(240, 185, 0.01), TVector2f.Create(111, 29), 1.0);

  Skin.Draw(teEditNormal, TVector3f.Create(120, 140, 0.01), TVector2f.Create(220, 27), 1.0);

  Transform:= Matrix_CreateTranslation(600, 100, 0.001) * Matrix_CreateRotationZ(Timer.ElapsedTime * 90);

  Skin.Draw(teButtonNormal  , Transform, TVector2f.Create(111, 29), 1.0);

  Transition.Draw(Canvas, TVector3f.Create(120, 300, 0.01), TVector2f.Create(111, 29), 1.0);

  Skin.Draw(tePanelDisabled , TVector3f.Create(600, 200, 0.01), TVector2f.Create(100, 100), 1.0);
  Skin.Draw(tePanelNormal   , TVector3f.Create(600, 350, 0.01), TVector2f.Create(100, 100), 1.0);

  Canvas.Flush;

  Device.Flip;
end;




//------------------------------------------------------------------------------
procedure TGame.KeyPressed(Key: TPHXVirtualKey; Shift: TPHXShiftStates);
begin
  inherited;
  if Key = VK_W then Device.SetWireFrame(true);
end;

procedure TGame.MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin
  inherited;
  Transition.Element:= Skin.Parts[teButtonPressed];
end;

//------------------------------------------------------------------------------
procedure TGame.MouseReleased(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
begin
  inherited;
  Transition.Element:= Skin.Parts[teButtonNormal];
end;



end.

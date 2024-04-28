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
  phxTexture,

  phxParticle,
  phxParticlePresets,
  phxParticleAffectors;

type

// Using the game template is the easy way to use Phoenix.
// Check the source in phxTemplate.pas to get an idea what the application class
// does.
//------------------------------------------------------------------------------
TGame = class(TPHXApplication)
  private
    Device  : TPHXDevice;
    Timer   : TPHXTimer;
    Canvas  : TPHXCanvas;
    Textures: TPHXTextureList;
    Fonts   : TPHXFontList;
    Particles: TPHXParticleManager;
  protected
    procedure MousePressed(X: Integer; Y: Integer; Shift: TPHXShiftStates;  Button: TPHXMouseButton); override;
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
var System: TPHXParticleSystem;
begin
  Device:= TPHXDevice.Create;
  Device.Initialize('Phoenix Demo', 800, 600);;
  // This loads a new icon for the window

  Timer:= TPHXTimer.Create;
  // Create our canvas
  Canvas:= Device.CreateCanvas;

  Textures:= TPHXTextureList.Create(Device);
  Textures.LoadTexture('data/particles.png', 'particles.png');

  Fonts:= TPHXFontList.Create(Device, Canvas);
  Fonts.LoadFont('data/calibri12.phxfnt');

  Particles:= TPHXParticleManager.Create;
  // Allow a maximum of 10 systems at once
  Particles.Quota:= 10;
  // Assign the texture list to the particle system
  Particles.Textures:= Textures;
  //
  Particles.Effects.Add(TPHXParticlePresets.Fire(Particles));
  Particles.Effects.Add(TPHXParticlePresets.Jumpgate(Particles));

  System:= Particles.Spawn('Fire');
  System.Position:= Vector3f(400, 400, 0);
  System.Initialize;
end;

//------------------------------------------------------------------------------
procedure TGame.Update;
begin
  Timer.Update;

  Device.Update;

  Particles.Update(Timer.FrameTime);
end;

//------------------------------------------------------------------------------
procedure TGame.Render;
begin
  Device.Clear;

  Particles.Render(Canvas);

  Fonts[0].TextOut(4, 4 + Fonts[0].Height * 0, Format('%d fps', [Timer.FrameRate]));

  if Particles.Systems.Count  >0 then
  begin
//    Fonts[0].TextOut(4, 4 + Fonts[0].Height * 1, Format('Particles: %d', [Particles.Systems[0].Particles.Count]));
  end;

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TGame.Shutdown;
begin
  Timer.Free;
  Textures.Free;
  Canvas.Free;
  Device.Free;
end;


//------------------------------------------------------------------------------
procedure TGame.MousePressed(X, Y: Integer; Shift: TPHXShiftStates; Button: TPHXMouseButton);
var System: TPHXParticleSystem;
begin
  inherited;

  if Button = mbLeft then
  begin
    System:= Particles.Spawn('Jumpgate');
    if Assigned(System) then
    begin
      System.Position:= Vector3f(X, Y, 0);
      System.Initialize;
    end;
//  Particles.Update(Timer.FrameTime);
  end;
  if Button = mbRight then
  begin
    Particles.Clear;
  end;

end;

end.

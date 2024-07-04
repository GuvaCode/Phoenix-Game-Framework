unit uPreview;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus ,

  phxTexture,
  phxCanvas,
  phxApplication,

  phxParticle,
  phxDraw;

type

//------------------------------------------------------------------------------
TFrmEffectPreview = class(TFrame)
    PHXDraw1: TPHXDraw;
    PopupMenu1: TPopupMenu;
    Clearsystems1: TMenuItem;
    Addsystem1: TMenuItem;


    procedure PHXDraw1Init(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1Render(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1Update(Sender: TObject; Device: TPHXDevice);
    procedure PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure Clearsystems1Click(Sender: TObject);
    procedure Addsystem1Click(Sender: TObject);
    procedure PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,     Y: Integer);
    procedure PHXDraw1DblClick(Sender: TObject);
  private
    FEffect: TPHXParticleEffect;
    Canvas    : TPHXCanvas;
    Timer     : TPHXTimer;
    Background: TPHXTexture;
    MousePos  : TPoint;


    procedure DrawBackground(Texture: TPHXTexture);

    procedure SetEffect(const Value: TPHXParticleEffect);
    function GetSystems: TPHXParticleSystems;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    property Effect: TPHXParticleEffect read FEffect write SetEffect;
    property Systems: TPHXParticleSystems read GetSystems;
  end;

implementation

{$R *.dfm}

uses uActions, uMain;


// TFrmEffectBasic
//==============================================================================
constructor TFrmEffectPreview.Create(AOwner: TComponent);
begin
  inherited;
//  FSystem:= TPHXParticleSystem.Create(nil);
end;

//------------------------------------------------------------------------------
destructor TFrmEffectPreview.Destroy;
begin
//  FSystem.Free;

  Canvas.Free;

  Timer.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectPreview.SetEffect(const Value: TPHXParticleEffect);
var Index: Integer;
var System: TPHXParticleSystem;
begin
  if FEffect <> Value then
  begin
    FEffect:= Value;

    Systems.Clear;

    if Assigned(FEffect) then
    begin
      System:= FEffect.Manager.Spawn(FEffect, False);

      if Assigned(System) then
      begin
        System.PositionX:= PHXDraw1.Width  / 2;
        System.PositionY:= PHXDraw1.Height / 2;
        System.Initialize(FEffect);
      end;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmEffectPreview.PHXDraw1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.PHXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  MousePos.X:= X;
  MousePos.Y:= Y;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.PHXDraw1DblClick(Sender: TObject);
begin
  Addsystem1Click(Sender);
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.Addsystem1Click(Sender: TObject);
var System: TPHXParticleSystem;
begin
  if Assigned(Effect) then
  begin
    System:= Systems.Next;

    if Assigned(System) then
    begin
      System.PositionX:= MousePos.X;
      System.PositionY:= MousePos.Y;
      System.Initialize(FEffect);
    end else
    begin
      MessageDlg('The quota of particle systems is exeeded', mtInformation, [mbOK], 0);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.Clearsystems1Click(Sender: TObject);
begin
  Systems.Clear;
end;




//------------------------------------------------------------------------------
procedure TFrmEffectPreview.PHXDraw1Init(Sender: TObject; Device: TPHXDevice);
begin
  ModActions.Device:= Device;

  Background:= ModActions.LoadTexture('data\background.png');
  //Texture   := ModActions.LoadTexture('data\particles.png');

  Timer:= TPHXTimer.Create;

  Canvas:= Device.CreateCanvas;
end;
//------------------------------------------------------------------------------
procedure TFrmEffectPreview.PHXDraw1Update(Sender: TObject; Device: TPHXDevice);
var Index: Integer;
var System: TPHXParticleSystem;
begin
  Timer.Update;

  FrmMain.StatusBar1.Panels[0].Text:= FloatToStr(Timer.FrameTime);

  for Index := 0 to Systems.Count-1 do
  begin
    System:= Systems[Index];
    System.Update(Timer.FrameTime);
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.PHXDraw1Render(Sender: TObject; Device: TPHXDevice);
var Index: Integer;
var System: TPHXParticleSystem;
begin
  Device.Clear;

  if Assigned(Background) then
  begin
    DrawBackground(Background);
  end;

  for Index := 0 to Systems.Count-1 do
  begin
    System:= Systems[Index];
    System.Render(Canvas);
  end;

  Canvas.Flush;

  Device.Flip;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.DrawBackground(Texture: TPHXTexture);
var X, Y: Integer;
begin
  Canvas.Texture:= Texture;

  Y:=0;
  while Y < PHXDraw1.Height do
  begin
    X:= 0;
    while X < PHXDraw1.Width do
    begin
      Canvas.FilledRectangle(X, Y, X + Texture.Width, Y + Texture.Width);

      Inc(X, Texture.Width);
    end;
    Inc(Y, Texture.Height);
  end;

  Canvas.Flush;
end;

//------------------------------------------------------------------------------
procedure TFrmEffectPreview.SetParent(AParent: TWinControl);
begin
  inherited;

  if Assigned(AParent) then
  begin
    PHXDraw1.Initialize;
  end;
end;

//------------------------------------------------------------------------------
function TFrmEffectPreview.GetSystems: TPHXParticleSystems;
begin
  Result:= ModActions.Manager.Systems;
end;

end.

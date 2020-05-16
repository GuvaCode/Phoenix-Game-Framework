unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Menus, Vcl.ExtCtrls,

  phxParticle,

  uActions,
  uSystems,
  uPreview,
  uEffect,

  uGraph1f,
  uGraphRGB
  ;


type
  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    menuRecent: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Settings1: TMenuItem;
    About1: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    N6: TMenuItem;
    lblVersion: TMenuItem;
    StatusBar1: TStatusBar;
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton9: TToolButton;
    Presets1: TMenuItem;
    Fire1: TMenuItem;
    Panel1: TPanel;
    actPresetJumpgate1: TMenuItem;
    PanelClient: TPanel;
    PageControl2: TPageControl;
    TabSystems: TTabSheet;
    TabGraphColor: TTabSheet;
    TabGraphAlpha: TTabSheet;
    TabGraphScale: TTabSheet;
    TabGraphVelocity: TTabSheet;
    TabGraphSpin: TTabSheet;
    TabGraphEmissionCount: TTabSheet;
    TimerUpdate: TTimer;
    Splitter2: TSplitter;
    ools1: TMenuItem;
    Setsystemquota1: TMenuItem;
    Debug1: TMenuItem;
    Showgraphvalues1: TMenuItem;
    Splitter1: TSplitter;
    procedure TimerUpdateTimer(Sender: TObject);
  private
    FrmEffect : TFrmEffect;
    FrmPreview: TFrmEffectPreview;
    FrmSystems: TFrmSystems;

    FrmGraphAlpha: TFrmGraph1f;
    FrmGraphScale: TFrmGraph1f;
    FrmGraphColor: TFrmGraphRGB;

    procedure DocumentChanged(Document: TDocument);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses phxGraphics_FreeImage;

// TFrmMain
//==============================================================================
constructor TFrmMain.Create(AOwner: TComponent);
var Index: Integer;
begin
  inherited;

  ModActions:= TModActions.Create(Self);
  ModActions.DocumentChanged.Add(DocumentChanged);
  ModActions.Settings.Recent.Menu:= menuRecent;

  FrmEffect:= TFrmEffect.Create(Self);
  FrmEffect.Parent:= Panel1;
  FrmEffect.Align := alClient;

  FrmPreview:= TFrmEffectPreview.Create(Self);
  FrmPreview.Parent:= PanelClient;
  FrmPreview.Align := alClient;

  FrmSystems:= TFrmSystems.Create(Self);
  FrmSystems.Parent:= TabSystems;
  FrmSystems.Align := alClient;

  FrmGraphAlpha:= TFrmGraph1f.Create(Self);
  FrmGraphAlpha.Name:= 'FrmGraphAlpha';
  FrmGraphAlpha.Parent:= TabGraphAlpha;
  FrmGraphAlpha.Align:= alClient;
//  FrmGraphAlpha.OnChanged := GraphChanged;

  FrmGraphScale:= TFrmGraph1f.Create(Self);
  FrmGraphScale.Name:= 'FrmGraphScale';
  FrmGraphScale.Parent:= TabGraphScale;
  FrmGraphScale.Align:= alClient;
  //FrmGraphScale.OnChanged := GraphChanged;

  FrmGraphColor:= TFrmGraphRGB.Create(Self);
  FrmGraphColor.Parent:= TabGraphColor;
  FrmGraphColor.Align:= alClient;
// FrmGraphColor.OnChanged := GraphChanged;

  ModActions.LoadTexture('data\particles.png');
  ModActions.LoadTexture('data\P_particle_master.png');

  if ParamCount > 0 then
  begin
    for Index := 1  to ParamCount  do
    begin
      ModActions.Open( ParamStr(Index) );
    end;
  end else
  begin
    DocumentChanged(nil);
  end;

  lblVersion.Caption:= 'Effect version: ' + IntToStr( PHXPARTICLE_VERSION );
end;

 //------------------------------------------------------------------------------
destructor TFrmMain.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.DocumentChanged(Document: TDocument);
begin
  if Assigned(Document) then
  begin

    if dsChanged in Document.State then
    begin
      Caption:= 'phxParticleEditor - ' + Document.Name + '*';

      StatusBar1.Panels[1].Text:= 'Modified';
    end else
    begin
      Caption:= 'phxParticleEditor - ' + Document.Name;

      StatusBar1.Panels[1].Text:= '';
    end;

    FrmEffect .Effect:= Document.Effect;
    FrmPreview.Effect:= Document.Effect;

    FrmGraphAlpha.Graph:= Document.Effect.Graphs.Alpha;
    FrmGraphScale.Graph:= Document.Effect.Graphs.Scale;
    FrmGraphColor.Graph:= Document.Effect.Graphs.Color;
  end else
  begin
    Caption:= 'phxParticleEditor';

    FrmEffect .Effect:= nil;
    FrmPreview.Effect:= nil;

    FrmGraphAlpha.Graph:= nil;
    FrmGraphScale.Graph:= nil;
    FrmGraphColor.Graph:= nil;

    StatusBar1.Panels[1].Text:= '';
  end;

  FrmSystems.lwSystemsUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.TimerUpdateTimer(Sender: TObject);
begin
  FrmSystems.lwSystemsUpdate;
end;



end.

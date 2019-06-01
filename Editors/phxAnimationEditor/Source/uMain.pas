unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls, Vcl.ImgList, Vcl.StdCtrls,
  Vcl.Menus,

  phxImage,

  uActions,

  uImage.List,
  uImage.Patterns,

  uAnimation.Properties,
  uAnimation.Preview,
  uAnimation.Frames,
  uAnimation.Frame;

type

TFrmMain = class(TForm)
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    TabProperties: TTabSheet;
    TabFrames: TTabSheet;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    menuRecent: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    ools1: TMenuItem;
    Export1: TMenuItem;
    Import1: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    N6: TMenuItem;
    lblVersion: TMenuItem;
    ImageList1: TImageList;
    Panel1: TPanel;
    gbFrames: TGroupBox;
    gbPatterns: TGroupBox;
    TabImages: TTabSheet;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    FramestoXML1: TMenuItem;
    FramesfromXML1: TMenuItem;
    Setanimationduration1: TMenuItem;
    ImageEditor1: TMenuItem;
    N1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure SetDocument(Document: TDocument);
  public
    FrmPatterns: TFrmPatterns;
    FrmImageList: TFrmImageList;

    FrmAnimationProperties: TFrmAnimationProperties;
    FrmAnimationPreview   : TFrmAnimationPreview;
    FrmAnimationFrames    : TFrmAnimationFrames;
    FrmAnimationFrame     : TFrmAnimationFrame;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
constructor TFrmMain.Create(AOwner: TComponent);
var Index: Integer;
begin
  inherited;

  ModActions:= TModActions.Create(Self);
  ModActions.DocumentChanged.Add(SetDocument);
  ModActions.Settings.Recent.Menu:= menuRecent;



  FrmImageList:= TFrmImageList.Create(Self);
  FrmImageList.Parent:= TabImages;
  FrmImageList.Align := alClient;

  FrmPatterns:= TFrmPatterns.Create(Self);
  FrmPatterns.Parent:= gbPatterns;
  FrmPatterns.Align := alClient;

  FrmAnimationPreview:= TFrmAnimationPreview.Create(Self);
  FrmAnimationPreview.Parent:= TabProperties;
  FrmAnimationPreview.Align := alClient;

  FrmAnimationProperties:= TFrmAnimationProperties.Create(Self);
  FrmAnimationProperties.Parent:= TabProperties;
  FrmAnimationProperties.Align := alTop;

  FrmAnimationFrame:= TFrmAnimationFrame.Create(Self);
  FrmAnimationFrame.Parent:= TabFrames;
  FrmAnimationFrame.Align := alClient;

  FrmAnimationFrames:= TFrmAnimationFrames.Create(Self);
  FrmAnimationFrames.Parent:= gbFrames;
  FrmAnimationFrames.Align := alClient;
  FrmAnimationFrames.Patterns:= FrmPatterns;
  FrmAnimationFrames.Editor  := FrmAnimationFrame;

  PageControl1.ActivePageIndex:= 0;

  if ParamCount > 0 then
  begin
    for Index := 1  to ParamCount  do
    begin
      ModActions.Open( ParamStr(Index) );
    end;
  end else
  begin
    SetDocument(nil);
  end;

  lblVersion.Caption:= 'Animation version: ' + IntToStr( PHXIMAGE_VERSION );
end;

//------------------------------------------------------------------------------
destructor TFrmMain.Destroy;
begin
  ModActions.DocumentChanged.Remove(SetDocument);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.SetDocument(Document: TDocument);
begin
  if Assigned(Document) then
  begin

    if dsChanged in Document.State then
    begin
      Caption:= 'phxAnimationEditor - ' + Document.Name + '*';

      StatusBar1.Panels[1].Text:= 'Modified';
    end else
    begin
      Caption:= 'phxAnimationEditor - ' + Document.Name;

      StatusBar1.Panels[1].Text:= '';
    end;

    FrmPatterns.Image:= Document.Animation.Image;

    FrmAnimationProperties.Animation:= Document.Animation;
    FrmAnimationPreview   .Animation:= Document.Animation;
    FrmAnimationFrames    .Animation:= Document.Animation;
    FrmAnimationFrame     .Animation:= Document.Animation;
  end else
  begin
    Caption:= 'phxAnimationEditor';

    FrmPatterns.Image:= nil;

    FrmAnimationProperties.Animation:= nil;
    FrmAnimationPreview   .Animation:= nil;
    FrmAnimationFrames    .Animation:= nil;
    FrmAnimationFrame     .Animation:= nil;

    StatusBar1.Panels[1].Text:= '';
  end;

  FrmImageList.UpdateImages;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModActions.Close(True) = mrCancel then
  begin
    Action:= caNone;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if FrmAnimationFrames.Focused then
  begin
    FrmAnimationFrames.KeyDown(Key, Shift);
  end;
end;

end.

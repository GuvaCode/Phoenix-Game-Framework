unit uAnimation;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ExtCtrls,

  phxImage,

  uActions,

  uImage.Patterns,

  uAnimation.Properties,
  uAnimation.Preview,
  uAnimation.Frames,
  uAnimation.Frame, Vcl.StdCtrls, Vcl.Menus, Vcl.ImgList;

type
  TFrmAnimationEditor = class(TForm)
    PageControl1: TPageControl;
    TabProperties: TTabSheet;
    TabFrames: TTabSheet;
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionList1: TActionList;
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    Panel1: TPanel;
    Splitter2: TSplitter;
    gbFrames: TGroupBox;
    gbPatterns: TGroupBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    About1: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    N6: TMenuItem;
    lblVersion: TMenuItem;
    actFileClose: TAction;
    ActionImages: TImageList;
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
  private
    FAnimation : TPHXAnimation;
    FAnimations: TPHXAnimationList;

    FImage : TPHXImage;
    FImages: TPHXImageList;

    FrmPatterns: TFrmPatterns;

    FrmAnimationProperties: TFrmAnimationProperties;
    FrmAnimationPreview   : TFrmAnimationPreview;
    FrmAnimationFrames    : TFrmAnimationFrames;
    FrmAnimationFrame     : TFrmAnimationFrame;

    procedure SetAnimation(const Value: TPHXAnimation);
    procedure SetImage(const Value: TPHXImage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    function Execute(Image: TPHXImage): Boolean;

    property Animation: TPHXAnimation read FAnimation write SetAnimation;
    property Animations: TPHXAnimationList read FAnimations;
    property Image: TPHXImage read FImage write SetImage;
    property Images: TPHXImageList read FImages;
  end;

var
  FrmAnimationEditor: TFrmAnimationEditor;

implementation

{$R *.dfm}

resourcestring
  SImageMissing = 'The image "%s" is not loaded, do you want to open it?';


// TFrmAnimationEditor
//==============================================================================
constructor TFrmAnimationEditor.Create(AOwner: TComponent);
begin
  inherited;

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

  FImage := nil;
  FImages:= TPHXImageList.CreateEx;

  FAnimation := nil;
  FAnimations:= TPHXAnimationList.Create(FImages);

  lblVersion.Caption:= 'Animation version: ' + IntToStr( PHXANIMATION_VERSION );
end;

//------------------------------------------------------------------------------
destructor TFrmAnimationEditor.Destroy;
begin
  FAnimations.Free;
  FImages.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TFrmAnimationEditor.Execute(Image: TPHXImage): Boolean;
begin
  SetImage(Image);
  SetAnimation(FAnimation);

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.SetAnimation(const Value: TPHXAnimation);
begin
  FAnimation := Value;

  FrmAnimationProperties.Animation:= FAnimation;
  FrmAnimationPreview   .Animation:= FAnimation;
  FrmAnimationFrames    .Animation:= FAnimation;
  FrmAnimationFrame     .Animation:= FAnimation;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.SetImage(const Value: TPHXImage);
begin
  FrmPatterns.Image:= Value;

  FImage := Value;
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.actFileUpdate(Sender: TObject);
begin
  actSave.Enabled:= Assigned(FAnimation);
end;

var AnimationCounter: Integer = 1;

//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.actNewExecute(Sender: TObject);
var Name     : String;
var Animation: TPHXAnimation;
begin
  Name := Format('Animation%d', [AnimationCounter]);

  Animation:= TPHXAnimation.Create(nil);
  Animation.Name := Name;
  Animation.Image:= FImage;

  FAnimations.Add(Animation);

  SetAnimation(Animation);

  Inc(AnimationCounter);
end;


//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.actOpenExecute(Sender: TObject);
var Animation: TPHXAnimation;
begin
  if OpenDialog.Execute then
  begin
    Animation:= TPHXAnimation.Create(nil);
    Animation.LoadFromFile(OpenDialog.FileName);

    if Animation.ImageName = FImage.Name then
    begin
      Animation.Image:= FImage;
    end;

    FAnimations.Add(Animation);

    SetAnimation(Animation);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.actSaveExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Animation.SaveToFile(OpenDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationEditor.actFileCloseExecute(Sender: TObject);
begin
  ModalResult:= mrOk;
end;



end.

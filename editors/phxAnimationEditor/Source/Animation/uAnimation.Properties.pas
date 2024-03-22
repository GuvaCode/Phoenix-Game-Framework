unit uAnimation.Properties;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs,StdCtrls, Spin,
  phxImage, Menus;

type
  TFrmAnimationProperties = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edAnimationName: TEdit;
    edAnimationImage: TComboBox;
    edAnimationFrameRate: TFloatSpinEdit;
    edAnimationLooped: TCheckBox;
    Label4: TLabel;
    Label2: TLabel;
    edAnimationAuthor: TEdit;
    edAnimationVersion: TEdit;
    edAnimationComment: TMemo;
    Label6: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    procedure edAnimationNameChange(Sender: TObject);
    procedure edAnimationImageChange(Sender: TObject);
    procedure edAnimationLoopedClick(Sender: TObject);
    procedure edAnimationFrameRateChange(Sender: TObject);
    procedure edAnimationAuthorChange(Sender: TObject);
    procedure edAnimationVersionChange(Sender: TObject);
    procedure edAnimationCommentChange(Sender: TObject);
  private
    FAnimation: TPHXAnimation;
    procedure EnableEditors(Enabled: Boolean);
    procedure edAnimationImageUpdate(Sender: TObject);
    procedure SetAnimation(const Value: TPHXAnimation);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Animation: TPHXAnimation read FAnimation write SetAnimation;
  end;

implementation

{$R *.lfm}

uses uActions;

// TFrmAnimationProperties
//------------------------------------------------------------------------------
constructor TFrmAnimationProperties.Create(AOwner: TComponent);
begin
  inherited;
  EnableEditors(False);
end;
//------------------------------------------------------------------------------
destructor TFrmAnimationProperties.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.SetAnimation(const Value: TPHXAnimation);
begin
  FAnimation := Value;

  edAnimationImageUpdate(Self);

  edAnimationName     .OnChange:= nil;
  edAnimationAuthor   .OnChange:= nil;
  edAnimationVersion  .OnChange:= nil;
  edAnimationComment  .OnChange:= nil;
  edAnimationImage    .OnChange:= nil;
  edAnimationFrameRate.OnChange:= nil;
  edAnimationLooped   .OnClick:= nil;

  if Assigned(FAnimation) then
  begin
    edAnimationName     .Text   := Animation.Name;
    edAnimationAuthor   .Text   := Animation.Author;
    edAnimationVersion  .Text   := Animation.Version;
    edAnimationComment  .Text   := Animation.Comment;
    edAnimationImage    .Text   := Animation.ImageName;
    edAnimationFrameRate.Value  := Animation.FrameRate;
    edAnimationLooped   .Checked:= Animation.Looped;

    edAnimationName     .OnChange:= edAnimationNameChange;
    edAnimationAuthor   .OnChange:= edAnimationAuthorChange;
    edAnimationVersion  .OnChange:= edAnimationVersionChange;
    edAnimationComment  .OnChange:= edAnimationCommentChange;
    edAnimationImage    .OnChange:= edAnimationImageChange;
    edAnimationFrameRate.OnChange:= edAnimationFrameRateChange;
    edAnimationLooped   .OnClick := edAnimationLoopedClick;

    EnableEditors(True);
  end else
  begin
    edAnimationName     .Text   := '';
    edAnimationAuthor   .Text   := '';
    edAnimationVersion  .Text   := '';
    edAnimationComment  .Text   := '';
    edAnimationImage    .Text   := '';
    edAnimationFrameRate.Value  := 0;

    EnableEditors(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.EnableEditors(Enabled: Boolean);
const EnabledColors: array [Boolean] of TColor = (clBtnFace, clWindow);
begin
  edAnimationName.Enabled := Enabled;
  edAnimationName.Color := EnabledColors[Enabled];

  edAnimationAuthor.Enabled := Enabled;
  edAnimationAuthor.Color := EnabledColors[Enabled];

  edAnimationVersion.Enabled := Enabled;
  edAnimationVersion.Color := EnabledColors[Enabled];

  edAnimationComment.Enabled := Enabled;
  edAnimationComment.Color := EnabledColors[Enabled];

  edAnimationImage.Enabled := Enabled;
  edAnimationImage.Color := EnabledColors[Enabled];

  edAnimationFrameRate.Enabled := Enabled;
  edAnimationFrameRate.Color := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationImageUpdate(Sender: TObject);
var Image: TPHXImage;
begin
  edAnimationImage.Items.BeginUpdate;
  edAnimationImage.Items.Clear;

  edAnimationImage.AddItem('', nil);
  for Image in ModActions.Images do
  begin
    edAnimationImage.AddItem(Image.Name, Image);
  end;

  edAnimationImage.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edAnimationName.Text;

  if Value <> FAnimation.Name then
  begin
    FAnimation.Name:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationVersionChange(Sender: TObject);
var Value: String;
begin
  Value:= edAnimationVersion.Text;

  if Value <> FAnimation.Version then
  begin
    FAnimation.Version:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationAuthorChange(Sender: TObject);
var Value: String;
begin
  Value:= edAnimationAuthor.Text;

  if Value <> FAnimation.Author then
  begin
    FAnimation.Author:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationCommentChange(Sender: TObject);
var Value: String;
begin
  Value:= edAnimationComment.Text;

  if Value <> FAnimation.Comment then
  begin
    FAnimation.Comment:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationImageChange(Sender: TObject);
var Value: TPHXImage;
begin
  if edAnimationImage.ItemIndex > 0 then
  begin
    Value:= TPHXImage(edAnimationImage.Items.Objects[edAnimationImage.ItemIndex]);
  end else
  begin
    Value:= nil;
  end;

  if Value <> FAnimation.Image then
  begin
    FAnimation.Image:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationFrameRateChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edAnimationFrameRate.Value);

  if Value <> FAnimation.FrameRate then
  begin
    FAnimation.FrameRate:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmAnimationProperties.edAnimationLoopedClick(Sender: TObject);
var Value: Boolean;
begin
  Value:= edAnimationLooped.Checked;

  if Value <> FAnimation.Looped then
  begin
    FAnimation.Looped:= Value;

    ModActions.Document.Changed;
  end;
end;




end.

unit uSkin.Properties;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  uActions,

  phxSkin,
  phxGraphics;

type

//------------------------------------------------------------------------------
TFrmSkinProperties = class(TFrame)
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    edName: TEdit;
    edAuthor: TEdit;
    edComment: TMemo;
    edVersion: TEdit;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edTextureWidth: TEdit;
    edTextureHeight: TEdit;
    edTextureFormat: TEdit;
    procedure edNameChange(Sender: TObject);
    procedure edAuthorChange(Sender: TObject);
    procedure edVersionChange(Sender: TObject);
    procedure edCommentChange(Sender: TObject);
  private
    FSkin: TPHXSkin;
    FActive: Boolean;

    procedure EnableControls(const Enabled: Boolean);

    procedure SetSkin(const Value: TPHXSkin);
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    Property Skin: TPHXSkin read FSkin write SetSkin;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

{$R *.dfm}

uses TypInfo;

// TFrmSkinProperties
//------------------------------------------------------------------------------
constructor TFrmSkinProperties.Create(AOwner: TComponent);
begin
  inherited;
  EnableControls(False);
end;


//------------------------------------------------------------------------------
procedure TFrmSkinProperties.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName.Enabled:= Enabled;
  edName.Color  := EnabledColors[Enabled];

  edAuthor.Enabled:= Enabled;
  edAuthor.Color  := EnabledColors[Enabled];

  edVersion.Enabled:= Enabled;
  edVersion.Color  := EnabledColors[Enabled];

  edComment.Enabled:= Enabled;
  edComment.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmSkinProperties.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinProperties.SetSkin(const Value: TPHXSkin);
begin
  FSkin := Value;

  if Assigned(Skin) then
  begin
    EnableControls(True);

    edName       .Text   := Skin.Name;
    edAuthor     .Text   := Skin.Author;
    edVersion    .Text   := Skin.Version;
    edComment    .Text   := Skin.Comment;

    edTextureWidth     .Text := IntToStr(Skin.Texture.Width);
    edTextureHeight    .Text := IntToStr(Skin.Texture.Height);
    edTextureFormat    .Text := GetEnumName(TypeInfo(TPHXPixelFormat), Integer(Skin.Texture.Format)) ;
  end else
  begin
    EnableControls(False);

    edName   .Text   := '';
    edAuthor .Text   := '';
    edVersion.Text   := '';
    edComment.Text   :='';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinProperties.edNameChange(Sender: TObject);
begin
  if Skin = nil then Exit;

  if Skin.Name <> edName.Text then
  begin
    Skin.Name:= edName.Text;

    ModActions.Document.Changed;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmSkinProperties.edAuthorChange(Sender: TObject);
begin
  if Skin = nil then Exit;

  if Skin.Author <> edAuthor.Text then
  begin
    Skin.Author:= edAuthor.Text;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinProperties.edVersionChange(Sender: TObject);
begin
  if Skin = nil then Exit;

  if Skin.Version <> edVersion.Text then
  begin
    Skin.Version:= edVersion.Text;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinProperties.edCommentChange(Sender: TObject);
begin
  if Skin = nil then Exit;

  if Skin.Comment <> edComment.Text then
  begin
    Skin.Comment:= edComment.Text;

    ModActions.Document.Changed;
  end;
end;









end.

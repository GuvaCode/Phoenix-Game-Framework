unit uImage.Properties;

interface

uses
  SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  phxGraphics,
  phxImage;

type

//------------------------------------------------------------------------------
TFrmImageProperties = class(TFrame)
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edTextureWidth: TEdit;
    edTextureHeight: TEdit;
    edTextureFormat: TEdit;
    btnTexture: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    edName: TEdit;
    edAuthor: TEdit;
    edComment: TMemo;
    edVersion: TEdit;
    procedure edNameChange(Sender: TObject);
    procedure edAuthorChange(Sender: TObject);
    procedure edVersionChange(Sender: TObject);
    procedure edCommentChange(Sender: TObject);
  private
    FImage: TPHXImage;

    procedure EnableControls(const Enabled: Boolean);

    procedure SetImage(const Value: TPHXImage);
  public
    constructor Create(AOwner: TComponent); override;

    Property Image: TPHXImage read FImage write SetImage;
  end;

implementation

{$R *.dfm}

uses TypInfo;

// TFrmImageProperties
//------------------------------------------------------------------------------
constructor TFrmImageProperties.Create(AOwner: TComponent);
begin
  inherited;

  EnableControls(False);
end;



//------------------------------------------------------------------------------
procedure TFrmImageProperties.EnableControls(const Enabled: Boolean);
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

  btnTexture.Enabled:= Enabled;
end;


//------------------------------------------------------------------------------
procedure TFrmImageProperties.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  if Assigned(Image) then
  begin
    EnableControls(True);

    edName       .Text   := Image.Name;
    edAuthor     .Text   := Image.Author;
    edVersion    .Text   := Image.Version;
    edComment    .Text   := Image.Comment;

    edTextureWidth     .Text := IntToStr(Image.Texture.Width);
    edTextureHeight    .Text := IntToStr(Image.Texture.Height);
    edTextureFormat    .Text := GetEnumName(TypeInfo(TPHXPixelFormat), Integer(Image.Texture.Format)) ;
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
procedure TFrmImageProperties.edNameChange(Sender: TObject);
begin
  if Image = nil then Exit;

  if Image.Name <> edName.Text then
  begin
    Image.Name:= edName.Text;
  end;
end;



//------------------------------------------------------------------------------
procedure TFrmImageProperties.edAuthorChange(Sender: TObject);
begin
  if Image = nil then Exit;

  if Image.Author <> edAuthor.Text then
  begin
    Image.Author:= edAuthor.Text;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmImageProperties.edVersionChange(Sender: TObject);
begin
  if Image = nil then Exit;

  if Image.Version <> edVersion.Text then
  begin
    Image.Version:= edVersion.Text;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmImageProperties.edCommentChange(Sender: TObject);
begin
  if Image = nil then Exit;

  if Image.Comment <> edComment.Text then
  begin
    Image.Comment:= edComment.Text;
  end;
end;



end.

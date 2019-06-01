unit uSkin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls,

  phxTypes,

  phxGraphics,
  phxSkin;


type
  TFrmSkin = class(TFrame)
    btnTexture: TButton;
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edCompressor: TComboBox;
    edWidth: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edHeight: TEdit;
    edFormat: TEdit;
    Label5: TLabel;
    procedure btnTextureClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
  private
    FSkin: TPHXSkin;
    FOnChanged: TNotifyEvent;
    procedure SetSkin(const Value: TPHXSkin);
    procedure EnableEvents(Bind: Boolean);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableControls(Enabled: Boolean);

    Property Skin: TPHXSkin read FSkin write SetSkin;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

{ TFrmSkin }

uses uTexture, TypInfo;

//------------------------------------------------------------------------------
constructor TFrmSkin.Create(AOwner: TComponent);
begin
  inherited;

  edCompressor.Items.BeginUpdate;
  edCompressor.Items.Clear;
  edCompressor.Items.Add('');
    GraphicFormats.ListFormats(edCompressor.Items);
  edCompressor.Items.EndUpdate;
end;




//------------------------------------------------------------------------------
procedure TFrmSkin.SetSkin(const Value: TPHXSkin);
begin
  EnableEvents(False);

  FSkin := Value;

  if Assigned(FSkin) then
  begin
    EnableControls(True);

    edName      .Text     := FSkin.Name;
    edWidth     .Text     := IntToStr(FSkin.Width);
    edHeight    .Text     := IntToStr(FSkin.Height);
//    edCompressor.ItemIndex:= GraphicCompressors.IndexOf(FImage.Compressor)+1;
    edFormat    .Text     := GetEnumName(TypeInfo(TPHXPixelFormat), Integer(FSkin.Texture.Format)) ;

    edCompressor.ItemIndex:= edCompressor.Items.IndexOf( String(FSkin.Texture.GraphicFormat.Extension) );
    //if Assigned(FSkin.Texture.GraphicFormat.Filer) then
    //begin
    //end else
    //begin
    //   edCompressor.Text:= '';
    //end;


    EnableEvents(True);
  end else
  begin
    EnableControls(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkin.edNameChange(Sender: TObject);
begin
  if Assigned(Skin) then
  begin
    Skin.Name:= edName.Text;

    if (edCompressor.ItemIndex <= 0) then
    begin
     // Skin.Texture.GraphicFormat.Extension:= '';
    end else
    begin
   //   Skin.Texture.GraphicFormat.Extension:= '';
//      Skin.Texture.GraphicFormat.Filer= TPHXGraphicExporter(  edCompressor.Items.Objects[edCompressor.ItemIndex - 1] );
    end;
  end;

  if Assigned(OnChanged) then OnChanged(Self);
end;

//------------------------------------------------------------------------------
procedure TFrmSkin.EnableEvents(Bind: Boolean);
begin
  if Bind then
  begin
    edName      .OnChange:= edNameChange;
    edCompressor.OnChange:= edNameChange;
  end else
  begin
    edName      .OnChange:= nil;
    edCompressor.OnChange:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkin.EnableControls(Enabled: Boolean);
begin
  edName        .Enabled:= Enabled;
  edCompressor  .Enabled:= Enabled;
  btnTexture    .Enabled:= Enabled;
  if Enabled then
  begin
    edName      .Color:= clWindow;
    edCompressor.Color:= clWindow;
  end else
  begin
    edName       .Color:= clBtnFace;
    edCompressor .Color:= clBtnFace;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkin.btnTextureClick(Sender: TObject);
begin
  FrmTexture.Execute(Skin.Texture);

end;

end.

unit uFont.Properties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin, ExtCtrls, TypInfo,


  phxGraphics,
  phxFont,
  phxFontEx, JvToolEdit;

type
  TFrmFontProperties = class(TFrame)
    GroupBox1: TGroupBox;
    edName: TEdit;
    edSize: TJvSpinEdit;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    edHeight: TJvSpinEdit;
    Label5: TLabel;
    edAscent: TJvSpinEdit;
    Label6: TLabel;
    edDescent: TJvSpinEdit;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    edTextureWidth: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    edTextureHeight: TEdit;
    edTextureFormat: TEdit;
    Label13: TLabel;
    edBold: TCheckBox;
    edItalic: TCheckBox;
    edAuthor: TEdit;
    Label3: TLabel;
    btnTexture: TButton;
    Label7: TLabel;
    edOffset: TJvSpinEdit;
    Label8: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    GroupBox3: TGroupBox;
    Label10: TLabel;
    Label14: TLabel;
    edWrapMode: TComboBox;
    Label9: TLabel;
    Label15: TLabel;
    edWrapChars: TJvComboEdit;
    edWrapStart: TJvComboEdit;
    edWrapEnd: TJvComboEdit;
    procedure edFontChange(Sender: TObject);
    procedure btnTextureClick(Sender: TObject);
    procedure edHeightChange(Sender: TObject);
    procedure edAscentChange(Sender: TObject);
    procedure edDescentChange(Sender: TObject);
    procedure edOffsetChange(Sender: TObject);
    procedure edWrapModeChange(Sender: TObject);
    procedure edWrapCharsChange(Sender: TObject);
    procedure edWrapStartChange(Sender: TObject);
    procedure edWrapEndChange(Sender: TObject);
    procedure Label14Click(Sender: TObject);
    procedure edWrapCharsBrowse(Sender: TObject);
    procedure edWrapStartBrowse(Sender: TObject);
    procedure edWrapEndBrowse(Sender: TObject);
  private
    FFont: TPHXFont;

    procedure SetFont(const Value: TPHXFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnableEvents(const Enabled: Boolean);
    procedure EnableControls(const Enabled: Boolean);

    Property Font: TPHXFont read FFont write SetFont;
  end;

implementation

{$R *.dfm}

uses uActions, uFont.WrapChars;

// TFrmFont
//------------------------------------------------------------------------------
constructor TFrmFontProperties.Create(AOwner: TComponent);
begin
  inherited;
end;

//------------------------------------------------------------------------------
destructor TFrmFontProperties.Destroy;
begin

  inherited;
end;


//------------------------------------------------------------------------------
procedure TFrmFontProperties.SetFont(const Value: TPHXFont);
begin
  FFont := Value;

  EnableEvents(False);

  if Assigned(Font) then
  begin
    EnableControls(True);

    edName     .Text   := Font.Name;
    edAuthor   .Text   := Font.Author;
    edSize     .Value  := Font.Size;
    edBold     .Checked:= fsBold in Font.Style;
    edItalic   .Checked:= fsItalic in Font.Style;

    edHeight      .Value := Font.Metric.Height;
    edOffset      .Value := Font.Metric.Offset;
    edAscent      .Value := Font.Metric.Ascent;
    edDescent     .Value := Font.Metric.Descent;

    edTextureWidth     .Text := IntToStr(Font.Texture.Width);
    edTextureHeight    .Text := IntToStr(Font.Texture.Height);
    edTextureFormat    .Text := GetEnumName(TypeInfo(TPHXPixelFormat), Integer(Font.Texture.Format)) ;

    edWrapMode.ItemIndex:= Ord(Font.WrapMode);
    edWrapChars.Text     := Font.WrapChars;
    edWrapStart.Text     := Font.WrapStart;
    edWrapEnd  .Text     := Font.WrapEnd;

    edWrapChars.Enabled:= (Font.WrapMode = wmWord);

    EnableEvents(True);
  end else
  begin
    EnableControls(False);

    edName     .Text   := '';
    edAuthor   .Text   := '';
    edSize     .Value  := 0;
    edBold     .Checked:= False;
    edItalic   .Checked:= False;

    edHeight      .Value := 0;
    edAscent      .Value := 0;
    edDescent     .Value := 0;
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edFontChange(Sender: TObject);
begin
  Font.Name   := edName.Text;
  Font.Author := edAuthor   .Text   ;
  Font.Size   := Round(edSize     .Value)  ;
end;

{$REGION 'Edit events'}

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edHeightChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edHeight.Value);

  if Assigned(Font) and (Font.Metric.Height <> Value) then
  begin
    Font.Metric.Height:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edOffsetChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edOffset.Value);

  if Assigned(Font) and (Font.Metric.Offset <> Value) then
  begin
    Font.Metric.Offset:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edAscentChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edAscent.Value);

  if Assigned(Font) and (Font.Metric.Ascent <> Value) then
  begin
    Font.Metric.Ascent:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edDescentChange(Sender: TObject);
var Value: Integer;
begin
  Value:= Round(edDescent.Value);

  if Assigned(Font) and (Font.Metric.Descent <> Value) then
  begin
    Font.Metric.Descent:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapModeChange(Sender: TObject);
var Value: TPHXFontWrap;
begin
  Value:= TPHXFontWrap(edWrapMode.ItemIndex);

  if Assigned(Font) and (Font.WrapMode <> Value) then
  begin
    Font.WrapMode:= Value;

    edWrapChars.Enabled:= Value = wmWord;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapCharsChange(Sender: TObject);
var Value: WideString;
begin
  Value:= edWrapChars.Text;

  if Assigned(Font) and (Font.WrapChars <> Value) then
  begin
    Font.WrapChars:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapCharsBrowse(Sender: TObject);
begin
  FrmWrapCharacters.Characters:= Font.WrapChars;

  if FrmWrapCharacters.Execute('Select wrap characters') then
  begin
    Font.WrapChars:= FrmWrapCharacters.Characters;

    edWrapChars.Text:= Font.WrapChars;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapStartChange(Sender: TObject);
var Value: WideString;
begin
  Value:= edWrapStart.Text;

  if Assigned(Font) and (Font.WrapStart <> Value) then
  begin
    Font.WrapStart:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapStartBrowse(Sender: TObject);
begin
  FrmWrapCharacters.Characters:= Font.WrapStart;

  if FrmWrapCharacters.Execute('Select start characters') then
  begin
    Font.WrapStart:= FrmWrapCharacters.Characters;

    edWrapStart.Text:= Font.WrapStart;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapEndChange(Sender: TObject);
var Value: WideString;
begin
  Value:= edWrapEnd.Text;

  if Assigned(Font) and (Font.WrapEnd <> Value) then
  begin
    Font.WrapEnd:= Value;

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.edWrapEndBrowse(Sender: TObject);
begin
  FrmWrapCharacters.Characters:= Font.WrapEnd;

  if FrmWrapCharacters.Execute('Select end characters') then
  begin
    Font.WrapEnd:= FrmWrapCharacters.Characters;

    edWrapEnd.Text:= Font.WrapEnd;

    ModActions.Document.Changed;
  end;
end;


{$ENDREGION}

//------------------------------------------------------------------------------
procedure TFrmFontProperties.EnableControls(const Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName.Enabled:= Enabled;
  edName.Color  := EnabledColors[Enabled];

  edAuthor.Enabled:= Enabled;
  edAuthor.Color  := EnabledColors[Enabled];

  edSize.Enabled:= Enabled;
  edSize.Color  := EnabledColors[Enabled];


  edHeight.Enabled:= Enabled;
  edHeight.Color  := EnabledColors[Enabled];

  edOffset.Enabled:= Enabled;
  edOffset.Color  := EnabledColors[Enabled];

  edAscent.Enabled:= Enabled;
  edAscent.Color  := EnabledColors[Enabled];

  edDescent.Enabled:= Enabled;
  edDescent.Color  := EnabledColors[Enabled];

  edBold   .Enabled:= Enabled;
  edItalic .Enabled:= Enabled;

  edWrapMode.Enabled:= Enabled;
  edWrapMode.Color  := EnabledColors[Enabled];

  edWrapChars.Enabled:= Enabled;
  edWrapChars.Color  := EnabledColors[Enabled];

  edWrapStart.Enabled:= Enabled;
  edWrapStart.Color  := EnabledColors[Enabled];

  edWrapEnd.Enabled:= Enabled;
  edWrapEnd.Color  := EnabledColors[Enabled];

  btnTexture.Enabled:= Enabled;
end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.EnableEvents(const Enabled: Boolean);
var EnabledEvents: Array[Boolean] of TNotifyEvent;
begin
  EnabledEvents[True ]:= edFontChange;
  EnabledEvents[False]:= nil;

  edName     .OnChange:= EnabledEvents[Enabled];
  edAuthor   .OnChange:= EnabledEvents[Enabled];
  edSize     .OnChange:= EnabledEvents[Enabled];
  edBold     .OnClick:= EnabledEvents[Enabled];
  edItalic   .OnClick:= EnabledEvents[Enabled];


  if Enabled then
  begin
    edHeight .OnChange:= edHeightChange;
    edOffset .OnChange:= edOffsetChange;
    edAscent .OnChange:= edAscentChange;
    edDescent.OnChange:= edDescentChange;
  end else
  begin
    edHeight .OnChange:= nil;
    edOffset .OnChange:= nil;
    edAscent .OnChange:= nil;
    edDescent.OnChange:= nil;
  end;

end;

procedure TFrmFontProperties.Label14Click(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TFrmFontProperties.btnTextureClick(Sender: TObject);
begin
//  FrmTexture.Execute(Image);

end;

end.

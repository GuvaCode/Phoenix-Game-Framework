unit uGenerator.Debug;

interface

uses
   LclType, LclIntf, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs,

  phxFont,

  uGenerator, StdCtrls, ExtCtrls;

type
  TFrmGeneratorDebug = class(TForm)
    GroupBox5: TGroupBox;
    Label3: TLabel;
    PaintBox1: TPaintBox;
    cbCharacter: TComboBox;
    procedure cbCharacterChange(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    Glyph: TGlyph;

    FGenerator: TPHXFontGenerator;
    procedure SetGenerator(const Value: TPHXFontGenerator);
  public
    property Generator: TPHXFontGenerator read FGenerator write SetGenerator;
  end;

var
  FrmGeneratorDebug: TFrmGeneratorDebug;

implementation

{$R *.dfm}

procedure TFrmGeneratorDebug.SetGenerator(const Value: TPHXFontGenerator);
var Index: Integer;
begin
  FGenerator := Value;

  cbCharacter.Items.BeginUpdate;
  cbCharacter.Items.Clear;
  for Index := 0 to FGenerator.Glyphs.Count - 1 do
  begin
    cbCharacter.Items.Add( Chr(FGenerator.Glyphs[Index].Code) );
  end;
  cbCharacter.Items.EndUpdate;
  cbCharacter.ItemIndex:= cbCharacter.Items.IndexOf(cbCharacter.Text);
  cbCharacterChange(nil);
end;


procedure TFrmGeneratorDebug.cbCharacterChange(Sender: TObject);
var Char : TPHXCharacter;
begin
  if cbCharacter.ItemIndex >= 0 then
  begin
    Glyph:= Generator.Glyphs[cbCharacter.ItemIndex];

    PaintBox1.Invalidate;

   // Image4.Picture.Assign(Glyph.Font);
  //  Image1.Picture.Assign(Glyph.Mask);

    Char.ID     := Glyph.Code;
    Char.X      := Glyph.X;
    Char.Y      := Glyph.Y;
    Char.Width  := Glyph.Width;
    Char.Height := Glyph.Height;
    Char.Offset.X:= Glyph.Offset.X;
    Char.Offset.Y:= Glyph.Offset.Y;
    Char.Advance:= Glyph.Advance;

    Label3.Caption:=
    'X: ' + IntToStr(Char.X) + sLineBreak +
    'Y: ' + IntToStr(Char.Y) + sLineBreak +
    'Width: ' + IntToStr(Char.Width) + sLineBreak +
    'Height: ' + IntToStr(Char.Height) + sLineBreak +
    'XOffset: ' + IntToStr(Char.Offset.X) + sLineBreak +
    'XAdvance: ' + IntToStr(Char.Advance) + sLineBreak +
    'YOffset: ' + IntToStr(Char.Offset.Y);
  end else
  begin
    Glyph.Mask:= nil;
    Glyph.Font:= nil;
  end;
  PaintBox1.Invalidate;
end;

procedure TFrmGeneratorDebug.PaintBox1Paint(Sender: TObject);
var R: TREct;
begin
  if Assigned(Glyph.Mask) and Assigned(Glyph.Font) then
  with PaintBox1.Canvas do
  begin
    R.Left  := 0;
    R.Top   := 0;
    R.Right := 0 + Glyph.Font.Width  * 4;
    R.Bottom:= 0 + Glyph.Font.Height * 4;

    StretchDraw(R, Glyph.Font);

    R.Left  := R.Right  + 4;
    R.Top   := 0;
    R.Right := R.Left + Glyph.Mask.Width  * 4;
    R.Bottom:= R.Top  + Glyph.Mask.Height * 4;

    StretchDraw(R, Glyph.Mask);
  end;

end;


end.

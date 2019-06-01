unit uGenerator.Characters;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs,

  Generics.Collections;

{$REGION 'Unicode Character Database'}

// Unicode Character Database
// http://www.unicode.org/Public/UNIDATA/Blocks.txt
//  {.*}..{.*}; {.*}

const UnicodeCharacterDatabase : array[0..196] of record
  First: Cardinal;
  Last : Cardinal;
  Name : String;
  end =
(
  (First: $0000; Last: $007F; Name : 'Basic Latin'),
  (First: $0080; Last: $00FF; Name : 'Latin-1 Supplement'),
  (First: $0100; Last: $017F; Name : 'Latin Extended-A'),
  (First: $0180; Last: $024F; Name : 'Latin Extended-B'),
  (First: $0250; Last: $02AF; Name : 'IPA Extensions'),
  (First: $02B0; Last: $02FF; Name : 'Spacing Modifier Letters'),
  (First: $0300; Last: $036F; Name : 'Combining Diacritical Marks'),
  (First: $0370; Last: $03FF; Name : 'Greek and Coptic'),
  (First: $0400; Last: $04FF; Name : 'Cyrillic'),
  (First: $0500; Last: $052F; Name : 'Cyrillic Supplement'),
  (First: $0530; Last: $058F; Name : 'Armenian'),
  (First: $0590; Last: $05FF; Name : 'Hebrew'),
  (First: $0600; Last: $06FF; Name : 'Arabic'),
  (First: $0700; Last: $074F; Name : 'Syriac'),
  (First: $0750; Last: $077F; Name : 'Arabic Supplement'),
  (First: $0780; Last: $07BF; Name : 'Thaana'),
  (First: $07C0; Last: $07FF; Name : 'NKo'),
  (First: $0800; Last: $083F; Name : 'Samaritan'),
  (First: $0900; Last: $097F; Name : 'Devanagari'),
  (First: $0980; Last: $09FF; Name : 'Bengali'),
  (First: $0A00; Last: $0A7F; Name : 'Gurmukhi'),
  (First: $0A80; Last: $0AFF; Name : 'Gujarati'),
  (First: $0B00; Last: $0B7F; Name : 'Oriya'),
  (First: $0B80; Last: $0BFF; Name : 'Tamil'),
  (First: $0C00; Last: $0C7F; Name : 'Telugu'),
  (First: $0C80; Last: $0CFF; Name : 'Kannada'),
  (First: $0D00; Last: $0D7F; Name : 'Malayalam'),
  (First: $0D80; Last: $0DFF; Name : 'Sinhala'),
  (First: $0E00; Last: $0E7F; Name : 'Thai'),
  (First: $0E80; Last: $0EFF; Name : 'Lao'),
  (First: $0F00; Last: $0FFF; Name : 'Tibetan'),
  (First: $1000; Last: $109F; Name : 'Myanmar'),
  (First: $10A0; Last: $10FF; Name : 'Georgian'),
  (First: $1100; Last: $11FF; Name : 'Hangul Jamo'),
  (First: $1200; Last: $137F; Name : 'Ethiopic'),
  (First: $1380; Last: $139F; Name : 'Ethiopic Supplement'),
  (First: $13A0; Last: $13FF; Name : 'Cherokee'),
  (First: $1400; Last: $167F; Name : 'Unified Canadian Aboriginal Syllabics'),
  (First: $1680; Last: $169F; Name : 'Ogham'),
  (First: $16A0; Last: $16FF; Name : 'Runic'),
  (First: $1700; Last: $171F; Name : 'Tagalog'),
  (First: $1720; Last: $173F; Name : 'Hanunoo'),
  (First: $1740; Last: $175F; Name : 'Buhid'),
  (First: $1760; Last: $177F; Name : 'Tagbanwa'),
  (First: $1780; Last: $17FF; Name : 'Khmer'),
  (First: $1800; Last: $18AF; Name : 'Mongolian'),
  (First: $18B0; Last: $18FF; Name : 'Unified Canadian Aboriginal Syllabics Extended'),
  (First: $1900; Last: $194F; Name : 'Limbu'),
  (First: $1950; Last: $197F; Name : 'Tai Le'),
  (First: $1980; Last: $19DF; Name : 'New Tai Lue'),
  (First: $19E0; Last: $19FF; Name : 'Khmer Symbols'),
  (First: $1A00; Last: $1A1F; Name : 'Buginese'),
  (First: $1A20; Last: $1AAF; Name : 'Tai Tham'),
  (First: $1B00; Last: $1B7F; Name : 'Balinese'),
  (First: $1B80; Last: $1BBF; Name : 'Sundanese'),
  (First: $1C00; Last: $1C4F; Name : 'Lepcha'),
  (First: $1C50; Last: $1C7F; Name : 'Ol Chiki'),
  (First: $1CD0; Last: $1CFF; Name : 'Vedic Extensions'),
  (First: $1D00; Last: $1D7F; Name : 'Phonetic Extensions'),
  (First: $1D80; Last: $1DBF; Name : 'Phonetic Extensions Supplement'),
  (First: $1DC0; Last: $1DFF; Name : 'Combining Diacritical Marks Supplement'),
  (First: $1E00; Last: $1EFF; Name : 'Latin Extended Additional'),
  (First: $1F00; Last: $1FFF; Name : 'Greek Extended'),
  (First: $2000; Last: $206F; Name : 'General Punctuation'),
  (First: $2070; Last: $209F; Name : 'Superscripts and Subscripts'),
  (First: $20A0; Last: $20CF; Name : 'Currency Symbols'),
  (First: $20D0; Last: $20FF; Name : 'Combining Diacritical Marks for Symbols'),
  (First: $2100; Last: $214F; Name : 'Letterlike Symbols'),
  (First: $2150; Last: $218F; Name : 'Number Forms'),
  (First: $2190; Last: $21FF; Name : 'Arrows'),
  (First: $2200; Last: $22FF; Name : 'Mathematical Operators'),
  (First: $2300; Last: $23FF; Name : 'Miscellaneous Technical'),
  (First: $2400; Last: $243F; Name : 'Control Pictures'),
  (First: $2440; Last: $245F; Name : 'Optical Character Recognition'),
  (First: $2460; Last: $24FF; Name : 'Enclosed Alphanumerics'),
  (First: $2500; Last: $257F; Name : 'Box Drawing'),
  (First: $2580; Last: $259F; Name : 'Block Elements'),
  (First: $25A0; Last: $25FF; Name : 'Geometric Shapes'),
  (First: $2600; Last: $26FF; Name : 'Miscellaneous Symbols'),
  (First: $2700; Last: $27BF; Name : 'Dingbats'),
  (First: $27C0; Last: $27EF; Name : 'Miscellaneous Mathematical Symbols-A'),
  (First: $27F0; Last: $27FF; Name : 'Supplemental Arrows-A'),
  (First: $2800; Last: $28FF; Name : 'Braille Patterns'),
  (First: $2900; Last: $297F; Name : 'Supplemental Arrows-B'),
  (First: $2980; Last: $29FF; Name : 'Miscellaneous Mathematical Symbols-B'),
  (First: $2A00; Last: $2AFF; Name : 'Supplemental Mathematical Operators'),
  (First: $2B00; Last: $2BFF; Name : 'Miscellaneous Symbols and Arrows'),
  (First: $2C00; Last: $2C5F; Name : 'Glagolitic'),
  (First: $2C60; Last: $2C7F; Name : 'Latin Extended-C'),
  (First: $2C80; Last: $2CFF; Name : 'Coptic'),
  (First: $2D00; Last: $2D2F; Name : 'Georgian Supplement'),
  (First: $2D30; Last: $2D7F; Name : 'Tifinagh'),
  (First: $2D80; Last: $2DDF; Name : 'Ethiopic Extended'),
  (First: $2DE0; Last: $2DFF; Name : 'Cyrillic Extended-A'),
  (First: $2E00; Last: $2E7F; Name : 'Supplemental Punctuation'),
  (First: $2E80; Last: $2EFF; Name : 'CJK Radicals Supplement'),
  (First: $2F00; Last: $2FDF; Name : 'Kangxi Radicals'),
  (First: $2FF0; Last: $2FFF; Name : 'Ideographic Description Characters'),
  (First: $3000; Last: $303F; Name : 'CJK Symbols and Punctuation'),
  (First: $3040; Last: $309F; Name : 'Hiragana'),
  (First: $30A0; Last: $30FF; Name : 'Katakana'),
  (First: $3100; Last: $312F; Name : 'Bopomofo'),
  (First: $3130; Last: $318F; Name : 'Hangul Compatibility Jamo'),
  (First: $3190; Last: $319F; Name : 'Kanbun'),
  (First: $31A0; Last: $31BF; Name : 'Bopomofo Extended'),
  (First: $31C0; Last: $31EF; Name : 'CJK Strokes'),
  (First: $31F0; Last: $31FF; Name : 'Katakana Phonetic Extensions'),
  (First: $3200; Last: $32FF; Name : 'Enclosed CJK Letters and Months'),
  (First: $3300; Last: $33FF; Name : 'CJK Compatibility'),
  (First: $3400; Last: $4DBF; Name : 'CJK Unified Ideographs Extension A'),
  (First: $4DC0; Last: $4DFF; Name : 'Yijing Hexagram Symbols'),
  (First: $4E00; Last: $9FFF; Name : 'CJK Unified Ideographs'),
  (First: $A000; Last: $A48F; Name : 'Yi Syllables'),
  (First: $A490; Last: $A4CF; Name : 'Yi Radicals'),
  (First: $A4D0; Last: $A4FF; Name : 'Lisu'),
  (First: $A500; Last: $A63F; Name : 'Vai'),
  (First: $A640; Last: $A69F; Name : 'Cyrillic Extended-B'),
  (First: $A6A0; Last: $A6FF; Name : 'Bamum'),
  (First: $A700; Last: $A71F; Name : 'Modifier Tone Letters'),
  (First: $A720; Last: $A7FF; Name : 'Latin Extended-D'),
  (First: $A800; Last: $A82F; Name : 'Syloti Nagri'),
  (First: $A830; Last: $A83F; Name : 'Common Indic Number Forms'),
  (First: $A840; Last: $A87F; Name : 'Phags-pa'),
  (First: $A880; Last: $A8DF; Name : 'Saurashtra'),
  (First: $A8E0; Last: $A8FF; Name : 'Devanagari Extended'),
  (First: $A900; Last: $A92F; Name : 'Kayah Li'),
  (First: $A930; Last: $A95F; Name : 'Rejang'),
  (First: $A960; Last: $A97F; Name : 'Hangul Jamo Extended-A'),
  (First: $A980; Last: $A9DF; Name : 'Javanese'),
  (First: $AA00; Last: $AA5F; Name : 'Cham'),
  (First: $AA60; Last: $AA7F; Name : 'Myanmar Extended-A'),
  (First: $AA80; Last: $AADF; Name : 'Tai Viet'),
  (First: $ABC0; Last: $ABFF; Name : 'Meetei Mayek'),
  (First: $AC00; Last: $D7AF; Name : 'Hangul Syllables'),
  (First: $D7B0; Last: $D7FF; Name : 'Hangul Jamo Extended-B'),
  (First: $D800; Last: $DB7F; Name : 'High Surrogates'),
  (First: $DB80; Last: $DBFF; Name : 'High Private Use Surrogates'),
  (First: $DC00; Last: $DFFF; Name : 'Low Surrogates'),
  (First: $E000; Last: $F8FF; Name : 'Private Use Area'),
  (First: $F900; Last: $FAFF; Name : 'CJK Compatibility Ideographs'),
  (First: $FB00; Last: $FB4F; Name : 'Alphabetic Presentation Forms'),
  (First: $FB50; Last: $FDFF; Name : 'Arabic Presentation Forms-A'),
  (First: $FE00; Last: $FE0F; Name : 'Variation Selectors'),
  (First: $FE10; Last: $FE1F; Name : 'Vertical Forms'),
  (First: $FE20; Last: $FE2F; Name : 'Combining Half Marks'),
  (First: $FE30; Last: $FE4F; Name : 'CJK Compatibility Forms'),
  (First: $FE50; Last: $FE6F; Name : 'Small Form Variants'),
  (First: $FE70; Last: $FEFF; Name : 'Arabic Presentation Forms-B'),
  (First: $FF00; Last: $FFEF; Name : 'Halfwidth and Fullwidth Forms'),
  (First: $FFF0; Last: $FFFF; Name : 'Specials'),
  (First: $10000; Last: $1007F; Name : 'Linear B Syllabary'),
  (First: $10080; Last: $100FF; Name : 'Linear B Ideograms'),
  (First: $10100; Last: $1013F; Name : 'Aegean Numbers'),
  (First: $10140; Last: $1018F; Name : 'Ancient Greek Numbers'),
  (First: $10190; Last: $101CF; Name : 'Ancient Symbols'),
  (First: $101D0; Last: $101FF; Name : 'Phaistos Disc'),
  (First: $10280; Last: $1029F; Name : 'Lycian'),
  (First: $102A0; Last: $102DF; Name : 'Carian'),
  (First: $10300; Last: $1032F; Name : 'Old Italic'),
  (First: $10330; Last: $1034F; Name : 'Gothic'),
  (First: $10380; Last: $1039F; Name : 'Ugaritic'),
  (First: $103A0; Last: $103DF; Name : 'Old Persian'),
  (First: $10400; Last: $1044F; Name : 'Deseret'),
  (First: $10450; Last: $1047F; Name : 'Shavian'),
  (First: $10480; Last: $104AF; Name : 'Osmanya'),
  (First: $10800; Last: $1083F; Name : 'Cypriot Syllabary'),
  (First: $10840; Last: $1085F; Name : 'Imperial Aramaic'),
  (First: $10900; Last: $1091F; Name : 'Phoenician'),
  (First: $10920; Last: $1093F; Name : 'Lydian'),
  (First: $10A00; Last: $10A5F; Name : 'Kharoshthi'),
  (First: $10A60; Last: $10A7F; Name : 'Old South Arabian'),
  (First: $10B00; Last: $10B3F; Name : 'Avestan'),
  (First: $10B40; Last: $10B5F; Name : 'Inscriptional Parthian'),
  (First: $10B60; Last: $10B7F; Name : 'Inscriptional Pahlavi'),
  (First: $10C00; Last: $10C4F; Name : 'Old Turkic'),
  (First: $10E60; Last: $10E7F; Name : 'Rumi Numeral Symbols'),
  (First: $11080; Last: $110CF; Name : 'Kaithi'),
  (First: $12000; Last: $123FF; Name : 'Cuneiform'),
  (First: $12400; Last: $1247F; Name : 'Cuneiform Numbers and Punctuation'),
  (First: $13000; Last: $1342F; Name : 'Egyptian Hieroglyphs'),
  (First: $1D000; Last: $1D0FF; Name : 'Byzantine Musical Symbols'),
  (First: $1D100; Last: $1D1FF; Name : 'Musical Symbols'),
  (First: $1D200; Last: $1D24F; Name : 'Ancient Greek Musical Notation'),
  (First: $1D300; Last: $1D35F; Name : 'Tai Xuan Jing Symbols'),
  (First: $1D360; Last: $1D37F; Name : 'Counting Rod Numerals'),
  (First: $1D400; Last: $1D7FF; Name : 'Mathematical Alphanumeric Symbols'),
  (First: $1F000; Last: $1F02F; Name : 'Mahjong Tiles'),
  (First: $1F030; Last: $1F09F; Name : 'Domino Tiles'),
  (First: $1F100; Last: $1F1FF; Name : 'Enclosed Alphanumeric Supplement'),
  (First: $1F200; Last: $1F2FF; Name : 'Enclosed Ideographic Supplement'),
  (First: $20000; Last: $2A6DF; Name : 'CJK Unified Ideographs Extension B'),
  (First: $2A700; Last: $2B73F; Name : 'CJK Unified Ideographs Extension C'),
  (First: $2F800; Last: $2FA1F; Name : 'CJK Compatibility Ideographs Supplement'),
  (First: $E0000; Last: $E007F; Name : 'Tags'),
  (First: $E0100; Last: $E01EF; Name : 'Variation Selectors Supplement'),
  (First: $F0000; Last: $FFFFF; Name : 'Supplementary Private Use Area-A'),
  (First: $100000; Last: $10FFFF; Name : 'Supplementary Private Use Area-B')
);
{$ENDREGION}

type

//------------------------------------------------------------------------------
TFrmGeneratorRange = class(TForm)
    PaintBox1: TPaintBox;
    ScrollBar1: TScrollBar;
    lwCodebase: TListBox;
    StatusBar1: TStatusBar;
    btnImport: TButton;
    btnCancel: TButton;
    btnOk: TButton;
    btnClear: TButton;
    OpenTextFileDialog1: TOpenDialog;
    btnDefaults: TButton;
    procedure PaintBox1Paint(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnImportClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
  private
    TextMetric: TTextMetric;

    FFont: TFont;

    FNumColumns: Integer;
    FNumRows   : Integer;

    FHover: Cardinal;
    FSelection: TDictionary<Cardinal, Boolean>;

    procedure SelectionChanged;

    procedure SetHover(const Value: Cardinal);
    function GetCodeBaseNameForCharacter(const Character: Cardinal): String;

    property NumColumns: Integer read FNumColumns write FNumColumns;
    property NumRows   : Integer read FNumRows    write FNumRows;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(Font: TFont): Boolean;

    property Hover: Cardinal read FHover write SetHover;

    property Selection: TDictionary<Cardinal, Boolean> read FSelection;
  end;

var
  FrmGeneratorRange: TFrmGeneratorRange;

implementation

{$R *.dfm}

const
 COLOR_BORDER0    = $00C0C0C0;
 COLOR_BORDER1    = $00D8E9EC;
 COLOR_CHARACTER  = $00F0F0F0;
 COLOR_SELECTED   = $00C56A31;

 CELL_SIZE = 33;

// http://www.codeguru.com/forum/showthread.php?p=1919785


// http://www.codeproject.com/KB/system/codepage.aspx
// TFrmGeneratorRange
//------------------------------------------------------------------------------
constructor TFrmGeneratorRange.Create(AOwner: TComponent);
var Character: Cardinal;
var Index    : Integer;
begin
  inherited;
  FSelection:= TDictionary<Cardinal, Boolean>.Create;

  for Character := 32 to 126 do
  begin
    FSelection.Add(Character, True);
  end;

  //DoubleBuffered:= True;

  lwCodebase.Items.BeginUpdate;
  lwCodebase.Items.Clear;
  for Index := Low(UnicodeCharacterDatabase) to High(UnicodeCharacterDatabase) do
  begin
    lwCodebase.Items.Add( UnicodeCharacterDatabase[Index].Name );
  end;
  lwCodebase.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
destructor TFrmGeneratorRange.Destroy;
begin
  FSelection.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TFrmGeneratorRange.GetCodeBaseNameForCharacter(const Character: Cardinal): String;
var Index     : Integer;
begin
  for Index:= Low(UnicodeCharacterDatabase) to High(UnicodeCharacterDatabase) do
  begin
    if (UnicodeCharacterDatabase[Index].First <= Character) and (UnicodeCharacterDatabase[Index].Last >= Character) then
    begin
      Result:= UnicodeCharacterDatabase[Index].Name;
      Exit;
    end;
  end;
  Result:= '';
end;


//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.btnClearClick(Sender: TObject);
begin
  Selection.Clear;

  SelectionChanged;
end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.btnDefaultsClick(Sender: TObject);
var Character: Cardinal;
begin
  Selection.Clear;
  for Character := 32 to 126 do
  begin
    FSelection.Add(Character, True);
  end;
  SelectionChanged;
end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.btnImportClick(Sender: TObject);
var Lines: TStrings;
var Index: Integer;
var Text : String;
var Character : Cardinal;
begin
  if OpenTextFileDialog1.Execute then
  begin
    Selection.Clear;

    Lines:= TStringList.Create;
    try
      Lines.LoadFromFile(OpenTextFileDialog1.FileName);

      Text:= Lines.Text;

      for Index := 1 to Length(Text) do
      begin
        Character:= Ord( Text[Index] );

        if (Character >= Ord(TextMetric.tmFirstChar)) and (Character <= Ord(TextMetric.tmLastChar )) and not Selection.ContainsKey(Character) then
        begin
          Selection.Add(Character, True);
        end;
      end;


    finally

    end;


    SelectionChanged;

  end;
end;

//------------------------------------------------------------------------------
function TFrmGeneratorRange.Execute(Font: TFont): Boolean;
var CodePage  : Cardinal;
begin
  FFont:= Font;

  PaintBox1.Canvas.Font:= Font;

  // http://msdn.microsoft.com/en-us/library/dd145132(v=VS.85).aspx
  GetTextMetrics(PaintBox1.Canvas.Handle , TextMetric);

 // GetCPInfoEx(1252, 0, CodePageInfo);


  FormResize(nil);

  SelectionChanged;

  Result:= ShowModal = mrOK;
end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.FormResize(Sender: TObject);
begin
  FNumColumns:= PaintBox1.Width  div CELL_SIZE;
  FNumRows   := PaintBox1.Height div CELL_SIZE;

  ScrollBar1.Max:= ( Ord(TextMetric.tmLastChar) - Ord(TextMetric.tmFirstChar)) div FNumColumns;
end;

// CharNextExA

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.PaintBox1Paint(Sender: TObject);
var Character : Cardinal;
var CharacterFirst: Cardinal;
var CharacterLast: Cardinal;

var Index: Integer;
var Charset: Integer;
var X,Y: Integer;
var Rect: TRect;
var c: Integer;
var d: Char;
var DefaultChar: Bool;
begin
  with PaintBox1.Canvas do
  begin
    Font.Size:= 14;

    Brush.Color:= clWhite;

    FillRect(ClientRect);
  end;
  CharacterFirst:= Ord(TextMetric.tmFirstChar) + (ScrollBar1.Position * NumColumns);
  CharacterLast := Ord(TextMetric.tmLastChar );

  Index:= 0;

  Charset:=  GetTextCharset(PaintBox1.Canvas.Handle);

  Y:= 0;
  X:= 0;
  for Character:= CharacterFirst to CharacterLast do
  begin
    Rect.Left  := (X * CELL_SIZE);
    Rect.Top   := (Y * CELL_SIZE);
    Rect.Right := Rect.Left + (CELL_SIZE);
    Rect.Bottom:= Rect.Top  + (CELL_SIZE);
//    CP_ACP

    c:= WideCharToMultiByte(
        Charset,
        0,//WC_NO_BEST_FIT_CHARS or WC_COMPOSITECHECK or WC_DEFAULTCHAR,
        @Character,
        1,
        @d,
        2,
        nil,
        @DefaultChar);
        //

           // if not DefaultChar then

    with PaintBox1.Canvas do
    begin
      Pen.Color:= COLOR_BORDER0;

      MoveTo(Rect.Left , Rect.Bottom);
      LineTo(Rect.Left , Rect.Top);
      LineTo(Rect.Right, Rect.Top);
      LineTo(Rect.Right, Rect.Bottom);
      LineTo(Rect.Left , Rect.Bottom);

      Rect.Left  := Rect.Left +1;
      Rect.Top   := Rect.Top  +1;
      Rect.Right := Rect.Right;
      Rect.Bottom:= Rect.Bottom;

      if FSelection.ContainsKey(Character) and FSelection[Character] then
      begin
        Brush.Color:= COLOR_SELECTED;
      end else
      begin
        Brush.Color:= COLOR_CHARACTER;
      end;
      FillRect(Rect);

      Pen.Color:= COLOR_BORDER1;
      Brush.Style:= bsClear;
      Rectangle(Rect);   

     // if GetGlyphSupported(Character, FFont) then
     // begin
        PaintBox1.Canvas.Font.Color:= clBlack;
      //end else
     // begin
     //   PaintBox1.Canvas.Font.Color:= clRed;
    //  end;
      DrawText(PaintBox1.Canvas.Handle, Chr(Character), 1, Rect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP or DT_NOPREFIX);


   //   if not DefaultChar then
   //   begin
   //   end;
    end;
    Inc(X);

    if X >= NumColumns then
    begin
      X:= 0;
      Y:= Y + 1;
      if Y >= NumRows then Exit;
    end;
   
  end;
(*  
  Character:= Ord(TextMetric.tmFirstChar);

  Y:= 2;

  Character:= Ord(TextMetric.tmFirstChar) + (ScrollBar1.Position * ColumnCount);
  while Character <= Ord(TextMetric.tmLastChar) do
  begin
    X:= 2;
    
    for Index := 1 to ColumnCount do
    begin
      Rect.Left  := X;
      Rect.Top   := Y;
      Rect.Right := X + TextMetric.tmMaxCharWidth;
      Rect.Bottom:= Y + TextMetric.tmHeight;

      PaintBox1.Canvas.Brush.Color:= COLOR_CHARACTER;
      PaintBox1.Canvas.FillRect(Rect);
      PaintBox1.Canvas.Brush.Style:= bsClear;
     
      DrawText(PaintBox1.Canvas.Handle, Chr(Character), 1, Rect, DT_TOP or DT_LEFT or DT_NOCLIP or DT_NOPREFIX);
    //  PaintBox1.Canvas.TextOut(X,Y, Chr(Character));

      Inc(Character);
      
      X:= X + CELL_SIZE;
    end;
    Y:= Y + CELL_SIZE;

    if Y > PaintBox1.Height then Exit;
    
  end;  *)


end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Hover:= Ord(TextMetric.tmFirstChar) + (ScrollBar1.Position * NumColumns) + ( X div CELL_SIZE) + (Y div CELL_SIZE) * FNumColumns;

  if ssLeft in Shift then
  begin

    if FSelection.ContainsKey(FHover) then
    begin
      FSelection.Remove(FHover);
    end else
    begin
      FSelection.Add(FHover, True);
    end;

    SelectionChanged;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Hover:= Ord(TextMetric.tmFirstChar) + (ScrollBar1.Position * NumColumns) + ( X div CELL_SIZE) + (Y div CELL_SIZE) * FNumColumns;

  if ssLeft in Shift then
  begin

    if not FSelection.ContainsKey(FHover) then
    begin
      FSelection.Add(FHover, True);

      SelectionChanged;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.ScrollBar1Change(Sender: TObject);
begin
  PaintBox1.Invalidate;


end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.SelectionChanged;
//var Count: Integer;
begin
//  Count:= Ord(TextMetric.tmLastChar ) - Ord(TextMetric.tmFirstChar) + 1;

  StatusBar1.Panels[0].Text:= Format('Selected %d characters', [Selection.Count ]);

  PaintBox1.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmGeneratorRange.SetHover(const Value: Cardinal);
begin
  FHover := Value;

  StatusBar1.Panels[1].Text:= 'Character: ' + Format('U+%.4X (%d)', [FHover, FHover]);
  StatusBar1.Panels[2].Text:= 'Codebase : ' + GetCodeBaseNameForCharacter(FHover);
end;

end.

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//    Phoenix Game Framework                                                  //
//                                                                            //
//    http://www.phoenixlib.net                                               //
//                                                                            //
//    The contents of this file are used with permission, subject to          //
//    the Mozilla Public License Version 1.1 (the "License"); you may         //
//    not use this file except in compliance with the License. You may        //
//    obtain a copy of the License at                                         //
//    http://www.mozilla.org/MPL/MPL-1.1.html                                 //
//                                                                            //
//    Software distributed under the License is distributed on an             //
//    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or          //
//    implied. See the License for the specific language governing            //
//    rights and limitations under the License.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
unit phxFont;
//< Text rendering Functions and bitmap fonts

interface

{$I phxConfig.inc}

uses
  SysUtils, Classes,

  phxLogger,
  phxTypes,
  phxClasses,
  phxMath,
  phxDevice,
  phxGraphics,
  phxTexture,
  phxCanvas;

const

// Font file extension
PHXFONT_EXT = '.phxfnt';
// Version
PHXFONT_VERSION = 11;

type

{$MINENUMSIZE 4}

// Text alignment for rectangles
//------------------------------------------------------------------------------
TPHXTextAlign = (
  // Align to the top left of the rectangle
  taTopLeft,
  // Align to the top
  taTop,
  // Align to the top right of the rectangle
  taTopRight,
  // Align to the left of the rectangle
  taLeft,
  // Align to the center of the rectangle
  taCenter,
  // Align to the right of the rectangle
  taRight,
  // Align to the bottom left of the rectangle
  taBottomLeft,
  // Align to the bottom of the rectangle
  taBottom,
  // Align to the bottom right of the rectangle
  taBottomRight
);

// Text alignment for a single line
//------------------------------------------------------------------------------
TPHXAlignment = (
  // Align the text to the left
  alLeft,
  // Align the text in the center
  alCenter,
  // Align the text to the right
  alRight
);

// Font style
//------------------------------------------------------------------------------
TPHXFontStyle = (
  // Bold
  fsBold,
  // Italic
  fsItalic,
  // Underline
  fsUnderline,
  // Strikeout
  fsStrikeOut
);

// Set of font styles
//------------------------------------------------------------------------------
TPHXFontStyles = set of TPHXFontStyle;

// Wrapping mode for fonts
//------------------------------------------------------------------------------
TPHXFontWrap = (
 // Word wrap for each word
 wmWord,
 // Word wrap on a character for character basis
 wmChar
);

{$REGION 'TPHXCharacter'}


// Font glyphs
//         X
//         |      Width    |
//    Y ---+---------------+--------------+-----------
//         |               |              | YOffset
//         |     ######    |   #######    +----
//         |    #      #   |   #      #   |
//         |    #      #   |   #      #   |
// Height  |    ########   |   ######     |          TPHXCharacterSet.Base
//         |    #      #   |   #      #   |
//         |    #      #   |   #      #   |
//         |    #      #   |   #######    +-----------
//         |               |              |
//      ---+----+----------+---+----------+
//         |    |              |
//         |XOff|   XAdvance   |
//
// Describes a single character.
// The total width of a character is the summation of the A, B, and C spaces.
//------------------------------------------------------------------------------
PPHXCharacter = ^TPHXCharacter;
TPHXCharacter = record
  public
    // The unicode code of the character.
    ID: Cardinal;
    // The x-position of the character in the texture.
    X: Integer;
    // The y-position of the character in the texture.
    Y: Integer;
    // The width of the character in pixels
    Width: Integer;
    // The height of the character in pixels
    Height: Integer;
    // The offset to add to the raster before drawing the glyph
    Offset: TVector2i;
    // The number of pixels to move the raster in the horisontal plane.
    Advance: Integer;
  public
  end;

PCharacterList = ^TCharacterList;
TCharacterList = array[0.. $000FFFFF] of TPHXCharacter;

// List of characters
//------------------------------------------------------------------------------
TPHXCharacterList = class
  private
    FCapacity: Integer;
    FCount   : Integer;

    FList: PCharacterList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXCharacter;
    procedure SetItem(Index: Integer; const Value: TPHXCharacter);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Default constructor
    constructor Create;
    destructor Destroy; override;

    // Removes all characters
    procedure Clear;

    // Add a new character to the list
    procedure Add(const Value: TPHXCharacter );
    // Delete a character
    procedure Delete(Index: Integer);

    // Load the character list from a stream
    procedure LoadFromStream(const Stream: TStream);
    // Save the character list to a stream
    procedure SaveToStream  (const Stream: TStream);

    // Find a character index
    function Find(const Character: Cardinal): Integer; overload;
    // Find a character index
    function Find(const Character: WideChar): Integer; overload;

    // Number of characters
    property Count: Integer read FCount    write SetCount;
    // Capacity
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal character list
    property List: PCharacterList read FList;
    // Gets or sets single characters
    property Items[Index: Integer]: TPHXCharacter read GetItem write SetItem; default;
  end;

// Maps a character by the unicode number to a index in the character list
TPHXCharacterMap = Array[Word] of Integer;

{$ENDREGION}

{$REGION 'TPHXKerning'}

// Describes the kerning for two characters
//------------------------------------------------------------------------------
TPHXKerning = record
  // Unicode number of the first character
  First : Word;
  // Unicode number of the second character
  Second: Word;
  // The ammount of kerning to apply
  Amount: Shortint;
end;

PKerningList = ^TKerningList;
TKerningList = Array[0.. $000FFFFF] of TPHXKerning;

// List of kerning pairs
//------------------------------------------------------------------------------
TPHXKerningList = class
  private
    FCapacity: Integer;
    FCount   : Integer;

    FList: PKerningList;

    procedure Grow;

    function  GetItem(Index: Integer): TPHXKerning;
    procedure SetItem(Index: Integer; const Value: TPHXKerning);

    procedure SetCapacity(const Value: Integer);
    procedure SetCount   (const Value: Integer);
  public
    // Default constructor
    constructor Create;
    destructor Destroy; override;

    // Removes all kerning pairs
    procedure Clear;

    // Add a new kerning pair to the list
    procedure Add(const Value: TPHXKerning );
    // Delete a kerning pair
    procedure Delete(Index: Integer);

    // Load the kerning list from a stream
    procedure LoadFromStream(const Stream: TStream);
    // Save the kerning list to a stream
    procedure SaveToStream  (const Stream: TStream);
    // Load the kerning list from a file
    procedure LoadFromFile(const FileName: String);
    // Save the kerning list to a file
    procedure SaveToFile(const FileName: String);

    // Number of kerning pairs
    property Count: Integer read FCount write SetCount;
    // Capacity of the list
    property Capacity: Integer read FCapacity write SetCapacity;
    // Pointer to the internal list
    property List: PKerningList read FList;
    // Retrieve or modify a kerning pair
    property Items[Index: Integer]: TPHXKerning read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXTextMetric'}

// Contains basic information about a font
//------------------------------------------------------------------------------
TPHXTextMetric = class
  private
    // The height (ascent + descent) of characters.
    FHeight: Integer;
    // The vertical offset to add to all charactes in the font
    FOffset: Integer;
    // The ascent (units above the base line) of characters.
    FAscent: Integer;
    // The descent (units below the base line) of characters.
    FDescent: Integer;
  public
    // Default constructor
    constructor Create;
    destructor Destroy; override;

    // Load the character list from a file
    procedure LoadFromFile(const FileName: String);
    // Load the character list from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the character list to a file
    procedure SaveToFile(const FileName: String);
    // Save the character list to a stream
    procedure SaveToStream(Stream: TStream);

    // The height (ascent + descent) of characters.
    property Height: Integer read FHeight write FHeight;
    // The vertical offset to add to all charactes in the font
    property Offset: Integer read FOffset write FOffset;
    // The ascent (units above the base line) of characters.
    property Ascent: Integer read FAscent write FAscent;
    // The descent (units below the base line) of characters.
    property Descent: Integer read FDescent write FDescent;
  end;

{$ENDREGION}

{$REGION 'TPHXFont'}

// The font file header.
//------------------------------------------------------------------------------
TPHXFontHeader = record
  // The id of the font file, should always be PHXFNT.
  Ident: Array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

// The data required to render a single character
//------------------------------------------------------------------------------
TPHXCharacterBuffer = record
  TexCoord: TRectf;
  Vertices: TRectf;
end;

// Bitmap font class
//------------------------------------------------------------------------------

{ TPHXFont }

TPHXFont = class
  private
    // The owning device
    FDevice: TPHXDevice;
    // The canvas to draw to
    FCanvas: TPHXCanvas;

    // Name of the font
    FName: String;
    // Font author
    FAuthor: String;
    // Version of the font
    FVersion: String;
    // Comment of the font
    FComment: String;

     // Size of the font
    FSize: Integer;
    // The font style
    FStyle: TPHXFontStyles;
    // Text metric
    FMetric: TPHXTextMetric;

    // The font texture
    FTexture: TPHXTexture;
    // List of characters in the font
    FCharacters: TPHXCharacterList;
    // List of kerning pairs in the font
    FKernings: TPHXKerningList;

    // Wrapping mode for the font
    FWrapMode: TPHXFontWrap;
    // Wrapping chars for word wrap
    FWrapChars: WideString;
    // The characters in this string may not start a line
    FWrapStart: WideString;
    // The characters in this string may not end a line
    FWrapEnd: WideString;

    function GetHeight: Integer;

    // Returns the next word from a text
    function GetWord(const Text: WideString; var Index: Integer; out LineBreak: Boolean): WideString;

    // Draw a single character into the draw buffer
    procedure DrawCharacter(const Character: TPHXCharacterBuffer; const Position: TVector3f; const ColorT, ColorB: TColor4f); overload;
    // Draw a single character into the draw buffer
    procedure DrawCharacter(const Character: TPHXCharacterBuffer; const Transform: TMatrix4f; const ColorT, ColorB: TColor4f); overload;

    // Draw text
    procedure DrawText(const Text: WideString; const Position: TVector3f; const ColorT, ColorB: TColor4f); overload;
    // Draw transformed text
    procedure DrawText(const Text: WideString; const Transform: TMatrix4f; const ColorT, ColorB: TColor4f); overload;
  protected
    CharacterMap   : TPHXCharacterMap;
    CharacterBuffer: array of TPHXCharacterBuffer;
  public
    // Creates a new font
    constructor Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
    // Creates a new font for editing
    constructor CreateEx(ATexture: TPHXTexture);
    // Destroys this font
    destructor Destroy; override;

    // Load the font from file
    procedure LoadFromFile(const FileName: String);
    // Load the font from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the font to a file
    procedure SaveToFile(const FileName: String);
    // Save the font to a stream
    procedure SaveToStream(Stream: TStream);

    ///  Initializes the font, must be called before any rendering
    procedure Initialize;

    // Returns the width of a specific character.
    function CharWidth(const Character: AnsiChar): Integer; overload;
    // Returns the width of a specific character.
    function CharWidth(const Character: WideChar): Integer; overload;

    // Returns the text size of a specific text.
    function TextExtent(const Text: AnsiString): TVector2i; overload;
    // Returns the text size of a specific text.
    function TextExtent(const Text: WideString): TVector2i; overload;

    // Returns the width of a specific text.
    function TextWidth(const Text: AnsiString): Integer; overload;
    // Returns the width of a specific text.
    function TextWidth(const Text: WideString): Integer; overload;

    // Returns the height of a specific text.
    function TextHeight(const Text: AnsiString): Integer; overload;
    // Returns the height of a specific text.
    function TextHeight(const Text: WideString): Integer; overload;

    // Draw text
    procedure TextOut(const X, Y: Single; const Text: WideString); overload;
    // Draw colored text
    procedure TextOut(const X, Y: Single; const Text: WideString; const Color: TColor4f); overload;
    // Draw gradient text
    procedure TextOut(const X, Y: Single; const Text: WideString; const ColorUp, ColorDown: TColor4f); overload;
    // Draw text
    procedure TextOut(const Position: TVector3f; const Text: WideString; const Color: TColor4f); overload;
    // Draw text
    procedure TextOut(const Position: TVector3f; const Text: WideString; const ColorA, ColorB: TColor4f); overload;
    // Draw transformed text with a color
    procedure TextOut(const Transform: TMatrix4f; const Text: WideString; const Color: TColor4f); overload;
    // Draw transformed text with two colors
    procedure TextOut(const Transform: TMatrix4f; const Text: WideString; const ColorA, ColorB: TColor4f); overload;

    // Draw alignmed text in a rectangle
    procedure TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign); overload;
    // Draw alignmed text in a rectangle with a single color
    procedure TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign; const Color: TColor4f); overload;
     // Draw alignmed text in a rectangle with gradient colors
    procedure TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign; const ColorT, ColorB: TColor4f); overload;

    // Draw alignmed text in a rectangle
    procedure TextOut(const Rect: TRecti; const Text: WideString; const Align: TPHXTextAlign); overload;
    // Draw alignmed text in a rectangle with a single color
    procedure TextOut(const Rect: TRecti; const Text: WideString; const Align: TPHXTextAlign; const Color: TColor4f); overload;
    // Draw alignmed text in a rectangle with gradient colors
    procedure TextOut(const Rect: TRecti; const Text: WideString; const Align: TPHXTextAlign; const ColorT, ColorB: TColor4f); overload;

    // Draw a formatted text
    procedure FormatText(const X, Y: Single; const Text: WideString; Args: array of const); overload;

    // Draw wrapped text, returns true if all text fitted inside the rectangle
    function WrapText(const Rect: TRectf; const Text: WideString; const Alignment: TPHXAlignment): Boolean; overload;
    // Draw wrapped text with a single color, returns true if all text fitted inside the rectangle
    function WrapText(const Rect: TRectf; const Text: WideString; const Alignment: TPHXAlignment; const Color: TColor4f): Boolean; overload;
    // Draw wrapped text with a gradient color, returns true if all text fitted inside the rectangle
    function WrapText(const Rect: TRectf; const Text: WideString; const Alignment: TPHXAlignment; const ColorT, ColorB: TColor4f): Boolean; overload;

    // Draw all lines in a string list
    procedure TextOut(const X, Y: Single; const Lines: TStrings); overload;

    // Draw a selection rectangle for a text
    procedure DrawSelection(const X,Y: Integer; const Text: WideString; const SelStart, SelLength: Integer); overload;
    // Draw a selection rectangle for a text with a given color
    procedure DrawSelection(const X,Y: Integer; const Text: WideString; const SelStart, SelLength: Integer; const Color: TColor4f); overload;
    // Draw a selection rectangle for a text with a given color and texture
    procedure DrawSelection(const X,Y: Integer; const Text: WideString; const SelStart, SelLength: Integer; const Color: TColor4f; const Texture: TPHXTexture); overload;



            {



    procedure TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign; const Color: TColor4f); overload;
    procedure TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign; const ColorTop, ColorBottom: TColor4f); overload;

    // Draw a colored text inside a rect with a align and vertical align, it
    // text that doesnt fit the rectangle, also supports linebreaks #13
    procedure WrapText(const Rect: TRectf; const Text: String; Align: TPHXAlign); overload;
    // Draw a colored text inside a rect with a align and vertical align, it
    // will wrap text that doesnt fit the rectangle, also supports linebreaks #13
    procedure WrapText(const Rect: TRectf; const Text: String; Align: TPHXAlign; Color: TColor4f); overload;
    // Textout with word wrapping
    procedure WrapText(const Rect: TRectf; const Text: String; const Align: TPHXAlign; const ColorTop, ColorBottom: TColor4f); overload;
            }


    // The owning device
    property Device: TPHXDevice read FDevice;
    // The canvas to draw to
    property Canvas: TPHXCanvas read FCanvas;
    // The name of the font.
    property Name: String read FName write FName;
    // The size of the font.
    property Size: Integer read FSize write FSize;
    // The font style
    property Style: TPHXFontStyles read FStyle write FStyle;
    // Font author
    property Author: String read FAuthor write FAuthor;
    // Version of the fon
    property Version: String read FVersion write FVersion;
    // Comment of the font
    property Comment: String read FComment write FComment;
    // Text metric
    property Metric: TPHXTextMetric read FMetric;
    // List of characters in the font
    property Characters: TPHXCharacterList read FCharacters;
    // List of kerning pairs in the font
    property Kernings: TPHXKerningList read FKernings;
    // The font texture
    property Texture: TPHXTexture read FTexture;

    // Wrapping mode for the font
    property WrapMode: TPHXFontWrap read FWrapMode write FWrapMode;
    // Wrapping characters for word wrap
    property WrapChars: WideString read FWrapChars write FWrapChars;
    // The characters in this string may not start a line
    property WrapStart: WideString read FWrapStart write FWrapStart;
    // The characters in this string may not end a line
    property WrapEnd: WideString read FWrapEnd write FWrapEnd;

    // The vertical distance between two text lines.
    property Height: Integer read GetHeight;
  end;

{$ENDREGION}

{$REGION 'TPHXFontList'}

PFontList = ^TFontList;
TFontList = array[0..$00FFFFFF] of TPHXFont;

// Container for a list of fonts
//------------------------------------------------------------------------------
TPHXFontList = class(TObject)
  private
    FDevice: TPHXDevice;
    FCanvas: TPHXCanvas;
    FItems : TList;

    function GetCount: Integer;
    function GetList: PFontList;
    function GetItem(Index: Integer): TPHXFont;
  public
    // Create a new font list
    constructor Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
    // Destroys the font list
    destructor Destroy; override;

    // Clears all the fonts from the font list.
    procedure Clear;

    // Add a new font to the fontlist
    function Add: TPHXFont; overload;
    // Add a new font with a specific name to the fontlist
    function Add(const Name : String): TPHXFont; overload;
    // Add an existing font to the fontlist
    function Add(const Font: TPHXFont): TPHXFont; overload;

    // Loads a phoenix font into the fontlist.
    function LoadFont(const Filename: String ): TPHXFont; overload;
    // Loads a phoenix font into the fontlist.
    function LoadFont(Stream: TStream): TPHXFont; overload;

    // Returns the index of the font with the specified name, -1 if none is found.
    function IndexOf(const Name: String): Integer;

    // Returns the font with the specified name, nil if not found
    function Find(const Name: String): TPHXFont; overload;
    // Returns the font with the specified name and size
    function Find(const Name: String; const Size: Integer): TPHXFont; overload;
    // Returns the font with the specified name, size and style
    function Find(const Name: String; const Size: Integer; const Style: TPHXFontStyles): TPHXFont; overload;


    // The owning device
    property Device  : TPHXDevice read FDevice;
    // The canvas to render to
    property Canvas  : TPHXCanvas read FCanvas;
    // The number of fonts in the font list.
    property Count: Integer read getCount;
    // Returns the pointer to the internal list
    property List: PFontList read GetList;
    // The fonts in the list.
    property Fonts[Index: Integer]: TPHXFont read GetItem; default;
  end;

{$ENDREGION}

implementation


resourcestring
  SVersionError = 'Font version missmatch: File: %d,  Code: %d.';
  SFormatError = 'Not a valid Phoenix font.';
     (*
//------------------------------------------------------------------------------
function CalculateTabPosition(const Offset, Position: Single; const TabWidth: Integer): Single;
var Index: Integer;
begin
  Index:= Trunc(Position - Offset) div TabWidth;

  Result:= Offset + (Index+1) * TabWidth;
end;
*)


{$REGION 'TPHXCharacterList'}

// TPHXCharacterList
//==============================================================================
constructor TPHXCharacterList.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXCharacterList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;




//------------------------------------------------------------------------------
procedure TPHXCharacterList.Clear;
begin
  SetCount(0);
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.Add(const Value: TPHXCharacter);
begin
  Inc(FCount);

  if FCount > FCapacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index + 1], FList^[Index],  (FCount - Index) * SizeOf(TPHXCharacter));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.LoadFromStream(const Stream: TStream);
var Index: Integer;
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  // Read the characters from the stream
  for Index:=0 to FCount - 1 do
  begin
    Stream.Read(FList^[Index].ID            , SizeOf( FList^[Index].ID ));
    Stream.Read(FList^[Index].X             , SizeOf( FList^[Index].X ));
    Stream.Read(FList^[Index].Y             , SizeOf( FList^[Index].Y ));
    Stream.Read(FList^[Index].Width         , SizeOf( FList^[Index].Width ));
    Stream.Read(FList^[Index].Height        , SizeOf( FList^[Index].Height ));
    Stream.Read(FList^[Index].Offset.X      , SizeOf( FList^[Index].Offset.X ));
    Stream.Read(FList^[Index].Offset.Y      , SizeOf( FList^[Index].Offset.Y ));
    Stream.Read(FList^[Index].Advance       , SizeOf( FList^[Index].Advance ));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.SaveToStream(const Stream: TStream);
var Index: Integer;
begin
  Stream.Write(FCount, SizeOf(FCount));

  for Index:=0 to FCount - 1 do
  begin
    Stream.Write(FList^[Index].ID            , SizeOf( FList^[Index].ID ));
    Stream.Write(FList^[Index].X             , SizeOf( FList^[Index].X ));
    Stream.Write(FList^[Index].Y             , SizeOf( FList^[Index].Y ));
    Stream.Write(FList^[Index].Width         , SizeOf( FList^[Index].Width ));
    Stream.Write(FList^[Index].Height        , SizeOf( FList^[Index].Height ));
    Stream.Write(FList^[Index].Offset.X      , SizeOf( FList^[Index].Offset.X ));
    Stream.Write(FList^[Index].Offset.Y      , SizeOf( FList^[Index].Offset.Y ));
    Stream.Write(FList^[Index].Advance       , SizeOf( FList^[Index].Advance ));
  end;
end;

//------------------------------------------------------------------------------
function TPHXCharacterList.Find(const Character: Cardinal): Integer;
var Index: Integer;
begin
  for Index := 0 to FCount - 1 do
  begin
    if List^[Index].ID = Character then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXCharacterList.Find(const Character: WideChar): Integer;
var Index: Integer;
begin
  for Index := 0 to FCount - 1 do
  begin
    if List^[Index].ID = Ord(Character) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;


//------------------------------------------------------------------------------
procedure TPHXCharacterList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
    begin
      Delta := 16
    end else
    begin
      Delta := 4;
    end;
  end;

  SetCapacity(Capacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXCharacter));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if (FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXCharacterList.GetItem(Index: Integer): TPHXCharacter;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXCharacterList.SetItem(Index: Integer; const Value: TPHXCharacter);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;

{$ENDREGION}

{$REGION 'TPHXKerningList'}

// TPHXKerningList
//==============================================================================
constructor TPHXKerningList.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXKerningList.Destroy;
begin
  SetCount   (0);
  SetCapacity(0);
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.Clear;
begin
  SetCount(0);
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.Add(const Value: TPHXKerning);
begin
  Inc(FCount);

  if FCount > FCapacity then Grow;

  FList^[Count - 1]:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.Delete(Index: Integer);
begin
  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index + 1], FList^[Index],  (FCount - Index) * SizeOf(TPHXKerning));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXKerningList.LoadFromStream(const Stream: TStream);
var Index: Integer;
begin
  Stream.Read(FCount, SizeOf(FCount));

  SetCapacity(FCount);

  for Index := 0 to FCount - 1 do
  begin
    Stream.Read(FList^[Index].First , SizeOf(FList^[Index].First ));
    Stream.Read(FList^[Index].Second, SizeOf(FList^[Index].Second));
    Stream.Read(FList^[Index].Amount, SizeOf(FList^[Index].Amount));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.SaveToStream(const Stream: TStream);
var Index: Integer;
begin
  Stream.Write(FCount, SizeOf(FCount));

  for Index := 0 to FCount - 1 do
  begin
    Stream.Write(FList^[Index].First , SizeOf(FList^[Index].First ));
    Stream.Write(FList^[Index].Second, SizeOf(FList^[Index].Second));
    Stream.Write(FList^[Index].Amount, SizeOf(FList^[Index].Amount));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  begin
    if FCapacity > 8 then
    begin
      Delta := 16
    end else
    begin
      Delta := 4;
    end;
  end;

  SetCapacity(Capacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.SetCapacity(const Value: Integer);
begin
  if FCapacity <> Value then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXKerning));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.SetCount(const Value: Integer);
begin
  FCount := Value;

  if (FCount > FCapacity) then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
function TPHXKerningList.GetItem(Index: Integer): TPHXKerning;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= FList^[Index];
end;

//------------------------------------------------------------------------------
procedure TPHXKerningList.SetItem(Index: Integer; const Value: TPHXKerning);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  FList^[Index]:= Value;
end;


{$ENDREGION}

{$REGION 'TPHXTextMetric'}

// TPHXTextMetric
//==============================================================================
constructor TPHXTextMetric.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TPHXTextMetric.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXTextMetric.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTextMetric.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFile, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXTextMetric.LoadFromStream(Stream: TStream);
begin
  Stream.Read(FHeight       , SizeOf( FHeight ));
  Stream.Read(FOffset       , SizeOf( FOffset ));
  Stream.Read(FAscent       , SizeOf( FAscent ));
  Stream.Read(FDescent      , SizeOf( FDescent ));
end;

//------------------------------------------------------------------------------
procedure TPHXTextMetric.SaveToStream(Stream: TStream);
begin
  Stream.Write(FHeight      , SizeOf( FHeight ));
  Stream.Write(FOffset      , SizeOf( FOffset ));
  Stream.Write(FAscent      , SizeOf( FAscent ));
  Stream.Write(FDescent     , SizeOf( FDescent ));
end;

{$ENDREGION}


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
////////////////////////////////////////////////////////////////////////////////


{$REGION 'TPHXFont'}


const DEFAULT_FONT_NAME = 'Unnamed';


// TPHXFont
//==============================================================================
constructor TPHXFont.Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
var Index    : Integer;
begin
  FDevice := ADevice;
  FCanvas := ACanvas;
  FTexture:= ADevice.CreateTexture;
  FName   := DEFAULT_FONT_NAME;

  FMetric    := TPHXTextMetric.Create;
  FCharacters:= TPHXCharacterList.Create;
  FKernings  := TPHXKerningList.Create;

  for Index:=Low(CharacterMap) to High(CharacterMap) do
  begin
    CharacterMap[Index]:= -1;
  end;
end;

//------------------------------------------------------------------------------
constructor TPHXFont.CreateEx(ATexture: TPHXTexture);
var Index    : Integer;
begin
  FDevice := nil;
  FCanvas := nil;
  FTexture:= ATexture;
  FName   := DEFAULT_FONT_NAME;

  FMetric    := TPHXTextMetric.Create;
  FCharacters:= TPHXCharacterList.Create;
  FKernings  := TPHXKerningList.Create;

  for Index:=Low(CharacterMap) to High(CharacterMap) do
  begin
    CharacterMap[Index]:= -1;
  end;
end;

//------------------------------------------------------------------------------
destructor TPHXFont.Destroy;
begin
  FMetric.Free;
  FCharacters.Free;
  FKernings.Free;
  FTexture.Free;

  inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFont, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckFont, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.LoadFromStream(Stream: TStream);
var Header: TPHXFontHeader;
var Reader: TPHXReader;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  // Check the header ident
  if (Header.Ident <> 'PHXFNT') then
  begin
    TPHXLogger.Error('TPHXFont.LoadFont', SFormatError);

    raise Exception.Create(SFormatError);
  end;
  // Check the header version
  if (Header.Version <> PHXFONT_VERSION) then
  begin
    TPHXLogger.Error('TPHXFont.LoadFont', SVersionError, [Header.Version, PHXFONT_VERSION]);

    raise Exception.CreateFmt(SVersionError, [Header.Version, PHXFONT_VERSION]);
  end;

  Reader:= TPHXReader.Create(Stream);
  try
    FName   := Reader.ReadString;
    FAuthor := Reader.ReadString;
    FVersion:= Reader.ReadString;
    FComment:= Reader.ReadString;

    FStyle := [];
    if Reader.ReadBoolean then Include(FStyle, fsBold);
    if Reader.ReadBoolean then Include(FStyle, fsItalic);
    if Reader.ReadBoolean then Include(FStyle, fsUnderline);
    if Reader.ReadBoolean then Include(FStyle, fsStrikeOut);

    Reader.Read(FWrapMode, SizeOf(FWrapMode));

    FWrapChars:= Reader.ReadString;
    FWrapStart:= Reader.ReadString;
    FWrapEnd  := Reader.ReadString;
  finally
    Reader.Free;
  end;

  Metric.LoadFromStream(Stream);
  Characters.LoadFromStream(Stream);
  Kernings.LoadFromStream(Stream);

  Texture.LoadFromStream(Stream);

  Texture.Name:= FName;

  //  Build the font
  Initialize();
end;

//------------------------------------------------------------------------------
procedure TPHXFont.SaveToStream(Stream: TStream);
var Header: TPHXFontHeader;
var Writer: TPHXWriter;
begin
  Header.Ident  := 'PHXFNT';
  Header.Version:= PHXFONT_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Writer:= TPHXWriter.Create(Stream);
  try
    Writer.WriteString(FName);
    Writer.WriteString(FAuthor);
    Writer.WriteString(FVersion);
    Writer.WriteString(FComment);

    Writer.WriteBoolean(fsBold      in FStyle );
    Writer.WriteBoolean(fsItalic    in FStyle );
    Writer.WriteBoolean(fsUnderline in FStyle );
    Writer.WriteBoolean(fsStrikeOut in FStyle );

    Writer.Write(FWrapMode, SizeOf(FWrapMode));

    Writer.WriteString(FWrapChars);
    Writer.WriteString(FWrapStart);
    Writer.WriteString(FWrapEnd);
  finally
    Writer.Free;
  end;

  Metric.SaveToStream(Stream);
  Characters.SaveToStream(Stream);
  Kernings.SaveToStream(Stream);
  Texture.SaveToStream(Stream);
end;


//------------------------------------------------------------------------------
procedure TPHXFont.Initialize;
var Index    : Integer;
var Character: PPHXCharacter;

var WInv: Single;
var HInv: Single;

var vx,vy: Single;
var vw,vh: Single;
begin
  if (FTexture.Width = 0) or (FTexture.Height = 0) then Exit;

  // Build the texture
  FTexture.Build;

  For Index:=Low(CharacterMap) to High(CharacterMap) do
  begin
    CharacterMap[Index]:= -1;
  end;

  WInv:= 1 / Texture.Width;
  HInv:= 1 / Texture.Height;

  SetLength(CharacterBuffer, Characters.Count);

  for Index:=0 to Characters.Count-1 do
  begin
    Character:= @Characters.List^[Index];

    if Character^.ID > High(CharacterMap) then
    begin
      raise Exception.CreateFmt('Unsupported character: %d', [Character^.ID]);
    end;

    // Calculate the vertex position of the character
    vx:= -Character^.Offset.X;
    vy:= -Character^.Offset.Y - Metric.Offset;
    vw:=  Character^.Width;
    vh:=  Character^.Height;

    CharacterBuffer[Index].TexCoord.Left  := (Character^.X                     ) * WInv;
    CharacterBuffer[Index].TexCoord.Right := (Character^.X + Character^.Width  ) * WInv;
    CharacterBuffer[Index].TexCoord.Top   := (Character^.Y                     ) * HInv;
    CharacterBuffer[Index].TexCoord.Bottom:= (Character^.Y + Character^.Height ) * HInv;

    CharacterBuffer[Index].Vertices.Left  := vx;
    CharacterBuffer[Index].Vertices.Right := vx + vw;
    CharacterBuffer[Index].Vertices.Top   := vy;
    CharacterBuffer[Index].Vertices.Bottom:= vy + vh;

    CharacterMap[ Character^.ID ]:= Index;
  end;
end;

//------------------------------------------------------------------------------
function TPHXFont.CharWidth(const Character: AnsiChar): Integer;
var Index : Integer;
begin
  Index:= CharacterMap[ Ord(Character) ];

  if Index >= 0 then
  begin
    Result:= FCharacters.List^[ Index ].Advance;
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
function TPHXFont.CharWidth(const Character: WideChar): Integer;
var Index : Integer;
begin
  Index:= CharacterMap[ Ord(Character) ];

  if Index >= 0 then
  begin
    Result:= FCharacters.List^[ Index ].Advance;
  end else
  begin
    Result:= 0;
  end;
end;

//------------------------------------------------------------------------------
function TPHXFont.TextExtent(const Text: AnsiString): TVector2i;
var Index    : Cardinal;
var Character: Integer;
var LineWidth: Integer;
begin
  // No text
  if Length(Text) = 0 then
  begin
    Result.X:= 0;
    Result.Y:= 0;

    Exit;
  end;
  Result.X:= 0;
  Result.Y:= Metric.Height;
      (*
  // Get the first character
  Character:= CharacterMap[ Ord( Text[1] ) ];

  if Character >= 0 then
  begin
    // Compensate for the offset of the first character
    LineWidth:= FCharacters.List^[Character].Width - FCharacters.List^[Character].Advance - FCharacters.List^[Character].Offset.X;
  end else
  begin
    LineWidth:= 0;
  end;
    *)
  LineWidth:= 0;

  for Index:=1 to Length(Text) do
  begin
    Character:= CharacterMap[ Ord( Text[Index] ) ];

    // Line feed
    if Text[Index] = #13 then
    begin
      Result.Y:= Result.Y + Metric.Height;

      if LineWidth > Result.X then Result.X:= LineWidth;

      LineWidth:= 0;//FCharacters.List^[Character].Width - FCharacters.List^[Character].Advance - FCharacters.List^[Character].Offset.X;
    end else
    // Visible character
    if Character >= 0 then
    begin
      LineWidth:= LineWidth + FCharacters.List^[Character].Advance;
    end;

    if LineWidth > Result.X then Result.X:= LineWidth;
  end;
end;

//------------------------------------------------------------------------------
function TPHXFont.TextExtent(const Text: WideString): TVector2i;
var Index    : Cardinal;
var Character: Integer;
var LineWidth: Integer;
begin
  // No text
  if Length(Text) = 0 then
  begin
    Result.X:= 0;
    Result.Y:= 0;

    Exit;
  end;

  Result.X:= 0;
  Result.Y:= Metric.Height;

  // Get the first character
  //Character:= CharacterMap[ Ord( Text[1] ) ];

  LineWidth:= 0;//FCharacters.List^[Character].Width - FCharacters.List^[Character].XAdvance - FCharacters.List^[Character].XOffset;
  for Index:=1 to Length(Text) do
  begin
    Character:= CharacterMap[ Ord( Text[Index] ) ];

    // Line feed
    if Text[Index] = #13 then
    begin
      Result.Y:= Result.Y + Metric.Height;

      LineWidth:= 0;//FCharacters.List^[Character].Width - FCharacters.List^[Character].XAdvance - FCharacters.List^[Character].XOffset;
    end else
    // Visible character
    if Character >= 0 then
    begin
      LineWidth:= LineWidth + FCharacters.List^[Character].Advance;
    end;

    if LineWidth > Result.X then Result.X:= LineWidth;
  end;
end;

//------------------------------------------------------------------------------
function TPHXFont.TextWidth(const Text: AnsiString): Integer;
begin
  Result:= TextExtent(Text).X;
end;

//------------------------------------------------------------------------------
function TPHXFont.TextWidth(const Text: WideString): Integer;
begin
  Result:= TextExtent(Text).X;
end;


//------------------------------------------------------------------------------
function TPHXFont.TextHeight(const Text: AnsiString): Integer;
begin
  Result:= TextExtent(Text).Y;
end;

//------------------------------------------------------------------------------
function TPHXFont.TextHeight(const Text: WideString): Integer;
begin
  Result:= TextExtent(Text).Y;
end;

// Draw a single character into the buffer
//------------------------------------------------------------------------------
procedure TPHXFont.DrawCharacter(const Character: TPHXCharacterBuffer; const Position: TVector3f; const ColorT, ColorB: TColor4f);
var Vertices: array[0..3] of TPHXVertex;
var Indicies: Array[0..5] of TPHXIndex;
begin
  // Top - left
  Vertices[0].Position.X:= Position.X + Character.Vertices.Left;
  Vertices[0].Position.Y:= Position.Y + Character.Vertices.Top;
  Vertices[0].Position.Z:= Position.Z;
  Vertices[0].TexCoord.X:= Character.TexCoord.Left;
  Vertices[0].TexCoord.Y:= Character.TexCoord.Top;
  Vertices[0].Color:= ColorT;

  // Top - right
  Vertices[1].Position.X:= Position.X + Character.Vertices.Right;
  Vertices[1].Position.Y:= Position.Y + Character.Vertices.Top;
  Vertices[1].Position.Z:= Position.Z;
  Vertices[1].TexCoord.X:= Character.TexCoord.Right;
  Vertices[1].TexCoord.Y:= Character.TexCoord.Top;
  Vertices[1].Color:= ColorT;

  // Bottom - Left
  Vertices[2].Position.X:= Position.X + Character.Vertices.Left;
  Vertices[2].Position.Y:= Position.Y + Character.Vertices.Bottom;
  Vertices[2].Position.Z:= Position.Z;
  Vertices[2].TexCoord.X:= Character.TexCoord.Left;
  Vertices[2].TexCoord.Y:= Character.TexCoord.Bottom;
  Vertices[2].Color:= ColorB;

  // Bottom - right
  Vertices[3].Position.X:= Position.X + Character.Vertices.Right;
  Vertices[3].Position.Y:= Position.Y + Character.Vertices.Bottom;
  Vertices[3].Position.Z:= Position.Z;
  Vertices[3].TexCoord.X:= Character.TexCoord.Right;
  Vertices[3].TexCoord.Y:= Character.TexCoord.Bottom;
  Vertices[3].Color:= ColorB;


  // Triangle #1
  Indicies[0]:= 0;
  Indicies[1]:= 1;
  Indicies[2]:= 2;
  // Triangle #2
  Indicies[3]:= 1;
  Indicies[4]:= 3;
  Indicies[5]:= 2;

  Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, Texture);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.DrawCharacter(const Character: TPHXCharacterBuffer; const Transform: TMatrix4f; const ColorT, ColorB: TColor4f);
var Vertices: array[0..3] of TPHXVertex;
var Indicies: Array[0..5] of TPHXIndex;
begin
  // Top - left
  Vertices[0].Position.X:= Character.Vertices.Left;
  Vertices[0].Position.Y:= Character.Vertices.Top;
  Vertices[0].Position.Z:= 0;
  Vertices[0].TexCoord.X:= Character.TexCoord.Left;
  Vertices[0].TexCoord.Y:= Character.TexCoord.Top;
  Vertices[0].Color:= ColorT;

  // Top - right
  Vertices[1].Position.X:= Character.Vertices.Right;
  Vertices[1].Position.Y:= Character.Vertices.Top;
  Vertices[1].Position.Z:= 0;
  Vertices[1].TexCoord.X:= Character.TexCoord.Right;
  Vertices[1].TexCoord.Y:= Character.TexCoord.Top;
  Vertices[1].Color:= ColorT;

  // Bottom - Left
  Vertices[2].Position.X:= Character.Vertices.Left;
  Vertices[2].Position.Y:= Character.Vertices.Bottom;
  Vertices[2].Position.Z:= 0;
  Vertices[2].TexCoord.X:= Character.TexCoord.Left;
  Vertices[2].TexCoord.Y:= Character.TexCoord.Bottom;
  Vertices[2].Color:= ColorB;

  // Bottom - right
  Vertices[3].Position.X:= Character.Vertices.Right;
  Vertices[3].Position.Y:= Character.Vertices.Bottom;
  Vertices[3].Position.Z:= 0;
  Vertices[3].TexCoord.X:= Character.TexCoord.Right;
  Vertices[3].TexCoord.Y:= Character.TexCoord.Bottom;
  Vertices[3].Color:= ColorB;

  // Transform the four verticies using the matrix
  Vertices[0].Position:= Matrix_Transform(Transform, Vertices[0].Position);
  Vertices[1].Position:= Matrix_Transform(Transform, Vertices[1].Position);
  Vertices[2].Position:= Matrix_Transform(Transform, Vertices[2].Position);
  Vertices[3].Position:= Matrix_Transform(Transform, Vertices[3].Position);

  // Triangle #1
  Indicies[0]:= 0;
  Indicies[1]:= 1;
  Indicies[2]:= 2;
  // Triangle #2
  Indicies[3]:= 1;
  Indicies[4]:= 3;
  Indicies[5]:= 2;

  Canvas.Draw(PHX_TRIANGLES, @Vertices, @Indicies, 4, 6, Texture);
end;





(*
1 . Add pre-draw spacing to draw position before drawing glyph. This is the abcA value from Win32 ABC structure.

DrawPosition.x += Spacing.A

2. Draw glyph using following coordinates.

X = DrawPosition.x + KerningPairs[PrevChar][ThisChar].KernAmount;
Y = DrawPosition.y + TextMetrics.Ascent - GlyphMetrics.Origin.y;

3. Add post-draw spacing to draw position after drawing glyph. This is the abcB and abcC values from ABC struct.

DrawPosition.x += Spacing.B + Spacing.C
*)
//------------------------------------------------------------------------------
procedure TPHXFont.DrawText(const Text: WideString; const Position: TVector3f; const ColorT, ColorB: TColor4f);
var Index    : Cardinal;
var Character: Integer;
var Raster   : TVector3f;
begin
 // Raster:= Position;
  Raster.X:= Trunc(Position.X);
  Raster.Y:= Trunc(Position.Y);
  Raster.Z:= Trunc(Position.Z);

  for Index:=1 to Length(Text) do
  begin
    Character:= CharacterMap[ Ord( Text[Index] ) ];

    // Line feed
    if Text[Index] = #13 then
    begin
      Raster.X:= Position.X;
      Raster.Y:= Raster.Y + Metric.Height;
    end else
    // Tab
   // if Text[Index] = #8 then
   // begin
   //   Raster.X:= CalculateTabPosition(Position.X, Raster.X, 100);
    //end else
    // Visible character
    if Character >= 0 then
    begin
      DrawCharacter(CharacterBuffer[Character], Raster, ColorT, ColorB);

      Raster.X:= Raster.X + FCharacters.List^[Character].Advance;
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXFont.DrawText(const Text: WideString; const Transform: TMatrix4f; const ColorT, ColorB: TColor4f);
var Index    : Cardinal;
var Character: Integer;
var Raster   : TMatrix4f;
begin
  Raster:= Transform;

  for Index:=1 to Length(Text) do
  begin
    Character:= CharacterMap[ Ord( Text[Index] ) ];

    // Line feed
    if Text[Index] = #13 then
    begin
      //Raster.X:= Position.X;
      Raster.v[12]:= Transform.v[12];
      // Raster.Y:= Raster.Y + Metric.Height;
      Raster.v[13]:= Raster.v[13] + Metric.Height;
    end else
    // Tab
   // if Text[Index] = #8 then
   // begin
   //   Raster.v[12]:= CalculateTabPosition(Transform.v[12], Raster.v[12], 100);
   // end else
    // Visible character
    if Character >= 0 then
    begin
      DrawCharacter(CharacterBuffer[Character], Raster, ColorT, ColorB);

      Raster.v[12]:= Raster.v[12] + Characters.List^[Character].Advance;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const X,Y: Single; const Text: WideString);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawText(Text, Position, clrWhite, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const X,Y: Single; const Text: WideString; const Color: TColor4f);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawText(Text, Position, Color, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const X,Y: Single; const Text: WideString; const ColorUp, ColorDown: TColor4f);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawText(Text, Position, ColorUp, ColorDown);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Position: TVector3f; const Text: WideString; const Color: TColor4f);
begin
  DrawText(Text, Position, Color, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Position: TVector3f; const Text: WideString; const ColorA, ColorB: TColor4f);
begin
  DrawText(Text, Position, ColorA, ColorB);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Transform: TMatrix4f; const Text: WideString; const Color: TColor4f);
begin
  DrawText(Text, Transform, Color, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Transform: TMatrix4f; const Text: WideString; const ColorA, ColorB: TColor4f);
begin
  DrawText(Text, Transform, ColorA, ColorB);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const X, Y: Single; const Lines: TStrings);
var Position: TVector3f;
var Index   : Integer;
var Line    : WideString;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;
  for Index := 0 to Lines.Count-1 do
  begin
    Line:= Lines[Index];

    DrawText(Line, Position, clrWhite, clrWhite);

    Position.Y:= Position.Y + Height;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign);
begin
  TextOut(Rect, Text, Align, clrWhite, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign; const Color: TColor4f);
begin
  TextOut(Rect, Text, Align, Color, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Rect: TRectf; const Text: WideString; const Align: TPHXTextAlign; const ColorT, ColorB: TColor4f);
var TextSize: TVector2i;
var TextPos : TVector3f;
begin
  TextSize := TextExtent(Text);
  TextPos.X:= Rect.Left;
  TextPos.Y:= Rect.Top;
  TextPos.Z:= 0;

  case Align of
    // Top
    taTopLeft:
    begin
      TextPos.X:= Rect.Left;
      TextPos.Y:= Rect.Top;
    end;
    taTop:
    begin
      TextPos.X:= Rect.Left + ((Rect.Right - Rect.Left) - TextSize.X) * 0.5;
      TextPos.Y:= Rect.Top;
    end;
    taTopRight:
    begin
      TextPos.X:= Rect.Right - TextSize.X;
      TextPos.Y:= Rect.Top;
    end;
    // Middle
    taLeft:
    begin
      TextPos.X:= Rect.Left;
      TextPos.Y:= Rect.Top + ((Rect.Bottom - Rect.Top) - TextSize.Y) * 0.5;
    end;
    taCenter:
    begin
      TextPos.X:= Rect.Left + ((Rect.Right - Rect.Left) - TextSize.X) * 0.5;
      TextPos.Y:= Rect.Top  + ((Rect.Bottom - Rect.Top) - TextSize.Y) * 0.5;
    end;
    taRight:
    begin
      TextPos.X:= Rect.Right - TextSize.X;
      TextPos.Y:= Rect.Top   + ((Rect.Bottom - Rect.Top) - TextSize.Y) * 0.5;
    end;
    // Bottom
    taBottomLeft:
    begin
      TextPos.X:= Rect.Left;
      TextPos.Y:= Rect.Bottom - TextSize.Y;
    end;
    taBottom:
    begin
      TextPos.X:= Rect.Left   + ((Rect.Right - Rect.Left) - TextSize.X) * 0.5;
      TextPos.Y:= Rect.Bottom - TextSize.Y;
    end;
    taBottomRight:
    begin
      TextPos.X:= Rect.Right  - TextSize.X;
      TextPos.Y:= Rect.Bottom - TextSize.Y;
    end;
  end;
  TextOut(TextPos, Text, ColorT, ColorB);
end;


//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Rect: TRecti; const Text: WideString; const Align: TPHXTextAlign);
begin
  TextOut(Rect, Text, Align, clrWhite);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Rect: TRecti; const Text: WideString; const Align: TPHXTextAlign; const Color: TColor4f);
begin
  TextOut(Rect, Text, Align, Color, Color);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.TextOut(const Rect: TRecti; const Text: WideString; const Align: TPHXTextAlign; const ColorT, ColorB: TColor4f);
var TextSize: TVector2i;
var TextPos : TVector3f;
begin
  TextSize := TextExtent(Text);
  TextPos.X:= Rect.Left;
  TextPos.Y:= Rect.Top;
  TextPos.Z:= 0;

  case Align of
    // Top
    taTopLeft:
    begin
      TextPos.X:= Rect.Left;
      TextPos.Y:= Rect.Top;
    end;
    taTop:
    begin
      TextPos.X:= Rect.Left + ((Rect.Right - Rect.Left) - TextSize.X) * 0.5;
      TextPos.Y:= Rect.Top;
    end;
    taTopRight:
    begin
      TextPos.X:= Rect.Right - TextSize.X;
      TextPos.Y:= Rect.Top;
    end;
    // Middle
    taLeft:
    begin
      TextPos.X:= Rect.Left;
      TextPos.Y:= Rect.Top + ((Rect.Bottom - Rect.Top) - TextSize.Y) * 0.5;
    end;
    taCenter:
    begin
      TextPos.X:= Rect.Left + ((Rect.Right - Rect.Left) - TextSize.X) * 0.5;
      TextPos.Y:= Rect.Top  + ((Rect.Bottom - Rect.Top) - TextSize.Y) * 0.5;
    end;
    taRight:
    begin
      TextPos.X:= Rect.Right - TextSize.X;
      TextPos.Y:= Rect.Top   + ((Rect.Bottom - Rect.Top) - TextSize.Y) * 0.5;
    end;
    // Bottom
    taBottomLeft:
    begin
      TextPos.X:= Rect.Left;
      TextPos.Y:= Rect.Bottom - TextSize.Y;
    end;
    taBottom:
    begin
      TextPos.X:= Rect.Left   + ((Rect.Right - Rect.Left) - TextSize.X) * 0.5;
      TextPos.Y:= Rect.Bottom - TextSize.Y;
    end;
    taBottomRight:
    begin
      TextPos.X:= Rect.Right  - TextSize.X;
      TextPos.Y:= Rect.Bottom - TextSize.Y;
    end;
  end;
  TextOut(TextPos, Text, ColorT, ColorB);
end;

//------------------------------------------------------------------------------
procedure TPHXFont.FormatText(const X, Y: Single; const Text: WideString; Args: array of const);
var Position: TVector3f;
begin
  Position.X:= X;
  Position.Y:= Y;
  Position.Z:= 0;

  DrawText( Format(Text, Args) , Position, clrWhite, clrWhite);
end;

//------------------------------------------------------------------------------
function TPHXFont.GetWord(const Text: WideString; var Index: Integer; out LineBreak: Boolean): WideString;
var Start: Integer;
var Len  : Integer;
begin
  LineBreak:= False;

  Len:= Length(Text);

  if Text[Index] = #13 then
  begin
    Result:= '';

    LineBreak:= True;

    Inc(Index);

    Exit;
  end;

  case WrapMode of
    wmWord:
    begin
      Start:= Index;

      // Skip spaces
      while (Index < Len) and (Pos(Text[Index],  WrapChars) <> 0) do
      begin
        Inc(Index);
      end;
      while (Index <= Len) and (Pos(Text[Index], WrapChars) = 0) do
      begin

        if Text[Index] = #13 then
        begin
          Break;
        end;

        Inc(Index);
      end;
      Result:= Copy(Text, Start, Index - Start);
    end;
    wmChar:
    begin
      Start:= Index;

      // This character may not end the current line
      while (Index <= Len) and (Pos(Text[Index], WrapEnd) > 0) do
      begin
        Inc(Index);
      end;

      // Add one character
      Inc(Index);

      // The next character may not start a new line
      while (Index <= Len) and (Pos(Text[Index], WrapStart) > 0) do
      begin
        Inc(Index);
      end;

      Result:= Copy(Text, Start, Index - Start);
    end;
  end;
end;

//------------------------------------------------------------------------------
function TPHXFont.WrapText(const Rect: TRectf; const Text: WideString; const Alignment: TPHXAlignment): Boolean;
begin
  Result:= WrapText(Rect, Text, Alignment, clrWhite, clrWhite);
end;

//------------------------------------------------------------------------------
function TPHXFont.WrapText(const Rect: TRectf; const Text: WideString; const Alignment: TPHXAlignment; const Color: TColor4f): Boolean;
begin
  Result:= WrapText(Rect, Text, Alignment, Color, Color);
end;

// http://en.wikipedia.org/wiki/Kinsoku_shori#Line_breaking_rules_in_Japanese_text_.28Kinsoku_Shori.29
//------------------------------------------------------------------------------
function TPHXFont.WrapText(const Rect: TRectf; const Text: WideString; const Alignment: TPHXAlignment; const ColorT, ColorB: TColor4f): Boolean;
var Position  : TVector3f;
var Index     : Integer;
var Width     : Single;
var Word      : String;
var WordWidth : Integer;
var Line      : String;
var LineBreak : Boolean;
var LineWidth : Integer;
begin
  Position.X:= Rect.Left;
  Position.Y:= Rect.Top;
  Position.Z:= 0;

  Width:= Rect.Right - Rect.Left;

  Line      := '';
  LineWidth:= 0;

  Index:= 1;

  while Index < Length(Text) do
  begin
    Word:= GetWord(Text, Index, LineBreak);
    WordWidth:= TextWidth(Word);

    // The line wont fit in the rectangle with the added word
    if (LineWidth + WordWidth > Width) or LineBreak then
    begin
      case Alignment of
        alLeft:
        begin
          Position.X:= Rect.Left;
        end;
        alCenter:
        begin
          Position.X:= Rect.Left + (Width - LineWidth) * 0.5;
        end;
        alRight:
        begin
          Position.X:= Rect.Right - LineWidth;
        end;
      end;
      DrawText(Line, Position, ColorT, ColorB);

      Position.Y:= Position.Y + Metric.Height;

      if Position.Y + Metric.Height > Rect.Bottom then
      begin
        Result:= False;
        Exit;
      end;

      // When starting on a new line trim the leading spaces
      Line:= TrimLeft(Word);
    end else
    begin
      Line:= Line + Word;
    end;
    // Get the width of the new line
    LineWidth:= TextWidth(Line);
  end;

  if Line <> '' then
  begin
    case Alignment of
      alLeft:
      begin
        Position.X:= Rect.Left;
      end;
      alCenter:
      begin
        Position.X:= Rect.Left + (Width - LineWidth) * 0.5;
      end;
      alRight:
      begin
        Position.X:= Rect.Right - LineWidth;
      end;
    end;
    DrawText(Line, Position, ColorT, ColorB);
  end;

  Result:= True;
end;

       (*
    // The word doesnt fit on the current line
    if Position.X + TextWidth(Word) > Rect.Right then
    begin
      Position.X:= Rect.Left;
      Position.Y:= Position.Y + Metric.Height;

      // When starting on a new line trim the leading spaces
      Word:= TrimLeft(Word);
    end;


    // Todo: Collect a full line and draw aligned text

    DrawWord(Word, Position, Color, Color);
    *)


//------------------------------------------------------------------------------
procedure TPHXFont.DrawSelection(const X, Y: Integer; const Text: WideString; const SelStart, SelLength: Integer);
var Rect : TRectf;
var Index: Integer;
var Count: Integer;
var Char : Integer;
begin
  Rect.Left  := X;
  Rect.Right := X;
  Rect.Top   := Y - Metric.Offset;
  Rect.Bottom:= Y - Metric.Offset + Metric.Height;

  Count:= 0;
  for Index := 1 to Length(Text) do
  begin
    Char:= CharacterMap[ Ord( Text[Index] ) ];

    // New line, draw the previous selection
    if Text[Index] = #13 then
    begin
      if Count > 0 then
      begin
        Canvas.FilledRectangle(Rect);
      end;
      // Reset the rect and move to the next line
      Rect.Left  := X;
      Rect.Right := X;
      Rect.Top   := Rect.Top + Metric.Height;
      Rect.Bottom:= Rect.Bottom + Metric.Height;
    end else
    // Visible character
    if Char >= 0 then
    begin
      // Before the selection
      if Index < SelStart then
      begin
        Rect.Left := Rect.Left + FCharacters.List^[Char].Advance;
        Rect.Right:= Rect.Left;
      end else
      // Start of the selection
      if Index = SelStart then
      begin
        Inc(Count);
        Rect.Left := Rect.Left  - FCharacters.List^[Char].Offset.X;
        Rect.Right:= Rect.Right + FCharacters.List^[Char].Advance ;
      end else
      // In the selection
      if Index < SelStart + SelLength then
      begin
        Inc(Count);
        Rect.Right:= Rect.Right + FCharacters.List^[Char].Advance;
      end else
      // End of the selection
      if Index = SelStart + SelLength then
      begin
        Rect.Right:= Rect.Right - FCharacters.List^[Char].Offset.X;

        Break;
      end;
    end;
  end;

  // Draw the remainder of the selection
  if Count > 0 then
  begin
    Canvas.FilledRectangle(Rect);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.DrawSelection(const X, Y: Integer; const Text: WideString; const SelStart, SelLength: Integer; const Color: TColor4f);
begin
  Canvas.Texture:= nil;
  Canvas.Color  := Color;

  DrawSelection(X, Y, Text, SelStart, SelLength);

  Canvas.Color:= clrWhite;
end;

//------------------------------------------------------------------------------
procedure TPHXFont.DrawSelection(const X, Y: Integer; const Text: WideString; const SelStart, SelLength: Integer; const Color: TColor4f; const Texture: TPHXTexture);
begin
  Canvas.Texture:= Texture;
  Canvas.Color  := Color;

  DrawSelection(X, Y, Text, SelStart, SelLength);

  Canvas.Color:= clrWhite;
end;


//------------------------------------------------------------------------------
function TPHXFont.GetHeight: Integer;
begin
  Result:= Metric.Height;
end;



{$ENDREGION}

{$REGION 'TPHXFontList'}

// TPHXFontList
//==============================================================================
constructor TPHXFontList.Create(ADevice: TPHXDevice; ACanvas: TPHXCanvas);
begin
  FDevice:= ADevice;
  FCanvas:= ACanvas;

  FItems := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXFontList.Destroy;
begin
  Clear;

  FItems.Free;

  inherited;
end;

//------------------------------------------------------------------------------
function TPHXFontList.Add: TPHXFont;
begin
  Result:= TPHXFont.Create(Device, Canvas);

  FItems.add(Result);
end;

//------------------------------------------------------------------------------
function TPHXFontList.Add(const Name: String): TPHXFont;
begin
  Result:= TPHXFont.Create(Device, Canvas);
  Result.Name:=Name;

  FItems.add(Result);
end;

//------------------------------------------------------------------------------
function TPHXFontList.Add(const Font: TPHXFont): TPHXFont;
begin
  Result:= Font;

  FItems.add(Result);
end;

//------------------------------------------------------------------------------
function TPHXFontList.LoadFont(const Filename: String): TPHXFont;
begin
  Result:= Add;
  Result.LoadFromFile(Filename);
end;

//------------------------------------------------------------------------------
function TPHXFontList.LoadFont(Stream: TStream): TPHXFont;
begin
  Result:= Add;
  Result.LoadFromStream(Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXFontList.Clear;
var Index: Integer;
begin
  For Index:=0 to FItems.Count - 1 do
  begin
    TPHXFont(FItems.List[Index]).Free;
  end;
  FItems.Clear;
end;

//------------------------------------------------------------------------------
function TPHXFontList.IndexOf(const Name: String): Integer;
var Index: Integer;
var Font : TPHXFont;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Font:= TPHXFont( FItems.List[Index] );

    if SameText(Font.Name, Name) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
function TPHXFontList.Find(const Name: String): TPHXFont;
var Index: Integer;
var Font : TPHXFont;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Font:= TPHXFont( FItems.List[Index] );

    if SameText(Font.Name, Name) then
    begin
      Result:= Font;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXFontList.Find(const Name: String; const Size: Integer): TPHXFont;
var Index: Integer;
var Font : TPHXFont;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Font:= TPHXFont( FItems.List[Index] );

    if SameText(Font.Name, Name) and (Font.Size = Size) then
    begin
      Result:= Font;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXFontList.Find(const Name: String; const Size: Integer; const Style: TPHXFontStyles): TPHXFont;
var Index: Integer;
var Font : TPHXFont;
begin
  for Index:=0 to FItems.Count - 1 do
  begin
    Font:= TPHXFont( FItems.List[Index] );

    if SameText(Font.Name, Name) and (Font.Size = Size) and (Font.Style = Style) then
    begin
      Result:= Font;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXFontList.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

//------------------------------------------------------------------------------
function TPHXFontList.GetList: PFontList;
begin
  Result:= PFontList(FItems.List);
end;

//------------------------------------------------------------------------------
function TPHXFontList.GetItem(Index: Integer): TPHXFont;
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Result:= TPHXFont(FItems.List[Index]);
end;



{$ENDREGION}


end.

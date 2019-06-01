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
unit phxTranslation;
{<
  @abstract(Add support for localisation your project)

  A example localisation file can look like this

  @preformatted(
    -- Commented line
    name = English
    code = en_US

    welcome = Welcome\nto this application
  )

  This file can be loaded with the @link(TPHXTranslationManager.Load) function.

  To translate a tag call the @link(TPHXTranslationManager.Translate) function
  or one of its overloads after the translation file is loaded.

  The default behaviour when translation a missing tag is to return the tag name
  to raise a exception instead the TAG_EXCEPTION define to the project.
}
interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxClasses;

// Raise a exception when a tag is missing instead of returning the tag name
//{$DEFINE TAG_EXCEPTION}

const

// Name for the tag containing the language name
tag_name = 'name';
// Name for the tag containing the language code
tag_code = 'code';

type

// Forward declarations
TPHXTranslationManager = class;

{$REGION 'TPHXTranslationItem'}

// A single tag for a language
//------------------------------------------------------------------------------
TPHXLanguageTag = record
  public
    // Hash code of the item
    Hash: Cardinal;
    // The original string
    Name: AnsiString;
    // The translated string
    Text: WideString;
  public
    // Create a new language tag
    class function Create(AName: AnsiString; AText: WideString): TPHXLanguageTag; static;
  end;

PTranslationTagList = ^TTranslationTagList;
TTranslationTagList = array[0..$00FFFFFF] of TPHXLanguageTag;

// Contains a list of tags for a language
//------------------------------------------------------------------------------
TPHXLanguageTags = class
  private
    // Number of items in the list
    FCount: Integer;
    // The capacity of the list
    FCapacity: Integer;
    // The list of translation texts
    FList: PTranslationTagList;

    procedure Grow;

    function GetItem(const Index: Integer): TPHXLanguageTag;

    procedure SetItem(const Index: Integer; const Value: TPHXLanguageTag);
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
  public
    // Create a new tag list
    constructor Create;
    // Destroy this tag list
    destructor Destroy; override;

    // Clear the list of tags
    procedure Clear;
    // Add a tag to the list
    procedure Add(const Name: AnsiString; const Text: WideString);
    // Delete a tag at a given index
    procedure Delete(const Index: Integer);

    // Return the index of a tag or -1 if not found
    function IndexOf(const Name: AnsiString): Integer;

    // Number of texts in the language
    property Count: Integer read FCount write SetCount;
    // Number of texts in the language
    property Capacity: Integer read FCapacity write SetCapacity;
    // Number of texts in the language
    property List: PTranslationTagList read FList;
    // Get or set a tag
    property Items[const Index: Integer]: TPHXLanguageTag read GetItem write SetItem; default;
  end;

{$ENDREGION}

{$REGION 'TPHXLanguage'}

// A language containing a list of tags for translations
//------------------------------------------------------------------------------
TPHXLanguage = class
  private
    FManager: TPHXTranslationManager;
    // Name of the language
    FName: String;
    // ISO code of the language
    FCode: String;
    // List of tags for the language
    FTags: TPHXLanguageTags;

    procedure Parse(const Text: WideString);
    procedure ParseLine(const Line: WideString);
  public
    // Creates a new language
    constructor Create(AManager: TPHXTranslationManager);
    // Free this language
    destructor Destroy; override;

    // Load the language from a file
    procedure LoadFromFile(const FileName: String);
    // Load the language from a steam
    procedure LoadFromStream(Stream: TStream);

    // Return the translation for a tag
    function Translate(const Tag: AnsiString): WideString; overload;
    // Return the translation for a tag with format support
    function Translate(const Tag: AnsiString; Args: array of const): WideString; overload;

    // Return the translation for a tag
    function Translate(const Tag: AnsiString; const Default: WideString): WideString; overload;
    // Return the translation for a tag with format support
    function Translate(const Tag: AnsiString; const Default: WideString; Args: array of const): WideString; overload;

    // Name of the language
    property Name: String read FName write FName;
    // Code of the language containing the ISO-3166 Country Codes and the ISO-639 Language Codes
    property Code: String read FCode write FCode;
    // List of tags for the language
    property Tags: TPHXLanguageTags read FTags;
  end;

{$ENDREGION}

{$REGION 'TPHXTranslationManager'}

// Class for managing languages
//------------------------------------------------------------------------------
TPHXTranslationManager = class
  private
    FLanguage: TPHXLanguage;
    function GetText(const Tag: AnsiString): WideString;
  public
    // Create a new translation manager
    constructor Create;
    // Destroys this translation manager
    destructor Destroy; override;

    // load a translation file
    procedure Load(const FileName: String);

    // Return the translation for a tag
    function Translate(const Tag: AnsiString): WideString; overload;
    // Return the translation for a tag with format support
    function Translate(const Tag: AnsiString; Args: array of const): WideString; overload;

    // The current language
    property Language: TPHXLanguage read FLanguage;
    // Return the translation for a tag
    property Texts[const Tag: AnsiString]: WideString read GetText; default;
  end;

{$ENDREGION}

// Returns the current translation manager
function Translation: TPHXTranslationManager;

implementation

{$IFDEF TAG_EXCEPTION}
resourcestring
  SMissingTag = 'Missing the tag "%s" in the language "%s".';
{$ENDIF}

const
  UTF8_Bom = #$EF#$BB#$BF;

var TranslationManager: TPHXTranslationManager;

//------------------------------------------------------------------------------
function Translation: TPHXTranslationManager;
begin
  if TranslationManager = nil then
  begin
    TranslationManager:= TPHXTranslationManager.Create;
  end;
  Result:= TranslationManager;
end;

{$REGION 'TPHXTranslationItem'}

// TPHXTranslationItem
//==============================================================================
class function TPHXLanguageTag.Create(AName: AnsiString; AText: WideString): TPHXLanguageTag;
begin
  Result.Hash := ELFHash(LowerCase(String(AName)));
  Result.Name := AName;
  Result.Text := AText;
end;

{$ENDREGION}

{$REGION 'TPHXTranslationItems'}

// TPHXGuiLanguage
//==============================================================================
constructor TPHXLanguageTags.Create;
begin
  FCount   := 0;
  FCapacity:= 0;
  FList    := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXLanguageTags.Destroy;
begin
  if FCount <> 0 then
  begin
    Finalize(FList^[0], FCount);
  end;

  SetCapacity(0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.Grow;
var Delta: Integer;
begin
  if FCapacity > 64 then
  begin
    Delta := FCapacity div 4
  end else
  if FCapacity > 8 then
  begin
    Delta := 16
  end else
  begin
    Delta := 4;
  end;

  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.Add(const Name: AnsiString; const Text: WideString);
begin
  Inc(FCount);

  if FCount >= FCapacity then Grow;

  Pointer(FList^[FCount-1].Name):= nil;
  Pointer(FList^[FCount-1].Text):= nil;

  FList^[FCount-1].Hash:= ELFHash(LowerCase(String(Name)));
  FList^[FCount-1].Name:= Name;
  FList^[FCount-1].Text:= Text;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.Delete(const Index: Integer);
begin
  Assert( (Index >= 0) and (Index < Count), 'List index out of bounds');

  Finalize(FList^[Index]);

  Dec(FCount);

  if Index < FCount then
  begin
    System.Move(FList^[Index + 1], FList^[Index],  (FCount - Index) * SizeOf(TPHXLanguageTag));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.Clear;
begin
  Finalize(FList^[0], FCount);

  SetCount   (0);
  SetCapacity(0);
end;


//------------------------------------------------------------------------------
function TPHXLanguageTags.IndexOf(const Name: AnsiString): Integer;
var Hash : Cardinal;
var Index: Integer;
begin
  Hash:= ELFHash(LowerCase(String(Name)));

  for Index:= 0 to FCount-1 do
  begin
    if (FList^[Index].Hash = Hash) and SameText(String(FList^[Index].Name), String(Name)) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= -1;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.SetCount(const Value: Integer);
begin
  FCount := Value;

  if FCount > FCapacity then SetCapacity(FCount);
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.SetCapacity(const Value: Integer);
begin
  if Value <> FCapacity then
  begin
    FCapacity := Value;

    ReallocMem(FList, FCapacity * SizeOf(TPHXLanguageTag));
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguageTags.SetItem(const Index: Integer; const Value: TPHXLanguageTag);
begin
  FList^[Index].Hash:= Value.Hash;
  FList^[Index].Name:= Value.Name;
  FList^[Index].Text:= Value.Text;
end;

//------------------------------------------------------------------------------
function TPHXLanguageTags.GetItem(const Index: Integer): TPHXLanguageTag;
begin
  Result.Hash:= FList^[Index].Hash;
  Result.Name:= FList^[Index].Name;
  Result.Text:= FList^[Index].Text;
end;

{$ENDREGION}

{$REGION 'TPHXLanguage'}

// TPHXLanguage
//==============================================================================
constructor TPHXLanguage.Create(AManager: TPHXTranslationManager);
begin
  FManager:= AManager;
  FName   := '';
  FCode   := '';
  FTags   := TPHXLanguageTags.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXLanguage.Destroy;
begin
  FTags.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguage.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:=  TPHXContentManager.CreateStream(FileName, ckText, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXLanguage.LoadFromStream(Stream: TStream);
var Data: AnsiString;
var Text: WideString;
begin
  FTags.Clear;

  SetLength(Data, Stream.Size);
  try
    Stream.Read(Data[1], Stream.Size);

    if Copy(Data, 1, 3) = UTF8_Bom then
    begin
      {$IFDEF FPC}
      Text:= UTF8Decode( Copy(Data, 4, MaxInt) );
      {$ELSE}
      Text:= UTF8ToString( Copy(Data, 4, MaxInt) );
      {$ENDIF}
    end else
    begin
      {$IFDEF FPC}
      Text:= UTF8Decode(Data);
      {$ELSE}
      Text:= UTF8ToString(RawByteString(Data));
      {$ENDIF}
    end;

    Parse(Text);
  finally
    SetLength(Data, 0);
  end;

  FName:= Translate(tag_name, '');
  FCode:= Translate(tag_code, '');
end;

//------------------------------------------------------------------------------
procedure TPHXLanguage.Parse(const Text: WideString);
var Len : Integer;
var Index: Integer;
var Line : WideString;
var Start: Integer;
begin
  Len  := Length(Text);
  Index:= 1;
  Start:= Index;
  while Index <= Len do
  begin
    // Found a newline
    if Text[Index] = #13 then
    begin
      // Extract and parse the line
      if Index <> Start then
      begin
        Line:= Copy(Text, Start, Index - Start);

        ParseLine( Line);
      end;

      Inc(Index);

      // Skip LF
      if (Index < Len) and (Text[Index] = #10) then
      begin
        Inc(Index);
      end;

      Start:= Index;
    end else
    begin
      Inc(Index);
    end;
  end;
  // Parse the last line in the file
  if Index <> Start then
  begin
    Line:= Copy(Text, Start, Index - Start);

    ParseLine( Line);
  end;

end;

//------------------------------------------------------------------------------
procedure TPHXLanguage.ParseLine(const Line: WideString);
var Len  : Integer;
var Index: Integer;
var Curr : WideChar;
var Next : WideChar;
var Value: WideString;

var TagMode : Boolean;
var TagName : WideString;
var TagValue: WideString;
begin
  Len  := Length(Line);
  Index:= 1;
  Value:= '';

  TagMode := True;
  TagName := '';
  TagValue:= '';
  while Index <= Len do
  begin
    Curr:= Line[Index];
    Next:= Line[Index+1];

    // Rest of the line is commented
    if (Curr = '-') and (Next = '-') then
    begin
      Break;
    end else
    // End of tag name, start of value
    if (Curr = '=') then
    begin
      TagMode:= False;
    end else
    // Insert a newline
    if (Curr = '\') and (Next = 'n')  then
    begin
      Value:= sLineBreak;
    end else
    // Insert a ecaped slash
    if (Curr = '\') and (Next = '\')  then
    begin
      Value:= '\';
    end else
    // Insert a normal character
    begin
      Value:= Line[Index];
    end;

    if Length(Value) > 0 then
    begin
      if TagMode then
      begin
        TagName:= TagName + Value;
      end else
      begin
        TagValue:= TagValue + Value;
      end;
      Value:= '';
    end;

    Inc(Index);
  end;

  TagName := Trim(TagName);
  TagValue:= Trim(TagValue);

  if (TagName <> '') and (TagValue <> '') then
  begin
    Tags.Add(AnsiString(TagName), TagValue);
  end;
end;

//------------------------------------------------------------------------------
function TPHXLanguage.Translate(const Tag: AnsiString): WideString;
var Index: Integer;
begin
  Index:= FTags.IndexOf(Tag);

  if Index >= 0 then
  begin
    Result:= FTags.List^[Index].Text;
  end else
  begin
    {$IFDEF TAG_EXCEPTION}
    raise ELanguageException.CreateFmt(SMissingTag, [String(Tag), FName]);
    {$ENDIF}

    Result:= WideString(Tag);
  end;
end;

//------------------------------------------------------------------------------
function TPHXLanguage.Translate(const Tag: AnsiString; Args: array of const): WideString;
var Index: Integer;
begin
  Index:= FTags.IndexOf(Tag);

  if Index >= 0 then
  begin
     Result:= Format(FTags.List^[Index].Text, Args);
  end else
  begin
    {$IFDEF TAG_EXCEPTION}
    raise ELanguageException.CreateFmt(SMissingTag, [String(Tag), FName]);
    {$ENDIF}
    Result:= WideString(Tag);
  end;
end;

//------------------------------------------------------------------------------
function TPHXLanguage.Translate(const Tag: AnsiString; const Default: WideString): WideString;
var Index: Integer;
begin
  Index:= FTags.IndexOf(Tag);

  if Index >= 0 then
  begin
    Result:= FTags.List^[Index].Text;
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXLanguage.Translate(const Tag: AnsiString; const Default: WideString; Args: array of const): WideString;
var Index: Integer;
begin
  Index:= FTags.IndexOf(Tag);

  if Index >= 0 then
  begin
     Result:= Format(FTags.List^[Index].Text, Args);
  end else
  begin
    Result:= Format(Default, Args);
  end;
end;


{$ENDREGION}

{$REGION 'TPHXTranslationManager'}

// http://docs.oracle.com/cd/E13214_01/wli/docs92/xref/xqisocodes.html

// TPHXTranslationManager
//==============================================================================
constructor TPHXTranslationManager.Create;
begin
  FLanguage:= TPHXLanguage.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TPHXTranslationManager.Destroy;
begin
  FLanguage.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXTranslationManager.Load(const FileName: String);
begin
  FLanguage.LoadFromFile(FileName)
end;

//------------------------------------------------------------------------------
function TPHXTranslationManager.Translate(const Tag: AnsiString): WideString;
begin
  Result:= FLanguage.Translate(Tag)
end;

//------------------------------------------------------------------------------
function TPHXTranslationManager.Translate(const Tag: AnsiString; Args: array of const): WideString;
begin
  Result:= FLanguage.Translate(Tag, Args)
end;

//------------------------------------------------------------------------------
function TPHXTranslationManager.GetText(const Tag: AnsiString): WideString;
begin
  Result:= FLanguage.Translate(Tag)
end;

{$ENDREGION}



initialization
  TranslationManager:= nil;
finalization
  if Assigned(TranslationManager) then
  begin
    TranslationManager.Free;
  end;
end.

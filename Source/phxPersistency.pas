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
unit phxPersistency;
//< Classes for loading and saving objects to version independent files

interface

{$I phxConfig.inc}

uses Classes, Types, SysUtils, Variants,

  phxTypes,
  phxClasses;

const

// Version number of the document files
DOCUMENT_VERSION = 3;
// Chunk identifier for nodes
IDENT_NODE = $0101;
// Chunk identifier for atributes
IDENT_ATTR = $0102;

type

TPHXNode  = class;
TPHXNodes = class;

// Load and save documents to xml using JclSimpleXml
//{$DEFINE DOCXML_JCL}
// Load and save documents to xml using the VCL Xml
//{$DEFINE DOCXML_VCL}

{$MINENUMSIZE 4}

// Posible datatypes of attributes
//------------------------------------------------------------------------------
TPHXValueKind = (
  // The attribute has no value
  vkEmpty = 0,
  // The attribute contains a placeholder variant value
  vkVariant = 1,
  // Binary blob of data
  vkBinary = 2,
  // 8 bit unsigned
  vkByte = 3,
  // 16 bit unsigned
  vkWord = 4,
  // 32 bit unsigned
  vkCardinal = 5,
  // 32 bit signed
  vkInteger = 6,
  // Single precision float
  vkSingle = 7,
  // Double precision float
  vkDouble = 8,
  // Boolean
  vkBoolean = 9,
  // Unicode string
  vkString = 10
);

// A attribute contains a value in a node
//------------------------------------------------------------------------------
TPHXAttribute = class
  private
    FNode: TPHXNode;
    FName: AnsiString;
    FHash: Cardinal;
    FKind: TPHXValueKind;
    FSize: Integer;
    FData: Pointer;

    // The value if the kind is vkVariant
    FValue: Variant;

    // Returns the value as a variant
    function GetValue: Variant;

    procedure SetName(const Value: AnsiString);
  public
    // Create a empty attribute
    constructor Create(ANode: TPHXNode);
    // Destroy the attribute
    destructor Destroy; override;

    // Load the attribute from a stream
    procedure LoadAttribute(Stream: TStream);
    // Save the attribute to a stream
    procedure SaveAttribute(Stream: TStream);

    // Resizes the attribute
    procedure Resize(const Size: Integer; const Kind: TPHXValueKind);

    // Read binary data from the attribute
    function ReadBinary(var Buffer: Pointer; const Size: Integer): PByte;
    // Read a byre from the attribute
    function ReadByte(const Default: Byte = 0): Byte;
    // Read a word from the attribute
    function ReadWord(const Default: Word = 0): Word;
    // Read a cardinal from the attribute
    function ReadCardinal(const Default: Cardinal = 0): Cardinal;
    // Read a integer from the attribute
    function ReadInteger(const Default: Integer = 0): Integer;
    // Read a single precision float
    function ReadSingle(const Default: Single = 0): Single;
    // Read a double precision float
    function ReadDouble(const Default: Double = 0): Double;
    // Read a boolean
    function ReadBoolean(const Default: Boolean = False): Boolean;
    // Read a string from the attribute
    function ReadString(const Default: String = ''): String;


    // Write binary data to the attribute
    procedure WriteBinary(const Buffer: Pointer; const Size: Integer);
    // Write a byre to the attribute
    procedure WriteByte(const Value: Byte);
    // Write a word to the attribute
    procedure WriteWord(const Value: Word);
    // Write a cardinal to the attribute
    procedure WriteCardinal(const Value: Cardinal);
    // Write a integer to the attribute
    procedure WriteInteger(const Value: Integer);
    // Write a single to the attribute
    procedure WriteSingle(const Value: Single);
    // Write a double to the attribute
    procedure WriteDouble(const Value: Double);
    // Write a boolean to the attribute
    procedure WriteBoolean(const Value: Boolean);
    // Write a string to the attribute
    procedure WriteString(const Value: String);

    // The owning document node
    property Node: TPHXNode read FNode;
    // Name of the attribute
    property Name: AnsiString read FName write SetName;
    // Hash code of the attribute name
    property Hash: Cardinal read FHash;
    // Data type of the attribute value
    property Kind: TPHXValueKind read FKind;
    // Size of the atribute in bytes
    property Size: Integer read FSize;
    // Hash code of the attribute name
    property Data: Pointer read FData;
    // Read the value of the attribute as a variant
    property Value: Variant read GetValue;
  end;

PAttributeList = ^ TAttributeList;
TAttributeList = array[0..$00FFFFFF] of TPHXAttribute;

// List of attributes
//------------------------------------------------------------------------------
TPHXAttributes = class
  private
    FNode: TPHXNode;
    FList: TList;

    // TODO: Add hash map for attributes
    // FMap:
    //FBucketList: array of THashBucket;

    function GetCount: Integer;
    function GetList: PAttributeList;
    function GetItem(const Index: Integer): TPHXAttribute;
  public
    // Create a empty list
    constructor Create(ANode: TPHXNode);
    destructor Destroy; override;

    // Load the attributes from a stream
    procedure LoadAttributes(Stream: TStream);
    // Save the attributes from a stream
    procedure SaveAttributes(Stream: TStream);

    // Remove and free all atributes
    procedure Clear;
    // Add a new attribute to the list
    function Add(const Name: AnsiString): TPHXAttribute;

    // Check if the list contains a attribute
    function Contains(const Name: AnsiString): Boolean;
    // Find a attribute in the list
    function Find(const Name: AnsiString): TPHXAttribute; overload;
    // Find a attribute in the list
    function Find(const Name: AnsiString; out Attribute: TPHXAttribute): Boolean; overload;

    // Read binary data from the attribute
    function ReadBinary(const Name: AnsiString; var Buffer: Pointer; const Size: Integer): PByte;
    // Read a byte attribute
    function ReadByte(const Name: AnsiString; const Default: Byte = 0): Byte;
    // Read a word attribute
    function ReadWord(const Name: AnsiString; const Default: Word = 0): Word;
    // Read a cardinal attribute
    function ReadCardinal(const Name: AnsiString; const Default: Cardinal = 0): Cardinal;
    // Read a integer attribute
    function ReadInteger(const Name: AnsiString; const Default: Integer = 0): Integer;
    // Read a single precision float
    function ReadSingle(const Name: AnsiString; const Default: Single = 0): Single;
    // Read a double precision float
    function ReadDouble(const Name: AnsiString; const Default: Double = 0): Double;
    // Read a boolean
    function ReadBoolean(const Name: AnsiString; const Default: Boolean = False): Boolean;
    // Read a string from the attribute
    function ReadString(const Name: AnsiString; const Default: String = ''): String;


    // Write binary data to the attribute
    procedure WriteBinary(const Name: AnsiString; const Buffer: Pointer; const Size: Integer);
    // Write a byte attribute
    procedure WriteByte(const Name: AnsiString; const Value: Byte);
    // Write a word to the attribute
    procedure WriteWord(const Name: AnsiString; const Value: Word);
    // Write a cardinal to the attribute
    procedure WriteCardinal(const Name: AnsiString; const Value: Cardinal);
    // Write a integer to the attribute
    procedure WriteInteger(const Name: AnsiString; const Value: Integer);
    // Write a single to the attribute
    procedure WriteSingle(const Name: AnsiString; const Value: Single);
    // Write a double to the attribute
    procedure WriteDouble(const Name: AnsiString; const Value: Double);
    // Write a boolean to the attribute
    procedure WriteBoolean(const Name: AnsiString; const Value: Boolean);
    // Write a string to the attribute
    procedure WriteString(const Name: AnsiString; const Value: String);

    // The owning document node
    property Node: TPHXNode read FNode;
    // Number of attributes
    property Count: Integer read GetCount;
    // Returns a pointer to the internal list
    property List: PAttributeList read GetList;
    // Return a attribute from the list
    property Items[const Index: Integer]: TPHXAttribute read GetItem; default;
  end;

//------------------------------------------------------------------------------
IPHXNode = interface
  // Add a child node tho this node
  function AddChild(const Name: AnsiString): TPHXNode;
  // Find a child node tho this node
  function FindChild(const Name: AnsiString): TPHXNode;
end;

//------------------------------------------------------------------------------
TPHXNode = class(TInterfacedObject, IPHXNode)
  private
    FName      : AnsiString;
    FHash      : Cardinal;
    FAttributes: TPHXAttributes;
    FChildren  : TPHXNodes;

    procedure SetName(const Value: AnsiString);
  public
    // Default constructor
    constructor Create;
    destructor Destroy; override;

    // Save the node to a stream
    procedure SaveNode(Stream: TStream);
    // Load the node from a stream
    procedure LoadNode(Stream: TStream);

    // Add a child node tho this node
    function AddChild(const Name: AnsiString): TPHXNode;
    // Find a child node tho this node
    function FindChild(const Name: AnsiString): TPHXNode;

    // Returns true if the node contains a attribute by name
    function HasAttribute(const Name: AnsiString): Boolean;

    {$REGION 'Attribute readers'}
    // Read binary data from the attributes
    function ReadBinary(const Name: AnsiString; var Buffer: Pointer; const Size: Integer): PByte;
    // Read a byte attribute
    function ReadByte(const Name: AnsiString; const Default: Byte = 0): Byte;
    // Read a word attribute
    function ReadWord(const Name: AnsiString; const Default: Word = 0): Word;
    // Read a cardinal attribute
    function ReadCardinal(const Name: AnsiString; const Default: Cardinal = 0): Cardinal;
    // Read a integer attribute
    function ReadInteger(const Name: AnsiString; const Default: Integer = 0): Integer;
    // Read a single precision float
    function ReadSingle(const Name: AnsiString; const Default: Single = 0): Single;
    // Read a double precision float
    function ReadDouble(const Name: AnsiString; const Default: Double = 0): Double;
    // Read a boolean
    function ReadBoolean(const Name: AnsiString; const Default: Boolean = False): Boolean;
    // Read a string from the attribute
    function ReadString(const Name: AnsiString; const Default: String = ''): String;
    {$ENDREGION}

    {$REGION 'Attribute writers'}
    // Write binary data to the attribute
    procedure WriteBinary(const Name: AnsiString; const Buffer: Pointer; const Size: Integer);
    // Write a byte attribute
    procedure WriteByte(const Name: AnsiString; const Value: Byte);
    // Write a word to the attribute
    procedure WriteWord(const Name: AnsiString; const Value: Word);
    // Write a cardinal to the attribute
    procedure WriteCardinal(const Name: AnsiString; const Value: Cardinal);
    // Write a integer to the attribute
    procedure WriteInteger(const Name: AnsiString; const Value: Integer);
    // Write a single to the attribute
    procedure WriteSingle(const Name: AnsiString; const Value: Single);
    // Write a double to the attribute
    procedure WriteDouble(const Name: AnsiString; const Value: Double);
    // Write a boolean to the attribute
    procedure WriteBoolean(const Name: AnsiString; const Value: Boolean);
    // Write a string to the attribute
    procedure WriteString(const Name: AnsiString; const Value: String);
    {$ENDREGION}

    // Name of the node
    property Name: AnsiString read FName write SetName;
    // Hash code of the node name
    property Hash: Cardinal read FHash;
    // List of attributes
    property Attributes: TPHXAttributes read FAttributes;
    // List of child nodes
    property Children: TPHXNodes read FChildren;
 end;

PNodeList = ^ TNodeList;
TNodeList = array[0..$00FFFFFF] of TPHXNode;

// List of document nodes
//------------------------------------------------------------------------------
TPHXNodes = class
  private
    FNode: TPHXNode;
    FList: TList;

    function GetCount: Integer;
    function GetList: PNodeList;
    function GetItem(const Index: Integer): TPHXNode;
  public
    // Default constructor
    constructor Create(ANode: TPHXNode);
    // Default destructor
    destructor Destroy; override;

    // Load the nodes from a stream
    procedure LoadNodes(Stream: TStream);
    // Save the nodes from a stream
    procedure SaveNodes(Stream: TStream);

    // Remove and free all nodes
    procedure Clear;
    // Add a new node to the list
    function Add(const Name: AnsiString): TPHXNode;

    // Check if the list contains a attribute
    function Contains(const Name: AnsiString): Boolean;
    // Find a node in the list
    function Find(const Name: AnsiString): TPHXNode; overload;
    // Find a node in the list
    function Find(const Name: AnsiString; out Node: TPHXNode): Boolean; overload;

    // The owning document node
    property Node: TPHXNode read FNode;
    // Number of attributes
    property Count: Integer read GetCount;
    // Returns a pointer to the internal list
    property List: PNodeList read GetList;
    // Return a attribute from the list
    property Items[const Index: Integer]: TPHXNode read GetItem; default;
  end;

// Header struct for documents
//------------------------------------------------------------------------------
TPHXDocumentHeader = record
  // Identifier of the document file PHXDOC
  Ident: array[0..5] of AnsiChar;
  // Version of the document file
  Version: Cardinal;
end;

// Format for document files
//------------------------------------------------------------------------------
TPHXDocumentFormat = (
  // Determine the format from the filename
  dfAuto,
  // Use the binary format
  dfBinary,
  // Use human readable format using xml
  dfXML
);

//------------------------------------------------------------------------------
IPHXDocument = interface
  {$REGION 'Property getters'}
  // Returns the name of the document
  function GetName: String;
  // Returns the root node of the document
  function GetRoot: TPHXNode;
  {$ENDREGION}

  // Load the document from a file
  procedure LoadFromFile(const FileName: String);
  // Save the document to a file
  procedure SaveToFile(const FileName: String);

  // Load the document from a stream
  procedure SaveToStream(Stream: TStream);
  // Save the document from a stream
  procedure LoadFromStream(Stream: TStream);

  // Load the document from a xml file
  procedure LoadFromXml(const FileName: String);
  // Save the document to a xml file
  procedure SaveToXml(const FileName: String);

  // Name of the document
  property Name: String read GetName;
  // The root node of the document
  property Root: TPHXNode read GetRoot;
end;

//------------------------------------------------------------------------------
TPHXDocument = class(TInterfacedObject, IPHXDocument)
  private
    FName: String;
    FRoot: TPHXNode;

    function GetName: String;
    function GetRoot: TPHXNode;
  public
    // Creates a new document
    constructor Create; overload;
    // Creates a new document with a named root node
    constructor Create(const Root: AnsiString); overload;
    // Destroys this document
    destructor Destroy; override;

    // Load the document from a file
    procedure LoadFromFile(const FileName: String);
    // Save the document to a file
    procedure SaveToFile(const FileName: String);

    // Load the document from a stream
    procedure SaveToStream(Stream: TStream);
    // Save the document from a stream
    procedure LoadFromStream(Stream: TStream);

    // Load the document from a xml file
    procedure LoadFromXml(const FileName: String);
    // Save the document to a xml file
    procedure SaveToXml(const FileName: String);

    // Name of the document
    property Name: String read FName write FName;
    // The root node of the document
    property Root: TPHXNode read FRoot;
  end;

// Creates a new document
function NewDocument: IPHXDocument; overload;
// Creates a new document with a named root node
function NewDocument(const Root: AnsiString): IPHXDocument; overload;

function LoadDocument(const FileName: String): IPHXDocument; overload;
function LoadDocument(Stream: TStream): IPHXDocument; overload;

implementation

{$IFDEF DOCXML_JCL}
uses JclSimpleXml;
{$ENDIF}
{$IFDEF DOCXML_VCL}
uses XMLDoc, XMLIntf;
{$ENDIF}

//------------------------------------------------------------------------------
function ELFHash(const Value: AnsiString) : Cardinal;
var i: Cardinal;
var x: Cardinal;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x      := Result and $F0000000;
    if (x <> 0) then
    begin
      Result := Result xor (x shr 24);
    end;
    Result := Result and (not x);
  end;
end;

//------------------------------------------------------------------------------
function NewDocument: IPHXDocument;
begin
  Result:= TPHXDocument.Create;
end;

//------------------------------------------------------------------------------
function NewDocument(const Root: AnsiString): IPHXDocument;
begin
  Result:= TPHXDocument.Create(Root);
end;

//------------------------------------------------------------------------------
function LoadDocument(const FileName: String): IPHXDocument;
begin
  Result:= TPHXDocument.Create;
  Result.LoadFromFile(FileName);
end;

//------------------------------------------------------------------------------
function LoadDocument(Stream: TStream): IPHXDocument;
begin
  Result:= TPHXDocument.Create;
  Result.LoadFromStream(Stream);
end;

//------------------------------------------------------------------------------
procedure WriteName(Stream: TStream; const Name: AnsiString);
var Len: Cardinal;
begin
  Len:= Length(Name);

  Stream.Write(Len, 4);

  if Len > 0 then
  begin
    Stream.WriteBuffer(Name[1], Len);
  end;
end;

//------------------------------------------------------------------------------
procedure ReadName(Stream: TStream; out Name: AnsiString);
var Len: Cardinal;
begin
  Stream.Read(Len, 4);

  if Len > 0 then
  begin
    SetLength(Name, Len);

    Stream.ReadBuffer(Name[1], Len);
  end else
  begin
    Name:= '';
  end;
end;

{$REGION 'TPHXAttribute'}

// TPHXAttribute
//==============================================================================
constructor TPHXAttribute.Create(ANode: TPHXNode);
begin
  FNode := ANode;
  FName := '';
  FHash := 0;
  FKind := vkEmpty;
  FSize := 0;
  FData := nil;
end;

//------------------------------------------------------------------------------
destructor TPHXAttribute.Destroy;
begin
  ReallocMem(FData, 0);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.LoadAttribute(Stream: TStream);
var Ident: Word;
begin
  // Read attribute ident
  Stream.Read(Ident, SizeOf(Word));

  if Ident <> IDENT_ATTR then
  begin
    raise Exception.Create('Invalid attribute ident.');
  end;

  ReadName(Stream, FName);

  Stream.Read(FHash, SizeOf(FHash));
  Stream.Read(FKind, SizeOf(FKind));
  Stream.Read(FSize, SizeOf(FSize));

  ReallocMem(FData, FSize);

  Stream.Read(FData^, FSize);
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.SaveAttribute(Stream: TStream);
var Ident: Word;
begin
  Ident:= IDENT_ATTR;

  // Write attribute ident
  Stream.Write(Ident, SizeOf(Word));

  WriteName(Stream, FName);

  Stream.Write(FHash, SizeOf(FHash));
  Stream.Write(FKind, SizeOf(FKind));
  Stream.Write(FSize, SizeOf(FSize));

  Stream.Write(FData^, FSize);
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.Resize(const Size: Integer; const Kind: TPHXValueKind);
begin
  FKind:= Kind;

  if FSize <> Size then
  begin
    FSize:= Size;

    ReallocMem(FData, FSize);
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadBinary(var Buffer: Pointer; const Size: Integer): PByte;
begin
  if (FSize = Size) and (Kind = vkBinary) then
  begin
    System.Move(FData, Buffer, Size);

    Result:= PByte(FData);
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    Result:= nil;
  end else
  // Return the default value
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadByte(const Default: Byte = 0): Byte;
begin
  if (Size = 1) and (Kind = vkByte) then
  begin
    Result:= PByte(FData)^;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadWord(const Default: Word = 0): Word;
begin
  if (Size = 2) and (Kind = vkWord) then
  begin
    Result:= PWord(FData)^;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadCardinal(const Default: Cardinal = 0): Cardinal;
begin
  if (Size = 4) and (Kind = vkCardinal) then
  begin
    Result:= PCardinal(FData)^;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadInteger(const Default: Integer): Integer;
begin
  if (Size = 4) and (Kind = vkInteger) then
  begin
    Result:= PInteger(FData)^;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadSingle(const Default: Single): Single;
begin
  if (Size = SizeOf(Single)) and (Kind = vkSingle) then
  begin
    Result:= PSingle(FData)^;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadDouble(const Default: Double): Double;
begin
  if (Size = SizeOf(Double)) and (Kind = vkDouble) then
  begin
    Result:= PDouble(FData)^;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadBoolean(const Default: Boolean): Boolean;
begin
  if (Size = 1) and (Kind = vkBoolean) then
  begin
    Result:= PByte(FData)^ = 1;
  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttribute.ReadString(const Default: String): String;
var Str: UnicodeString;
var Len: Cardinal;
begin
  if (Size >= 4) and (Kind = vkString) then
  begin
    // read the string lengh
    Len:= PCardinal(FData)^;

    if Len > 0 then
    begin
      SetLength(Str, Len);

      System.Move( PByte(PByte(FData) + 4)^, Str[1], 2 * Len);

      Result:= String(Str);
    end else
    begin
      Result:= '';
    end;

  end else
  // Return the placeholder variant
  if (Kind = vkVariant) then
  begin
    if VarIsNull(FValue) then
    begin
      Result:= Default;
    end else
    begin
      Result:= FValue;
    end;
  end else
  // Return the default value
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteBinary(const Buffer: Pointer; const Size: Integer);
begin
  Resize(Size, vkBinary);

  System.Move(Buffer, FData, Size);
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteByte(const Value: Byte);
begin
  Resize(1, vkByte);

  PByte(FData)^:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteWord(const Value: Word);
begin
  Resize(2, vkWord);

  PWord(FData)^:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteCardinal(const Value: Cardinal);
begin
  Resize(4, vkCardinal);

  PCardinal(FData)^:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteInteger(const Value: Integer);
begin
  Resize(4, vkInteger);

  PInteger(FData)^:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteSingle(const Value: Single);
begin
  Resize(SizeOf(Single), vkSingle);

  PSingle(FData)^:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteDouble(const Value: Double);
begin
  Resize(SizeOf(Double), vkDouble);

  PDouble(FData)^:= Value;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteBoolean(const Value: Boolean);
begin
  Resize(SizeOf(Byte), vkBoolean);

  if Value then
  begin
    PByte(FData)^:= 1;
  end else
  begin
    PByte(FData)^:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.WriteString(const Value: String);
var Str: UnicodeString;
var Len: Cardinal;
begin
  Str:= UnicodeString(Value);
  Len:= Length(Str);

  Resize(4 + 2 * Len, vkString);

  PCardinal(FData)^:= Len;

  System.Move(Str[1], PByte(PByte(FData) + 4)^, 2 * Len);
end;

//------------------------------------------------------------------------------
function TPHXAttribute.GetValue: Variant;
begin
  Result:= '';

  case Kind of
    vkVariant : Result:= FValue;
    vkBinary  : Result:= '';
    vkByte    : Result:= ReadByte;
    vkWord    : Result:= ReadWord;
    vkCardinal: Result:= ReadCardinal;
    vkInteger : Result:= ReadInteger;
    vkSingle  : Result:= ReadSingle;
    vkDouble  : Result:= ReadDouble;
    vkBoolean : Result:= ReadBoolean;
    vkString  : Result:= ReadString;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttribute.SetName(const Value: AnsiString);
begin
  FName:= Value;
  FHash:= ELFHash(FName);
end;

{$ENDREGION}

{$REGION 'TPHXAttributeList'}


// TPHXAttributeList
//==============================================================================
constructor TPHXAttributes.Create(ANode: TPHXNode);
begin
  FNode:= ANode;
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXAttributes.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.LoadAttributes(Stream: TStream);
var Count: Integer;
var Index: Integer;
var Item : TPHXAttribute;
begin
  Clear;

  Stream.Read(Count, SizeOf(Count));

  for Index := 0 to Count-1 do
  begin
    Item:= TPHXAttribute.Create(Node);
    Item.LoadAttribute(Stream);

    FList.Add(Item);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.SaveAttributes(Stream: TStream);
var Count: Integer;
var Index: Integer;
var Item : TPHXAttribute;
begin
  Count:= FList.Count;

  Stream.Write(Count, SizeOf(Count));

  for Index := 0 to Count-1 do
  begin
    Item:= TPHXAttribute(FList.List[Index]);

    Item.SaveAttribute(Stream);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    TPHXAttribute(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.Add(const Name: AnsiString): TPHXAttribute;
begin
  Result:= TPHXAttribute.Create(Node);
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXAttributes.Contains(const Name: AnsiString): Boolean;
begin
  Result:= Find(Name) <> nil;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.Find(const Name: AnsiString): TPHXAttribute;
var Index: Integer;
var Hash : Cardinal;
var Item : TPHXAttribute;
begin
  Hash:= ELFHash(Name);

  for Index:= 0 to FList.Count - 1 do
  begin
    Item:= TPHXAttribute(FList.List[Index]);

    if (Item.Hash = Hash) and (Item.Name = Name) then
    begin
      Result:= Item;

      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.Find(const Name: AnsiString; out Attribute: TPHXAttribute): Boolean;
var Index: Integer;
var Hash : Cardinal;
var Item : TPHXAttribute;
begin
  Hash:= ELFHash(Name);

  for Index := 0 to FList.Count - 1 do
  begin
    Item:= TPHXAttribute(FList.List[Index]);

    if (Item.Hash = Hash) and (Item.Name = Name) then
    begin
      Result   := True;
      Attribute:= Item;

      Exit;
    end;
  end;
  Result   := False;
  Attribute:= nil;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadBinary(const Name: AnsiString; var Buffer: Pointer; const Size: Integer): PByte;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadBinary(Buffer, Size);
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadByte(const Name: AnsiString; const Default: Byte): Byte;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadByte(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadWord(const Name: AnsiString; const Default: Word): Word;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadWord(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadCardinal(const Name: AnsiString; const Default: Cardinal): Cardinal;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadCardinal(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadInteger(const Name: AnsiString; const Default: Integer): Integer;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadInteger(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadSingle(const Name: AnsiString; const Default: Single): Single;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadSingle(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadDouble(const Name: AnsiString; const Default: Double): Double;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadDouble(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadBoolean(const Name: AnsiString; const Default: Boolean): Boolean;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadBoolean(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.ReadString(const Name: AnsiString; const Default: String): String;
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadString(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteBinary(const Name: AnsiString; const Buffer: Pointer; const Size: Integer);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteBinary(Buffer, Size);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteBinary(Buffer, Size);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteByte(const Name: AnsiString; const Value: Byte);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteByte(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteByte(Value);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteWord(const Name: AnsiString; const Value: Word);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteWord(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteWord(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteCardinal(const Name: AnsiString; const Value: Cardinal);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteCardinal(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteCardinal(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteInteger(const Name: AnsiString; const Value: Integer);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteInteger(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteInteger(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteSingle(const Name: AnsiString; const Value: Single);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteSingle(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteSingle(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteDouble(const Name: AnsiString; const Value: Double);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteDouble(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteDouble(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteBoolean(const Name: AnsiString; const Value: Boolean);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteBoolean(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteBoolean(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAttributes.WriteString(const Name: AnsiString; const Value: String);
var Attribute: TPHXAttribute;
begin
  if Find(Name, Attribute) then
  begin
    Attribute.WriteString(Value);
  end else
  begin
    Attribute:= Add(Name);
    Attribute.WriteString(Value);
  end;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXAttributes.GetList: PAttributeList;
begin
  Result:= PAttributeList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXAttributes.GetItem(const Index: Integer): TPHXAttribute;
begin
  Result:= FList.List[Index];
end;


{$ENDREGION}

{$REGION 'TPHXNode'}

// TPHXNode
//==============================================================================
constructor TPHXNode.Create;
begin
  FName      := '';
  FHash      := 0;
  FAttributes:= TPHXAttributes.Create(Self);
  FChildren  := TPHXNodes.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TPHXNode.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXNode.LoadNode(Stream: TStream);
var Ident: Word;
begin
  Stream.Read(Ident, SizeOf(Word));

  if Ident <> IDENT_NODE then
  begin
    raise Exception.Create('Invalid node ident.');
  end;

  ReadName(Stream, FName);

  Stream.Read(FHash, SizeOf(FHash));

  FAttributes.LoadAttributes(Stream);

  FChildren.LoadNodes(Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXNode.SaveNode(Stream: TStream);
var Ident: Word;
begin
  Ident:= IDENT_NODE;

  Stream.Write(Ident, SizeOf(Word));

  WriteName(Stream, FName);

  Stream.Write(FHash, SizeOf(FHash));

  FAttributes.SaveAttributes(Stream);

  FChildren.SaveNodes(Stream);
end;

//------------------------------------------------------------------------------
function TPHXNode.AddChild(const Name: AnsiString): TPHXNode;
begin
  Result:= Children.Add(Name);
end;

//------------------------------------------------------------------------------
function TPHXNode.FindChild(const Name: AnsiString): TPHXNode;
begin
  Result:= Children.Find(Name);
end;

//------------------------------------------------------------------------------
function TPHXNode.HasAttribute(const Name: AnsiString): Boolean;
begin
  Result:= FAttributes.Find(Name) <> nil;
end;

{$REGION 'Attribute readers'}

//------------------------------------------------------------------------------
function TPHXNode.ReadBinary(const Name: AnsiString; var Buffer: Pointer; const Size: Integer): PByte;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadBinary(Buffer, Size);
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadByte(const Name: AnsiString; const Default: Byte): Byte;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadByte(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadWord(const Name: AnsiString; const Default: Word): Word;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadWord(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadCardinal(const Name: AnsiString; const Default: Cardinal): Cardinal;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadCardinal(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadInteger(const Name: AnsiString; const Default: Integer): Integer;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadInteger(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadSingle(const Name: AnsiString; const Default: Single): Single;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadSingle(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadDouble(const Name: AnsiString; const Default: Double): Double;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadDouble(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadBoolean(const Name: AnsiString; const Default: Boolean): Boolean;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadBoolean(Default);
  end else
  begin
    Result:= Default;
  end;
end;

//------------------------------------------------------------------------------
function TPHXNode.ReadString(const Name: AnsiString; const Default: String): String;
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Result:= Attribute.ReadString(Default);
  end else
  begin
    Result:= Default;
  end;
end;

{$ENDREGION}

{$REGION 'Attribute writers'}

//------------------------------------------------------------------------------
procedure TPHXNode.WriteBinary(const Name: AnsiString; const Buffer: Pointer; const Size: Integer);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteBinary(Buffer, Size);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteBinary(Buffer, Size);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteByte(const Name: AnsiString; const Value: Byte);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteByte(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteByte(Value);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXNode.WriteWord(const Name: AnsiString; const Value: Word);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteWord(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteWord(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteCardinal(const Name: AnsiString; const Value: Cardinal);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteCardinal(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteCardinal(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteInteger(const Name: AnsiString; const Value: Integer);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteInteger(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteInteger(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteSingle(const Name: AnsiString; const Value: Single);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteSingle(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteSingle(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteDouble(const Name: AnsiString; const Value: Double);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteDouble(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteDouble(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteBoolean(const Name: AnsiString; const Value: Boolean);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteBoolean(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteBoolean(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNode.WriteString(const Name: AnsiString; const Value: String);
var Attribute: TPHXAttribute;
begin
  if Attributes.Find(Name, Attribute) then
  begin
    Attribute.WriteString(Value);
  end else
  begin
    Attribute:= Attributes.Add(Name);
    Attribute.WriteString(Value);
  end;
end;

{$ENDREGION}



//------------------------------------------------------------------------------
procedure TPHXNode.SetName(const Value: AnsiString);
begin
  FName:= Value;
  FHash:= ELFHash(Value);
end;

{$ENDREGION}

{$REGION 'TPHXNodeList'}

// TPHXNodeList
//==============================================================================
constructor TPHXNodes.Create(ANode: TPHXNode);
begin
  FNode:= ANode;
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TPHXNodes.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXNodes.LoadNodes(Stream: TStream);
var Count: Integer;
var Index: Integer;
var Item : TPHXNode;
begin
  Clear;

  Stream.Read(Count, SizeOf(Count));

  for Index := 0 to Count-1 do
  begin
    Item:= TPHXNode.Create;
    Item.LoadNode(Stream);

    FList.Add(Item);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNodes.SaveNodes(Stream: TStream);
var Count: Integer;
var Index: Integer;
var Item : TPHXNode;
begin
  Count:= FList.Count;

  Stream.Write(Count, SizeOf(Count));

  for Index := 0 to Count-1 do
  begin
    Item:= TPHXNode(FList.List[Index]);

    Item.SaveNode(Stream);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXNodes.Clear;
var Index: Integer;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    TPHXNode(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TPHXNodes.Add(const Name: AnsiString): TPHXNode;
begin
  Result:= TPHXNode.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TPHXNodes.Contains(const Name: AnsiString): Boolean;
begin
  Result:= Find(Name) <> nil;
end;

//------------------------------------------------------------------------------
function TPHXNodes.Find(const Name: AnsiString): TPHXNode;
var Index: Integer;
var Hash : Cardinal;
var Item : TPHXNode;
begin
  Hash:= ELFHash(Name);

  for Index := 0 to FList.Count - 1 do
  begin
    Item:= TPHXNode(FList.List[Index]);

    if (Item.Hash = Hash) and (Item.Name = Name) then
    begin
      Result:= Item;

      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TPHXNodes.Find(const Name: AnsiString; out Node: TPHXNode): Boolean;
var Index: Integer;
var Hash : Cardinal;
var Item : TPHXNode;
begin
  Hash:= ELFHash(Name);

  for Index := 0 to FList.Count - 1 do
  begin
    Item:= TPHXNode(FList.List[Index]);

    if (Item.Hash = Hash) and (Item.Name = Name) then
    begin
      Result:= True;
      Node  := Item;

      Exit;
    end;
  end;
  Result:= False;
  Node  := nil;
end;


//------------------------------------------------------------------------------
function TPHXNodes.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TPHXNodes.GetList: PNodeList;
begin
  Result:= PNodeList(FList.List);
end;

//------------------------------------------------------------------------------
function TPHXNodes.GetItem(const Index: Integer): TPHXNode;
begin
  Result:= FList.List[Index];
end;

{$ENDREGION}

{$REGION 'TPHXDocument'}

// TPHXDocument
//==============================================================================
constructor TPHXDocument.Create;
begin
  FRoot:= TPHXNode.Create;
  FRoot.Name:= 'document';
end;

//------------------------------------------------------------------------------
constructor TPHXDocument.Create(const Root: AnsiString);
begin
  FRoot:= TPHXNode.Create;
  FRoot.Name:= Root;
end;

//------------------------------------------------------------------------------
destructor TPHXDocument.Destroy;
begin
  FRoot.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXDocument.LoadFromFile(const FileName: String);
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
procedure TPHXDocument.SaveToFile(const FileName: String);
var Stream: TStream;
begin
  Stream:=  TPHXContentManager.CreateStream(FileName, ckText, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXDocument.SaveToStream(Stream: TStream);
var Header: TPHXDocumentHeader;
begin
  Header.Ident  := 'PHXDOC';
  Header.Version:= DOCUMENT_VERSION;

  Stream.Write(Header.Ident  , SizeOf(Header.Ident));
  Stream.Write(Header.Version, SizeOf(Header.Version));

  Root.SaveNode(Stream);
end;

//------------------------------------------------------------------------------
procedure TPHXDocument.LoadFromStream(Stream: TStream);
var Header: TPHXDocumentHeader;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXDOC') then
  begin
    raise Exception.Create('Invalid document file');
  end;
  If (Header.Version <> DOCUMENT_VERSION) then
  begin
    raise Exception.Create('Invalid document version');
  end;

  Root.LoadNode(Stream);
end;



{$IFDEF DOCXML_JCL}
//------------------------------------------------------------------------------
procedure TPHXDocument.LoadFromXml(const FileName: String);
var XMLDocument: TJclSimpleXML;

  procedure LoadAttributes(XMLNode: TJclSimpleXMLElem; Node: TPHXNode);
  var Index    : Integer;
  var Attribute: TPHXAttribute;
  var Prop     :  TJclSimpleXMLProp;
  begin
    for Index := 0 to XMLNode.Properties.Count - 1 do
    begin
      Prop:= XMLNode.Properties[Index];

      Attribute:= Node.Attributes.Add( AnsiString(Prop.Name));
      Attribute.FKind := vkVariant;
      Attribute.FValue:= Prop.Value;
    end;
  end;

  procedure LoadNode(XMLParent: TJclSimpleXMLElem; Parent: TPHXNode);
  var Index: Integer;
  var Node : TPHXNode;

  var XMLNode: TJclSimpleXMLElem;
  begin
    LoadAttributes(XMLParent, Parent);

    for Index := 0 to XMLParent.Items.Count-1 do
    begin
      XMLNode:= XMLParent.Items[Index];

      Node:= Parent.Children.Add( AnsiString(XMLNode.Name));

      LoadNode(XMLNode, Node);
    end;
  end;

begin
  XMLDocument:= TJclSimpleXML.Create();
  try
    XMLDocument.LoadFromFile(FileName);

    LoadNode(XMLDocument.Root, FRoot);
  finally
    XMLDocument.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXDocument.SaveToXml(const FileName: String);
var XMLDocument: TJclSimpleXML;

  procedure SaveNode(XMLParent: TJclSimpleXMLElem; Parent: TPHXNode);
  var Index: Integer;
  var Node : TPHXNode;

  var XMLNode: TJclSimpleXMLElem;
  begin
    SaveAttributes(XMLParent, Parent);

    for Index:= 0 to Parent.Children.Count-1 do
    begin
      Node:= Parent.Children[Index];

      XMLNode:= XMLParent.Items.Add( AnsiString(Node.Name));

      SaveNode(XMLNode, Node);
    end;
  end;

  procedure SaveAttributes(XMLNode: TJclSimpleXMLElem; Node: TPHXNode);
  var Index    : Integer;
  var Attribute: TPHXAttribute;
  var Value    : String;
  begin
    for Index := 0 to Node.Attributes.Count-1 do
    begin
      Attribute:= Node.Attributes[Index];

      Value:= '';
      case Attribute.Kind of
        vkByte    :  Value:=   IntToStr(Attribute.ReadByte);
        vkWord    :  Value:=   IntToStr(Attribute.ReadWord);
        vkCardinal:  Value:=   IntToStr(Attribute.ReadCardinal);
        vkInteger :  Value:=   IntToStr(Attribute.ReadInteger);
        vkSingle  :  Value:= FloatToStr(Attribute.ReadSingle);
        vkDouble  :  Value:= FloatToStr(Attribute.ReadDouble);
        vkBoolean :  Value:=  BoolToStr(Attribute.ReadBoolean);
        vkString  :  Value:=            Attribute.ReadString;
      end;

      XMLNode.Properties.Add(String(Attribute.Name), Value);
    end;
  end;

begin
  XMLDocument:= TJclSimpleXML.Create();
  try
    XMLDocument.Root.Name:= String(FRoot.FName);

    SaveNode(XMLDocument.Root, FRoot);

    XMLDocument.SaveToFile(FileName);
  finally
    XMLDocument.Free;
  end;
end;

{$ELSE}

{$IFDEF DOCXML_VCL}

//------------------------------------------------------------------------------
procedure TPHXDocument.LoadFromXml(const FileName: String);
var XMLDocument: IXMLDocument;

  procedure LoadAttributes(XMLNode: IXMLNode; Node: TPHXNode);
  var Index    : Integer;
  var Attribute: TPHXAttribute;
  var Prop     : IXMLNode;
  begin
    for Index := 0 to XMLNode.AttributeNodes.Count - 1 do
    begin
      Prop:= XMLNode.AttributeNodes[Index];

      Attribute:= Node.Attributes.Add( AnsiString(Prop.NodeName));
      Attribute.FKind := vkVariant;
      Attribute.FValue:= Prop.NodeValue;
    end;
  end;

  procedure LoadNode(XMLParent: IXMLNode; Parent: TPHXNode);
  var Index: Integer;
  var Node : TPHXNode;

  var XMLNode: IXMLNode;
  begin
    LoadAttributes(XMLParent, Parent);

    for Index := 0 to XMLParent.ChildNodes.Count-1 do
    begin
      XMLNode:= XMLParent.ChildNodes[Index];

      Node:= Parent.Children.Add( AnsiString(XMLNode.NodeName));

      LoadNode(XMLNode, Node);
    end;
  end;

begin
  XMLDocument:= LoadXMLDocument(FileName);

  LoadNode(XMLDocument.DocumentElement, FRoot);
end;

//------------------------------------------------------------------------------
procedure TPHXDocument.SaveToXml(const FileName: String);
var XMLDocument: IXMLDocument;

  procedure SaveAttributes(XMLNode: IXMLNode; Node: TPHXNode);
  var Index    : Integer;
  var Attribute: TPHXAttribute;
  var Value    : String;
  begin
    for Index := 0 to Node.Attributes.Count-1 do
    begin
      Attribute:= Node.Attributes[Index];

      Value:= '';
      case Attribute.Kind of
        vkByte    :  Value:=   IntToStr(Attribute.ReadByte);
        vkWord    :  Value:=   IntToStr(Attribute.ReadWord);
        vkCardinal:  Value:=   IntToStr(Attribute.ReadCardinal);
        vkInteger :  Value:=   IntToStr(Attribute.ReadInteger);
        vkSingle  :  Value:= FloatToStr(Attribute.ReadSingle);
        vkDouble  :  Value:= FloatToStr(Attribute.ReadDouble);
        vkBoolean :  Value:=  BoolToStr(Attribute.ReadBoolean);
        vkString  :  Value:=            Attribute.ReadString;
      end;

      XMLNode.Attributes[String(Attribute.Name)]:= Value;
    end;
  end;

  procedure SaveNode(XMLParent: IXMLNode; Parent: TPHXNode);
  var Index: Integer;
  var Node : TPHXNode;

  var XMLNode: IXMLNode;
  begin
    SaveAttributes(XMLParent, Parent);

    for Index:= 0 to Parent.Children.Count-1 do
    begin
      Node:= Parent.Children[Index];

      XMLNode:= XMLParent.AddChild( String(Node.Name));

      SaveNode(XMLNode, Node);
    end;
  end;

begin
  XMLDocument:= NewXMLDocument;
  XMLDocument.Options:= XMLDocument.Options + [doNodeAutoIndent];
  XMLDocument.AddChild(String(FRoot.Name));

  SaveNode(XMLDocument.DocumentElement, FRoot);

  XMLDocument.SaveToFile(FileName);
end;


{$ELSE}

//------------------------------------------------------------------------------
procedure TPHXDocument.SaveToXml(const FileName: String);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXDocument.LoadFromXml(const FileName: String);
begin
end;

{$ENDIF}
{$ENDIF}


//------------------------------------------------------------------------------
function TPHXDocument.GetName: String;
begin
  Result:= FName;
end;

//------------------------------------------------------------------------------
function TPHXDocument.GetRoot: TPHXNode;
begin
  Result:= FRoot;
end;

{$ENDREGION}


end.

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
unit phxSimpleXML;
//< Classes for loading and saving XML files

interface

{$I phxConfig.inc}

uses
  Classes, SysUtils,

  phxClasses;

type

TXMLAttribute     = class;
TXMLAttributeList = class;
TXMLNode          = class;
TXMLNodeList      = class;

// Exception class for the xml classes
EXMLError = class(Exception);

// A attribute for a xml node
//------------------------------------------------------------------------------
TXMLAttribute = class
  private
    FName : WideString;
    FValue: Variant;
  public
    constructor Create;
    destructor Destroy; override;

    // Name of the attribute
    property Name: WideString read FName write FName;
    // Value of the attribute
    property Value: Variant read FValue write FValue;
  end;

TAttributeList = array[0..$00FFFFFF] of TXMLAttribute;
PAttributeList = ^TAttributeList;

// List of attributes for a xml node
//------------------------------------------------------------------------------
TXMLAttributeList = class
  private
    FList: TList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TXMLAttribute;
    function GetList: PAttributeList;
  public
    constructor Create(ANode: TXMLNode);
    destructor Destroy; override;

    // Remove and free all attributes
    procedure Clear;

    // Add a attribute
    function Add(const Name: String): TXMLAttribute; overload;
    // Add a attribute
    function Add(const Name: String; const Value: Variant): TXMLAttribute; overload;

    // Find a attribute
    function Find(const Name: String): TXMLAttribute; overload;
    // Find a attribute
    function Find(const Name: String; out Attribute: TXMLAttribute): Boolean; overload;

    // Checks if a attribute exists
    // @param(Name The name of the attribute to find)
    // @returns(True if the attribute exists, false othervise)
    function Contains(const Name: String): Boolean;

    // Number of attributes in the list
    property Count: Integer read GetCount;
    // Return a pointer to the internal list
    property List: PAttributeList read GetList;
    // Return a attribute from the list
    property Items[const Index: Integer]: TXMLAttribute read GetItem; default;
  end;

//------------------------------------------------------------------------------
TXMLNode = class
  private
    FParent    : TXMLNode;
    FName      : String;
    FAttributes: TXMLAttributeList;
    FNodes     : TXMLNodeList;
    FValue     : Variant;

    function GetAttribute(const Name: String): Variant;
    function GetChildValue(const Name: String): Variant;
    procedure SetChildValue(const Name: String; const Value: Variant);
    procedure SetAttribute(const Name: String; const Value: Variant);
  public
    constructor Create;
    destructor Destroy; override;

    // Find a child
    function FindChild(const Name: String): TXMLNode;

    // Returns true if
    function HasAttribute(const Name: String): Boolean;

    // The parent node
    property Parent: TXMLNode read FParent write FParent;
    // Name of the xml node
    property Name: String read FName write FName;
    // List of child nodes
    property Nodes: TXMLNodeList read FNodes;
    // The node value
    property Value: Variant read FValue write FValue;

    // List of child nodes
    property ChildNodes: TXMLNodeList read FNodes;
    // Read or write the value for a child node
    property ChildValues[const Name: String]: Variant read GetChildValue write SetChildValue; default;

    // Read or write a attribute by name
    property Attributes[const Name: String]: Variant read GetAttribute write SetAttribute;
    // The list of attributes for this node
    property AttributeList: TXMLAttributeList read FAttributes;
  end;

TNodeList = array[0..$00FFFFFF] of TXMLNode;
PNodeList = ^TNodeList;

//------------------------------------------------------------------------------
TXMLNodeList = class
  private
    FNode: TXMLNode;
    FList: TList;

    function GetCount: Integer;
    function GetList: PNodeList;
    function GetItem(const Index: Integer): TXMLNode;
  public
    // Create a new list of nodes
    constructor Create(ANode: TXMLNode);
    // Free this list
    destructor Destroy; override;

    // Remove and free all nodes
    procedure Clear;
    // Add a node
    function Add(const Name: String): TXMLNode;

    // Find a node by name
    function Find(const Name: String): TXMLNode; overload;
    // Find a node by name
    function Find(const Name: String; out Node: TXMLNode): Boolean; overload;

    // The owning node
    property Node: TXMLNode read FNode;
    // Number of nodes in the list
    property Count: Integer read GetCount;
    // Returns a pointer to the internal list
    property List: PNodeList read GetList;
    // Return a item from the node
    property Items[const Index: Integer]: TXMLNode read GetItem; default;
  end;

// Interface for a xml document
IXMLDocument = interface

end;

// Contains an xml document
//------------------------------------------------------------------------------
TXMLDocument = class
  private
    FIndent: String;
    FHeader: TXMLNode;
    FRoot  : TXMLNode;

    // Parse a xml document
    procedure Parse(const Text: WideString);
    // Export a xml document to a string
    procedure Export(Lines: TStrings; Node: TXMLNode; const Indent: String);
  public
    // Create a empty xml document
    constructor Create;
    // Free this document
    destructor Destroy; override;

    // Load the XML document from a file
    procedure LoadFromFile(const FileName: String);
    // Load the XML document from a stream
    procedure LoadFromStream(Stream: TStream);

    // Save the XML document to a file
    procedure SaveToFile(const FileName: String);
    // Save the XML document to a stream
    procedure SaveToStream(Stream: TStream);

    // Declaration header
    property Header: TXMLNode read FHeader;
    // Document root
    property Root: TXMLNode read FRoot;
    // Set Ident:='' if you want a compact output
    property Indent: String read FIndent write FIndent;
  end;


implementation

//------------------------------------------------------------------------------
function XMLEncode(Value: WideString): WideString;
begin
  Result:= Value;
  Result:= StringReplace(Result, '&'    , '&amp;' , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '<'    , '&lt;'  , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '>'    , '&gt;'  , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, chr(39), '&apos;', [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '"'    , '&quot;', [rfReplaceAll, rfIgnoreCase]);
end;

//------------------------------------------------------------------------------
function XMLDecode(Value: WideString): WideString;
begin
  Result:= Value;
  Result:= StringReplace(Result, '&lt;'  , '<'    , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '&gt;'  , '>'    , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '&apos;', chr(39), [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '&quot;', '"'    , [rfReplaceAll, rfIgnoreCase]);
  Result:= StringReplace(Result, '&amp;' , '&'    , [rfReplaceAll, rfIgnoreCase]);
end;


{$REGION 'TXMLAttribute'}

// TXMLAttribute
//==============================================================================
constructor TXMLAttribute.Create;
begin

end;

//------------------------------------------------------------------------------
destructor TXMLAttribute.Destroy;
begin

  inherited;
end;

{$ENDREGION}

{$REGION 'TXMLAttributeList'}

// TXMLAttributeList
//==============================================================================
constructor TXMLAttributeList.Create(ANode: TXMLNode);
begin
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TXMLAttributeList.Destroy;
begin
  Clear;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TXMLAttributeList.Clear;
var Index   : Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    TXMLAttribute(FList.List[Index]).Free;
  end;
  FList.Clear;
end;


//------------------------------------------------------------------------------
function TXMLAttributeList.Add(const Name: String): TXMLAttribute;
begin
  Result:= TXMLAttribute.Create;
  Result.Name:= Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.Add(const Name: String; const Value: Variant): TXMLAttribute;
begin
  Result:= TXMLAttribute.Create;
  Result.Name := Name;
  Result.Value:= Value;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.Find(const Name: String): TXMLAttribute;
var Index   : Integer;
var Attribute: TXMLAttribute;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Attribute:= TXMLAttribute(FList.List[Index]);

    if SameText(Attribute.Name, Name) then
    begin
      Result:= Attribute;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.Find(const Name: String; out Attribute: TXMLAttribute): Boolean;
var Index   : Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Attribute:= TXMLAttribute(FList.List[Index]);

    if SameText(Attribute.Name, Name) then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Attribute:= nil;
  Result   := False;
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.Contains(const Name: String): Boolean;
begin
  Result:= Find(Name) <> nil;
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.GetList: PAttributeList;
begin
  Result:= PAttributeList(FList.List);
end;

//------------------------------------------------------------------------------
function TXMLAttributeList.GetItem(const Index: Integer): TXMLAttribute;
begin
  Result:= TXMLAttribute(FList.List[Index]);
end;

{$ENDREGION}

{$REGION 'TXMLNode'}

// TXMLNode
//==============================================================================
constructor TXMLNode.Create;
begin
  FAttributes:= TXMLAttributeList.Create(Self);
  FNodes     := TXMLNodeList.Create(Self);
end;

//------------------------------------------------------------------------------
destructor TXMLNode.Destroy;
begin
  FAttributes.Free;
  FNodes.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TXMLNode.FindChild(const Name: String): TXMLNode;
begin
  Result:= FNodes.Find(Name);
end;

//------------------------------------------------------------------------------
function TXMLNode.HasAttribute(const Name: String): Boolean;
begin
  Result:= AttributeList.Contains(Name);
end;

//------------------------------------------------------------------------------
function TXMLNode.GetAttribute(const Name: String): Variant;
var Attribute: TXMLAttribute;
begin
  if AttributeList.Find(Name, Attribute)  then
  begin
    Result:= Attribute.Value;
  end else
  begin
    raise EXMLError.CreateFmt('The attribute "%s" was not found in the node "%s"', [Name, FName]);
  end;
end;

//------------------------------------------------------------------------------
procedure TXMLNode.SetAttribute(const Name: String; const Value: Variant);
var Attribute: TXMLAttribute;
begin
  if AttributeList.Find(Name, Attribute)  then
  begin
    Attribute.Value:= Value;
  end else
  begin
    AttributeList.Add(Name, Value);
  end;
end;

//------------------------------------------------------------------------------
function TXMLNode.GetChildValue(const Name: String): Variant;
var Node: TXMLNode;
begin
  if ChildNodes.Find(Name, Node)  then
  begin
    Result:= Node.Value;
  end else
  begin
    raise EXMLError.CreateFmt('The node %s was not found in the node %s', [Name, FName]);
  end;
end;

//------------------------------------------------------------------------------
procedure TXMLNode.SetChildValue(const Name: String; const Value: Variant);
var Node: TXMLNode;
begin
  if ChildNodes.Find(Name, Node)  then
  begin
    Node.Value:= Value;
  end else
  begin
    ChildNodes.Add(Name).Value:= Value;
  end;
end;

{$ENDREGION}

{$REGION 'TXMLNodeList'}

// TXMLNodeList
//==============================================================================
constructor TXMLNodeList.Create(ANode: TXMLNode);
begin
  FNode:= ANode;
  FList:= TList.Create;
end;

//------------------------------------------------------------------------------
destructor TXMLNodeList.Destroy;
begin
  Clear;


  FList.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TXMLNodeList.Clear;
var Index   : Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    TXMLNode(FList.List[Index]).Free;
  end;
  FList.Clear;
end;

//------------------------------------------------------------------------------
function TXMLNodeList.Add(const Name: String): TXMLNode;
begin
  Result:= TXMLNode.Create;
  Result.Parent:= FNode;
  Result.Name  := Name;

  FList.Add(Result);
end;

//------------------------------------------------------------------------------
function TXMLNodeList.Find(const Name: String): TXMLNode;
var Index: Integer;
var Node : TXMLNode;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Node:= TXMLNode(FList.List[Index]);

    if SameText(Node.Name, Name) then
    begin
      Result:= Node;
      Exit;
    end;
  end;
  Result:= nil;
end;

//------------------------------------------------------------------------------
function TXMLNodeList.Find(const Name: String; out Node: TXMLNode): Boolean;
var Index: Integer;
begin
  for Index := 0 to FList.Count-1 do
  begin
    Node:= TXMLNode(FList.List[Index]);

    if SameText(Node.Name, Name) then
    begin
      Result:= True;
      Exit;
    end;
  end;
  Node  := nil;
  Result:= False;
end;

//------------------------------------------------------------------------------
function TXMLNodeList.GetCount: Integer;
begin
  Result:= FList.Count;
end;

//------------------------------------------------------------------------------
function TXMLNodeList.GetList: PNodeList;
begin
  Result:= PNodeList(FList.List);
end;

//------------------------------------------------------------------------------
function TXMLNodeList.GetItem(const Index: Integer): TXMLNode;
begin
  Result:= TXMLNode(FList.List[Index]);
end;


{$ENDREGION}

{$REGION 'TXMLDocument'}

// TXMLDocument
//==============================================================================
constructor TXMLDocument.Create;
begin
  FHeader := TXmlNode.Create;
  // Default XML Header
  FHeader.Name:= '?xml';
  // Default XML Version
  FHeader.Attributes['version'] := '1.0';

  FRoot:= TXmlNode.Create;

  FIndent:= '  ';
end;

//------------------------------------------------------------------------------
destructor TXMLDocument.Destroy;
begin
  FHeader.Free;
  FRoot.Free;
  inherited;
end;



//------------------------------------------------------------------------------
procedure TXMLDocument.LoadFromFile(const FileName: String);
var Stream: TStream;
begin
  Stream:= TPHXContentManager.CreateStream(FileName, ckText, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TXMLDocument.LoadFromStream(Stream: TStream);
var Lines: TStrings;
begin
  Lines:= TStringList.Create;
  try
    Lines.LoadFromStream(Stream);

    Parse(Lines.Text);
  finally
    Lines.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TXMLDocument.SaveToFile(const FileName: String);
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
procedure TXMLDocument.SaveToStream(Stream: TStream);
var Lines: TStrings;
begin
  Lines:= TStringList.Create;
  try
    Export(Lines, FHeader, '');
    Lines.Add('');
    Export(Lines, FRoot, '');

    Lines.SaveToStream(Stream);
  finally
    Lines.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TXMLDocument.Export(Lines: TStrings; Node: TXMLNode; const Indent: String);
var Index     : Integer;
var Attributes: String;
begin
  Attributes:= '';
  for Index := 0 to Node.AttributeList.Count-1 do
  begin
    Attributes:= Attributes + ' ' + Node.AttributeList.List[Index].Name + '="' + XMLEncode(Node.AttributeList.List[Index].Value) + '"';
  end;

  // Header node
  if Node = Header then
  begin
    Lines.Add(Indent + '<' + Node.Name + Attributes + '?>');
  end else
  // Node has no value and no childrem
  if (Node.Nodes.Count = 0) and (Node.Value='') then
  begin
    Lines.Add(Indent + '<' + Node.Name + Attributes + '/>');
  end else
  // Node has no child nodes but a value
  if (Node.Nodes.Count = 0)  then
  begin
    Lines.Add(Indent + '<' + Node.Name + Attributes + '>' + XMLEncode(Node.Value) + '</' + Node.Name + '>');;
  end else
  // Node has child nodes
  begin
    Lines.Add(Indent + '<' + Node.Name + Attributes + '>' + XMLEncode(Node.Value));

    // Export the child nodes
    for Index := 0 to Node.Nodes.Count-1 do
    begin
      Export(Lines, Node.Nodes.List^[Index], Indent + FIndent);
    end;

    Lines.Add(Indent + '</' + Node.Name + '>');
  end;
end;

// Parse parameters
// attribute = ""
// attribute = 100
//------------------------------------------------------------------------------
procedure ParseAttributes(Node: TXMLNode; var Curr: PWideChar);
var Start: PWideChar;
var Name : WideString;
var Value: WideString;
var Quote: WideChar;
begin
  Name :='';
  Value:= '';

  while (Curr^ <> #0) do
  begin
    // Skip whitespace
    while (Curr^ <> #0) and (Curr^  <= #32) do
    begin
      Inc(Curr);
    end;

    // End of attributes
    if (Curr^ = '>') or (Curr^ = '/') or (Curr^ = '?') then
    begin
      Exit;
    end;

    Start:= Curr;
    while (Curr^ <> #0) and (Curr^ > #32) and (Curr^ <> '=') do
    begin
      Inc(Curr);
    end;
    SetString(Name, Start, Curr - Start);

    // Skip whitespace
    while (Curr^ <> #0) and (Curr^ <= #32) do
    begin
      Inc(Curr);
    end;

    Assert(Curr^ = '=');

    Inc(Curr);

    // Skip whitespace
    while (Curr^ <> #0) and (Curr^ <= #32) do
    begin
      Inc(Curr);
    end;

    // Escaped value with double quotes
    if (Curr^ = '"') or (Curr^ = '''') then
    begin
      Quote:= Curr^;

      Inc(Curr);

      Start:= Curr;
      while (Curr^ <> #0) and (Curr^ <> Quote) do
      begin
        Inc(Curr);
      end;
      SetString(Value, Start, Curr - Start);
       // Skip trailing quote
      Inc(Curr);
    end else
    // Unquoted value
    begin
      Start:= Curr;
      while (Curr^ > #32) do
      begin
        Inc(Curr);
      end;
      SetString(Value, Start, Curr - Start);
    end;

    Node.AttributeList.Add(Name, XMLDecode(Value));
  end;
end;

//------------------------------------------------------------------------------
procedure TXMLDocument.Parse(const Text: WideString);
var Node  : TXMLNode;
var Parent: TXMLNode;
var Curr  : PWideChar;
var Start : PWideChar;
var Name  : WideString;
var Value : WideString;
begin
  FRoot.AttributeList.Clear;
  FRoot.Nodes.Clear;

  FHeader.AttributeList.Clear;

  Node  := nil;
  Parent:= nil;
  Curr  := @Text[1];

  // Find opening tag
  while (Curr^ <> #0) and (Curr^ <> '<') do
  begin
    Inc(Curr);
  end;

  while (Curr^ <> #0) do
  begin
    Assert(Curr^ = '<');

    // Comment
    if Copy(Curr, 1, 4) = '<!--' then
    begin
      Inc(Curr, 4);

      // Find end comment tag
      while (Curr^ <> #0) and (Copy(Curr, 1, 3) <> '-->') do
      begin
        Inc(Curr);
      end;
      Inc(Curr, 3);

      // Find next opening tag
      while (Curr^ <> #0) and (Curr^ <> '<') do
      begin
        Inc(Curr);
      end;
      Continue;
    end;

    // Skip <
    Inc(Curr);

    // End tag
    if (Curr^ = '/') then
    begin
      Inc(Curr);

      // Check so the end tag is the same as the start tag
      Assert( SameText(Copy(Curr, 1, Length(Node.Name)), Node.Name));

      // Skip node name and >
      Inc(Curr, Length(Node.Name) + 1) ;


      Parent:= Node.Parent;
      Node  := Parent;

      // Find opening tag
      while (Curr^ <> #0) and (Curr^ <> '<') do
      begin
        Inc(Curr);
      end;
      Continue;
    end;

    Start:= Curr;
    // Find end of node name
    while (Curr^ <> #0) and (Curr^ > #32) and (Curr^ <> '/') and (Curr^ <> '>') do
    begin
      Inc(Curr);
    end;

    if Curr <> Start then
    begin
      SetString(Name, Start, Curr - Start);

      if SameText(Name, '?xml') then
      begin
        Node:= FHeader;
      end else
      if Parent = nil then
      begin
        Node:= Root;
        Node.Name:= Name;
      end else
      begin
        Node:= Parent.Nodes.Add(Name);
      end;

      ParseAttributes(Node, Curr);

      // End of this node
      if (Curr^ = '>') then
      begin
        Inc(Curr);

        Parent:= Node;
     end;

      // Node has no child nodes
      if (Curr^ = '/') or (Curr^ = '?') then
      begin
        Inc(Curr);
        Inc(Curr);
        Parent:= Node.Parent;
        Node  := Parent;
      end;
    end;

    Start:= Curr;
    // Find next tag
    while (Curr^ <> #0) and (Curr^ <> '<') do
    begin
      Inc(Curr);
    end;
    // Save the text to the node value
    if Assigned(Node) then
    begin
      SetString(Value, Start, Curr - Start);

      Node.Value:= XMLDecode(Trim(Value));
    end;

  end;
end;

{$ENDREGION}



end.

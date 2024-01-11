unit phxFontEx;

interface

uses Graphics, Dialogs, SysUtils, Types,

  {xmldom} Laz2_Dom, {XMLIntf}Laz2_XMLRead, Laz2_XMLUtils, Laz2_XMLWrite,

  //phxGraphics,
  phxGraphics,
  phxFont;

  //Phoenix.GraphicsEx,


type

TPHXFontEx = class helper for TPHXFont
  private
  public
    procedure LoadFromXml(const FileName: String);
    procedure SaveToXml(const FileName: String);


    procedure DrawCharacter(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; const Character: TPHXCharacter); overload;

    procedure DrawText(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; const Text: String);
  end;

implementation


//------------------------------------------------------------------------------
procedure TPHXFontEx.LoadFromXml(const FileName: String);
var Index      : Integer;
var XMLDocument  : TXMLDocument;
var XMLNode      : TDOMNode;//IXMLNode;
var XMLFont      : TDOMNode;//IXMLNode;
var XMLMetric    : TDOMNode;//IXMLNode;
var XMLCharacters: TDOMNode;//IXMLNode;
var XMLKernings  : TDOMNode;//IXMLNode;

var TextureName: String;
begin

  ReadXMLFile(XMLDocument,FileName);
  //XMLDocument:= LoadXMLDocument(FileName);
  XMLFont:= XMLDocument.FindNode('phxFont');
 // XMLFont:= XMLDocument.ChildNodes.FindNode('phxFont');
  if not Assigned(XMLFont) then
  begin
    MessageDlg('The selected file is not a valid font export.', mtError, [mbOK], 0);
    Exit;
  end;

  Name := XMLFont.Attributes.GetNamedItem('Name').NodeValue;
  //Name       := XMLFont.Attributes.GetNamedItem().['Name'   ];
  Size := StrToInt(XMLFont.Attributes.GetNamedItem('Size').NodeValue);
  // Size       := XMLFont.Attributes['Size'   ];
  Style      := TPHXFontStyles( Byte(StrToInt(XMLFont.Attributes.GetNamedItem('Name').NodeValue)));
  //Style      := TPHXFontStyles( Byte(XMLFont.Attributes['Style'   ]));
  Author := XMLFont.Attributes.GetNamedItem('Author').NodeValue;
  //Author     := XMLFont.Attributes['Author'];
  TextureName := XMLFont.Attributes.GetNamedItem('Texture').NodeValue;
  //TextureName:= XMLFont.Attributes['Texture'];

  if XMLFont.Attributes.GetNamedItem('version').HasAttributes
  then Version:= XMLFont.Attributes.GetNamedItem('Texture').NodeValue;

  //if XMLFont.HasAttribute('version')
  //then Version:= XMLFont.Attributes['version' ];

  if XMLFont.Attributes.GetNamedItem('comment').HasAttributes
  then Version:= XMLFont.Attributes.GetNamedItem('comment').NodeValue;
  //if XMLFont.HasAttribute('comment')
  //then Comment:= XMLFont.Attributes['comment' ];

  XMLMetric:= XMLFont.FindNode('Metric');
 // XMLMetric:= XMLFont.ChildNodes.FindNode('Metric');
  if Assigned(XMLMetric) then
  begin
    if XMLMetric.Attributes.GetNamedItem('Height').HasAttributes then
    Metric.Height := StrToInt(XMLMetric.Attributes.GetNamedItem('Height').NodeValue);
    //if XMLMetric.HasAttribute('Height' ) then Metric.Height := XMLMetric.Attributes['Height' ];
    if XMLMetric.Attributes.GetNamedItem('Offset').HasAttributes then
    Metric.Height := StrToInt(XMLMetric.Attributes.GetNamedItem('Offset').NodeValue);
    //if XMLMetric.HasAttribute('Offset' ) then Metric.Offset := XMLMetric.Attributes['Offset' ];
    if XMLMetric.Attributes.GetNamedItem('Ascent').HasAttributes then
    Metric.Height := StrToInt(XMLMetric.Attributes.GetNamedItem('Ascent').NodeValue);
    //if XMLMetric.HasAttribute('Ascent' ) then Metric.Ascent := XMLMetric.Attributes['Ascent' ];
    if XMLMetric.Attributes.GetNamedItem('Descent').HasAttributes then
    Metric.Height := StrToInt(XMLMetric.Attributes.GetNamedItem('Descent').NodeValue);
    //if XMLMetric.HasAttribute('Descent') then Metric.Descent:= XMLMetric.Attributes['Descent'];
  end;


  XMLCharacters:= XMLFont.FindNode('Characters');
 // XMLCharacters:= XMLFont.ChildNodes.FindNode('Characters');
  if Assigned(XMLCharacters) then
  begin
    Characters.Count:= StrToInt(XMLCharacters.Attributes.GetNamedItem('Count').NodeValue);
    //Characters.Count:= XMLCharacters.Attributes['Count'];

    for Index := 0 to Characters.Count - 1 do
    begin
      XMLNode:= XMLCharacters.ChildNodes[Index];

      if not SameText(XMLNode.NodeName, 'Character') then Continue;

      Characters.List^[Index].ID     := StrToInt(XMLNode.Attributes.GetNamedItem('ID').NodeValue);
      //Characters.List^[Index].ID     := XMLNode.Attributes['ID'       ];
      Characters.List^[Index].X      := StrToInt(XMLNode.Attributes.GetNamedItem('X').NodeValue);
      //Characters.List^[Index].X      := XMLNode.Attributes['X'        ];
      Characters.List^[Index].Y      := StrToInt(XMLNode.Attributes.GetNamedItem('Y').NodeValue);
      //Characters.List^[Index].Y      := XMLNode.Attributes['Y'        ];
      Characters.List^[Index].Width  := StrToInt(XMLNode.Attributes.GetNamedItem('Width').NodeValue);
      //Characters.List^[Index].Width  := XMLNode.Attributes['Width'    ];
      Characters.List^[Index].Height := StrToInt(XMLNode.Attributes.GetNamedItem('Height').NodeValue);
      //Characters.List^[Index].Height := XMLNode.Attributes['Height'   ];
      Characters.List^[Index].Offset.X:= StrToInt(XMLNode.Attributes.GetNamedItem('OffsetX').NodeValue);
      //Characters.List^[Index].Offset.X:= XMLNode.Attributes['OffsetX'  ];
      Characters.List^[Index].Offset.Y:= StrToInt(XMLNode.Attributes.GetNamedItem('OffsetY').NodeValue);
      //Characters.List^[Index].Offset.Y:= XMLNode.Attributes['OffsetY'  ];
      Characters.List^[Index].Advance:= StrToInt(XMLNode.Attributes.GetNamedItem('Advance').NodeValue);
      //Characters.List^[Index].Advance:= XMLNode.Attributes['Advance' ];
    end;
  end;

  XMLKernings:= XMLFont.FindNode('Kernings');
  //XMLKernings:= XMLFont.ChildNodes.FindNode('Kernings');
  if Assigned(XMLKernings) then
  begin
    Kernings.Count:= StrToInt(XMLKernings.Attributes.GetNamedItem('Count').NodeValue);
    //Kernings.Count:= XMLKernings.Attributes['Count'];

    for Index := 0 to Kernings.Count - 1 do
    begin
      XMLNode:= XMLCharacters.ChildNodes[Index];

      if not SameText(XMLNode.NodeName, 'Kerning') then Continue;
      Kernings.List^[Index].First:= StrToInt(XMLNode.Attributes.GetNamedItem('First').NodeValue);
      //Kernings.List^[Index].First := XMLNode.Attributes['First' ];
      Kernings.List^[Index].Second:= StrToInt(XMLNode.Attributes.GetNamedItem('Second').NodeValue);
      //Kernings.List^[Index].Second:= XMLNode.Attributes['Second'];
      Kernings.List^[Index].Amount:= StrToInt(XMLNode.Attributes.GetNamedItem('Amount').NodeValue);
      //Kernings.List^[Index].Amount:= XMLNode.Attributes['Amount'];
    end;
  end;

  if FileExists(TextureName) then
  begin
    Texture.LoadTexture( TextureName  );
  end;

  Initialize();
end;

//------------------------------------------------------------------------------
procedure TPHXFontEx.SaveToXml(const FileName: String);
var Index      : Integer;
var XMLDocument  : TXMLDocument;//IXMLDocument;
var XMLNode      : TDOMNode;//IXMLNode;
var XMLFont      : TDOMNode;//IXMLNode;
var XMLMetric    : TDOMNode;//IXMLNode;
var XMLCharacters: TDOMNode;//IXMLNode;
var XMLKernings  : TDOMNode;//IXMLNode;

var TextureName: String;
var GraphicFormat: TPHXGraphicFormat;
begin

  // XMLDocument:= NewXMLDocument;
  // XMLDocument.Options:= XMLDocument.Options + [doNodeAutoIndent ];
  XMLDocument := TXMLDocument.create;
  XMLFont := XMLDocument.CreateElement('phxFont');


//  XMLFont:= XMLDocument.AddChild('phxFont');
  TDOMElement(XMLFont).SetAttribute('Version', IntToStr(PHXFONT_VERSION));
  //XMLFont.Attributes['Version']:= PHXFONT_VERSION;
  TDOMElement(XMLFont).SetAttribute('Name', Name);
  //XMLFont.Attributes['Name'   ]:= Name;
  TDOMElement(XMLFont).SetAttribute('Size', IntToStr(Size));
  //XMLFont.Attributes['Size'   ]:= Size;
  TDOMElement(XMLFont).SetAttribute('Style', IntToStr(Byte(Style)));
  //XMLFont.Attributes['Style'  ]:= Byte(Style);
  TDOMElement(XMLFont).SetAttribute('Author', Author);
  //XMLFont.Attributes['Author' ]:= Author;
  TDOMElement(XMLFont).SetAttribute('Comment', Comment);
  //XMLFont.Attributes['Comment' ]:= Comment;
  TDOMElement(XMLFont).SetAttribute('Version', Version);
  //XMLFont.Attributes['Version' ]:= Version;
  TDOMElement(XMLFont).SetAttribute('Texture', TextureName);
  //XMLFont.Attributes['Texture']:= TextureName;

  XMLMetric := XMLFont.OwnerDocument.CreateElement('Metric');
  //XMLMetric:= XMLFont.AddChild('Metric');

  TDOMElement(XMLMetric).SetAttribute('Height', IntToStr(Metric.Height));
  //XMLMetric.Attributes['Height' ]:= Metric.Height;
  TDOMElement(XMLMetric).SetAttribute('Offset', IntToStr(Metric.Offset));
  //XMLMetric.Attributes['Offset' ]:= Metric.Offset;
  TDOMElement(XMLMetric).SetAttribute('Ascent', IntToStr(Metric.Ascent));
  //XMLMetric.Attributes['Ascent' ]:= Metric.Ascent;
  TDOMElement(XMLMetric).SetAttribute('Descent', IntToStr(Metric.Descent));
  //XMLMetric.Attributes['Descent']:= Metric.Descent;

  //XMLCharacters:= XMLFont.AddChild('Characters');
  XMLCharacters := XMLFont.OwnerDocument.CreateElement('Characters');

  for Index := 0 to Characters.Count - 1 do
  begin
    //XMLNode:= XMLCharacters.AddChild('Character');
    XMLNode := XMLDocument.CreateElement('Character');
   // XMLNode:= XMLCharacters.AddChild('Character');
    TDOMElement(XMLNode).SetAttribute('ID', IntToStr(Characters.List^[Index].ID));
    //XMLNode.Attributes['ID'      ]:= Characters.List^[Index].ID;
    TDOMElement(XMLNode).SetAttribute('X', IntToStr(Characters.List^[Index].X));
    //XMLNode.Attributes['X'       ]:= Characters.List^[Index].X;
    TDOMElement(XMLNode).SetAttribute('Y', IntToStr(Characters.List^[Index].Y));
    //XMLNode.Attributes['Y'       ]:= Characters.List^[Index].Y;
    TDOMElement(XMLNode).SetAttribute('Width', IntToStr(Characters.List^[Index].Width));
    //XMLNode.Attributes['Width'   ]:= Characters.List^[Index].Width;
    TDOMElement(XMLNode).SetAttribute('Height', IntToStr(Characters.List^[Index].Height));
    //XMLNode.Attributes['Height'  ]:= Characters.List^[Index].Height;
    TDOMElement(XMLNode).SetAttribute('OffsetX', IntToStr(Characters.List^[Index].Offset.X));
    //XMLNode.Attributes['OffsetX' ]:= Characters.List^[Index].Offset.X;
    TDOMElement(XMLNode).SetAttribute('OffsetY', IntToStr(Characters.List^[Index].Offset.Y));
    //XMLNode.Attributes['OffsetY' ]:= Characters.List^[Index].Offset.Y;
    TDOMElement(XMLNode).SetAttribute('OffsetY', IntToStr(Characters.List^[Index].Advance));
    //XMLNode.Attributes['Advance' ]:= Characters.List^[Index].Advance;
  end;
  TDOMElement(XMLCharacters).SetAttribute('Count', IntToStr(Characters.Count));
  //XMLCharacters.Attributes['Count']:= Characters.Count;

  XMLKernings := XMLFont.OwnerDocument.CreateElement('Kernings');
  //XMLKernings:= XMLFont.AddChild('Kernings');
  for Index := 0 to Kernings.Count - 1 do
  begin
    XMLNode := XMLDocument.CreateElement('Kerning');
    //XMLNode:= XMLCharacters.AddChild('Kerning');
    TDOMElement(XMLNode).SetAttribute('First', IntToStr(Kernings.List^[Index].First));
    //XMLNode.Attributes['First' ]:= Kernings.List^[Index].First;
    TDOMElement(XMLNode).SetAttribute('Second', IntToStr(Kernings.List^[Index].Second));
    //XMLNode.Attributes['Second']:= Kernings.List^[Index].Second;
    TDOMElement(XMLNode).SetAttribute('Amount', IntToStr(Kernings.List^[Index].Amount));
    //XMLNode.Attributes['Amount']:= Kernings.List^[Index].Amount;
  end;
  TDOMElement(XMLKernings).SetAttribute('Count', IntToStr(Kernings.Count));
  //XMLKernings.Attributes['Count']:= Kernings.Count;

  TextureName:= ChangeFileExt(FileName, '.png');
  if GraphicFormats.Find(TextureName, GraphicFormat) then
  begin
    GraphicFormat.Filer.SaveToFile(TextureName, Texture.Graphic);
  end;

 // FormatXMLData()
 WriteXMLFile(XMLDocument,Filename);
 // XMLDocument.SaveToFile(FileName);
end;


//------------------------------------------------------------------------------
procedure TPHXFontEx.DrawCharacter(Dest: TCanvas; Buffer: TBitmap; X,Y: Integer; const Character: TPHXCharacter);
var DstRect: TRect;
var SrcRect: TRect;
begin
  DstRect.Left  := X;
  DstRect.Top   := Y;
  DstRect.Right := X + Character.Width;
  DstRect.Bottom:= Y + Character.Height;

  SrcRect.Left  := Character.X;
  SrcRect.Top   := Character.Y;
  SrcRect.Right := Character.X + Character.Width;
  SrcRect.Bottom:= Character.Y + Character.Height;

  Dest.CopyRect(DstRect, Buffer.Canvas, SrcRect);
end;



//------------------------------------------------------------------------------
procedure TPHXFontEx.DrawText(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; const Text: String);
var Index : Integer;
var Char  : Integer;
var Raster: TPoint;
begin
  Raster.X:= X;
  Raster.Y:= Y;
  For Index:=1 to Length(Text) do
  begin
    Char:= CharacterMap[ Ord( Text[Index] ) ];

    if Text[Index] = #13 then
    begin
      Raster.X:= X;
      Raster.Y:= Y + Metric.Height;
    end else
    if Char >= 0 then
    begin
      DrawCharacter(Dest, Buffer, Raster.X, Raster.Y, Characters.List^[Char]);

      Raster.X:= Raster.X + Characters.List^[Char].Advance;
    end;
  end;
end;



end.

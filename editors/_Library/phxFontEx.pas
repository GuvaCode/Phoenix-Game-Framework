unit phxFontEx;

interface

uses Graphics, Dialogs, SysUtils, Types,

  xmldom, XMLIntf, XMLDoc,

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
var XMLDocument  : IXMLDocument;
var XMLNode      : IXMLNode;
var XMLFont      : IXMLNode;
var XMLMetric    : IXMLNode;
var XMLCharacters: IXMLNode;
var XMLKernings  : IXMLNode;

var TextureName: String;
begin
  XMLDocument:= LoadXMLDocument(FileName);


  XMLFont:= XMLDocument.ChildNodes.FindNode('phxFont');
  if not Assigned(XMLFont) then
  begin
    MessageDlg('The selected file is not a valid font export.', mtError, [mbOK], 0);
    Exit;
  end;

  Name       := XMLFont.Attributes['Name'   ];
  Size       := XMLFont.Attributes['Size'   ];
  Style      := TPHXFontStyles( Byte(XMLFont.Attributes['Style'   ]));
  Author     := XMLFont.Attributes['Author' ];
  TextureName:= XMLFont.Attributes['Texture'];

  if XMLFont.HasAttribute('version') then Version:= XMLFont.Attributes['version' ];
  if XMLFont.HasAttribute('comment') then Comment:= XMLFont.Attributes['comment' ];


  XMLMetric:= XMLFont.ChildNodes.FindNode('Metric');
  if Assigned(XMLMetric) then
  begin
    if XMLMetric.HasAttribute('Height' ) then Metric.Height := XMLMetric.Attributes['Height' ];
    if XMLMetric.HasAttribute('Offset' ) then Metric.Offset := XMLMetric.Attributes['Offset' ];
    if XMLMetric.HasAttribute('Ascent' ) then Metric.Ascent := XMLMetric.Attributes['Ascent' ];
    if XMLMetric.HasAttribute('Descent') then Metric.Descent:= XMLMetric.Attributes['Descent'];
  end;

  XMLCharacters:= XMLFont.ChildNodes.FindNode('Characters');
  if Assigned(XMLCharacters) then
  begin
    Characters.Count:= XMLCharacters.Attributes['Count'];

    for Index := 0 to Characters.Count - 1 do
    begin
      XMLNode:= XMLCharacters.ChildNodes[Index];

      if not SameText(XMLNode.NodeName, 'Character') then Continue;

      Characters.List^[Index].ID     := XMLNode.Attributes['ID'       ];
      Characters.List^[Index].X      := XMLNode.Attributes['X'        ];
      Characters.List^[Index].Y      := XMLNode.Attributes['Y'        ];
      Characters.List^[Index].Width  := XMLNode.Attributes['Width'    ];
      Characters.List^[Index].Height := XMLNode.Attributes['Height'   ];
      Characters.List^[Index].Offset.X:= XMLNode.Attributes['OffsetX'  ];
      Characters.List^[Index].Offset.Y:= XMLNode.Attributes['OffsetY'  ];
      Characters.List^[Index].Advance:= XMLNode.Attributes['Advance' ];
    end;
  end;

  XMLKernings:= XMLFont.ChildNodes.FindNode('Kernings');
  if Assigned(XMLKernings) then
  begin
    Kernings.Count:= XMLKernings.Attributes['Count'];

    for Index := 0 to Kernings.Count - 1 do
    begin
      XMLNode:= XMLCharacters.ChildNodes[Index];

      if not SameText(XMLNode.NodeName, 'Kerning') then Continue;

      Kernings.List^[Index].First := XMLNode.Attributes['First' ];
      Kernings.List^[Index].Second:= XMLNode.Attributes['Second'];
      Kernings.List^[Index].Amount:= XMLNode.Attributes['Amount'];
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
var XMLDocument  : IXMLDocument;
var XMLNode      : IXMLNode;
var XMLFont      : IXMLNode;
var XMLMetric    : IXMLNode;
var XMLCharacters: IXMLNode;
var XMLKernings  : IXMLNode;

var TextureName: String;
var GraphicFormat: TPHXGraphicFormat;
begin

  XMLDocument:= NewXMLDocument;
  XMLDocument.Options:= XMLDocument.Options + [doNodeAutoIndent ];

  XMLFont:= XMLDocument.AddChild('phxFont');
  XMLFont.Attributes['Version']:= PHXFONT_VERSION;
  XMLFont.Attributes['Name'   ]:= Name;
  XMLFont.Attributes['Size'   ]:= Size;
  XMLFont.Attributes['Style'  ]:= Byte(Style);
  XMLFont.Attributes['Author' ]:= Author;
  XMLFont.Attributes['Comment' ]:= Comment;
  XMLFont.Attributes['Version' ]:= Version;
  XMLFont.Attributes['Texture']:= TextureName;

  XMLMetric:= XMLFont.AddChild('Metric');
  XMLMetric.Attributes['Height' ]:= Metric.Height;
  XMLMetric.Attributes['Offset' ]:= Metric.Offset;
  XMLMetric.Attributes['Ascent' ]:= Metric.Ascent;
  XMLMetric.Attributes['Descent']:= Metric.Descent;

  XMLCharacters:= XMLFont.AddChild('Characters');
  for Index := 0 to Characters.Count - 1 do
  begin
    XMLNode:= XMLCharacters.AddChild('Character');

    XMLNode.Attributes['ID'      ]:= Characters.List^[Index].ID;
    XMLNode.Attributes['X'       ]:= Characters.List^[Index].X;
    XMLNode.Attributes['Y'       ]:= Characters.List^[Index].Y;
    XMLNode.Attributes['Width'   ]:= Characters.List^[Index].Width;
    XMLNode.Attributes['Height'  ]:= Characters.List^[Index].Height;
    XMLNode.Attributes['OffsetX' ]:= Characters.List^[Index].Offset.X;
    XMLNode.Attributes['OffsetY' ]:= Characters.List^[Index].Offset.Y;
    XMLNode.Attributes['Advance' ]:= Characters.List^[Index].Advance;
  end;
  XMLCharacters.Attributes['Count']:= Characters.Count;

  XMLKernings:= XMLFont.AddChild('Kernings');
  for Index := 0 to Kernings.Count - 1 do
  begin
    XMLNode:= XMLCharacters.AddChild('Kerning');

    XMLNode.Attributes['First' ]:= Kernings.List^[Index].First;
    XMLNode.Attributes['Second']:= Kernings.List^[Index].Second;
    XMLNode.Attributes['Amount']:= Kernings.List^[Index].Amount;
  end;
  XMLKernings.Attributes['Count']:= Kernings.Count;

  TextureName:= ChangeFileExt(FileName, '.png');
  if GraphicFormats.Find(TextureName, GraphicFormat) then
  begin
    GraphicFormat.Filer.SaveToFile(TextureName, Texture.Graphic);
  end;

 // FormatXMLData()
  XMLDocument.SaveToFile(FileName);
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

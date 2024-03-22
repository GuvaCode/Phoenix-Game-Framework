unit phxSkinEx;

interface

uses Classes, Types, SysUtils, Windows, Graphics, Dialogs,

  Generics.Collections,  LCLType,

  xmldom, XMLIntf, msxmldom, XMLDoc,

  phxTypes,

  phxGraphics,
  phxGraphicsEx,

  phxSkin,
  phxEditor;

type

TDragArea =(
   daNone,

   daBoundsLeft,
   daBoundsRight,
   daBoundsTop,
   daBoundsBottom,

   daMarginLeft,
   daMarginRight,
   daMarginTop,
   daMarginBottom
 );
type


//------------------------------------------------------------------------------
TPHXSkinElementTools = class helper for TPHXSkinElement
  public
    //procedure LoadVersion1(Stream  : TStream); overload;
    //procedure LoadVersion2(Stream  : TStream); overload;
    //procedure LoadVersion3(Stream  : TStream); overload;

    function DragArea(const Position: TVector2i; Tolerance: Integer): TDragArea;
  end;

//------------------------------------------------------------------------------
TPHXSkinEx = class helper for TPHXSkin
  public
    procedure LoadFromXml(const FileName: String);
    procedure SaveToXml(const FileName: String);

    // Read the version for a skin file
    class function ReadVersion(const FileName: String ): Integer; overload;
    class function ReadVersion(const Stream  : TStream): Integer; overload;

    //procedure LoadVersion1(const FileName: String ); overload;
    //procedure LoadVersion1(Stream  : TStream); overload;

    //procedure LoadVersion2(const FileName: String ); overload;
   // procedure LoadVersion2(Stream  : TStream); overload;

   // procedure LoadVersion3(const FileName: String ); overload;
  //  procedure LoadVersion3(Stream  : TStream); overload;

    procedure SaveElement(const Filename: String; Element: TPHXSkinElement); overload;
    procedure SaveElement(const Filename: String; ElementIndex: Integer); overload;

    procedure Draw(Dest: TBitmap); overload;
    procedure Draw(Dest: TBitmap; Background: TBitmap ); overload;


    // Return a list of all elements in the skin for a control
    Procedure GetElementsForControl(const ControlName: String; const Elements: TList<TPHXSkinElement>);
  end;

TPHXSkinEditor = class;

// Selection for the skin editor
//------------------------------------------------------------------------------
TPHXSkinSelection = class
  private
    FOwner: TPHXSkinEditor;

    FidxElement: Integer;

    procedure SetElement(const Value: Integer);

    function GetElement: TPHXSkinElement;
  public
    constructor Create(Owner: TPHXSkinEditor);

    function ValidElement: Boolean;


    procedure SelectedChanged;

    property Editor: TPHXSkinEditor read FOwner;

    Property Element: TPHXSkinElement read GetElement;

    Property idxElement: Integer read FidxElement write SetElement;
  end;


//------------------------------------------------------------------------------
TPHXSkinEditor = class(TPHXEditor)
 private
    FSkin      : TPHXSkin;
    FBuffer    : TBitmap;
    FBackground: TBitmap;

    FSelection: TPHXSkinSelection;

    FOnChange: TNotifyEvent;

    FOnStateChange: TNotifyEvent;
    FOnElementChange: TNotifyEvent;

    //    FOptions: TPHXSkinEditorOptions;
    procedure SetSkin(const Value: TPHXSkin);
//    procedure SetOptions(const Value: TPHXSkinEditorOptions);
  protected
    procedure GetDocumentSize(out Width: Integer; out Height: Integer); override;
    procedure PaintDocument(const Offset: TVector2i; const Zoom: Single);   override;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;


    // Draw the skin handles for an element
    procedure DrawElement(const Index: Integer); overload;
    procedure DrawElement(const Element: TPHXSkinElement); overload;

    procedure DrawElementOutline(const Index: Integer); overload;
    procedure DrawElementOutline(const Element: TPHXSkinElement); overload;


    // The current skin
    Property Skin     : TPHXSkin read FSkin write SetSkin;
    property Selection: TPHXSkinSelection read FSelection;

    property Buffer: TBitmap read FBuffer;
    property Background: TBitmap read FBackground;
  published
    // Options for the image editor
//    property Options: TPHXSkinEditorOptions read FOptions write SetOptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    // Event thats called when the selected pattern is changed
    Property OnElementChange: TNotifyEvent read FOnElementChange write FOnElementChange;
    // Event thats called when the selected pattern is changed
    Property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;



procedure Register;

implementation


procedure Register;
begin
//  RegisterComponents('Phoenix', [TPHXEditor]);

  RegisterComponents('Phoenix', [TPHXSkinEditor]);
end;

//------------------------------------------------------------------------------
function TPHXSkinElementTools.DragArea(const Position: TVector2i; Tolerance: Integer): TDragArea;
var MinX, MinY: Integer;
var MaxX, MaxY: Integer;

var ML, MR, MT, MB: Integer;
begin
  MinX:= Position.X - Tolerance;
  MinY:= Position.Y - Tolerance;
  MaxX:= Position.X + Tolerance;
  MaxY:= Position.Y + Tolerance;

  ML:=  Bounds.Left   + Margins.Left;
  MR:=  Bounds.Right  - Margins.Right;
  MT:=  Bounds.Top    + Margins.Top;
  MB:=  Bounds.Bottom - Margins.Bottom;

  Result:= daNone;
  if (MinX < ML ) and (MaxX > ML ) then Result:=daMarginLeft;
  if (MinX < MR ) and (MaxX > MR ) then Result:=daMarginRight;

  if (MinY < MT ) and (MaxY > MT ) then Result:=daMarginTop;
  if (MinY < MB ) and (MaxY > MB ) then Result:=daMarginBottom;

  if (MinX < Bounds.Left  ) and (MaxX > Bounds.Left  ) then Result:=daBoundsLeft;
  if (MinX < Bounds.Right ) and (MaxX > Bounds.Right ) then Result:=daBoundsRight;

  if (MinY < Bounds.Top   ) and (MaxY > Bounds.Top   ) then Result:=daBoundsTop;
  if (MinY < Bounds.Bottom) and (MaxY > Bounds.Bottom) then Result:=daBoundsBottom;
end;



//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadFromXml(const FileName: String);
var XMLDocument    : IXMLDocument;
var XMLRoot        : IXMLNode;
var XMLElements    : IXMLNode;
var XMLElement    : IXMLNode;
var XMLNode        : IXMLNode;
var TextureName    : String;
var Index          : Integer;
var Element        : TPHXSkinElement;
begin
  XMLDocument:= LoadXMLDocument(FileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxSkin') then
  begin
    MessageDlg('The selected file is not a Phoenix XML Skin', mtError, [mbOk], 0);

    Exit;
  end;

  Name       := XMLRoot.Attributes['Name'   ];
  Width      := XMLRoot.Attributes['Width'  ];
  Height     := XMLRoot.Attributes['Height' ];
  TextureName:= XMLRoot.Attributes['Texture'];

  XMLElements:= XMLRoot.ChildNodes.FindNode('Elements');
  if Assigned(XMLElements) then
  begin
    for Index := 0 to XMLElements.ChildNodes.Count - 1 do
    begin
      XMLElement:= XMLElements.ChildNodes[Index];

      if not SameText(XMLElement.NodeName, 'Element') then Continue;

      Element:= Elements.Add;

      // OLD Format:
      if XMLElement.HasAttribute('Bounds.Left') then
      begin
       Element.Bounds:= TRecti.Create(
          XMLElement.Attributes['Bounds.Left'  ],
          XMLElement.Attributes['Bounds.Top' ],
          XMLElement.Attributes['Bounds.Right'   ],
          XMLElement.Attributes['Bounds.Bottom']
          );
      end;
      if XMLElement.HasAttribute('Margins.Left') then
      begin
       Element.Margins:= TRecti.Create(
          XMLElement.Attributes['Margins.Left'  ],
          XMLElement.Attributes['Margins.Top'   ],
          XMLElement.Attributes['Margins.Right' ],
          XMLElement.Attributes['Margins.Bottom']
          );
      end;

      if XMLElement.HasAttribute('Foreground') then
      begin
       // Element.Color       := TColor4f.Create(XMLElement.Attributes['Foreground']);
        Element.TextColor:= TColor4f.Create(XMLElement.Attributes['Background']);
      end;

      if XMLElement.HasAttribute('Offset.X') and XMLElement.HasAttribute('Offset.Y') then
      begin
        Element.TextPadding:= TRecti.Create(
          XMLElement.Attributes['Offset.X'],
          XMLElement.Attributes['Offset.Y'],
          0,
          0
        );
      end;

      Element.Name := XMLElement.Attributes['Name'];

      if XMLElement.HasAttribute('Color') then
      begin
        //Element.Color:= TColor4f.Create(XMLElement.Attributes['Color']);
      end;

      XMLNode:= XMLElement.ChildNodes.FindNode('Bounds');
      if Assigned(XMLNode) then
      begin
        Element.Bounds:= TRecti.Create(
          XMLNode.Attributes['Left'  ],
          XMLNode.Attributes['Top' ],
          XMLNode.Attributes['Right'   ],
          XMLNode.Attributes['Bottom']
        );
      end;

      XMLNode:= XMLElement.ChildNodes.FindNode('FixedMargins');
      if Assigned(XMLNode) then
      begin
        Element.Margins:= TRecti.Create(
          XMLNode.Attributes['Left'  ],
          XMLNode.Attributes['Top' ],
          XMLNode.Attributes['Right'   ],
          XMLNode.Attributes['Bottom']
        );
      end;

      XMLNode:= XMLElement.ChildNodes.FindNode('OuterMargins');
      if Assigned(XMLNode) then
      begin
        Element.Shadow:= TRecti.Create(
          XMLNode.Attributes['Left'  ],
          XMLNode.Attributes['Top' ],
          XMLNode.Attributes['Right'   ],
          XMLNode.Attributes['Bottom']
        );
      end;


      XMLNode:= XMLElement.ChildNodes.FindNode('ContentMargins');
      if Assigned(XMLNode) then
      begin
        Element.TextPadding:= TRecti.Create(
          XMLNode.Attributes['Left'  ],
          XMLNode.Attributes['Top' ],
          XMLNode.Attributes['Right'   ],
          XMLNode.Attributes['Bottom']
        );
      end;

      if XMLElement.HasAttribute('ContentColor') then
      begin
        Element.TextColor:= TColor4f.Create(XMLElement.Attributes['ContentColor']);
      end;


    end;

  end;
  //Texture.LoadTexture(TextureName);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.SaveToXml(const FileName: String);
var XMLDocument    : IXMLDocument;
var XMLRoot        : IXMLNode;
var XMLElements    : IXMLNode;
var XMLElement     : IXMLNode;
var XMLNode        : IXMLNode;
var TextureName    : String;
var Index          : Integer;
var Element        : TPHXSkinElement;

begin
  TextureName:= ChangeFileExt(FileName, '.png') ;

  XMLDocument:= NewXMLDocument;
  XMLDocument.Options:= XMLDocument.Options + [doNodeAutoIndent];


  XMLRoot:= XMLDocument.AddChild('phxSkin');
  XMLRoot.Attributes['Name'   ]:= Name;
  XMLRoot.Attributes['Width'  ]:= Width;
  XMLRoot.Attributes['Height' ]:= Height;
  XMLRoot.Attributes['Texture']:= ExtractRelativePath(FileName, TextureName);

  //  XMLSettings:= XMLRoot.AddChild('settings');
  //  XMLSettings.Attributes['ScrollBarWidth' ]:=Editor.Skin.Settings.ScrollBarWidth;
  //  XMLSettings.Attributes['ScrollBarHeight']:=Editor.Skin.Settings.ScrollBarHeight;

  XMLElements:= XMLRoot.AddChild('Elements');
  XMLElements.Attributes['Count']:= Elements.Count;

  for Index := 0 to Elements.Count - 1 do
  begin
    Element:= Elements[Index];

    XMLElement:= XMLElements.AddChild('Element');
    XMLElement.Attributes['Name' ]:= Element.Name;
   // XMLElement.Attributes['Color']:= Element.Color.ToColor;


    XMLNode:= XMLElement.AddChild('Bounds');
    begin
      XMLNode.Attributes['Left'  ]:=Element.Bounds.Left;
      XMLNode.Attributes['Top'   ]:=Element.Bounds.Top;
      XMLNode.Attributes['Right' ]:=Element.Bounds.Right;
      XMLNode.Attributes['Bottom']:=Element.Bounds.Bottom;
    end;

    XMLNode:= XMLElement.AddChild('Margins');
    begin
      XMLNode.Attributes['Left'  ]:= Element.Margins.Left;
      XMLNode.Attributes['Top'   ]:= Element.Margins.Top;
      XMLNode.Attributes['Right' ]:= Element.Margins.Right;
      XMLNode.Attributes['Bottom']:= Element.Margins.Bottom;
    end;

    XMLNode:= XMLElement.AddChild('Shadow');
    begin
      XMLNode.Attributes['Left'  ]:= Element.Shadow.Left;
      XMLNode.Attributes['Top'   ]:= Element.Shadow.Top;
      XMLNode.Attributes['Right' ]:= Element.Shadow.Right;
      XMLNode.Attributes['Bottom']:= Element.Shadow.Bottom;
    end;

    XMLNode:= XMLElement.AddChild('TextPadding');
    begin
      XMLNode.Attributes['Left'  ]:= Element.TextPadding.Left;
      XMLNode.Attributes['Top'   ]:= Element.TextPadding.Top;
      XMLNode.Attributes['Right' ]:= Element.TextPadding.Right;
      XMLNode.Attributes['Bottom']:= Element.TextPadding.Bottom;
    end;
    XMLElement.Attributes['TextColor']:=Element.TextColor.ToColor;
       (*

            // FixedMargins
    // OuterMargins
    // ContentMargins
    // ContentColor

    XMLElement.Attributes['Bounds.Left'  ]:=Element.Bounds.Left;
    XMLElement.Attributes['Bounds.Top'   ]:=Element.Bounds.Top;
    XMLElement.Attributes['Bounds.Right' ]:=Element.Bounds.Right;
    XMLElement.Attributes['Bounds.Bottom']:=Element.Bounds.Bottom;

    XMLElement.Attributes['Margins.Left'  ]:=Element.FixedMargins.Left;
    XMLElement.Attributes['Margins.Top'   ]:=Element.FixedMargins.Top;
    XMLElement.Attributes['Margins.Right' ]:=Element.FixedMargins.Right;
    XMLElement.Attributes['Margins.Bottom']:=Element.FixedMargins.Bottom;

    XMLElement.Attributes['OuterMargins.Left'  ]:=Element.OuterMargins.Left;
    XMLElement.Attributes['OuterMargins.Top'   ]:=Element.OuterMargins.Top;
    XMLElement.Attributes['OuterMargins.Right' ]:=Element.OuterMargins.Right;
    XMLElement.Attributes['OuterMargins.Bottom']:=Element.OuterMargins.Bottom;


    XMLElement.Attributes['Foreground']:= ColorToRGBA(Element.Foreground);
    XMLElement.Attributes['Background']:= ColorToRGBA(Element.Background);


    XMLElement.Attributes['Offset.X']:=Element.Offset.X;
    XMLElement.Attributes['Offset.Y']:=Element.Offset.Y;
    *)
  end;
  XMLDocument.SaveToFile(FileName);

  Texture.SaveTexture(TextureName);
end;


//------------------------------------------------------------------------------
class function TPHXSkinEx.ReadVersion(const FileName: String): Integer;
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result:= ReadVersion(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
class function TPHXSkinEx.ReadVersion(const Stream: TStream): Integer;
var Header: TPHXSkinHeader;
begin
  Header.Ident  := #0#0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXSKIN') then
  begin
    raise Exception.Create('Not a valid Phoenix Skin.');
  end;

  Result:= Header.Version;
end;
                               {
//------------------------------------------------------------------------------
procedure TPHXSkinElementTools.LoadVersion1(Stream: TStream);
var State: Cardinal;
var FBounds   : TRecti;
var FMargins  : TRecti;
var FForeground: TColor4f;
var FBackground: TColor4f;
var FOffset    : TVector2i;
begin
  Name:= ReadStr(Stream);

  Stream.Read(State , SizeOf(Cardinal));

  Stream.Read(FBounds    , SizeOf(FBounds    )); Bounds    := FBounds;
  Stream.Read(FMargins   , SizeOf(FMargins   )); FixedMargins  := FMargins;
  Stream.Read(FForeground, SizeOf(FForeground)); Color     := FForeground;
  Stream.Read(FBackground, SizeOf(FBackground)); ContentColor:= FBackground;
  Stream.Read(FOffset    , SizeOf(FOffset    )); ContentMargins:= Recti(FOffset.X, FOffset.Y, 0, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElementTools.LoadVersion2(Stream: TStream);
var FBounds   : TRecti;
var FMargins  : TRecti;
var FForeground: TColor4f;
var FBackground: TColor4f;
var FOffset    : TVector2i;
begin
  Name:= ReadString(Stream);

  Stream.Read(FBounds    , SizeOf(FBounds    )); Bounds    := FBounds;
  Stream.Read(FMargins   , SizeOf(FMargins   )); FixedMargins   := FMargins;
  Stream.Read(FForeground, SizeOf(FForeground)); Color:= FForeground;
  Stream.Read(FBackground, SizeOf(FBackground)); ContentColor:= FBackground;
  Stream.Read(FOffset    , SizeOf(FOffset    )); ContentMargins    :=Recti(FOffset.X, FOffset.Y, 0, 0);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinElementTools.LoadVersion3(Stream: TStream);
var FBounds   : TRecti;
var FMargins  : TRecti;
var FForeground: TColor4f;
var FBackground: TColor4f;
var FOffset    : TVector2i;
begin
  Name:= ReadString(Stream);

  Stream.Read(FBounds       , SizeOf(FBounds));
  Stream.Read(FMargins     , SizeOf(FMargins));
  Stream.Read(FForeground   , SizeOf(FForeground));
  Stream.Read(FBackground   , SizeOf(FBackground));
  Stream.Read(FOffset       , SizeOf(FOffset));

  Bounds         := FBounds;
  FixedMargins   := FMargins;
  Color          := FForeground;
  ContentColor   := FBackground;
  ContentMargins :=Recti(FOffset.X, FOffset.Y, 0, 0);
end;


//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadVersion1(const FileName: String);
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadVersion1(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadVersion2(const FileName: String);
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadVersion2(Stream);
  finally
    Stream.Free;
  end;
end;




//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadVersion1(Stream: TStream);
var Header     : TPHXSkinHeader;
var Counter    : Integer;
var Element    : TPHXSkinElement;

var AWidth: Integer;
var AHeight: Integer;
var ACount     : Integer;

var SkinName : String;
var SkinCount: Integer;
var SkinIndex: Integer;
var SkinState: Integer;
begin
  Header.Ident  := #0#0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXSKIN') then
  begin
    Exit;
  end;
  If (Header.Version <> 1) then
  begin
    raise Exception.CreateFmt('Image version missmatch [File: %d Code: %d].', [Header.Version, 1]);
  end;


  // The name of the image.
  Name:= ReadStr(Stream);
  // The width of the image
  Stream.Read(AWidth, SizeOf(AWidth)); Width:= AWidth;
  // The height of the image
  Stream.Read(AHeight, SizeOf(AHeight)); Height:= AHeight;
  // The pattern count
  Stream.Read(ACount, SizeOf(ACount));


  Elements.Clear;
  // Write all skins
  for Counter:=0 to ACount - 1 do
  begin
    ReadString(Stream, SkinName);

    Stream.Read(SkinCount, SizeOf(SkinCount));

    for SkinIndex := 0 to SkinCount - 1 do
    begin
      Element:= Elements.Add;
      Element.LoadVersion1(Stream);
      Element.Name:= SkinName + '.' + Element.Name;
    end;
  end;
  Texture.LoadFromStream(Stream);

  Initialize;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadVersion2(Stream: TStream);
var Header     : TPHXSkinHeader;
var Counter    : Integer;
var Element    : TPHXSkinElement;

var AWidth: Integer;
var AHeight: Integer;
var ACount     : Integer;

var SkinName : String;
var SkinCount: Integer;
var SkinIndex: Integer;
begin
  Header.Ident  := #0#0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXSKIN') then
  begin
    Exit;
  end;
  If (Header.Version <> 2) then
  begin
    raise Exception.CreateFmt('Image version missmatch [File: %d Code: %d].', [Header.Version, 2]);
  end;


  // The name of the image.
  Name:= ReadString(Stream);
  // The width of the image
  Stream.Read(AWidth, SizeOf(AWidth)); Width:= AWidth;
  // The height of the image
  Stream.Read(AHeight, SizeOf(AHeight)); Height:= AHeight;
  // The pattern count
  Stream.Read(ACount, SizeOf(ACount));

  Elements.Clear;
  // Write all elements
  for Counter:=0 to ACount - 1 do
  begin
    ReadString(Stream, SkinName);

    Stream.Read(SkinCount, SizeOf(SkinCount));

    for SkinIndex := 0 to SkinCount - 1 do
    begin
      Element:= Elements.Add;
      Element.LoadVersion2(Stream);
      Element.Name:= SkinName + '.' + Element.Name;
    end;
    //Current.LoadVersion1(Stream);
  end;
  Texture.LoadFromStream(Stream);

  Initialize;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadVersion3(const FileName: String);
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadVersion3(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.LoadVersion3(Stream: TStream);
var Header     : TPHXSkinHeader;
var Counter    : Integer;
var ACount     : Integer;
var Current    : TPHXSkinElement;

var AWidth: Integer;
var AHeight: Integer;
begin
  Header.Ident  := #0#0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));


  If (Header.Ident <> 'PHXSKIN') then
  begin
    raise Exception.Create('Not a valid Phoenix Skin.');
  end;
  If (Header.Version <> 3) then
  begin
    raise Exception.CreateFmt('Skin version missmatch [File: %d Code: %d].', [Header.Version, PHXSKIN_VERSION]);
  end;

  // The name of the skin.
  Name:= ReadString(Stream);
  // The width of the image
  Stream.Read(AWidth, SizeOf(AWidth)); Width:= AWidth;
  // The height of the image
  Stream.Read(AHeight, SizeOf(AHeight)); Height:= AHeight;
  // The pattern count
  Stream.Read(ACount, SizeOf(ACount));

  Elements.Clear;
  // Write all skins
  for Counter:=0 to ACount - 1 do
  begin
    Current:= Elements.Add;

    Current.LoadVersion3(Stream);
  end;
  Texture.LoadFromStream(Stream);

  Initialize;
end;
     }
//------------------------------------------------------------------------------
procedure TPHXSkinEx.SaveElement(const Filename: String; Element: TPHXSkinElement);
var Bitmap: TPHXBitmap;
begin
  Bitmap:= TPHXBitmap.Create;
  try
    Bitmap.Resize(Element.Width, Element.Height, Texture.Format);
    Bitmap.CopyFrom(Texture.Graphic, Element.Bounds, TVector2i.Create(0,0) );
    Bitmap.SaveBitmap(Filename);

  finally
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.SaveElement(const Filename: String; ElementIndex: Integer);
begin
  SaveElement(Filename, Elements[ElementIndex] );
end;


//------------------------------------------------------------------------------
procedure TPHXSkinEx.Draw(Dest: TBitmap);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Texture.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEx.Draw(Dest: TBitmap; Background: TBitmap );
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf32bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  if (Background.Width > 0) and (Background.Height > 0) then
  begin
    Y:=0;
    while Y < Height do
    begin
      X:=0;
      while X < Width do
      begin
        Dest.Canvas.Draw(X,Y, Background);
        Inc(X, Background.Width);
      end;
      Inc(Y, Background.Height);
   end;
  end;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= Texture.ScanLine(Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXSkinEx.GetElementsForControl(const ControlName: String; const Elements: TList<TPHXSkinElement>);
var Index  : Integer;
var Element: TPHXSkinElement;
var ElementControl: String;
begin
  for Index := 0 to Self.Elements.Count - 1 do
  begin
    Element:= Self.Elements[Index];

    ElementControl:= GetControlName(Element.Name);

    if SameText(ControlName, ElementControl ) then
    begin
      Elements.Add(Element);
    end;
  end;
end;








// TPHXSkinSelection
//==============================================================================
constructor TPHXSkinSelection.Create(Owner: TPHXSkinEditor);
begin
  FOwner:= Owner;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinSelection.SelectedChanged;
begin
  if Assigned(Editor.OnStateChange) then Editor.OnStateChange(Editor);
end;

//------------------------------------------------------------------------------
procedure TPHXSkinSelection.SetElement(const Value: Integer);
begin
  FidxElement := Value;

  SelectedChanged
end;

//------------------------------------------------------------------------------
function TPHXSkinSelection.ValidElement: Boolean;
begin
  Result:= Assigned(Editor.Skin) and (IdxElement >= 0) and (IdxElement < FOwner.Skin.Elements.Count);
end;

//------------------------------------------------------------------------------
function TPHXSkinSelection.GetElement: TPHXSkinElement;
begin
  if ValidElement then
  begin
    Result:= Editor.Skin.Elements[IdxElement];
  end else
  begin
    Result:= nil;
  end;
end;




// TPHXSkinEditor
//==============================================================================
constructor TPHXSkinEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered:= True;

  FBuffer    := TBitmap.Create;
  FBackground:= CreateTransparentImage(8);

  FSelection:= TPHXSkinSelection.Create(Self);
//  FGridSize:= 16;

//  FBackground:= TBitmap.Create;

//  FPatternIndex:= NO_PATTERN;
end;

//------------------------------------------------------------------------------
destructor TPHXSkinEditor.Destroy;
begin
  FBuffer.Free;
  FBackground.Free;
  FSelection.Free;
  inherited;
end;


//------------------------------------------------------------------------------
procedure TPHXSkinEditor.GetDocumentSize(out Width, Height: Integer);
begin
  if Assigned(Skin) then
  begin
    Width := Skin.Width;
    Height:= Skin.Height;
  end else
  begin
    Width := 0;
    Height:= 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEditor.PaintDocument(const Offset: TVector2i; const Zoom: Single);
var Rect: TRect;
begin
  if Assigned(Skin) then
  begin

    if (Skin.Width > 0) and (Skin.Height > 0) then
    begin
      Rect.Left  := Offset.X;
      Rect.Top   := Offset.Y;
      Rect.Right := Offset.X + Trunc(Skin.Width  * Zoom);
      Rect.Bottom:= Offset.Y + Trunc(Skin.Height * Zoom);

      with Canvas do
      begin
      //  Brush.Color:= clWhite;
      //  Brush.Style:= bsSolid;
      //  Pen.Color:= clWhite;

      //  FillRect(ClipRect);

        StretchDraw(Rect, FBuffer);

        Dec(Rect.Left);
        Inc(Rect.Right);
        Dec(Rect.Top);
        Inc(Rect.Bottom);

        Brush.Style := bsClear;
        Pen  .Color:= clBlack;
        Pen  .Style:= psSolid;

     //   Rectangle(Rect);


      end;

      Grid.Draw(Canvas, Rect);

    end else
    begin
      with Canvas do
      begin
    //    Brush.Color:= clBtnFace;
     //   Brush.Style:= bsSolid;
    //    Pen.Color  := clBtnFace;
    //    Pen.Style  := psSolid;

//        FillRect(ClipRect);

     //   W:= TextWidth ('Image is empty');
     //   H:= TextHeight('Image is empty');

      //  TextOut((Width - W) div 2, (Height - H) div 2, 'Image is empty' );
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEditor.DrawElement(const Index: Integer);
var Element: TPHXSkinElement;
begin
  if Assigned(Skin) and (Index >= 0) and (Index < Skin.Elements.Count) then
  begin
    Element:= Skin.Elements[Index];

    DrawElement(Element);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEditor.DrawElement(const Element: TPHXSkinElement);
var Offset : TVector2i;

var rBounds : TRect;
var rMargins: TRect;
begin
  Offset:= Viewport.Offset;


  rBounds.Left  := Offset.X + Round( Element.Bounds.Left   * Viewport.Zoom);
  rBounds.Right := Offset.X + Round( Element.Bounds.Right  * Viewport.Zoom);
  rBounds.Top   := Offset.Y + Round( Element.Bounds.Top    * Viewport.Zoom);
  rBounds.Bottom:= Offset.Y + Round( Element.Bounds.Bottom * Viewport.Zoom);

  rMargins.Left  := Offset.X + Round( ( Element.Bounds.Left   + Element.Margins.Left   ) * Viewport.Zoom);
  rMargins.Right := Offset.X + Round( ( Element.Bounds.Right  - Element.Margins.Right  ) * Viewport.Zoom);
  rMargins.Top   := Offset.Y + Round( ( Element.Bounds.Top    + Element.Margins.Top    ) * Viewport.Zoom);
  rMargins.Bottom:= Offset.Y + Round( ( Element.Bounds.Bottom - Element.Margins.Bottom ) * Viewport.Zoom);


  with Canvas do
  begin
     Brush.Style:= bsClear;

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(rBounds);

     Pen.Color  := clBlack;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(rBounds);


     Pen.Color  := clYellow;
     Pen.Style  := psSolid;

     if Element.Margins.Left  > 0 then
     begin
       MoveTo(rMargins.Left        , rBounds.Top);
       LineTo(rMargins.Left        , rBounds.Bottom);
     end;

     if Element.Margins.Right  > 0 then
     begin
       MoveTo(rMargins.Right        , rBounds.Top);
       LineTo(rMargins.Right        , rBounds.Bottom);
     end;

     if Element.Margins.Top  > 0 then
     begin
       MoveTo(rBounds.Left   , rMargins.Top);
       LineTo(rBounds.Right  , rMargins.Top);
     end;

     if Element.Margins.Bottom  > 0 then
     begin
       MoveTo(rBounds.Left   , rMargins.Bottom);
       LineTo(rBounds.Right  , rMargins.Bottom);
     end;

     Pen.Color  := clBlack;
     Pen.Style  := psDot;

     if Element.Margins.Left  > 0 then
     begin
       MoveTo(rMargins.Left        , rBounds.Top);
       LineTo(rMargins.Left        , rBounds.Bottom);
     end;

     if Element.Margins.Right  > 0 then
     begin
       MoveTo(rMargins.Right        , rBounds.Top);
       LineTo(rMargins.Right        , rBounds.Bottom);
     end;

     if Element.Margins.Top  > 0 then
     begin
       MoveTo(rBounds.Left   , rMargins.Top);
       LineTo(rBounds.Right  , rMargins.Top);
     end;

     if Element.Margins.Bottom  > 0 then
     begin
       MoveTo(rBounds.Left   , rMargins.Bottom);
       LineTo(rBounds.Right  , rMargins.Bottom);
     end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEditor.DrawElementOutline(const Index: Integer);
var Element: TPHXSkinElement;
begin
  if (Index >= 0) and (Index < Skin.Elements.Count) then
  begin
    Element:= Skin.Elements[Index];

    DrawElementOutline(Element);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXSkinEditor.DrawElementOutline(const Element: TPHXSkinElement);
var Offset : TVector2i;
var rBounds : TRect;
begin
  Offset:= Viewport.Offset;

  rBounds.Left  := Offset.X + Round( Element.Bounds.Left   * Viewport.Zoom);
  rBounds.Right := Offset.X + Round( Element.Bounds.Right  * Viewport.Zoom);
  rBounds.Top   := Offset.Y + Round( Element.Bounds.Top    * Viewport.Zoom);
  rBounds.Bottom:= Offset.Y + Round( Element.Bounds.Bottom * Viewport.Zoom);

  with Canvas do
  begin
     Brush.Style:= bsClear;

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(rBounds);

     Pen.Color  := clBlack;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(rBounds);

     Pen.Color  := clYellow;
     Pen.Style  := psSolid;

     Brush.Style:= bsSolid;
     Brush.Color:= clSilver;
     Font.Color := clBlack;

     if Length(Element.Name) > 0  then
     begin
       TextOut(rBounds.Right+2, rBounds.Top, Element.Name );
     end;
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXSkinEditor.SetSkin(const Value: TPHXSkin);
begin
  if FSkin <> Value then
  begin
    FSkin := Value;

    ScrollToCenter;
  end;

  if Assigned(FSkin) then
  begin
    if doTransparent in Options then
    begin
      Skin.Draw(FBuffer, FBackground);
    end else
    begin
      Skin.Draw(FBuffer);
    end;
  end
  else
  begin
    ScrollToCenter;

    FBuffer.Width := 0;
    FBuffer.Height:= 0;
  end;
  DocumentChanged;

  Invalidate;

  if Assigned(OnChange) then OnChange(Self);
end;





end.

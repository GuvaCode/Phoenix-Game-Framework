unit phxImageEx;

interface

uses Classes, Types, SysUtils, Windows, Graphics, Dialogs,

  xmldom, XMLIntf, msxmldom, XMLDoc,

  phxTypes,
  phxMath,

  phxGraphics,
  phxGraphicsEx,

  phxImage;

type

//------------------------------------------------------------------------------
TPHXImageEx = class helper for TPHXImage
  private
    procedure SavePatterns(Parent: IXMLNode);
    procedure SaveTags(Parent: IXMLNode);
    procedure SaveTexture(const FileName: String) ;

    procedure LoadPatterns(Parent: IXMLNode);
    procedure LoadTags(Parent: IXMLNode);
    procedure LoadTexture(const FileName: String);
  public
    // Load the image from a xml file
    procedure LoadFromXML(const FileName: String);
    // Load the image from a xml file
    procedure SaveToXML(const FileName: String);


    procedure Draw(Dest: TBitmap); overload;
    procedure Draw(Dest: TBitmap; Background: TBitmap ); overload;
    procedure Draw(Dest: TBitmap; Background: TBitmap; Mask: Single ); overload;
    procedure Draw(Dest: TBitmap; Background: TColor ); overload;

    procedure DrawPattern(Dest: TCanvas; X, Y: Single; PatternIndex: Integer; Zoom: Single); overload;

    procedure DrawPattern(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; PatternIndex: Integer); overload;
    procedure DrawPattern(Dest: TCanvas; Buffer: TBitmap; X, Y: Integer; PatternIndex: Integer; Zoom: Single); overload;

    procedure SavePattern(const Filename: String; Pattern: TPHXPattern); overload;
    procedure SavePattern(const Filename: String; PatternIndex: Integer);overload;

    procedure LoadPattern(const Filename: String; Pattern: TPHXPattern); overload;

    class function ReadVersion(const FileName: String ): Integer; overload;
    class function ReadVersion(const Stream  : TStream): Integer; overload;

   // procedure LoadVersion4(const FileName: String); overload;
   // procedure LoadVersion4(const Stream  : TStream); overload;
  end;

//------------------------------------------------------------------------------
TPHXAnimationEx = class helper for TPHXAnimation
  private
    procedure LoadFrames(Parent: IXMLNode);
    procedure SaveFrames(Parent: IXMLNode);
  public
    // Load the animation from a xml file
    procedure LoadFromXML(const FileName: String);
    // Load the animation from a xml file
    procedure SaveToXML(const FileName: String);
  end;


implementation


{$REGION 'TPHXImageEx'}

//------------------------------------------------------------------------------
procedure TPHXImageEx.SaveToXML(const FileName: String);
var Document: IXMLDocument;
var Root    : IXMLNode;
var Node    : IXMLNode;
begin
  Document:= NewXMLDocument;
  Document.Options:= Document.Options + [doNodeAutoIndent];

  Root:= Document.AddChild('phxImage');
  Root.Attributes['Version']:= PHXIMAGE_VERSION;

  Node:= Root.AddChild('Image');
  begin
    Node.Attributes['Name'   ]:= Name;
    Node.Attributes['Author' ]:= Author;
    Node.Attributes['Version']:= Version;
    Node.Attributes['Comment']:= Comment;
    Node.Attributes['Width'  ]:= Width;
    Node.Attributes['Height' ]:= Height;
  end;
  Node:= Root.AddChild('Patterns');
  begin
    SavePatterns(Node);
  end;
  Node:= Root.AddChild('Tags');
  begin
    SaveTags(Node);
  end;

  Document.SaveToFile(FileName);

  SaveTexture(ChangeFileExt(FileName, '.png'));
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadFromXML(const FileName: String);
var Document: IXMLDocument;
var Root    : IXMLNode;
var Node    : IXMLNode;
begin
  Document:= LoadXMLDocument(FileName);

  Root:= Document.DocumentElement;

  if Root.NodeName <> 'phxImage' then
  begin
    MessageDlg( Format('The file "%s" is not a valid phoenix image', [FileName]), mtError, [mbOK], 0);

    Exit;
  end;

  Node:= Root.ChildNodes.FindNode('Image');
  if Assigned(Node) then
  begin
    Name   := Node.Attributes['Name'   ];
    Author := Node.Attributes['Author' ];
    Version:= Node.Attributes['Version'];
    Comment:= Node.Attributes['Comment'];
    Width  := Node.Attributes['Width'  ];
    Height := Node.Attributes['Height' ];
  end;
  Node:= Root.ChildNodes.FindNode('Patterns');
  if Assigned(Node) then
  begin
    LoadPatterns(Node);
  end;
  Node:= Root.ChildNodes.FindNode('Tags');
  if Assigned(Node) then
  begin
    LoadTags(Node);
  end;

  if FileExists(ChangeFileExt(FileName, '.png')) then
  begin
    LoadTexture(ChangeFileExt(FileName, '.png'));
  end else
  begin
    MessageDlg( Format('Could not load the texture "%s".', [ChangeFileExt(FileName, '.png')]), mtError, [mbOK], 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.SaveTexture(const FileName: String);
var Bitmap: TPHXBitmap;
begin
  Bitmap:= TPHXBitmap.Create;
  try
    Bitmap.Import(Texture.Graphic);

    Bitmap.SaveBitmap(FileName);
  finally
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.SavePatterns(Parent: IXMLNode);
var Index  : Integer;
var Pattern: TPHXPattern;
var Node   : IXMLNode;
begin
  for Index:= 0 to Patterns.Count - 1 do
  begin
    Pattern:= Patterns[Index];

    Node:= Parent.AddChild('Pattern');
    Node.Attributes['Name'   ]:=Pattern.Name;
    Node.Attributes['X'      ]:=Pattern.X;
    Node.Attributes['Y'      ]:=Pattern.Y;
    Node.Attributes['Width'  ]:=Pattern.Width;
    Node.Attributes['Height' ]:=Pattern.Height;
    Node.Attributes['Pivot.X']:=Pattern.Pivot.X;
    Node.Attributes['Pivot.Y']:=Pattern.Pivot.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.SaveTags(Parent: IXMLNode);
var Index  : Integer;
var Tag    : TPHXTag;
var Node   : IXMLNode;
begin
  for Index:= 0 to Tags.Count - 1 do
  begin
    Tag:= Tags[Index];

    Node:= Parent.AddChild('Tag');
    Node.Attributes['Name'    ]:= Tag.Name;
    Node.Attributes['Pattern' ]:= Tag.Pattern;
    Node.Attributes['X'       ]:= Tag.X;
    Node.Attributes['Y'       ]:= Tag.Y;
    Node.Attributes['Rotation']:= Tag.Rotation;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadTexture(const FileName: String);
begin
  Texture.LoadTexture(FileName);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadPatterns(Parent: IXMLNode);
var Index  : Integer;
var Pattern: TPHXPattern;
var Node   : IXMLNode;
begin
  Patterns.Clear;

  for Index := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node:= Parent.ChildNodes[Index];

    if Node.NodeName <> 'Pattern' then Continue;

    if Node.HasAttribute('Name')    then Pattern.Name    := ShortString(Node.Attributes['Name']);
    if Node.HasAttribute('X')       then Pattern.X       :=             Node.Attributes['X'];
    if Node.HasAttribute('Y')       then Pattern.Y       :=             Node.Attributes['Y'];
    if Node.HasAttribute('Width')   then Pattern.Width   :=             Node.Attributes['Width'];
    if Node.HasAttribute('Height')  then Pattern.Height  :=             Node.Attributes['Height'];
    if Node.HasAttribute('Pivot.X') then Pattern.Pivot.X :=             Node.Attributes['Pivot.X'];
    if Node.HasAttribute('Pivot.Y') then Pattern.Pivot.Y :=             Node.Attributes['Pivot.Y'];

    Patterns.Add(Pattern);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadTags(Parent: IXMLNode);
var Index  : Integer;
var Tag    : TPHXTag;
var Node   : IXMLNode;
begin
  Tags.Clear;
  for Index := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node:= Parent.ChildNodes[Index];

    if Node.NodeName <> 'Tag' then Continue;

    if Node.HasAttribute('Name')     then Tag.Name    := ShortString(Node.Attributes['Name']);
    if Node.HasAttribute('Pattern')  then Tag.Pattern :=             Node.Attributes['Pattern'];
    if Node.HasAttribute('X')        then Tag.X       :=             Node.Attributes['X'];
    if Node.HasAttribute('Y')        then Tag.Y       :=             Node.Attributes['Y'];
    if Node.HasAttribute('Rotation') then Tag.Rotation:=             Node.Attributes['Rotation'];

    Tags.Add(Tag);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest: TBitmap);
var X,Y  : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;

var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf24bit;
  Dest.Width       := Width;
  Dest.Height      := Height;

  for y := 0 to Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      DstColor.rgbtRed  := SrcColor.Red;
      DstColor.rgbtGreen:= SrcColor.Green;
      DstColor.rgbtBlue := SrcColor.Blue;

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 3);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest: TBitmap; Background: TBitmap );
var X,Y     : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha   : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf24bit;
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
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor          :=  PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc(DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc(DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc(DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 3);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest: TBitmap; Background: TColor);
var X,Y     : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha   : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf24bit;
  Dest.Width       := Texture.Width;
  Dest.Height      := Texture.Height;

  for y := 0 to Texture.Height - 1 do
  begin
    DstPixel:= Dest.Scanline[y];
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Texture.Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= SrcColor.Alpha / 255;

      DstColor.rgbtRed  := Trunc( GetRValue(Background) * (1- Alpha) + SrcColor.Red   * (Alpha));
      DstColor.rgbtGreen:= Trunc( GetGValue(Background) * (1- Alpha) + SrcColor.Green * (Alpha));
      DstColor.rgbtBlue := Trunc( GetBValue(Background) * (1- Alpha) + SrcColor.Blue  * (Alpha));

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 3);
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImageEx.DrawPattern(Dest: TCanvas; X,Y: Single; PatternIndex: Integer; Zoom: Single);
var Pattern: TPHXPattern ;
var rPattern: TRect;
var rPivot  : TPoint;
begin
  if (PatternIndex < 0) and (PatternIndex < Patterns.Count) then
  begin
    Exit;
  end;

  Pattern:=Patterns[PatternIndex];

  rPattern.Left  := Round(X + (Pattern.X                 ) * Zoom);
  rPattern.Top   := Round(Y + (Pattern.Y                 ) * Zoom);
  rPattern.Right := Round(X + (Pattern.X + Pattern.Width ) * Zoom);
  rPattern.Bottom:= Round(Y + (Pattern.Y + Pattern.Height) * Zoom);

  rPivot.X:= Round(X + (Pattern.X + Pattern.Pivot.X ) * Zoom);
  rPivot.Y:= Round(Y + (Pattern.Y + Pattern.Pivot.Y ) * Zoom);

  with Dest do begin
     Brush.Style:= bsClear;

     Pen.Color  := clYellow;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(rPattern);

     Pen.Color  := clBlack;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(rPattern);


     Pen.Color  := clYellow;
     Pen.Style  := psDot;

     if Pattern.Pivot.X  > 0 then
     begin
       MoveTo(rPivot.X        , rPattern.Top);
       LineTo(rPivot.X        , rPattern.Bottom);
     end;

     if Pattern.Pivot.Y  > 0 then
     begin
       MoveTo(rPattern.Left   , rPivot.Y);
       LineTo(rPattern.Right  , rPivot.Y);
     end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.Draw(Dest: TBitmap; Background: TBitmap; Mask: Single );
var X,Y     : Integer;
var DstColor: TRGBTriple;
var SrcColor: TPHXPixel;
var DstPixel: PByte;
var SrcPixel: PByte;
var Alpha  : Single;
var GetPixel: TGetPixel;
begin
  GetPixel:= GetPixelFormatGetter(Texture.Format);

  Dest.PixelFormat := pf24bit;
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
    SrcPixel:= ScanLine(Texture, Y);
    for x := 0 to Width - 1 do
    begin
      GetPixel(SrcPixel, SrcColor);

      Alpha:= (SrcColor.Alpha / 255);

      DstColor          := PRGBTriple(DstPixel)^;
      DstColor.rgbtRed  := Trunc((DstColor.rgbtRed   * (1- Alpha) + SrcColor.Red   * (Alpha)) * Mask);
      DstColor.rgbtGreen:= Trunc((DstColor.rgbtGreen * (1- Alpha) + SrcColor.Green * (Alpha)) * Mask);
      DstColor.rgbtBlue := Trunc((DstColor.rgbtBlue  * (1- Alpha) + SrcColor.Blue  * (Alpha)) * Mask);

      PRGBTriple(DstPixel)^:= DstColor;

      Inc(DstPixel, 3);//  pf32bit
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.DrawPattern(Dest: TCanvas; Buffer: TBitmap; X,Y: Integer; PatternIndex: Integer);
var SrcRect: TRect;
var DstRect: TRect;
begin
  if (PatternIndex < 0) or (PatternIndex >= Patterns.Count) then Exit;

  SrcRect.Left  := Patterns[PatternIndex].X;
  SrcRect.Top   := Patterns[PatternIndex].Y;
  SrcRect.Right := Patterns[PatternIndex].X + Patterns[PatternIndex].Width;
  SrcRect.Bottom:= Patterns[PatternIndex].Y + Patterns[PatternIndex].Height;

  DstRect.Left  :=  X;
  DstRect.Top    := Y;
  DstRect.Right  := X + Patterns[PatternIndex].Width;
  DstRect.Bottom := Y + Patterns[PatternIndex].Height;

  Dest.CopyRect(DstRect, Buffer.Canvas, SrcRect);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.DrawPattern(Dest: TCanvas; Buffer: TBitmap; X,Y: Integer; PatternIndex: Integer; Zoom: Single);
var SrcRect: TRect;
var DstRect: TRect;
begin
  if (PatternIndex < 0) or (PatternIndex >= Patterns.Count) then Exit;

  SrcRect.Left  := Patterns[PatternIndex].X;
  SrcRect.Top   := Patterns[PatternIndex].Y;
  SrcRect.Right := Patterns[PatternIndex].X + Patterns[PatternIndex].Width;
  SrcRect.Bottom:= Patterns[PatternIndex].Y + Patterns[PatternIndex].Height;

  DstRect.Left  :=  X;
  DstRect.Top    := Y;
  DstRect.Right  := X + Round(Patterns[PatternIndex].Width  * Zoom);
  DstRect.Bottom := Y + Round(Patterns[PatternIndex].Height * Zoom);

  Dest.CopyRect(DstRect, Buffer.Canvas, SrcRect);
end;


//------------------------------------------------------------------------------
procedure TPHXImageEx.SavePattern(const Filename: String; Pattern: TPHXPattern);
var Bitmap: TPHXBitmap;
begin
  Bitmap:= TPHXBitmap.Create;
  try
    Bitmap.Resize(Pattern.Width, Pattern.Height, Texture.Format);
   // Bitmap.CopyFrom(Texture.Graphic, TRecti.Create(Pattern.X, Pattern.Y, Pattern.X + Pattern.Width, Pattern.Y + Pattern.Height), TVector2i.Create(0,0) );

    Bitmap.CopyFrom(Texture.Graphic, TRecti.Create(Pattern.X, Pattern.Y, Pattern.X + Pattern.Width, Pattern.Y + Pattern.Height), TVector2i.Create(0,0) );

    Bitmap.SaveBitmap(Filename);
  finally
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.SavePattern(const Filename: String; PatternIndex: Integer);
begin
  SavePattern(Filename, Patterns.list^[PatternIndex] );
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadPattern(const Filename: String; Pattern: TPHXPattern);
var Bitmap: TPHXBitmap;
begin
  Bitmap:= TPHXBitmap.Create;
  try
    Bitmap.LoadBitmap(Filename);

    if (Bitmap.Width <> Pattern.Width) or (Bitmap.Height <> Pattern.Height)  then
    begin
      MessageDlg('To import an pattern the size of the image must be the same size as the pattern.', mtError, [mbOk], 0);
    end else
    begin
      Bitmap.CopyTo(Texture.Graphic, TRectI.Create(0, 0, Pattern.Width, Pattern.Height), TVector2i.Create(Pattern.X, Pattern.Y) );
    end;


  finally
    Bitmap.Free;
  end;
end;

// The image file header.
//------------------------------------------------------------------------------
Type TPHXImageHeader = record
  // The id of the image file, should always be PHXIMG.
  Ident: Array[1..6] of AnsiChar;
  // The file version.
  Version: Integer;
end;

//------------------------------------------------------------------------------
class function TPHXImageEx.ReadVersion(const FileName: String): Integer;
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
class function TPHXImageEx.ReadVersion(const Stream: TStream): Integer;
var Header     : TPHXImageHeader;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXIMG') then
  begin
    {$IFDEF LOG_ENABLED}
    TPHXLogger.getInstance.Log(logSevere, 'TPHXImage.LoadFromStream', 'Not a valid Phoenix image.');
    {$ELSE}
    raise Exception.Create('Not a valid Phoenix image.');
    {$ENDIF}
    Exit;
  end;

  Result:= Header.Version;
end;
                         (*
//------------------------------------------------------------------------------
procedure TPHXImageTools.LoadVersion4(const FileName: String);
var Stream: TStream;
begin
  Stream:=TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadVersion4(Stream);
  finally
    Stream.Free;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXImageTools.LoadVersion4(const Stream  : TStream);
var Header     : TPHXImageHeader;
var AName    : String;
var AWidth   : Integer;
var AHeight  : Integer;
begin
  Header.Ident  := #0#0#0#0#0#0;
  Header.Version:= 0;

  Stream.Read(Header.Ident  , SizeOf(Header.Ident));
  Stream.Read(Header.Version, SizeOf(Header.Version));

  If (Header.Ident <> 'PHXIMG') then
  begin
    {$IFDEF LOG_ENABLED}
    TPHXLogger.getInstance.Log(logSevere, 'TPHXImage.LoadFromStream', 'Not a valid Phoenix image.');
    {$ELSE}
    raise Exception.Create('Not a valid Phoenix image.');
    {$ENDIF}
    Exit;
  end;

  If (Header.Version <> 4) then
  begin
    {$IFDEF LOG_ENABLED}
    TPHXLogger.getInstance.Log(logSevere, 'TPHXImage.LoadFromStream', 'Image version missmatch [File: %d Code: %d].', [Header.Version, PHXIMAGE_VERSION]);
    {$ELSE}
    raise Exception.CreateFmt('Image version missmatch [File: %d Code: %d].', [Header.Version, PHXIMAGE_VERSION]);
    {$ENDIF}
    Exit;
  end;

  // The name of the image.
  AName:= String( ReadStr(Stream) );
  // The width of the image
  Stream.Read(AWidth, SizeOf(AWidth));
  // The height of the image
  Stream.Read(AHeight, SizeOf(AHeight));

  Name:= AName;
  Width:= AWidth;
  Height:= AHeight;

  // Load the patterns
  Patterns.LoadFromStream(Stream);

 // Tags.LoadFromStream(Stream);

  // Load the texture
  Texture.LoadFromStream(Stream);


  Initialize;
end;
*)

{$ENDREGION}


{$REGION 'TPHXAnimationEx'}

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.SaveToXML(const FileName: String);
var Document: IXMLDocument;
var Root    : IXMLNode;
var Node    : IXMLNode;
begin
  Document:= NewXMLDocument;
  Document.Options:= Document.Options + [doNodeAutoIndent];

  Root:= Document.AddChild('phxAnimation');
  Root.Attributes['Version']:= PHXANIMATION_VERSION;

  Node:= Root.AddChild('Animation');
  begin
    Node.Attributes['Name'     ]:= Name;
    Node.Attributes['Author'   ]:= Author;
    Node.Attributes['Version'  ]:= Version;
    Node.Attributes['Comment'  ]:= Comment;
    Node.Attributes['Image'    ]:= ImageName;
    Node.Attributes['Looped'   ]:= Looped;
    Node.Attributes['FrameRate']:= FrameRate;
  end;

  Node:= Root.AddChild('Frames');
  begin
    SaveFrames(Node);
  end;

  Document.SaveToFile(FileName);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.LoadFromXML(const FileName: String);
var Document: IXMLDocument;
var Root    : IXMLNode;
var Node    : IXMLNode;
begin
  Document:= LoadXMLDocument(FileName);

  Root:= Document.DocumentElement;

  if Root.NodeName <> 'phxAnimation' then
  begin
    MessageDlg( Format('The file "%s" is not a valid phoenix animation', [FileName]), mtError, [mbOK], 0);

    Exit;
  end;

  Node:= Root.ChildNodes.FindNode('Animation');
  if Assigned(Node) then
  begin
    if Node.HasAttribute('Name'     ) then Name     := Node.Attributes['Name'   ];
    if Node.HasAttribute('Author'   ) then Author   := Node.Attributes['Author' ];
    if Node.HasAttribute('Version'  ) then Version  := Node.Attributes['Version'];
    if Node.HasAttribute('Comment'  ) then Comment  := Node.Attributes['Comment'];
    if Node.HasAttribute('Image'    ) then ImageName:= Node.Attributes['Image'];
    if Node.HasAttribute('Looped'   ) then Looped := Node.Attributes['Looped'];
    if Node.HasAttribute('FrameRate') then FrameRate:= Node.Attributes['FrameRate'];
  end;

  Node:= Root.ChildNodes.FindNode('Frames');
  if Assigned(Node) then
  begin
    LoadFrames(Node);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.SaveFrames(Parent: IXMLNode);
var Index: Integer;
var Frame: TPHXAnimationFrame;
var Node : IXMLNode;
begin
  for Index:= 0 to Frames.Count - 1 do
  begin
    Frame:= Frames[Index];

    Node:= Parent.AddChild('Frame');
    Node.Attributes['Name'    ]:= Frame.Name;
    Node.Attributes['Time'    ]:= Frame.Time;
    Node.Attributes['Pattern' ]:= Frame.Pattern;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.LoadFrames(Parent: IXMLNode);
var Index: Integer;
var Frame: TPHXAnimationFrame;
var Node : IXMLNode;
begin
  Frames.Clear;
  for Index := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node:= Parent.ChildNodes[Index];

    if Node.NodeName <> 'Frame' then Continue;

    if Node.HasAttribute('Name')    then Frame.Name    := ShortString(Node.Attributes['Name']);
    if Node.HasAttribute('Time')    then Frame.Time   :=              Node.Attributes['Time'];
    if Node.HasAttribute('Pattern') then Frame.Pattern :=             Node.Attributes['Pattern'];

    Frames.Add(Frame);
  end;
end;


{$ENDREGION}


end.

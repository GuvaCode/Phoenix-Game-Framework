unit phxImageEx;

{$MODE Delphi}

interface

uses Classes, Types, SysUtils, Graphics, Dialogs,
     Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite,
     LCLIntf, LCLType,
     phxTypes,
     phxMath,
     phxGraphics,
     phxGraphicsEx,
     phxImage;

type

//------------------------------------------------------------------------------
TPHXImageEx = class helper for TPHXImage
  private
    procedure SavePatterns(Document: TXMLDocument; Parent: TDOMNode);
    procedure SaveTags(Document: TXMLDocument; Parent: TDOMNode);
    procedure SaveTexture(const FileName: String) ;

    procedure LoadPatterns(Parent: TDOMNode);
    procedure LoadTags(Parent: TDOMNode);
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

{ TPHXAnimationEx }

TPHXAnimationEx = class helper for TPHXAnimation
  private
    procedure LoadFrames(Parent: TDOMNode);
    procedure SaveFrames(Document: TXMLDocument; Parent: TDOMNode);
  public
    // Load the animation from a xml file
    procedure LoadFromXML(const FileName: String);
    // Load the animation from a xml file
    procedure SaveToXML(const FileName: String);
     // Reset a animation state to the default value
    procedure Reset(var State: TPHXAnimationState );
    // Update a animation state
    procedure Update(var State: TPHXAnimationState; FrameTime: Single);
    // Draw the animation at a coordinate
    procedure Draw(const State: TPHXAnimationState; const X, Y: Integer); overload;
    // Draw the animation using a transformation matrix
    procedure Draw(const State: TPHXAnimationState; const Transform: TMatrix4f); overload;
  end;


implementation


{$REGION 'TPHXImageEx'}

//------------------------------------------------------------------------------
procedure TPHXImageEx.SaveToXML(const FileName: String);
var Document: TXMLDocument;
var Root    : TDOMNode;
var Node    : TDOMNode;

begin
  Document := TXMLDocument.Create;
  // создаем корневой узел
  Root := Document.CreateElement('phxImage');
  TDOMElement(Root).SetAttribute('Version', IntToStr(PHXIMAGE_VERSION));
  Document.Appendchild(Root);
  Root:= Document.DocumentElement;

  Node:= Document.CreateElement('Image');
  TDOMElement(Node).SetAttribute('Name', Name);
  TDOMElement(Node).SetAttribute('Author', Author);
  TDOMElement(Node).SetAttribute('Version', Version);
  TDOMElement(Node).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Node).SetAttribute('Height', IntToStr(Height));
  Document.Appendchild(Root);

  Node:= Document.CreateElement('Patterns');
  begin
    SavePatterns(Document, Node);
  end;
  Document.Appendchild(Root);

  Node:= Document.CreateElement('Patterns');
  begin
    SaveTags(Document, Node);
  end;
  Document.Appendchild(Root);

  WriteXMLFile(Document, Filename);

  SaveTexture(ChangeFileExt(FileName, '.png'));
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadFromXML(const FileName: String);
var Document: TXMLDocument;
var Root    : TDOMNode;
var Node    : TDOMNode;
begin
 { Document:= LoadXMLDocument(FileName);

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
  }
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
procedure TPHXImageEx.SavePatterns(Document: TXMLDocument; Parent: TDOMNode);
var Index  : Integer;
var Pattern: TPHXPattern;
var Node   : TDOMNode;
begin
  for Index:= 0 to Patterns.Count - 1 do
  begin
    Pattern:= Patterns[Index];
    Node:= Document.CreateElement('Pattern');
    TDOMElement(Node).SetAttribute('Name', Pattern.Name);
    TDOMElement(Node).SetAttribute('X', IntToStr(Pattern.X));
    TDOMElement(Node).SetAttribute('Y', IntToStr(Pattern.Y));
    TDOMElement(Node).SetAttribute('Width', IntToStr(Pattern.Width));
    TDOMElement(Node).SetAttribute('Height', IntToStr(Pattern.Height));
    TDOMElement(Node).SetAttribute('Pivot.X', IntToStr(Pattern.Pivot.X));
    TDOMElement(Node).SetAttribute('Pivot.Y', IntToStr(Pattern.Pivot.Y));
    Document.Appendchild(Node);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.SaveTags(Document: TXMLDocument; Parent: TDOMNode);
var Index  : Integer;
var Tag    : TPHXTag;
var Node   : TDOMNode;
begin
  for Index:= 0 to Tags.Count - 1 do
  begin
    Tag:= Tags[Index];
    Node:= Document.CreateElement('Pattern');
    TDOMElement(Node).SetAttribute('Name', Tag.Name);
    TDOMElement(Node).SetAttribute('Pattern', FloatToStr(Tag.Pattern));
    TDOMElement(Node).SetAttribute('X', FloatToStr(Tag.X));
    TDOMElement(Node).SetAttribute('Y', FloatToStr(Tag.Y));
    TDOMElement(Node).SetAttribute('Rotation', FloatToStr(Tag.Rotation));
    Document.Appendchild(Node);
    end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadTexture(const FileName: String);
begin
  Texture.LoadTexture(FileName);
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadPatterns(Parent: TDOMNode);
var Index  : Integer;
var Pattern: TPHXPattern;
var Node   : TDOMNode;
begin
  Patterns.Clear;

  for Index := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node := Parent.ChildNodes[Index];
    if Node.NodeName <> 'Pattern' then Continue;

    if TDOMElement(Node).hasAttribute('Name') then
    Pattern.Name := Node.Attributes.GetNamedItem('Name').NodeValue;

    if TDOMElement(Node).hasAttribute('X') then
    Pattern.X := StrToInt(Node.Attributes.GetNamedItem('X').NodeValue);

    if TDOMElement(Node).hasAttribute('Y') then
    Pattern.Y := StrToInt(Node.Attributes.GetNamedItem('Y').NodeValue);

    if TDOMElement(Node).hasAttribute('Width') then
    Pattern.Width := StrToInt(Node.Attributes.GetNamedItem('Width').NodeValue);

    if TDOMElement(Node).hasAttribute('Height') then
    Pattern.Height := StrToInt(Node.Attributes.GetNamedItem('Height').NodeValue);

    if TDOMElement(Node).hasAttribute('Pivot.X') then
    Pattern.Pivot.X := StrToInt(Node.Attributes.GetNamedItem('Pivot.X').NodeValue);

    if TDOMElement(Node).hasAttribute('Pivot.Y') then
    Pattern.Pivot.Y := StrToInt(Node.Attributes.GetNamedItem('Pivot.Y').NodeValue);

    {

    if Node.HasAttribute('Name')    then Pattern.Name    := ShortString(Node.Attributes['Name']);
    if Node.HasAttribute('X')       then Pattern.X       :=             Node.Attributes['X'];
    if Node.HasAttribute('Y')       then Pattern.Y       :=             Node.Attributes['Y'];
    if Node.HasAttribute('Width')   then Pattern.Width   :=             Node.Attributes['Width'];
    if Node.HasAttribute('Height')  then Pattern.Height  :=             Node.Attributes['Height'];
    if Node.HasAttribute('Pivot.X') then Pattern.Pivot.X :=             Node.Attributes['Pivot.X'];
    if Node.HasAttribute('Pivot.Y') then Pattern.Pivot.Y :=             Node.Attributes['Pivot.Y'];
      }
    Patterns.Add(Pattern);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXImageEx.LoadTags(Parent: TDOMNODE);
var Index  : Integer;
var Tag    : TPHXTag;
var Node   : TDOMNode;
begin
  Tags.Clear;
  for Index := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node:= Parent.ChildNodes[Index];

    if Node.NodeName <> 'Tag' then Continue;

    if TDOMElement(Node).hasAttribute('Name') then
    Tag.Name := Node.Attributes.GetNamedItem('Name').NodeValue;

    if TDOMElement(Node).hasAttribute('Pattern') then
    Tag.Pattern := StrToInt(Node.Attributes.GetNamedItem('Pattern').NodeValue);

    if TDOMElement(Node).hasAttribute('X') then
    Tag.X := StrToFloat(Node.Attributes.GetNamedItem('X').NodeValue);

    if TDOMElement(Node).hasAttribute('Y') then
    Tag.Y := StrToFloat(Node.Attributes.GetNamedItem('Y').NodeValue);

    if TDOMElement(Node).hasAttribute('Rotation') then
    Tag.Y := StrToFloat(Node.Attributes.GetNamedItem('Rotation').NodeValue);


    {
    if Node.HasAttribute('Name')     then Tag.Name    := ShortString(Node.Attributes['Name']);
    if Node.HasAttribute('Pattern')  then Tag.Pattern :=             Node.Attributes['Pattern'];
    if Node.HasAttribute('X')        then Tag.X       :=             Node.Attributes['X'];
    if Node.HasAttribute('Y')        then Tag.Y       :=             Node.Attributes['Y'];
    if Node.HasAttribute('Rotation') then Tag.Rotation:=             Node.Attributes['Rotation'];
    }
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

      Inc(DstPixel, 4);//  pf32bit
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
  Dest.BeginUpdate();
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
      Inc(DstPixel, 4);//  pf24bit
    end;
  end;
  Dest.EndUpdate();
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

      Inc(DstPixel, 4);
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
  Dest.BeginUpdate();
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

      Inc(DstPixel, 4);//  pf32bit
    end;
  end;
  Dest.EndUpdate();
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

{$REGION 'TPHXAnimationEx'}

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.SaveToXML(const FileName: String);
var Document: TXMLDocument;
var Root    : TDOMNode;
var Node    : TDOMNode;
begin
  {
    Document := TXMLDocument.Create;
  // создаем корневой узел
  Root := Document.CreateElement('phxImage');
  TDOMElement(Root).SetAttribute('Version', IntToStr(PHXIMAGE_VERSION));
  Document.Appendchild(Root);
  Root:= Document.DocumentElement;

  Node:= Document.CreateElement('Image');
  TDOMElement(Node).SetAttribute('Name', Name);
  TDOMElement(Node).SetAttribute('Author', Author);
  TDOMElement(Node).SetAttribute('Version', Version);
  TDOMElement(Node).SetAttribute('Width', IntToStr(Width));
  TDOMElement(Node).SetAttribute('Height', IntToStr(Height));
  Document.Appendchild(Root);

  Node:= Document.CreateElement('Patterns');
  begin
    SavePatterns(Document, Node);
  end;
  Document.Appendchild(Root);     }

  Document := TXMLDocument.Create;
  Root := Document.CreateElement('phxAnimation');
  TDOMElement(Root).SetAttribute('Version', IntToStr(PHXANIMATION_VERSION));
  Document.Appendchild(Root);
  Root:= Document.DocumentElement;

  Node:= Document.CreateElement('Animation');
  TDOMElement(Node).SetAttribute('Name', Name);
  TDOMElement(Node).SetAttribute('Author', Author);
  TDOMElement(Node).SetAttribute('Version', Version);
  TDOMElement(Node).SetAttribute('Comment', Comment);
  TDOMElement(Node).SetAttribute('Looped', BoolToStr(Looped));
  TDOMElement(Node).SetAttribute('FrameRate', IntToStr(FrameRate));
  Document.Appendchild(Root);


  Node:= Document.CreateElement('Frames');
  begin
    SaveFrames(Document, Node);
  end;
  Document.Appendchild(Root);
  //Document.SaveToFile(FileName);
end;

procedure TPHXAnimationEx.Reset(var State: TPHXAnimationState);
begin
  State.Finished:= False;
  State.Active  := True;
  State.Time    := 0;
  State.Frame   := 0;

  if Frames.Count > 0 then
  begin
    State.Pattern:= Frames.List^[0].Pattern;
  end else
  begin
    State.Pattern:= -1;
  end;
end;

procedure TPHXAnimationEx.Update(var State: TPHXAnimationState;
  FrameTime: Single);
begin
  //  Only update if active
  if (State.Active) and (Frames.Count > 0) then
  begin
    // Add the time to the state
    State.Time := State.Time + FrameTime;

    // test if we shall change to the next frame
    if( State.Time > Frames[State.Frame].Time ) then
    begin
      State.Time:= State.Time - Frames[State.Frame].Time;

      Inc(State.Frame);

      // Test if we reached the end of the animation
      if( State.Frame >= Frames.Count) then
      begin

        // Check if looped
        if Looped then
        begin
          State.Time    := 0;
          State.Frame   := 0;
        end else
        begin
          State.Frame   := Frames.Count-1;
          State.Active  := False;
          State.Finished:= True;
        end;
      end;

      // Update the pattern index
      State.Pattern := Frames.List^[State.Frame].Pattern;
    end;
  end;
end;

procedure TPHXAnimationEx.Draw(const State: TPHXAnimationState; const X,
  Y: Integer);
begin
  Assert( Assigned(Image ) );
  Image.Draw(X, Y, Frames[State.Frame].Pattern);
end;

procedure TPHXAnimationEx.Draw(const State: TPHXAnimationState;
  const Transform: TMatrix4f);
begin
  Assert( Assigned(Image ) );
  Image.DrawTransform(Transform, Frames[State.Frame].Pattern);
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.LoadFromXML(const FileName: String);
var Document: TXMLDocument;
var Root    : TDOMNode;
var Node    : TDOMNode;
begin
  {
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
  end; }
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.SaveFrames(Document: TXMLDocument;  Parent: TDOMNode);
var Index: Integer;
var Frame: TPHXAnimationFrame;
var Node : TDOMNode;
begin
  {
   Pattern:= Patterns[Index];
    Node:= Document.CreateElement('Pattern');
    TDOMElement(Node).SetAttribute('Name', Pattern.Name);
    TDOMElement(Node).SetAttribute('X', IntToStr(Pattern.X));
    TDOMElement(Node).SetAttribute('Y', IntToStr(Pattern.Y));
    TDOMElement(Node).SetAttribute('Width', IntToStr(Pattern.Width));
    TDOMElement(Node).SetAttribute('Height', IntToStr(Pattern.Height));
    TDOMElement(Node).SetAttribute('Pivot.X', IntToStr(Pattern.Pivot.X));
    TDOMElement(Node).SetAttribute('Pivot.Y', IntToStr(Pattern.Pivot.Y));
    Document.Appendchild(Node);
    }
  for Index:= 0 to Frames.Count - 1 do
  begin
    Frame:= Frames[Index];
    Node:= Document.CreateElement('Pattern');
    TDOMElement(Node).SetAttribute('Name', Frame.Name);
    TDOMElement(Node).SetAttribute('Time', FloatToStr(Frame.Time));
    TDOMElement(Node).SetAttribute('Pattern', IntToStr(Frame.Pattern));
    Document.Appendchild(Node);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXAnimationEx.LoadFrames(Parent: TDOMNode);
var Index: Integer;
var Frame: TPHXAnimationFrame;
var Node : TDOMNode;
begin
  Frames.Clear;
  for Index := 0 to Parent.ChildNodes.Count - 1 do
  begin
    Node:= Parent.ChildNodes[Index];

    if Node.NodeName <> 'Frame' then Continue;

    
    if TDOMElement(Node).hasAttribute('Name') then
    Frame.Name := Node.Attributes.GetNamedItem('Name').NodeValue;


    if TDOMElement(Node).hasAttribute('Time') then
    Frame.Time := StrTOFloat(Node.Attributes.GetNamedItem('Time').NodeValue);

    if TDOMElement(Node).hasAttribute('Pattern') then
    Frame.Pattern := StrToInt(Node.Attributes.GetNamedItem('Pattern').NodeValue);


{
    if Node.HasAttribute('Name')    then Frame.Name    := ShortString(Node.Attributes['Name']);
    if Node.HasAttribute('Time')    then Frame.Time   :=              Node.Attributes['Time'];
    if Node.HasAttribute('Pattern') then Frame.Pattern :=             Node.Attributes['Pattern'];
 }
    Frames.Add(Frame);
  end;
end;


{$ENDREGION}


end.

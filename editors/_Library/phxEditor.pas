unit phxEditor;
{$mode Delphi}
interface

uses
  SysUtils, Classes, Controls,  LCLType, LCLIntf, Graphics, Types, Messages, LMessages, Math,

  ComCtrls,  StdCtrls,  ExtCtrls,  Forms, ClipBrd,

  Generics.Collections,

  {XMLIntf,}

  phxTypes,
  phxGraphics,
  phxGraphicsEx;

type

//{$DEFINE SHOW_DEBUG}

TFileEvent = procedure(Sender: TObject; const FileName: String) of object;

TPHXEditor = class;
TPHXEditorTools = class;

{$REGION 'TPHXEditorGrid'}

//------------------------------------------------------------------------------
TPHXGridStyle =  (
  gsLine,
  gsLineDotted,
  gsDots
);

//------------------------------------------------------------------------------
TPHXEditorGrid = class(TPersistent)
  private
    FEditor : TPHXEditor;

    FVisible: Boolean;
    FEnabled: Boolean;
    FColor  : TColor;
    FSize   : TVector2i;
    FStyle  : TPHXGridStyle;

    procedure DrawLine      (Canvas: TCanvas; const Rect: TRect); overload;
    procedure DrawDots      (Canvas: TCanvas; const Rect: TRect); overload;
    procedure DrawLineDotted(Canvas: TCanvas; const Rect: TRect); overload;

    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: TVector2i);
    procedure SetStyle(const Value: TPHXGridStyle);
  protected
    procedure Changed;
  public
    constructor Create(Editor: TPHXEditor);

    procedure Draw(Canvas: TCanvas   ; const Rect: TRect); overload;

  //  procedure SaveToXML(Node: IXMLNode);
  //  procedure LoadFromXml(Node: IXMLNode);

    procedure Snap(var Position: TVector2i); overload;
    procedure Snap(var Position: TVector2f); overload;

    function SnapX(const X: Integer): Integer; overload;
    function SnapX(const X: Single ): Single; overload;
    function SnapY(const Y: Integer): Integer; overload;
    function SnapY(const Y: Single ): Single; overload;

    procedure Assign(Source: TPersistent); override;

    property Editor: TPHXEditor read FEditor;
    property Size   : TVector2i read FSize write SetSize;
  published
    // If the grid is visible
    property Visible: Boolean read FVisible write SetVisible;
    // If the grid is enabled (snap to grid)
    property Enabled: Boolean read FEnabled write SetEnabled;
    // Width of the grid
    property Width: Integer read FSize.X write SetWidth;
    // Height of the grid
    property Height: Integer read FSize.Y write SetHeight;
    // Grid line color
    property Color: TColor read FColor write SetColor;
    // Grid line style
    property Style: TPHXGridStyle read FStyle write SetStyle;
  end;

{$ENDREGION}

{$REGION 'TPHXEditorViewport'}

// Todo:
// FCenter: Center position in the viewport
// FZoom  : Zoom factor
//------------------------------------------------------------------------------
TPHXEditorViewport = class(TPersistent)
  private
    FOwner: TPHXEditor;
    // The zoom level, 2.0 is 200%
    FZoom  : Single;
    // Center position in document coordinates
    FCenter: TVector2f;

    function GetViewportRect: TRectf;

    function GetOffset: TVector2i;

    procedure SetZoom   (const Value: Single);
    procedure SetCenter(const Value: TVector2f);
    procedure SetCenterX(const Value: Single);
    procedure SetCenterY(const Value: Single);
  public
    constructor Create(AOwner: TPHXEditor);

    // Converts a document coordinate to a screen coordinate
    function DocumentToScreen(const Point: TVector2f): TVector2f;
    // Converts an screen coordinate to an document coordinate
    function ScreenToDocument(const Point: TVector2f): TVector2f;

    procedure ClipViewport;

    procedure ScrollToCenter;

    Procedure ZoomIn; overload;
    // Zoom in on a point in document coordinates
    Procedure ZoomIn(const X,Y: Integer); overload;

    Procedure ZoomOut;overload;
    // Zoom out on a point in document coordinates
    Procedure ZoomOut(const X,Y: Integer); overload;

    Procedure Zoom100;

    // Zoom to fit a rect in document coordinates
    Procedure ZoomRect(Rect: TRecti);
    Procedure ZoomFit;

    // Returns a rectangle of the current zoomed angle
    Property ViewportRect: TRectf read GetViewportRect;

    // The owning editor
    property Owner: TPHXEditor read FOwner;
    // Center point of the viewport, in document coordinates
    Property Center: TVector2f read FCenter  write SetCenter;

    // Return the document offset depending on the center position and zoom level
    Property Offset: TVector2i read GetOffset;
  published
    // The zoom level of the editor
    Property Zoom: Single    read FZoom write SetZoom;
    // Horisontal center position
    Property CenterX: Single read FCenter.X  write SetCenterX;
    // Vertical center position
    Property CenterY: Single read FCenter.Y  write SetCenterY;
  end;

{$ENDREGION}

{$REGION 'TPHXEditor'}

//------------------------------------------------------------------------------
TEditorOptions = set of (
  doInvalidate,
  doTransparent,
  doMouseScroll,
  doTooltip,
  // Allow dropping files
  doAcceptFiles
);

//------------------------------------------------------------------------------
TPHXEditor = class(TCustomControl)
  private
    FViewport: TPHXEditorViewport;
    FGrid    : TPHXEditorGrid;
    FTools   : TPHXEditorTools;
    FOptions : TEditorOptions;

    FBorderStyle: TBorderStyle;

    MousePos: TVector2i;

    FHoverTimer  : TTimer;
    FHoverVisible: Boolean;

    FOnCustomPaint: TNotifyEvent;
    FOnFileDropped: TFileEvent;

    // Clips the viewport so the image is shown
    procedure ClipViewport;

    Procedure WMVScroll(var Message: TWMSCROLL ); message WM_VSCROLL;
    Procedure WMHScroll(var Message: TWMSCROLL ); message WM_HSCROLL;

    Procedure HandleScrollbar(var msg: TWMSCROLL; bar: Integer );
    procedure UpdateScrollBar;

    procedure ShowHover(Sender: TObject);

    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetTools(const Value: TPHXEditorTools);
  protected
    procedure Click; override;
    procedure Loaded; override;
    procedure Resize; override;

    procedure CreateParams(var Params: TCreateParams); override;

    // Returns the size of the document
    Procedure GetDocumentSize(out Width: Integer; out Height: Integer); virtual; abstract;

    // paint the editor control
    procedure Paint; override;
    // Paint the document
    procedure PaintDocument(const Offset: TVector2i; const Zoom: Single); virtual; abstract;

    // Notify the editor that the document is changef
    procedure DocumentChanged;
    {
   // procedure WMMouseWheel(var Message: {TWMMouseWheel} //TCMMouseWheel); message LM_MOUSEWHEEL;

    Procedure WMGetDlgCode(var Message: TWMGetDlgCode ); message WM_GETDLGCODE;
    /// todo procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;

   // procedure WMRButtonDown(var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMMouseMove  (var Message: TWMMouseMove  ); message WM_MOUSEMOVE;
    procedure CMMouseLeave  (var Message: TMessage); message CM_MOUSELEAVE;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;

    procedure ScrollToCenter;

    // Converts a image coordinate to a screen coordinate
    Function DocumentToScreen(const X, Y : Integer  ): TVector2i; overload;
    Function DocumentToScreen(const X, Y : Single   ): TVector2f; overload;
    Function DocumentToScreen(const Point: TVector2i): TVector2i; overload;
    Function DocumentToScreen(const Point: TVector2f): TVector2f; overload;
    Function DocumentToScreen(const Point: TVector3f): TVector3f; overload;

    // Converts an screen coordinate to an image coordinate
    Function ScreenToDocument(const X, Y : Integer  ): TVector2i; overload;
    Function ScreenToDocument(const X, Y : Single   ): TVector2f; overload;
    Function ScreenToDocument(const Point: TVector2i): TVector2i; overload;
    Function ScreenToDocument(const Point: TVector2f): TVector2f; overload;
    Function ScreenToDocument(const Point: TVector3f): TVector3f; overload;

    procedure DrawHover(const ScreenX, ScreenY: Integer; const Text: String);

    // Zoom the image
    Procedure DoZoom(Factor: Single); overload;
    // Zoom the image
    Procedure DoZoom(X,Y: Integer; Factor: Single); overload;
    // Scroll the image
    Procedure DoScroll(DX, DY: Integer);
    // Scroll with the mouse
    Procedure MouseScroll(MouseX, MouseY: Integer; Enabled: Boolean);


    // The center position of the image
    Property Grid: TPHXEditorGrid read FGrid;
    property Tools: TPHXEditorTools read FTools write SetTools;
  published
    property Options: TEditorOptions read FOptions write FOptions;
    // The zoom level of the editor
    Property Viewport: TPHXEditorViewport read FViewport write FViewport;

    Property OnCustomPaint: TNotifyEvent read FOnCustomPaint write FOnCustomPaint;

    property OnFileDropped: TFileEvent read FOnFileDropped write FOnFileDropped;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;

    property Anchors;
    property Align;
    property Canvas;

    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
  end;

{$ENDREGION}

//------------------------------------------------------------------------------
TPHXEditorTool = class(TObject)
  private
    FTools : TPHXEditorTools;
    FActive: Boolean;

    FButton: TToolButton;

    function GetEditor: TPHXEditor;

    procedure ButtonClicked(Sender: TObject);
    procedure SetButton(const Value: TToolButton);
    procedure SetActive(const Value: Boolean);
  protected
    function GetName: String; virtual; abstract;
    function GetIdent: Cardinal; virtual; abstract;
    function GetEnabled: Boolean; virtual;
  public
    constructor Create(ATools: TPHXEditorTools); overload; virtual;
    constructor Create(ATools: TPHXEditorTools; AButton: TToolButton); overload;
    destructor Destroy; override;

    // Called when the tool is started
    procedure Activated; virtual;
    // Called when the tool is finished
    procedure Deactivated; virtual;

    // Update the tool
    procedure Update;
    // Paint the tool
    procedure Paint(Canvas: TCanvas); virtual;
    // Show a hint text
    procedure ShowHint(const Text: String);

    // Called when the mouse is moved
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    // Called when the mouse is pressed
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    // Called when the mouse is released
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;

    // The owning editor
    property Editor: TPHXEditor read GetEditor;
    // The owning tool manager
    property Tools: TPHXEditorTools read FTools;
    // Name of the tool
    property Name: String read GetName;
    // Ident of the tool
    property Ident: Cardinal read GetIdent;
    // If the tool is enables
    property Enabled: Boolean read GetEnabled;
    // Returns if the tool is active
    property Active: Boolean read FActive write SetActive;
    // The toolbutton
    property Button: TToolButton read FButton write SetButton;
  end;

TPHXEditorToolClass = class of TPHXEditorTool;

//------------------------------------------------------------------------------
TPHXEditorHintEvent = procedure(Sender: TObject; const Hint: String) of object;

//------------------------------------------------------------------------------
TPHXEditorTools = class(TPersistent)
  private
    FEditor: TPHXEditor;
    FTools : TObjectList<TPHXEditorTool>;
    FActive: TPHXEditorTool;
    FTimer : TTimer;

    FOnHint: TPHXEditorHintEvent;

    procedure TimerEvent(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const Tool: TPHXEditorTool); overload;
    procedure Add(const Tool: TPHXEditorToolClass); overload;
    procedure Add(const Tool: TPHXEditorToolClass; Button: TToolButton); overload;

    // Change the tool
    procedure SetActive(const Tool: TPHXEditorTool); overload;
    // Change the tool by name
    procedure SetActive(const Name: String); overload;
    // Change the tool by ident
    procedure SetActive(const Ident: Cardinal); overload;

    // Return true if the tool is active
    function IsActive(const Tool: TPHXEditorTool): Boolean; overload;
    // Return true if the tool is active
    function IsActive(const Name: String): Boolean; overload;
    // Return true if the tool is active
    function IsActive(const Ident: Cardinal): Boolean; overload;

    // Paint the active tool
    procedure Paint(Canvas: TCanvas);
    // Show a hint text
    procedure ShowHint(const Text: String);

    procedure HandleMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure HandleKeyDown(var Key: Word; Shift: TShiftState);
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState);

    // The current, active tool
    property Active: TPHXEditorTool read FActive;
    // List of tools
    property Tools: TObjectList<TPHXEditorTool> read FTools;
    // The owning editor
    property Editor: TPHXEditor read FEditor write FEditor;

    // Hint event
    property OnHint: TPHXEditorHintEvent read FOnHint write FOnHint;
  end;

//procedure CopyStreamToClipboard(fmt: Cardinal; S: TStream);
//procedure CopyStreamFromClipboard(fmt: Cardinal; S: TStream);

procedure Register;

implementation
//uses cmem;
//uses ShellAPI;

//------------------------------------------------------------------------------
procedure Register;
begin
 // RegisterComponents('Phoenix', [TPHXEditor]);

  //RegisterComponents('Phoenix', [TPHXTool]);
  //RegisterComponents('Phoenix', [TPHXToolManager]);

end;

//------------------------------------------------------------------------------
{
procedure CopyStreamToClipboard(fmt: Cardinal; S: TStream);
var
  hMem: THandle;
  pMem: Pointer;
begin
  Assert(Assigned(S));
  S.Position := 0;
  hMem       := GlobalAlloc(GHND or GMEM_DDESHARE, S.Size);
  hMem       := GlobalAlloc(GHND or GMEM_DDESHARE, S.Size);
  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      try
        S.Read(pMem^, S.Size);
        S.Position := 0;
      finally
        GlobalUnlock(hMem);
      end;
      Clipboard.Open;
      try
        Clipboard.SetAsHandle(fmt, hMem);
      finally
        Clipboard.Close;
      end;
    end
    else
    begin
      GlobalFree(hMem);
      OutOfMemoryError;
    end;
  end
  else
    OutOfMemoryError;
end;} { CopyStreamToClipboard }

//------------------------------------------------------------------------------
{
procedure CopyStreamFromClipboard(fmt: Cardinal; S: TStream);
var
  hMem: THandle;
  pMem: Pointer;
begin
  Assert(Assigned(S));
  hMem := Clipboard.GetAsHandle(fmt);
  if hMem <> 0 then
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then
    begin
      try
        S.Write(pMem^, GlobalSize(hMem));
        S.Position := 0;
      finally
        GlobalUnlock(hMem);
      end;
    end
    else
      raise Exception.Create('CopyStreamFromClipboard: could not lock global handle ' +
        'obtained from clipboard!');
  end;
end; }{ CopyStreamFromClipboard }



{$REGION 'TPHXEditorGrid'}

//------------------------------------------------------------------------------
function GridStyleToString(const Value: TPHXGridStyle): String;
begin
  Result:= '';
  case Value of
    gsLine      : Result:= 'Line';
    gsLineDotted: Result:= 'LineDotted';
    gsDots      : Result:= 'Dots';
  end;
end;

//------------------------------------------------------------------------------
function StringToGridStyle(const Value: String): TPHXGridStyle;
var Index: TPHXGridStyle;
begin
  for Index := Low(TPHXGridStyle) to High(TPHXGridStyle) do
  begin
    if SameText(Value, GridStyleToString(Index) ) then
    begin
      Result:= Index;
      Exit;
    end;
  end;
  Result:= gsLine;
end;


// TPHXEditorGrid
//==============================================================================
constructor TPHXEditorGrid.Create(Editor: TPHXEditor);
begin
  FEditor := Editor;
  FEnabled:= False;
  FSize   := TVector2i.Create(32, 32);
  FColor  := clSilver;
  FStyle  := gsLineDotted;
end;

//------------------------------------------------------------------------------
{procedure TPHXEditorGrid.SaveToXML(Node: IXMLNode);
begin
  Node.Attributes['Visible']:= Visible;
  Node.Attributes['Enabled']:= Enabled;
  Node.Attributes['Width'  ]:= Width;
  Node.Attributes['Height' ]:= Height;
  Node.Attributes['Color'  ]:= Graphics.ColorToString(Color);
  Node.Attributes['Style'  ]:= GridStyleToString(Style);
end;}


//------------------------------------------------------------------------------
{procedure TPHXEditorGrid.LoadFromXml(Node: IXMLNode);
begin
  FVisible:=                            Node.Attributes['Visible'];
  FEnabled:=                            Node.Attributes['Enabled'];
  FSize.X :=                            Node.Attributes['Width'  ];
  FSize.Y :=                            Node.Attributes['Height' ];
  FColor  := Graphics.StringToColor    (Node.Attributes['Color'  ]);
  FStyle  :=          StringToGridStyle(Node.Attributes['Style'  ]);

  Changed;
end;}

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.DrawLine(Canvas: TCanvas; const Rect: TRect);
var X, Y: Integer;
begin
  Canvas.Pen  .Color:= FColor;
  Canvas.Pen  .Style:= Graphics.psSolid;
  Canvas.Brush.Style:= Graphics.bsClear;

  X:= Rect.Left;
  while X < Rect.Right do
  begin
    Canvas.MoveTo(X, Rect.Top    );
    Canvas.LineTo(X, Rect.Bottom );

    X:= X + Trunc(Size.X * Editor.Viewport.Zoom);
  end;

  Y:= Rect.Top;
  while Y < Rect.Bottom do
  begin
    Canvas.MoveTo(Rect.Left , Y);
    Canvas.LineTo(Rect.Right, Y);

    Y:= Y + Trunc(Size.Y * Editor.Viewport.Zoom);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.DrawLineDotted(Canvas: TCanvas; const Rect: TRect);
var X, Y: Integer;
begin
  Canvas.Pen  .Color:= FColor;
  Canvas.Pen  .Style:= Graphics.psDot;
  Canvas.Brush.Style:= Graphics.bsClear;

  X:= Rect.Left;
  while X < Rect.Right do
  begin
    Canvas.MoveTo(X, Rect.Top    );
    Canvas.LineTo(X, Rect.Bottom );

    X:= X + Trunc(Size.X * Editor.Viewport.Zoom);
  end;

  Y:= Rect.Top;
  while Y < Rect.Bottom do
  begin
    Canvas.MoveTo(Rect.Left , Y);
    Canvas.LineTo(Rect.Right, Y);

    Y:= Y + Trunc(Size.Y * Editor.Viewport.Zoom);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXEditorGrid.DrawDots(Canvas: TCanvas; const Rect: TRect);
var X, Y: Integer;
begin
  Canvas.Pen  .Color:= FColor;
  Canvas.Pen  .Style:= Graphics.psSolid;
  Canvas.Brush.Color:= FColor;
  Canvas.Brush.Style:= Graphics.bsSolid;

  Y:= Rect.Top;
  while Y < Rect.Bottom do
  begin
    X:= Rect.Left;
    while X < Rect.Right do
    begin
      Canvas.Rectangle(X-1, Y-1, X + 1, Y + 1);

      X:= X + Trunc(Size.X * Editor.Viewport.Zoom);
    end;
    Y:= Y + Trunc(Size.Y * Editor.Viewport.Zoom);
  end;
end;



//------------------------------------------------------------------------------
procedure TPHXEditorGrid.Draw(Canvas: TCanvas; const Rect: TRect);
begin
  // No grid
  if (Visible = False) or (Size.X <= 0) or (Size.Y <= 0) then Exit;

  case Style of
    gsLine:
    begin
      DrawLine(Canvas, Rect);
    end;
    gsLineDotted:
    begin
      DrawLineDotted(Canvas, Rect);
    end;
    gsDots:
    begin
      DrawDots(Canvas, Rect);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.Snap(var Position: TVector2i);
var ux: Single;
var uy: Single;
begin
  if Enabled and (Size.X > 0) and (Size.Y > 0) then
  begin
    //scale to units
    ux:= Position.X / Size.X;
    uy:= Position.Y / Size.Y;

    //find the closest unit and scale back
    Position.X:= Round(ux) * Size.X;
    Position.Y:= Round(uy) * Size.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.Snap(var Position: TVector2f);
var ux: Single;
var uy: Single;
begin
  if Enabled and (Size.X > 0) and (Size.Y > 0) then
  begin
    //scale to units
    ux:= Position.X / Size.X;
    uy:= Position.Y / Size.Y;

    //find the closest unit and scale back
    Position.X:= Round(ux) * Size.X;
    Position.Y:= Round(uy) * Size.Y;
  end;
end;


//------------------------------------------------------------------------------
function TPHXEditorGrid.SnapX(const X: Integer): Integer;
var ux: Single;
begin
  if Enabled and (Size.X > 0) and (Size.Y > 0) then
  begin
    //scale to units
    ux:= X / Size.X;

    //find the closest unit and scale back
    Result:= Round(ux) * Size.X;
  end else
  begin
    Result:= X;
  end;
end;

//------------------------------------------------------------------------------
function TPHXEditorGrid.SnapX(const X: Single): Single;
var ux: Single;
begin
  if Enabled and (Size.X > 0) and (Size.Y > 0) then
  begin
    //scale to units
    ux:= X / Size.X;

    //find the closest unit and scale back
    Result:= Round(ux) * Size.X;
  end else
  begin
    Result:= X;
  end;
end;

//------------------------------------------------------------------------------
function TPHXEditorGrid.SnapY(const Y: Integer): Integer;
var uy: Single;
begin
  if Enabled and (Size.X > 0) and (Size.Y > 0) then
  begin
    //scale to units
    uy:= Y / Size.Y;

    //find the closest unit and scale back
    Result:= Round(uy) * Size.Y;
  end else
  begin
    Result:= Y;
  end;
end;

//------------------------------------------------------------------------------
function TPHXEditorGrid.SnapY(const Y: Single): Single;
var uy: Single;
begin
  if Enabled and (Size.X > 0) and (Size.Y > 0) then
  begin
    //scale to units
    uy:= Y / Size.Y;

    //find the closest unit and scale back
    Result:= Round(uy) * Size.Y;
  end else
  begin
    Result:= Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.Assign(Source: TPersistent);
begin
  Assert( Source is TPHXEditorGrid);

  FVisible:= TPHXEditorGrid(Source).FVisible;
  FEnabled:= TPHXEditorGrid(Source).FEnabled;
  FColor  := TPHXEditorGrid(Source).FColor;
  FSize   := TPHXEditorGrid(Source).FSize;
  FStyle  := TPHXEditorGrid(Source).FStyle;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.Changed;
begin
  if Assigned(Editor) then Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetColor(const Value: TColor);
begin
  FColor := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;

  Changed;
end;



//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetSize(const Value: TVector2i);
begin
  FSize := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetVisible(const Value: Boolean);
begin
  FVisible := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetWidth(const Value: Integer);
begin
  FSize.X := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetHeight(const Value: Integer);
begin
  FSize.Y := Value;

  Changed;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorGrid.SetStyle(const Value: TPHXGridStyle);
begin
  FStyle := Value;

  Changed;
end;

{$ENDREGION}

{$REGION 'TPHXEditor'}

//------------------------------------------------------------------------------
procedure Swap(var A: Integer; var B: Integer);
var T: Integer;
begin
  T:= A;
  A:= B;
  B:= T;
end;

// TPHXEditorViewport
//==============================================================================
constructor TPHXEditorViewport.Create(AOwner: TPHXEditor);
begin
  FOwner := AOwner;
  FCenter:= TVector2f.Create(0,0 );
  FZoom  := 1.0;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ClipViewport;
var Size    :  TVector2i;
begin
  Owner.GetDocumentSize(Size.X, Size.Y);

  if (Zoom <> 0) then
  begin
    if CenterX > Size.X then FCenter.X:= Size.X;
    if CenterX < 0      then FCenter.X:= 0;

    if CenterY > Size.Y then FCenter.Y:= Size.Y;
    if CenterY < 0      then FCenter.Y:= 0;
  end;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ScrollToCenter;
var Size    :  TVector2i;
begin
  Owner.GetDocumentSize(Size.X, Size.Y);

  // Center the image
  FCenter.X:= Size.X * 0.5;
  FCenter.Y:= Size.Y * 0.5;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ZoomRect(Rect: TRecti);
var Zoom:  TVector2f;
begin
  // Empty rectangle
  if (Rect.Right = Rect.Left) or (Rect.Bottom = Rect.Top) then Exit;

  // Swap so the width is positive
  if (Rect.Right  < Rect.Left) then Swap(Rect.Right, Rect.Left);
  if (Rect.Bottom < Rect.Top ) then Swap(Rect.Bottom, Rect.Top);

  Zoom.X:= Owner.ClientWidth  / ( Rect.Right  - Rect.Left );
  Zoom.Y:= Owner.ClientHeight / ( Rect.Bottom - Rect.Top  );

  if Zoom.X > Zoom.Y then
  begin
    FZoom:= Zoom.Y;
  end else
  begin
    FZoom:= Zoom.X;
  end;

  FCenter.X:= Rect.Left + (Rect.Right  - Rect.Left) / 2;
  FCenter.Y:= Rect.Top  + (Rect.Bottom - Rect.Top ) / 2;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ZoomFit;
var Size:  TVector2i;
var Zoom:  TVector2f;
begin
  Owner.GetDocumentSize(Size.X, Size.Y);

  // Empty document
  if (Size.X = 0) or (Size.Y = 0) then Exit;

  Zoom.X:= (Owner.ClientWidth - 20) / Size.X;
  Zoom.Y:= (Owner.ClientHeight- 20) / Size.Y;

  if Zoom.X > Zoom.Y then
  begin
    FZoom:= Zoom.Y;
  end else
  begin
    FZoom:= Zoom.X;
  end;

  // Center the image
  FCenter.X:= Size.X * 0.5;
  FCenter.Y:= Size.Y * 0.5;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.Zoom100;
begin
  SetZoom(1.0);

  ScrollToCenter;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ZoomIn;
begin
  Owner.DoZoom(2.0);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ZoomIn(const X, Y: Integer);
begin
  FZoom    := FZoom * 2.0;
  FCenter.X:= X;
  FCenter.Y:= Y;

  ClipViewport;

  Owner.UpdateScrollBar;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ZoomOut;
begin
  Owner.DoZoom(0.5);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.ZoomOut(const X, Y: Integer);
begin
  FZoom    := FZoom * 0.5;
  FCenter.X:= X;
  FCenter.Y:= Y;

  ClipViewport;

  Owner.UpdateScrollBar;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;


//------------------------------------------------------------------------------
function TPHXEditorViewport.GetOffset: TVector2i;
var Size    :  TVector2i;
begin
  Owner.GetDocumentSize(Size.X, Size.Y);

//  Result.X:= Trunc( ((Size.X * 0.5) + FCenter.X) * Zoom);
//  Result.Y:= Trunc( ((Size.Y * 0.5) + FCenter.Y) * Zoom);
  Result.X:= Trunc( (FCenter.X - Size.X * 0.5) * Zoom);
  Result.Y:= Trunc( (FCenter.Y - Size.Y * 0.5) * Zoom);

//  Result.X:= Trunc( Owner.Width  * 0.5 - (FCenter.X - Size.X) * Zoom);
//  Result.Y:= Trunc( Owner.Height * 0.5 - (FCenter.Y - Size.Y) * Zoom);

  Result.X:= -Trunc( FCenter.X * Zoom - (Owner.ClientWidth * 0.5));
  Result.Y:= -Trunc( FCenter.Y * Zoom - (Owner.ClientHeight* 0.5));
end;

//------------------------------------------------------------------------------
function TPHXEditorViewport.DocumentToScreen(const Point: TVector2f): TVector2f;
var Offset: TVector2i;
begin
  Offset:= GetOffset;

  Result.X:= ((Point.X * Zoom) + Offset.X);
  Result.Y:= ((Point.Y * Zoom) + Offset.Y);
end;

//------------------------------------------------------------------------------
function TPHXEditorViewport.ScreenToDocument(const Point: TVector2f): TVector2f;
var Offset: TVector2i;
begin
  Offset:= GetOffset;

  if Zoom > 0.0 then
  begin
    Result.X:= ((Point.X - Offset.X) / Zoom);
    Result.Y:= ((Point.Y - Offset.Y) / Zoom);
  end else
  begin
    Result.X:= 0;
    Result.Y:= 0;
  end;
end;


//------------------------------------------------------------------------------
function TPHXEditorViewport.GetViewportRect: TRectf;
begin
//  Owner.getd
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.SetCenter(const Value: TVector2f);
begin
  FCenter := Value;

  Owner.UpdateScrollBar;

  if doInvalidate in Owner.Options then Owner.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.SetCenterX(const Value: Single);
begin
  if FCenter.X <> Value then
  begin
    FCenter.X := Value;

    Owner.UpdateScrollBar;

    if doInvalidate in Owner.Options then Owner.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.SetCenterY(const Value: Single);
begin
  if FCenter.Y <> Value then
  begin
    FCenter.Y := Value;

    Owner.UpdateScrollBar;

    if doInvalidate in Owner.Options then Owner.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorViewport.SetZoom(const Value: Single);
begin
  if FZoom <> Value then
  begin
    FZoom:= Value;

    Owner.UpdateScrollBar;

    if doInvalidate in Owner.Options then Owner.Invalidate;
  end;
end;


{$REGION 'TPHXEditor'}

// TPHXEditor
//==============================================================================
constructor TPHXEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DoubleBuffered:= True;

  FOptions:=[doInvalidate, doTransparent, doMouseScroll];

  FGrid:= TPHXEditorGrid.Create(Self);

  FViewport:= TPHXEditorViewport.Create(Self);

  FBorderStyle:= bsSingle;
  
  FHoverTimer:= TTimer.Create(Self);
  FHoverTimer.Interval:= 250;
  FHoverTimer.Enabled:= False;
  FHoverTimer.OnTimer:= ShowHover;
end;

//------------------------------------------------------------------------------
destructor TPHXEditor.Destroy;
begin
  SetTools(nil);

  FViewport.Free;
  FGrid.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_VSCROLL or WS_HSCROLL;// or WS_BORDER;// or WS_HSCROLL;// or WS_CLIPCHILDREN or WS_BORDER;

  if (FBorderStyle = bsSingle) then
  begin
    Params.Style   :=  Params.Style and not WS_BORDER;
    Params.ExStyle :=  Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
    //  Params.ExStyle:= Params.ExStyle or WS_EX_CLIENTEDGE;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.result := DLGC_WANTARROWS;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTALLKEYS or DLGC_WANTCHARS or DLGC_WANTTAB;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.WMHScroll(var Message: TWMSCROLL);
begin
  HandleScrollbar( Message, SB_HORZ );
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.WMVScroll(var Message: TWMSCROLL);
begin
  HandleScrollbar( Message, SB_VERT );
end;

//------------------------------------------------------------------------------
{procedure TPHXEditor.WMMouseWheel(var Message: TCMMouseWheel);
var Delta: Integer;
begin
 // Delta:= (Message.WheelDelta div WHEEL_DELTA);

  if Delta > 0 then
  begin
    FViewport.ZoomIn;
  end else
  if Delta < 0 then
  begin
    FViewport.ZoomOut;
  end;
end;}

//    FScrollVert: Integer;
//    FScrollHorz: Integer;

//------------------------------------------------------------------------------
procedure TPHXEditor.UpdateScrollBar;
var ScrollInfo: TScrollInfo;
var Size     :  TVector2i;
var Page     : TVector2i;
begin
  GetDocumentSize(Size.X, Size.Y);

  Page.X:= Size.X div 16;
  Page.Y:= Size.Y div 16;

  //Size.X:= Round(Size.X * Viewport.Zoom);
  //Size.Y:= Round(Size.Y * Viewport.Zoom);

 // FScrollRange.Y:= (Size.Y + 18 ) - ClientHeight;
 // FScrollRange.X:= (Size.X + 18 ) - ClientWidth;

//  FScrollRange.Y:= (Size.Y);
  //FScrollRange.X:= (Size.X);

  ScrollInfo.cbSize := SizeOf(ScrollInfo);

  ScrollInfo.fMask    := SIF_ALL;
  ScrollInfo.nMin     := 0;
  ScrollInfo.nMax     := Max(10, Size.Y) + Page.Y;
  ScrollInfo.nPage    := Page.Y;
  ScrollInfo.nPos     := Round(Viewport.FCenter.Y);
  ScrollInfo.nTrackPos:= Round(Viewport.FCenter.Y);

  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);

  if Size.Y > 0 then
  begin
    EnableScrollBar(Handle, SB_VERT, SB_BOTH);
  end else
  begin
    EnableScrollBar(Handle, SB_VERT, SB_BOTH);
  end;

  ScrollInfo.fMask    := SIF_ALL;
  ScrollInfo.nMin     := 0;
  ScrollInfo.nMax     := Max(10, Size.X) +  Page.X;
  ScrollInfo.nPage    := Page.X;
  ScrollInfo.nPos     := Round(Viewport.FCenter.X);
  ScrollInfo.nTrackPos:= Round(Viewport.FCenter.X);

  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

  if Size.X > 0 then
  begin
    EnableScrollBar(Handle, SB_HORZ, SB_BOTH);
  end else
  begin
    EnableScrollBar(Handle, SB_HORZ, SB_BOTH);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.HandleScrollbar(var msg: TWMSCROLL; bar: Integer);
var ScrollInfo: TScrollInfo;
begin
  msg.result := 0;

  ScrollInfo.cbSize := Sizeof( TscrollInfo );
  ScrollInfo.fMask  := SIF_ALL;
  GetScrollInfo( Handle, bar, ScrollInfo );
  ScrollInfo.fMask  := SIF_POS;

  case msg.ScrollCode Of
    SB_TOP      : ScrollInfo.nPos := ScrollInfo.nMin;
    SB_BOTTOM   : ScrollInfo.nPos := ScrollInfo.nMax;
    SB_LINEUP   : Dec( ScrollInfo.nPos, ScrollInfo.nPage div 10 );
    SB_LINEDOWN : Inc( ScrollInfo.nPos, ScrollInfo.nPage div 10 );
    SB_PAGEUP   : Dec( ScrollInfo.nPos, ScrollInfo.nPage );
    SB_PAGEDOWN : Inc( ScrollInfo.nPos, ScrollInfo.nPage);
    SB_THUMBTRACK, SB_THUMBPOSITION
                : ScrollInfo.nPos := msg.Pos;
    SB_ENDSCROLL: Exit;
  End;

  ScrollInfo.fMask  := SIF_POS;
  If ScrollInfo.nPos < ScrollInfo.nMin Then ScrollInfo.nPos := ScrollInfo.nMin;
  If ScrollInfo.nPos > ScrollInfo.nMax Then ScrollInfo.nPos := ScrollInfo.nMax;
  SetScrollInfo( Handle, bar, ScrollInfo, true );


  if (Bar = SB_VERT) and (Viewport.FCenter.Y <>  ScrollInfo.nPos) then
  begin
    Viewport.FCenter.Y:= ScrollInfo.nPos;

    Invalidate;
  end;
  if (Bar = SB_HORZ) and (Viewport.FCenter.X <>  ScrollInfo.nPos) then
  begin
    Viewport.FCenter.X:= ScrollInfo.nPos;

    Invalidate;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXEditor.DocumentChanged;
begin
  UpdateScrollBar;

  ClipViewport;
end;

//------------------------------------------------------------------------------
function TPHXEditor.DocumentToScreen(const Point: TVector2f): TVector2f;
begin
  Result:= Viewport.DocumentToScreen(Point);
end;

//------------------------------------------------------------------------------
function TPHXEditor.DocumentToScreen(const Point: TVector2i): TVector2i;
var Vector:  TVector2f;
begin
  Vector:= Viewport.DocumentToScreen( TVector2f.Create(Point.X, Point.Y) );

  Result.X:= Trunc(Vector.X);
  Result.Y:= Trunc(Vector.Y);
end;

//------------------------------------------------------------------------------
Function TPHXEditor.DocumentToScreen(const X,  Y: Integer): TVector2i;
begin
  Result:= DocumentToScreen( TVector2i.Create(X, Y) );
end;

//------------------------------------------------------------------------------
Function  TPHXEditor.DocumentToScreen(const X, Y : Single ): TVector2f;
begin
  Result:= Viewport.DocumentToScreen( TVector2f.Create(X, Y) );
end;

//------------------------------------------------------------------------------
function TPHXEditor.DocumentToScreen(const Point: TVector3f): TVector3f;
var Vector:  TVector2f;
begin
  Vector:= Viewport.DocumentToScreen( TVector2f.Create(Point.X, Point.Y) );

  Result.X:= Vector.X;
  Result.Y:= Vector.Y;
  Result.Z:= Point.Z;
end;

//------------------------------------------------------------------------------
function TPHXEditor.ScreenToDocument(const Point: TVector2f): TVector2f;
begin
  Result:= Viewport.ScreenToDocument(Point);
end;

//------------------------------------------------------------------------------
function TPHXEditor.ScreenToDocument(const Point: TVector2i): TVector2i;
var Vector:  TVector2f;
begin
  Vector:= Viewport.ScreenToDocument( TVector2f.Create(Point.X, Point.Y) );

  Result.X:= Trunc(Vector.X);
  Result.Y:= Trunc(Vector.Y);
end;

//------------------------------------------------------------------------------
function TPHXEditor.ScreenToDocument(const X, Y: Integer): TVector2i;
begin
  Result:= ScreenToDocument( TVector2i.Create(X, Y));
end;

//------------------------------------------------------------------------------
function TPHXEditor.ScreenToDocument(const X, Y: Single): TVector2f;
begin
  Result:= Viewport.ScreenToDocument( TVector2f.Create(X, Y));
end;

//------------------------------------------------------------------------------
function TPHXEditor.ScreenToDocument(const Point: TVector3f): TVector3f;
var Vector:  TVector2f;
begin
  Vector:= Viewport.ScreenToDocument( TVector2f.Create(Point.X, Point.Y) );

  Result.X:= Vector.X;
  Result.Y:= Vector.Y;
  Result.Z:= Point.Z;
end;


//------------------------------------------------------------------------------
procedure TPHXEditor.ClipViewport;
begin
  Viewport.ClipViewport;

  UpdateScrollBar;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.ScrollToCenter;
begin
  Viewport.ScrollToCenter;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.DoZoom(Factor: Single);
begin
  Viewport.FZoom:= Viewport.FZoom * Factor;

  ClipViewport;

  UpdateScrollBar;

  if doInvalidate in Options then Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.DoZoom(X, Y: Integer; Factor: Single);
var Size    :  TVector2i;
var Position: TVector2i;
begin
  GetDocumentSize(Size.X, Size.Y);

  Position:=ScreenToDocument( TVector2i.Create(X,Y) );

  Viewport.FZoom:= Viewport.FZoom * Factor;

  if (Viewport.FZoom <> 0) then
  begin

    // Dont allow zooming outside the image
    if Factor > 1.0 then
    begin
      if Position.X < 0      then Position.X:= 0;
      if Position.Y < 0      then Position.Y:= 0;
      if Position.X > Size.X then Position.X:= Size.X;
      if Position.Y > Size.Y then Position.Y:= Size.Y;

//      Viewport.FScroll.X:= Trunc(Position.X - (Width  * 0.5) / Viewport.FZoom);
//      Viewport.FScroll.Y:= Trunc(Position.Y - (Height * 0.5) / Viewport.FZoom);
    end else
    begin
      ClipViewport;
    end;

  end;

  UpdateScrollBar;

  if doInvalidate in Options then Invalidate;
end;


//------------------------------------------------------------------------------
procedure TPHXEditor.DoScroll(DX, DY: Integer);
begin
  if (Viewport.FZoom <> 0) then
  begin
    Viewport.CenterX:= Viewport.CenterX + DX / Viewport.FZoom;
    Viewport.CenterY:= Viewport.CenterY + DY / Viewport.FZoom;

    ClipViewport;
  end;

  UpdateScrollBar;

  if doInvalidate in Options then Invalidate;
end;


//------------------------------------------------------------------------------
procedure TPHXEditor.MouseScroll(MouseX, MouseY: Integer; Enabled: Boolean);
begin
  if Enabled then
  begin
    DoScroll(MouseX - MousePos.X, MouseY - MousePos.Y);
  end;

  MousePos.X:= MouseX;
  MousePos.Y:= MouseY;
end;



const COLOR_BACKGROUND = $00C0C0C0;
const COLOR_BORDER     = $00000000;
const COLOR_SHADOW0    = $00787878;
const COLOR_SHADOW1    = $009D9D9D;
const COLOR_SHADOW2    = $00B5B5B5;

//------------------------------------------------------------------------------
procedure TPHXEditor.Paint;
var s: String;
var w, H: Integer;
begin
  If (csDesigning in ComponentState) then
  begin
    Canvas.Brush.Style := Graphics.bsSolid;
    Canvas.Brush.Color := inherited Color;
    Canvas.Pen.Color   := clBlack;
    Canvas.Pen.Style   := Graphics.psDash;
    Canvas.Rectangle(0, 0, Width, Height);

    Canvas.Pen.Style := Graphics.psSolid;
    Canvas.Pen.Color := clGray;
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, Height);

    Canvas.MoveTo(0, Height);
    Canvas.LineTo(Width, 0);

    s := Format('(%s)', [ClassName]);

    w := Canvas.TextWidth(s);
    h := Canvas.TextHeight(s);

    Canvas.Brush.Style := Graphics.bsSolid;
    Canvas.Brush.Color := clBtnFace;

    Canvas.TextOut(Width div 2-w div 2, Height div 2-h div 2, s);
  end else
  begin
    GetDocumentSize(W, H);

    W:= Round(W * Viewport.Zoom);
    H:= Round(H * Viewport.Zoom);
    with Canvas do
    begin
      Brush.Style := Graphics.bsSolid;
      Brush.Color := COLOR_BACKGROUND;

      FillRect(ClipRect);

      // Draw border
      if (W > 0) and (H > 0) then
      begin
        Brush.Style:= Graphics.bsClear;

        Pen.Style:= psSolid;

        Pen.Color:= COLOR_BORDER;
        Rectangle( Viewport.Offset.X-1, Viewport.Offset.Y-1, Viewport.Offset.X + W+1, Viewport.Offset.Y + H+1);
        Pen.Color:= COLOR_SHADOW0;
        Rectangle( Viewport.Offset.X-2, Viewport.Offset.Y-2, Viewport.Offset.X + W+2, Viewport.Offset.Y + H+2);
        Pen.Color:= COLOR_SHADOW1;
        Rectangle( Viewport.Offset.X-3, Viewport.Offset.Y-3, Viewport.Offset.X + W+3, Viewport.Offset.Y + H+3);
        Pen.Color:= COLOR_SHADOW2;
        Rectangle( Viewport.Offset.X-4, Viewport.Offset.Y-4, Viewport.Offset.X + W+4, Viewport.Offset.Y + H+4);
      end;

    end;

    PaintDocument(Viewport.Offset, Viewport.Zoom);

    {$IFDEF SHOW_DEBUG}
    Canvas.TextOut(4, 4 + 0 * Canvas.TextHeight('W'), Format('CenterX: %.2f ', [Viewport.CenterX]));
    Canvas.TextOut(4, 4 + 1 * Canvas.TextHeight('W'), Format('CenterY: %.2f ', [Viewport.CenterY]));
    Canvas.TextOut(4, 4 + 2 * Canvas.TextHeight('W'), Format('Zoom   : %.2f '   , [Viewport.Zoom]));
    {$ENDIF}

    if Assigned(OnCustomPaint) then OnCustomPaint(Self);

    if Assigned(FTools) then
    begin
      FTools.Paint(Canvas);
    end;

  end;
end;


//------------------------------------------------------------------------------
procedure TPHXEditor.Resize;
begin
  inherited;

  UpdateScrollBar;

  ClipViewport;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.Loaded;
begin
  inherited;

  if (ComponentState = []) and (doAcceptFiles in Options) then
  begin
   // TODO DragAcceptFiles(Handle, true) ;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.Click;
begin
  inherited;

  SetFocus;
end;

//------------------------------------------------------------------------------
{procedure TPHXEditor.WMDropFiles(var Message: TWMDropFiles);
var Count : longInt;
var Index : Integer;
var Buffer: array[0..MAX_PATH] of Char;
var FileName: String;
begin
   Count := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0) ;

   for Index:= 0 to Count - 1 do
   begin
     DragQueryFile(Message.Drop, 0, @buffer, sizeof(buffer)) ;

     FileName:= Buffer;

     if Assigned(FOnFileDropped) then FOnFileDropped(Self, FileName);
   end;
   //  TFileEvent
end; }

//------------------------------------------------------------------------------
//procedure WMRButtonDown(var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
{procedure TPHXEditor.WMRButtonDown(var Message: TLMRButtonDown);
begin
  inherited;

  MousePos.X:= Message.XPos;
  MousePos.Y:= Message.YPos;
end;
 }
//------------------------------------------------------------------------------
procedure TPHXEditor.WMMouseMove(var Message: TWMMouseMove);
var Shift: TShiftState;
begin
  inherited;

  if doMouseScroll in Options then
  begin
    Shift:=  KeysToShiftState(Message.Keys);

    if ssRight in Shift then
    begin
      DoScroll(MousePos.X - Message.XPos, MousePos.Y - Message.YPos);
    end;
  end;
  if doTooltip in Options then
  begin
    if (MousePos.X <> Message.XPos) or ( MousePos.Y <> Message.YPos) then
    begin
      FHoverVisible        := False;
      FHoverTimer  .Enabled:= True;
    end;
  end;
  MousePos.X:= Message.XPos;
  MousePos.Y:= Message.YPos;
end;

//----------------------------------------------------------------------------
procedure TPHXEditor.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  FHoverVisible:= False;
  FHoverTimer.Enabled:= False;

  Invalidate;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FTools) then
  begin
    FTools.HandleMouseMove(Shift, X, Y);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FTools) then
  begin
    FTools.HandleMouseDown(Button, Shift, X, Y);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FTools) then
  begin
    FTools.HandleMouseUp(Button, Shift, X, Y);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Assigned(FTools) then
  begin
    FTools.HandleKeyDown(Key, Shift);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXEditor.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Assigned(FTools) then
  begin
    FTools.HandleKeyUp(Key, Shift);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXEditor.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    //UpdateHeight;
    RecreateWnd(sELF);
  end;
end;

//----------------------------------------------------------------------------
procedure TPHXEditor.ShowHover(Sender: TObject);
begin
  FHoverVisible      := True;
  FHoverTimer.Enabled:= False;

  Invalidate;
end;

//----------------------------------------------------------------------------
procedure TPHXEditor.DrawHover(const ScreenX, ScreenY: Integer; const Text: String);
var DstRect: TRect;
begin
  if FHoverVisible then
  begin
    DstRect.Top   := ScreenY + 2;
    DstRect.Left  := ScreenX + 2;
    DstRect.Bottom:= ScreenY + Trunc( Canvas.TextHeight(Text) * Viewport.Zoom) + 4;
    DstRect.Right := ScreenX + Trunc( Canvas.TextWidth (Text) * Viewport.Zoom) + 6;

    Canvas.Brush.Style:= Graphics.bsSolid;
    Canvas.Brush.Color:= $00A6FFFF;

    Canvas.Pen.Style:= Graphics.psSolid;
    Canvas.Pen.Color:= clBlack;

    Canvas.Rectangle(DstRect);

    Canvas.Font.Color:= clBlack;

    Canvas.Brush.Style:= Graphics.bsClear;
    Canvas.TextOut(DstRect.Left + 2 , DstRect.Top, Text);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditor.SetTools(const Value: TPHXEditorTools);
begin
  if Assigned(FTools) then
  begin
    FTools.Editor:= nil;
  end;

  FTools := Value;

  if Assigned(FTools) then
  begin
    FTools.Editor:= Self;
  end;

  Invalidate;
end;

{$ENDREGION}

{$REGION 'TPHXEditorTool'}


// TPHXEditorTool
//==============================================================================
constructor TPHXEditorTool.Create(ATools: TPHXEditorTools);
begin
  FTools:= ATools;
end;

//------------------------------------------------------------------------------
constructor TPHXEditorTool.Create(ATools: TPHXEditorTools; AButton: TToolButton);
begin
  Create(ATools);

  SetButton(AButton)
end;

//------------------------------------------------------------------------------
destructor TPHXEditorTool.Destroy;
begin
  SetButton(nil);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.ButtonClicked(Sender: TObject);
begin
  if Assigned(FTools) then
  begin
    FTools.SetActive(Self);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXEditorTool.Activated;
begin
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.Deactivated;
begin
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.MouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.KeyDown(var Key: Word; Shift: TShiftState);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.KeyUp(var Key: Word; Shift: TShiftState);
begin

end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.Paint(Canvas: TCanvas);
begin
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.ShowHint(const Text: String);
begin
  Tools.ShowHint(Text);
end;

//------------------------------------------------------------------------------
function TPHXEditorTool.GetEditor: TPHXEditor;
begin
  if Assigned(Tools) then
  begin
    Result:= Tools.Editor;
  end else
  begin
    Result:= nil;
  end;
end;

//------------------------------------------------------------------------------
function TPHXEditorTool.GetEnabled: Boolean;
begin
  Result:= True;
end;


//------------------------------------------------------------------------------
procedure TPHXEditorTool.SetActive(const Value: Boolean);
begin
  FActive := Value;

  if Assigned(Button) then
  begin
    Button.Down:= FActive;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTool.SetButton(const Value: TToolButton);
begin
  if Assigned(FButton) then
  begin
    FButton.OnClick:= nil;
  end;

  FButton := Value;

  if Assigned(FButton) then
  begin
    FButton.Enabled:= Enabled;
    FButton.Down   := Active;
    FButton.OnClick:= ButtonClicked;
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXEditorTool.Update;
begin
  if Assigned(FButton) then
  begin
    FButton.Enabled:= Enabled;
  end;
end;

{$ENDREGION}


{$REGION 'TPHXEditorTools'}

// TPHXEditorTools
//==============================================================================
constructor TPHXEditorTools.Create;
begin
  FTools:= TObjectList<TPHXEditorTool>.Create;
  FActive:= nil;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer:= TimerEvent;
  FTimer.Interval:= 100;
  FTimer.Enabled:= True;
end;

//------------------------------------------------------------------------------
destructor TPHXEditorTools.Destroy;
begin
  FTimer.Free;
  FTools.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.Add(const Tool: TPHXEditorTool);
begin
  FTools.Add(Tool);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.Add(const Tool: TPHXEditorToolClass);
begin
  FTools.Add(Tool.Create(Self));
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.Add(const Tool: TPHXEditorToolClass; Button: TToolButton);
begin
  FTools.Add(Tool.Create(Self, Button));
end;


//------------------------------------------------------------------------------
procedure TPHXEditorTools.Paint(Canvas: TCanvas);
begin
  if Assigned(Active) and (Active.Active) then
  begin
    Active.Paint(Canvas);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.ShowHint(const Text: String);
begin
  if Assigned(OnHint) then
  begin
    OnHint(Self, Text);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.TimerEvent(Sender: TObject);
var Tool: TPHXEditorTool;
begin
  for Tool in FTools do
  begin
    Tool.Update;
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.HandleMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Active) and (Active.Active) then
  begin
    Active.MouseMove(Shift, X,Y);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.HandleMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Active) and (Active.Active) then
  begin
    Active.MouseDown(Button, Shift, X,Y);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.HandleMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(Active) and (Active.Active) then
  begin
    Active.MouseUp( Button, Shift, X,Y);
  end;
end;


//------------------------------------------------------------------------------
procedure TPHXEditorTools.HandleKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(Active) and (Active.Active) then
  begin
    Active.KeyDown(Key, Shift);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.HandleKeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(Active) and (Active.Active) then
  begin
    Active.KeyUp(Key, Shift);
  end;
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.SetActive(const Tool: TPHXEditorTool);
begin
  ShowHint('');

  if Assigned(FActive) then
  begin
    FActive.Active:= False;
    FActive.Deactivated;
  end;

  FActive := Tool;

  if Assigned(FActive) then
  begin
    Editor.SetFocus;

    FActive.Active:= True;
    FActive.Activated;
  end;

//  ActiveChange(Previous, FActiveTool);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.SetActive(const Name: String);
var Tool: TPHXEditorTool;
begin
  for Tool in Tools do
  begin
    if SameText(Tool.Name, Name) then
    begin
     SetActive(Tool);
     Exit;
    end;
  end;

  SetActive(nil);
end;

//------------------------------------------------------------------------------
procedure TPHXEditorTools.SetActive(const Ident: Cardinal );
var Tool: TPHXEditorTool;
begin
  for Tool in Tools do
  begin
    if Tool.Ident = Ident then
    begin
      SetActive(Tool);
      Exit;
    end;
  end;

  SetActive(nil);
end;

//------------------------------------------------------------------------------
function TPHXEditorTools.IsActive(const Tool: TPHXEditorTool): Boolean;
begin
  Result:= Assigned(FActive) and (FActive.Active) and (FActive = Tool);
end;

//------------------------------------------------------------------------------
function TPHXEditorTools.IsActive(const Name: String): boolean;
begin
  Result:= Assigned(FActive) and (FActive.Active) and (FActive.Name = Name);
end;

//------------------------------------------------------------------------------
function TPHXEditorTools.IsActive(const Ident: Cardinal): Boolean;
begin
  Result:= Assigned(FActive) and (FActive.Active) and (FActive.Ident = Ident);

end;



{$ENDREGION}


end.

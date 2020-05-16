unit uTools;

interface

uses
  SysUtils, Classes, Graphics, Controls, Types,

  phxEditor,

  phxTypes,

  phxGraphics,
  phxGraphicsEx,

  phxSkin,
  phxSkinEx;

const

TOOL_MOVE_REGION = $1002;
TOOL_SELECT_REGION = $1003;


TOOL_SELECT_STATE_FOREGROUND = $2001;
TOOL_SELECT_STATE_BACKGROUND = $2002;

TOOL_STATE_IMPORT = $2003;

type

//------------------------------------------------------------------------------
TSkinTool = class(TPHXEditorTool)
  private
    function GetEditor: TPHXSkinEditor;
  public
    property Editor: TPHXSkinEditor read GetEditor;
  end;

//------------------------------------------------------------------------------
TElementTool = class(TSkinTool)
  private
    function GetElement: TPHXSkinElement;
  public
    property Element: TPHXSkinElement read GetElement;
  end;

//------------------------------------------------------------------------------
TToolElementMove = class(TSkinTool)
  private
    Started: Boolean;
    Mouse: TVector2i;
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
    function GetEnabled: Boolean; override;

  public
    procedure Activated; override;
    procedure Deactivated; override;


    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  end;

//------------------------------------------------------------------------------
TToolElementSelect = class(TElementTool)
  private
    Started: Boolean;
    Mouse: TVector2i;

  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
    function GetEnabled: Boolean; override;

  public
    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  end;

//------------------------------------------------------------------------------
TToolElementImport = class(TElementTool)
  private
    Bitmap: TBitmap;
    Buffer: TPHXBitmap;
    Position: TVector2i;
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
    function GetEnabled: Boolean; override;

  public
    constructor Create(ATools: TPHXEditorTools); override;
    destructor Destroy; override;

    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;

    procedure Paint(Canvas: TCanvas); override;
  end;


//TOOL_SELECT_STATE_FOREGROUND = $2001;

//------------------------------------------------------------------------------
TToolElementSelectForeground = class(TElementTool)
  private
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    constructor Create(ATools: TPHXEditorTools); override;
    destructor Destroy; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  end;

//TOOL_SELECT_STATE_BACKGROUND = $2002;
//------------------------------------------------------------------------------
TToolElementSelectBackground = class(TElementTool)
  private
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    constructor Create(ATools: TPHXEditorTools); override;
    destructor Destroy; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  end;


//------------------------------------------------------------------------------
TModTools = class(TDataModule)
  private
    FEditor: TPHXSkinEditor;
    FTools: TPHXEditorTools;
    procedure SetEditor(const Value: TPHXSkinEditor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Tools: TPHXEditorTools read FTools;

    property Editor: TPHXSkinEditor read FEditor write SetEditor;
  end;

var
  ModTools: TModTools;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses uActions, uMain;

{$R *.dfm}

// TSkinTool
//------------------------------------------------------------------------------
function TSkinTool.GetEditor: TPHXSkinEditor;
begin
  Result:= TPHXSkinEditor(inherited Editor);
end;

// TElementTool
//------------------------------------------------------------------------------
function TElementTool.GetElement: TPHXSkinElement;
begin
  Result:= ModTools.Editor.Selection.Element;
end;


{$REGION 'TOOL_ELEMENT_MOVE'}

// TToolStateMove
//------------------------------------------------------------------------------
function TToolElementMove.GetEnabled: Boolean;
begin
  Result:= Assigned(Editor) and Assigned(Editor.Selection.Element)
end;

//------------------------------------------------------------------------------
function TToolElementMove.GetIdent: Cardinal;
begin
  Result:= TOOL_MOVE_REGION;
end;

//------------------------------------------------------------------------------
function TToolElementMove.GetName: string;
begin
  Result:= 'Tools.Element.Move';
end;

//------------------------------------------------------------------------------
procedure TToolElementMove.Activated;
begin
  Started:= False;
  Mouse.X:= 0;
  Mouse.Y:= 0;

  Editor.Cursor:= crSizeAll;
end;

//------------------------------------------------------------------------------
procedure TToolElementMove.Deactivated;
begin
  inherited;

  Editor.Cursor:= crDefault;
end;

//------------------------------------------------------------------------------
procedure TToolElementMove.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  if ModTools.Editor.Selection.ValidElement then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Mouse.X:= Position.X;
    Mouse.Y:= Position.Y;

    if Editor.Skin.ElementAt(Position.X, Position.Y) <> Editor.Selection.idxElement then
    begin
       Tools.SetActive( nil );
    end else
    begin
      Started:= True;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolElementMove.MouseMove(Shift: TShiftState; X, Y: Integer);
var MousePos1: TVector2i;
var MousePos2: TVector2i;
var Delta    : TVector2i;
begin
  if Started and Editor.Selection.ValidElement then
  begin

    MousePos1.X:= Mouse.X;
    MousePos1.Y:= Mouse.Y;
    MousePos2  := Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(MousePos2);

    Delta.X:= MousePos2.X - MousePos1.X;
    Delta.Y:= MousePos2.Y - MousePos1.Y;


    Editor.Selection.Element.Bounds:= TRecti.Create(Editor.Selection.Element.Bounds.Left   + Delta.X,
                                                    Editor.Selection.Element.Bounds.Top    + Delta.Y,
                                                    Editor.Selection.Element.Bounds.Right  + Delta.X,
                                                    Editor.Selection.Element.Bounds.Bottom + Delta.Y
             );
    Editor.Selection.SelectedChanged;

    Editor.Repaint;

    Mouse.X:= MousePos2.X;
    Mouse.Y:= MousePos2.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolElementMove.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Tools.SetActive( nil );
end;

{$ENDREGION}

{$REGION 'TOOL_ELEMENT_SELECT'}

//------------------------------------------------------------------------------
function TToolElementSelect.GetEnabled: Boolean;
begin
  Result:= Assigned(Editor) and Assigned(Editor.Selection.Element)
end;

//------------------------------------------------------------------------------
function TToolElementSelect.GetIdent: Cardinal;
begin
  Result:= TOOL_SELECT_REGION;
end;

//------------------------------------------------------------------------------
function TToolElementSelect.GetName: string;
begin
  Result:= 'Tools.Element.Select';
end;

//------------------------------------------------------------------------------
procedure TToolElementSelect.Activated;
begin
  Started:= False;
  Mouse.X:= 0;
  Mouse.Y:= 0;
end;

//------------------------------------------------------------------------------
procedure TToolElementSelect.Deactivated;
begin
  inherited;

end;

//------------------------------------------------------------------------------
procedure TToolElementSelect.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  if Editor.Selection.ValidElement then
  begin
    Position:=Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Started:= True;
    Mouse.X:= Position.X;
    Mouse.Y:= Position.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolElementSelect.MouseMove(Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
begin
  if Started and Assigned(Element) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Editor.Selection.Element.Bounds:= TRecti.Create(Mouse.X, Mouse.Y, Position.X, Position.Y);
    Editor.Repaint;
  end;
  Editor.Cursor:= crCross;
end;

//------------------------------------------------------------------------------
procedure TToolElementSelect.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 // Started:= False;
  Tools.SetActive(nil);

  Editor.Selection.SelectedChanged;
end;

{$ENDREGION}

{$REGION 'TOOL_ELEMENT_IMPORT'}


// TToolElementImport
//==============================================================================
constructor TToolElementImport.Create(ATools: TPHXEditorTools);
begin
  inherited;
  Buffer:= TPHXBitmap.Create;
  Bitmap:= TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor TToolElementImport.Destroy;
begin
  Buffer.Free;
  Bitmap.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TToolElementImport.GetEnabled: Boolean;
begin
  Result:= Assigned(Editor) and Assigned(Editor.Selection.Element)
end;

//------------------------------------------------------------------------------
function TToolElementImport.GetIdent: Cardinal;
begin
  Result:= TOOL_STATE_IMPORT;
end;

//------------------------------------------------------------------------------
function TToolElementImport.GetName: string;
begin
  Result:= 'Tools.Element.Import';
end;

//------------------------------------------------------------------------------
procedure TToolElementImport.Activated;
var FileName: String;
begin
  if modActions.OpenTextureDialog.Execute then
  begin
    FileName:= modActions.OpenTextureDialog.FileName;

//    Buffer.Name:= ChangeFileExt( ExtractFileName(FileName), '' );
    Buffer.LoadBitmap(FileName);

    DrawBitmap(Buffer, Bitmap);//, Editor.Background);
  end else
  begin
    Tools.SetActive(nil);
  end;
end;

//------------------------------------------------------------------------------
procedure TToolElementImport.Deactivated;
begin
  inherited;

end;

//------------------------------------------------------------------------------
procedure TToolElementImport.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------
procedure TToolElementImport.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crSizeAll;

  Position:= Editor.ScreenToDocument(X,Y);

  Editor.Grid.Snap(Position);

  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TToolElementImport.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Bounds: TRecti;
begin
  if (Button = TMouseButton.mbLeft) and (Editor.Selection.ValidElement) then
  begin
    Position:=Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);


    Bounds.Left  := Position.X;
    Bounds.Top   := Position.Y;
    Bounds.Right := Position.X + Buffer.Width;
    Bounds.Bottom:= Position.Y + Buffer.Height;

    Editor.Selection.Element.Bounds:= Bounds;

    Buffer.CopyTo(Editor.Skin.Texture.Graphic, TRecti.Create(0, 0, Bitmap.Width, Bitmap.Height), Position );


    Editor.Skin:= Editor.Skin;
  end;
  Tools.SetActive(nil);
end;

//------------------------------------------------------------------------------
procedure TToolElementImport.Paint(Canvas: TCanvas);
var Rect: TRect;
var DrawPosition: TVector2i;
begin
  DrawPosition:= Editor.DocumentToScreen(Position);

  Rect.Left  := DrawPosition.X;
  Rect.Top   := DrawPosition.Y;
  Rect.Right := DrawPosition.X + Trunc(Bitmap.Width  * Editor.Viewport.Zoom);
  Rect.Bottom:= DrawPosition.Y + Trunc(Bitmap.Height * Editor.Viewport.Zoom);

  Editor.Canvas.StretchDraw(Rect, Bitmap);

  with Editor.Canvas do
  begin
     Brush.Style:= bsClear;

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(Rect);

     Pen.Color  := clBlack;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(Rect);
  end;

end;

{$ENDREGION}

{$REGION 'TOOL_SELECT_STATE_FOREGROUND'}

// TToolElementSelectForeground
//==============================================================================
constructor TToolElementSelectForeground.Create(ATools: TPHXEditorTools);
begin
  inherited;

end;

//------------------------------------------------------------------------------
destructor TToolElementSelectForeground.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TToolElementSelectForeground.GetIdent: Cardinal;
begin
  Result:= TOOL_SELECT_STATE_FOREGROUND;
end;

//------------------------------------------------------------------------------
function TToolElementSelectForeground.GetName: string;
begin
  Result:= 'Element.Select.Foreground';
end;

//------------------------------------------------------------------------------
procedure TToolElementSelectForeground.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
//var Position: TVector2i;
//var Color   : TColor;
begin
  if Assigned(Element) then
  begin
//    Position:=Editor.ScreenToDocument(X,Y);

//    Color:= Editor.Buffer.Canvas.Pixels[Position.X, Position.Y];
//    Element.Color:= TColor4f.Create(Color);

    Editor.Selection.SelectedChanged;
  end;
  Tools.SetActive( nil );
end;

//------------------------------------------------------------------------------
procedure TToolElementSelectForeground.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crCross;
end;

{$ENDREGION}


{$REGION 'TOOL_SELECT_STATE_BACKGROUND'}

// TToolElementSelectBackground
//==============================================================================
constructor TToolElementSelectBackground.Create(ATools: TPHXEditorTools);
begin
  inherited;

end;

//------------------------------------------------------------------------------
destructor TToolElementSelectBackground.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------
function TToolElementSelectBackground.GetIdent: Cardinal;
begin
  Result:= TOOL_SELECT_STATE_BACKGROUND;
end;

//------------------------------------------------------------------------------
function TToolElementSelectBackground.GetName: string;
begin
  Result:= 'Element.Select.Foreground';
end;

//------------------------------------------------------------------------------
procedure TToolElementSelectBackground.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Color   : TColor;
begin
  if Assigned(Element) then
  begin
    Position:=Editor.ScreenToDocument(X,Y);

    Color:= Editor.Buffer.Canvas.Pixels[Position.X, Position.Y];

    Element.TextColor:= TColor4f.Create(Color);

    Editor.Selection.SelectedChanged;
  end;
  Tools.SetActive( nil );
end;

//------------------------------------------------------------------------------
procedure TToolElementSelectBackground.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crCross;
end;

{$ENDREGION}




//------------------------------------------------------------------------------
constructor TModTools.Create(AOwner: TComponent);
begin
  inherited;
  FTools:= TPHXEditorTools.Create;
 // modTools.ToolStateMove  .Button:= btnToolMove;
//  modTools.ToolStateSelect.Button:= btnToolSelect;
//  modTools.ToolStateImport.Button:= btnToolImport;

  FTools.Add(TToolElementMove  , frmMain.btnToolMove);
  FTools.Add(TToolElementSelect, frmMain.btnToolSelect);
  FTools.Add(TToolElementImport, frmMain.btnToolImport);

  FTools.Add(TToolElementSelectBackground);
  FTools.Add(TToolElementSelectForeground);
end;

//------------------------------------------------------------------------------
destructor TModTools.Destroy;
begin
  FTools.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TModTools.SetEditor(const Value: TPHXSkinEditor);
begin
  if Assigned(FEditor) then
  begin
    FEditor.Tools:= nil;
  end;

  FEditor := Value;

  if Assigned(FEditor) then
  begin
    FEditor.Tools:= FTools;
  end;

//  Tools.Editor:=  Value;
end;




end.

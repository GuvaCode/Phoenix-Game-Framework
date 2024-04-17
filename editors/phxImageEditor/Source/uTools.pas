unit uTools;

interface

uses
  SysUtils, Classes, Graphics, Controls, Types, Math,
  LCLType,


  Dialogs,

  phxEditor,

  phxTypes,
  phxImage,
  phxGraphics,
  phxGraphicsEx,

  uEditor;

const



TOOL_PATTERN_ADD    = $1000;
TOOL_PATTERN_PICK   = $1001;
TOOL_PATTERN_SELECT = $1002;
TOOL_PATTERN_MOVE   = $1003;
TOOL_PATTERN_IMPORT = $1004;
TOOL_PATTERN_PACKER = $1005;

TOOL_TAG_MOVE       = $2001;

type

//------------------------------------------------------------------------------
TImageTool = class(TPHXEditorTool)
  private
    function GetEditor: TPHXImageEditor;
    function GetImage: TPHXImage;
  protected
    function GetEnabled: Boolean; override;
  public
    property Editor: TPHXImageEditor read GetEditor;
    property Image: TPHXImage read GetImage;
  end;

//------------------------------------------------------------------------------
TPatternTool = class(TImageTool)
  protected
    function GetEnabled: Boolean; override;
  end;

// TOOL_PATTERN_ADD
//------------------------------------------------------------------------------
TToolPatternAdd = class(TImageTool)
  private
    Started: Boolean;
    PosMouseDown: TVector2i;
    PosMouseMove: TVector2i;
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;

    procedure Paint(Canvas: TCanvas); override;
  end;

// TOOL_PATTERN_ADD
//------------------------------------------------------------------------------
TToolPatternPick = class(TImageTool)
  private
    Width  : Integer;
    Height : Integer;
    Mask   : array of Boolean;
    Visited: array of Boolean;

    procedure Initialize;
    procedure Finalize;

    procedure Search(const X,Y: Integer; var Rect: TRecti);
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  end;

// TOOL_PATTERN_SELECT
//------------------------------------------------------------------------------
TToolPatternSelect = class(TPatternTool)
  private
    Started: Boolean;
    Start  : TVector2i;

  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  end;

// TOOL_PATTERN_MOVE
//------------------------------------------------------------------------------
TToolPatternMove = class(TPatternTool)
  private
    Started: Boolean;
    Start  : TVector2i;

  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  end;

// TOOL_PATTERN_IMPORT
//
// Import a pattern image from a bitmap
//------------------------------------------------------------------------------
TToolPatternImport = class(TImageTool)
  private
    Name    : String;
    Bitmap  : TBitmap;
    Buffer  : TPHXBitmap;
    Position: TVector2i;

    procedure Finish;
    procedure Cancel;
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    constructor Create(ATools: TPHXEditorTools); override;
    destructor Destroy; override;

    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;

    procedure Paint(Canvas: TCanvas); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

// TOOL_PATTERN_PACKER
//
// Import a list of patterns into a revtangle using the texture packer
//------------------------------------------------------------------------------
TToolPatternPacker = class(TImageTool)
  private
//    Name    : String;
    Bitmap  : TBitmap;
    Buffer  : TPHXBitmap;
//    Position: TVector2i;

    procedure Finish;
    procedure Cancel;
  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    constructor Create(ATools: TPHXEditorTools); override;
    destructor Destroy; override;

    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;

    procedure Paint(Canvas: TCanvas); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;



//------------------------------------------------------------------------------
TTagTool = class(TImageTool)
  protected
    function GetEnabled: Boolean; override;
  end;



// TOOL_TAG_MOVE
//------------------------------------------------------------------------------
TToolTagMove = class(TTagTool)
  private
    Started: Boolean;
    Start  : TVector2i;

  protected
    function GetName: string; override;
    function GetIdent: Cardinal; override;
  public
    procedure Activated; override;
    procedure Deactivated; override;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;  Y: Integer); override;
  end;

//------------------------------------------------------------------------------
TModTools = class(TDataModule)
  private
    FEditor: TPHXImageEditor;
    FTools : TPHXEditorTools;

    procedure SetEditor(const Value: TPHXImageEditor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Tools: TPHXEditorTools read FTools;

    property Editor: TPHXImageEditor read FEditor write SetEditor;
  end;
var
  ModTools: TModTools;

implementation



{%CLASSGROUP 'System.Classes.TPersistent'}

uses uMain, uActions, uTag.Actions;

{$R *.dfm}

// TImageTool
//------------------------------------------------------------------------------
function TImageTool.GetEditor: TPHXImageEditor;
begin
  Result:= TPHXImageEditor(inherited Editor);
end;

//------------------------------------------------------------------------------
function TImageTool.GetEnabled: Boolean;
begin
  Result:= Assigned(Editor) and Assigned(Editor.Image);
end;

//------------------------------------------------------------------------------
function TImageTool.GetImage: TPHXImage;
begin
  if Assigned(Editor) then
  begin
    Result:= Editor.Image;
  end else
  begin
    Result:= nil;
  end;
end;

// TPatternTool
//------------------------------------------------------------------------------
function TPatternTool.GetEnabled: Boolean;
begin
  Result:= Assigned(Editor) and Assigned(Editor.Image) and (ModActions.Selected.Pattern >= 0) and (ModActions.Selected.Pattern < Image.Patterns.Count);
end;

// TTagTool
//------------------------------------------------------------------------------
function TTagTool.GetEnabled: Boolean;
begin
  Result:= Assigned(Editor) and Assigned(Editor.Image) and (ModTags.SelectedIndex >= 0) and (ModTags.SelectedIndex < Image.Tags.Count);

end;

{$REGION 'TOOL_PATTERN_ADD'}

//------------------------------------------------------------------------------
function TToolPatternAdd.GetIdent: Cardinal;
begin
  Result:= TOOL_PATTERN_ADD;
end;

//------------------------------------------------------------------------------
function TToolPatternAdd.GetName: string;
begin
  Result:= 'Tools.Pattern.Select';
end;

//------------------------------------------------------------------------------
procedure TToolPatternAdd.Activated;
begin
  Started:= False;

  ModActions.Selected.Pattern:= -1;
end;

//------------------------------------------------------------------------------
procedure TToolPatternAdd.Deactivated;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TToolPatternAdd.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  Position:= Editor.ScreenToDocument(X,Y);

  Started:= True;

  PosMouseDown.X:= Position.X;
  PosMouseDown.Y:= Position.Y;

  Editor.Grid.Snap(PosMouseDown);

  Editor.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TToolPatternAdd.MouseMove(Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
begin
  Position:= Editor.ScreenToDocument(X,Y);

  PosMouseMove.X:= Position.X;
  PosMouseMove.Y:= Position.Y;

  Editor.Grid.Snap(PosMouseMove);

  Editor.Cursor:= crCross;

  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TToolPatternAdd.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Pattern : TPHXPattern;
begin
  Editor.Cursor:= crDefault;

  if Started  then
  begin
    Pattern.Name  := ShortString(GeneratePatternName(Image));
    Pattern.X     := PosMouseDown.X;
    Pattern.Y     := PosMouseDown.Y;
    Pattern.Width := PosMouseMove.X - PosMouseDown.X;
    Pattern.Height:= PosMouseMove.Y - PosMouseDown.Y;

    if ModActions.Settings.CenterPivots then
    begin
      Pattern.Pivot.X:= Pattern.Width  div 2;
      Pattern.Pivot.Y:= Pattern.Height div 2;
    end else
    begin
      Pattern.Pivot.X:= 0;
      Pattern.Pivot.Y:= 0;
    end;

    Pattern.Flip  := False;
    Pattern.Mirror:= False;

    Image.Patterns.Add(Pattern);

    ModActions.Selected.Pattern:= Image.Patterns.Count - 1;

    ModActions.Document.Changed;

    Editor.Repaint;
  end;

  Tools.SetActive(nil);
end;

//------------------------------------------------------------------------------
procedure TToolPatternAdd.Paint(Canvas: TCanvas);
var Rect: TRect;
var P1,P2: TVector2i;
begin
  if Started then
  begin
    P1:= Editor.DocumentToScreen(PosMouseDown);
    P2:= Editor.DocumentToScreen(PosMouseMove);

    Rect.Left  := P1.X;
    Rect.Top   := P1.Y;
    Rect.Right := P2.X;
    Rect.Bottom:= P2.Y;

    with Canvas do
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
end;

{$ENDREGION}


{$REGION 'TOOL_PATTERN_PICK'}

//------------------------------------------------------------------------------
function TToolPatternPick.GetIdent: Cardinal;
begin
  Result:= TOOL_PATTERN_PICK;
end;

//------------------------------------------------------------------------------
function TToolPatternPick.GetName: string;
begin
  Result:= 'Tools.Pattern.Pick';
end;

//------------------------------------------------------------------------------
procedure TToolPatternPick.Activated;
begin
  ModActions.Selected.Pattern:= -1;
end;

//------------------------------------------------------------------------------
procedure TToolPatternPick.Deactivated;
begin
  inherited;
end;


//------------------------------------------------------------------------------
procedure TToolPatternPick.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Rect    : TRecti;
var Pattern : TPHXPattern;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  Position:= Editor.ScreenToDocument(X,Y);

  Rect.Left  := Position.X;
  Rect.Right := Position.X;
  Rect.Top   := Position.Y;
  Rect.Bottom:= Position.Y;

  Initialize;
  try
    Search(Position.X, Position.Y, Rect);
  finally
    Finalize
  end;

  Pattern.Name  := ShortString(GeneratePatternName(Image));
  Pattern.X     := Rect.Left;
  Pattern.Y     := Rect.Top;
  Pattern.Width := Rect.Right - Rect.Left;
  Pattern.Height:= Rect.Bottom - Rect.Top;

  if ModActions.Settings.CenterPivots then
  begin
    Pattern.Pivot.X:= Pattern.Width  div 2;
    Pattern.Pivot.Y:= Pattern.Height div 2;
  end else
  begin
    Pattern.Pivot.X:= 0;
    Pattern.Pivot.Y:= 0;
  end;

  Pattern.Flip  := False;
  Pattern.Mirror:= False;

  Image.Patterns.Add(Pattern);

  ModActions.Selected.Pattern:= Image.Patterns.Count - 1;

  ModActions.Document.Changed;

  Editor.Repaint;
end;


//------------------------------------------------------------------------------
procedure TToolPatternPick.MouseMove(Shift: TShiftState; X, Y: Integer);
//var Position: TVector2i;
begin
//  Position:= Editor.ScreenToDocument(X,Y);

 // PosMouseMove.X:= Position.X;
 // PosMouseMove.Y:= Position.Y;

 // Editor.Grid.Snap(PosMouseMove);

  Editor.Cursor:= crCross;

  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TToolPatternPick.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crDefault;

  Tools.SetActive(nil);
end;


//------------------------------------------------------------------------------
procedure TToolPatternPick.Initialize;
var X, Y    : Integer;
var Line    : PByte;
var Getter  : TGetPixel;
var Color   : TPHXPixel;
begin
  Width := Image.Texture.Width;
  Height:= Image.Texture.Height;

  Getter:= GetPixelFormatGetter(Image.Texture.Format);

  SetLength(Mask   , Width * Height);
  SetLength(Visited, Width * Height);

  for Y := 0 to Height - 1 do
  begin
    Line:= Image.Texture.ScanLine(Y);
    for X := 0 to Width - 1 do
    begin
      Getter(Line, Color);

      Mask   [X + Y * Width]:= Color.Alpha > 0;
      Visited[X + Y * Width]:= False;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolPatternPick.Search(const X, Y: Integer; var Rect: TRecti);
begin
  // Outside the image
  if (X < 0) or (Y < 0) or (X >= Image.Texture.Width) or (Y >= Image.Texture.Height) then  Exit;
  // Already visited
  if Visited[X + Y * Width] then Exit;

  Visited[X + Y * Width]:= True;

  if Mask[X + Y * Width] then
  begin
    Rect.Left  := Min(Rect.Left  , X);
    Rect.Right := Max(Rect.Right , X);
    Rect.Top   := Min(Rect.Top   , Y);
    Rect.Bottom:= Max(Rect.Bottom, Y);

    Search(X-1, Y, Rect);
    Search(X+1, Y, Rect);

    Search(X, Y+1, Rect);
    Search(X, Y-1, Rect);
  end;

end;

//------------------------------------------------------------------------------
procedure TToolPatternPick.Finalize;
begin
  SetLength(Mask   , 0);
  SetLength(Visited, 0);
end;




{$ENDREGION}

{$REGION 'TOOL_PATTERN_SELECT'}

//------------------------------------------------------------------------------
function TToolPatternSelect.GetIdent: Cardinal;
begin
  Result:= TOOL_PATTERN_SELECT;
end;

//------------------------------------------------------------------------------
function TToolPatternSelect.GetName: string;
begin
  Result:= 'Tools.Pattern.Select';
end;

//------------------------------------------------------------------------------
procedure TToolPatternSelect.Activated;
begin
  Started:= False;
  Start.X:= 0;
  Start.Y:= 0;
end;

//------------------------------------------------------------------------------
procedure TToolPatternSelect.Deactivated;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TToolPatternSelect.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Index   : Integer;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  Index:= ModActions.Selected.Pattern;
  if (Index >= 0) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Started:= True;
    Start.X:= Position.X;
    Start.Y:= Position.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolPatternSelect.MouseMove(Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Index   : Integer;
begin
  Index:= ModActions.Selected.Pattern;

  if Started and (Index >= 0) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Image.Patterns.List^[Index].X     := Start.X;
    Image.Patterns.List^[Index].Y     := Start.Y;
    Image.Patterns.List^[Index].Width := Position.X - Start.X;
    Image.Patterns.List^[Index].Height:= Position.Y - Start.Y;

    ModActions.Document.Changed;

    Editor.Repaint;
  end;
  Editor.Cursor:= crCross;
end;

//------------------------------------------------------------------------------
procedure TToolPatternSelect.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crDefault;

  Tools.SetActive(nil);
end;

{$ENDREGION}

{$REGION 'TOOL_PATTERN_MOVE'}

//------------------------------------------------------------------------------
function TToolPatternMove.GetIdent: Cardinal;
begin
  Result:= TOOL_PATTERN_MOVE;
end;

//------------------------------------------------------------------------------
function TToolPatternMove.GetName: string;
begin
  Result:= 'Tools.Pattern.Move';
end;

//------------------------------------------------------------------------------
procedure TToolPatternMove.Activated;
begin
  Started:= False;
  Start.X:= 0;
  Start.Y:= 0;
end;

//------------------------------------------------------------------------------
procedure TToolPatternMove.Deactivated;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TToolPatternMove.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Index   : Integer;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  Index:= ModActions.Selected.Pattern;
  if (Index >= 0) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Started:= True;
    Start.X:= Position.X;
    Start.Y:= Position.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolPatternMove.MouseMove(Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Index   : Integer;
begin
  Index:= ModActions.Selected.Pattern;

  if Started and (Index >= 0) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Image.Patterns.List^[Index].X:= Image.Patterns.List^[Index].X + (Position.X - Start.X);
    Image.Patterns.List^[Index].Y:= Image.Patterns.List^[Index].Y + (Position.Y - Start.Y);

    ModActions.Document.Changed;

    Editor.Repaint;

    Start.X:= Position.X;
    Start.Y:= Position.Y;
  end;
  Editor.Cursor:= crSizeAll;
end;

//------------------------------------------------------------------------------
procedure TToolPatternMove.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crDefault;

  Tools.SetActive(nil);
end;

{$ENDREGION}

{$REGION 'TOOL_PATTERN_IMPORT'}

// TToolPatternImport
//==============================================================================
constructor TToolPatternImport.Create(ATools: TPHXEditorTools);
begin
  inherited;
  Buffer:= TPHXBitmap.Create;
  Bitmap:= TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor TToolPatternImport.Destroy;
begin
  Buffer.Free;
  Bitmap.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TToolPatternImport.GetIdent: Cardinal;
begin
  Result:= TOOL_PATTERN_IMPORT;
end;

//------------------------------------------------------------------------------
function TToolPatternImport.GetName: string;
begin
  Result:= 'Tools.Pattern.Import';
end;


//------------------------------------------------------------------------------
procedure TToolPatternImport.Activated;
var Background: TBitmap;
begin
  if ModActions.OpenTextureDialog.Execute then
  begin
    Name:= modActions.OpenTextureDialog.FileName;

    Buffer.LoadBitmap(Name);

    if (Buffer.Width > Image.Width) or (Buffer.Height > Image.Height) then
    begin
      MessageDlg('The selected bitmap is larger then the image and cant be inserted. Please resize the image first.', mtInformation, [mbOK], 0);

      Tools.SetActive(nil);

      Exit;
    end;

    Background:= CreateTransparentImage(4);
    try
      DrawBitmap(Buffer, Bitmap, Background);
    finally
      Background.Free;
    end;


    ShowHint('Select the position to insert the image and press Enter to finish');
  end else
  begin
    Tools.SetActive(nil);
  end;
  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.Deactivated;
begin
  inherited;

  Editor.Cursor:= crDefault;
end;



//------------------------------------------------------------------------------
procedure TToolPatternImport.Finish;
var Pattern : TPHXPattern;
begin
  Buffer.CopyTo(Editor.Image.Texture.Graphic, TRecti.Create(0, 0, Bitmap.Width, Bitmap.Height), Position );

  Pattern.Name  := ShortString( ChangeFileExt( ExtractFileName(Name), ''));
  Pattern.X     := Position.X;
  Pattern.Y     := Position.Y;
  Pattern.Width := Bitmap.Width;
  Pattern.Height:= Bitmap.Height;

  if ModActions.Settings.CenterPivots then
  begin
    Pattern.Pivot.X:= Pattern.Width  div 2;
    Pattern.Pivot.Y:= Pattern.Height div 2;
  end else
  begin
    Pattern.Pivot.X:= 0;
    Pattern.Pivot.Y:= 0;
  end;

  Pattern.Flip  := False;
  Pattern.Mirror:= False;

  Image.Patterns.Add(Pattern);

  ModActions.Document.Changed;

  ModActions.Selected.Pattern:= Image.Patterns.Count - 1;

  Tools.SetActive(nil);
end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.Cancel;
begin
  Tools.SetActive(nil);
end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    Editor.Cursor:= crCross;
  if ssLeft in Shift then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    if Position.X < 0 then Position.X:= 0;
    if Position.Y < 0 then Position.Y:= 0;

    if Position.X + Buffer.Width  > Image.Width  then Position.X:= Image.Width  - Buffer.Width ;
    if Position.Y + Buffer.Height > Image.Height then Position.Y:= Image.Height - Buffer.Height ;

    Editor.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crCross;

  if ssLeft in Shift then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    if Position.X < 0 then Position.X:= 0;
    if Position.Y < 0 then Position.Y:= 0;

    if Position.X + Buffer.Width  > Image.Width  then Position.X:= Image.Width  - Buffer.Width ;
    if Position.Y + Buffer.Height > Image.Height then Position.Y:= Image.Height - Buffer.Height ;

    Editor.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  Editor.Cursor:= crCross;

  if Key = LCLType.VK_RETURN  then
  begin
    Finish;
  end else
  if Key = LCLType.VK_ESCAPE  then
  begin
    Cancel;
  end else
  if Key = LCLType.VK_LEFT then
  begin
    if Editor.Grid.Enabled then
    begin
      Position.X:= Max(0, Position.X - Editor.Grid.Width);
    end else
    begin
      Position.X:= Max(0, Position.X - 1);
    end;
  end else
  // Move right
  if Key = LCLType.VK_RIGHT then
  begin
    if Editor.Grid.Enabled then
    begin
      Position.X:= Min( Position.X + Editor.Grid.Width, Image.Width  - Buffer.Width);
    end else
    begin
      Position.X:= Min( Position.X + 1                , Image.Width  - Buffer.Width);
    end;
  end else
  if Key = LCLType.VK_UP then
  begin
    if Editor.Grid.Enabled then
    begin
      Position.Y:= Max(0, Position.Y - Editor.Grid.Height);
    end else
    begin
      Position.Y:= Max(0, Position.Y - 1);
    end;
  end else
  if Key = LCLType.VK_DOWN then
  begin
    if Editor.Grid.Enabled then
    begin
      Position.Y:= Min( Position.Y + Editor.Grid.Height, Image.Height  - Buffer.Height);
    end else
    begin
      Position.Y:= Min( Position.Y + 1                 , Image.Height  - Buffer.Height);
    end;
  end;

end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternImport.Paint(Canvas: TCanvas);
var Index: Integer;
var Rect: TRect;
var DrawPosition: TVector2i;
begin
  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    Editor.MaskPattern(Index);

    Editor.DrawPattern(Index, False);
  end;

  DrawPosition:= Editor.DocumentToScreen(Position);

  Rect.Left  := DrawPosition.X;
  Rect.Top   := DrawPosition.Y;
  Rect.Right := DrawPosition.X + Trunc(Bitmap.Width  * Editor.Viewport.Zoom);
  Rect.Bottom:= DrawPosition.Y + Trunc(Bitmap.Height * Editor.Viewport.Zoom);

  Editor.Canvas.StretchDraw(Rect, Bitmap);

  InflateRect(Rect, 1, 1);

  with Editor.Canvas do
  begin
     Brush.Style:= bsClear;

     Pen.Color  := clMaroon;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(Rect);

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(Rect);
  end;


end;

{$ENDREGION}

{$REGION 'TOOL_PATTERN_PACKER'}

// TToolPatternPacker
//==============================================================================
constructor TToolPatternPacker.Create(ATools: TPHXEditorTools);
begin
  inherited;
  Buffer:= TPHXBitmap.Create;
  Bitmap:= TBitmap.Create;
end;

//------------------------------------------------------------------------------
destructor TToolPatternPacker.Destroy;
begin
  Buffer.Free;
  Bitmap.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TToolPatternPacker.GetIdent: Cardinal;
begin
  Result:= TOOL_PATTERN_PACKER;
end;

//------------------------------------------------------------------------------
function TToolPatternPacker.GetName: string;
begin
  Result:= 'Tools.Pattern.Paxker';
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.Activated;
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.Deactivated;
begin
  inherited;

end;



//------------------------------------------------------------------------------
procedure TToolPatternPacker.Finish;
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.Cancel;
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

//------------------------------------------------------------------------------
procedure TToolPatternPacker.Paint(Canvas: TCanvas);
begin

end;

{$ENDREGION}


{$REGION 'TOOL_TAG_MOVE'}

//------------------------------------------------------------------------------
function TToolTagMove.GetIdent: Cardinal;
begin
  Result:= TOOL_TAG_MOVE;
end;

//------------------------------------------------------------------------------
function TToolTagMove.GetName: string;
begin
  Result:= 'Tools.Tag.Move';
end;

//------------------------------------------------------------------------------
procedure TToolTagMove.Activated;
begin
  Started:= False;
  Start.X:= 0;
  Start.Y:= 0;
end;

//------------------------------------------------------------------------------
procedure TToolTagMove.Deactivated;
begin
  inherited;
end;

//------------------------------------------------------------------------------
procedure TToolTagMove.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Index   : Integer;
begin
  if Button <> TMouseButton.mbLeft then
  begin
    Tools.SetActive(nil);
    Exit;
  end;

  Index:= ModTags.SelectedIndex;

  if (Index >= 0) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Started:= True;
    Start.X:= Position.X;
    Start.Y:= Position.Y;
  end;
end;

//------------------------------------------------------------------------------
procedure TToolTagMove.MouseMove(Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
var Index   : Integer;
begin
  Index:= ModTags.SelectedIndex;

  if Started and (Index >= 0) then
  begin
    Position:= Editor.ScreenToDocument(X,Y);

    Editor.Grid.Snap(Position);

    Image.Tags.List^[Index].X:= Image.Tags.List^[Index].X + (Position.X - Start.X);
    Image.Tags.List^[Index].Y:= Image.Tags.List^[Index].Y + (Position.Y - Start.Y);

    ModActions.Document.Changed;

    Editor.Repaint;

    Start.X:= Position.X;
    Start.Y:= Position.Y;
  end;
  Editor.Cursor:= crSizeAll;
end;

//------------------------------------------------------------------------------
procedure TToolTagMove.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Editor.Cursor:= crDefault;

  Tools.SetActive(nil);
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

  //FTools.Add(TToolElementMove  , frmMain.btnToolMove);



  FTools.Add(TToolPatternAdd   , frmMain.btnPatternAdd);
  FTools.Add(TToolPatternPick  , frmMain.btnPatternPick);
  FTools.Add(TToolPatternMove  , frmMain.btnPatternMove);
  FTools.Add(TToolPatternSelect, frmMain.btnPatternSelect);
  FTools.Add(TToolPatternImport, frmMain.btnPatternImport);

  FTools.Add(TToolTagMove, frmMain.btnTagMove);

//  FTools.Add(TToolElementImport, frmMain.btnToolImport);

//  FTools.Add(TToolElementSelectBackground);
//  FTools.Add(TToolElementSelectForeground);
end;

//------------------------------------------------------------------------------
destructor TModTools.Destroy;
begin
  FTools.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TModTools.SetEditor(const Value: TPHXImageEditor);
begin
  if Assigned(FEditor) then
  begin
    FEditor.Tools:= nil;
  end;

  FEditor:= Value;

  FTools.Editor:= FEditor;

  if Assigned(FEditor) then
  begin
    FEditor.Tools:= FTools;
  end;
end;




end.

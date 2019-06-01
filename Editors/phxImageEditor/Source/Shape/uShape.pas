unit uShape;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ImgList, Vcl.Menus,
  Vcl.ComCtrls, Vcl.ActnList, Vcl.ToolWin, System.Math,

  phxEditor,
  phxTypes,
  phxImage,
  phxImageEx,
  phxShape,

  phxGraphics,
  phxGraphicsEx,

  uShapeProperties,
  uShapeTools;

type

//------------------------------------------------------------------------------
TFrmShapeEditor = class(TForm)
    ActionImages: TImageList;
    PanelLeft: TPanel;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    actAddPoint: TAction;
    btnShapeAdd: TToolButton;
    PopupMenu1: TPopupMenu;
    actAddPoint1: TMenuItem;
    btnShapeDelete: TToolButton;
    lwShapes: TListView;
    StatusBar1: TStatusBar;
    PanelClient: TPanel;
    PanelEditor: TPanel;
    actAddLine: TAction;
    actAddCircle: TAction;
    actAddBox: TAction;
    actAddPolygon: TAction;
    actAddPolygon1: TMenuItem;
    actAddCircle1: TMenuItem;
    actAddLine1: TMenuItem;
    actAddBox1: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Shape1: TMenuItem;
    Polygon1: TMenuItem;
    Circle1: TMenuItem;
    Point1: TMenuItem;
    Box1: TMenuItem;
    Line1: TMenuItem;
    actFileSave: TAction;
    actFileSaveList: TAction;
    actFileOpen: TAction;
    actFileOpenList: TAction;
    actFileExit: TAction;
    New1: TMenuItem;
    Polygon2: TMenuItem;
    Circle2: TMenuItem;
    Line2: TMenuItem;
    Box2: TMenuItem;
    Point2: TMenuItem;
    N1: TMenuItem;
    Open1: TMenuItem;
    Openall1: TMenuItem;
    N2: TMenuItem;
    Save1: TMenuItem;
    Saveall1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GroupProperties: TGroupBox;
    GroupShapes: TGroupBox;
    Panel1: TPanel;
    cbPatterns: TComboBox;
    Label1: TLabel;
    PanelTools: TPanel;
    ToolBarZoom: TToolBar;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    btnZoom100: TToolButton;
    ToolButton8: TToolButton;
    ToolBarPattern: TToolBar;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolBar2: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Splitter1: TSplitter;
    OpenDialogList: TOpenDialog;
    SaveDialogList: TSaveDialog;
    ShapeImages: TImageList;
    actDelete: TAction;
    ToolButton1: TToolButton;
    ToolButton7: TToolButton;
    ToolButton9: TToolButton;
    procedure actAddPointExecute(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnZoom100Click(Sender: TObject);
    procedure lwShapesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actAddPolygonExecute(Sender: TObject);
    procedure actAddCircleExecute(Sender: TObject);
    procedure actAddBoxExecute(Sender: TObject);
    procedure actAddLineExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actFileSaveListExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileOpenListExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure cbPatternsChange(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
  private

    FShapes: TPHXShapeList;
    FShape : TPHXShape;
    FImage : TPHXImage;

    FEditor    : TPHXShapeEditor;
    FProperties: TFrmShape;


    procedure SetImage(const Value: TPHXImage);
    procedure SetShape(const Value: TPHXShape);
    procedure cbPatternsUpdate;
    procedure lwShapesUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute(Image: TPHXImage): Boolean;

    procedure Changed;

    property Shape: TPHXShape read FShape write SetShape;
    property Shapes: TPHXShapeList read FShapes;
    property Image: TPHXImage read FImage write SetImage;
    property Editor  : TPHXShapeEditor read FEditor;
  end;

var
  FrmShapeEditor: TFrmShapeEditor;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
function GetKindName(const Shape: TPHXShapeKind): String;
begin
  Result:= '';
  case Shape of
    PHX_SHAPE_POINT  : Result:= 'Point';
    PHX_SHAPE_BOX    : Result:= 'Box';
    PHX_SHAPE_POLYGON: Result:= 'Polygon';
    PHX_SHAPE_CIRCLE : Result:= 'Circle';
    PHX_SHAPE_LINE   : Result:= 'Line';
  end;
end;

// TFrmShapeEditor
//==============================================================================
constructor TFrmShapeEditor.Create(AOwner: TComponent);
begin
  inherited;

  FShapes:= TPHXShapeList.Create;
  FShape := nil;

  FEditor:= TPHXShapeEditor.Create(Self);
  FEditor.Align := alClient;
  FEditor.Parent:= PanelEditor;

  FProperties:= TFrmShape.Create(Self);
  FProperties.Align := alClient;
  FProperties.Parent:= GroupProperties;
end;

//------------------------------------------------------------------------------
destructor TFrmShapeEditor.Destroy;
begin
  FShapes.Free;
  inherited;
end;

//------------------------------------------------------------------------------
function TFrmShapeEditor.Execute(Image: TPHXImage): Boolean;
begin
  SetImage(Image);
  SetShape(nil);

  lwShapesUpdate;

  FEditor.Image:= Image;

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.Changed;
begin
  FEditor.Invalidate;

  lwShapesUpdate;
  //  ModActions.Document.State:= ModActions.Document.State + [dsChanged];
//  ModActions.Editor.Invalidate;
end;


//------------------------------------------------------------------------------
procedure TFrmShapeEditor.lwShapesUpdate;
var Index: Integer;
var Shape: TPHXShape;
var Item : TListItem;
begin
  lwShapes.Items.BeginUpdate;

  if lwShapes.Items.Count <> Shapes.Count then
  begin
    lwShapes.Items.Clear;
    for Index:= 0 to Shapes.Count-1 do
    begin
      Item:= lwShapes.Items.Add;
      Item.Caption:= '';
      Item.SubItems.Add('');
    end;
  end;

  for Index:= 0 to Shapes.Count-1 do
  begin
    Shape:= Shapes[Index];

    Item:= lwShapes.Items[Index];
    Item.Caption:= Shape.Name;
    Item.SubItems[0]:= GetKindName(Shape.Kind);

    Item.ImageIndex:= -1;
    case Shape.Kind of
      PHX_SHAPE_POINT  : Item.ImageIndex:= 0;
      PHX_SHAPE_BOX    : Item.ImageIndex:= 1;
      PHX_SHAPE_POLYGON: Item.ImageIndex:= 2;
      PHX_SHAPE_CIRCLE : Item.ImageIndex:= 3;
      PHX_SHAPE_LINE   : Item.ImageIndex:= 4;
    end;
    Item.StateIndex:= Item.ImageIndex;


    if Shape = FShape then Item.Selected:= True;
  end;

  lwShapes.Items.EndUpdate;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.lwShapesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var Index: Integer;
begin
  Index:= lwShapes.ItemIndex;
  if (Index >= 0) and (Index < Shapes.Count) then
  begin
    SetShape(Shapes[Index]);
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmShapeEditor.cbPatternsUpdate;
var Index: Integer;
begin
  cbPatterns.OnChange:= nil;
  cbPatterns.Items.BeginUpdate;
  cbPatterns.Items.Clear;

  cbPatterns.Items.Add('');
  for Index := 0 to Image.Patterns.Count - 1 do
  begin
    cbPatterns.Items.Add( String(Image.Patterns.List^[Index].Name) );
  end;

  cbPatterns.Items.EndUpdate;

  cbPatterns.ItemIndex:= FEditor.Pattern + 1;

  cbPatterns.OnChange:= cbPatternsChange;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.cbPatternsChange(Sender: TObject);
begin
  if cbPatterns.ItemIndex > 0 then
  begin
    FEditor.Pattern:= cbPatterns.ItemIndex - 1;
  end else
  begin
    FEditor.Pattern:= -1;
  end;
end;


{$REGION 'Shape Actions'}

var CounterPolygon: Integer = 1;
var CounterCircle : Integer = 1;
var CounterPoint  : Integer = 1;
var CounterBox    : Integer = 1;
var CounterLine   : Integer = 1;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actAddPolygonExecute(Sender: TObject);
var Name : String;
var Shape: TPHXPolygon;
begin
  Name := Format('Polygon%d', [CounterPolygon]);
  Shape:= Shapes.AddPolygon(Name);

  SetShape(Shape);

  lwShapesUpdate;

  Inc(CounterPolygon);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actAddCircleExecute(Sender: TObject);
var Name : String;
var Shape: TPHXCircle;
begin
  Name := Format('Circle%d', [CounterCircle]);
  Shape:= Shapes.AddCircle(Name);

  // Create some standard values
  if Editor.RelativeToPattern then
  begin
    Shape.CenterX:= 0;
    Shape.CenterY:= 0;
    Shape.Radius  := Max(Editor.GetPattern.Width, Editor.GetPattern.Height) / 2;
  end;

  SetShape(Shape);

  lwShapesUpdate;

  Inc(CounterCircle);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actAddPointExecute(Sender: TObject);
var Name : String;
var Shape: TPHXPoint;
begin
  Name := Format('Point%d', [CounterPoint]);
  Shape:= Shapes.AddPoint(Name);

  SetShape(Shape);

  lwShapesUpdate;

  Inc(CounterPoint);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actAddBoxExecute(Sender: TObject);
var Name : String;
var Shape: TPHXBox;
begin
  Name := Format('Box%d', [CounterBox]);
  Shape:= Shapes.AddBox(Name);

  // Create some standard values
  if Editor.RelativeToPattern then
  begin
    Shape.CenterX:= 0;
    Shape.CenterY:= 0;
    Shape.SizeX  := Editor.GetPattern.Width;
    Shape.SizeY  := Editor.GetPattern.Height;
  end;

  SetShape(Shape);

  lwShapesUpdate;

  Inc(CounterBox);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actAddLineExecute(Sender: TObject);
var Name : String;
var Shape: TPHXLine;
begin
  Name := Format('Line%d', [CounterLine]);
  Shape:= Shapes.AddLine(Name);

  SetShape(Shape);

  lwShapesUpdate;

  Inc(CounterLine);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actDeleteUpdate(Sender: TObject);
begin
  actDelete.Enabled:= Assigned(FShape);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actDeleteExecute(Sender: TObject);
begin
  if MessageDlg(Format('Delete the shape %s?', [Shape.Name]), mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    FShapes.Remove(FShape);

    lwShapesUpdate;
  end;
end;



{$ENDREGION}

{$REGION 'File actions'}

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actFileUpdate(Sender: TObject);
begin
  actFileSave.Enabled:= Assigned(Shape);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actFileOpenExecute(Sender: TObject);
var Shape: TPHXShape;
begin
  if OpenDialog.Execute then
  begin
    Shape:= Shapes.LoadShape(OpenDialog.FileName);

    SetShape(Shape);

    lwShapesUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actFileSaveExecute(Sender: TObject);
begin
  SaveDialog.FileName:= Shape.Name + PHXSHAPE_EXT;

  if SaveDialog.Execute then
  begin
    Shape.SaveToFile(SaveDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actFileOpenListExecute(Sender: TObject);
begin
  if OpenDialogList.Execute then
  begin
    SetShape(nil);
  //  D:\Program\Programmering\Phoenix\Design\Documentation\tutorials\_Binary

    Shapes.LoadFromFile(OpenDialogList.FileName);

    lwShapesUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actFileSaveListExecute(Sender: TObject);
begin
  if SaveDialogList.Execute then
  begin
    Shapes.SaveToFile(SaveDialogList.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.actFileExitExecute(Sender: TObject);
begin
  ModalResult:= mrOk;
end;



{$ENDREGION}


//------------------------------------------------------------------------------
procedure TFrmShapeEditor.btnZoomInClick(Sender: TObject);
begin
  FEditor.Viewport.ZoomIn;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.btnZoomOutClick(Sender: TObject);
begin
  FEditor.Viewport.ZoomOut;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.btnZoom100Click(Sender: TObject);
begin
  FEditor.Viewport.Zoom100;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.SetImage(const Value: TPHXImage);
begin
  FImage := Value;

  if Assigned(Image) then
  begin
    cbPatterns.Enabled:= True;

    cbPatternsUpdate;
  end else
  begin
    cbPatterns.Enabled:= False;

    cbPatterns.Items.Clear;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeEditor.SetShape(const Value: TPHXShape);
begin
  FShape:= Value;

  FProperties.Shape:= Value;
  FEditor    .Shape:= Value;
end;






end.

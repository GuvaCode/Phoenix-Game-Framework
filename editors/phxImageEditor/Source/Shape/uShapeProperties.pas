unit uShapeProperties;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Spin,
  Generics.Collections,
  phxTypes,
  phxShape,
  phxImage,
  uActions;

type

TPHXShapeClass = class of TPHXShape;

TEditorClass = class of TFrame;

//------------------------------------------------------------------------------
IShapeEditor = interface
  ['{B0D0FF18-0F30-4C16-A8FA-0D71CD4D7B26}']
  procedure SetShape(Shape: TPHXShape);
end;

//------------------------------------------------------------------------------
TFrmShape = class(TFrame)
    Panel1: TPanel;
    Label1: TLabel;
    edName: TEdit;
    edKind: TEdit;
    Label4: TLabel;
    procedure edNameChange(Sender: TObject);
  private
    FShape: TPHXShape;
    FEditor: TFrame;

    procedure Changed;

    procedure EnableEvents(Enabled: Boolean);
    procedure EnableEditors(Enabled: Boolean);

    procedure SetShape(const Value: TPHXShape);
  public
    constructor Create(AOwner: TComponent); override;

    property Shape: TPHXShape read FShape write SetShape;
  end;

procedure RegisterShapeEditor(Shape: TPHXShapeClass; Editor: TEditorClass);

implementation

{$R *.dfm}

uses uShape;

var EditorRegistry: TDictionary<TPHXShapeClass, TEditorClass>;

//------------------------------------------------------------------------------
procedure RegisterShapeEditor(Shape: TPHXShapeClass; Editor: TEditorClass);
begin
  EditorRegistry.Add(Shape, Editor);
end;

//------------------------------------------------------------------------------
function GetKindName(const Shape: TPHXShapeKind): String;
begin
  Result:= '';
  case Shape of
    PHX_SHAPE_POINT  : Result:= 'PHX_SHAPE_POINT';
    PHX_SHAPE_BOX    : Result:= 'PHX_SHAPE_BOX';
    PHX_SHAPE_POLYGON: Result:= 'PHX_SHAPE_POLYGON';
    PHX_SHAPE_CIRCLE : Result:= 'PHX_SHAPE_CIRCLE';
    PHX_SHAPE_LINE   : Result:= 'PHX_SHAPE_LINE';
  end;
end;

// TFrmShapeEditor
//==============================================================================
constructor TFrmShape.Create(AOwner: TComponent);
begin
  inherited;
  FEditor:= nil;

  EnableEditors(False);
end;

//------------------------------------------------------------------------------
procedure TFrmShape.SetShape(const Value: TPHXShape);
var EditorClass: TEditorClass;
var EditorIF   : IShapeEditor;
begin
  if FShape <> Value then
  begin

    // Free the current editor
    if Assigned(FEditor) then
    begin
      if Supports(FEditor, IShapeEditor, EditorIF) then
      begin
        EditorIF.SetShape(nil);
        EditorIF:= nil;
      end;

      FEditor.Free;
      FEditor:= nil;
    end;

    FShape:= Value;

    EnableEvents(False);

    // TPHXShape
    if Assigned(Shape) then
    begin
      EnableEditors(True);

      edName.Text:= Shape.Name;
      edKind.Text:= GetKindName(Shape.Kind);

      if EditorRegistry.TryGetValue( TPHXShapeClass(Shape.ClassType), EditorClass)  then
      begin
        FEditor:= EditorClass.Create(Self);
        FEditor.Parent:= Self;
        FEditor.Align := alClient;

        if Supports(FEditor, IShapeEditor, EditorIF) then
        begin
          EditorIF.SetShape(FShape);
          EditorIF:= nil;
        end;
      end;

      EnableEvents(True);
    end else
    begin
      EnableEditors(False);

      edName.Text:= '';
      edKind.Text:= '';
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShape.Changed;
begin
  FrmShapeEditor.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmShape.EnableEditors(Enabled: Boolean);
const EnabledColors : array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edName.Enabled:= Enabled;
  edName.Color  := EnabledColors[Enabled];
end;

//------------------------------------------------------------------------------
procedure TFrmShape.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edName.OnChange:= edNameChange;
  end else
  begin
    edName.OnChange:= nil;
   end;
end;


//------------------------------------------------------------------------------
procedure TFrmShape.edNameChange(Sender: TObject);
var Value: String;
begin
  Value:= edName.Text;

  if Shape.Name <> Value then
  begin
    Shape.Name:= Value;

    Changed;
  end;
end;







initialization
  EditorRegistry:= TDictionary<TPHXShapeClass, TEditorClass>.Create;
finalization
  EditorRegistry.Free;
end.

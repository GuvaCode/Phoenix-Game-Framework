unit uShapeLine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin,

  phxShape,
  phxTypes,

  uShapeProperties;

type

//------------------------------------------------------------------------------
TFrmShapeLine = class(TFrame, IShapeEditor)
    Label1: TLabel;
    edMinY: TJvSpinEdit;
    edMinX: TJvSpinEdit;
    edMaxX: TJvSpinEdit;
    edMaxY: TJvSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure edMinYChange(Sender: TObject);
    procedure edMaxXChange(Sender: TObject);
    procedure edMinXChange(Sender: TObject);
    procedure edMaxYChange(Sender: TObject);
  private
  private
    FShape: TPHXLine;

    procedure Changed;

    procedure EnableEditors(Enabled: Boolean);
    procedure EnableEvents(Enabled: Boolean);

    procedure SetShape(Value: TPHXLine); overload;
    procedure SetShape(Value: TPHXShape); overload;
  public
    constructor Create(AOwner: TComponent); override;

    property Shape: TPHXLine read FShape write SetShape;
  end;

implementation

{$R *.dfm}

uses uShape;

//------------------------------------------------------------------------------
constructor TFrmShapeLine.Create(AOwner: TComponent);
begin
  inherited;

  EnableEditors(False);
end;

//------------------------------------------------------------------------------
procedure TFrmShapeLine.Changed;
begin
  FrmShapeEditor.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeLine.EnableEvents(Enabled: Boolean);
begin
  if Enabled then
  begin
    edMinX.OnChange:= edMinXChange;
    edMaxX.OnChange:= edMaxXChange;

    edMinY.OnChange:= edMinYChange;
    edMaxY.OnChange:= edMaxYChange;
  end else
  begin
    edMinX.OnChange:= nil;
    edMaxX.OnChange:= nil;
    edMinY.OnChange:= nil;
    edMaxY.OnChange:= nil;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeLine.EnableEditors(Enabled: Boolean);
const EnabledColors : array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  edMinX.Enabled:= Enabled;
  edMinX.Color  := EnabledColors[Enabled];

  edMaxX.Enabled:= Enabled;
  edMaxX.Color  := EnabledColors[Enabled];

  edMinY.Enabled:= Enabled;
  edMinY.Color  := EnabledColors[Enabled];

  edMaxY.Enabled:= Enabled;
  edMaxY.Color  := EnabledColors[Enabled];
end;



procedure TFrmShapeLine.edMinXChange(Sender: TObject);
begin
  Shape.Min:= Vector2f( edMinX.Value, edMinY.Value);

  Changed;
end;

procedure TFrmShapeLine.edMinYChange(Sender: TObject);
begin
  Shape.Min:= Vector2f( edMinX.Value, edMinY.Value);

  Changed;
end;


procedure TFrmShapeLine.edMaxXChange(Sender: TObject);
begin
  Shape.Max:= Vector2f( edMaxX.Value, edMaxY.Value);

  Changed;
end;


procedure TFrmShapeLine.edMaxYChange(Sender: TObject);
begin
  Shape.Max:= Vector2f( edMaxX.Value, edMaxY.Value);

  Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeLine.SetShape(Value: TPHXLine);
begin
  EnableEvents(False);

  FShape := Value;

  if Assigned(Shape) then
  begin
    EnableEditors(True);

    edMinX.Value:= Shape.Min.X;
    edMinY.Value:= Shape.Min.Y;
    edMaxX.Value:= Shape.Max.X;
    edMaxY.Value:= Shape.Max.Y;

    EnableEvents(True);
  end else
  begin
    EnableEvents(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmShapeLine.SetShape(Value: TPHXShape);
begin
  if Value is TPHXLine then
  begin
    SetShape(TPHXLine(Value));
  end else
  begin
    SetShape(nil);
  end;
end;


initialization
  RegisterShapeEditor(TPHXLine, TFrmShapeLine);
end.

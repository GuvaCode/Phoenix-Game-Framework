unit uInspector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  JvExControls, JvInspector,

  Generics.Collections,

  uProject, JvComponentBase;

type

//------------------------------------------------------------------------------
TPropertyDesc = class
  private
    FItems: TDictionary<String,String>;

    function GetDescription(Instance: TObject; const Prop: String): String;
  public
    constructor Create;
    destructor Destroy; override;

    property Description[Instance: TObject; const Prop: String]: String read GetDescription; default;
  end;

//------------------------------------------------------------------------------
TJvInspectorEx = class helper for TJvInspector
  public
  //  property Color;
  end;

//------------------------------------------------------------------------------
TFrmInspector = class(TFrame)
    cbControls: TComboBox;
    Inspector: TJvInspector;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    lblProperty: TLabel;
    lblDescription: TLabel;
    InspectorPainter: TJvInspectorDotNETPainter;
    procedure cbControlsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbControlsChange(Sender: TObject);
    procedure InspectorItemSelected(Sender: TObject);
    procedure InspectorDataValueChanged(Sender: TObject;
      Data: TJvCustomInspectorData);
  private
    FControl: TGuiControl;
    FProject: TGuiProject;

    FPropertyDesc: TPropertyDesc;

    procedure SetControl(const Value: TGuiControl);
    procedure SetProject(const Value: TGuiProject);
    procedure cbControlsUpdate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    property Project: TGuiProject read FProject write SetProject;
    property Control: TGuiControl read FControl write SetControl;
  end;

var FrmInspector: TFrmInspector;

implementation

{$R *.dfm}

uses uActions;

{$REGION 'TPropertyDesc'}

// TPropertyDesc
//------------------------------------------------------------------------------
constructor TPropertyDesc.Create;
begin
  FItems:= TDictionary<String,String>.Create;

  // TPHXGuiControl
  FItems.Add('TGuiControl.Name'  , 'Name of the control.');
  FItems.Add('TGuiControl.X'     , 'Horisontal position of the control.');
  FItems.Add('TGuiControl.Y'     , 'Vertical position of the control.');
  FItems.Add('TGuiControl.Width' , 'Horisontal size of the control.');
  FItems.Add('TGuiControl.Height', 'Vertical size of the control.');

  // TPHXButton
  FItems.Add('TGuiButton.Caption', 'The text of the button');
  FItems.Add('TGuiButton.Event'   , 'Name of the event procedure to call when the button is clicked');

  // TPHXLabel
  FItems.Add('TGuiText.Text'     , 'Text expression of the label.');
end;

//------------------------------------------------------------------------------
destructor TPropertyDesc.Destroy;
begin
  FItems.Free;
  inherited;
end;
//------------------------------------------------------------------------------
function TPropertyDesc.GetDescription(Instance: TObject; const Prop: String): String;
var Name: String;
var Parent: TClass;
begin
  Name:= Instance.ClassName  + '.' +  Prop;

  if FItems.ContainsKey(Name) then
  begin
    Result:= FItems[Name];
  end else
  begin
    Parent:= Instance.ClassParent;

    while Assigned(Parent) do
    begin
      Name:= Parent.ClassName + '.' +  Prop;

      if FItems.ContainsKey(Name) then
      begin
        Result:= FItems[Name];

        Exit;
      end;

      Parent:= Parent.ClassParent;
    end;

    Result:= '';
  end;
end;

{$ENDREGION}

// TFrmInspector
//==============================================================================
constructor TFrmInspector.Create(AOwner: TComponent);
begin
  inherited;

  FPropertyDesc:= TPropertyDesc.Create;

  lblProperty   .Caption:= '';
  lblDescription.Caption:= '';

  SetProject(nil);
end;

//------------------------------------------------------------------------------
destructor TFrmInspector.Destroy;
begin
  FPropertyDesc.Free;
  inherited;
end;

{$REGION 'cbControls'}

//------------------------------------------------------------------------------
procedure TFrmInspector.cbControlsUpdate;
var Index  : Integer;
var Control: TGuiControl;
begin
  cbControls.OnChange:= nil;

  cbControls.Items.BeginUpdate;
  for Index := 0 to Project.Controls.Count - 1 do
  begin
    Control:= Project.Controls[Index];

    cbControls.Items.AddObject(Control.Name, Control);

  end;
  cbControls.Items.EndUpdate;

  cbControls.OnChange:= cbControlsChange;
end;

//------------------------------------------------------------------------------
procedure TFrmInspector.cbControlsChange(Sender: TObject);
var Control: TGuiControl;
begin
  cbControls.OnChange:= nil;

  if cbControls.ItemIndex >= 0 then
  begin
    Control:= TGuiControl( cbControls.Items.Objects[cbControls.ItemIndex] );

    ModActions.Control:= Control;
  end;

  cbControls.OnChange:= cbControlsChange;
end;

//------------------------------------------------------------------------------
procedure TFrmInspector.cbControlsDrawItem(Control: TWinControl; Index: Integer;Rect: TRect; State: TOwnerDrawState);
var AControl: TGuiControl;
var W: Integer;
begin
  AControl:= TGuiControl(cbControls.Items.Objects[Index]);

  with (Control as TComboBox).Canvas do
  begin
    FillRect(Rect);

    Font.Style:= [fsBold];
    Font.Color:= clBlack;

    TextOut(Rect.Left + 4, Rect.Top, AControl.Name );

    W:= TextWidth(AControl.Name);

    Font.Style:= [];
    Font.Color:= clSilver;

    TextOut(Rect.Left + 4 + W + 8, Rect.Top, AControl.ClassName );
  end;
end;
{$ENDREGION}

{$REGION 'JvInspector'}

//------------------------------------------------------------------------------
procedure TFrmInspector.InspectorItemSelected(Sender: TObject);
var Name: String;
begin
  if Assigned(Inspector.Selected) then
  begin
    Name:= Inspector.Selected.Data.Name;

    lblProperty   .Caption:= Name;
    lblDescription.Caption:= FPropertyDesc[Inspector.InspectObject, Inspector.Selected.Data.Name];
  end else
  begin
    lblProperty   .Caption:= '';
    lblDescription.Caption:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmInspector.InspectorDataValueChanged(Sender: TObject; Data: TJvCustomInspectorData);
begin
 // ModActions.Project.Changed;
end;

{$ENDREGION}


//------------------------------------------------------------------------------
procedure TFrmInspector.SetProject(const Value: TGuiProject);
begin
  if FProject <> Value then
  begin
    FProject := Value;

    SetControl(nil)
  end;

  if Assigned(Project) then
  begin
    cbControls.Enabled:= True;
    cbControls.Color  := clWindow;

    Inspector.Enabled:= True;
    InspectorPainter.BackgroundColor:= clWindow;

    cbControlsUpdate;
  end else
  begin
    cbControls.Enabled:= False;
    cbControls.Color  := clBtnFace;

    Inspector.Enabled:= False;
    InspectorPainter.BackgroundColor:= clBtnFace;

    cbControls.Items.Clear;
  end;

end;

//------------------------------------------------------------------------------
procedure TFrmInspector.SetControl(const Value: TGuiControl);
var Index: Integer;
begin
  FControl := Value;

  if Assigned(Project) then
  begin
    Index:= Project.Controls.IndexOf(FControl);

    cbControls.ItemIndex:= Index;
  end;

  Inspector.InspectObject:= Value;
end;


end.

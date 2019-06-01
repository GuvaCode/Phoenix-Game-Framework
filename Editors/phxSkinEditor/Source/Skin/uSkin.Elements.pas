unit uSkin.Elements;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.ImgList, Vcl.ActnList, Vcl.Menus, Vcl.ExtCtrls,

  JvExMask, JvSpin,

  uActions,

  phxSkin,
  phxSkinEx,

  uElementName;

const
  ICON_SKIN          =  0;
  ICON_CONTROL       =  1;
  ICON_BUTTON        =  2;
  ICON_LABEL         =  3;
  ICON_WINDOW        =  4;
  ICON_IMAGE         =  5;
  ICON_ROOT          =  6;
  ICON_EDIT          =  7;
  ICON_SCROLLBAR     = 08;
  ICON_LISTBOX       = 09;
  ICON_CHECKBOX      = 10;
  ICON_RADIOBUTTON   = 11;
  ICON_SLIDER        = 12;
  ICON_GRID          = 13;
  ICON_PAGECONTROL   = 14;
  ICON_PANEL         = 15;
  ICON_COMBOBOX      = 16;

type

//------------------------------------------------------------------------------
TFrmSkinElements = class(TFrame)
    ListImages: TImageList;
    twElements: TTreeView;
    ToolBar1: TToolBar;
    btnElementAdd: TToolButton;
    btnElementDelete: TToolButton;
    ToolButton2: TToolButton;
    btnDuplicate: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ListActions: TActionList;
    actElementAdd: TAction;
    actElementDel: TAction;
    actElementUp: TAction;
    actElementDown: TAction;
    actElementDuplicate: TAction;
    ListPopup: TPopupMenu;
    MenuAdd: TMenuItem;
    MenuDelete: TMenuItem;
    Panel1: TPanel;
    ControlImages: TImageList;

    procedure twElementsChange(Sender: TObject; Node: TTreeNode);

    procedure actElementUpdate(Sender: TObject);
    procedure actElementAddExecute(Sender: TObject);
    procedure actElementDelExecute(Sender: TObject);
    procedure actElementDuplicateExecute(Sender: TObject);
    procedure actElementDownExecute(Sender: TObject);
    procedure actElementUpExecute(Sender: TObject);
  private
    FSkin: TPHXSkin;

   // FrmPattern: TfrmPattern;
    FActive: Boolean;

    FSelectedIndex: Integer;
    FSelectedElement: TPHXSkinElement;
    FEditor: TPHXSkinEditor;

    function FindParent(Element: TPHXSkinElement): TTreeNode;

    procedure twElementsUpdate;

    procedure EnableControls(Enabled: Boolean);

    procedure SetSkin(const Value: TPHXSkin);
    procedure SetActive(const Value: Boolean);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetSelectedElement(const Value: TPHXSkinElement);
  public
    constructor Create(AOwner: TComponent); override;

    Property Skin: TPHXSkin read FSkin write SetSkin;
    property Active: Boolean read FActive write SetActive;
    property Editor: TPHXSkinEditor read FEditor write FEditor;

    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property SelectedElement: TPHXSkinElement read FSelectedElement write SetSelectedElement;
  end;

implementation

{$R *.dfm}


//------------------------------------------------------------------------------
constructor TFrmSkinElements.Create(AOwner: TComponent);
begin
  inherited;
    {
  edControls.Items.BeginUpdate;
  edControls.Items.Clear;
  edControls.Items.Add('< all controls >');
  TPHXSkinFactory.DefaultControls(edControls.Items);
  edControls.Items.EndUpdate;

  edControls.ItemIndex:= 0;

      }
  EnableControls(False);
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.SetActive(const Value: Boolean);
begin
  FActive := Value;

  if Active then
  begin
    actElementAdd.ShortCut:= ShortCut(VK_INSERT, []);
    actElementDel.ShortCut:= ShortCut(VK_DELETE, []);
  end else
  begin
    actElementAdd.ShortCut:= ShortCut(0, []);
    actElementDel.ShortCut:= ShortCut(0, []);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.SetSkin(const Value: TPHXSkin);
begin
  if FSkin <> Value then
  begin
    FSkin:= Value;

    FSelectedIndex:= -1;
  end;

  if Assigned(Skin) then
  begin
    twElements.Enabled:= True;
    twElements.Color  := clWindow;

    twElementsUpdate
  end else
  begin
    twElements.Enabled:= False;
    twElements.Color  := clBtnFace;

    twElements.Items.BeginUpdate;
    twElements.Items.Clear;
    twElements.Items.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.SetSelectedElement(const Value: TPHXSkinElement);
begin
  FSelectedElement:= Value;
  FSelectedIndex  := Skin.Elements.IndexOf(FSelectedElement);

  Editor.Selection.idxElement:= FSelectedIndex;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.SetSelectedIndex(const Value: Integer);
begin
  FSelectedIndex  := Value;
  FSelectedElement:= Skin.Elements[FSelectedIndex];

  Editor.Selection.idxElement:= FSelectedIndex;
end;



//------------------------------------------------------------------------------
function TFrmSkinElements.FindParent(Element: TPHXSkinElement): TTreeNode;
var Index: Integer;
var Name : String;
var Item : TTreeNode;
begin
  Name:= GetControlName(Element.Name);

  if Length(Name) = 0 then
  begin
    Result:= nil;
    Exit;
  end;

  for Index:= 0 to twElements.Items.Count-1 do
  begin
    Item:= twElements.Items[Index];

    if SameText(Item.Text, Name) then
    begin
      Result:= Item;
      Exit;
    end;
  end;

  Result:= twElements.Items.AddChild(nil, Name);
  Result.ImageIndex:= ModActions.ImageForControl( Name);
  Result.StateIndex:= Result.ImageIndex;
  Result.SelectedIndex:= Result.ImageIndex;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.twElementsUpdate;
var Index   : Integer;
var Element : TPHXSkinElement;
var Parent  : TTreeNode;
var Node    : TTreeNode;
begin
  if Skin = nil then Exit;

  twElements.OnChange:= nil;
//  Editor.Skin.GetControlNames(

  twElements.Items.BeginUpdate;
  twElements.Items.Clear;
  for Index:=0 to Skin.Elements.Count-1 do
  begin
    Element:= Skin.Elements[Index];

    Parent:= FindParent(Element);

    Node:= twElements.Items.AddChild(Parent, GetPartName(Element.Name));
    Node.Data         := Element;
    Node.Selected     := Index = FSelectedIndex;
    Node.ImageIndex   := 0;
    Node.SelectedIndex:= 0;
    Node.StateIndex   := 0;

    if Assigned(Parent) and (FSelectedIndex = Index) then
    begin
      Parent.Expand(True);
   end;

  end;
  twElements.Items.EndUpdate;

  if Assigned(twElements.Selected) then
  begin
    twElements.Selected.MakeVisible;
  end;


  twElements.OnChange:= twElementsChange;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.twElementsChange(Sender: TObject; Node: TTreeNode);
var Element: TPHXSkinElement;
begin
  if Node.Data = nil then
  begin
    FSelectedIndex   := -1;
    FSelectedElement:= nil;
  end else
  begin
    Element:= TPHXSkinElement(Node.Data);

    FSelectedIndex  := Skin.Elements.IndexOf(Element);
    FSelectedElement:= Element;
  end;

  Editor.Selection.idxElement:= FSelectedIndex;

  Editor.Invalidate;
end;



//------------------------------------------------------------------------------
procedure TFrmSkinElements.EnableControls(Enabled: Boolean);
const EnabledColors: Array[Boolean] of TColor = ( clBtnFace, clWindow);
begin {
  edPatternName        .Enabled:= Enabled;
  edPatternX           .Enabled:= Enabled;
  edPatternY           .Enabled:= Enabled;
  edPatternWidth       .Enabled:= Enabled;
  edPatternHeight      .Enabled:= Enabled;
  edPatternPivotX      .Enabled:= Enabled;
  edPatternPivotY      .Enabled:= Enabled;
  btnCenterPivot.Enabled:= Enabled;

  edPatternName   .Color:= EnabledColors[Enabled];
  edPatternX      .Color:= EnabledColors[Enabled];
  edPatternY      .Color:= EnabledColors[Enabled];
  edPatternWidth  .Color:= EnabledColors[Enabled];
  edPatternHeight .Color:= EnabledColors[Enabled];
  edPatternPivotX .Color:= EnabledColors[Enabled];
  edPatternPivotY .Color:= EnabledColors[Enabled];   }
end;

{$REGION 'Element actions'}

//------------------------------------------------------------------------------
procedure Swap(var A,B: TPHXSkinElement);
var T: TPHXSkinElement;
begin
  T:= A;
  A:= B;
  B:= T;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.actElementUpdate(Sender: TObject);
begin
  actElementAdd.Enabled   := Assigned(Skin);

  actElementDel      .Enabled:= Assigned(Skin) and (FSelectedIndex >= 0);
  actElementDuplicate.Enabled:= Assigned(Skin) and (FSelectedIndex >= 0);
  actElementUp       .Enabled:= Assigned(Skin) and (FSelectedIndex >= 0) and (FSelectedIndex >= 1);
  actElementDown     .Enabled:= Assigned(Skin) and (FSelectedIndex >= 0) and (FSelectedIndex < Skin.Elements.Count - 1);
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.actElementAddExecute(Sender: TObject);
var Element: TPHXSkinElement;
var Dialog : TfrmElementName;
begin

  Dialog:= TfrmElementName.Create(Self);
  try
    Dialog.Caption:= 'New skin element';

    if Assigned(twElements.Selected) then
    begin

      if Assigned(twElements.Selected.Parent) then
      begin
        Dialog.ElementControl:= twElements.Selected.Parent.Text;
        Dialog.ElementPart   := '';
      end else
      begin
        Dialog.ElementControl:= twElements.Selected.Text;
        Dialog.ElementPart   := '';
      end;

    end;

    if Dialog.Execute then
    begin
      Element:= ModActions.Document.Skin.Elements.Add;
      Element.Name:= Dialog.ElementName;

      SetSelectedElement(Element);

      ModActions.Document.Changed;
    end;
  finally
    Dialog.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.actElementDelExecute(Sender: TObject);
begin
  if Assigned(SelectedElement) then
  begin
    if MessageDlg( Format('Delete element %s?', [ SelectedElement.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrOk then
    begin
      Skin.Elements.Delete(SelectedIndex );

      ModActions.Document.Changed;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.actElementDuplicateExecute(Sender: TObject);
var Element: TPHXSkinElement;
begin
  Element:= ModActions.Document.Skin.Elements.Add;
  Element.Name       := SelectedElement.Name + '1';
  Element.Bounds     := SelectedElement.Bounds;
  Element.Margins    := SelectedElement.Margins;
  Element.Shadow     := SelectedElement.Shadow;
  Element.TextPadding:= SelectedElement.TextPadding;
  Element.TextColor  := SelectedElement.TextColor;

  SetSelectedElement(Element);

  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.actElementUpExecute(Sender: TObject);
begin

  if (FSelectedIndex >= 1) and (FSelectedIndex < Skin.Elements.Count) then
  begin
    Swap( Skin.Elements.List^[FSelectedIndex], Skin.Elements.List^[FSelectedIndex-1]);

    SetSelectedIndex(FSelectedIndex - 1);

    ModActions.Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmSkinElements.actElementDownExecute(Sender: TObject);
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < Skin.Elements.Count - 1) then
  begin
    Swap( Skin.Elements.List^[FSelectedIndex], Skin.Elements.List^[FSelectedIndex+1]);

    SetSelectedIndex(FSelectedIndex + 1);

    ModActions.Document.Changed;
  end;
end;

{$ENDREGION}



end.

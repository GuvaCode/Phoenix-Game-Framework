unit uPalette;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.CategoryButtons,

  uActions,
  uProject,
  uControls;

type

//------------------------------------------------------------------------------
TFrmPalette = class(TFrame)
    CategoryButtons1: TCategoryButtons;
    procedure CategoryButtons1MouseMove(Sender: TObject; Shift: TShiftState; X,   Y: Integer);
  private
    ItemText    : TButtonItem;
    ItemButton   : TButtonItem;
    ItemWindow   : TButtonItem;
//    ItemDialog   : TButtonItem;

    ItemEdit     : TButtonItem;
    ItemSlider   : TButtonItem;


    procedure CreateButtons;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Return if a drag source is from the palette
    function IsDragSource(Source: TObject): Boolean;
    // Create a control
    function CreateControl(Project: TGuiProject): TGuiControl;
  end;

var FrmPalette: TFrmPalette;

implementation

{$R *.dfm}

// TFrmPalette
//==============================================================================
constructor TFrmPalette.Create(AOwner: TComponent);
begin
  inherited;

  CategoryButtons1.Images:= ModActions.ControlImages;

  CreateButtons;
end;


//------------------------------------------------------------------------------
destructor TFrmPalette.Destroy;
begin
  inherited;
end;


//------------------------------------------------------------------------------
procedure TFrmPalette.CreateButtons;
var Category: TButtonCategory;
begin
  CategoryButtons1.Categories.Clear;


  //  ItemLabel    : TButtonItem;
  //  ItemImage    : TButtonItem;
   // ItemRectangle: TButtonItem;
   // ItemLine     : TButtonItem;
   // ItemBarcode  : TButtonItem;
  Category:= CategoryButtons1.Categories.Add;
  Category.Caption:= 'Common';

    ItemText := Category.Items.Add;
    ItemText.Caption      := 'Text';
    ItemText.Hint         := 'Add a text to the project';
    ItemText.ImageIndex   := ICON_LABEL;

    ItemButton := Category.Items.Add;
    ItemButton.Caption      := 'Button';
    ItemButton.Hint         := 'Add a button  to the project';
    ItemButton.ImageIndex   := ICON_BUTTON;

  Category:= CategoryButtons1.Categories.Add;
  Category.Caption:= 'Containers';

    ItemWindow := Category.Items.Add;
    ItemWindow.Caption      := 'Window';
    ItemWindow.Hint         := 'Add a window to the project';
    ItemWindow.ImageIndex   := ICON_WINDOW;
              (*
    ItemDialog := Category.Items.Add;
    ItemDialog.Caption      := 'Dialog';
    ItemDialog.Hint         := 'Add a dialog to the project';
    ItemDialog.ImageIndex   := ICON_WINDOW;
    *)

  Category:= CategoryButtons1.Categories.Add;
  Category.Caption:= 'Edit controls';

    ItemEdit := Category.Items.Add;
    ItemEdit.Caption      := 'Edit';
    ItemEdit.Hint         := 'Add a edit to the project';
    ItemEdit.ImageIndex   := ICON_EDIT;
      (*
    ItemSlider := Category.Items.Add;
    ItemSlider.Caption      := 'Slider';
    ItemSlider.Hint         := 'Add a slider to the project';
    ItemSlider.ImageIndex   := ICON_SLIDER;
    *(

      (*
  Category:= CategoryButtons1.Categories.Add;
  Category.Caption:= 'All Phoenix Controls';

    Controls:= TStringList.Create;
    try
      EnumerateControls(Controls);
      for Index := 0 to Controls.Count - 1 do
      begin
        Item := Category.Items.Add;
        Item.Data         := Pointer(1);
        Item.Caption      := Controls[Index];
        Item.Hint         := '';
        Item.ImageIndex   := ImageForControl(Item.Caption);
      end;
      Category.Collapsed:= True;
    finally
      Controls.Free;
    end;   *)
end;

//------------------------------------------------------------------------------
function TFrmPalette.CreateControl(Project: TGuiProject): TGuiControl;
begin
  // TGuiWindow
  if CategoryButtons1.SelectedItem = ItemWindow then
  begin
    Result:= TGuiWindow.Create(Project);
  end else
  // TGuiText
  if CategoryButtons1.SelectedItem = ItemText then
  begin
    Result:= TGuiText.Create(Project);
  end else
  // TGuiButton
  if CategoryButtons1.SelectedItem = ItemButton then
  begin
    Result:= TGuiButton.Create(Project);
  end else
  // TGuiEdit
  if CategoryButtons1.SelectedItem = ItemEdit then
  begin
    Result:= TGuiEdit.Create(Project);
  end else
  begin
    Result:= nil;
  end;
end;


//------------------------------------------------------------------------------
function TFrmPalette.IsDragSource(Source: TObject): Boolean;
begin
  Result:= (Source = CategoryButtons1);// or Source(Self);
end;


//------------------------------------------------------------------------------
procedure TFrmPalette.CategoryButtons1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if  Assigned(ModActions.Project) and (ssLeft in Shift) then
  begin
    CategoryButtons1.SelectedItem:= CategoryButtons1.GetButtonAt(X,Y);

    if Assigned(CategoryButtons1.SelectedItem) then
    begin
      CategoryButtons1.BeginDrag(True);
    end;
  end;
end;


end.

unit uTag.List;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, ToolWin,

  Spin,

  phxTypes,
  phxImage,

  uActions,

  uTag.Properties,
  uTag.Actions;

type

//------------------------------------------------------------------------------

{ TFrmTagList }

TFrmTagList = class(TFrame, ITagList)
    lwTags: TListView;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    PatternAdd: TToolButton;
    PatternDel: TToolButton;
    ToolButton1: TToolButton;
    PatternDown: TToolButton;
    PatternUp: TToolButton;
    ToolButton3: TToolButton;
    btnTagSort: TToolButton;
    ToolButton2: TToolButton;
    Panel1: TPanel;
    procedure lwTagsClick(Sender: TObject);
    procedure lwTagsDblClick(Sender: TObject);
    procedure lwTagsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lwTagsSelectItem(Sender: TObject; Item: TListItem;   Selected: Boolean);
  private
    FActive: Boolean;
    FImage: TPHXImage;
    FTag  : Integer;

    FrmTag: TFrmTagProperties;

    function GetSelected: Integer;

    procedure lwTagsUpdate;
    procedure lwTagsSelected;

    procedure SetSelected(const Value: Integer);
    procedure SetTag(const Value: Integer);
    procedure SetImage(const Value: TPHXImage);
    procedure SetActive(const Value: Boolean);
  public
  public
    constructor Create(AOwner: TComponent); override;

    Property Image: TPHXImage read FImage write SetImage;
    property Tag: Integer read FTag write SetTag;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

{$R *.dfm}


// TFrmImageTags
//------------------------------------------------------------------------------
constructor TFrmTagList.Create(AOwner: TComponent);
begin
  inherited;
  FrmTag:= TFrmTagProperties.Create(Self);
  FrmTag.Parent:= Panel1;
  FrmTag.Align := alClient;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.SetTag(const Value: Integer);
begin
  lwTags.OnSelectItem:= nil;
  try
    FTag:= Value;

    FrmTag.Tag:= FTag;

    if (Value >= 0) and (Value < lwTags.Items.Count) then
    begin
      lwTags.ItemIndex:= Value;
    end else
    begin
      lwTags.ItemIndex:= -1;
    end;

  finally
    lwTags.OnSelectItem:= lwTagsSelectItem;
  end;

  ModActions.Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.SetActive(const Value: Boolean);
begin
  FActive := Value;

  if Active then
  begin
    //ModPattern.actPatternAdd.ShortCut:= ShortCut(VK_INSERT, []);
    //ModPattern.actPatternDel.ShortCut:= ShortCut(VK_DELETE, []);
  end else
  begin
    //ModPattern.actPatternAdd.ShortCut:= ShortCut(0, []);
   // ModPattern.actPatternDel.ShortCut:= ShortCut(0, []);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.SetImage(const Value: TPHXImage);
begin
  if FImage <> Value then
  begin
    FImage:= Value;
    FTag  := -1;

    FrmTag.Image:= FImage;
    FrmTag.Tag  := FTag;
  end;

  if Assigned(Image) then
  begin
    lwTags.Enabled:= True;
    lwTags.Color  := clWindow;
    lwTagsUpdate
  end else
  begin
    lwTags.Enabled:= False;
    lwTags.Color  := clBtnFace;

    lwTags.Items.BeginUpdate;
    lwTags.Items.Clear;
    lwTags.Items.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.lwTagsClick(Sender: TObject);
begin
  lwTagsSelected();
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.lwTagsDblClick(Sender: TObject);
begin
  lwTagsSelected();

  ModTags.actTagZoom.Execute;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.lwTagsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  lwTagsSelected();
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.lwTagsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  lwTagsSelected();
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.lwTagsUpdate;
var Index: Integer;
var Item  : TListItem;
begin
  lwTags.Items.BeginUpdate;

  if Image.Tags.Count <> lwTags.Items.Count then
  begin
    lwTags.Items.Clear;

    for Index:=0 to Image.Tags.Count - 1 do
    begin
      Item:= lwTags.Items.Add;
      Item.Caption := String(Image.Tags[Index].Name);
    end;
  end else
  begin
    for Index:=0 to Image.Tags.Count - 1 do
    begin
      Item:= lwTags.Items[Index];
      Item.Caption := String(Image.Tags[Index].Name);
    end;
  end;
  lwTags.Items.EndUpdate;

  lwTags.ItemIndex:= ModTags.SelectedIndex;

  if lwTags.Selected <> nil then
  begin
    lwTags.Selected.MakeVisible(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.lwTagsSelected;
begin
  ModTags.SelectedIndex:= lwTags.ItemIndex;

  SetTag(lwTags.ItemIndex);
end;

//------------------------------------------------------------------------------
function TFrmTagList.GetSelected: Integer;
begin
  Result:= lwTags.ItemIndex;
end;

//------------------------------------------------------------------------------
procedure TFrmTagList.SetSelected(const Value: Integer);
begin
  if FTag <> Value then
  begin
    SetTag(Value);
  end;
end;

end.

unit uPattern.List;

interface

uses
  Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, ToolWin,
  ExtCtrls, Menus, StdCtrls, ActnList,
  Spin, LCLType,

  phxTypes,
  phxImage,

  uActions,

  uPattern.Properties,
  uPattern.Search,
  uPattern.Sort;


type

//------------------------------------------------------------------------------

{ TFrmPatternList }

TFrmPatternList = class(TFrame, IPatternList)
    lwPatterns: TListView;
    Panel1: TPanel;
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
    btnTagSearch: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    PopupMenu1: TPopupMenu;
    actPatternAdd1: TMenuItem;
    actPatternDel1: TMenuItem;
    actPattern: TActionList;
    actPatternAdd: TAction;
    actPatternDel: TAction;
    actPatternUp: TAction;
    actPatternDown: TAction;
    actPatternSort: TAction;
    actPatternSearch: TAction;
    actPatternZoom: TAction;
    N1: TMenuItem;
    actPatternClear: TAction;
    actPatternClear1: TMenuItem;
    procedure lwPatternsClick(Sender: TObject);
    procedure lwPatternsDblClick(Sender: TObject);
    procedure lwPatternsSelectItem(Sender: TObject; Item: TListItem;  Selected: Boolean);
    procedure lwPatternsKeyUp(Sender: TObject; var Key: Word;  Shift: TShiftState);

    procedure actPatternUpdate(Sender: TObject);
    procedure actPatternAddExecute(Sender: TObject);
    procedure actPatternDelExecute(Sender: TObject);
    procedure actPatternUpExecute(Sender: TObject);
    procedure actPatternDownExecute(Sender: TObject);
    procedure actPatternSortExecute(Sender: TObject);
    procedure actPatternZoomExecute(Sender: TObject);
    procedure actPatternSearchExecute(Sender: TObject);
    procedure actPatternClearExecute(Sender: TObject);
  private
    FActive: Boolean;
    FImage : TPHXImage;
    FPattern: Integer;

    FrmPattern: TFrmPatternProperties;

    function GetPattern: TPHXPattern;
    function GetSelected: Integer;

    procedure lwPatternsUpdate;
    procedure lwPatternsSelected;

    procedure SetImage(const Value: TPHXImage);
    procedure SetActive(const Value: Boolean);
    procedure SetPattern(const Value: Integer);
    procedure SetSelected(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    property Active: Boolean read FActive write SetActive;
    Property Image: TPHXImage read FImage write SetImage;

    Property Pattern: Integer read FPattern write SetPattern;
  end;

implementation

{$R *.dfm}

resourcestring
  SDeletePattern = 'Delete the pattern "%s"?';
  SClearPatterns = 'Delete all patterns?';

//------------------------------------------------------------------------------
procedure Swap(var A: TPHXPattern; var B: TPHXPattern);
var T: TPHXPattern;
begin
  T:= A;
  A:= B;
  B:= T;
end;

//------------------------------------------------------------------------------
function PatternSortByName(const A: TPHXPattern; const B: TPHXPattern): Integer;
begin
  Result:= CompareText( String(A.Name), String(B.Name));
end;

//------------------------------------------------------------------------------
function PatternSortByNameReversed(const A: TPHXPattern; const B: TPHXPattern): Integer;
begin
  Result:= CompareText( String(B.Name),  String(A.Name));
end;




// TFrmPatternList
//==============================================================================
constructor TFrmPatternList.Create(AOwner: TComponent);
begin
  inherited;
  FrmPattern:= TFrmPatternProperties.Create(Self);
  FrmPattern.Parent:= Panel2;
  FrmPattern.Align := alClient;
end;

//------------------------------------------------------------------------------
function TFrmPatternList.GetPattern: TPHXPattern;
begin
  Result:= Image.Patterns.List^[FPattern];
end;

//------------------------------------------------------------------------------
function TFrmPatternList.GetSelected: Integer;
begin
  Result:= FPattern;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.lwPatternsClick(Sender: TObject);
begin
  lwPatternsSelected();
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.lwPatternsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  lwPatternsSelected();
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.lwPatternsDblClick(Sender: TObject);
begin
  lwPatternsSelected();

  actPatternZoom.Execute;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.lwPatternsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  lwPatternsSelected();
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.SetActive(const Value: Boolean);
begin
  FActive := Value;

  if Active then
  begin
    actPatternAdd.ShortCut:= ShortCut(LCLType.VK_INSERT, []);
    actPatternDel.ShortCut:= ShortCut(LClType.VK_DELETE, []);
  end else
  begin
    actPatternAdd.ShortCut:= ShortCut(0, []);
    actPatternDel.ShortCut:= ShortCut(0, []);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.SetImage(const Value: TPHXImage);
begin
  if FImage <> Value then
  begin
    FImage  := Value;
    FPattern:= -1;

    FrmPattern.Image  := FImage;
    FrmPattern.Pattern:= FPattern;
  end;

  if Assigned(Image) then
  begin
    lwPatterns.Enabled:= True;
    lwPatterns.Color  := clWindow;
    lwPatternsUpdate
  end else
  begin
    lwPatterns.Enabled:= False;
    lwPatterns.Color  := clBtnFace;

    lwPatterns.Items.BeginUpdate;
    lwPatterns.Items.Clear;
    lwPatterns.Items.EndUpdate;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.SetPattern(const Value: Integer);
begin
  lwPatterns.OnSelectItem:= nil;
  try
    FPattern := Value;

    FrmPattern.Pattern:= FPattern;

    if (Value >= 0) and (Value < lwPatterns.Items.Count) then
    begin
      lwPatterns.ItemIndex:= Value;
    end else
    begin
      lwPatterns.ItemIndex:= -1;
    end;

  finally
    lwPatterns.OnSelectItem:= lwPatternsSelectItem;
  end;

  ModActions.Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.SetSelected(const Value: Integer);
begin
  if FPattern <> Value then
  begin
    SetPattern(Value);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.lwPatternsUpdate;
var Index: Integer;
var Item : TListItem;
begin
  lwPatterns.Items.BeginUpdate;

  if Image.Patterns.Count <> lwPatterns.Items.Count then
  begin
    lwPatterns.Items.Clear;

    for Index:=0 to Image.Patterns.Count - 1 do
    begin
      Item:=lwPatterns.Items.Add;
      Item.Caption := String(Image.Patterns.List^[Index].Name);
    end;
  end else
  begin
    for Index:=0 to Image.Patterns.Count - 1 do
    begin
      Item:=lwPatterns.Items[Index];
      Item.Caption := String(Image.Patterns.List^[Index].Name);
    end;
  end;
  lwPatterns.Items.EndUpdate;

  lwPatterns.ItemIndex:= FPattern;

  if lwPatterns.Selected <> nil then
  begin
    lwPatterns.Selected.MakeVisible(False);
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.lwPatternsSelected;
begin
  SetPattern(lwPatterns.ItemIndex);
end;

{$REGION 'Actions'}


//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternUpdate(Sender: TObject);
begin
  actPatternAdd .Enabled:= Assigned(ModActions.Document);
  actPatternDel .Enabled:= Assigned(ModActions.Document) and (FPattern >= 0);

  actPatternUp  .Enabled:= Assigned(ModActions.Document) and (FPattern >= 0) and (FPattern > 0);
  actPatternDown.Enabled:= Assigned(ModActions.Document) and (FPattern >= 0) and (FPattern < Image.Patterns.Count - 1);

  actPatternZoom  .Enabled:= Assigned(ModActions.Document) and (FPattern >= 0);

  actPatternSearch.Enabled:= Assigned(ModActions.Document);
  actPatternSort  .Enabled:= Assigned(ModActions.Document);
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternAddExecute(Sender: TObject);
var Name   : ShortString;
var Pattern: TPHXPattern;
begin
  Name:= GeneratePatternName(Image);

  Pattern.Name   := Name;
  Pattern.X      := 0;
  Pattern.Y      := 0;
  Pattern.Width  := Image.Width;
  Pattern.Height := Image.Height;
  Pattern.Mirror := False;
  Pattern.Flip   := False;

  if ModActions.Settings.CenterPivots then
  begin
    Pattern.Pivot.X:= Pattern.Width  div 2;
    Pattern.Pivot.Y:= Pattern.Height div 2;
  end else
  begin
    Pattern.Pivot.X:= 0;
    Pattern.Pivot.Y:= 0;
  end;

  FPattern:= Image.Patterns.Count;

  Image.Patterns.Add(Pattern);

  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternDelExecute(Sender: TObject);
begin
  if FPattern >= 0 then
  begin
    if MessageDlg( Format(SDeletePattern, [GetPattern.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Image.Patterns.Delete(FPattern);

      SetPattern(FPattern - 1);

      ModActions.Document.Changed
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternUpExecute(Sender: TObject);
var Pattern: TPHXPattern;
begin
  if FPattern >= 0then
  begin
    Pattern:= Image.Patterns[FPattern];

    if (FPattern >= 1) then
    begin
      Swap(Image.Patterns.List^[FPattern], Image.Patterns.List^[FPattern-1]);

      SetPattern(FPattern - 1);

      ModActions.Document.Changed;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternDownExecute(Sender: TObject);
var Pattern: TPHXPattern;
begin
  if FPattern >= 0 then
  begin
    Pattern:= Image.Patterns[FPattern];

    if (FPattern < Image.Patterns.Count - 1) then
    begin
      Swap(Image.Patterns.List^[FPattern], Image.Patterns.List^[FPattern+1]);

      SetPattern(FPattern + 1);

      ModActions.Document.Changed;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternClearExecute(Sender: TObject);
begin
  if MessageDlg(SClearPatterns, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Image.Patterns.Clear;

    SetPattern(- 1);

    ModActions.Document.Changed
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternSearchExecute(Sender: TObject);
var Dialog: TFrmPatternSearch;
begin
  Dialog:= TFrmPatternSearch.Create(Application);
  try
    if Dialog.Execute(ModActions.Document.Image) then
    begin
      SetPattern(Dialog.PatternIndex);

      ModActions.Document.Changed;
    end;
  finally
    Dialog.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternSortExecute(Sender: TObject);
var Dialog: TFrmPatternSort;
begin
  Dialog:= TFrmPatternSort.Create(Application);
  try
    if Dialog.Execute then
    begin

      case Dialog.SortType of
        0: ModActions.Document.Image.Patterns.Sort(PatternSortByName);
        1: ModActions.Document.Image.Patterns.Sort(PatternSortByNameReversed);
      end;

      SetPattern(-1);

      ModActions.Document.Changed;
    end;
  finally
    Dialog.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmPatternList.actPatternZoomExecute(Sender: TObject);
var Pattern: TPHXPattern;
var Rect   : TRecti;
begin
  if FPattern >= 0 then
  begin
    Pattern:= Image.Patterns[FPattern];

    Rect.Left  := Pattern.X - 1;
    Rect.Top   := Pattern.Y - 1;
    Rect.Right := Pattern.X + Pattern.Width+1;
    Rect.Bottom:= Pattern.Y + Pattern.Height+1;

    ModActions.Editor.Viewport.ZoomRect(Rect);
  end;
end;

{$ENDREGION}

end.

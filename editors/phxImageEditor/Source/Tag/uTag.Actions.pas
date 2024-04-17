unit uTag.Actions;

interface

uses
  SysUtils, Classes, ActnList, Forms, Dialogs,
  Controls,

  phxTypes,
  phxImage,

  uActions,
  uEditor;

type

//------------------------------------------------------------------------------
TModTags = class(TDataModule)
    actTags: TActionList;
    actTagAdd: TAction;
    actTagDel: TAction;
    actTagUp: TAction;
    actTagDown: TAction;
    actTagSort: TAction;
    actTagZoom: TAction;
    procedure actTagAddUpdate(Sender: TObject);
    procedure actTagAddExecute(Sender: TObject);
    procedure actTagDelExecute(Sender: TObject);
    procedure actTagUpExecute(Sender: TObject);
    procedure actTagDownExecute(Sender: TObject);
    procedure actTagZoomExecute(Sender: TObject);
  private
    { Private declarations }
  private
    FImage: TPHXImage;
    FSelectedIndex: Integer;
    FEditor: TPHXImageEditor;

    function GetSelectedTag: TPHXTag;

    procedure SetImage(const Value: TPHXImage);

    procedure SetSelectedIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    function GenerateTagName(const Template: String = 'Tag'): String;

    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property SelectedTag: TPHXTag read GetSelectedTag;

    property Image: TPHXImage read FImage write SetImage;
    property Editor: TPHXImageEditor read FEditor write FEditor;
  end;

var
  ModTags: TModTags;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

resourcestring
  SDeleteTag = 'Delete the tag "%s"?';

//------------------------------------------------------------------------------
function TagSortByName(const A: TPHXTag; const B: TPHXTag): Integer;
begin
  Result:= CompareText( String(A.Name), String(B.Name));
end;

//------------------------------------------------------------------------------
function TagSortByNameReversed(const A: TPHXTag; const B: TPHXTag): Integer;
begin
  Result:= CompareText( String(B.Name),  String(A.Name));
end;

//------------------------------------------------------------------------------
constructor TModTags.Create(AOwner: TComponent);
begin
  inherited;

  FSelectedIndex:= -1;
end;

var TagCounter: Integer = 1;


//------------------------------------------------------------------------------
function TModTags.GenerateTagName(const Template: String = 'Tag'): String;
begin
  Result:=Format('%s%d', [Template, TagCounter]);

  while Image.Tags.IndexOf(Result) <> -1 do
  begin
    Inc(TagCounter);

    Result:=Format('%s%d', [Template, TagCounter]);
  end;
end;

//------------------------------------------------------------------------------
function TModTags.GetSelectedTag: TPHXTag;
begin
  Result:= Image.Tags[FSelectedIndex];
end;

//------------------------------------------------------------------------------
procedure TModTags.SetImage(const Value: TPHXImage);
begin
  if FImage <> Value then
  begin
    FImage := Value;

    SetSelectedIndex(-1);
  end;
end;

//------------------------------------------------------------------------------
procedure TModTags.SetSelectedIndex(const Value: Integer);
begin
  if Value = FSelectedIndex then Exit;

  if Assigned(Image) then
  begin

    if Value >= Image.Tags.Count then
    begin
      FSelectedIndex:= Image.Tags.Count- 1;
    end else
    begin
      FSelectedIndex := Value;
    end;

  end else
  begin
    FSelectedIndex:= -1;
  end;

  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TModTags.actTagAddUpdate(Sender: TObject);
begin
  actTagAdd .Enabled:= Assigned(ModActions.Document);
  actTagDel .Enabled:= Assigned(ModActions.Document) and (FSelectedIndex >= 0);

  actTagUp  .Enabled:= Assigned(ModActions.Document) and (FSelectedIndex >= 0) and (FSelectedIndex > 0);
  actTagDown.Enabled:= Assigned(ModActions.Document) and (FSelectedIndex >= 0) and (FSelectedIndex < Image.Tags.Count - 1);

  actTagZoom .Enabled:= Assigned(ModActions.Document) and (FSelectedIndex >= 0);
end;

//------------------------------------------------------------------------------
procedure TModTags.actTagAddExecute(Sender: TObject);
var Name: String;
var Tag : TPHXTag;
begin
  Name:= GenerateTagName;

  Tag.Name    := ShortString(Name);
  Tag.X       := 0;
  Tag.Y       := 0;
  Tag.Rotation:= 0;

  if ModActions.Selected.Pattern >= 0 then
  begin
    Tag.Pattern := ModActions.Selected.Pattern;
  end else
  begin
    Tag.Pattern := PHXPATTERN_NONE;
  end;

  FSelectedIndex:= Image.Tags.Count;

  Image.Tags.Add(Tag);

  ModActions.Document.Changed;
end;

//------------------------------------------------------------------------------
procedure TModTags.actTagDelExecute(Sender: TObject);
begin
  if FSelectedIndex >= 0 then
  begin
    if MessageDlg( Format(SDeleteTag, [SelectedTag.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Image.Tags.Delete(FSelectedIndex);

      SetSelectedIndex(FSelectedIndex - 1);

      ModActions.Document.Changed
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TModTags.actTagUpExecute(Sender: TObject);
var Tag: TPHXTag;
begin
  if FSelectedIndex >= 0then
  begin
    Tag:= Image.Tags[FSelectedIndex];

    if (FSelectedIndex >= 1) then
    begin
      Image.Tags[FSelectedIndex  ]:= Image.Tags[FSelectedIndex-1];
      Image.Tags[FSelectedIndex-1]:= Tag;

      FSelectedIndex:= FSelectedIndex - 1;

      ModActions.Document.Changed;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TModTags.actTagDownExecute(Sender: TObject);
var Tag: TPHXTag;
begin
  if FSelectedIndex >= 0 then
  begin
    Tag:= Image.Tags[FSelectedIndex];

    if (FSelectedIndex < Image.Tags.Count - 1) then
    begin
      Image.Tags[FSelectedIndex  ]:= Image.Tags[FSelectedIndex+1];
      Image.Tags[FSelectedIndex+1]:= Tag;

      FSelectedIndex:= FSelectedIndex + 1;

      ModActions.Document.Changed;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TModTags.actTagZoomExecute(Sender: TObject);
var Tag : TPHXTag;
var Pattern: TPHXPattern;
var Rect: TRecti;
begin
  if FSelectedIndex >= 0 then
  begin
    Tag:= Image.Tags[FSelectedIndex];

    if(Tag.Pattern >= 0) and (Tag.Pattern < Image.Patterns.Count) then
    begin
      Pattern:= Image.Patterns[Tag.Pattern];

      Rect.Left  := Pattern.X - 1;
      Rect.Top   := Pattern.Y - 1;
      Rect.Right := Pattern.X + Pattern.Width+1;
      Rect.Bottom:= Pattern.Y + Pattern.Height+1;

      Editor.Viewport.ZoomRect(Rect);
    end;
  end;
end;




end.

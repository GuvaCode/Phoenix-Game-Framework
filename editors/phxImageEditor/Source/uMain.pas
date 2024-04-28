unit uMain;

interface

uses
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, ComCtrls, ExtCtrls,
  phxTypes,
  phxMath,
  phxImage,
  uActions,
  uEditor,
  uTools,
  uImage.Properties,
  uPattern.List,
  uTag.Actions,
  uTag.List;

type

//------------------------------------------------------------------------------

{ TFrmMain }

TFrmMain = class(TForm)
    ToolBarStandard: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    menuRecent: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    ools1: TMenuItem;
    About1: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    N6: TMenuItem;
    lblVersion: TMenuItem;
    PageControl1: TPageControl;
    TabProperties: TTabSheet;
    TabPatterns: TTabSheet;
    StatusBar1: TStatusBar;
    pnlClient: TPanel;
    TabTags: TTabSheet;
    Splitter1: TSplitter;
    ToolButton1: TToolButton;
    btnImageTextureLoad: TToolButton;
    Panel1: TPanel;
    pnlTools: TPanel;
    ToolBarZoom: TToolBar;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    btnZoom100: TToolButton;
    ToolBarPattern: TToolBar;
    btnPatternAdd: TToolButton;
    btnPatternImport: TToolButton;
    btnPatternSelect: TToolButton;
    btnPatternMove: TToolButton;
    ToolBarTag: TToolBar;
    btnTagMove: TToolButton;
    Export1: TMenuItem;
    Patternsasimages1: TMenuItem;
    Patternsasxml1: TMenuItem;
    Import1: TMenuItem;
    Patternsasxml2: TMenuItem;
    btnImageTextureSave: TToolButton;
    N1: TMenuItem;
    agstoxml1: TMenuItem;
    agsfromxmldocument1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Settings1: TMenuItem;
    Close1: TMenuItem;
    Image1: TMenuItem;
    Resize1: TMenuItem;
    N9: TMenuItem;
    exture3: TMenuItem;
    exture4: TMenuItem;
    N7: TMenuItem;
    ileWizard1: TMenuItem;
    actTileEditor1: TMenuItem;
    N3: TMenuItem;
    actToolShapes1: TMenuItem;
    btnPatternPick: TToolButton;
    N8: TMenuItem;
    actEditCut1: TMenuItem;
    actEditCopy1: TMenuItem;
    actEditPaste1: TMenuItem;
    procedure PageControl1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Panel1Resize(Sender: TObject);
  private
    Editor: TPHXImageEditor;


    FrmImageProperties: TFrmImageProperties;
    FrmPatternList    : TFrmPatternList;
    FrmTagList        : TFrmTagList;

    procedure DrawTags;

    procedure ToolHint(Sender: TObject; const Hint: String);

    procedure DocumentChanged(Document: TDocument);

    procedure EditorCustomPaint(Sender: TObject);
    procedure EditorSelectPattern(Sender: TObject; PatternIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses phxGraphics_Vampyre;

// TFrmMain
//==============================================================================
constructor TFrmMain.Create(AOwner: TComponent);
var Index: Integer;
begin
  inherited;

  Editor:= TPHXImageEditor.Create(Self);
  Editor.Parent:= pnlClient;
  Editor.Align := alClient;
  Editor.OnCustomPaint:= EditorCustomPaint;
  Editor.OnSelectPattern:= EditorSelectPattern;

  ModActions:= TModActions.Create(Self);
  ModActions.DocumentChanged.Add(DocumentChanged);
  ModActions.Settings.Recent.Menu:= menuRecent;
  ModActions.Editor:= Editor;

  ModTools:= TModTools.Create(Self);
  ModTools.Editor:= Editor;
  ModTools.Tools.OnHint:= ToolHint;


  ModTags:= TModTags.Create(Self);
  ModTags.Editor:= Editor;

  FrmImageProperties:= TFrmImageProperties.Create(Self);
  FrmImageProperties.Parent:= TabProperties;
  FrmImageProperties.Align := alClient;

  FrmPatternList:= TFrmPatternList.Create(Self);
  FrmPatternList.Parent:= TabPatterns;
  FrmPatternList.Align := alClient;

  FrmTagList:= TFrmTagList.Create(Self);
  FrmTagList.Parent:= TabTags;
  FrmTagList.Align := alClient;


  ModActions.WindowPatterns:= FrmPatternList;
  ModActions.WindowTags    := FrmTagList;

  PageControl1.ActivePageIndex:= 0;

  if ParamCount > 0 then
  begin
    for Index := 1  to ParamCount  do
    begin
      ModActions.Open( ParamStr(Index) );
    end;
  end else
  begin
    DocumentChanged(nil);
  end;

  PageControl1Change(Self);

  lblVersion.Caption:= 'Image version: ' + IntToStr( PHXIMAGE_VERSION );
end;

//------------------------------------------------------------------------------
destructor TFrmMain.Destroy;
begin
  ModActions.DocumentChanged.Remove(DocumentChanged);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.DocumentChanged(Document: TDocument);
begin
  if Assigned(Document) then
  begin
    Editor.Image:= Document.Image;

    if dsChanged in Document.State then
    begin
      Caption:= 'phxImageEditor - ' + Document.Name + '*';

      StatusBar1.Panels[1].Text:= 'Modified';
    end else
    begin
      Caption:= 'phxImageEditor - ' + Document.Name;

      StatusBar1.Panels[1].Text:= '';
    end;

    ModTags   .Image:= Document.Image;

    FrmImageProperties.Image:= Document.Image;
    FrmPatternList    .Image:= Document.Image;
    FrmTagList        .Image:= Document.Image;
  end else
  begin
    Caption:= 'phxImageEditor';

    Editor.Image:= nil;

    ModTags   .Image:= nil;

    FrmImageProperties.Image:= nil;
    FrmPatternList    .Image:= nil;
    FrmTagList        .Image:= nil;

    StatusBar1.Panels[1].Text:= '';
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModActions.Close(True) = mrCancel then
  begin
    Action:= caNone;
  end;
end;

procedure TFrmMain.Panel1Resize(Sender: TObject);
begin
  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0: ModActions.EditorMode:= emImage;
    1: ModActions.EditorMode:= emPatterns;
    2: ModActions.EditorMode:= emTags;
  end;

  ToolBarPattern.Visible:= PageControl1.ActivePage = TabPatterns;
  if ToolBarPattern.Visible then ToolBarPattern.Repaint;
  ToolBarTag    .Visible:= PageControl1.ActivePage = TabTags;

  Editor.Invalidate;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.ToolHint(Sender: TObject; const Hint: String);
begin
  StatusBar1.Panels[2].Text:= Hint;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.EditorSelectPattern(Sender: TObject; PatternIndex: Integer);
begin
  if (PageControl1.ActivePage = TabPatterns) then
  begin
    ModActions.Selected.Pattern:= PatternIndex;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.EditorCustomPaint(Sender: TObject);
var Image  : TPHXImage;
var Pattern: Integer;

begin
  if not Assigned(ModActions.Document) then Exit;

  Image:=  ModActions.Document.Image;

  if (PageControl1.ActivePage = TabPatterns) then
  begin
    Pattern:= ModActions.Selected.Pattern;

    if (Pattern >= 0) and (Pattern < Image.Patterns.Count) then
    // Draw a single pattern
    begin
      Editor.DrawPattern(Pattern);
    end else
    // Draw all patterns
    begin
      for Pattern := 0 to Image.Patterns.Count-1 do
      begin
        Editor.DrawPattern(Pattern, False);
      end;
    end;

  end;
  if (PageControl1.ActivePage = TabTags) then
  begin
    DrawTags;
  end;


  //ModTools.Tools.Paint;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.DrawTags;
var Index: Integer;
var Tag  : TPHXTag;
//var Pattern : TPHXPattern;
var Position: TVector2f;
var Direction: TVector2f;
var TagPattern: Integer;
begin

  if ModTags.SelectedIndex >= 0 then
  begin
    TagPattern:= ModTags.SelectedTag.Pattern;
  end else
  begin
    TagPattern:= -1;
  end;

  with Editor.Canvas do
  begin
    Brush.Style:= bsClear;

    if (TagPattern >= 0) and (TagPattern < Editor.Image.Patterns.Count) then
    begin
      Editor.DrawPattern(TagPattern, False);
    end;

    Pen.Style:= psSolid;
    Pen.Color:= clRed;

    Brush.Color:= clSilver;
    for Index:= 0 to Editor.Image.Tags.Count - 1 do
    begin
      Tag:= Editor.Image.Tags.List[Index];

      Pen.Style:= psSolid;
      if Tag.Pattern = TagPattern then
      begin
        Position:= Editor.Image.Tags.TagToImage(Tag, TVector2f.Create(0,0) );
        Position:= Editor.DocumentToScreen(Position);

        if Index = ModTags.SelectedIndex  then
        begin
          Brush.Style:= bsSolid;

          Rectangle( Round(Position.X - 4),
                     Round(Position.Y - 4),
                     Round(Position.X + 4),
                     Round(Position.Y + 4));

          Direction:= Matrix_Transform( Matrix_CreateRotationZ(Tag.Rotation), Vector2f_AxisX);

          MoveTo( Round(Position.X + Direction.X *  4), Round(Position.Y + Direction.Y *  4));
          LineTo( Round(Position.X + Direction.X * 24), Round(Position.Y + Direction.Y * 24));
        end else
        begin
          Brush.Style:= bsClear;

          Rectangle( Round(Position.X - 4),
                     Round(Position.Y - 4),
                     Round(Position.X + 4),
                     Round(Position.Y + 4));
        end;


      end;

    end;
  end;
end;


end.

unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ToolWin, ImgList, Menus,

  Generics.Collections,

  Themes,

  phxTypes,
  phxSkin,
  phxSkinEx,
  phxDevice,

  phxEditor,

  uSkin.Properties,
  uSkin.Elements,

  uElement,
  uElementPreview,
  uActions,
  uTools;

// http://www.skin-soft.co.uk/Products/VisualStyler/Overview.aspx

type
  TFrmMain = class(TForm)
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ImageList2: TImageList;
    PopupMenu2: TPopupMenu;
    ToolBar1: TToolBar;
    btnToolMove: TToolButton;
    btnToolSelect: TToolButton;
    Panel2: TPanel;
    pnlEditor: TPanel;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    About1: TMenuItem;
    lblVersion: TMenuItem;
    N2: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    ools1: TMenuItem;
    ToolButton8: TToolButton;
    ToolButton11: TToolButton;
    btnToolImport: TToolButton;
    menuRecent: TMenuItem;
    SkinDelete1: TMenuItem;
    Export1: TMenuItem;
    Import1: TMenuItem;
    actExportToXML1: TMenuItem;
    actImportFromXML1: TMenuItem;
    actExportToImages1: TMenuItem;
    Copyelementnames1: TMenuItem;
    N4: TMenuItem;
    panProperties: TPanel;
    pnlElementPreview: TGroupBox;
    Add1: TMenuItem;
    Splitter2: TSplitter;
    PageControl1: TPageControl;
    TabProperties: TTabSheet;
    TabElements: TTabSheet;
    ControlBar1: TControlBar;
    ToolBarStandard: TToolBar;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    Splitter3: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure EditorCustomPaint(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure EditorStateChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure SelectedChanged(Document: TDocument);
  public
    { Public declarations }
//    Skin: TPHXSkin;
    Editor: TPHXSkinEditor;

    FrmSkinProperties: TFrmSkinProperties;
    FrmSkinElements  : TFrmSkinElements;

    FrmElement       : TFrmElement;
    FrmElementPreview: TFrmElementPreview;
  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.dfm}

uses phxGraphics_FreeImage;

//------------------------------------------------------------------------------
procedure TFrmMain.FormCreate(Sender: TObject);
begin
 // Width:= 1024;
 // Height:= 768;

  //  Skin:= TPHXSkin.Create(TPHXTexture2D.Create);
//  Skin.LoadFromFile('skins\ClearlooksDarkLime.phxskn');

//  Skin.Texture.LoadTexture('Freezy-themes-pack_3.png');
//  Skin.Width := Skin.Texture.Width;
//  Skin.Height:= Skin.Texture.Height;
  Editor:= TPHXSkinEditor.Create(Self);
  Editor.Parent        := pnlEditor;
  Editor.Align         := alClient;
  Editor.Options       := [doInvalidate,doTransparent,doMouseScroll];
  Editor.OnCustomPaint := EditorCustomPaint;
  Editor.OnMouseMove   := EditorMouseMove;
  Editor.OnStateChange := EditorStateChange;

  modTools:= TModTools.Create(Self);
  modTools.Editor:= Editor;

 // modTools.ToolStateMove  .Button:= btnToolMove;
//  modTools.ToolStateSelect.Button:= btnToolSelect;
//  modTools.ToolStateImport.Button:= btnToolImport;


  ModActions:= TmodActions.Create(Self);
  ModActions.Editor:= Editor;
  ModActions.Settings.Recent.Menu:= MenuRecent;

  ModActions.DocumentChanged.Add(SelectedChanged);

  FrmSkinProperties:= TFrmSkinProperties.Create(Self);
  FrmSkinProperties.Parent:= TabProperties;
  FrmSkinProperties.Align := alClient;
  FrmSkinProperties.Skin  := nil;

  FrmSkinElements:= TFrmSkinElements.Create(Self);
  FrmSkinElements.Parent:= TabElements;
  FrmSkinElements.Align := alClient;
  FrmSkinElements.Skin  := nil;
  FrmSkinElements.Editor:= Editor;

  FrmElement:= TFrmElement.Create(Self);
  FrmElement.Parent:= panProperties;
  FrmElement.Align:= alClient;
  FrmElement.Element:= nil;

  FrmElementPreview:= TFrmElementPreview.Create(Self);
  FrmElementPreview.Parent:= pnlElementPreview;
  FrmElementPreview.Align:= alClient;
  FrmElementPreview.Element:= nil;

  if ParamCount > 0 then
  begin
    modActions.Open(ParamStr(1));
  end;
  PageControl1.ActivePageIndex:= 0;

  PageControl1Change(nil);

  lblVersion.Caption:= 'Skin version: ' + IntToStr( PHXSKIN_VERSION );
end;

//------------------------------------------------------------------------------
procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  ModActions.DocumentChanged.Remove(SelectedChanged);
end;

//------------------------------------------------------------------------------
procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= ModActions.Close(True) <> mrCancel;
end;
//------------------------------------------------------------------------------
procedure TFrmMain.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = TabProperties then
  begin
    FrmSkinProperties.Active:= True;
    FrmSkinElements  .Active:= False;
  end else
  if PageControl1.ActivePage = TabElements then
  begin
    FrmSkinProperties.Active:= False;
    FrmSkinElements  .Active:= True;
  end;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.SelectedChanged(Document: TDocument);
begin

  if Assigned(Document) then
  begin
    if dsChanged in Document.State then
    begin
      Caption:= 'phxSkinEditor - ' + Document.Name + '*';

      StatusBar1.Panels[1].Text:= 'Changed';
    end else
    begin
      Caption:= 'phxSkinEditor - ' + Document.Name;

      StatusBar1.Panels[1].Text:= '';
    end;

    FrmSkinProperties.Skin:= Document.Skin;
    FrmSkinElements  .Skin:= Document.Skin;
  end else
  begin
    Caption:= 'phxSkinEditor';

    StatusBar1.Panels[1].Text:= '';

    FrmSkinProperties.Skin:= nil;
    FrmSkinElements  .Skin:= nil;
  end;
  FrmElement.Element:= Editor.Selection.Element;

  FrmElementPreview.Skin   := Editor.Skin;
  FrmElementPreview.Element:= Editor.Selection.Element;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.EditorCustomPaint(Sender: TObject);
var Index   : Integer;
var Elements: TList<TPHXSkinElement>;
var Element : TPHXSkinElement;
begin
  if not Assigned(Editor.Skin) then Exit;

  if Editor.Selection.ValidElement then
  begin
    Editor.DrawElement(Editor.Selection.idxElement);
  end else

  if Assigned(FrmSkinElements.twElements.Selected) and (FrmSkinElements.twElements.Selected.Data = nil) then
  begin
    Elements:= TList<TPHXSkinElement>.Create;
    try
      Editor.Skin.GetElementsForControl(FrmSkinElements.twElements.Selected.Text, Elements);

      for Index := 0 to Elements.Count - 1 do
      begin
        Element:= Elements[Index];

        Editor.DrawElementOutline(Element);
      end;

    finally
      Elements.Free;
    end;

  end;
end;


//------------------------------------------------------------------------------
procedure TFrmMain.EditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Position: TVector2i;
begin


  Position:= Editor.ScreenToDocument(X,Y);

  if Assigned(Editor.Skin) and (Position.X >= 0) and (Position.Y >= 0) and (Position.X < Editor.Skin.Width) and (Position.Y < Editor.Skin.Height) then
  begin
    StatusBar1.Panels[0].Text:= Format('X: %d Y: %d', [Position.X, Position.Y]);
  end else
  begin
    StatusBar1.Panels[0].Text:= '';
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmMain.EditorStateChange(Sender: TObject);
begin
  FrmElementPreview.Element:= Editor.Selection.Element;
  FrmElement       .Element:= Editor.Selection.Element;
end;

//------------------------------------------------------------------------------
procedure TFrmMain.Timer1Timer(Sender: TObject);
begin
//  ModTools.T

  //btnToolMove  .Enabled:= Assigned(Editor.Selection.Element);
 // btnToolSelect.Enabled:= Assigned(Editor.Selection.Element);
//  btnToolImport.Enabled:= Assigned(Editor.Selection.Element);
end;





end.

unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, ToolWin, StdCtrls, ExtCtrls,

  phxFont,

  uActions,

  uFont.Properties,
  uFont.Texture,
  uFont.Kernings,
  uFont.Preview,

  uCharacter.List
  ;


type
  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    Save1: TMenuItem;
    Save2: TMenuItem;
    Close1: TMenuItem;
    menuRecent: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    ools1: TMenuItem;
    Export1: TMenuItem;
    Import1: TMenuItem;
    About1: TMenuItem;
    PhoenixImageEditor1: TMenuItem;
    N6: TMenuItem;
    lblVersion: TMenuItem;
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    TabTexture: TTabSheet;
    TabCharacters: TTabSheet;
    FontStudio41: TMenuItem;
    Splitter1: TSplitter;
    TabFont: TTabSheet;
    AngelcodeBitmapFontXML1: TMenuItem;
    N1: TMenuItem;
    TabKernings: TTabSheet;
    actToolExportTexture1: TMenuItem;
    extureColors1: TMenuItem;
    extureAlpha1: TMenuItem;
    Import2: TMenuItem;
    exture1: TMenuItem;
    TabPreview: TTabSheet;
    actFileLoadXML1: TMenuItem;
    N4: TMenuItem;
    N7: TMenuItem;
    actFileSaveXML1: TMenuItem;
    ControlBar2: TControlBar;
    ToolBarStandard: TToolBar;
    btnFileNew: TToolButton;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    ToolButton10: TToolButton;
    btnImportTexture: TToolButton;
    btnExportTexture: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure CharacterChanged(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FrmFont         : TFrmFontProperties;
    FrmTexture      : TFrmFontTexture;
    FrmCharacterList: TFrmFontCharacters;
    FrmKernings     : TFrmFontKernings;
    FrmPreview      : TFrmFontPreview;

    procedure SetSelected(Document: TDocument);
  public

    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation


{$R *.dfm}

uses phxGraphics_FreeImage;



//------------------------------------------------------------------------------
procedure TFrmMain.FormCreate(Sender: TObject);
var Index: Integer;
begin
  ModActions:= TModActions.Create(Self);
  ModActions.OnSelectedChange:= SetSelected;
  ModActions.Settings.Recent.Menu:= menuRecent;

  FrmCharacterList:= TFrmFontCharacters.Create(Self);
  FrmCharacterList.Parent:= TabCharacters;
  FrmCharacterList.Align := alClient;
  FrmCharacterList.OnChange:= CharacterChanged;

  FrmFont:= TFrmFontProperties.Create(Self);
  FrmFont.Parent:= TabFont;
  FrmFont.Align := alClient;
  FrmFont.EnableControls(False);

  FrmTexture:= TFrmFontTexture.Create(Self);
  FrmTexture.Parent:= TabTexture;
  FrmTexture.Align := alClient;
  FrmTexture.EnableControls(False);

  FrmKernings:= TFrmFontKernings.Create(Self);
  FrmKernings.Parent:= TabKernings;
  FrmKernings.Align := alClient;
  FrmKernings.EnableControls(False);

  FrmPreview:= TFrmFontPreview.Create(Self);
  FrmPreview.Parent:= TabPreview;
  FrmPreview.Align := alClient;
  FrmPreview.EnableControls(False);


  PageControl1.ActivePage:= TabFont;
//  ModActions.Open('Calibri_12px.phxfnt') ;
//  ModActions.Open('Vani14.phxfnt');
 // ModActions.Open('Palatino_Linotype_15.phxfnt');
//  Editor.Image:= nil;
  if ParamCount > 0 then
  begin
    for Index := 1  to ParamCount  do
    begin
      ModActions.Open( ParamStr(Index) );
    end;
  end else
  begin
    SetSelected(nil);
  end;

  lblVersion.Caption:= 'Font version: ' + IntToStr( PHXFONT_VERSION );
end;



//------------------------------------------------------------------------------
procedure TFrmMain.SetSelected(Document: TDocument);
begin
  if Assigned(Document) then
  begin

    if dsChanged in Document.State then
    begin
      Caption:= 'phxFontEditor - ' + Document.Name + '*';

      StatusBar1.Panels[1].Text:= 'Modified';
    end else
    begin
      Caption:= 'phxFontEditor - ' + Document.Name;

      StatusBar1.Panels[1].Text:= '';
    end;

    FrmFont         .Font:= Document.Font;
    FrmTexture      .Font:= Document.Font;
    FrmCharacterList.Font:= Document.Font;
    FrmKernings     .Font:= Document.Font;
    FrmPreview      .Font:= Document.Font;
  end else
  begin
    FrmFont         .Font:= nil;
    FrmTexture      .Font:= nil;
    FrmCharacterList.Font:= nil;
    FrmKernings     .Font:= nil;
    FrmPreview      .Font:= nil;

    Caption:= 'phxFontEditor';
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

//------------------------------------------------------------------------------
procedure TFrmMain.PageControl1Change(Sender: TObject);
begin
//  FrmFont         .Font:= FrmFont.Font;
 // FrmTexture      .Font:= FrmFont.Font;
 // FrmCharacterList.Font:= FrmFont.Font;
//  FrmKernings     .Font:= FrmFont.Font;
//  FrmPreview      .Font:= FrmFont.Font;
  if PageControl1.ActivePage = TabPreview then
  begin
    FrmPreview.RedrawFont;
  end;
end;


//------------------------------------------------------------------------------
procedure TFrmMain.CharacterChanged(Sender: TObject);
begin
  if Assigned(ModActions.Document) then
  begin
    ModActions.Document.Changed;

    Caption:= 'phxFontEditor - ' + ModActions.Document.Name + '*';
  end;

end;




end.

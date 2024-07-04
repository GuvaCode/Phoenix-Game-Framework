unit uActions;

interface

uses
  SysUtils, Classes, ImgList, Controls, Dialogs, ActnList, Menus, FileCtrl,
  Clipbrd, Forms, Windows,

  xmldom, XMLIntf, msxmldom, XMLDoc,

  Generics.Collections,

  phxEditor,
  phxTexture,

  phxTypes,
  phxDevice,
  phxGraphics,
  phxSkin,
  phxSkinEx;

type


TRecentEvent = procedure(Sender: TObject; const Filename: String) of object;

//------------------------------------------------------------------------------
TRecent = class
  private
    FItems: TStrings;
    FMenu: TMenuItem;
    FOnClick: TRecentEvent;
    procedure SetMenu(const Value: TMenuItem);
    procedure UpdateMenu;
    procedure MenuClicked(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromXML(Node : IXMLNode);
    procedure SaveToXML(Node : IXMLNode);

    procedure Add(const FileName: String);


    property Items: TStrings read FItems write FItems;
    property Menu: TMenuItem read FMenu write SetMenu;
    property OnClick: TRecentEvent read FOnClick write FOnClick;
  end;

//------------------------------------------------------------------------------
TSettings = class
  private
    FRecent: TRecent;
    FGrid: TPHXEditorGrid;
    procedure SetGrid(const Value: TPHXEditorGrid);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);

    // List of recent documents
    property Recent: TRecent read FRecent;
    // Grid settings
    property Grid: TPHXEditorGrid read FGrid write SetGrid;
  end;


TDocument  = class;

//------------------------------------------------------------------------------
TDocumentState = set of (
  dsNew,
  dsChanged
  );

TDocumentEvent = procedure(Document: TDocument) of object;

//------------------------------------------------------------------------------
TDocument = class
  private
    FSkin : TPHXSkin;
    FName  : String;
    FState : TDocumentState;
  public
    constructor Create; overload;
    destructor Destroy; override;

    // Load the document from a file.
    Procedure LoadDocument;
    // Save the document to a file.
    Procedure SaveDocument;

    procedure Changed;

    // The image for the document
    property Skin: TPHXSkin read FSkin;
    // The filename
    property Name: String read FName write FName;
    // State of the doccument
    property State: TDocumentState read FState write FState;
  end;


//------------------------------------------------------------------------------
TModActions = class(TDataModule)
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    OpenTextureDialog: TOpenDialog;
    SaveImageDialog: TSaveDialog;
    ToolBarImages: TImageList;
    ActionList: TActionList;
    actFileOpen: TAction;
    ActionZoom100: TAction;
    actFileClose: TAction;
    actFileSaveAs: TAction;
    actFileNew: TAction;
    actFileSave: TAction;
    actFileImport: TAction;
    actFileExport: TAction;
    actFileExit: TAction;
    ActionZoomOut: TAction;
    ActionZoomIn: TAction;
    actToolsRegister: TAction;
    actOptions: TAction;
    ElementImages: TImageList;
    actExportXML: TAction;
    actImportXML: TAction;
    OpenXMLDialog: TOpenDialog;
    SaveXMLDialog: TSaveDialog;
    actExportImages: TAction;
    actToolsCopyElementNames: TAction;
    ControlImages: TImageList;
    ActionImages: TImageList;
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileImportExecute(Sender: TObject);
    procedure actFileExportExecute(Sender: TObject);
    procedure ActionZoom100Execute(Sender: TObject);
    procedure ActionZoomOutExecute(Sender: TObject);
    procedure ActionZoomInExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actExportXMLExecute(Sender: TObject);
    procedure actExportImagesExecute(Sender: TObject);
    procedure actToolsCopyElementNamesExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actToolsUpdate(Sender: TObject);
    procedure actImportXMLExecute(Sender: TObject);
    procedure actExportUpdate(Sender: TObject);
    procedure actImportUpdate(Sender: TObject);
  private
    FEditor: TPHXSkinEditor;

    FDocument       : TDocument;
    FDocumentCounter: Integer;
    FDocumentChanged:  TList<TDocumentEvent> ;

    FSettings: TSettings;

    procedure RecentClicked(Sender: TObject; const Filename: String);

    procedure SetEditor(const Value: TPHXSkinEditor);
    procedure SetDocument(const Value: TDocument);
    procedure NewPacker;
    procedure NewTexture;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Create a new document
    function New: TDocument;
    // Open a document
    function Open(const FileName: String): TDocument;
    // Close the current image
    function Close(SaveQuery: Boolean = False): Integer;


    // Load the editor settings from the settings file
    procedure LoadSettings;
    // Save the editor settings to the settings file
    procedure SaveSettings;

    Function ImageForControl(const ClassName: String): Integer;

    // Editor settings
    property Settings: TSettings read FSettings;
    // The skin editor
    property Editor: TPHXSkinEditor read FEditor write SetEditor;

    property Document: TDocument read FDocument write SetDocument;

    property DocumentChanged: TList<TDocumentEvent> read FDocumentChanged;
  end;

var
  ModActions: TModActions;

implementation

{$R *.dfm}

uses uElementName, uMain, uSettings, uNewDialog, uPacker, uDialog.New;

resourcestring
  SRenameText = 'The skin name "%s" doesnt match the filename.'#13'Do you want to rename the skin to "%s" before saving?';

  STextureResize = 'The texture is not power of two in size, resize the texture?';

  SSaveQuery = 'Save changes to "%s"?';

//------------------------------------------------------------------------------
function GetUserFromWindows: string;
var UserName    : String;
var UserNameLen : Cardinal;
Begin
  UserNameLen := 255;

  SetLength(userName, UserNameLen) ;

  If GetUserName(PChar(UserName), UserNameLen) then
  begin
    Result := Copy(UserName,1,UserNameLen - 1)
  end else
  begin
     Result := 'Unknown';
  end;
end;


{$REGION 'TRecent'}

//------------------------------------------------------------------------------
constructor TRecent.Create;
begin
  FItems:= TStringList.Create;
end;

//------------------------------------------------------------------------------
destructor TRecent.Destroy;
begin
  FItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TRecent.Add(const FileName: String);
var Index: Integer;
begin
  Index:= Items.IndexOf(FileName);

  if Index >= 0 then
  begin
    Items.Delete(Index);
  end;
  Items.Insert(0, FileName);

  UpdateMenu;
end;

//------------------------------------------------------------------------------
procedure TRecent.UpdateMenu;
var Index: Integer;
var Item : TMenuItem;
begin
  if FMenu = nil then Exit;

  FMenu.Clear;
  FMenu.Visible:= Items.Count > 0;

  for Index := 0 to Items.Count - 1 do
  begin
    Item:= TMenuItem.Create(FMenu);
    Item.Caption:= Format('&%d %s', [Index, Items[Index]]);
    Item.OnClick:= MenuClicked;
    Item.Tag    := Index;

    FMenu.Add(Item);
  end;

end;

//------------------------------------------------------------------------------
procedure TRecent.MenuClicked(Sender: TObject);
var Index   : Integer;
var FileName: String;
begin
  Index:= TMenuItem(Sender).Tag;

  if (Index >= 0) and (Index < Items.Count) then
  begin
    FileName:= Items[Index];

    if FileExists(FileName) then
    begin
      if Assigned(OnClick) then OnClick(Self, FileName);
    end else
    begin
      Items.Delete(Index);

      UpdateMenu;

      MessageDlg( Format('The file %s could no longer be found.', [FileName]), mtError, [mbOK], 0);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TRecent.LoadFromXML(Node: IXMLNode);
var Index: Integer;
begin
  Items.BeginUpdate;
  Items.Clear;
  for Index := 0 to Node.ChildNodes.Count - 1 do
  begin
    Items.Add( Node.ChildNodes[Index].Attributes['filename'] )
  end;
  Items.EndUpdate;

  UpdateMenu;
end;

//------------------------------------------------------------------------------
procedure TRecent.SaveToXML(Node: IXMLNode);
var Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
  begin
    Node.AddChild('item').Attributes['filename']:= Items[Index];
  end;
end;

//------------------------------------------------------------------------------
procedure TRecent.SetMenu(const Value: TMenuItem);
begin
  FMenu := Value;

  UpdateMenu;
end;

{$ENDREGION}

{$REGION 'TSettings'}

//------------------------------------------------------------------------------
constructor TSettings.Create;
begin
  FRecent:= TRecent.Create;
end;

//------------------------------------------------------------------------------
destructor TSettings.Destroy;
begin
  FRecent.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TSettings.LoadFromFile(const FileName: String);
var XMLDocument    : IXMLDocument;
var XMLRoot     : IXMLNode;
var XMLRecent   : IXMLNode;
var XMLConfig   : IXMLNode;
var XMLGrid     : IXMLNode;
begin
  XMLDocument:= LoadXMLDocument(FileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxSkinEditor') then Exit;

  XMLRecent:= XMLRoot.ChildNodes.FindNode('Recent');

  if Assigned(XMLRecent) then
  begin
    Recent.LoadFromXML(XMLRecent);
  end;

  XMLConfig:= XMLRoot.ChildNodes.FindNode('Config');
  if Assigned(XMLConfig) then
  begin
  end;

  XMLGrid:= XMLRoot.ChildNodes.FindNode('Grid');
  if Assigned(XMLGrid) and Assigned(Grid) then
  begin
    Grid.LoadFromXml(XMLGrid);
  end;
end;

//------------------------------------------------------------------------------
procedure TSettings.SaveToFile(const FileName: String);
var XMLDocument : IXMLDocument;
var XMLRoot     : IXMLNode;
var XMLRecent   : IXMLNode;
var XMLConfig   : IXMLNode;
var XMLGrid     : IXMLNode;
begin

  XMLDocument:= NewXMLDocument();


  XMLRoot:= XMLDocument.AddChild('phxSkinEditor');

  XMLRecent:= XMLRoot.AddChild('Recent');
  Recent.SaveToXML(XMLRecent);

  XMLConfig:= XMLRoot.AddChild('Config');

  XMLGrid:= XMLRoot.AddChild('Grid');
  Grid.SaveToXML(XMLGrid);

  XMLDocument.SaveToFile(FileName);
end;

//------------------------------------------------------------------------------
procedure TSettings.SetGrid(const Value: TPHXEditorGrid);
begin
  FGrid := Value;
end;

{$ENDREGION}


{$REGION 'TDocument'}

// TDocument
//------------------------------------------------------------------------------
constructor TDocument.Create;
begin
  FSkin:= TPHXSkin.CreateEx(TPHXTexture.Create);
end;


//------------------------------------------------------------------------------
destructor TDocument.Destroy;
begin
  FSkin.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDocument.LoadDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FSkin.LoadFromXml(Name);
  end else
  begin
    FSkin.LoadFromFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FSkin.SaveToXml(Name);
  end else
  begin
    FSkin.SaveToFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.Changed;
begin
  Include(FState, dsChanged);

  ModActions.SetDocument(Self);
end;


{$ENDREGION}


constructor TModActions.Create(AOwner: TComponent);
var Index: Integer;
var List : TPHXGraphicFormats;
var Filter: String;
begin
  inherited;

  List:= GraphicFormats;

  Filter:= 'All supported image formats|';
  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + ';*'+ String(List.Items[Index].Extension);
  end;
  Filter:= Filter + '|';

  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + Format('%s|*.%s|', [List.Items[Index].Extension, List.Items[Index].Extension]);
  end;
  FDocumentCounter:= 1;

  FDocumentChanged:= TList<TDocumentEvent>.Create;

 // FDocument:= TDocument.Create;
 // FDocument.FileName := 'Unnamed';
  //FDocument.State    := [dsNew];

  FSettings:= TSettings.Create;
  FSettings.Recent.OnClick:= RecentClicked;

  OpenTextureDialog.Filter:= Filter;
  SaveImageDialog.Filter:= Filter;

  LoadSettings;
end;

//------------------------------------------------------------------------------
destructor TModActions.Destroy;
begin
  SaveSettings;

  FDocumentChanged.Free;

  FSettings.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TModActions.LoadSettings;
var FileName: String;
begin
  FileName:= ChangeFileExt( ParamStr(0), '.cfg');

  if not FileExists(FileName) then Exit;

  Settings.LoadFromFile(FileName);
end;


//------------------------------------------------------------------------------
procedure TModActions.SaveSettings;
var FileName: String;
begin
  FileName:= ChangeFileExt( ParamStr(0), '.cfg');

  Settings.SaveToFile(FileName);
end;

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

//------------------------------------------------------------------------------
Function TModActions.ImageForControl(const ClassName: String): Integer;
begin
  // TPHXWindow
  if SameText(ClassName, 'TPHXWindow') then
  begin
    Result:= ICON_WINDOW;
  end else
  // TPHXDialog
  if SameText(ClassName, 'TPHXDialog') then
  begin
    Result:= ICON_WINDOW;
  end else
  // TPHXLabel
  if SameText(ClassName, 'TPHXLabel') then
  begin
    Result:= ICON_LABEL;
  end else
  // TPHXButton
  if SameText(ClassName, 'TPHXButton') then
  begin
    Result:= ICON_BUTTON;
  end else
  // TPHXCheckbox
  if SameText(ClassName, 'TPHXCheckbox') then
  begin
    Result:= ICON_CHECKBOX;
  end else
  // TPHXRadioButton
  if SameText(ClassName, 'TPHXRadioButton') then
  begin
    Result:= ICON_RADIOBUTTON;
  end else
  // TPHXComboBox
  if SameText(ClassName, 'TPHXComboBox') then
  begin
    Result:= ICON_COMBOBOX;
  end else
  // TPHXPicture
  if SameText(ClassName, 'TPHXPicture') then
  begin
    Result:= ICON_IMAGE;
  end else
  // TPHXEdit
  if SameText(ClassName, 'TPHXEdit') then
  begin
    Result:= ICON_EDIT;
  end else
  // TPHXListBox
  if SameText(ClassName, 'TPHXListBox') then
  begin
    Result:= ICON_LISTBOX;
  end else
  // TPHXSlider
  if SameText(ClassName, 'TPHXSlider') then
  begin
    Result:= ICON_SLIDER;
  end else

  if SameText(ClassName, 'TPHXScrollBar') then
  begin
    Result:= ICON_SCROLLBAR;
  end else

  if SameText(ClassName, 'TPHXPanel') then
  begin
    Result:= ICON_PANEL;
  end else
 //   ICON_RADIOBUTTON   = 13;
//  ICON_SLIDER        = 14;

  // Other control
  begin
    Result:= ICON_CONTROL;
  end;

end;

//------------------------------------------------------------------------------
procedure TModActions.SetEditor(const Value: TPHXSkinEditor);
begin
  FEditor := Value;

  FSettings.Grid:= Editor.Grid;
end;

//------------------------------------------------------------------------------
procedure TModActions.SetDocument(const Value: TDocument);
var Event: TDocumentEvent;
begin
  FDocument := Value;

  if Assigned(FDocument) then
  begin
    Editor.Skin:= FDocument.Skin;
  end else
  begin
    Editor.Skin:= nil;
  end;

  for Event in DocumentChanged do
  begin
    Event(FDocument);
  end;
end;

//------------------------------------------------------------------------------
function TModActions.New: TDocument;
var Dialog: TFrmNewDialog;
begin
  Dialog:= TFrmNewDialog.Create(Self);
  try
    Dialog.Name:= Format('Skin%d', [FDocumentCounter] );

    if Dialog.ShowModal = mrOk then
    begin
      Result:= TDocument.Create;
      Result.Name   := Format('Skin%d', [FDocumentCounter] );
      Result.State  := [dsNew];

      Result.Skin.Name  := Dialog.Name;
      Result.Skin.Width := Dialog.Width;
      Result.Skin.Height:= Dialog.Height;
      Result.Skin.Texture.Resize(Dialog.Width, Dialog.Height, Dialog.Format);

      SetDocument(Result);

      Inc(FDocumentCounter);
    end else
    begin
      Result:= nil;
    end;
  finally
    Dialog.Free;
  end;

end;

//------------------------------------------------------------------------------
procedure TModActions.NewTexture;
var Document: TDocument;
var Name   : String;
var Bitmap: TPHXBitmap;
begin
  if OpenTextureDialog.Execute then
  begin
    Name:= ChangeFileExt(ExtractFileName(OpenTextureDialog.FileName), '');

    Bitmap:= TPHXBitmap.Create;
    try
      Document:= TDocument.Create;
      Document.Name := Name + PHXSKIN_EXT;
      Document.State:= [dsNew];

      Bitmap.LoadBitmap(OpenTextureDialog.FileName);

      if not isPowerOfTwo(Bitmap.Width) or not isPowerOfTwo(Bitmap.Height) then
      begin
        if (MessageDlg(STextureResize, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
           Bitmap.ResizeCanvasPowerOfTwo;
        end;
      end;

      Document.Skin.Texture.Import(Bitmap.Graphic);

      Document.Skin.Name       := Name;
      Document.Skin.Author     := GetUserFromWindows;
      Document.Skin.Version    := DateToStr(Now);
      Document.Skin.Comment    := 'Imported from ' + ExtractFileName(OpenTextureDialog.FileName);
      Document.Skin.Width      := Bitmap.Width;
      Document.Skin.Height     := Bitmap.Height;
    finally
      Bitmap.Free;
    end;

    SetDocument(Document);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.NewPacker;
var Document: TDocument;
var Index   : Integer;
var Item    : TPHXPackerItem;
var Element : TPHXSkinElement;
begin
  frmPacker:=TfrmPacker.Create(Self);
  try
    if (frmPacker.ShowDialog = mrOk) then
    begin
      Document:= TDocument.Create;

      Document.Skin.Name   := Format('Skin%d',[FDocumentCounter]);
      Document.Skin.Width  := frmPacker.Image.Width;
      Document.Skin.Height := frmPacker.Image.Width;
      Document.Skin.Texture.Import( frmPacker.Image.Width, frmPacker.Image.Height, frmPacker.Image.Format, frmPacker.Image.Pixels);
      Document.Skin.Initialize;

      Document.State := [dsNew];
      Document.Name  := Document.Skin.Name + PHXSKIN_EXT;

      for Index := 0 to frmPacker.Items.Count - 1 do
      begin
        Item:= frmPacker.Items[Index];

        Element:= Document.Skin.Elements.Add;
        Element.Name:= String(Item.Name);
        Element.Bounds:= Recti(Item.Position.X, Item.Position.Y, Item.Position.X + Item.Width,  Item.Position.Y + Item.Height);
      end;

      SetDocument(Document);

      Inc(FDocumentCounter);
    end;
  finally
     frmPacker.Free;
  end;
end;


//------------------------------------------------------------------------------
function TModActions.Open(const FileName: String): TDocument;
begin
  Result:= TDocument.Create;

  Result.Name  := FileName;
  Result.State   := [];

  Result.LoadDocument;

  Settings.Recent.Add(FileName);

  SetDocument(Result);
end;

//------------------------------------------------------------------------------
function TModActions.Close(SaveQuery: Boolean = False): Integer;
var ADocument: TDocument;
begin
  Result:= mrYes;

  if Assigned(Document) then
  begin
    ADocument:= Document;

    if SaveQuery and (dsChanged in Document.State) then
    begin
      Result:= MessageDlg(Format(SSaveQuery, [ADocument.Name]), mtConfirmation,  mbYesNoCancel, 0);

      case Result of
        mrNo:
        begin
          SetDocument(nil);
        end;
        mrYes:
        begin
          actFileSave.Execute;
        end;
        mrCancel:
        begin
          Exit;
        end;
      end;
    end;
    SetDocument(nil);

    ADocument.Free;
  end;
end;

{$REGION 'File actions'}

//------------------------------------------------------------------------------
procedure TModActions.actFileUpdate(Sender: TObject);
begin
  actFileSave  .Enabled:= Assigned(Document);
  actFileSaveAs.Enabled:= Assigned(Document);

  actFileImport.Enabled:= Assigned(Document);
  actFileExport.Enabled:= Assigned(Document);
end;

{
//------------------------------------------------------------------------------
function TModActions.New: TDocument;
var Name: String;
begin
  Name:= Format('Image%d', [DocumentCounter] );

  Result:= TDocument.Create;
  Result.Name := Name + PHXIMAGE_EXT;
  Result.State:= [dsNew];

  Result.Image.Name       := Name;
  Result.Image.Author     := GetUserFromWindows;
  Result.Image.Version    := DateToStr(Now);
  Result.Image.Comment    := '';

  SetDocument(Result);

  Inc(DocumentCounter);
end;
}

//------------------------------------------------------------------------------
procedure TModActions.actFileNewExecute(Sender: TObject);
var Dialog: TNewDialog;
begin
  Dialog:= TNewDialog.Create(Application);
  try
    if Dialog.Execute then
    begin

      case Dialog.Mode of
        nmEmpty:
        begin
          New;
        end;
        nmTexture:
        begin
          NewTexture;
        end;
        nmPacker:
        begin
          NewPacker;
        end;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Open(OpenDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveAsExecute(Sender: TObject);
var Name: String;
begin
  SaveDialog.FileName:= Document.Name;
  
  if SaveDialog.Execute then
  begin
    Name:= ChangeFileExt( ExtractFileName( SaveDialog.FileName), '');

    if not SameText(Name, Document.Skin.Name) and (MessageDlg( Format(SRenameText, [Document.Skin.Name, Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      Document.Skin.Name:= Name;
    end;
    Document.Name := SaveDialog.FileName;
    Document.State:= [];

    Document.SaveDocument;

    Settings.Recent.Add(SaveDialog.FileName);

    SetDocument(Document);
  end;
end;
  
//------------------------------------------------------------------------------
procedure TModActions.actFileSaveExecute(Sender: TObject);
begin
  if dsNew in Document.State then
  begin
    actFileSaveAs.Execute;
  end
  else
  begin
    Document.SaveDocument;
    Document.State:= [];

    SetDocument(Document);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.RecentClicked(Sender: TObject; const Filename: String);
begin
  Open(FileName);
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileImportExecute(Sender: TObject);
begin
  if OpenTextureDialog.Execute then
  begin
    if not Assigned(Editor.Skin) then
    begin
      actFileNew.Execute;
    end;
  
     Editor.Skin.Texture.LoadTexture(OpenTextureDialog.FileName);
  
     Document.State:= Document.State + [dsChanged];
  
     Editor.Skin.Width := Editor.Skin.Texture.Width;
     Editor.Skin.Height:= Editor.Skin.Texture.Height;
     Editor.ScrollToCenter;

     SetDocument(Document);
  end;
end;
  
//------------------------------------------------------------------------------
procedure TModActions.actFileExportExecute(Sender: TObject);
var Bitmap: TPHXBitmap;
begin
  
  if SaveImageDialog.Execute then
  begin
  
    Bitmap:= TPHXBitmap.Create(Editor.Skin.Texture.Graphic);
    try
      Bitmap.SaveBitmap(SaveImageDialog.FileName);
    finally
      Bitmap.Free;
    end;

  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileExitExecute(Sender: TObject);
begin
  FrmMain.Close;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
procedure TModActions.ActionZoom100Execute(Sender: TObject);
begin
  Editor.Viewport.Zoom100;
end;

//------------------------------------------------------------------------------
procedure TModActions.ActionZoomInExecute(Sender: TObject);
begin
  Editor.Viewport.ZoomIn;
end;

//------------------------------------------------------------------------------
procedure TModActions.ActionZoomOutExecute(Sender: TObject);
begin
  Editor.Viewport.ZoomOut;
end;


//------------------------------------------------------------------------------
procedure TModActions.actOptionsExecute(Sender: TObject);
begin
  FrmSettings:= TFrmSettings.Create(Self);
  if FrmSettings.Execute(Settings) then
  begin
    SaveSettings;
  end;
  FrmSettings.Free;
  FrmSettings:= nil;
end;

{$REGION 'Import & Export'}

//------------------------------------------------------------------------------
procedure TModActions.actImportUpdate(Sender: TObject);
begin
  actImportXML.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actImportXMLExecute(Sender: TObject);
begin
  if OpenXMLDialog.Execute then
  begin
    Document.Skin.LoadFromXml(OpenXMLDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actExportUpdate(Sender: TObject);
begin
  actExportXML   .Enabled:= Assigned(Document);
  actExportImages.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actExportXMLExecute(Sender: TObject);
begin
  SaveXMLDialog.InitialDir:= ExtractFilePath(Document.Name);
  SaveXMLDialog.FileName:= ExtractFileName(ChangeFileExt(Document.Name, '.xml'));

  if SaveXMLDialog.Execute then
  begin
    Document.Skin.SaveToXml(SaveXMLDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actExportImagesExecute(Sender: TObject);
var Dir    : String;
var Filename: String;
var Index  : Integer;
var Element: TPHXSkinElement;
begin
  if Assigned(Document) then
  begin
    Dir:= ExtractFilePath(ParamStr(0));

    if SelectDirectory('Select a directory', '', Dir) then
    begin


      for Index := 0 to Editor.Skin.Elements.Count - 1 do
      begin
        Element:= Editor.Skin.Elements[Index];

        Filename:= Dir + '\' + Element.Name + '.png';

        Editor.Skin.SaveElement(Filename, Index);
      end;

    end;
  end;
end;


{$ENDREGION}

{$REGION 'Tools'}

//------------------------------------------------------------------------------
procedure TModActions.actToolsUpdate(Sender: TObject);
begin

end;



//------------------------------------------------------------------------------
procedure TModActions.actToolsCopyElementNamesExecute(Sender: TObject);
var Lines: TStrings;
var Element: TPHXThemedPart;
begin
  Lines:= TStringList.Create;
  try

    for Element := Low(TPHXThemedPart) to High(TPHXThemedPart) do
    begin
        Lines.Add( PartToString(Element) );
    end;
    Clipboard.AsText:= Lines.Text;

  finally
    Lines.Free;
  end;

  MessageDlg('The names of all known skin element has been copied to the clipboard', mtInformation, [mbOK], 0);
end;


{$ENDREGION}

end.

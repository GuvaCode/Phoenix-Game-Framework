unit uActions;

interface

uses
  SysUtils, Classes, ImgList, Controls,ActnList,
  Menus, Dialogs, Forms, ClipBrd,

  FileCtrl,

 // xmldom, XMLIntf, msxmldom, XMLDoc,

  Generics.Collections,

  phxTypes,
  phxImage,
  phxImageEx,
  phxGraphicsEx,
  phxGraphics,
  phxDevice,
  phxTexture,
  phxEditor,

  uEditor,


  uImage.New,
  uImage.New.Empty,
  uImage.New.Packer,

  uTile.Wizard,
  uTile.Editor;

type


{$REGION 'TRecent'}

TRecentEvent = procedure(Sender: TObject; const Filename: String) of object;

//------------------------------------------------------------------------------
TRecent = class
  private
    FItems  : TStrings;
    FMenu   : TMenuItem;

    procedure MenuClicked(Sender: TObject);

    procedure UpdateMenu;

    procedure SetMenu(const Value: TMenuItem);
  public
    constructor Create;
    destructor Destroy; override;

   // procedure LoadFromXML(Node : IXMLNode);
   // procedure SaveToXML(Node : IXMLNode);

    procedure Add(const FileName: String);

    property Items: TStrings read FItems write FItems;
    property Menu: TMenuItem read FMenu write SetMenu;
  end;

{$ENDREGION}

{$REGION 'TSettings'}

//------------------------------------------------------------------------------
TSettings = class
  private
    FRecent: TRecent;
    FCenterPivots: Boolean;
    FGrid: TPHXEditorGrid;

    function GetFileName: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;

    property FileName: String read GetFileName;
    // List of recent documents
    property Recent: TRecent read FRecent;

     property Grid: TPHXEditorGrid read FGrid write FGrid;
    property CenterPivots: Boolean read FCenterPivots write FCenterPivots;
  end;

{$ENDREGION}

{$REGION 'TDocument'}

TDocument = class;

//------------------------------------------------------------------------------
TDocumentState = set of (
  dsNew,
  dsChanged
  );

TDocumentEvent = procedure(Document: TDocument) of object;

//------------------------------------------------------------------------------
TDocument = class
  public
    FName  : String;
    FState : TDocumentState;
    FImage : TPHXImage;
  public
    constructor Create; overload;
    destructor Destroy; override;

    // Load the document from a file.
    Procedure LoadDocument;
    // Save the document to a file.
    Procedure SaveDocument;

    procedure Changed;

    // The filename
    property Name: String read FName write FName;
    // State of the doccument
    property State: TDocumentState read FState write FState;
    // The image for the document
    property Image: TPHXImage read FImage;
  end;

{$ENDREGION}


//------------------------------------------------------------------------------
IPatternList = interface
  function GetSelected: Integer;
  procedure SetSelected(const Value: Integer);

  property Selected: Integer read GetSelected write SetSelected;
end;

//------------------------------------------------------------------------------
ITagList = interface
  function GetSelected: Integer;
  procedure SetSelected(const Value: Integer);

  property Selected: Integer read GetSelected write SetSelected;
end;

//------------------------------------------------------------------------------
TSelection = class
  private
    function GetPattern: Integer;
    procedure SetPattern(const Value: Integer);
    function GetTag: Integer;
    procedure SetTag(const Value: Integer);
  public
    property Tag: Integer read GetTag write SetTag;
    property Pattern: Integer read GetPattern write SetPattern;
 end;

//------------------------------------------------------------------------------
TEditorMode = (
  emImage,
  emPatterns,
  emTags
);

//------------------------------------------------------------------------------
TModActions = class(TDataModule)
    ActionList1: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actImageTextureLoad: TAction;
    actFileExit: TAction;
    ActionImages: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OpenTextureDialog: TOpenDialog;
    SaveTextureDialog: TSaveDialog;
    ListImages: TImageList;
    ToolBarImages: TImageList;
    actToolTexturePacker: TAction;
    OpenXMLDialog: TOpenDialog;
    SaveXMLDialog: TSaveDialog;
    actImportPatternXML: TAction;
    actExportPatternXML: TAction;
    actExportPatternImages: TAction;
    ToolActions: TActionList;
    actEditorZoomOut: TAction;
    actEditorZoomIn: TAction;
    actEditorZoomOriginal: TAction;
    ToolImages: TImageList;
    actImageTextureSave: TAction;
    actTileWizard: TAction;
    actExportTagXML: TAction;
    actImportTagXML: TAction;
    actEditSettings: TAction;
    actFileClose: TAction;
    actImageResize: TAction;
    actTileEditor: TAction;
    actToolShapeEditor: TAction;
    actEditCopy: TAction;
    actEditCut: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;

    procedure actFileUpdate(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actImageTextureLoadExecute(Sender: TObject);

    procedure actEditorUpdate(Sender: TObject);
    procedure actEditorZoomInExecute(Sender: TObject);
    procedure actEditorZoomOutExecute(Sender: TObject);
    procedure actEditorZoomOriginalExecute(Sender: TObject);

    procedure actToolTexturePackerExecute(Sender: TObject);

    procedure actImportPatternXMLExecute(Sender: TObject);
    procedure actExportPatternImagesExecute(Sender: TObject);
    procedure actExportPatternXMLExecute(Sender: TObject);
    procedure actImportUpdate(Sender: TObject);
    procedure actImageTextureSaveExecute(Sender: TObject);
    procedure actTileWizardExecute(Sender: TObject);
    procedure actTileUpdate(Sender: TObject);
    procedure actExportTagXMLExecute(Sender: TObject);
    procedure actImportTagXMLExecute(Sender: TObject);
    procedure actEditSettingsExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actImageUpdate(Sender: TObject);
    procedure actImageResizeExecute(Sender: TObject);
    procedure actTileEditorExecute(Sender: TObject);
    procedure actToolShapeEditorExecute(Sender: TObject);
    procedure actToolUpdate(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditUpdate(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
  private
    FSettings: TSettings;

    FDocument       : TDocument;
    FDocumentChanged: TList<TDocumentEvent>;

    FEditor  : TPHXImageEditor;
    FSelected: TSelection;



    ClipboardFormat_Pattern: Integer;
    ClipboardFormat_Tag    : Integer;

    FEditorMode: TEditorMode;

    FWindowPatterns: IPatternList;
    FWindowTags    : ITagList;

    procedure CreateEmpty;
    procedure CreateFromTexture;

    procedure SetDocument(const Value: TDocument);
    procedure SetEditor(const Value: TPHXImageEditor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Create a new image
    function New: TDocument;
    // Open a existing image
    function Open(const FileName: String): TDocument;
    // Close the current image
    function Close(SaveQuery: Boolean = False): Integer;


    property EditorMode: TEditorMode read FEditorMode write FEditorMode;
    // Editor settings
    property Settings: TSettings read FSettings;
    // The current document
    property Document: TDocument read FDocument write SetDocument;
    // Document listeners
    property DocumentChanged: TList<TDocumentEvent> read FDocumentChanged;
    // The image editor
    property Editor: TPHXImageEditor read FEditor write SetEditor;
    // Selected pattern and tag
    property Selected: TSelection read FSelected;


    property WindowPatterns: IPatternList read FWindowPatterns write FWindowPatterns;
    property WindowTags    : ITagList read FWindowTags write FWindowTags;
  end;

var
  ModActions: TModActions;

// Generate a new pattern name
function GeneratePatternName(Image: TPHXImage; const Template: String = 'Pattern'): ShortString;
// Generate a new tag name
function GenerateTagName(Image: TPHXImage; const Template: String = 'Tag'): ShortString;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses uSettings, uImage.Resize,
uShape;

resourcestring
  SRenameText = 'The image name "%s" doesnt match the filename.'#13'Do you want to rename the image to "%s" before saving?';
  SImageResize = 'The image is not power of two in size, resize and add a default pattern?';
  SImageSave = 'Save changes to "%s"?';


//------------------------------------------------------------------------------
function GetUserFromWindows: string;
var UserName    : String;
var UserNameLen : Cardinal;
Begin
{  UserNameLen := 255;

  SetLength(userName, UserNameLen) ;

  If GetUserName(PChar(UserName), UserNameLen) then
  begin
    Result := Copy(UserName,1,UserNameLen - 1)
  end else
  begin
     Result := 'Unknown';
  end;  }
end;

var PatternCounter: Integer = 1;

//------------------------------------------------------------------------------
function GeneratePatternName(Image: TPHXImage; const Template: String = 'Pattern'): ShortString;
var Name: String;
begin
  Name:=Format('%s%d', [Template, PatternCounter]);

  while Image.Patterns.IndexOf(Name) <> -1 do
  begin
    Inc(PatternCounter);

    Name:=Format('%s%d', [Template, PatternCounter]);
  end;

  Result:= ShortString(Name);
end;

var TagCounter: Integer = 1;

//------------------------------------------------------------------------------
function GenerateTagName(Image: TPHXImage; const Template: String = 'Tag'): ShortString;
var Name: String;
begin
  Name:=Format('%s%d', [Template, TagCounter]);

  while Image.Tags.IndexOf(Name) <> -1 do
  begin
    Inc(TagCounter);

    Name:=Format('%s%d', [Template, TagCounter]);
  end;

  Result:= ShortString(Name);
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
      ModActions.Open(Filename);
    end else
    begin
      Items.Delete(Index);

      UpdateMenu;

      MessageDlg( Format('The file %s could no longer be found.', [FileName]), mtError, [mbOK], 0);
    end;
  end;
end;

//------------------------------------------------------------------------------
{procedure TRecent.LoadFromXML(Node: IXMLNode);
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
end;}

//------------------------------------------------------------------------------
{procedure TRecent.SaveToXML(Node: IXMLNode);
var Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
  begin
    Node.AddChild('item').Attributes['filename']:= Items[Index];
  end;
end; }

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
  FGrid:= TPHXEditorGrid.Create(nil);

  FCenterPivots:= True;
end;

//------------------------------------------------------------------------------
destructor TSettings.Destroy;
begin
  FRecent.Free;
  FGrid.Free;

  inherited;
end;

//------------------------------------------------------------------------------
function TSettings.GetFileName: String;
begin
  Result:= ChangeFileExt( ParamStr(0), '.cfg');
end;

//------------------------------------------------------------------------------
procedure TSettings.LoadSettings;
//var XMLDocument    : IXMLDocument;
//var XMLRoot     : IXMLNode;
//var XMLNode    : IXMLNode;
begin
 { if not FileExists(FileName) then Exit;

  XMLDocument:= LoadXMLDocument(FileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxImageEditor') then Exit;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Recent');
  if Assigned(XMLNode) then
  begin
    Recent.LoadFromXML(XMLNode);
  end;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Settings');
  if Assigned(XMLNode) then
  begin
    if XMLNode.HasAttribute('CenterPivots') then CenterPivots:= XMLNode.Attributes['CenterPivots'];
    //FNormalLength:= Graphics.StringToColor(XMLNode.Attributes['NormalLength']);
  end;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Grid');
  if Assigned(XMLNode) then
  begin
    Grid.LoadFromXml(XMLNode);
  end;
 }

end;

//------------------------------------------------------------------------------
procedure TSettings.SaveSettings;
//var XMLDocument : IXMLDocument;
//var XMLRoot     : IXMLNode;
//var XMLNode   : IXMLNode;
begin
{
  XMLDocument:= NewXMLDocument();

  XMLRoot:= XMLDocument.AddChild('phxImageEditor');

  XMLNode:= XMLRoot.AddChild('Recent');
  begin
    Recent.SaveToXML(XMLNode);
  end;

  XMLNode:= XMLRoot.AddChild('Settings');
  begin
    XMLNode.Attributes['CenterPivots' ]:= CenterPivots;
    //XMLNode.Attributes['NormalColor' ]:= Graphics.ColorToString(FNormalColor);
    //XMLNode.Attributes['NormalLength']:=                        FNormalLength;
  end;

  XMLNode:=  XMLRoot.AddChild('Grid');
  if Assigned(XMLNode) then
  begin
    Grid.SaveToXML(XMLNode);
  end;


  XMLDocument.SaveToFile(FileName);
  }
  end;

{$ENDREGION}

{$REGION 'TDocument'}

// TDocument
//------------------------------------------------------------------------------
constructor TDocument.Create;
begin
  FImage:= TPHXImage.CreateEx(TPHXTexture.Create);
end;


//------------------------------------------------------------------------------
destructor TDocument.Destroy;
begin
  FImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDocument.LoadDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FImage.LoadFromXml(Name);
  end else
  begin
    try
      FImage.LoadFromFile(Name);
    except on E: Exception do
      MessageDlg('Failed to open image: ' +sLineBreak + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FImage.SaveToXml(Name);
  end else
  begin
    FImage.SaveToFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.Changed;
begin
  Include(FState, dsChanged);

  ModActions.SetDocument(Self);
end;


{$ENDREGION}

{$REGION 'TSelection'}

function TSelection.GetPattern: Integer;
begin
  Result:= ModActions.WindowPatterns.Selected;
end;

function TSelection.GetTag: Integer;
begin
  Result:= ModActions.WindowTags.Selected;
end;

procedure TSelection.SetPattern(const Value: Integer);
begin
  ModActions.FWindowPatterns.Selected:= Value;
end;

procedure TSelection.SetTag(const Value: Integer);
begin
  ModActions.FWindowTags.Selected:= Value;
end;

{$ENDREGION}





const
  ClipboardFormatStr_Pattern = 'TPHXPattern';
  ClipboardFormatStr_Tag     = 'TPHXTag';


// TModActions
//------------------------------------------------------------------------------
constructor TModActions.Create(AOwner: TComponent);
var Index: Integer;
var List : TPHXGraphicFormats;
var Filter: String;
begin
  inherited;
  List:= GraphicFormats;

  Filter:= 'All supported texture formats|';
  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + ';*' + String(List.Items[Index].Extension);
  end;
  Filter:= Filter + '|';

  for Index := 0 to List.Count - 1 do
  begin
    Filter:=Filter + Format('%s|*.%s|', [List.Items[Index].Extension, List.Items[Index].Extension]);
  end;

  OpenTextureDialog.Filter:= Filter;
  SaveTextureDialog.Filter:= Filter;

  ClipboardFormat_Pattern := RegisterClipboardFormat(ClipboardFormatStr_Pattern);
  ClipboardFormat_Tag     := RegisterClipboardFormat(ClipboardFormatStr_Tag);


  FSelected:= TSelection.Create;

  FDocumentChanged:= TList<TDocumentEvent>.Create;

  FSettings:= TSettings.Create;
  FSettings.LoadSettings;
end;

//------------------------------------------------------------------------------
destructor TModActions.Destroy;
begin
  FSettings.SaveSettings;
  FSettings.Free;

  FSelected.Free;

  FDocumentChanged.Free;

  inherited;
end;


{$REGION 'Document Functions'}



var DocumentCounter : Integer;

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

//------------------------------------------------------------------------------
function TModActions.Open(const FileName: String): TDocument;
begin
  Result:= nil;

  if FileExists(FileName) then
  begin
    Screen.Cursor:= crHourGlass;
    try
      Result:= TDocument.Create;
      Result.Name := FileName;
      Result.State:= [];

      Result.LoadDocument;

      Settings.Recent.Add(Filename);

      SetDocument(Result);
    finally
      Screen.Cursor:= crDefault;
    end;
  end;
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
      Result:= MessageDlg(Format(SImageSave, [ADocument.Name]), mtConfirmation,  mbYesNoCancel, 0);

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

//------------------------------------------------------------------------------
procedure TModActions.SetDocument(const Value: TDocument);
var Event: TDocumentEvent;
begin
  FDocument := Value;

  for Event in DocumentChanged do
  begin
    Event(FDocument);
  end;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
procedure TModActions.SetEditor(const Value: TPHXImageEditor);
begin
  FEditor := Value;

  if Assigned(FEditor) then
  begin
    FEditor.Grid.Assign(Settings.Grid);
  end;
end;

{$REGION 'File actions '}

//------------------------------------------------------------------------------
procedure TModActions.actFileUpdate(Sender: TObject);
begin
  actFileSave  .Enabled:= Assigned(Document);
  actFileSaveAs.Enabled:= Assigned(Document);
  actFileClose .Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.CreateEmpty;
var Dialog  : TFrmImageEmpty;
var Document: TDocument;
begin
  Dialog:= TFrmImageEmpty.Create(Self);
  try
    Dialog.ImageName:= Format('Image%d', [DocumentCounter] );

    if Dialog.ShowModal = mrOk then
    begin
      Document:= TDocument.Create;

      Document.State       := [dsNew];
      Document.Name        := Dialog.ImageName + '.phximg';

      Document.Image.Name       := Dialog.ImageName;
      Document.Image.Author     := GetUserFromWindows;
      Document.Image.Version    := DateToStr(Now);
      Document.Image.Comment    := '';

      Document.Image.Texture.Resize(Dialog.ImageWidth, Dialog.ImageHeight, Dialog.ImageFormat);
      Document.Image.Initialize;

      SetDocument(Document);

      Inc(DocumentCounter);
    end;
  finally
    Dialog.free;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.CreateFromTexture;
var Name: String;
var Document: TDocument;
var Bitmap: TPHXBitmap;
begin
  if OpenTextureDialog.Execute then
  begin
    Name:= ChangeFileExt(ExtractFileName(OpenTextureDialog.FileName), '');

    Bitmap:= TPHXBitmap.Create;
    try
      Document:= TDocument.Create;
      Document.Name := Name + PHXIMAGE_EXT;
      Document.State:= [dsNew];

      Bitmap.LoadBitmap(OpenTextureDialog.FileName);

      if not isPowerOfTwo(Bitmap.Width) or not isPowerOfTwo(Bitmap.Height) then
      begin
        if (MessageDlg(SImageResize, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          Document.Image.Patterns.Add('Image', 0, 0, Bitmap.Width, Bitmap.Height);

          Bitmap.ResizeCanvasPowerOfTwo;
        end;
      end;

      Document.Image.Texture.Import(Bitmap.Graphic); //.LoadImage(OpenTextureDialog.FileName);

      Document.Image.Name       := Name;
      Document.Image.Author     := GetUserFromWindows;
      Document.Image.Version    := DateToStr(Now);
      Document.Image.Comment    := 'Imported from ' + ExtractFileName(OpenTextureDialog.FileName);
      Document.Image.Width      := Bitmap.Width;
      Document.Image.Height     := Bitmap.Height;
    finally
      Bitmap.Free;
    end;

    SetDocument(Document);
  end;
end;


//------------------------------------------------------------------------------
procedure TModActions.actFileNewExecute(Sender: TObject);
var Dialog: TNewDialog;
begin
  Dialog:= TNewDialog.Create(Application);
  try
    if Dialog.Execute then
    begin

      case Dialog.Mode of
        newEmpty:
        begin
          CreateEmpty;
        end;
        newTexture:
        begin
          CreateFromTexture;
        end;
        newPacker:
        begin
          actToolTexturePacker.Execute;
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
procedure TModActions.actFileSaveExecute(Sender: TObject);
begin
  if dsNew in Document.State then
  begin
    actFileSaveAs.Execute;
  end
  else
  begin
    Document.State:= [];
    Document.SaveDocument;

    SetDocument(Document);

    Settings.Recent.Add(Document.Name);

  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveAsExecute(Sender: TObject);
var Name_: String;
begin
  SaveDialog.FileName:= Document.Name;

  if SaveDialog.Execute then
  begin
    Document.Name    :=  SaveDialog.FileName;
    Name_:= ChangeFileExt( ExtractFileName( SaveDialog.FileName), '');

    Document.State   := [];

    if not SameText(Name_, Document.Image.Name) and
    (MessageDlg( Format(SRenameText, [Document.Image.Name, Name_]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      Document.Image.Name:= Name_;
      SetDocument(FDocument);
    end;
    Document.SaveDocument;
    Settings.Recent.Add(SaveDialog.Filename);
  end;
end;


//------------------------------------------------------------------------------
procedure TModActions.actFileCloseExecute(Sender: TObject);
begin
  Close(True);
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

{$ENDREGION}

{$REGION 'Edit actions '}

//------------------------------------------------------------------------------
procedure TModActions.actEditUpdate(Sender: TObject);
begin
  case EditorMode of
    emImage:
    begin
      actEditCut  .Enabled:= False;
      actEditCopy .Enabled:= False;
      actEditPaste.Enabled:= False;
    end;
    emPatterns:
    begin
      actEditCut  .Enabled:= Assigned(Document) and (Selected.Pattern <> -1);
      actEditCopy .Enabled:= Assigned(Document) and (Selected.Pattern <> -1);
      actEditPaste.Enabled:= Clipboard.HasFormat(ClipboardFormat_Pattern);
    end;
    emTags:
    begin
      actEditCut  .Enabled:= Assigned(Document) and (Selected.Tag <> -1);
      actEditCopy .Enabled:= Assigned(Document) and (Selected.Tag <> -1);
      actEditPaste.Enabled:= Clipboard.HasFormat(ClipboardFormat_Tag);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actEditCutExecute(Sender: TObject);
begin
  actEditCopy.Execute;
  actEditDelete.Execute;
end;


//------------------------------------------------------------------------------
procedure TModActions.actEditCopyExecute(Sender: TObject);
var Stream : TMemoryStream;
var Image  : TPHXImage;
var Pattern: TPHXPattern;
var Tag    : TPHXTag;
begin
  Image:= Document.Image;

  if (EditorMode = emPatterns) and (Selected.Pattern >= 0) and (Selected.Pattern < Image.Patterns.Count) then
  begin
    Pattern:= Image.Patterns[Selected.Pattern];

    Stream:= TMemoryStream.Create;
    try

      Stream.Write(Pattern, SizeOf(Pattern));
      /// fixme CopyStreamToClipboard(ClipboardFormat_Pattern, Stream);

    finally
      Stream.Free;
    end;

  end;
  if (EditorMode = emTags) and (Selected.Tag >= 0) and (Selected.Tag < Image.Tags.Count) then
  begin
    Tag:= Image.Tags[Selected.Tag];

    Stream:= TMemoryStream.Create;
    try
      Stream.Write(Tag, SizeOf(Tag));

      //// fixme CopyStreamToClipboard(ClipboardFormat_Tag, Stream);
    finally
      Stream.Free;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TModActions.actEditPasteExecute(Sender: TObject);
var Stream : TMemoryStream;
var Image  : TPHXImage;
var Pattern: TPHXPattern;
var Tag    : TPHXTag;
begin
  Image:= Document.Image;

  if (EditorMode = emPatterns) and Clipboard.HasFormat(ClipboardFormat_Pattern)  then
  begin
    Stream:= TMemoryStream.Create;
    try
      /// fixme CopyStreamFromClipboard(ClipboardFormat_Pattern, Stream);

      Stream.Read(Pattern, SizeOf(Pattern));

      // Generate a new name if the pattern exists
      if Image.Patterns.IndexOf(String(Pattern.Name)) >= 0 then
      begin
        Pattern.Name:= GeneratePatternName(Image, String(Pattern.Name));
      end;

      Image.Patterns.Add(Pattern);

      Selected.Pattern:= Image.Patterns.Count-1;
    finally
      Stream.Free;
    end;
    Document.Changed;
  end;
  if (EditorMode = emTags) and Clipboard.HasFormat(ClipboardFormat_Tag) then
  begin
    Stream:= TMemoryStream.Create;
    try
      //// fixme  CopyStreamFromClipboard(ClipboardFormat_Tag, Stream);

      Stream.Read(Tag, SizeOf(Tag));

      // Generate a new name if the tag exists
      if Image.Tags.IndexOf(String(Tag.Name)) >= 0 then
      begin
        Tag.Name:= GenerateTagName(Image, String(Tag.Name));
      end;
      Image.Tags.Add(Tag);

      Selected.Tag:= Image.Tags.Count-1;
    finally
      Stream.Free;
    end;
    Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actEditDeleteExecute(Sender: TObject);
begin  {
//  Editor.Selection.PatternIndex:= lwPatterns.ItemIndex;
  if (Editor.CurrentTab = edPattern)  then
  begin
    if MessageDlg( Format('Delete pattern %s?',  [Editor.Selection.Pattern.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      actPatternDel.Execute;
    end;
  end;
  if (Editor.CurrentTab = edTag)  then
  begin
    if MessageDlg( Format('Delete tag %s?',  [Editor.Selection.Tag.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      actTagDel.Execute;
    end;
  end;   }
end;





//------------------------------------------------------------------------------
procedure TModActions.actEditSettingsExecute(Sender: TObject);
var Dialog: TFrmSettings;
begin
  Dialog:= TFrmSettings.Create(Application);
  try
    if Dialog.Execute(Settings) then
    begin
      Settings.SaveSettings;

      Editor.Grid.Assign(Settings.Grid);

      Editor.Invalidate;
    end;

  finally
    Dialog.Free;
  end;

end;

{$ENDREGION}

{$REGION 'Image actions '}

//------------------------------------------------------------------------------
procedure TModActions.actImageUpdate(Sender: TObject);
begin
  actImageResize .Enabled:= Assigned(Document);

  actImageTextureLoad   .Enabled:= Assigned(Document);
  actImageTextureSave   .Enabled:= Assigned(Document);
end;

// Resizes the current image
//------------------------------------------------------------------------------
procedure TModActions.actImageResizeExecute(Sender: TObject);
var Dialog: TFrmImageResize;
begin
  Dialog:= TFrmImageResize.Create(Application);
  try
    Dialog.ImageWidth := Document.Image.Texture.Width;
    Dialog.ImageHeight:= Document.Image.Texture.Height;
    Dialog.ImageFormat:= Document.Image.Texture.Format;

    if Dialog.Execute then
    begin
      Document.Image.Texture.Resize(Dialog.ImageWidth, Dialog.ImageHeight, Dialog.ImageFormat);

      Document.Image.Initialize;

      Document.Changed;
    end;

  finally
    Dialog.Free;
  end;
end;

{$ENDREGION}

{$REGION 'Editor actions '}

//------------------------------------------------------------------------------
procedure TModActions.actEditorUpdate(Sender: TObject);
begin
  actEditorZoomOriginal.Enabled:=Assigned(Document);
  actEditorZoomIn      .Enabled:=Assigned(Document);
  actEditorZoomOut    .Enabled:=Assigned(Document);
end;


//------------------------------------------------------------------------------
procedure TModActions.actEditorZoomInExecute(Sender: TObject);
begin
  Editor.Viewport.ZoomIn;
end;

//------------------------------------------------------------------------------
procedure TModActions.actEditorZoomOriginalExecute(Sender: TObject);
begin
  Editor.Viewport.Zoom:= 1.0;

//  Editor.ScrollToCenter;
end;

//------------------------------------------------------------------------------
procedure TModActions.actEditorZoomOutExecute(Sender: TObject);
begin
  Editor.Viewport.ZoomOut;
end;





{$ENDREGION}

{$REGION 'Tool actions '}

//------------------------------------------------------------------------------
procedure TModActions.actToolUpdate(Sender: TObject);
begin
  actToolShapeEditor.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolShapeEditorExecute(Sender: TObject);
begin
  if Assigned(Document) then
  begin
    FrmShapeEditor.Execute(Document.Image);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolTexturePackerExecute(Sender: TObject);
var Document: TDocument;
var Index   : Integer;
var Item    : TPHXPackerItem;
var Pattern: TPHXPattern;
begin
  frmPacker:=TfrmPacker.Create(Self);
  try
    if frmPacker.Execute then
    begin
      Document:= TDocument.Create;

      Document.Image.Name  := Format('Image%d',[DocumentCounter]);
      Document.State       := [dsNew];
      Document.Name        := Document.Image.Name + '.phximg';

//      Document.Image.Compressor:= GraphicFormats.FindExporter('.png');
      Document.Image.Texture.Import( frmPacker.Image.Width, frmPacker.Image.Height, frmPacker.Image.Format, frmPacker.Image.Pixels);
      Document.Image.Initialize;


      Document.Image.Patterns.Clear;
      for Index := 0 to frmPacker.Items.Count - 1 do
      begin
        Item:= frmPacker.Items[Index];

        Pattern.Name   := ShortString(Item.Name);
        Pattern.X      := Item.Position.X;
        Pattern.Y      := Item.Position.Y;
        Pattern.Width  := Item.Width;
        Pattern.Height := Item.Height;
        Pattern.Mirror := False;
        Pattern.Flip   := False;

       if Settings.CenterPivots then
        begin
          Pattern.Pivot.X:= Pattern.Width  div 2;
          Pattern.Pivot.Y:= Pattern.Height div 2;
        end else
        begin
          Pattern.Pivot.X:= 0;
          Pattern.Pivot.Y:= 0;
        end;

        Document.Image.Patterns.Add(Pattern);
      end;

      SetDocument(Document);

      Inc(DocumentCounter);
    end;

  finally
     frmPacker.Free;
  end;
end;




{$ENDREGION}

{$REGION 'Tile actions '}

//------------------------------------------------------------------------------
procedure TModActions.actTileUpdate(Sender: TObject);
begin
  actTileWizard.Enabled:= Assigned(Document);
  actTileEditor.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actTileEditorExecute(Sender: TObject);
var Dialog: TFrmTileEditor;
begin
  Dialog:= TFrmTileEditor.Create(Application);
  try
    if Dialog.Execute(Document.Image) then
    begin
      Document.Changed;
    end;
  finally
    Dialog.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actTileWizardExecute(Sender: TObject);
var Dialog: TFrmTileWizard;
begin
  Dialog:= TFrmTileWizard.Create(Application);
  try
    if Dialog.Execute(Document.Image) then
    begin
      Document.Changed;
    end;
  finally
    Dialog.Free;
  end;
end;

{$ENDREGION}


{$REGION 'Import & Export actions '}

//------------------------------------------------------------------------------
procedure TModActions.actImportUpdate(Sender: TObject);
begin

  actImportPatternXML.Enabled:= Assigned(Document);

  actExportPatternXML   .Enabled:= Assigned(Document);
  actExportPatternImages.Enabled:= Assigned(Document);

  actExportTagXML.Enabled:= Assigned(Document);
  actImportTagXML.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actImageTextureLoadExecute(Sender: TObject);
var Bitmap: TPHXBitmap;
begin
  if OpenTextureDialog.Execute then
  begin
    Bitmap:= TPHXBitmap.Create;
    try
      Bitmap.LoadBitmap(OpenTextureDialog.FileName);

      Document.Image.Texture.Import(Bitmap.Graphic);
    finally
      Bitmap.Free;
    end;
    Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actImageTextureSaveExecute(Sender: TObject);
var Bitmap: TPHXBitmap;
begin
  if SaveTextureDialog.Execute then
  begin
    Bitmap:= TPHXBitmap.Create;
    try
     {$IFDEF LINUX} { #todo : разобраться с какого хера инверсия в лине  }
       Bitmap.ImportAndSwapColor(Document.Image.Texture.Graphic);
     {$ELSE}
       Bitmap.Import(Document.Image.Texture.Graphic, pcRGBA);
     {$ENDIF}
       Bitmap.SaveBitmap(SaveTextureDialog.FileName);
    finally
      Bitmap.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actExportPatternXMLExecute(Sender: TObject);
var Index  : Integer;
var Pattern: TPHXPattern;
//var Doc    : IXMLDocument;
//var Node   : IXMLNode;
begin
 { if SaveXMLDialog.Execute then
  begin
    Doc:= NewXMLDocument;
    Doc.DocumentElement:=Doc.AddChild('patterns');
    Doc.Options:= Doc.Options + [doNodeAutoIndent];

    for Index := 0 to Editor.Image.Patterns.Count - 1 do
    begin
      Pattern:= Editor.Image.Patterns[Index];

      Node:= Doc.DocumentElement.AddChild('pattern');
      Node.Attributes['name']:=Pattern.Name;
      Node.Attributes['x']:=Pattern.X;
      Node.Attributes['y']:=Pattern.Y;
      Node.Attributes['width']:=Pattern.Width;
      Node.Attributes['height']:=Pattern.Height;
      Node.Attributes['pivot.x']:=Pattern.Pivot.X;
      Node.Attributes['pivot.y']:=Pattern.Pivot.Y;
    end;
    Doc.SaveToFile(SaveXMLDialog.FileName);
  end;}
end;

//------------------------------------------------------------------------------
procedure TModActions.actExportTagXMLExecute(Sender: TObject);
var Index  : Integer;
var Tag    : TPHXTag;
//var Doc    : IXMLDocument;
//var Node   : IXMLNode;
begin
{  if SaveXMLDialog.Execute then
  begin
    Doc:= NewXMLDocument;
    Doc.DocumentElement:=Doc.AddChild('Tags');
    Doc.Options:= Doc.Options + [doNodeAutoIndent];

    for Index := 0 to Editor.Image.Tags.Count - 1 do
    begin
      Tag:= Editor.Image.Tags[Index];

      Node:= Doc.DocumentElement.AddChild('Tag');
      Node.Attributes['Name'    ]:= Tag.Name;
      Node.Attributes['Pattern' ]:= Tag.Pattern;
      Node.Attributes['X'       ]:= Tag.X;
      Node.Attributes['Y'       ]:= Tag.Y;
      Node.Attributes['Rotation']:= Tag.Rotation;
    end;

    Doc.SaveToFile(SaveXMLDialog.FileName);
  end; }
end;



//------------------------------------------------------------------------------
procedure TModActions.actImportPatternXMLExecute(Sender: TObject);
var Index  : Integer;
var Pattern: TPHXPattern;
//var Doc    : IXMLDocument;
//var Node   : IXMLNode;
begin
{  if OpenXMLDialog.Execute then
  begin
    Doc:= LoadXMLDocument(OpenXMLDialog.FileName);

    if not SameText(Doc.DocumentElement.NodeName, 'patterns') then
    begin
      raise Exception.Create('Not a valid pattern export file');
    end;


    for Index := 0 to Doc.DocumentElement.ChildNodes.Count - 1 do
    begin
      Node:= Doc.DocumentElement.ChildNodes[Index];

      Pattern.Name    := ShortString(Node.Attributes['name']);
      Pattern.X       :=             Node.Attributes['x'];
      Pattern.Y       :=             Node.Attributes['y'];
      Pattern.Width   :=             Node.Attributes['width'];
      Pattern.Height  :=             Node.Attributes['height'];
      Pattern.Pivot.X :=             Node.Attributes['pivot.x'];
      Pattern.Pivot.Y :=             Node.Attributes['pivot.y'];

      Editor.Image.Patterns.Add(Pattern);
    end;
    Document.Changed
  end; }
end;

//------------------------------------------------------------------------------
procedure TModActions.actImportTagXMLExecute(Sender: TObject);
var Index  : Integer;
var Tag    : TPHXTag;
//var Doc    : IXMLDocument;
//var Node   : IXMLNode;
begin
{  if OpenXMLDialog.Execute then
  begin
    Doc:= LoadXMLDocument(OpenXMLDialog.FileName);

    if not SameText(Doc.DocumentElement.NodeName, 'Tags') then
    begin
      raise Exception.Create('Not a valid tag export file');
    end;

    Editor.Image.Tags.Clear;

    for Index := 0 to Doc.DocumentElement.ChildNodes.Count - 1 do
    begin
      Node:= Doc.DocumentElement.ChildNodes[Index];

      Tag.Name    := ShortString(Node.Attributes['Name'    ]);
      Tag.Pattern :=             Node.Attributes['Pattern' ];
      Tag.X       :=             Node.Attributes['X'       ];
      Tag.Y       :=             Node.Attributes['Y'       ];
      Tag.Rotation:=             Node.Attributes['Rotation'];

      Editor.Image.Tags.Add(Tag);
    end;
    Document.Changed
  end;}
end;


//------------------------------------------------------------------------------
procedure TModActions.actExportPatternImagesExecute(Sender: TObject);
var Dir    : String;
var Filename: String;
var Index  : Integer;
var Pattern: TPHXPattern;
begin
  if Assigned(Editor.Image) then
  begin {
    if not Assigned(Editor.Image.Compressor) then
    begin
      raise Exception.Create('You have to select a compressor for the image first');
    end;
           }

    // fix mememem
    {   Dir:= ExtractFilePath(ParamStr(0));

    if SelectDirectory('Select directory to save images in', '', Dir, [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI, sdValidateDir]) then
    begin

      Screen.Cursor:= crHourGlass;
      try
        for Index := 0 to Editor.Image.Patterns.Count - 1 do
        begin
          Pattern:= Editor.Image.Patterns[Index];

          Filename:= Dir + '\' + String(Editor.Image.Patterns[Index].Name) + '.png';

          Editor.Image.SavePattern(Filename, Index);
        end;
        MessageDlg( Format('Exported %d patterns to %s.', [Editor.Image.Patterns.Count, Dir]), mtInformation, [mbOK], 0);
      finally
        Screen.Cursor:= crDefault;
      end;

    end;
  end;}
end;

end;

{$ENDREGION}




end.

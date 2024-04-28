unit uActions;

interface

uses
  System.SysUtils, System.Classes, Vcl.ImgList, Vcl.Controls, Vcl.ActnList, Windows,
  Menus, Dialogs, Forms, FileCtrl,

  xmldom, XMLIntf, msxmldom, XMLDoc,

  Generics.Collections,

  phxTypes,
  phxGraphics,
  phxDevice,
  phxTexture,

  phxParticle,
  phxParticlePresets,

  uEditor;

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

    procedure LoadFromXML(Node : IXMLNode);
    procedure SaveToXML(Node : IXMLNode);

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

    function GetFileName: String;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;

    property FileName: String read GetFileName;
    // List of recent documents
    property Recent: TRecent read FRecent;
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

    FEffect: TPHXParticleEffect;
  public
    constructor Create; overload;
    constructor Create(AEffect: TPHXParticleEffect); overload;
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
    // The effect for the document
    property Effect: TPHXParticleEffect read FEffect;
  end;

{$ENDREGION}

//------------------------------------------------------------------------------
TModActions = class(TDataModule)
    ActionList1: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    ActionImages: TImageList;
    OpenTextureDialog: TOpenDialog;
    SaveTextureDialog: TSaveDialog;
    OpenXMLDialog: TOpenDialog;
    SaveXMLDialog: TSaveDialog;
    actPresetFire: TAction;
    actPresetJumpgate: TAction;
    ToolImages: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actSystemQuota: TAction;
    actDebugGraphs: TAction;
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actPresetFireExecute(Sender: TObject);
    procedure actPresetJumpgateExecute(Sender: TObject);
    procedure actSystemQuotaExecute(Sender: TObject);
    procedure actDebugGraphsExecute(Sender: TObject);
  private
    FSettings: TSettings;

    FDocument       : TDocument;
    FDocumentChanged: TList<TDocumentEvent>;

    FDevice: TPHXDevice;
    // Particle manager
    FManager: TPHXParticleManager;
    // List of textures
    FTextures: TPHXTextureList;

    procedure SetDocument(const Value: TDocument);
    procedure SetDevice(const Value: TPHXDevice);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Create a new particle effect
    function New: TDocument; overload;
    // Create a new particle effect
    function New(Effect: TPHXParticleEffect): TDocument; overload;

    // Open a existing image
    function Open(const FileName: String): TDocument;
    // Close the current image
    function Close(SaveQuery: Boolean = False): Integer;

    function LoadTexture(const FileName: String): TPHXTexture;

   // function FindTexture(const Name: String): TPHXTexture;

    property Device: TPHXDevice read FDevice write SetDevice;
    // Editor settings
    property Settings: TSettings read FSettings;
    // The current document
    property Document: TDocument read FDocument write SetDocument;
    // Document listeners
    property DocumentChanged: TList<TDocumentEvent> read FDocumentChanged;
    // Particle manager
    property Manager: TPHXParticleManager read FManager;
    // List of loaded textures
    property Textures: TPHXTextureList read FTextures;
  end;


var
  ModActions: TModActions;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses uDebugGraphs;

{$R *.dfm}

resourcestring
  SRenameText = 'The effect name "%s" doesnt match the filename.'#13'Do you want to rename the effect to "%s" before saving?';
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
function TSettings.GetFileName: String;
begin
  Result:= ChangeFileExt( ParamStr(0), '.cfg');
end;

//------------------------------------------------------------------------------
procedure TSettings.LoadSettings;
var XMLDocument    : IXMLDocument;
var XMLRoot     : IXMLNode;
var XMLNode    : IXMLNode;
begin
  if not FileExists(FileName) then Exit;

  XMLDocument:= LoadXMLDocument(FileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxParticleEditor') then Exit;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Recent');
  if Assigned(XMLNode) then
  begin
    Recent.LoadFromXML(XMLNode);
  end;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Settings');
  if Assigned(XMLNode) then
  begin
    //FNormalLength:= Graphics.StringToColor(XMLNode.Attributes['NormalLength']);
  end;
end;

//------------------------------------------------------------------------------
procedure TSettings.SaveSettings;
var XMLDocument : IXMLDocument;
var XMLRoot     : IXMLNode;
var XMLNode   : IXMLNode;
begin

  XMLDocument:= NewXMLDocument();

  XMLRoot:= XMLDocument.AddChild('phxParticleEditor');

  XMLNode:= XMLRoot.AddChild('Recent');
  begin
    Recent.SaveToXML(XMLNode);
  end;

  XMLNode:= XMLRoot.AddChild('Settings');
  begin

  end;
  XMLDocument.SaveToFile(FileName);
end;

{$ENDREGION}

{$REGION 'TDocument'}

// TDocument
//------------------------------------------------------------------------------
constructor TDocument.Create;
begin
  FEffect:= TPHXParticleEffect.Create(ModActions.Manager);
end;

//------------------------------------------------------------------------------
constructor TDocument.Create(AEffect: TPHXParticleEffect);
begin
  FEffect:= AEffect;
end;

//------------------------------------------------------------------------------
destructor TDocument.Destroy;
begin
  FEffect.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDocument.LoadDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);
  try
    if SameText(FileExt, '.xml') then
    begin
      FEffect.LoadFromXml(Name);
    end else
    begin
      FEffect.LoadFromFile(Name);
    end;
  except on E: Exception do
    MessageDlg('Failed to open effect: ' + sLineBreak + E.Message, mtError, [mbOK], 0);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FEffect.SaveToXml(Name);
  end else
  begin
    FEffect.SaveToFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.Changed;
begin
  Include(FState, dsChanged);

  ModActions.SetDocument(Self);
end;


{$ENDREGION}

// TModActions
//==============================================================================
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

  FTextures:= TPHXTextureList.Create(nil);

  FManager:= TPHXParticleManager.Create;
  FManager.Textures:= FTextures;
  FManager.Quota   := 5;

  FDocumentChanged:= TList<TDocumentEvent>.Create;

  FSettings:= TSettings.Create;
  FSettings.LoadSettings;
end;

//------------------------------------------------------------------------------
destructor TModActions.Destroy;
begin
  FSettings.SaveSettings;
  FSettings.Free;

  FManager.Free;

  FTextures.Free;

  FDocumentChanged.Free;

  inherited;
end;

//------------------------------------------------------------------------------
function TModActions.LoadTexture(const FileName: String): TPHXTexture;
begin
  if FileExists(FileName) then
  begin
    Result:= FTextures.LoadTexture(FileName);
  end else
  begin
    Result:= nil;
  end;
end;
   {
//------------------------------------------------------------------------------
function TModActions.FindTexture(const Name: String): TPHXTexture;
var
var Texture: TPHXTexture;
begin
  for Texture in FTextures do
  begin
    if SameText(Texture.Name, Name) then
    begin
      Exit(Texture);
    end;
  end;
  Result:= nil;
end;
    }
{$REGION 'Document Functions'}



var DocumentCounter : Integer;

//------------------------------------------------------------------------------
function TModActions.New: TDocument;
var Name: String;
begin
  Name:= Format('Effect%d', [DocumentCounter] );

  Result:= TDocument.Create;
  Result.Name := Name + PHXPARTICLE_EXT;
  Result.State:= [dsNew];

  Result.Effect.Name       := Name;
  Result.Effect.Author     := GetUserFromWindows;
  Result.Effect.Version    := DateToStr(Now);
  Result.Effect.Comment    := '';

  SetDocument(Result);

  Inc(DocumentCounter);
end;

//------------------------------------------------------------------------------
function TModActions.New(Effect: TPHXParticleEffect): TDocument;
var Name: String;
begin
  Name:= Format('%s%d', [Effect.Name, DocumentCounter] );

  Result:= TDocument.Create(Effect);
  Result.Name := Name + PHXPARTICLE_EXT;
  Result.State:= [dsNew];

  Result.Effect.Name       := Name;
  Result.Effect.Author     := GetUserFromWindows;
  Result.Effect.Version    := DateToStr(Now);
  Result.Effect.Comment    := '';

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
      Result:= MessageDlg(Format(SSaveQuery, [Document.Name]), mtConfirmation,  mbYesNoCancel, 0);

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
procedure TModActions.SetDevice(const Value: TPHXDevice);
begin
  FDevice := Value;

  FTextures.Device:= Value;
end;

//------------------------------------------------------------------------------
procedure TModActions.SetDocument(const Value: TDocument);
var Event: TDocumentEvent;
var Effect: TPHXParticleEffect;
begin
  if FDocument <> Value then
  begin
    FManager.Clear;

    FDocument:= Value;

  end;

  if Assigned(FDocument) then
  begin
    Effect:= FDocument.Effect;

    if (Effect.TextureName <> '') and (Effect.Texture = nil) then
    begin
      if MessageDlg(Format('The texture "%s" uses by the effect is not loaded, load it now?', [Effect.TextureName]), mtConfirmation, mbYesNo, 0) = mrYes then
      begin
        OpenTextureDialog.FileName:= Effect.TextureName;

        if OpenTextureDialog.Execute then
        begin
          LoadTexture(OpenTextureDialog.FileName);

          Effect.TextureName:= Effect.TextureName;
        end;
      end;
    end;
  end;

  for Event in DocumentChanged do
  begin
    Event(FDocument);
  end;
end;

{$ENDREGION}

{$REGION 'File actions '}

//------------------------------------------------------------------------------
procedure TModActions.actFileUpdate(Sender: TObject);
begin
  actFileSave  .Enabled:= Assigned(Document);
  actFileSaveAs.Enabled:= Assigned(Document);
end;


//------------------------------------------------------------------------------
procedure TModActions.actFileNewExecute(Sender: TObject);
begin
  New();
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
var Name: String;
begin
  SaveDialog.FileName:= Document.Name;

  if SaveDialog.Execute then
  begin
    Name:= ChangeFileExt( ExtractFileName( SaveDialog.FileName), '');

    if not SameText(Name, Document.Effect.Name) and (MessageDlg( Format(SRenameText, [Document.Effect.Name, Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      Document.Effect.Name:= Name;
    end;

    Document.Name    := SaveDialog.FileName;
    Document.State   := [];
    Document.SaveDocument;

    Settings.Recent.Add(SaveDialog.Filename);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

{$ENDREGION}

{$REGION 'Preset actions '}

//------------------------------------------------------------------------------
procedure TModActions.actPresetFireExecute(Sender: TObject);
begin
  New(TPHXParticlePresets.Fire(FManager));
end;

//------------------------------------------------------------------------------
procedure TModActions.actPresetJumpgateExecute(Sender: TObject);
begin
  New(TPHXParticlePresets.Jumpgate(FManager));
end;


{$ENDREGION}

//------------------------------------------------------------------------------
procedure TModActions.actSystemQuotaExecute(Sender: TObject);
var Value: String;
begin
  Value:= IntToStr(FManager.Quota);

  if InputQuery('Set system quota', 'Value', Value) then
  begin
    FManager.Quota:= StrToIntDef(Value, FManager.Quota);

    SetDocument(FDocument);
  end;
end;


{$REGION 'Debug actions '}

procedure TModActions.actDebugGraphsExecute(Sender: TObject);
begin
 FrmDebugGraphs.Effect:= FDocument.Effect;
 FrmDebugGraphs.Show;
end;

{$ENDREGION}

end.

unit uActions;

interface

uses
  SysUtils, Classes, Dialogs, ImgList, Controls, Menus, Forms, ActnList,
  Graphics,

  Generics.Collections,

 // xmldom, XMLIntf, msxmldom, XMLDoc,

  phxTypes,
  phxmath,
  phxDevice,
  phxModel,
  phxTexture,
  phxGraphics,

  uModel;

type

{$REGION 'TPHXMeshEx'}

TPHXMeshEx = class helper for TPHXMesh
  private
    function MeasureBounds: TBoxf;
  public
    function MeasureSize: TVector3f;
    function MeasureCenter: TVector3f;
  end;

{$ENDREGION}

{$REGION 'TRecent'}

TRecentEvent = procedure(Sender: TObject; const Filename: String) of object;

//------------------------------------------------------------------------------
TRecent = class
  private
    FItems  : TStrings;
    FMenu   : TMenuItem;
    FOnClick: TRecentEvent;
    procedure SetMenu(const Value: TMenuItem);
    procedure UpdateMenu;
    procedure MenuClicked(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

  //  procedure LoadFromXML(Node : IXMLNode);
  //  procedure SaveToXML(Node : IXMLNode);

    procedure Add(const FileName: String);


    property Items: TStrings read FItems write FItems;
    property Menu: TMenuItem read FMenu write SetMenu;
    property OnClick: TRecentEvent read FOnClick write FOnClick;
  end;

{$ENDREGION}

{$REGION 'TSettings'}

//------------------------------------------------------------------------------
TViewMode = (
  vmPerspective,
  vmOrthoTop,
  vmOrthoBottom,
  vmOrthoY,
  vmOrthoZ
);

//------------------------------------------------------------------------------
TSettings = class
  private
    FRecent: TRecent;

    FViewMode: TViewMode;

    FNormalLength: Single;
    FNormalColor: TColor;

    FShowNormals  : Boolean;
    FShowWireframe: Boolean;
    FShowGrid     : Boolean;
    FShowBounds   : Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);

    // List of recent documents
    property Recent: TRecent read FRecent;

    property ViewMode : TViewMode read FViewMode write FViewMode;

    property NormalColor : TColor read FNormalColor write FNormalColor;
    property NormalLength: Single read FNormalLength write FNormalLength;

    property ShowNormals: Boolean read FShowNormals write FShowNormals;
    property ShowWireframe: Boolean read FShowWireframe write FShowWireframe;
    property ShowBounds: Boolean read FShowBounds write FShowBounds;
    property ShowGrid: Boolean read FShowGrid write FShowGrid;
  end;

{$ENDREGION}

{$REGION 'TDocument'}

//------------------------------------------------------------------------------
TDocumentState = set of (
  dsNew,
  dsChanged
);

//------------------------------------------------------------------------------
TDocument = class
  private
    FMesh    : TPHXMesh;
    FFileName: String;
    FState   : TDocumentState;
  public
    constructor Create; overload;
    destructor Destroy; override;

    // Load the document from a file.
    Procedure LoadDocument;
    // Save the document to a file.
    Procedure SaveDocument;

    procedure Changed;

    // The particle effect for the document
    property Mesh: TPHXMesh read FMesh write FMesh;
    // The filename
    property FileName: String read FFileName write FFileName;
    // State of the doccument
    property State: TDocumentState read FState write FState;
  end;

TDocumentEvent = procedure(Document: TDocument) of object;

{$ENDREGION}

type

//------------------------------------------------------------------------------
TModActions = class(TDataModule)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OpenTextureDialog: TOpenDialog;
    SaveTextureDialog: TSaveDialog;
    ActionList1: TActionList;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    ImportDialog: TOpenDialog;
    actToolTextureLoad: TAction;
    actEditCenter: TAction;
    actEditScale: TAction;
    actFileImport: TAction;
    actEditRotate: TAction;
    actToolsTextureList: TAction;
    actShowNormals: TAction;
    actShowWireframe: TAction;
    actShowGrid: TAction;
    actShowBounds: TAction;
    ActionImages: TImageList;
    actToolTextureMapper: TAction;
    actViewPerspective: TAction;
    actViewOrthoTop: TAction;
    actViewOrthoY: TAction;
    actViewOrthoZ: TAction;
    actViewOrthoBottom: TAction;
    actToolXmlExport: TAction;
    actToolXmlImport: TAction;
    OpenXmlDialog: TOpenDialog;
    SaveXmlDialog: TSaveDialog;
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);

    procedure actImportUpdate(Sender: TObject);
    procedure actFileImportExecute(Sender: TObject);

    procedure actToolTextureLoadExecute(Sender: TObject);

    procedure actEditCenterExecute(Sender: TObject);
    procedure actEditUpdate(Sender: TObject);
    procedure actEditScaleExecute(Sender: TObject);

    procedure actEditRotateExecute(Sender: TObject);
    procedure actToolsTextureListExecute(Sender: TObject);
    procedure actShowNormalsExecute(Sender: TObject);
    procedure actShowWireframeExecute(Sender: TObject);
    procedure actShowGridExecute(Sender: TObject);
    procedure actShowBoundsExecute(Sender: TObject);
    procedure actToolTextureMapperExecute(Sender: TObject);
    procedure actToolUpdate(Sender: TObject);
    procedure actViewPerspectiveExecute(Sender: TObject);
    procedure actViewOrthoTopExecute(Sender: TObject);
    procedure actViewOrthoYExecute(Sender: TObject);
    procedure actViewOrthoZExecute(Sender: TObject);
    procedure actViewModeUpdate(Sender: TObject);
    procedure actViewOrthoBottomExecute(Sender: TObject);
  private
    FDevice  : TPHXDevice;
    FSettings: TSettings;
    FDocument: TDocument;
    FTextures: TObjectDictionary<String, TPHXTexture>;

    FOnDocumentChange: TDocumentEvent;

    procedure menuImportClick(Sender: TObject);
    procedure RecentClicked(Sender: TObject; const Filename: String);

    procedure LoadTexture(const Name: String);

    procedure SetSelected(const Value: TDocument);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSettings;
    procedure SaveSettings;

    // Load the textures for the current mesh
    procedure LoadModelTextures;

    function FindTexture(const Name: String): TPHXTexture;

    procedure CreateImportMenu(Menu: TMenuItem);


    // Create a new document
    function New: TDocument;
    // Open a document
    function Open(const FileName: String): TDocument;
    function Import(const FileName: String): TDocument;

   // function Save(Document: TDocument): TDocument;
   // function SaveAs(Document: TDocument): TDocument;

    property Device: TPHXDevice read FDevice write FDevice;
    // List of recent documents
    property Settings: TSettings read FSettings;
    // List of textures
    property Textures: TObjectDictionary<String, TPHXTexture> read FTextures;

    // Selected document
    property Document : TDocument read FDocument write SetSelected;
    // Event for when the document is changed
    property OnDocumentChange: TDocumentEvent read FOnDocumentChange write FOnDocumentChange;
  end;

var
  ModActions: TModActions;

implementation

uses uTextures,

     uImport,
     uImportMS3D,
     uImport3DS,
     uImportOBJ,
     uImportMesh,

     uDialogScale,
     uDialogRotate,
     uDialogCenter, uTextureMapper;

{$R *.dfm}

resourcestring
   SRenameText = 'The mesh name "%s" doesnt match the filename.'#13'Do you want to rename the mesh to "%s" before saving?';


//------------------------------------------------------------------------------
function GetUserFromWindows: string;
var UserName    : String;
var UserNameLen : Cardinal;
Begin
  UserNameLen := 255;
  Result := 'Unknown';
  {
  SetLength(userName, UserNameLen) ;

  If GetUserName(PChar(UserName), UserNameLen) then
  begin
    Result := Copy(UserName,1,UserNameLen - 1)
  end else
  begin
     Result := 'Unknown';
  end; }
end;


{$REGION 'TPHXMeshEx'}

// TPHXMeshEx
//------------------------------------------------------------------------------
function TPHXMeshEx.MeasureBounds: TBoxf;
var Index : Integer;
begin
  if Vertices.Count = 0 then
  begin
    Result.MinX:= 0;
    Result.MaxX:= 0;

    Result.MinY:= 0;
    Result.MaxY:= 0;

    Result.MinZ:= 0;
    Result.MaxZ:= 0;
  end else
  begin
    Result.MinX:= Vertices.List^[0].Position.X;
    Result.MaxX:= Vertices.List^[0].Position.X;

    Result.MinY:= Vertices.List^[0].Position.Y;
    Result.MaxY:= Vertices.List^[0].Position.Y;

    Result.MinZ:= Vertices.List^[0].Position.Z;
    Result.MaxZ:= Vertices.List^[0].Position.Z;

    for Index := 1 to Vertices.Count - 1 do
    begin
      if Vertices.List^[Index].Position.X < Result.MinX then Result.MinX:=Vertices.List^[Index].Position.X;
      if Vertices.List^[Index].Position.Y < Result.MinY then Result.MinY:=Vertices.List^[Index].Position.Y;
      if Vertices.List^[Index].Position.Z < Result.MinZ then Result.MinZ:=Vertices.List^[Index].Position.Z;

      if Vertices.List^[Index].Position.X > Result.MaxX then Result.MaxX:=Vertices.List^[Index].Position.X;
      if Vertices.List^[Index].Position.Y > Result.MaxY then Result.MaxY:=Vertices.List^[Index].Position.Y;
      if Vertices.List^[Index].Position.Z > Result.MaxZ then Result.MaxZ:=Vertices.List^[Index].Position.Z;
    end;
  end;
end;


//------------------------------------------------------------------------------
function TPHXMeshEx.MeasureCenter: TVector3f;
var Bounds: TBoxf;
begin
  Bounds:= MeasureBounds;

  Result.X:= (Bounds.MinX + Bounds.MaxX) / 2;
  Result.Y:= (Bounds.MinY + Bounds.MaxY) / 2;
  Result.Z:= (Bounds.MinZ + Bounds.MaxZ) / 2;
end;

//------------------------------------------------------------------------------
function TPHXMeshEx.MeasureSize: TVector3f;
var Bounds: TBoxf;
begin
  Bounds:= MeasureBounds;

  Result.X:= (Bounds.MaxX - Bounds.MinX);
  Result.Y:= (Bounds.MaxY - Bounds.MinY);
  Result.Z:= (Bounds.MaxZ - Bounds.MinZ);
end;

{$ENDREGION}


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
end;

//------------------------------------------------------------------------------
procedure TRecent.SaveToXML(Node: IXMLNode);
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

  FShowNormals  := False;
  FShowWireframe:= False;
  FShowGrid     := True;
  FShowBounds   := False;

  FNormalLength:= 1.0;
  FNormalColor := clYellow;
end;

//------------------------------------------------------------------------------
destructor TSettings.Destroy;
begin
  FRecent.Free;

  inherited;
end;

//------------------------------------------------------------------------------
procedure TSettings.LoadFromFile(const FileName: String);
//var XMLDocument    : IXMLDocument;
//var XMLRoot     : IXMLNode;
//var XMLNode    : IXMLNode;
begin
 { XMLDocument:= LoadXMLDocument(FileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxModelEditor') then Exit;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Recent');
  if Assigned(XMLNode) then
  begin
    Recent.LoadFromXML(XMLNode);
  end;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Settings');
  if Assigned(XMLNode) then
  begin
    FNormalColor := Graphics.StringToColor(XMLNode.Attributes['NormalColor']);
    FNormalLength:= Graphics.StringToColor(XMLNode.Attributes['NormalLength']);
  end;}
end;

//------------------------------------------------------------------------------
procedure TSettings.SaveToFile(const FileName: String);
//var XMLDocument : IXMLDocument;
//var XMLRoot     : IXMLNode;
//var XMLNode   : IXMLNode;
begin
  {
  XMLDocument:= NewXMLDocument();

  XMLRoot:= XMLDocument.AddChild('phxModelEditor');

  XMLNode:= XMLRoot.AddChild('Recent');
  begin
    Recent.SaveToXML(XMLNode);
  end;

  XMLNode:= XMLRoot.AddChild('Settings');
  begin
    XMLNode.Attributes['NormalColor' ]:= Graphics.ColorToString(FNormalColor);
    XMLNode.Attributes['NormalLength']:=                        FNormalLength;
  end;

  XMLDocument.SaveToFile(FileName);   }
end;



{$ENDREGION}

{$REGION 'TDocument'}

// TDocument
//------------------------------------------------------------------------------
constructor TDocument.Create;
begin
  FMesh:= TPHXMesh.Create;
end;


//------------------------------------------------------------------------------
destructor TDocument.Destroy;
begin
  FMesh.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDocument.LoadDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(FileName);

  if SameText(FileExt, '.xml') then
  begin
   // FFont.LoadFromXml(FileName);
  end else
  begin
    FMesh.LoadFromFile(FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(FileName);

  if SameText(FileExt, '.xml') then
  begin
   // FFont.SaveToXml(FileName);
  end else
  begin
    FMesh.SaveToFile(FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.Changed;
begin
  Include(FState, dsChanged);

  ModActions.SetSelected(Self);
end;


{$ENDREGION}

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

  FSettings:= TSettings.Create;
  FSettings.Recent.OnClick:= RecentClicked;

  FTextures:= TObjectDictionary<String, TPHXTexture>.Create;

  LoadSettings;

  actShowGrid.Checked:= Settings.ShowGrid;
  actShowBounds.Checked:= Settings.ShowBounds;
  actShowNormals.Checked:= Settings.ShowNormals;
  actShowWireframe.Checked:= Settings.ShowWireframe;
end;

//------------------------------------------------------------------------------
destructor TModActions.Destroy;
begin
  SaveSettings;

  FSettings.Free;
  FTextures.Free;
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

//------------------------------------------------------------------------------
procedure TModActions.SetSelected(const Value: TDocument);
begin
  if FDocument <> Value then
  begin

    if Assigned(FDocument) then
    begin
      FDocument.Free;
    end;

    FDocument := Value;

    if Assigned(FDocument) then
    begin
      LoadModelTextures
    end;
  end;

  if Assigned(OnDocumentChange) then OnDocumentChange(FDocument);
end;
// Load the textures for the current mesh

//------------------------------------------------------------------------------
procedure TModActions.LoadModelTextures;
var Index: Integer;
var Name : String;
begin
  for Index := 0 to Document.Mesh.Material.Textures.Count-1 do
  begin
    Name:= String(Document.Mesh.Material.Textures[Index].Name);

    LoadTexture(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.LoadTexture(const Name: String);
var Texture  : TPHXTexture;
begin
  // Empty texture
  if Name = '' then Exit;

  // The texture is already loaded
  if Textures.ContainsKey( LowerCase(Name) ) then Exit;

  if FileExists(ExtractFilePath(Document.FileName) + Name) then
  begin
    Texture:= Device.CreateTexture;
    Texture.LoadTexture(ExtractFilePath(Document.FileName) + Name);

    Textures.Add( LowerCase(Name), Texture);
  end else
  if FileExists( ExtractFilePath(Application.ExeName) + Name) then
  begin
    Texture:= Device.CreateTexture;
    Texture.LoadTexture( ExtractFilePath(Application.ExeName) + Name);

    Textures.Add( LowerCase(Name), Texture);
  end else
  if FileExists( GetCurrentDir + Name) then
  begin
    Texture:= Device.CreateTexture;
    Texture.LoadTexture( GetCurrentDir + Name);

    Textures.Add( LowerCase(Name), Texture);
  end;
end;

//------------------------------------------------------------------------------
function TModActions.FindTexture(const Name: String): TPHXTexture;
begin
  Result:= nil;

  Textures.TryGetValue( LowerCase(Name), Result);
end;



{$REGION 'Document'}


//------------------------------------------------------------------------------
procedure TModActions.RecentClicked(Sender: TObject; const Filename: String);
begin
  Open(Filename);
end;

var DocumentCounter : Integer;

//------------------------------------------------------------------------------
function TModActions.New: TDocument;
var Name: String;
begin
  Name:= Format('Mesh%d', [DocumentCounter] );

  Result:= TDocument.Create;
  Result.Filename:= Result.Mesh.Name + PHXMESH_EXT;
  Result.State   := [dsNew];

  Result.Mesh.Name   := Name;
  Result.Mesh.Author := GetUserFromWindows;
  Result.Mesh.Version:= DateToStr(Now);
  Result.Mesh.Comment:= '';

  SetSelected(Result);

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
      Result.Filename:= FileName;
      Result.State   := [];

      Result.LoadDocument;

      Settings.Recent.Add(Filename);

      SetSelected(Result);
    finally
      Screen.Cursor:= crDefault;
    end;
  end;

end;

//------------------------------------------------------------------------------
function TModActions.Import(const FileName: String): TDocument;
var Importer: TImporter;
begin
  Importer:= TImporterFactory.CreateImporterForFile(FileName);

  if Assigned(Importer) then
  begin
    Screen.Cursor:= crHourGlass;
    try
      Result:= TDocument.Create;
      Result.Filename:= ChangeFileExt(FileName, PHXMESH_EXT);
      Result.State   := [dsNew];

      Result.Mesh.Name   := ChangeFileExt(ExtractFileName(FileName), '');
      Result.Mesh.Author := GetUserFromWindows;
      Result.Mesh.Version:= DateToStr(Now);
      Result.Mesh.Comment:= 'Imported from ' + ExtractFileName(FileName);
      //
      Importer.Import(Result.Mesh, FileName);
      Importer.Free;

      Result.Mesh.Bounds:= Result.Mesh.CalculateBounds;

      SetSelected(Result);
    finally
      Screen.Cursor:= crDefault;
    end;
  end else
  begin
    Result:= nil;

    MessageDlg( Format('No importer found for %s.', [FileName]), mtError, [mbOK], 0);
  end;
end;


{$ENDREGION}


{$REGION 'File Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actFileUpdate(Sender: TObject);
begin
  actFileSave  .Enabled:= Assigned(Document);
  actFileSaveAs.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileNewExecute(Sender: TObject);
begin
  New;
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


    Settings.Recent.Add(Document.Filename);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveAsExecute(Sender: TObject);
var Name: String;
begin
  SaveDialog.FileName:= Document.FileName;

  if SaveDialog.Execute then
  begin
    Name:= ChangeFileExt( ExtractFileName( SaveDialog.FileName), '');

    if not SameText(Name, Document.Mesh.Name) and (MessageDlg( Format(SRenameText, [Document.Mesh.Name, Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      Document.Mesh.Name:= Name;
    end;
    Document.Filename:= SaveDialog.FileName;
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

//------------------------------------------------------------------------------
procedure TModActions.actToolTextureLoadExecute(Sender: TObject);
var Texture: TPHXTexture;
var Name   : String;
begin
  if OpenTextureDialog.Execute then
  begin
    Name:= ExtractFileName(OpenTextureDialog.FileName);

    // The texture is already loaded
    if Textures.ContainsKey( LowerCase(Name) ) then Exit;

    Texture:= Device.CreateTexture;
    Texture.LoadTexture(OpenTextureDialog.FileName);

     Textures.Add( LowerCase(Name), Texture);
  end;
end;
{$ENDREGION}


{$REGION 'Edit Actions'}


//------------------------------------------------------------------------------
procedure TModActions.actEditUpdate(Sender: TObject);
begin
  actEditCenter  .Enabled:= Assigned(Document);
  actEditScale   .Enabled:= Assigned(Document);
  actEditRotate  .Enabled:= Assigned(Document);
end;


// Center the model around the origin
//------------------------------------------------------------------------------
procedure TModActions.actEditCenterExecute(Sender: TObject);
var Dialog: TFrmDialogCenter;
var Mesh : TPHXMesh;
var Index: Integer;
var Bounds: TBoxf;
var Center: TVector3f;
begin
  Mesh:= Document.Mesh;

  Dialog:= TFrmDialogCenter.Create(Application);
  try
    if Dialog.ShowModal = mrOk then
    begin
      Bounds:= Mesh.CalculateBounds;

      if Dialog.CenterX then
      begin
        Center.X:= (Bounds.MaxX + Bounds.MinX) / 2;

        for Index := 0 to Mesh.Vertices.Count - 1 do
        begin
          Mesh.Vertices.List^[Index].Position.X:= Mesh.Vertices.List^[Index].Position.X - Center.X;
        end;

        for Index := 0 to Mesh.Joints.Count - 1 do
        begin
          Mesh.Joints.List^[Index].Position.X:= Mesh.Joints.List^[Index].Position.X - Center.X;
        end;

        for Index := 0 to Mesh.Tags.Count - 1 do
        begin
          Mesh.Tags.List^[Index].Position.X:= Mesh.Tags.List^[Index].Position.X - Center.X;
        end;
      end;

      if Dialog.CenterY then
      begin
        Center.Y:= (Bounds.MaxY + Bounds.MinY) / 2;

        for Index := 0 to Mesh.Vertices.Count - 1 do
        begin
          Mesh.Vertices.List^[Index].Position.Y:= Mesh.Vertices.List^[Index].Position.Y - Center.Y;
        end;

        for Index := 0 to Mesh.Joints.Count - 1 do
        begin
          Mesh.Joints.List^[Index].Position.Y:= Mesh.Joints.List^[Index].Position.Y - Center.Y;
        end;

        for Index := 0 to Mesh.Tags.Count - 1 do
        begin
          Mesh.Tags.List^[Index].Position.Y:= Mesh.Tags.List^[Index].Position.Y - Center.Y;
        end;

      end;

      if Dialog.CenterZ then
      begin
        Center.Z:= (Bounds.MaxZ + Bounds.MinZ) / 2;

        for Index := 0 to Mesh.Vertices.Count - 1 do
        begin
          Mesh.Vertices.List^[Index].Position.Z:= Mesh.Vertices.List^[Index].Position.Z - Center.Z;
        end;

        for Index := 0 to Mesh.Joints.Count - 1 do
        begin
          Mesh.Joints.List^[Index].Position.Z:= Mesh.Joints.List^[Index].Position.Z - Center.Z;
        end;

        for Index := 0 to Mesh.Tags.Count - 1 do
        begin
          Mesh.Tags.List^[Index].Position.Z:= Mesh.Tags.List^[Index].Position.Z - Center.Z;
        end;
      end;

      // Recalculate the bounding box
      Mesh.Bounds:= Mesh.CalculateBounds;

      Document.State:= Document.State + [dsChanged];
    end;
  finally
    Dialog.Free;
  end;

  SetSelected(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actEditScaleExecute(Sender: TObject);
var Dialog: TFrmDialogScale;
var Mesh : TPHXMesh;
var Index: Integer;
var Scale: TVector3f;
begin
  Mesh:= Document.Mesh;

  Dialog:= TFrmDialogScale.Create(Application);
  try
    if Dialog.ShowModal = mrOk then
    begin
      Scale:= Dialog.Scale;

      // Todo re
      for Index := 0 to Mesh.Vertices.Count - 1 do
      begin
        Mesh.Vertices.List^[Index].Position.X:= Scale.X * Mesh.Vertices.List^[Index].Position.X;
        Mesh.Vertices.List^[Index].Position.Y:= Scale.Y * Mesh.Vertices.List^[Index].Position.Y;
        Mesh.Vertices.List^[Index].Position.Z:= Scale.Z * Mesh.Vertices.List^[Index].Position.Z;
      end;
      // Recalculate the bounding box
      Mesh.Bounds:= Mesh.CalculateBounds;

   end;

  finally
    Dialog.Free;
  end;

  Document.State:= Document.State + [dsChanged];

  SetSelected(Document);
end;



//------------------------------------------------------------------------------
procedure TModActions.actEditRotateExecute(Sender: TObject);
var Dialog: TFrmDialogRotate;
var Mesh : TPHXMesh;
var Rotation: TVector3f;
var Matrix  : TMatrix4f;
begin
  Mesh:= Document.Mesh;

  Dialog:= TFrmDialogRotate.Create(Application);
  try
    if Dialog.ShowModal = mrOk then
    begin
      Rotation:= Dialog.Rotation;

      Matrix:= Matrix_CreateRotation(Rotation);

      Mesh.Transform(Matrix);

      // Recalculate the bounding box
      Mesh.Bounds:= Mesh.CalculateBounds;

      Document.State:= Document.State + [dsChanged];
    end;
  finally
    Dialog.Free;
  end;
  SetSelected(Document);
end;

{$ENDREGION}

{$REGION 'Tool Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actToolUpdate(Sender: TObject);
begin
  actToolTextureMapper.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolTextureMapperExecute(Sender: TObject);
var Dialog: TFrmTextureMapper;
begin
  Dialog:= TFrmTextureMapper.Create(Application);
  try
    Dialog.Mesh:= Document.Mesh;

    if Dialog.Execute then
    begin

    end;

  finally
    Dialog.Free;
  end;

end;



{$ENDREGION}


{$REGION 'View Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actToolsTextureListExecute(Sender: TObject);
begin
  FrmTextures.Show;
  //
end;

//------------------------------------------------------------------------------
procedure TModActions.actShowBoundsExecute(Sender: TObject);
begin
  Settings.ShowBounds:= not Settings.ShowBounds;

  actShowBounds.Checked:= Settings.ShowBounds;
end;

//------------------------------------------------------------------------------
procedure TModActions.actShowGridExecute(Sender: TObject);
begin
  Settings.ShowGrid:= not Settings.ShowGrid;

  actShowGrid.Checked:= Settings.ShowGrid;
end;

//------------------------------------------------------------------------------
procedure TModActions.actShowNormalsExecute(Sender: TObject);
begin
  Settings.ShowNormals:= not Settings.ShowNormals;

  actShowNormals.Checked:= Settings.ShowNormals;
end;

//------------------------------------------------------------------------------
procedure TModActions.actShowWireframeExecute(Sender: TObject);
begin
  Settings.ShowWireframe:= not Settings.ShowWireframe;

  actShowWireframe.Checked:= Settings.ShowWireframe;
end;


//------------------------------------------------------------------------------
procedure TModActions.actViewModeUpdate(Sender: TObject);
begin

  actViewPerspective .Checked:= (Settings.ViewMode = TViewMode.vmPerspective);
  actViewOrthoTop    .Checked:= (Settings.ViewMode = TViewMode.vmOrthoTop);
  actViewOrthoBottom .Checked:= (Settings.ViewMode = TViewMode.vmOrthoBottom);

  actViewOrthoY     .Checked:= (Settings.ViewMode = TViewMode.vmOrthoY);
  actViewOrthoZ     .Checked:= (Settings.ViewMode = TViewMode.vmOrthoZ);
end;

//------------------------------------------------------------------------------
procedure TModActions.actViewPerspectiveExecute(Sender: TObject);
begin
  Settings.ViewMode:= TViewMode.vmPerspective;
end;


//------------------------------------------------------------------------------
procedure TModActions.actViewOrthoTopExecute(Sender: TObject);
begin
  Settings.ViewMode:= TViewMode.vmOrthoTop;
end;

//------------------------------------------------------------------------------
procedure TModActions.actViewOrthoBottomExecute(Sender: TObject);
begin
  Settings.ViewMode:= TViewMode.vmOrthoBottom;
end;

//------------------------------------------------------------------------------
procedure TModActions.actViewOrthoYExecute(Sender: TObject);
begin
  Settings.ViewMode:= TViewMode.vmOrthoY;
end;

//------------------------------------------------------------------------------
procedure TModActions.actViewOrthoZExecute(Sender: TObject);
begin
  Settings.ViewMode:= TViewMode.vmOrthoZ;
end;



{$ENDREGION}


{$REGION 'Import Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actImportUpdate(Sender: TObject);
begin
 // actImportMilkshape3D.Enabled:= Assigned(Selected);
end;


//------------------------------------------------------------------------------
procedure TModActions.CreateImportMenu(Menu: TMenuItem);
var Item    : TMenuItem;
var Index   : Integer;
var Importer: TImporterItem;
begin
  Item:= TMenuItem.Create(Menu.Owner);
  Item.Action:= actFileImport;
  Menu.Add(Item);

  Item:= TMenuItem.Create(Menu.Owner);
  Item.Caption:= '-';
  Menu.Add(Item);

  for Index := 0 to TImporterFactory.Items.Count - 1 do
  begin
    Importer:=  TImporterFactory.Items[Index];

    Item:= TMenuItem.Create(Menu.Owner);
    Item.Caption:= Format('%s (*%s)', [Importer.Name, Importer.Ext]);
    Item.Tag    := Index;
    Item.OnClick:= menuImportClick;

    Menu.Add(Item);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileImportExecute(Sender: TObject);
begin
  ImportDialog.DefaultExt:= '';
  ImportDialog.Filter    := 'All supported (*.ms3d;*.3ds;*.obj;*.mesh)|*.ms3d;*.3ds;*.obj;*.mesh';

  if ImportDialog.Execute then
  begin
    Import(ImportDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.menuImportClick(Sender: TObject);
var Index   : Integer;
var Importer: TImporterItem;
begin
  Index:= TMenuItem(Sender).Tag;

  Importer:= TImporterFactory.Items[Index];

  ImportDialog.DefaultExt:= Importer.Ext;
  ImportDialog.Filter    := Format('%s (*%s)|*%s|All files (*.*)|*.*', [Importer.Name, Importer.Ext, Importer.Ext]);

  if ImportDialog.Execute then
  begin
    Import(ImportDialog.FileName);
  end;
end;


{$ENDREGION}


end.

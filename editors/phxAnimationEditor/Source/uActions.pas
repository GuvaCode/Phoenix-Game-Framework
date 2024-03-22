unit uActions;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, ImgList, Controls, ActnList, Menus,
  Dialogs, Forms,
  Laz2_DOM, Laz2_XMLRead, Laz2_XMLWrite,

  //xmldom, XMLIntf, msxmldom, XMLDoc,

  Generics.Collections,

  phxTypes,
  phxTexture,
  phxImage,
  //phxImageEx,

  phxGraphics,
  phxDevice;

type


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

    procedure LoadFromXML(Node : TDOMNode);
    procedure SaveToXML(Node : TDOMNode);

    procedure Add(const FileName: String);


    property Items: TStrings read FItems write FItems;
    property Menu: TMenuItem read FMenu write SetMenu;
    property OnClick: TRecentEvent read FOnClick write FOnClick;
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
    FName     : String;
    FState    : TDocumentState;
    FAnimation: TPHXAnimation;
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
    property Animation: TPHXAnimation read FAnimation;
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
    ListImages: TImageList;
    OpenImageDialog: TOpenDialog;
    OpenDocumentDialog: TOpenDialog;
    SaveDocumentDialog: TSaveDialog;
    actLoadImage: TAction;
    actExportXML: TAction;
    actImportXML: TAction;
    OpenXMLDialog: TOpenDialog;
    SaveXMLDialog: TSaveDialog;
    actToolDuration: TAction;
    actToolImageEditor: TAction;
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actLoadImageExecute(Sender: TObject);
    procedure actExportUpdate(Sender: TObject);
    procedure actExportXMLExecute(Sender: TObject);
    procedure actImportXMLExecute(Sender: TObject);
    procedure actToolDurationExecute(Sender: TObject);
    procedure actToolUpdate(Sender: TObject);
    procedure actToolImageEditorExecute(Sender: TObject);
  private
    FSettings: TSettings;

    FDocument       : TDocument;
    FDocumentChanged: TList<TDocumentEvent>;

    FImages       : TObjectList<TPHXImage>;

    procedure CheckFrames(Animation: TPHXAnimation);

    procedure RecentClicked(Sender: TObject; const Filename: String);

    procedure SetDocument(const Value: TDocument);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Create a new document
    function New: TDocument;
    // Open a existing document
    function Open(const FileName: String): TDocument;
    // Close the current document
    function Close(SaveQuery: Boolean = False): Integer;


    function FindImage(const Name: String): TPHXImage;

    // Editor settings
    property Settings: TSettings read FSettings;
    // The current document
    property Document: TDocument read FDocument write SetDocument;
    // Document listeners
    property DocumentChanged: TList<TDocumentEvent> read FDocumentChanged;

    // List of opened images
    property Images: TObjectList<TPHXImage> read FImages;
  end;

var
  ModActions: TModActions;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.lfm}

uses LCLIntf, LCLType;

resourcestring
  SRenameText = 'The animation name "%s" doesnt match the filename.'#13'Do you want to rename the animation to "%s" before saving?';
  SSaveText = 'Save changes to "%s"?';
  SImageMissing = 'The image "%s" is not loaded, do you want to open it?';
  SImageExisting = 'The image "%s" is already loaded.';


//------------------------------------------------------------------------------
function GetUser: string;
Begin
 Result := String(GetEnvironmentVariable('USER'));
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
procedure TRecent.LoadFromXML(Node: TDOMNode);
var Index: Integer;
begin
  Items.BeginUpdate;
  Items.Clear;
  for Index := 0 to Node.ChildNodes.Count - 1 do
  begin
    { #todo : Fix Me } //Items.Add( Node.ChildNodes[Index].Attributes['filename'] )
  end;
  Items.EndUpdate;

  UpdateMenu;
end;

//------------------------------------------------------------------------------
procedure TRecent.SaveToXML(Node: TDOMNode);
var Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
  begin
  { #todo : Fix Me } // Node.AddChild('item').Attributes['filename']:= Items[Index];
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
var XMLDocument    : TXMLDocument;
var XMLRoot     : TDOMNode;
var XMLNode    : TDOMNode;
begin
  { #todo : Fix Me }
  {
  if not FileExists(FileName) then Exit;
  XMLDocument:= LoadXMLDocument(FileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxAnimationEditor') then Exit;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Recent');
  if Assigned(XMLNode) then
  begin
    Recent.LoadFromXML(XMLNode);
  end;

  XMLNode:= XMLRoot.ChildNodes.FindNode('Settings');
  if Assigned(XMLNode) then
  begin
   // if XMLNode.HasAttribute('CenterPivots') then CenterPivots:= XMLNode.Attributes['CenterPivots'];
    //FNormalLength:= Graphics.StringToColor(XMLNode.Attributes['NormalLength']);
  end;
  }
end;

//------------------------------------------------------------------------------
procedure TSettings.SaveSettings;
var XMLDocument : TXMLDocument;
var XMLRoot     : TDOMNode;
var XMLNode   : TDOMNode;
begin
  { #todo : Fix Me }
 {
  XMLDocument:= NewXMLDocument();

  XMLRoot:= XMLDocument.AddChild('phxAnimationEditor');

  XMLNode:= XMLRoot.AddChild('Recent');
  begin
    Recent.SaveToXML(XMLNode);
  end;

  XMLNode:= XMLRoot.AddChild('Settings');
  begin
    //XMLNode.Attributes['CenterPivots' ]:= CenterPivots;
    //XMLNode.Attributes['NormalColor' ]:= Graphics.ColorToString(FNormalColor);
    //XMLNode.Attributes['NormalLength']:=                        FNormalLength;
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
  FAnimation:= TPHXAnimation.Create(nil);
end;


//------------------------------------------------------------------------------
destructor TDocument.Destroy;
begin
  FAnimation.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDocument.LoadDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
   { #todo : Fix ME }// Animation.LoadFromXml(Name);
  end else
  begin
    Animation.LoadFromFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    { #todo : Fix ME }//Animation.SaveToXml(Name);
  end else
  begin
    FAnimation.SaveToFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.Changed;
begin
  Include(FState, dsChanged);

  ModActions.SetDocument(Self);
end;


{$ENDREGION}



// TDataModule1

//------------------------------------------------------------------------------
constructor TModActions.Create(AOwner: TComponent);
begin
  inherited;

  FImages       := TObjectList<TPHXImage>.Create;

  FDocumentChanged:= TList<TDocumentEvent>.Create;

  FSettings:= TSettings.Create;
  FSettings.Recent.OnClick:= RecentClicked;
  FSettings.LoadSettings;
end;

//------------------------------------------------------------------------------
destructor TModActions.Destroy;
begin
  FSettings.SaveSettings;
  FSettings.Free;

  FImages.Free;

  FDocumentChanged.Free;

  inherited;
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

//------------------------------------------------------------------------------
procedure TModActions.CheckFrames(Animation: TPHXAnimation);
var Index  : Integer;
var Frame  : TPHXAnimationFrame;
var Pattern: Integer;
var Text   : String;
begin
  if Assigned(Animation.Image) then
  begin
    Text:= '';
    for Index := 0 to Animation.Frames.Count - 1 do
    begin
      Frame:= Animation.Frames.List^[Index];

      Pattern:= Animation.Image.Patterns.IndexOf( String(Frame.Name));
      if Pattern < 0 then
      begin
        Text:= Text + Format('The pattern %s was not found in the image.', [String(Frame.Name)]) + sLineBreak;

        Animation.Frames.List^[Index].Pattern:= -1;
      end else
      begin
        Animation.Frames.List^[Index].Pattern:= Pattern;
      end;
    end;

    if Text <> '' then
    begin
      MessageDlg(Text, mtError, [mbOK], 0);
    end;
  end;
end;



{$REGION 'Document Functions'}

//------------------------------------------------------------------------------
procedure TModActions.RecentClicked(Sender: TObject; const Filename: String);
begin
  Open(Filename);
end;

var DocumentCounter : Integer = 1;

//------------------------------------------------------------------------------
function TModActions.New: TDocument;
var Name: String;
begin
  Name:= Format('Animation%d', [DocumentCounter] );

  Result:= TDocument.Create;
  Result.Name := Name + PHXANIMATION_EXT;
  Result.State:= [dsNew];

  Result.Animation.Name       := Name;
  Result.Animation.Author     := GetUser;
  Result.Animation.Version    := DateToStr(Now);
  Result.Animation.Comment    := '';
  {
  if FImages.Count > 0 then
  begin
    Result.Animation.Image:= FImages.First;
    Result.Animation.Frames.Add('Frame 1', 0.5, 0);
    Result.Animation.Frames.Add('Frame 1', 0.5, 1);
    Result.Animation.Frames.Add('Frame 1', 0.5, 2);
  end;
  }
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

      Result.Animation.Image:= FindImage(Result.Animation.ImageName);

      if Result.Animation.Image = nil then
      begin
        if MessageDlg( Format(SImageMissing, [Result.Animation.ImageName]), mtConfirmation, [mbYes, mbOK], 0) = mrYes then
        begin
          actLoadImage.Execute;
          Result.Animation.Image:= FindImage(Result.Animation.ImageName);
       end;
      end;

      CheckFrames(Result.Animation);

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
      Result:= MessageDlg(Format(SSaveText, [Document.Name]), mtConfirmation,  mbYesNoCancel, 0);

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

{$ENDREGION}

//------------------------------------------------------------------------------
function TModActions.FindImage(const Name: String): TPHXImage;
var Image: TPHXImage;
begin
  for Image in ModActions.Images do
  begin
    if Image.Name = Name then
    begin
      Exit(Image);
    end;
  end;
  Result:= nil;
end;


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
  New;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileOpenExecute(Sender: TObject);
begin
  if OpenDocumentDialog.Execute then
  begin
    Open(OpenDocumentDialog.FileName);
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

    Settings.Recent.Add(Document.Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveAsExecute(Sender: TObject);
var Name: String;
begin
  SaveDocumentDialog.FileName:= Document.Name;

  if SaveDocumentDialog.Execute then
  begin
    Name:= ChangeFileExt( ExtractFileName( SaveDocumentDialog.FileName), '');

    if not SameText(Name, Document.Animation.Name) and (MessageDlg( Format(SRenameText, [Document.Animation.Name, Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      Document.Animation.Name:= Name;

      Document.Changed;
    end;
    Document.Name    := SaveDocumentDialog.FileName;
    Document.State   := [];
    Document.SaveDocument;

    Settings.Recent.Add(SaveDocumentDialog.Filename);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
procedure TModActions.actExportUpdate(Sender: TObject);
begin
  actExportXML.Enabled:= Assigned(Document);
  actImportXML.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actExportXMLExecute(Sender: TObject);
var Document: TXMLDocument;
var Node    : TDOMNode;
var Index   : Integer;
var Frame   : TPHXAnimationFrame;
begin
  { #todo : Fix Me }
  {
  SaveXMLDialog.FileName:= ChangeFileExt(ExtractFileName(FDocument.Name), '.xml');

  if SaveXMLDialog.Execute then
  begin
    Document:= NewXMLDocument;
    Document.Options:= Document.Options + [doNodeAutoIndent];
    Document.DocumentElement:= Document.AddChild('Animation');

    for Index := 0 to FDocument.Animation.Frames.Count - 1 do
    begin
      Frame:= FDocument.Animation.Frames[Index];

      Node:= Document.DocumentElement.AddChild('Frame');
      Node.Attributes['Name'   ]:=Frame.Name;
      Node.Attributes['Time'   ]:=Frame.Time;
      Node.Attributes['Pattern']:=Frame.Pattern;
    end;
    Document.SaveToFile(SaveXMLDialog.FileName);
  end;
  }
end;

//------------------------------------------------------------------------------
procedure TModActions.actImportXMLExecute(Sender: TObject);
var Document: TXMLDocument;
var Node    : TDOMNode;
var Index   : Integer;
var Frame   : TPHXAnimationFrame;
begin
  { #todo : Fix Me }
  {
  if OpenXMLDialog.Execute then
  begin
    Document:= LoadXMLDocument(OpenXMLDialog.FileName);

    if not SameText(Document.DocumentElement.NodeName, 'Animation') then
    begin
      raise Exception.Create('Not a valid animation export file');
    end;

    FDocument.Animation.Frames.Clear;
    for Index := 0 to Document.DocumentElement.ChildNodes.Count - 1 do
    begin
      Node:= Document.DocumentElement.ChildNodes[Index];

      if not SameText(Node.NodeName, 'Frame') then Continue;


      Frame.Name    := ShortString(Node.Attributes['Name']);
      Frame.Time    :=             Node.Attributes['Time'];
      Frame.Pattern :=             Node.Attributes['Pattern'];

      FDocument.Animation.Frames.Add(Frame);
    end;
    FDocument.Changed
  end;
  }
end;



{$REGION 'File actions '}

//------------------------------------------------------------------------------
procedure TModActions.actLoadImageExecute(Sender: TObject);
var Image: TPHXImage;
var Index: Integer;
begin
  if OpenImageDialog.Execute then
  begin
    for Index := 0 to OpenImageDialog.Files.Count - 1 do
    begin
      Image:= TPHXImage.CreateEx(TPHXTexture.Create);
      Image.LoadImage(OpenImageDialog.Files[Index]);

      if FindImage(Image.Name) <> nil then
      begin
        MessageDlg( Format(SImageExisting, [Image.Name]), mtError, [mbOK], 0);

        Image.Free;

        Continue;
      end;

      Images.Add(Image);
    end;

    if Assigned(FDocument) then
    begin
      FDocument.Animation.Image:= FindImage(FDocument.Animation.ImageName);
    end;

    SetDocument(FDocument);
  end;
end;

{$ENDREGION}


{$REGION 'Tool actions '}

//------------------------------------------------------------------------------
procedure TModActions.actToolUpdate(Sender: TObject);
begin
  actToolDuration.Enabled:= Assigned(Document);

  actToolImageEditor.Enabled:= FileExists(ExtractFilePath(Application.ExeName) + 'phxImageEditor.exe');
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolDurationExecute(Sender: TObject);
var Value   : String;
var Duration: Single;
var Index   : Integer;
var Frame   : TPHXAnimationFrame;
begin
  Value:= FloatToStr(FDocument.Animation.Duration);

  if InputQuery('Set animation duration', 'Duration (sec):', Value) then
  begin
    Duration:= StrToFloatDef(Value, FDocument.Animation.Duration);

    for Index := 0 to FDocument.Animation.Frames.Count - 1 do
    begin
      Frame:= FDocument.Animation.Frames[Index];
      Frame.Time:= Duration / FDocument.Animation.Frames.Count;

      FDocument.Animation.Frames[Index]:= Frame;
    end;

    FDocument.Animation.FrameRate:= Round(FDocument.Animation.Frames.Count / Duration);

    Document.Changed;
  end;
end;


//------------------------------------------------------------------------------
procedure TModActions.actToolImageEditorExecute(Sender: TObject);
var App: String;
begin  { #todo : Fix me }
  App:= ExtractFilePath(Application.ExeName) + 'phxImageEditor.exe';

   OpenDocument(PChar(App)); { *Преобразовано из ShellExecute* }
end;

{$ENDREGION}




end.

unit uActions;

interface

uses
  SysUtils, Classes, Dialogs, ImgList, Controls, Menus, Forms, ActnList, Windows,

  Generics.Collections,

  xmldom, XMLIntf, msxmldom, XMLDoc,


  phxFont,
  phxFontEx,
  phxDevice,
  phxGraphics,
  phxTexture;

const
  BuildDate = '2012-09-07';


type

//------------------------------------------------------------------------------
TDocumentState = set of (
  dsNew,
  dsChanged
);
TRecentEvent = procedure(Sender: TObject; const Filename: String) of object;

//------------------------------------------------------------------------------
TRecent = class
  private
    FItems: TStrings;
    FMenu: TMenuItem;

    procedure MenuUpdate;
    procedure MenuClicked(Sender: TObject);

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

    // List of recent documents
    property Recent: TRecent read FRecent;
  end;

//------------------------------------------------------------------------------
TDocument = class
  private
    FFont    : TPHXFont;
    FName: String;
    FState   : TDocumentState;
  public
    constructor Create; overload;
    destructor Destroy; override;

    // Load the document from a file.
    procedure LoadDocument;
    procedure SaveDocument;


    procedure Changed;


    // Load the document from a file.
    //Procedure LoadFromFile(const FileName: String);
    // Save the document to a file.
    //Procedure SaveToFile(const FileName: String);


    // The particle effect for the document
    property Font: TPHXFont read FFont write FFont;
    // The filename
    property  Name: String read FName write FName;
    // State of the doccument
    property State: TDocumentState read FState write FState;
  end;
TDocumentEvent = procedure(Document: TDocument) of object;

//------------------------------------------------------------------------------
TModActions = class(TDataModule)
    ToolBarImages: TImageList;
    ToolImages: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    OpenTextureDialog: TOpenDialog;
    SaveTextureDialog: TSaveDialog;
    ActionList1: TActionList;
    actFileOpen: TAction;
    actFileNew: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFileExit: TAction;
    actFileClose: TAction;
    actImportFontStudio4: TAction;
    OpenXMLDialog: TOpenDialog;
    SaveXMLDialog: TSaveDialog;
    actToolDropShadow: TAction;
    actImportBMFontXML: TAction;
    ImportDialog: TOpenDialog;
    actToolExportMask: TAction;
    actToolExportColors: TAction;
    actToolExportTexture: TAction;
    actToolImportTexture: TAction;
    actFileSaveXML: TAction;
    actFileLoadXML: TAction;
    ActionImages: TImageList;
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileUpdate(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actImportFontStudio4Execute(Sender: TObject);
    procedure actImportBMFontXMLExecute(Sender: TObject);
    procedure actToolUpdate(Sender: TObject);
    procedure actToolExportMaskExecute(Sender: TObject);
    procedure actToolExportColorsExecute(Sender: TObject);
    procedure actToolExportTextureExecute(Sender: TObject);
    procedure actToolImportTextureExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileSaveXMLExecute(Sender: TObject);
    procedure actFileLoadXMLExecute(Sender: TObject);
  private
    FSettings: TSettings;
    FOnSelectedChange: TDocumentEvent;
    FDocument: TDocument;

    procedure SetDocument(const Value: TDocument);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Create a new document
    function New: TDocument;
    // Open a document
    function Open(const FileName: String): TDocument;
    // Close the current document
    function Close(SaveQuery: Boolean = False): Integer;

    function Save: TDocument;
    function SaveAs: TDocument;


    // List of recent documents
    property Settings: TSettings read FSettings;
    // Selected document
    property Document : TDocument read FDocument write SetDocument;
    // Event for when the document is changed
    property OnSelectedChange: TDocumentEvent read FOnSelectedChange write FOnSelectedChange;
  end;

var
  ModActions: TModActions;

implementation

uses
  uGenerator.Dialog,

   uFont.New;

resourcestring
  SSaveQuery = 'Save changes to "%s"?';

  SRenameFont = 'The font name "%s" doesnt match the filename.'#13'Do you want to rename the font to "%s" before saving?';

{$R *.dfm}


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

  MenuUpdate;
end;

//------------------------------------------------------------------------------
procedure TRecent.MenuUpdate;
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
      ModActions.Open(FileName);
    end else
    begin
      Items.Delete(Index);

      MenuUpdate;

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

  MenuUpdate;
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

  MenuUpdate;
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
procedure TSettings.LoadSettings;
var XMLDocument    : IXMLDocument;
var XMLRoot     : IXMLNode;
var XMLRecent   : IXMLNode;
begin
  if not FileExists(GetFileName) then Exit;

  XMLDocument:= LoadXMLDocument(GetFileName);

  XMLRoot:= XMLDocument.DocumentElement;

  if not SameText(XMLRoot.NodeName, 'phxFontEditor') then Exit;

  XMLRecent:= XMLRoot.ChildNodes.FindNode('Recent');

  if Assigned(XMLRecent) then
  begin
    Recent.LoadFromXML(XMLRecent);
  end;
end;

//------------------------------------------------------------------------------
procedure TSettings.SaveSettings;
var XMLDocument : IXMLDocument;
var XMLRoot     : IXMLNode;
var XMLRecent   : IXMLNode;
begin

  XMLDocument:= NewXMLDocument();

  XMLRoot:= XMLDocument.AddChild('phxFontEditor');

  XMLRecent:= XMLRoot.AddChild('Recent');
  Recent.SaveToXML(XMLRecent);

  XMLDocument.SaveToFile(GetFileName);
end;

//------------------------------------------------------------------------------
function TSettings.GetFileName: String;
begin
  Result:= ChangeFileExt( ParamStr(0), '.cfg');
end;



{$ENDREGION}

{$REGION 'TDocument'}

// TDocument
//------------------------------------------------------------------------------
constructor TDocument.Create;
begin
  FFont:= TPHXFont.CreateEx(TPHXTexture.Create);

  FFont.Author:=GetUserFromWindows;
end;

//------------------------------------------------------------------------------
destructor TDocument.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TDocument.LoadDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FFont.LoadFromXml(Name);
  end else
  begin
    FFont.LoadFromFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveDocument;
var FileExt: String;
begin
  FileExt:= ExtractFileExt(Name);

  if SameText(FileExt, '.xml') then
  begin
    FFont.SaveToXml(Name);
  end else
  begin
    FFont.SaveToFile(Name);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.Changed;
begin
  Include(FState, dsChanged);

  ModActions.Document:= Self;
end;

{
//------------------------------------------------------------------------------
procedure TDocument.LoadFromFile(const FileName: String);
var FileExt: String;
begin
  FileExt:= ExtractFileExt(FileName);

  if SameText(FileExt, '.xml') then
  begin
    FFont.LoadFromXml(FileName);
  end else
  begin
    FFont.LoadFromFile(FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TDocument.SaveToFile(const FileName: String);
var FileExt: String;
begin
  FileExt:= ExtractFileExt(FileName);

  if SameText(FileExt, '.xml') then
  begin
    FFont.SaveToXml(FileName);
  end else
  begin
    FFont.SaveToFile(FileName);
  end;
end;
        }



{$ENDREGION}

// TModActions
//------------------------------------------------------------------------------
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
  FSettings.LoadSettings;
end;

//------------------------------------------------------------------------------
destructor TModActions.Destroy;
begin
  FSettings.SaveSettings;

  FSettings.Free;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TModActions.SetDocument(const Value: TDocument);
begin
  FDocument := Value;

  if Assigned(OnSelectedChange) then OnSelectedChange(FDocument);
end;

{$REGION 'Document'}

var DocumentCounter : Integer;

//------------------------------------------------------------------------------
function TModActions.New: TDocument;
//var Image: TPHXImage;
begin
  Result:= TDocument.Create;
  Result.Name    := Format('Font%d', [DocumentCounter] );
  Result.State   := [dsNew];

  SetDocument(Result);

  Inc(DocumentCounter);
end;

//------------------------------------------------------------------------------
function TModActions.Open(const FileName: String): TDocument;
begin
  Screen.Cursor:= crHourGlass;

  Result:= TDocument.Create;

  if FileExists(FileName) then
  begin
    Result.Name := FileName;
    Result.State:= [];

    Result.LoadDocument;

    Settings.Recent.Add(Filename);

    SetDocument(Result);
  end;

  Screen.Cursor:= crDefault;
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
function TModActions.Save: TDocument;
begin
  if dsNew in Document.State then
  begin
    SaveAs;
  end
  else
  begin
    Document.SaveDocument;

    Document.State:= [];

    Settings.Recent.Add(Document.Name);

    SetDocument(Document);
  end;

  Result:= Document;
end;


//------------------------------------------------------------------------------
function TModActions.SaveAs: TDocument;
var Name: String;
begin
  SaveDialog.FileName:= Document.Name;

  if SaveDialog.Execute then
  begin
    Name:= ChangeFileExt( ExtractFileName( SaveDialog.FileName), '');

    if not SameText(Name, Document.Font.Name) and (MessageDlg( Format(SRenameFont, [Document.Font.Name, Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      Document.Font.Name:= Name;
    end;
    Document.Name := SaveDialog.FileName;
    Document.State:= [];

    Document.SaveDocument;

    Settings.Recent.Add(SaveDialog.Filename);

    SetDocument(Document);
  end;

  Result:= Document;
end;

{$ENDREGION}

{$REGION 'File Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actFileUpdate(Sender: TObject);
begin
  actFileSave  .Enabled:= Assigned(Document);
  actFileSaveAs.Enabled:= Assigned(Document);
  actFileClose .Enabled:= Assigned(Document);

  actFileSaveXML.Enabled:= Assigned(Document);
  actFileLoadXML.Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileNewExecute(Sender: TObject);
var Dialog: TFrmFontNew;
var Document: TDocument;
begin
  if Close(True) = mrCancel then Exit;

  Dialog:= TFrmFontNew.Create(Application);
  try
    if not Dialog.Execute then Exit;

    // Create a empty font
    if Dialog.Mode = nmEmpty then
    begin
      Document:= TDocument.Create;
      Document.Name    := Format('Font%d', [DocumentCounter] );
      Document.State   := [dsNew];
      SetDocument(Document);

      Inc(DocumentCounter);
    end;
    // Create a font using the font generator
    if Dialog.Mode = nmGenerator then
    begin
      Document:= TDocument.Create;
      Document.Name    := 'Font';
      Document.State   := [dsNew];

      if FrmGenerator.Execute(Document.Font) then
      begin
        Document.Name:= Document.Font.Name + PHXFONT_EXT;
        SetDocument(Document);
      end else
      begin
        Document.Free;
      end;

    end;
  finally
    Dialog.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileOpenExecute(Sender: TObject);
begin
  if Close(True) = mrCancel then Exit;

  if OpenDialog.Execute then
  begin
    Open(OpenDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveAsExecute(Sender: TObject);
begin
  SaveAs;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveExecute(Sender: TObject);
begin
  Save;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileCloseExecute(Sender: TObject);
var ASelected: TDocument;
begin
  ASelected:= FDocument;

  SetDocument(nil);

  ASelected.Free;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileExitExecute(Sender: TObject);
begin
  Application.MainForm.Close;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileSaveXMLExecute(Sender: TObject);
begin
  if OpenXMLDialog.Execute then
  begin
    Document.Font.SaveToXml(OpenXMLDialog.FileName);
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actFileLoadXMLExecute(Sender: TObject);
begin
  if OpenXMLDialog.Execute then
  begin
    Document.Font.LoadFromXml(OpenXMLDialog.FileName);
  end;
end;

{$ENDREGION}


{$REGION 'Import Actions'}

(*
<font numchars="94" spacewidth="3">
  <characters>
    <chardata char="33" A="1" C="3" wid="9" hgt="19" X1="0" Y1="0,07421875" X2="0,03515625" Y2="0,1484375" />
    <chardata char="126" A="0" C="8" wid="14" hgt="19" X1="0" Y1="0" X2="0,0546875" Y2="0,07421875" />
  </characters>
</font>

*)
//------------------------------------------------------------------------------
procedure TModActions.actImportFontStudio4Execute(Sender: TObject);
var Document: TDocument;
var Index  : Integer;
var Font     : TPHXFont;
var Character : TPHXCharacter;

var XMLDocument  : IXMLDocument;
var XMLFont      : IXMLNode;
var XMLCharacters: IXMLNode;
var XMLCharacter : IXMLNode;

var char        : Cardinal;
var A,C,wid, hgt: Integer;
var X1,X2,Y1,Y2 : Single;
begin
  if OpenXMLDialog.Execute and OpenTextureDialog.Execute then
  begin
    XMLDocument:= LoadXMLDocument(OpenXMLDialog.FileName);

    XMLFont:= XMLDocument.DocumentElement;

    if not SameText(XMLFont.NodeName, 'font') then
    begin
      raise Exception.Create('Invalid Font Studio XML Font.');
    end;
    Document:= TDocument.Create;
    Document.State   := [dsNew];

    Font:= Document.Font;

    Font.Name:= ChangeFileExt(ExtractFileName(OpenXMLDialog.FileName), '');
    // Load the texture
    Font.Texture.LoadTexture( OpenTextureDialog.FileName);



    XMLCharacters:= XMLFont.ChildNodes.FindNode('characters');
    for Index := 0 to XMLCharacters.ChildNodes.Count - 1 do
    begin
      XMLCharacter:= XMLCharacters.ChildNodes[Index];
      (*
      char="33"
      A="1"
      C="3"
      wid="9"
      hgt="19"
      X1="0"
      Y1="0,07421875"
      X2="0,03515625"
      Y2="0,1484375"
      *)

      char:= XMLCharacter.Attributes['char'];
      A   := XMLCharacter.Attributes['A'  ];
      C   := XMLCharacter.Attributes['C'  ];
      wid := XMLCharacter.Attributes['wid'];
      hgt := XMLCharacter.Attributes['hgt'];
      X1  := XMLCharacter.Attributes['X1'];
      Y1  := XMLCharacter.Attributes['Y1'];
      X2  := XMLCharacter.Attributes['X2'];
      Y2  := XMLCharacter.Attributes['Y2'];

      Character.ID      := char;
      Character.X       := Round( X1     * Document.Font.Texture.Width);
      Character.Y       := Round( Y1     * Document.Font.Texture.Height);
      Character.Width   := Round((X2-X1) * Document.Font.Texture.Width);
      Character.Height  := Round((Y2-Y1) * Document.Font.Texture.Height);
      Character.Offset.X:= A;
      Character.Offset.Y:= 0;
      Character.Advance := C;//(wid - A - C);

      if hgt > Document.Font.Metric.Height then Document.Font.Metric.Height:= hgt;

      Document.Font.Characters.Add(Character);
    end;
    Document.Font.Initialize;

    Document.Name:= Document.Font.Name + PHXFONT_EXT;

    SetDocument(Document);
  end;
end;

(*

<?xml version="1.0"?>
<font>
  <info face="Tahoma" size="16" bold="0" italic="0" charset="" unicode="1" stretchH="100" smooth="1" aa="1" padding="0,0,0,0" spacing="1,1" outline="0"/>
  <common lineHeight="16" base="13" scaleW="256" scaleH="128" pages="1" packed="0" alphaChnl="1" redChnl="0" greenChnl="0" blueChnl="0"/>
  <pages>
    <page id="0" file="Tahoma16px.fnt.xml_0.png" />
  </pages>
  <chars count="331">
    <char id="32" x="43" y="102" width="1" height="1" xoffset="0" yoffset="13" xadvance="4" page="0" chnl="15" />
    <char id="33" x="227" y="80" width="1" height="9" xoffset="1" yoffset="4" xadvance="4" page="0" chnl="15" />
    <char id="511" x="18" y="40" width="8" height="11" xoffset="-1" yoffset="3" xadvance="7" page="0" chnl="15" />
  </chars>
  <kernings count="106">
    <kerning first="39" second="65" amount="-1" />
    <kerning first="39" second="198" amount="-1" />
    <kerning first="121" second="46" amount="-1" />
  </kernings>
</font>

  *)
// Import fonts from the AngelCode Bitmap Font Generator
//------------------------------------------------------------------------------
procedure TModActions.actImportBMFontXMLExecute(Sender: TObject);
var XMLDocument  : IXMLDocument;
var XMLInfo      : IXMLNode;
var XMLCommon    : IXMLNode;
var XMLPages     : IXMLNode;
var XMLCharacters: IXMLNode;
var XMLKernings  : IXMLNode;
var XMLNode      : IXMLNode;

var Index    : Integer;
var Path     : String;
var Font     : TPHXFont;
var Character: TPHXCharacter;
var Kerning  : TPHXKerning;
var Document : TDocument;
begin
  ImportDialog.DefaultExt:= '.fnt';
  ImportDialog.Filter    := 'Angelcode Bitmap Font (*.fnt)|*.fnt|All files (*.*)|*.*';

  if ImportDialog.Execute  then
  begin
    Path:= ExtractFilePath(ImportDialog.FileName);

    XMLDocument:= LoadXMLDocument(ImportDialog.FileName);

    XMLInfo      := XMLDocument.DocumentElement.ChildNodes.FindNode('info');
    XMLCommon    := XMLDocument.DocumentElement.ChildNodes.FindNode('common');
    XMLPages     := XMLDocument.DocumentElement.ChildNodes.FindNode('pages');
    XMLCharacters:= XMLDocument.DocumentElement.ChildNodes.FindNode('chars');
    XMLKernings  := XMLDocument.DocumentElement.ChildNodes.FindNode('kernings');

    // Check format
    if (XMLInfo = nil) or (XMLCommon = nil) or (XMLPages = nil) or (XMLCharacters = nil) or (XMLKernings = nil) then
    begin
      MessageDlg('Invalid Angelcode Bitmap Font XML Document.', mtError, [mbOK], 0);

      Exit;
    end;

    // We only supports single paged fonts
    if XMLCommon.Attributes['pages'] > 1 then
    begin
      MessageDlg('Phoenix only supports one paged fonts. Increase the texture size and try again', mtError, [mbOK], 0);

      Exit;
    end;
    Document:= TDocument.Create;
    Document.Name    := ChangeFileExt(ImportDialog.FileName, PHXFONT_EXT);
    Document.State   := [dsNew];



    Font:= Document.Font;

    //  <info
    //      face="Tahoma"
    //      size="16"
    //      bold="0"
    //      italic="0"
    //      charset=""
    //      unicode="1"
    //      stretchH="100"
    //      smooth="1"
    //      aa="1"
    //      padding="0,0,0,0"
    //      spacing="1,1"
    //      outline="0"/>
    Font.Name  := XMLInfo.Attributes['face'];
    Font.Size  := XMLInfo.Attributes['size'];
    Font.Style := [];
    if XMLInfo.Attributes['bold'] > 0 then Font.Style:= Font.Style + [fsBold];
    if XMLInfo.Attributes['italic'] > 0 then Font.Style:= Font.Style + [fsItalic];

    //  <common
    //      lineHeight="16"
    //      base="13"
    //      scaleW="256"
    //      scaleH="128"
    //      pages="1"
    //      packed="0"
    //      alphaChnl="1"
    //      redChnl="0"
    //      greenChnl="0"
    //      blueChnl="0"/>
    Font.Metric.Height := XMLCommon.Attributes['lineHeight'];
    Font.Metric.Ascent := XMLCommon.Attributes['base'];
    Font.Metric.Descent:= Font.Metric.Height - Font.Metric.Ascent;
    //  <pages>
    //    <page id="0" file="Tahoma16px.fnt.xml_0.png" />
    //  </pages>
    Font.Texture.LoadTexture(Path + XMLPages.ChildNodes[0].Attributes['file']);

    //  <chars count="331">
    //  <char
    //      id="32"
    //      x="43"
    //      y="102"
    //      width="1"
    //      height="1"
    //      xoffset="0"
    //      yoffset="13"
    //      xadvance="4"
    //      page="0"
    //      chnl="15"
    //  />
    for Index := 0 to XMLCharacters.ChildNodes.Count - 1 do
    begin
      XMLNode:= XMLCharacters.ChildNodes[Index];

      Character.ID      := XMLNode.Attributes['id'];
      Character.X       := XMLNode.Attributes['x'];
      Character.Y       := XMLNode.Attributes['y'];
      Character.Width   := XMLNode.Attributes['width'];
      Character.Height  := XMLNode.Attributes['height'];
      Character.Offset.X:= XMLNode.Attributes['xoffset'];
      Character.Offset.Y:= XMLNode.Attributes['yoffset'];
      Character.Advance := XMLNode.Attributes['xadvance'];

      Font.Characters.Add(Character);
    end;
    //  <kernings count="106">
    //    <kerning first="39" second="65" amount="-1" />
    //    <kerning first="39" second="198" amount="-1" />
    //    <kerning first="121" second="46" amount="-1" />
    //  </kernings>
    for Index := 0 to XMLKernings.ChildNodes.Count - 1 do
    begin
      XMLNode:= XMLKernings.ChildNodes[Index];

      Kerning.First  := XMLNode.Attributes['first'];
      Kerning.Second := XMLNode.Attributes['second'];
      Kerning.Amount := XMLNode.Attributes['amount'];

      Font.Kernings.Add(Kerning);
    end;
    Font.Initialize;

    Document.Name:= Font.Name + PHXFONT_EXT;

    SetDocument(Document);
  end;

end;

{$ENDREGION}



{$REGION 'Tool Actions'}

//------------------------------------------------------------------------------
procedure TModActions.actToolUpdate(Sender: TObject);
begin
  actToolExportTexture.Enabled:= Assigned(Document);
  actToolImportTexture.Enabled:= Assigned(Document);

  actToolExportColors .Enabled:= Assigned(Document);
  actToolExportMask   .Enabled:= Assigned(Document);
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolExportTextureExecute(Sender: TObject);
var GraphicFormat: TPHXGraphicFormat;
begin
  if SaveTextureDialog.Execute then
  begin
    if GraphicFormats.Find(SaveTextureDialog.FileName, GraphicFormat) then
    begin
      GraphicFormat.Filer.SaveToFile(SaveTextureDialog.FileName, Document.Font.Texture.Graphic);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolExportColorsExecute(Sender: TObject);
var Bitmap: TPHXBitmap;
begin
  if SaveTextureDialog.Execute then
  begin
    Bitmap:= TPHXBitmap.Create;
    Bitmap.Import(Document.Font.Texture.Graphic, pcRGB);

    Bitmap.SaveBitmap(SaveTextureDialog.FileName);
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolExportMaskExecute(Sender: TObject);
var Bitmap: TPHXBitmap;
begin
  if SaveTextureDialog.Execute then
  begin
    Bitmap:= TPHXBitmap.Create;
    Bitmap.Import(Document.Font.Texture.Graphic, pcAlpha);

    Bitmap.SaveBitmap(SaveTextureDialog.FileName);
    Bitmap.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolImportTextureExecute(Sender: TObject);
begin
  if OpenTextureDialog.Execute then
  begin
    Document.Font.Texture.LoadTexture(OpenTextureDialog.FileName);

    SetDocument(Document);
  end;
end;
         (*
//------------------------------------------------------------------------------
procedure TModActions.actToolsSetWrapStartExecute(Sender: TObject);
var Values   : String;
var Index    : Integer;
var Character: TPHXCharacter;
var Text     : String;
var Count    : Integer;
begin
  Values:= '';
  for Index := 0 to Document.Font.Characters.Count - 1 do
  begin
    Character:= Document.Font.Characters.List^[Index];

    Text:= Chr(Character.ID);

    if Character.AllowWrapStart then
    begin
      Values:= Values + Text;
    end;
  end;

  if InputQuery('Assign characters that cant start a wrapped line', 'Characters:', Values) then
  begin
    Count:= 0;
    for Index := 0 to Document.Font.Characters.Count - 1 do
    begin
      Character:= Document.Font.Characters.List^[Index];

      Text:= Chr(Character.ID);

      if Pos(Text, Values) > 0 then
      begin
        Inc(Count);

        Document.Font.Characters.List^[Index].AllowWrapStart:= True;
      end else
      begin
        Document.Font.Characters.List^[Index].AllowWrapStart:= False;
      end;
    end;
    MessageDlg(Format('%d characters marked', [Count]), mtInformation, [mbOK], 0);

    Document.Changed;
  end;
end;

//------------------------------------------------------------------------------
procedure TModActions.actToolsSetWrapEndExecute(Sender: TObject);
var Values: String;
var Index : Integer;
var Char  : TPHXCharacter;
var Text     : String;
var Count    : Integer;
begin

  Values:= '';
  for Index := 0 to Document.Font.Characters.Count - 1 do
  begin
    Char:= Document.Font.Characters.List^[Index];

    if Char.AllowWrapEnd then
    begin
      Values:= Values + Chr(Char.ID);
    end;
  end;

  if InputQuery('Assign characters that cant end a wrapped line', 'Characters:', Values) then
  begin
    Count:= 0;
    for Index := 0 to Document.Font.Characters.Count - 1 do
    begin
      Char:= Document.Font.Characters.List^[Index];

      Text:= Chr(Char.ID);

       if Pos(Text, Values) > 0 then
      begin
        Inc(Count);

        Document.Font.Characters.List^[Index].AllowWrapEnd:= True;
      end else
      begin
        Document.Font.Characters.List^[Index].AllowWrapEnd:= False;
      end;
      MessageDlg(Format('%d characters marked', [Count]), mtInformation, [mbOK], 0);
    end;
    Document.Changed;
  end;
end;

*)
{$ENDREGION}


end.

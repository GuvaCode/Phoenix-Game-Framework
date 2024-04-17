unit uImage.New.Packer;

{$mode Delphi}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ImgList, ExtCtrls, ToolWin, Menus,

 // Generics.Defaults,
  //Generics.Collections,
 // Generics.Helpers,

  Codebot.Collections,
  Codebot.System,
  Spin,

  phxDevice,
  phxGraphics,
  phxGraphicsEx,
  phxTypes, ActnList;

type

//------------------------------------------------------------------------------
TPHXPackerItem = class
  public
    // The name of the bitmap
    Name: String;
    Path: String;
    // Width of the item
    Width: Integer;
    // Height of the item
    Height: Integer;
    // The image
    Image: TPHXBitmap;
    // True if the bitmap was added to the target
    Placed: Boolean;
    // The position of the bitmap in the packed image
    Position: TVector2i;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
  end;
//------------------------------------------------------------------------------
{
TPHXPackerWidthComparer = class(TInterfacedObject, TCompare<TPHXPackerItem>)
public
  function Compare(const Left, Right: TPHXPackerItem): Integer;
end;
}

//------------------------------------------------------------------------------
TfrmPacker = class(TForm)
    ImageList1: TImageList;
    OpenImageDialog: TOpenDialog;
    ImageList2: TImageList;
    PopupMenu1: TPopupMenu;
    btnClear: TMenuItem;
    Insert1: TMenuItem;
    ActionList1: TActionList;
    actImageAdd: TAction;
    actImageRemove: TAction;
    actImageClear: TAction;
    N1: TMenuItem;
    actImageClear1: TMenuItem;
    PanelClient: TPanel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    PanelLeft: TPanel;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbWidth: TComboBox;
    cbHeight: TComboBox;
    edPadding: TSpinEdit;
    GroupBox1: TGroupBox;
    lwImages: TListView;
    ToolBar1: TToolBar;
    ButtonAdd: TToolButton;
    ButtonDel: TToolButton;
    btnOk: TButton;
    Button1: TButton;
    btnPack: TButton;
    Splitter1: TSplitter;
    actPackerPlace: TAction;
    procedure PaintBox1Paint(Sender: TObject);
    procedure lwImagesSelectItem(Sender: TObject; Item: TListItem;   Selected: Boolean);

    procedure actImageAddExecute(Sender: TObject);
    procedure actImageRemoveExecute(Sender: TObject);
    procedure actImageClearExecute(Sender: TObject);
    procedure actPackerPlaceExecute(Sender: TObject);
  private   { #todo : https://forum.lazarus.freepascal.org/index.php?topic=42920.0 }
     //procedure DropFiles(var msg : TWMDropFiles) ; message WM_DROPFILES;
  private
   FBuffer     : TBitmap;
    FTransparent: TBitmap;
    FItems      : TObjectList<TPHXPackerItem>;

    FImage: TPHXBitmap;

    procedure AddImage(const FileName: String);


    procedure DetermineSize(out AWidth: Integer; out AHeight: Integer);


    procedure lwImagesUpdate;
    procedure DrawPattern(Dest: TCanvas; Index: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean;

    // The current bitmap
    Property Image: TPHXBitmap read FImage;
     // List of packer items
    Property Items: TObjectList<TPHXPackerItem> read FItems;
  end;

var
  frmPacker: TfrmPacker;

implementation

{$R *.dfm}

uses Partitions;
          {
procedure QuickSort(SortList: PPackerItemList; L, R: Integer);
var
  I, J: Integer;
  P, T: TPHXPackerItem;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while (P.Width - SortList^[I].Width) < 0 do
        Inc(I);
      while (P.Width - SortList^[J].Width) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J);
    L := I;
  until I >= R;
end;}

// TPHXPackerItem
//==============================================================================
constructor TPHXPackerItem.Create(const FileName: String);
begin
  Image:= TPHXBitmap.Create;
  Image.LoadBitmap(FileName);

  Name      := ChangeFileExt(ExtractFileName(FileName), '');
  Path      := ExtractFilePath(FileName);
  Width     := Image.Width;
  Height    := Image.Height;
  Placed    := False;
  Position.X:= -1;
  Position.Y:= -1;
end;

//------------------------------------------------------------------------------
destructor TPHXPackerItem.Destroy;
begin
  Image.Free;
  inherited;
end;

// TPHXPackerWidthComparer
//==============================================================================
{function TPHXPackerWidthComparer.Compare(const Left, Right: TPHXPackerItem): Integer;
begin
  Result:= Right.Width - Left.Width;
end;
 }
// TfrmPacker
//==============================================================================
constructor TfrmPacker.Create(AOwner: TComponent);
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
  //form is ready to accept files

  //////
  {todo
  DragAcceptFiles( Handle, True ) ;
  }
  OpenImageDialog.Filter:= Filter;

  FItems      := TObjectList<TPHXPackerItem>.Create(True);
  FImage      := TPHXBitmap.Create;
  FBuffer     := TBitmap.Create;
  FTransparent:= CreateTransparentImage(4);

  ScrollBox1.DoubleBuffered:= True;

  btnOk.Enabled:= False;
end;
//------------------------------------------------------------------------------
destructor TfrmPacker.Destroy;
begin
  FTransparent.Free;
  FImage.Free;
  FBuffer.Free;
  FItems.Free;
  inherited;
end;


//------------------------------------------------------------------------------
function TfrmPacker.Execute: Boolean;
begin
  lwImagesUpdate;

  btnOk.Enabled:= False;

  Result:= ShowModal = mrOk;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.AddImage(const FileName: String);
var Item: TPHXPackerItem;
begin
  Item:= TPHXPackerItem.Create(FileName);

  Items.Add(Item);
end;


//------------------------------------------------------------------------------
procedure TfrmPacker.lwImagesUpdate;
var Index: Integer;
var ListItem: TListItem;
begin
  lwImages.Items.BeginUpdate;
  lwImages.Items.Clear;

  for Index := 0 to Items.Count - 1 do
  begin
    ListItem:= lwImages.Items.Add;
    ListItem.Caption:= String(Items[Index].Name);

    if Items[Index].Placed then
    begin
      ListItem.ImageIndex:= 0;

      ListItem.SubItems.Add(IntToStr(Items[Index].Position.X));
      ListItem.SubItems.Add(IntToStr(Items[Index].Position.Y));
    end else
    begin
      ListItem.ImageIndex:= -1;

      ListItem.SubItems.Add('');
      ListItem.SubItems.Add('');
    end;

  end;

  lwImages.Items.EndUpdate;
end;



{$REGION 'Drawing'}

//------------------------------------------------------------------------------
Procedure TfrmPacker.DrawPattern(Dest: TCanvas; Index: Cardinal);
var Pattern: TPHXPackerItem ;
var rPattern: TRect;
begin
  Pattern:=Items[Index];

  rPattern.Left  := (Pattern.Position.X                 );
  rPattern.Top   := (Pattern.Position.Y                 );
  rPattern.Right := (Pattern.Position.X + Pattern.Width );
  rPattern.Bottom:= (Pattern.Position.Y + Pattern.Height);

  with Dest do begin
     Brush.Style:= bsClear;

     Pen.Color  := clWhite;
     Pen.Width  := 1;
     Pen.Style  := psSolid;
     Rectangle(rPattern.Left, rPattern.Top, rPattern.Right, rPattern.Bottom);

     Pen.Color  := clBlack;
     Pen.Width  := 1;
     Pen.Style  := psDot;
     Rectangle(rPattern.Left, rPattern.Top, rPattern.Right, rPattern.Bottom);
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Width:= FBuffer.Width;
  PaintBox1.Height:= FBuffer.Height;

  PaintBox1.Canvas.Draw(0, 0, FBuffer);

  if btnOk.Enabled and (lwImages.SelCount > 0) then
  begin
    DrawPattern(PaintBox1.Canvas, lwImages.Selected.Index);
  end;
end;

{$ENDREGION}

//------------------------------------------------------------------------------
procedure TfrmPacker.DetermineSize(out AWidth, AHeight: Integer);
var Partition: TPartition;
var Index    : Integer;
var ARect    : TRect;
var Done     : Boolean;
var APadding : Integer;
begin
  APadding := Round( edPadding.Value );

  AWidth := 16;
  AHeight:= 16;
  repeat
    Partition:= TPartition.Create(0, 0, AWidth, AHeight, ePackingMode_BestFitFromNWCorner);

    Done:= True;
    for Index := 0 to Items.Count - 1 do
    begin
      Done:= Done and Partition.Insert(Items[Index].Width + APadding, Items[Index].Height + APadding, ARect);

      if not Done then Break;
    end;
    Partition.Free;

    if Done then Exit;

    if AWidth <= AHeight then
    begin
      AWidth := AWidth  * 2;
    end else
    begin
      AHeight:= AHeight * 2;
    end;

  until Done;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.lwImagesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (not btnOk.Enabled) and Selected then
  begin
    Items[Item.Index].Image.Draw(FBuffer, FTransparent);
  end;
  ScrollBox1.Invalidate;
end;


{$REGION 'Actions'}

//------------------------------------------------------------------------------
procedure TfrmPacker.actImageClearExecute(Sender: TObject);
begin
  Items.Clear;

  lwImagesUpdate;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.actImageAddExecute(Sender: TObject);
var Index: Integer;
begin
  if OpenImageDialog.Execute then
  begin
    for Index := 0 to OpenImageDialog.Files.Count - 1 do
    begin
       AddImage(OpenImageDialog.Files[Index]);
    end;
  end;

  for Index := 0 to Items.Count - 1 do
  begin
    Items[Index].Placed:= False;
  end;
  btnOk.Enabled:= False;

  lwImagesUpdate;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.actImageRemoveExecute(Sender: TObject);
var Index: Integer;
begin
  if lwImages.SelCount = 0 then Exit;

  Items.Delete(lwImages.Selected.Index);

  for Index := 0 to Items.Count - 1 do
  begin
    Items[Index].Placed:= False;
  end;
  btnOk.Enabled:= False;

  lwImagesUpdate;
end;

//------------------------------------------------------------------------------
procedure TfrmPacker.actPackerPlaceExecute(Sender: TObject);
var Partition: TPartition;
var Index    : Integer;
var ARect    : TRect;
var AllPacked: Boolean;

var Source: TRecti;
var Dest: TVector2i;
var AWidth  : Integer;
var AHeight : Integer;
var APadding: Integer;
begin
  Screen.Cursor:= crHourGlass;
  /// FixME
  Items.Sort({TPHXPackerWidthComparer.Create});
  //Items.Sort(TPHXPackerWidthComparer.Create);

  if SameText(cbWidth.Text, '(auto)') or SameText(cbHeight.Text, '(auto)') then
  begin
    DetermineSize(AWidth, AHeight);
  end else
  begin
    AWidth := StrToIntDef( cbWidth.Text , 256);
    AHeight:= StrToIntDef( cbHeight.Text, 256);
  end;
  APadding := Round( edPadding.Value );


  Image.Resize(AWidth, AHeight, pfRGBA);
  Image.Fill(0, 0, 0, 0);

  Partition:= TPartition.Create(0, 0, AWidth, AHeight, ePackingMode_BestFitFromNWCorner);

  AllPacked:= True;
  for Index := 0 to Items.Count - 1 do
  begin
    Items[Index].Placed:= Partition.Insert(Items[Index].Width + APadding, Items[Index].Height + APadding, ARect);

    if Items[Index].Placed then
    begin
      Source.Left  := 0;
      Source.Top   := 0;
      Source.Right := Items[Index].Width - 1;
      Source.Bottom:= Items[Index].Height - 1;

      Dest.X:= ARect.Left;
      Dest.Y:= ARect.Top;

      Image.CopyFrom(Items[Index].Image, Source, Dest);

      Items[Index].Position.X:= ARect.Left;
      Items[Index].Position.Y:= ARect.Top;
    end else
    begin
      AllPacked:= False;
    end;
  end;
  Partition.Free;

  FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);

  Image.Draw(FBuffer, FTransparent);

  lwImagesUpdate;

  ScrollBox1.Invalidate;
  PaintBox1.Invalidate;

  Screen.Cursor:= crDefault;

  if not AllPacked then
  begin
    MessageDlg('There was not enough space in the destination image to pack'#13'all the images.', mtInformation, [mbOK], 0)
  end;

  btnOk.Enabled:= True;
end;

{$ENDREGION}





//------------------------------------------------------------------------------
{
procedure TfrmPacker.DropFiles(var msg: TWMDropFiles);
var Count: Integer;
var Index: Integer;
var Name : array [0..255] of char;
begin
  // how many files dropped?
  Count:= DragQueryFile(msg.Drop, $FFFFFFFF, Name, 255) ;
  try
    // query for file names
    for Index:= 0 to Count - 1 do
    begin
      DragQueryFile(msg.Drop, Index, Name, 255) ;

      AddImage(Name);
    end;
  finally
    DragFinish(msg.Drop) ;
  end;

  lwImagesUpdate;
end;
}




end.
